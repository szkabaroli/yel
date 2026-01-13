//! AST to HIR lowering.

use std::collections::HashMap;

use crate::context::CompilerContext;
use crate::definitions::{
    ComponentDef, DefKind, EnumDef, FieldDef, FunctionDef, Namespace, ParameterDef, RecordDef,
    SignalDef, VariantCaseDef, VariantDef,
};
use crate::ids::{DefId, FieldIdx, NodeId, VariantIdx};
use crate::source::Span;
use crate::syntax::ast::{self, PropModifier};
use crate::types::Ty;

use super::expr::{BinOp, HirExpr, HirExprKind, HirInterpolationPart, HirLiteral, HirStatement, UnaryOp};
use super::local_scope::LocalScope;
use super::node::{HirBinding, HirComponent, HirHandler, HirNode, HirNodeKind};

/// Lower an AST file, populating definitions in ctx.
pub fn lower_file(file: &ast::File, ctx: &mut CompilerContext) -> Vec<HirComponent> {
    let mut lowering = HirLowering::new(ctx);
    lowering.lower_file(file)
}

/// HIR lowering state.
struct HirLowering<'ctx> {
    ctx: &'ctx mut CompilerContext,
    /// Local variables in current body.
    locals: LocalScope,
    /// Next node ID.
    next_node_id: u32,
}

impl<'ctx> HirLowering<'ctx> {
    fn new(ctx: &'ctx mut CompilerContext) -> Self {
        Self {
            ctx,
            locals: LocalScope::new(),
            next_node_id: 0,
        }
    }

    fn fresh_node_id(&mut self) -> NodeId {
        let id = NodeId::new(self.next_node_id);
        self.next_node_id += 1;
        id
    }

    fn lower_file(&mut self, file: &ast::File) -> Vec<HirComponent> {
        // Phase 1: Register all top-level type definitions
        for record in &file.records {
            self.register_record(&record.node, record.span);
        }

        for enum_decl in &file.enums {
            self.register_enum(&enum_decl.node, enum_decl.span);
        }

        for variant in &file.variants {
            self.register_variant(&variant.node, variant.span);
        }

        // Phase 2: Register component definitions (without bodies)
        for component in &file.components {
            self.register_component(&component.node, component.span);
        }

        // Phase 3: Lower component bodies
        let mut hir_components = Vec::new();
        for component in &file.components {
            if let Some(hir) = self.lower_component(&component.node, component.span) {
                hir_components.push(hir);
            }
        }

        hir_components
    }

    fn register_record(&mut self, record: &ast::Record, span: Span) {
        let name = self.ctx.intern(&record.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Record(RecordDef {
                def_id: DefId::INVALID, // Will update
                name,
                fields: vec![],
            }),
            span,
        );

        // Update def_id in the RecordDef
        if let Some(r) = self.ctx.defs.as_record_mut(def_id) {
            r.def_id = def_id;
        }

        self.ctx.defs.register_name(name, Namespace::Type, def_id);

        // Register fields
        let mut field_ids = Vec::new();
        for (idx, field) in record.fields.iter().enumerate() {
            let field_name = self.ctx.intern(&field.node.name);
            let field_ty = self.ctx.intern_ast_ty(&field.node.ty.kind);
            let field_id = self.ctx.defs.alloc(
                field_name,
                DefKind::Field(FieldDef {
                    owner: def_id,
                    name: field_name,
                    ty: field_ty,
                    idx: FieldIdx::new(idx as u32),
                }),
                field.span,
            );
            self.ctx.defs.set_type(field_id, field_ty);
            field_ids.push(field_id);
        }

        // Update record with field IDs
        if let Some(r) = self.ctx.defs.as_record_mut(def_id) {
            r.fields = field_ids;
        }

        // Create type for this record
        let record_ty = self.ctx.mk_adt(def_id);
        self.ctx.defs.set_type(def_id, record_ty);
    }

    fn register_enum(&mut self, enum_decl: &ast::Enum, span: Span) {
        let name = self.ctx.intern(&enum_decl.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Enum(EnumDef {
                def_id: DefId::INVALID,
                name,
                cases: vec![],
            }),
            span,
        );

        self.ctx.defs.register_name(name, Namespace::Type, def_id);

        // Register cases
        let mut case_ids = Vec::new();
        for (idx, case) in enum_decl.cases.iter().enumerate() {
            let case_name = self.ctx.intern(&case.node);
            let case_id = self.ctx.defs.alloc(
                case_name,
                DefKind::VariantCase(VariantCaseDef {
                    owner: def_id,
                    name: case_name,
                    payload: None,
                    idx: VariantIdx::new(idx as u32),
                }),
                case.span,
            );
            case_ids.push(case_id);
        }

        // Update enum with case IDs - need to get mutable reference differently
        if let DefKind::Enum(e) = &mut self.ctx.defs.get_mut(def_id).kind {
            e.def_id = def_id;
            e.cases = case_ids;
        }

        let enum_ty = self.ctx.mk_adt(def_id);
        self.ctx.defs.set_type(def_id, enum_ty);
    }

    fn register_variant(&mut self, variant: &ast::Variant, span: Span) {
        let name = self.ctx.intern(&variant.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Variant(VariantDef {
                def_id: DefId::INVALID,
                name,
                cases: vec![],
            }),
            span,
        );

        self.ctx.defs.register_name(name, Namespace::Type, def_id);

        // Register cases
        let mut case_ids = Vec::new();
        for (idx, case) in variant.cases.iter().enumerate() {
            let case_name = self.ctx.intern(&case.node.name);
            let payload = case
                .node
                .payload
                .as_ref()
                .map(|ty| self.ctx.intern_ast_ty(&ty.kind));
            let case_id = self.ctx.defs.alloc(
                case_name,
                DefKind::VariantCase(VariantCaseDef {
                    owner: def_id,
                    name: case_name,
                    payload,
                    idx: VariantIdx::new(idx as u32),
                }),
                case.span,
            );
            case_ids.push(case_id);
        }

        if let DefKind::Variant(v) = &mut self.ctx.defs.get_mut(def_id).kind {
            v.def_id = def_id;
            v.cases = case_ids;
        }

        let variant_ty = self.ctx.mk_adt(def_id);
        self.ctx.defs.set_type(def_id, variant_ty);
    }

    fn register_component(&mut self, component: &ast::Component, span: Span) {
        let name = self.ctx.intern(&component.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Component(ComponentDef {
                def_id: DefId::INVALID,
                name,
                properties: vec![],
                callbacks: vec![],
                is_export: component.is_export,
            }),
            span,
        );

        self.ctx
            .defs
            .register_name(name, Namespace::Component, def_id);

        // Register properties as signals
        let mut prop_ids = Vec::new();
        for (idx, prop) in component.properties.iter().enumerate() {
            let prop_name = self.ctx.intern(&prop.node.name);
            let prop_ty = self.ctx.intern_ast_ty(&prop.node.ty.kind);

            // Lower default expression if present
            let default_expr = prop.node.default.as_ref().map(|expr| {
                self.lower_expr(&expr.node, expr.span)
            });

            let prop_id = self.ctx.defs.alloc(
                prop_name,
                DefKind::Signal(SignalDef {
                    owner: def_id,
                    name: prop_name,
                    ty: prop_ty,
                    idx: FieldIdx::new(idx as u32),
                    default: default_expr,
                }),
                prop.span,
            );
            self.ctx.defs.set_type(prop_id, prop_ty);
            prop_ids.push(prop_id);
        }

        // Register functions/callbacks
        let mut func_ids = Vec::new();
        for func in &component.functions {
            let func_name = self.ctx.intern(&func.node.name);

            // Register parameters
            let mut param_ids = Vec::new();
            let mut param_tys = Vec::new();
            for (idx, (param_name, param_ty)) in func.node.params.iter().enumerate() {
                let pname = self.ctx.intern(param_name);
                let pty = self.ctx.intern_ast_ty(&param_ty.kind);
                let param_id = self.ctx.defs.alloc(
                    pname,
                    DefKind::Parameter(ParameterDef {
                        owner: DefId::INVALID, // Will update
                        name: pname,
                        ty: pty,
                        idx: idx as u32,
                    }),
                    func.span,
                );
                self.ctx.defs.set_type(param_id, pty);
                param_ids.push(param_id);
                param_tys.push(pty);
            }

            let ret_ty = func
                .node
                .return_type
                .as_ref()
                .map(|ty| self.ctx.intern_ast_ty(&ty.kind))
                .unwrap_or(Ty::UNIT);

            let func_id = self.ctx.defs.alloc(
                func_name,
                DefKind::Function(FunctionDef {
                    def_id: DefId::INVALID,
                    name: func_name,
                    params: param_ids.clone(),
                    ret_ty,
                    is_export: func.node.is_export,
                }),
                func.span,
            );

            // Update function def_id
            if let DefKind::Function(f) = &mut self.ctx.defs.get_mut(func_id).kind {
                f.def_id = func_id;
            }

            // Update parameter owners
            for param_id in &param_ids {
                if let DefKind::Parameter(p) = &mut self.ctx.defs.get_mut(*param_id).kind {
                    p.owner = func_id;
                }
            }

            // Set function type
            let func_ty = self.ctx.mk_func(
                param_tys,
                if ret_ty == Ty::UNIT {
                    None
                } else {
                    Some(ret_ty)
                },
            );
            self.ctx.defs.set_type(func_id, func_ty);

            func_ids.push(func_id);
        }

        if let DefKind::Component(c) = &mut self.ctx.defs.get_mut(def_id).kind {
            c.def_id = def_id;
            c.properties = prop_ids;
            c.callbacks = func_ids;
        }

        let comp_ty = self.ctx.mk_adt(def_id);
        self.ctx.defs.set_type(def_id, comp_ty);
    }

    fn lower_component(&mut self, component: &ast::Component, span: Span) -> Option<HirComponent> {
        let name = self.ctx.intern(&component.name);
        let def_id = self.ctx.defs.lookup(name, Namespace::Component)?;

        // Reset local scope for this component
        self.locals = LocalScope::new();

        // Add properties to local scope (with their def_ids for signal tracking)
        if let Some(comp_def) = self.ctx.defs.as_component(def_id) {
            for &prop_id in &comp_def.properties.clone() {
                let prop_name = self.ctx.defs.name(prop_id);
                let prop_ty = self.ctx.defs.type_of(prop_id).unwrap_or(Ty::ERROR);
                let prop_span = self.ctx.defs.span(prop_id);
                self.locals.define_with_def_id(prop_name, prop_ty, prop_span, Some(prop_id));
            }
        }

        // Lower body nodes
        let body: Vec<HirNode> = component
            .body
            .iter()
            .filter_map(|node| self.lower_node(&node.node, node.span))
            .collect();

        Some(HirComponent {
            def_id,
            name,
            span,
            is_export: component.is_export,
            body,
        })
    }

    fn lower_node(&mut self, node: &ast::Node, span: Span) -> Option<HirNode> {
        let id = self.fresh_node_id();

        let kind = match node {
            ast::Node::Element(elem) => self.lower_element(elem),
            ast::Node::Text(text) => HirNodeKind::Text(self.lower_expr(&text.content.node, text.content.span)),
            ast::Node::If(if_node) => self.lower_if(if_node),
            ast::Node::For(for_node) => self.lower_for(for_node),
        };

        Some(HirNode::new(id, kind, span))
    }

    fn lower_element(&mut self, elem: &ast::ElementNode) -> HirNodeKind {
        // Merge AST bindings with the same name into HirBindings
        // Map: name -> (name_span, value, setter)
        let mut binding_map: HashMap<String, (Span, Option<HirExpr>, Option<Vec<HirStatement>>)> =
            HashMap::new();

        for b in &elem.bindings {
            let entry = binding_map
                .entry(b.node.name.clone())
                .or_insert_with(|| (b.node.name_span, None, None));

            match b.node.modifier {
                PropModifier::None => {
                    // Getter - lower the value expression
                    entry.1 = Some(self.lower_expr(&b.node.value.node, b.node.value.span));
                }
                PropModifier::Set => {
                    // Setter - extract closure body as statements
                    if let ast::Expr::Closure { body, .. } = &b.node.value.node {
                        entry.2 = Some(
                            body.iter()
                                .map(|s| self.lower_statement(&s.node, s.span))
                                .collect(),
                        );
                    }
                }
            }
        }

        let bindings: Vec<HirBinding> = binding_map
            .into_iter()
            .map(|(name, (name_span, value, setter))| {
                // Error if setter is defined without a getter
                if setter.is_some() && value.is_none() {
                    self.ctx.diagnostics.error(
                        name_span,
                        format!(
                            "binding `{}` has a setter but no getter; add a value binding like `{}: <expr>`",
                            name, name
                        ),
                    );
                }
                HirBinding {
                    name,
                    name_span,
                    value,
                    setter,
                }
            })
            .collect();

        let handlers: Vec<HirHandler> = elem
            .handlers
            .iter()
            .map(|h| HirHandler {
                name: h.node.name.clone(),
                name_span: h.node.name_span,
                body: h
                    .node
                    .body
                    .iter()
                    .map(|s| self.lower_statement(&s.node, s.span))
                    .collect(),
            })
            .collect();

        let children: Vec<HirNode> = elem
            .children
            .iter()
            .filter_map(|n| self.lower_node(&n.node, n.span))
            .collect();

        HirNodeKind::Element {
            name: elem.name.clone(),
            bindings,
            handlers,
            children,
        }
    }

    fn lower_if(&mut self, if_node: &ast::IfNode) -> HirNodeKind {
        let condition = self.lower_expr(&if_node.condition.node, if_node.condition.span);

        let then_branch: Vec<HirNode> = if_node
            .then_branch
            .iter()
            .filter_map(|n| self.lower_node(&n.node, n.span))
            .collect();

        let else_if_branches: Vec<(HirExpr, Vec<HirNode>)> = if_node
            .else_if_branches
            .iter()
            .map(|(cond, nodes)| {
                let c = self.lower_expr(&cond.node, cond.span);
                let n: Vec<HirNode> = nodes
                    .iter()
                    .filter_map(|node| self.lower_node(&node.node, node.span))
                    .collect();
                (c, n)
            })
            .collect();

        let else_branch: Option<Vec<HirNode>> = if_node.else_branch.as_ref().map(|nodes| {
            nodes
                .iter()
                .filter_map(|n| self.lower_node(&n.node, n.span))
                .collect()
        });

        HirNodeKind::If {
            condition,
            then_branch,
            else_if_branches,
            else_branch,
        }
    }

    fn lower_for(&mut self, for_node: &ast::ForNode) -> HirNodeKind {
        // Create a new scope for the loop
        self.locals.push_scope();

        let item_name = self.ctx.intern(&for_node.item_name);
        let item_span = for_node.item_name_span;
        // Item type will be inferred during type checking
        let item_id = self.locals.define(item_name, Ty::ERROR, item_span);

        let iterable = self.lower_expr(&for_node.iterable.node, for_node.iterable.span);
        let key = for_node
            .key
            .as_ref()
            .map(|k| self.lower_expr(&k.node, k.span));

        let body: Vec<HirNode> = for_node
            .body
            .iter()
            .filter_map(|n| self.lower_node(&n.node, n.span))
            .collect();

        self.locals.pop_scope();

        HirNodeKind::For {
            item: item_id,
            item_name,
            item_span,
            item_ty: Ty::ERROR, // Will be inferred
            iterable,
            key,
            body,
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr, span: Span) -> HirExpr {
        let kind = match expr {
            ast::Expr::Ident(name) => {
                let interned = self.ctx.intern(name);

                // First check locals
                if let Some(local_id) = self.locals.lookup(interned) {
                    HirExprKind::Local(local_id)
                }
                // Then check definitions (properties, functions, types)
                else if let Some(def_id) = self.ctx.defs.lookup(interned, Namespace::Value) {
                    HirExprKind::Def(def_id)
                } else if let Some(def_id) = self.ctx.defs.lookup(interned, Namespace::Type) {
                    HirExprKind::Def(def_id)
                } else if let Some(def_id) = self.ctx.defs.lookup(interned, Namespace::Component) {
                    HirExprKind::Def(def_id)
                } else {
                    // Unresolved - might be a builtin or error
                    // Keep as identifier for now, will be resolved in THIR
                    HirExprKind::Call {
                        func: name.clone(),
                        args: vec![],
                    }
                }
            }

            ast::Expr::Literal(lit) => HirExprKind::Literal(self.lower_literal(lit)),

            ast::Expr::Binary(lhs, op, rhs) => {
                let bin_op = BinOp::from_str(op).unwrap_or(BinOp::Add);
                HirExprKind::Binary {
                    op: bin_op,
                    lhs: Box::new(self.lower_expr(&lhs.node, lhs.span)),
                    rhs: Box::new(self.lower_expr(&rhs.node, rhs.span)),
                }
            }

            ast::Expr::Unary(op, operand) => {
                let unary_op = UnaryOp::from_str(op).unwrap_or(UnaryOp::Neg);
                HirExprKind::Unary {
                    op: unary_op,
                    operand: Box::new(self.lower_expr(&operand.node, operand.span)),
                }
            }

            ast::Expr::Call(func, args) => HirExprKind::Call {
                func: func.clone(),
                args: args
                    .iter()
                    .map(|a| self.lower_expr(&a.node, a.span))
                    .collect(),
            },

            ast::Expr::PathCall { base, member, args } => {
                // Handle Type.case(args) for variant constructors
                if let ast::Expr::Ident(type_name) = &base.node {
                    HirExprKind::PathCall {
                        base: type_name.clone(),
                        member: member.clone(),
                        args: args
                            .iter()
                            .map(|a| self.lower_expr(&a.node, a.span))
                            .collect(),
                    }
                } else {
                    // Nested path calls not supported yet
                    HirExprKind::Error
                }
            }

            ast::Expr::Member(base, field) => {
                // Check if base is a type name (enum, variant, etc.)
                if let ast::Expr::Ident(name) = &base.node {
                    let interned = self.ctx.intern(name);
                    if let Some(def_id) = self.ctx.defs.lookup(interned, Namespace::Type) {
                        // This is a path like Enum.case or Variant.ctor
                        if self.ctx.defs.as_enum(def_id).is_some()
                            || self.ctx.defs.as_variant(def_id).is_some()
                        {
                            return HirExpr::new(
                                HirExprKind::Path {
                                    segments: vec![name.clone(), field.clone()],
                                },
                                span,
                            );
                        }
                    }
                }
                // Regular field access
                HirExprKind::Field {
                    base: Box::new(self.lower_expr(&base.node, base.span)),
                    field: field.clone(),
                }
            }

            ast::Expr::OptionalMember(base, field) => HirExprKind::OptionalField {
                base: Box::new(self.lower_expr(&base.node, base.span)),
                field: field.clone(),
            },

            ast::Expr::Index(base, index) => HirExprKind::Index {
                base: Box::new(self.lower_expr(&base.node, base.span)),
                index: Box::new(self.lower_expr(&index.node, index.span)),
            },

            ast::Expr::Range {
                start,
                end,
                inclusive,
            } => HirExprKind::Range {
                start: Box::new(self.lower_expr(&start.node, start.span)),
                end: Box::new(self.lower_expr(&end.node, end.span)),
                inclusive: *inclusive,
            },

            ast::Expr::Ternary {
                condition,
                then_expr,
                else_expr,
            } => HirExprKind::Ternary {
                condition: Box::new(self.lower_expr(&condition.node, condition.span)),
                then_expr: Box::new(self.lower_expr(&then_expr.node, then_expr.span)),
                else_expr: Box::new(self.lower_expr(&else_expr.node, else_expr.span)),
            },

            ast::Expr::Closure { params, body } => {
                let lowered_params: Vec<(String, Ty)> = params
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.ctx.intern_ast_ty(&ty.kind)))
                    .collect();

                let lowered_body: Vec<HirStatement> = body
                    .iter()
                    .map(|s| self.lower_statement(&s.node, s.span))
                    .collect();

                HirExprKind::Closure {
                    params: lowered_params,
                    body: lowered_body,
                }
            }

            ast::Expr::Interpolation(parts) => {
                let lowered_parts: Vec<HirInterpolationPart> = parts
                    .iter()
                    .map(|p| match p {
                        ast::InterpolationPart::Literal(s) => HirInterpolationPart::Literal(s.clone()),
                        ast::InterpolationPart::Expr(e) => {
                            HirInterpolationPart::Expr(self.lower_expr(&e.node, e.span))
                        }
                    })
                    .collect();
                HirExprKind::Interpolation(lowered_parts)
            }
        };

        HirExpr::new(kind, span)
    }

    fn lower_literal(&mut self, lit: &ast::Literal) -> HirLiteral {
        match lit {
            ast::Literal::Int(v) => HirLiteral::Int(*v),
            ast::Literal::Float(v) => HirLiteral::Float(*v),
            ast::Literal::String(s) => HirLiteral::String(s.clone()),
            ast::Literal::Char(c) => HirLiteral::Char(*c),
            ast::Literal::Bool(b) => HirLiteral::Bool(*b),
            ast::Literal::Unit(v, u) => HirLiteral::Unit(*v, u.clone()),
            ast::Literal::Color(c) => HirLiteral::Color(c.clone()),
            ast::Literal::List(items) => HirLiteral::List(
                items
                    .iter()
                    .map(|i| self.lower_expr(&i.node, i.span))
                    .collect(),
            ),
            ast::Literal::Tuple(items) => HirLiteral::Tuple(
                items
                    .iter()
                    .map(|i| self.lower_expr(&i.node, i.span))
                    .collect(),
            ),
            ast::Literal::Record { fields } => HirLiteral::Record {
                fields: fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.lower_expr(&expr.node, expr.span)))
                    .collect(),
            },
        }
    }

    fn lower_statement(&mut self, stmt: &ast::Statement, _span: Span) -> HirStatement {
        match stmt {
            ast::Statement::Expr(e) => HirStatement::Expr(self.lower_expr(&e.node, e.span)),

            ast::Statement::Assign(target, value) => HirStatement::Assign {
                target: self.lower_expr(&target.node, target.span),
                value: self.lower_expr(&value.node, value.span),
            },

            ast::Statement::CompoundAssign(target, op, value) => {
                // Desugar: target op= value  →  target = target op value
                let bin_op = match op.as_str() {
                    "+=" => BinOp::Add,
                    "-=" => BinOp::Sub,
                    "*=" => BinOp::Mul,
                    "/=" => BinOp::Div,
                    _ => BinOp::Add,
                };
                let target_expr = self.lower_expr(&target.node, target.span);
                let value_expr = self.lower_expr(&value.node, value.span);
                // Build: target op value
                let combined = HirExpr {
                    kind: HirExprKind::Binary {
                        op: bin_op,
                        lhs: Box::new(target_expr.clone()),
                        rhs: Box::new(value_expr),
                    },
                    span: target.span,
                };
                HirStatement::Assign {
                    target: target_expr,
                    value: combined,
                }
            }

            ast::Statement::If {
                condition,
                then_branch,
                else_branch,
            } => HirStatement::If {
                condition: self.lower_expr(&condition.node, condition.span),
                then_branch: then_branch
                    .iter()
                    .map(|s| self.lower_statement(&s.node, s.span))
                    .collect(),
                else_branch: else_branch
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.lower_statement(&s.node, s.span)).collect()),
            },
        }
    }
}
