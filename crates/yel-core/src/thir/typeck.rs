//! Bidirectional type checking with constraint-based inference for HIR → THIR.
//!
//! This module implements a bidirectional type checker that separates type checking
//! into two modes:
//! - **Infer (⇒)**: Synthesize a type from the expression (bottom-up)
//! - **Check (⇐)**: Verify expression against an expected type (top-down)
//!
//! This enables type inference for expressions that cannot determine their own type
//! (anonymous records, empty lists, none, lambdas without annotations).

use crate::context::CompilerContext;
use crate::definitions::{DefKind, Namespace};
use crate::hir::expr::{BinOp, HirExpr, HirExprKind, HirInterpolationPart, HirLiteral, HirStatement, UnaryOp};
use crate::interner::Name;
use crate::hir::local_scope::LocalScope;
use crate::hir::node::{HirBinding, HirComponent, HirHandler, HirNode, HirNodeKind};
use crate::ids::{DefId, ExprId, FieldIdx, NodeId, VariantIdx};
use crate::source::Span;
use crate::types::{InternedTyKind, Ty};

use super::expr::{ThirExpr, ThirExprKind, ThirInterpolationPart, ThirStatement};
use super::node::{ThirBinding, ThirComponent, ThirHandler, ThirNode, ThirNodeKind};

// ============================================================================
// BIDIRECTIONAL TYPE CHECKING MODE
// ============================================================================

/// The mode for type checking an expression.
#[derive(Debug, Clone)]
pub enum Mode {
    /// Synthesize/infer the type from the expression.
    Infer,
    /// Check the expression against an expected type.
    Check(Ty),
}

impl Mode {
    /// Get the expected type if in Check mode.
    pub fn expected(&self) -> Option<Ty> {
        match self {
            Mode::Infer => None,
            Mode::Check(ty) => Some(*ty),
        }
    }
}

// ============================================================================
// TYPE MAP (for IDE features)
// ============================================================================

/// Map from spans to inferred types (for hover, completion, etc.).
#[derive(Debug, Default, Clone)]
pub struct TypeMap {
    entries: Vec<(Span, Ty)>,
}

impl TypeMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the type at a given offset.
    pub fn type_at(&self, offset: usize) -> Option<Ty> {
        self.entries
            .iter()
            .filter(|(span, _)| span.start <= offset && offset < span.end)
            .min_by_key(|(span, _)| span.end - span.start)
            .map(|(_, ty)| *ty)
    }

    /// Record a type at a span.
    pub fn insert(&mut self, span: Span, ty: Ty) {
        self.entries.push((span, ty));
    }
}

// ============================================================================
// TYPE CHECK RESULT
// ============================================================================

/// Result of type checking a component.
#[derive(Debug)]
pub struct TypeCheckResult {
    /// The typed component.
    pub component: ThirComponent,
    /// Inferred types at each expression (for IDE features).
    pub type_map: TypeMap,
}

// ============================================================================
// TYPE CHECKER
// ============================================================================

/// Type check a component, producing THIR.
pub fn type_check(component: &HirComponent, ctx: &mut CompilerContext) -> ThirComponent {
    let mut checker = TypeChecker::new(ctx);
    checker.check_component(component)
}

/// Type check a component and get both result and type map.
pub fn type_check_with_map(
    component: &HirComponent,
    ctx: &mut CompilerContext,
) -> TypeCheckResult {
    let mut checker = TypeChecker::new(ctx);
    let thir = checker.check_component(component);
    TypeCheckResult {
        component: thir,
        type_map: checker.type_map,
    }
}

/// Type checker state.
struct TypeChecker<'ctx> {
    ctx: &'ctx mut CompilerContext,
    /// Local variables in current scope.
    locals: LocalScope,
    /// Type map for IDE features.
    type_map: TypeMap,
    /// Next expression ID.
    next_expr_id: u32,
    /// Next node ID.
    next_node_id: u32,
    /// Current component DefId (for property lookup).
    current_component: DefId,
}

impl<'ctx> TypeChecker<'ctx> {
    fn new(ctx: &'ctx mut CompilerContext) -> Self {
        Self {
            ctx,
            locals: LocalScope::new(),
            type_map: TypeMap::new(),
            next_expr_id: 0,
            next_node_id: 0,
            current_component: DefId::INVALID,
        }
    }

    fn fresh_expr_id(&mut self) -> ExprId {
        let id = ExprId::new(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }

    fn fresh_node_id(&mut self) -> NodeId {
        let id = NodeId::new(self.next_node_id);
        self.next_node_id += 1;
        id
    }

    // ========================================================================
    // Component checking
    // ========================================================================

    fn check_component(&mut self, component: &HirComponent) -> ThirComponent {
        self.current_component = component.def_id;
        self.locals = LocalScope::new();

        // Add component properties to local scope (with their def_ids for signal tracking)
        // and type check their default values
        if let Some(comp_def) = self.ctx.defs.as_component(component.def_id) {
            for &prop_id in &comp_def.properties.clone() {
                let prop_name = self.ctx.defs.name(prop_id);
                let prop_ty = self.ctx.defs.type_of(prop_id).unwrap_or(Ty::ERROR);
                let prop_span = self.ctx.defs.span(prop_id);
                self.locals.define_with_def_id(prop_name, prop_ty, prop_span, Some(prop_id));

                // Type check default value against declared type
                if let Some(signal_def) = self.ctx.defs.as_signal(prop_id) {
                    if let Some(ref default_expr) = signal_def.default.clone() {
                        let _ = self.type_check_expr(default_expr, Mode::Check(prop_ty));
                    }
                }
            }
        }

        // Type check body nodes
        let body = component
            .body
            .iter()
            .map(|node| self.check_node(node))
            .collect();

        ThirComponent {
            def_id: component.def_id,
            name: component.name,
            span: component.span,
            is_export: component.is_export,
            locals: std::mem::take(&mut self.locals),
            body,
        }
    }

    // ========================================================================
    // Node checking
    // ========================================================================

    fn check_node(&mut self, node: &HirNode) -> ThirNode {
        let id = self.fresh_node_id();
        let kind = match &node.kind {
            HirNodeKind::Element {
                name,
                bindings,
                handlers,
                children,
            } => {
                // Resolve component if uppercase name
                let component_def = if name.chars().next().map_or(false, |c| c.is_uppercase()) {
                    let name_interned = self.ctx.intern(name);
                    self.ctx.defs.lookup(name_interned, Namespace::Component)
                } else {
                    None
                };

                let thir_bindings = bindings.iter().map(|b| self.check_binding(b, component_def)).collect();
                let thir_handlers = handlers.iter().map(|h| self.check_handler(h)).collect();
                let thir_children = children.iter().map(|n| self.check_node(n)).collect();

                ThirNodeKind::Element {
                    component: component_def,
                    tag: name.clone(),
                    bindings: thir_bindings,
                    handlers: thir_handlers,
                    children: thir_children,
                }
            }

            HirNodeKind::Text(expr) => {
                // Text content should be string
                let thir_expr = self.type_check_expr(expr, Mode::Check(Ty::STRING));
                ThirNodeKind::Text(thir_expr)
            }

            HirNodeKind::If {
                condition,
                then_branch,
                else_if_branches,
                else_branch,
            } => {
                // Condition must be bool
                let thir_condition = self.type_check_expr(condition, Mode::Check(Ty::BOOL));

                let thir_then = then_branch.iter().map(|n| self.check_node(n)).collect();

                let thir_else_ifs = else_if_branches
                    .iter()
                    .map(|(cond, nodes)| {
                        let c = self.type_check_expr(cond, Mode::Check(Ty::BOOL));
                        let n = nodes.iter().map(|node| self.check_node(node)).collect();
                        (c, n)
                    })
                    .collect();

                let thir_else = else_branch
                    .as_ref()
                    .map(|nodes| nodes.iter().map(|n| self.check_node(n)).collect());

                ThirNodeKind::If {
                    condition: thir_condition,
                    then_branch: thir_then,
                    else_if_branches: thir_else_ifs,
                    else_branch: thir_else,
                }
            }

            HirNodeKind::For {
                item: _,
                item_name,
                item_span,
                item_ty: _,
                iterable,
                key,
                body,
            } => {
                // Infer iterable type and extract element type
                let thir_iterable = self.type_check_expr(iterable, Mode::Infer);

                let item_ty = match self.ctx.ty_kind(thir_iterable.ty) {
                    InternedTyKind::List(elem) => *elem,
                    _ => {
                        self.ctx.diagnostics.error(
                            iterable.span,
                            format!(
                                "for loop requires a list type, found `{}`",
                                self.type_to_string(thir_iterable.ty)
                            ),
                        );
                        Ty::ERROR
                    }
                };

                // Push scope for loop body and define the loop variable
                self.locals.push_scope();
                let new_item = self.locals.define(*item_name, item_ty, *item_span);

                let thir_key = key.as_ref().map(|k| self.type_check_expr(k, Mode::Infer));
                let thir_body = body.iter().map(|n| self.check_node(n)).collect();

                self.locals.pop_scope();

                ThirNodeKind::For {
                    item: new_item,
                    item_name: *item_name,
                    item_span: *item_span,
                    item_ty,
                    iterable: thir_iterable,
                    key: thir_key,
                    body: thir_body,
                }
            }
        };

        ThirNode::new(id, kind, node.span)
    }

    fn check_binding(&mut self, binding: &HirBinding, component: Option<DefId>) -> ThirBinding {
        // Try to get expected type from component property
        let expected_ty = component.and_then(|comp_def| {
            let prop_name = self.ctx.intern(&binding.name);
            self.ctx
                .defs
                .find_field(comp_def, prop_name)
                .and_then(|(_, prop_id)| self.ctx.defs.type_of(prop_id))
        });

        let mode = match expected_ty {
            Some(ty) => Mode::Check(ty),
            None => Mode::Infer,
        };

        // Type check getter (value) if present
        let thir_value = binding
            .value
            .as_ref()
            .map(|expr| self.type_check_expr(expr, mode));

        // Type check setter body if present
        let thir_setter: Option<Vec<ThirStatement>> = binding
            .setter
            .as_ref()
            .map(|stmts| stmts.iter().map(|s| self.check_stmt(s)).collect());

        // Check for setter writing to same signal that getter reads (infinite loop)
        if let (Some(ref getter), Some(ref setter)) = (&thir_value, &thir_setter) {
            self.check_setter_overwrites_getter(
                &binding.name,
                binding.name_span,
                getter,
                setter,
            );
        }

        let prop_def = component.and_then(|comp_def| {
            let prop_name = self.ctx.intern(&binding.name);
            self.ctx.defs.find_field(comp_def, prop_name).map(|(_, id)| id)
        });

        ThirBinding {
            name: binding.name.clone(),
            name_span: binding.name_span,
            prop_def,
            value: thir_value,
            setter: thir_setter,
        }
    }

    fn check_handler(&mut self, handler: &HirHandler) -> ThirHandler {
        let thir_body = handler.body.iter().map(|s| self.check_stmt(s)).collect();

        ThirHandler {
            name: handler.name.clone(),
            name_span: handler.name_span,
            body: thir_body,
        }
    }

    // ========================================================================
    // Statement checking
    // ========================================================================

    fn check_stmt(&mut self, stmt: &HirStatement) -> ThirStatement {
        match stmt {
            HirStatement::Expr(expr) => {
                let thir = self.type_check_expr(expr, Mode::Infer);
                ThirStatement::Expr(thir)
            }

            HirStatement::Assign { target, value } => {
                let thir_target = self.type_check_expr(target, Mode::Infer);
                let thir_value = self.type_check_expr(value, Mode::Check(thir_target.ty));

                ThirStatement::Assign {
                    target: thir_target,
                    value: thir_value,
                }
            }

            HirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let thir_cond = self.type_check_expr(condition, Mode::Check(Ty::BOOL));
                let thir_then = then_branch.iter().map(|s| self.check_stmt(s)).collect();
                let thir_else = else_branch
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.check_stmt(s)).collect());

                ThirStatement::If {
                    condition: thir_cond,
                    then_branch: thir_then,
                    else_branch: thir_else,
                }
            }
        }
    }

    // ========================================================================
    // Expression type checking (bidirectional)
    // ========================================================================

    fn type_check_expr(&mut self, expr: &HirExpr, mode: Mode) -> ThirExpr {
        match mode {
            Mode::Infer => {
                let (kind, ty) = self.infer_expr(expr);
                self.type_map.insert(expr.span, ty);
                ThirExpr::new(self.fresh_expr_id(), kind, ty, expr.span)
            }
            Mode::Check(expected) => {
                let thir = self.check_expr_against(expr, expected);
                self.type_map.insert(expr.span, thir.ty);
                thir
            }
        }
    }

    /// Check an expression against an expected type.
    fn check_expr_against(&mut self, expr: &HirExpr, expected: Ty) -> ThirExpr {
        let span = expr.span;
        let id = self.fresh_expr_id();

        // Get the expected type kind for matching
        let expected_kind = self.ctx.ty_kind(expected).clone();

        // Try expression-specific check rules first
        let (kind, ty) = match (&expr.kind, &expected_kind) {
            // List checked against list type - propagate element type
            (HirExprKind::Literal(HirLiteral::List(elements)), InternedTyKind::List(elem_ty)) => {
                let elem_ty = *elem_ty;
                let thir_elements: Vec<_> = elements
                    .iter()
                    .map(|e| self.type_check_expr(e, Mode::Check(elem_ty)))
                    .collect();
                (ThirExprKind::ListLiteral {
                    elements: thir_elements,
                    element_ty: elem_ty,
                }, expected)
            }


            // Tuple checked against tuple type - propagate element types
            (HirExprKind::Literal(HirLiteral::Tuple(elements)), InternedTyKind::Tuple(elem_tys)) => {
                let elem_tys = elem_tys.clone();
                let thir_elements: Vec<_> = elements
                    .iter()
                    .zip(elem_tys.iter())
                    .map(|(e, ty)| self.type_check_expr(e, Mode::Check(*ty)))
                    .collect();
                (ThirExprKind::TupleLiteral {
                    elements: thir_elements,
                }, expected)
            }

            // Record checked against ADT type - match fields
            (HirExprKind::Literal(HirLiteral::Record { fields }), InternedTyKind::Adt(record_def)) => {
                if let Some(record) = self.ctx.defs.as_record(*record_def) {
                    // Reorder fields to match definition order
                    let record_fields = record.fields.clone();
                    let mut thir_fields = Vec::new();
                    for &field_def_id in &record_fields {
                        let field = self.ctx.defs.as_field(field_def_id);
                        if let Some(field) = field {
                            let field_name = self.ctx.str(field.name);
                            // Find the corresponding field in the literal
                            if let Some((_, expr)) = fields.iter().find(|(name, _)| name == &field_name) {
                                let thir_expr = self.type_check_expr(expr, Mode::Check(field.ty));
                                thir_fields.push(thir_expr);
                            } else {
                                // Field not provided in literal
                                self.ctx.diagnostics.error(
                                    span,
                                    format!("missing field `{}` in record literal", field_name),
                                );
                                thir_fields.push(ThirExpr::error(self.fresh_expr_id(), span));
                            }
                        }
                    }
                    (ThirExprKind::RecordLiteral {
                        record_def: *record_def,
                        fields: thir_fields,
                    }, expected)
                } else {
                    (ThirExprKind::Error, Ty::ERROR)
                }
            }

            // Integer literal - polymorphic
            (HirExprKind::Literal(lit @ HirLiteral::Int(_)), _)
                if self.is_integer_type(expected) =>
            {
                (ThirExprKind::Literal(lit.clone()), expected)
            }

            // Float literal - polymorphic
            (HirExprKind::Literal(lit @ HirLiteral::Float(_)), _)
                if self.is_float_type(expected) =>
            {
                (ThirExprKind::Literal(lit.clone()), expected)
            }

            // Ternary - propagate expected to both branches
            (
                HirExprKind::Ternary {
                    condition,
                    then_expr,
                    else_expr,
                },
                _,
            ) => {
                let cond_thir = self.type_check_expr(condition, Mode::Check(Ty::BOOL));
                let then_thir = self.type_check_expr(then_expr, Mode::Check(expected));
                let else_thir = self.type_check_expr(else_expr, Mode::Check(expected));

                (
                    ThirExprKind::Ternary {
                        condition: Box::new(cond_thir),
                        then_expr: Box::new(then_thir),
                        else_expr: Box::new(else_thir),
                    },
                    expected,
                )
            }

            // Binary arithmetic - propagate expected integer/float type to operands
            (HirExprKind::Binary { op, lhs, rhs }, _)
                if !op.is_comparison() && !op.is_logical() && self.is_numeric_type(expected) =>
            {
                let lhs_thir = self.type_check_expr(lhs, Mode::Check(expected));
                let rhs_thir = self.type_check_expr(rhs, Mode::Check(expected));

                (
                    ThirExprKind::Binary {
                        op: *op,
                        lhs: Box::new(lhs_thir),
                        rhs: Box::new(rhs_thir),
                    },
                    expected,
                )
            }

            // Unary negation - propagate expected numeric type
            (HirExprKind::Unary { op: UnaryOp::Neg, operand }, _)
                if self.is_numeric_type(expected) =>
            {
                let operand_thir = self.type_check_expr(operand, Mode::Check(expected));

                (
                    ThirExprKind::Unary {
                        op: UnaryOp::Neg,
                        operand: Box::new(operand_thir),
                    },
                    expected,
                )
            }

            // Index expression - propagate expected element type to list base
            (HirExprKind::Index { base, index }, _) => {
                // Create expected list type: list<expected>
                let expected_list_ty = self.ctx.mk_list(expected);
                let base_thir = self.type_check_expr(base, Mode::Check(expected_list_ty));
                let index_thir = self.type_check_expr(index, Mode::Check(Ty::S32));

                (
                    ThirExprKind::Index {
                        base: Box::new(base_thir),
                        index: Box::new(index_thir),
                    },
                    expected,
                )
            }

            // Call expression - pass expected type for option constructors
            (HirExprKind::Call { func, args }, _) => {
                let (kind, inferred) = self.check_call(func, args, span, Some(expected));
                if !self.types_compatible(inferred, expected) {
                    self.ctx.diagnostics.error(
                        span,
                        format!(
                            "type mismatch: expected `{}`, found `{}`",
                            self.type_to_string(expected),
                            self.type_to_string(inferred)
                        ),
                    );
                }
                (kind, expected)
            }

            // Default: infer then check compatibility
            _ => {
                let (kind, inferred) = self.infer_expr(expr);
                if !self.types_compatible(inferred, expected) {
                    self.ctx.diagnostics.error(
                        span,
                        format!(
                            "type mismatch: expected `{}`, found `{}`",
                            self.type_to_string(expected),
                            self.type_to_string(inferred)
                        ),
                    );
                }
                (kind, expected)
            }
        };

        ThirExpr::new(id, kind, ty, span)
    }

    /// Infer the type of an expression (synthesis mode).
    fn infer_expr(&mut self, expr: &HirExpr) -> (ThirExprKind, Ty) {
        match &expr.kind {
            HirExprKind::Local(local_id) => {
                let info = self.locals.get(*local_id);
                (ThirExprKind::Local(*local_id), info.ty)
            }

            HirExprKind::Def(def_id) => {
                let ty = self.ctx.defs.type_of(*def_id).unwrap_or(Ty::ERROR);
                (ThirExprKind::Def(*def_id), ty)
            }

            HirExprKind::Literal(lit) => {
                // Handle complex literals specially
                match lit {
                    HirLiteral::List(elements) => {
                        // Infer element type from first element
                        let thir_elements: Vec<_> = elements
                            .iter()
                            .map(|e| self.type_check_expr(e, Mode::Infer))
                            .collect();
                        let elem_ty = thir_elements
                            .first()
                            .map(|e| e.ty)
                            .unwrap_or(Ty::ERROR);
                        let list_ty = self.ctx.mk_list(elem_ty);
                        (ThirExprKind::ListLiteral {
                            elements: thir_elements,
                            element_ty: elem_ty,
                        }, list_ty)
                    }
                    HirLiteral::Tuple(elements) => {
                        let thir_elements: Vec<_> = elements
                            .iter()
                            .map(|e| self.type_check_expr(e, Mode::Infer))
                            .collect();
                        let elem_tys: Vec<_> = thir_elements.iter().map(|e| e.ty).collect();
                        let tuple_ty = self.ctx.types.intern(InternedTyKind::Tuple(elem_tys));
                        (ThirExprKind::TupleLiteral {
                            elements: thir_elements,
                        }, tuple_ty)
                    }
                    HirLiteral::Record { fields: _ } => {
                        // Anonymous record - needs expected type context
                        self.ctx.diagnostics.error(
                            expr.span,
                            "cannot infer type of anonymous record literal; add type annotation",
                        );
                        (ThirExprKind::Error, Ty::ERROR)
                    }
                    _ => {
                        let ty = self.infer_literal_type(lit);
                        (ThirExprKind::Literal(lit.clone()), ty)
                    }
                }
            }

            HirExprKind::Binary { op, lhs, rhs } => {
                let lhs_thir = self.type_check_expr(lhs, Mode::Infer);
                let rhs_thir = self.type_check_expr(rhs, Mode::Infer);

                let result_ty = if op.is_comparison() || op.is_logical() {
                    Ty::BOOL
                } else {
                    lhs_thir.ty
                };

                (
                    ThirExprKind::Binary {
                        op: *op,
                        lhs: Box::new(lhs_thir),
                        rhs: Box::new(rhs_thir),
                    },
                    result_ty,
                )
            }

            HirExprKind::Unary { op, operand } => {
                let operand_thir = self.type_check_expr(operand, Mode::Infer);
                let result_ty = match op {
                    UnaryOp::Not => Ty::BOOL,
                    UnaryOp::Neg => operand_thir.ty,
                };

                (
                    ThirExprKind::Unary {
                        op: *op,
                        operand: Box::new(operand_thir),
                    },
                    result_ty,
                )
            }

            HirExprKind::Field { base, field } => {
                let base_thir = self.type_check_expr(base, Mode::Infer);
                let (field_ty, field_idx, field_def) = self.resolve_field(base_thir.ty, field, expr.span);

                (
                    ThirExprKind::Field {
                        base: Box::new(base_thir),
                        field_idx,
                        field_def,
                    },
                    field_ty,
                )
            }

            HirExprKind::OptionalField { base, field } => {
                let base_thir = self.type_check_expr(base, Mode::Infer);

                // Unwrap option to get inner type
                let inner_ty = match self.ctx.ty_kind(base_thir.ty) {
                    InternedTyKind::Option(inner) => *inner,
                    _ => {
                        self.ctx.diagnostics.error(
                            expr.span,
                            format!(
                                "optional chaining requires option type, found `{}`",
                                self.type_to_string(base_thir.ty)
                            ),
                        );
                        Ty::ERROR
                    }
                };

                let (field_ty, field_idx, field_def) = self.resolve_field(inner_ty, field, expr.span);

                // Result is option of field type
                let result_ty = self.ctx.mk_option(field_ty);

                (
                    ThirExprKind::OptionalField {
                        base: Box::new(base_thir),
                        field_idx,
                        field_def,
                    },
                    result_ty,
                )
            }

            HirExprKind::Index { base, index } => {
                let base_thir = self.type_check_expr(base, Mode::Infer);
                let index_thir = self.type_check_expr(index, Mode::Check(Ty::S32));

                let elem_ty = match self.ctx.ty_kind(base_thir.ty) {
                    InternedTyKind::List(elem) => *elem,
                    _ => {
                        self.ctx.diagnostics.error(
                            expr.span,
                            format!(
                                "indexing requires list type, found `{}`",
                                self.type_to_string(base_thir.ty)
                            ),
                        );
                        Ty::ERROR
                    }
                };

                (
                    ThirExprKind::Index {
                        base: Box::new(base_thir),
                        index: Box::new(index_thir),
                    },
                    elem_ty,
                )
            }

            HirExprKind::Call { func, args } => {
                // Check if it's a builtin
                if let Some((kind, ty)) = self.check_builtin_call(func, args, expr.span) {
                    return (kind, ty);
                }

                // Try to resolve as a function in Value namespace
                let func_name = self.ctx.intern(func);
                let mut func_def = self.ctx.defs.lookup(func_name, Namespace::Value);

                // If not found, check current component's callbacks
                if func_def.is_none() {
                    if let Some(comp) = self.ctx.defs.as_component(self.current_component) {
                        for &callback_id in &comp.callbacks.clone() {
                            if self.ctx.defs.name(callback_id) == func_name {
                                func_def = Some(callback_id);
                                break;
                            }
                        }
                    }
                }

                if let Some(func_def) = func_def {
                    let func_ty = self.ctx.defs.type_of(func_def).unwrap_or(Ty::ERROR);

                    let (param_tys, ret_ty) = match self.ctx.ty_kind(func_ty) {
                        InternedTyKind::Func { params, ret } => {
                            (params.clone(), ret.unwrap_or(Ty::UNIT))
                        }
                        _ => (vec![], Ty::ERROR),
                    };

                    let thir_args: Vec<_> = args
                        .iter()
                        .zip(param_tys.iter().chain(std::iter::repeat(&Ty::ERROR)))
                        .map(|(arg, &param_ty)| self.type_check_expr(arg, Mode::Check(param_ty)))
                        .collect();

                    return (
                        ThirExprKind::Call {
                            func: func_def,
                            args: thir_args,
                        },
                        ret_ty,
                    );
                }

                // Unknown function - emit error
                self.ctx.diagnostics.error(
                    expr.span,
                    format!("unknown function `{}`", func),
                );
                (ThirExprKind::Error, Ty::ERROR)
            }

            HirExprKind::PathCall { base, member, args } => {
                // This could be either:
                // 1. Type.case(args) - variant constructor with payload
                // 2. variable.method(args) - method call on a variable
                let base_name = self.ctx.intern(base);
                let member_name = self.ctx.intern(member);

                // First, check if base is a variable in scope
                if let Some(local_id) = self.locals.lookup(base_name) {
                    // It's a variable - treat as method call
                    let local_ty = self.locals.get(local_id).ty;

                    // Check for known methods based on type
                    let ty_kind = self.ctx.ty_kind(local_ty);
                    match (member.as_str(), ty_kind) {
                        ("len", InternedTyKind::List(_)) => {
                            // list.len() -> s32
                            if !args.is_empty() {
                                self.ctx.diagnostics.error(
                                    expr.span,
                                    "len() takes no arguments".to_string(),
                                );
                            }
                            let base_expr = ThirExpr::new(
                                self.fresh_expr_id(),
                                ThirExprKind::Local(local_id),
                                local_ty,
                                expr.span,
                            );
                            // For now, emit a Call to a builtin len function
                            let len_func = self.ctx.known.functions.len();
                            return (
                                ThirExprKind::Call {
                                    func: len_func,
                                    args: vec![base_expr],
                                },
                                Ty::S32,
                            );
                        }
                        ("len", InternedTyKind::String) => {
                            // string.len() -> s32
                            if !args.is_empty() {
                                self.ctx.diagnostics.error(
                                    expr.span,
                                    "len() takes no arguments".to_string(),
                                );
                            }
                            let base_expr = ThirExpr::new(
                                self.fresh_expr_id(),
                                ThirExprKind::Local(local_id),
                                local_ty,
                                expr.span,
                            );
                            let len_func = self.ctx.known.functions.len();
                            return (
                                ThirExprKind::Call {
                                    func: len_func,
                                    args: vec![base_expr],
                                },
                                Ty::S32,
                            );
                        }
                        ("get", InternedTyKind::List(element_ty)) => {
                            // list.get(idx) -> option<T>
                            // Safe element access that returns none on out-of-bounds
                            // Copy element_ty early to avoid borrow conflict
                            let element_ty = *element_ty;

                            if args.len() != 1 {
                                self.ctx.diagnostics.error(
                                    expr.span,
                                    "get() takes exactly one argument (index)".to_string(),
                                );
                                return (ThirExprKind::Error, Ty::ERROR);
                            }

                            let base_expr = ThirExpr::new(
                                self.fresh_expr_id(),
                                ThirExprKind::Local(local_id),
                                local_ty,
                                expr.span,
                            );

                            // Type check the index argument as s32
                            let index_expr = self.type_check_expr(&args[0], Mode::Check(Ty::S32));

                            let list_get_func = self.ctx.known.functions.list_get();

                            // Return type is option<element_ty>
                            let option_ty = self.ctx.types.intern(InternedTyKind::Option(element_ty));

                            return (
                                ThirExprKind::Call {
                                    func: list_get_func,
                                    args: vec![base_expr, index_expr],
                                },
                                option_ty,
                            );
                        }
                        _ => {
                            self.ctx.diagnostics.error(
                                expr.span,
                                format!("unknown method `{}` on type", member),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                    }
                }

                // Not a variable - try type lookup for variant constructor
                if let Some(type_def) = self.ctx.defs.lookup(base_name, Namespace::Type) {
                    // Check if it's a variant
                    if let Some(variant) = self.ctx.defs.as_variant(type_def) {
                        // Find the case
                        for &case_def_id in &variant.cases.clone() {
                            if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id).clone() {
                                if case.name == member_name {
                                    // Found the case - build the variant constructor
                                    let payload = if let Some(payload_ty) = case.payload {
                                        if args.len() != 1 {
                                            self.ctx.diagnostics.error(
                                                expr.span,
                                                format!("variant case `{}` expects 1 argument, found {}", member, args.len()),
                                            );
                                            return (ThirExprKind::Error, Ty::ERROR);
                                        }
                                        Some(Box::new(self.type_check_expr(&args[0], Mode::Check(payload_ty))))
                                    } else {
                                        if !args.is_empty() {
                                            self.ctx.diagnostics.error(
                                                expr.span,
                                                format!("variant case `{}` takes no arguments", member),
                                            );
                                            return (ThirExprKind::Error, Ty::ERROR);
                                        }
                                        None
                                    };

                                    let result_ty = self.ctx.mk_adt(type_def);
                                    return (
                                        ThirExprKind::VariantCtor {
                                            ty_def: type_def,
                                            case_idx: case.idx,
                                            payload,
                                        },
                                        result_ty,
                                    );
                                }
                            }
                        }
                        // Case not found
                        self.ctx.diagnostics.error(
                            expr.span,
                            format!("variant `{}` has no case `{}`", base, member),
                        );
                    } else {
                        self.ctx.diagnostics.error(
                            expr.span,
                            format!("`{}` is not a variant type", base),
                        );
                    }
                } else {
                    self.ctx.diagnostics.error(
                        expr.span,
                        format!("unknown type or variable `{}`", base),
                    );
                }
                (ThirExprKind::Error, Ty::ERROR)
            }

            HirExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                let start_thir = self.type_check_expr(start, Mode::Check(Ty::S32));
                let end_thir = self.type_check_expr(end, Mode::Check(Ty::S32));

                let list_ty = self.ctx.mk_list(Ty::S32);

                (
                    ThirExprKind::Range {
                        start: Box::new(start_thir),
                        end: Box::new(end_thir),
                        inclusive: *inclusive,
                    },
                    list_ty,
                )
            }

            HirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_thir = self.type_check_expr(condition, Mode::Check(Ty::BOOL));
                let then_thir = self.type_check_expr(then_expr, Mode::Infer);
                let else_thir = self.type_check_expr(else_expr, Mode::Check(then_thir.ty));

                (
                    ThirExprKind::Ternary {
                        condition: Box::new(cond_thir),
                        then_expr: Box::new(then_thir.clone()),
                        else_expr: Box::new(else_thir),
                    },
                    then_thir.ty,
                )
            }

            HirExprKind::Closure { params, body } => {
                self.locals.push_scope();

                let mut thir_params = Vec::new();
                for (name, ty) in params {
                    let name_interned = self.ctx.intern(name);
                    let local_id = self.locals.define(name_interned, *ty, expr.span);
                    thir_params.push((local_id, *ty));
                }

                let thir_body: Vec<_> = body.iter().map(|s| self.check_stmt(s)).collect();

                self.locals.pop_scope();

                // TODO: capture analysis
                let captures = vec![];

                // TODO: infer function type from params and body
                let func_ty = Ty::ERROR;

                (
                    ThirExprKind::Closure {
                        params: thir_params,
                        body: thir_body,
                        captures,
                    },
                    func_ty,
                )
            }

            HirExprKind::Interpolation(parts) => {
                let thir_parts: Vec<_> = parts
                    .iter()
                    .map(|p| match p {
                        HirInterpolationPart::Literal(s) => ThirInterpolationPart::Literal(s.clone()),
                        HirInterpolationPart::Expr(e) => {
                            // Interpolated expressions are converted to string
                            let thir = self.type_check_expr(e, Mode::Infer);
                            ThirInterpolationPart::Expr(thir)
                        }
                    })
                    .collect();

                (ThirExprKind::Interpolation(thir_parts), Ty::STRING)
            }

            HirExprKind::Path { segments } => {
                // Path should be Type.case format (2 segments)
                if segments.len() == 2 {
                    let type_name = &segments[0];
                    let case_name = &segments[1];
                    let type_name_interned = self.ctx.intern(type_name);
                    let case_name_interned = self.ctx.intern(case_name);

                    if let Some(def_id) = self.ctx.defs.lookup(type_name_interned, Namespace::Type) {
                        // Check if it's an enum
                        if let Some(enum_def) = self.ctx.defs.as_enum(def_id) {
                            for (idx, &case_def_id) in enum_def.cases.iter().enumerate() {
                                if self.ctx.defs.name(case_def_id) == case_name_interned {
                                    let enum_ty = self.ctx.defs.type_of(def_id).unwrap_or(Ty::ERROR);
                                    return (
                                        ThirExprKind::EnumCase {
                                            ty_def: def_id,
                                            case_idx: VariantIdx::new(idx as u32),
                                        },
                                        enum_ty,
                                    );
                                }
                            }
                            self.ctx.diagnostics.error(
                                expr.span,
                                format!("no case `{}` in enum `{}`", case_name, type_name),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }

                        // Check if it's a variant
                        if let Some(variant_def) = self.ctx.defs.as_variant(def_id) {
                            for (idx, &case_def_id) in variant_def.cases.iter().enumerate() {
                                if self.ctx.defs.name(case_def_id) == case_name_interned {
                                    let variant_ty = self.ctx.defs.type_of(def_id).unwrap_or(Ty::ERROR);
                                    // For now, no payload support in path syntax
                                    return (
                                        ThirExprKind::VariantCtor {
                                            ty_def: def_id,
                                            case_idx: VariantIdx::new(idx as u32),
                                            payload: None,
                                        },
                                        variant_ty,
                                    );
                                }
                            }
                            self.ctx.diagnostics.error(
                                expr.span,
                                format!("no case `{}` in variant `{}`", case_name, type_name),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                    }

                    self.ctx.diagnostics.error(
                        expr.span,
                        format!("unknown type `{}` in path", type_name),
                    );
                }
                (ThirExprKind::Error, Ty::ERROR)
            }

            HirExprKind::Error => (ThirExprKind::Error, Ty::ERROR),
        }
    }

    // ========================================================================
    // Helper methods
    // ========================================================================

    /// Check a function call expression.
    fn check_call(
        &mut self,
        func: &str,
        args: &[HirExpr],
        span: Span,
        expected: Option<Ty>,
    ) -> (ThirExprKind, Ty) {
        // Check if it's a builtin function
        if let Some((kind, ty)) = self.check_builtin_call(func, args, span) {
            return (kind, ty);
        }

        let func_name = self.ctx.intern(func);

        // Try to resolve as a function in Value namespace
        let mut func_def = self.ctx.defs.lookup(func_name, Namespace::Value);

        // If not found, check current component's callbacks
        if func_def.is_none() {
            if let Some(comp) = self.ctx.defs.as_component(self.current_component) {
                for &callback_id in &comp.callbacks.clone() {
                    if self.ctx.defs.name(callback_id) == func_name {
                        func_def = Some(callback_id);
                        break;
                    }
                }
            }
        }

        if let Some(func_def) = func_def {
            let func_ty = self.ctx.defs.type_of(func_def).unwrap_or(Ty::ERROR);

            let (param_tys, ret_ty) = match self.ctx.ty_kind(func_ty) {
                InternedTyKind::Func { params, ret } => {
                    (params.clone(), ret.unwrap_or(Ty::UNIT))
                }
                _ => (vec![], Ty::ERROR),
            };

            let thir_args: Vec<_> = args
                .iter()
                .zip(param_tys.iter().chain(std::iter::repeat(&Ty::ERROR)))
                .map(|(arg, &param_ty)| self.type_check_expr(arg, Mode::Check(param_ty)))
                .collect();

            return (
                ThirExprKind::Call {
                    func: func_def,
                    args: thir_args,
                },
                ret_ty,
            );
        }

        // Try to resolve as a variant constructor (e.g., `some(value)`, `rgba(...)`)
        if let Some((kind, ty)) = self.check_variant_ctor_call(func_name, args, span, expected) {
            return (kind, ty);
        }

        // Unknown function - emit error
        self.ctx.diagnostics.error(
            span,
            format!("unknown function `{}`", func),
        );
        (ThirExprKind::Error, Ty::ERROR)
    }

    /// Check if a call is actually a variant constructor.
    /// Looks for variant cases with payloads that match the function name.
    fn check_variant_ctor_call(
        &mut self,
        case_name: Name,
        args: &[HirExpr],
        span: Span,
        expected: Option<Ty>,
    ) -> Option<(ThirExprKind, Ty)> {
        // First, try to find from expected type if it's a variant
        if let Some(expected_ty) = expected {
            if let InternedTyKind::Adt(expected_def) = self.ctx.ty_kind(expected_ty) {
                if let Some(variant) = self.ctx.defs.as_variant(*expected_def) {
                    for &case_def_id in &variant.cases.clone() {
                        if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id).clone() {
                            if case.name == case_name {
                                return self.build_variant_ctor(
                                    *expected_def,
                                    case.idx,
                                    case.payload,
                                    args,
                                    span,
                                );
                            }
                        }
                    }
                }
            }
            // Check for option<T> - special built-in variant
            if let InternedTyKind::Option(inner_ty) = self.ctx.ty_kind(expected_ty) {
                let some_name = self.ctx.intern("some");
                let none_name = self.ctx.intern("none");
                if case_name == some_name {
                    return self.build_option_some(args, *inner_ty, span);
                } else if case_name == none_name {
                    return self.build_option_none(args, expected_ty, span);
                }
            }
            // Check for result<ok, err> - special built-in variant
            if let InternedTyKind::Result { ok: ok_ty, err: err_ty } = self.ctx.ty_kind(expected_ty) {
                let ok_name = self.ctx.intern("ok");
                let err_name = self.ctx.intern("err");
                if case_name == ok_name {
                    let ok = ok_ty.unwrap_or(Ty::ERROR);
                    let err = err_ty.unwrap_or(Ty::ERROR);
                    return self.build_result_ok(args, ok, err, span);
                } else if case_name == err_name {
                    let ok = ok_ty.unwrap_or(Ty::ERROR);
                    let err = err_ty.unwrap_or(Ty::ERROR);
                    return self.build_result_err(args, ok, err, span);
                }
            }
        }

        // Search all variants for a matching case name with payload
        let variants: Vec<_> = self.ctx.defs.variants().collect();
        for variant_def_id in variants {
            if let Some(variant) = self.ctx.defs.as_variant(variant_def_id) {
                for &case_def_id in &variant.cases.clone() {
                    if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id).clone() {
                        if case.name == case_name && case.payload.is_some() {
                            return self.build_variant_ctor(
                                variant_def_id,
                                case.idx,
                                case.payload,
                                args,
                                span,
                            );
                        }
                    }
                }
            }
        }

        // Check for some/none without expected type context
        let some_name = self.ctx.intern("some");
        let none_name = self.ctx.intern("none");
        if case_name == some_name {
            return self.build_option_some(args, Ty::ERROR, span);
        } else if case_name == none_name {
            let result_ty = self.ctx.mk_option(Ty::ERROR);
            return self.build_option_none(args, result_ty, span);
        }

        // Check for ok/err (result constructors) without expected type context
        let ok_name = self.ctx.intern("ok");
        let err_name = self.ctx.intern("err");
        if case_name == ok_name {
            return self.build_result_ok(args, Ty::ERROR, Ty::ERROR, span);
        } else if case_name == err_name {
            return self.build_result_err(args, Ty::ERROR, Ty::ERROR, span);
        }

        None
    }

    fn build_variant_ctor(
        &mut self,
        ty_def: DefId,
        case_idx: VariantIdx,
        payload_ty: Option<Ty>,
        args: &[HirExpr],
        span: Span,
    ) -> Option<(ThirExprKind, Ty)> {
        let payload = if let Some(expected_payload_ty) = payload_ty {
            if args.len() != 1 {
                self.ctx.diagnostics.error(
                    span,
                    format!("variant constructor expects 1 argument, found {}", args.len()),
                );
                return Some((ThirExprKind::Error, Ty::ERROR));
            }
            Some(Box::new(self.type_check_expr(&args[0], Mode::Check(expected_payload_ty))))
        } else {
            if !args.is_empty() {
                self.ctx.diagnostics.error(
                    span,
                    "variant case takes no arguments",
                );
                return Some((ThirExprKind::Error, Ty::ERROR));
            }
            None
        };

        let result_ty = self.ctx.mk_adt(ty_def);
        Some((
            ThirExprKind::VariantCtor {
                ty_def,
                case_idx,
                payload,
            },
            result_ty,
        ))
    }

    fn build_option_some(
        &mut self,
        args: &[HirExpr],
        inner_ty: Ty,
        span: Span,
    ) -> Option<(ThirExprKind, Ty)> {
        if args.len() != 1 {
            self.ctx.diagnostics.error(span, "some expects exactly 1 argument");
            return Some((ThirExprKind::Error, Ty::ERROR));
        }

        let thir_arg = if inner_ty == Ty::ERROR {
            self.type_check_expr(&args[0], Mode::Infer)
        } else {
            self.type_check_expr(&args[0], Mode::Check(inner_ty))
        };

        let result_ty = self.ctx.mk_option(thir_arg.ty);
        let option_def = self.ctx.known.builtin_types.option();
        Some((
            ThirExprKind::VariantCtor {
                ty_def: option_def,
                case_idx: VariantIdx::new(0), // some = 0
                payload: Some(Box::new(thir_arg)),
            },
            result_ty,
        ))
    }

    fn build_option_none(
        &mut self,
        args: &[HirExpr],
        result_ty: Ty,
        span: Span,
    ) -> Option<(ThirExprKind, Ty)> {
        if !args.is_empty() {
            self.ctx.diagnostics.error(span, "none takes no arguments");
            return Some((ThirExprKind::Error, Ty::ERROR));
        }

        let option_def = self.ctx.known.builtin_types.option();
        Some((
            ThirExprKind::VariantCtor {
                ty_def: option_def,
                case_idx: VariantIdx::new(1), // none = 1
                payload: None,
            },
            result_ty,
        ))
    }

    fn build_result_ok(
        &mut self,
        args: &[HirExpr],
        ok_ty: Ty,
        err_ty: Ty,
        span: Span,
    ) -> Option<(ThirExprKind, Ty)> {
        if args.len() != 1 {
            self.ctx.diagnostics.error(span, "ok expects exactly 1 argument");
            return Some((ThirExprKind::Error, Ty::ERROR));
        }

        let thir_arg = if ok_ty == Ty::ERROR {
            self.type_check_expr(&args[0], Mode::Infer)
        } else {
            self.type_check_expr(&args[0], Mode::Check(ok_ty))
        };

        let result_ty = self.ctx.mk_result(thir_arg.ty, err_ty);
        let result_def = self.ctx.known.builtin_types.result();
        Some((
            ThirExprKind::VariantCtor {
                ty_def: result_def,
                case_idx: VariantIdx::new(0), // ok = 0
                payload: Some(Box::new(thir_arg)),
            },
            result_ty,
        ))
    }

    fn build_result_err(
        &mut self,
        args: &[HirExpr],
        ok_ty: Ty,
        err_ty: Ty,
        span: Span,
    ) -> Option<(ThirExprKind, Ty)> {
        if args.len() != 1 {
            self.ctx.diagnostics.error(span, "err expects exactly 1 argument");
            return Some((ThirExprKind::Error, Ty::ERROR));
        }

        let thir_arg = if err_ty == Ty::ERROR {
            self.type_check_expr(&args[0], Mode::Infer)
        } else {
            self.type_check_expr(&args[0], Mode::Check(err_ty))
        };

        let result_ty = self.ctx.mk_result(ok_ty, thir_arg.ty);
        let result_def = self.ctx.known.builtin_types.result();
        Some((
            ThirExprKind::VariantCtor {
                ty_def: result_def,
                case_idx: VariantIdx::new(1), // err = 1
                payload: Some(Box::new(thir_arg)),
            },
            result_ty,
        ))
    }

    fn check_builtin_call(
        &mut self,
        name: &str,
        args: &[HirExpr],
        span: Span,
    ) -> Option<(ThirExprKind, Ty)> {
        match name {
            "concat" => {
                // concat is variadic: func(string...) -> string
                let thir_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.type_check_expr(arg, Mode::Check(Ty::STRING)))
                    .collect();
                let func_def = self.ctx.known.functions.concat();
                Some((
                    ThirExprKind::Call {
                        func: func_def,
                        args: thir_args,
                    },
                    Ty::STRING,
                ))
            }

            "to-string" => {
                // to-string: func(any) -> string
                // Resolves to type-specific conversion function based on argument type
                if args.len() != 1 {
                    self.ctx
                        .diagnostics
                        .error(span, "to-string expects exactly 1 argument");
                    return Some((ThirExprKind::Error, Ty::STRING));
                }
                let thir_arg = self.type_check_expr(&args[0], Mode::Infer);
                // Select the appropriate type-specific conversion function
                let func_def = self.get_to_string_func_for_type(thir_arg.ty);
                Some((
                    ThirExprKind::Call {
                        func: func_def,
                        args: vec![thir_arg],
                    },
                    Ty::STRING,
                ))
            }

            "len" | "length" => {
                // len: func(list<T> | string) -> s32
                if args.len() != 1 {
                    self.ctx
                        .diagnostics
                        .error(span, format!("{} expects exactly 1 argument", name));
                    return Some((ThirExprKind::Error, Ty::S32));
                }
                let thir_arg = self.type_check_expr(&args[0], Mode::Infer);
                let func_def = self.ctx.known.functions.len();
                Some((
                    ThirExprKind::Call {
                        func: func_def,
                        args: vec![thir_arg],
                    },
                    Ty::S32,
                ))
            }

            // Note: some/none are handled as variant constructors in check_variant_ctor_call

            _ => None,
        }
    }

    fn resolve_field(&mut self, base_ty: Ty, field: &str, span: Span) -> (Ty, FieldIdx, DefId) {
        match self.ctx.ty_kind(base_ty) {
            InternedTyKind::Adt(def_id) => {
                let field_name = self.ctx.intern(field);
                if let Some((idx, field_def)) = self.ctx.defs.find_field(*def_id, field_name) {
                    let field_ty = self.ctx.defs.type_of(field_def).unwrap_or(Ty::ERROR);
                    return (field_ty, idx, field_def);
                }
                self.ctx.diagnostics.error(
                    span,
                    format!(
                        "no field `{}` on type `{}`",
                        field,
                        self.type_to_string(base_ty)
                    ),
                );
                (Ty::ERROR, FieldIdx::new(0), DefId::INVALID)
            }

            InternedTyKind::String => {
                // Built-in string fields
                match field {
                    "len" | "length" => (Ty::S32, FieldIdx::new(0), DefId::INVALID),
                    "is_empty" => (Ty::BOOL, FieldIdx::new(1), DefId::INVALID),
                    _ => {
                        self.ctx.diagnostics.error(
                            span,
                            format!("no field `{}` on string", field),
                        );
                        (Ty::ERROR, FieldIdx::new(0), DefId::INVALID)
                    }
                }
            }

            InternedTyKind::List(_) => {
                // Built-in list fields
                match field {
                    "len" | "length" | "count" => (Ty::S32, FieldIdx::new(0), DefId::INVALID),
                    "is_empty" => (Ty::BOOL, FieldIdx::new(1), DefId::INVALID),
                    _ => {
                        self.ctx.diagnostics.error(
                            span,
                            format!("no field `{}` on list", field),
                        );
                        (Ty::ERROR, FieldIdx::new(0), DefId::INVALID)
                    }
                }
            }

            _ => {
                self.ctx.diagnostics.error(
                    span,
                    format!(
                        "cannot access field `{}` on type `{}`",
                        field,
                        self.type_to_string(base_ty)
                    ),
                );
                (Ty::ERROR, FieldIdx::new(0), DefId::INVALID)
            }
        }
    }

    fn infer_literal_type(&mut self, lit: &HirLiteral) -> Ty {
        match lit {
            HirLiteral::Int(_) => Ty::S32,
            HirLiteral::Float(_) => self.ctx.types.intern(InternedTyKind::F32),
            HirLiteral::String(_) => Ty::STRING,
            HirLiteral::Char(_) => self.ctx.types.intern(InternedTyKind::Char),
            HirLiteral::Bool(_) => Ty::BOOL,
            HirLiteral::Unit(_, unit) => match unit.as_str() {
                "px" | "pt" | "in" | "mm" | "cm" | "rem" => {
                    self.ctx.types.intern(InternedTyKind::Length)
                }
                "phx" => self.ctx.types.intern(InternedTyKind::PhysicalLength),
                "deg" | "rad" | "turn" => self.ctx.types.intern(InternedTyKind::Angle),
                "ms" | "s" => self.ctx.types.intern(InternedTyKind::Duration),
                "%" => self.ctx.types.intern(InternedTyKind::Percent),
                _ => Ty::ERROR,
            },
            HirLiteral::Color(_) => self.ctx.types.intern(InternedTyKind::Color),
            HirLiteral::List(elems) => {
                let elem_ty = elems
                    .first()
                    .map(|e| {
                        let (_, ty) = self.infer_expr_readonly(e);
                        ty
                    })
                    .unwrap_or(Ty::ERROR);
                self.ctx.types.intern(InternedTyKind::List(elem_ty))
            }
            HirLiteral::Tuple(elems) => {
                let elem_tys: Vec<_> = elems
                    .iter()
                    .map(|e| {
                        let (_, ty) = self.infer_expr_readonly(e);
                        ty
                    })
                    .collect();
                self.ctx.types.intern(InternedTyKind::Tuple(elem_tys))
            }
            HirLiteral::Record { .. } => {
                // Anonymous records need expected type
                Ty::ERROR
            }
        }
    }

    /// Infer expression type without modifying state (for nested inference).
    /// Note: Returns basic types only, complex literals return ERROR.
    fn infer_expr_readonly(&self, expr: &HirExpr) -> (ThirExprKind, Ty) {
        match &expr.kind {
            HirExprKind::Local(local_id) => {
                let info = self.locals.get(*local_id);
                (ThirExprKind::Local(*local_id), info.ty)
            }
            HirExprKind::Def(def_id) => {
                let ty = self.ctx.defs.type_of(*def_id).unwrap_or(Ty::ERROR);
                (ThirExprKind::Def(*def_id), ty)
            }
            HirExprKind::Literal(lit) => {
                // Simple literal type inference without mutation
                let ty = match lit {
                    HirLiteral::Int(_) => Ty::S32,
                    HirLiteral::String(_) => Ty::STRING,
                    HirLiteral::Bool(_) => Ty::BOOL,
                    _ => Ty::ERROR, // Complex literals need full inference
                };
                (ThirExprKind::Literal(lit.clone()), ty)
            }
            _ => (ThirExprKind::Error, Ty::ERROR),
        }
    }

    fn is_integer_type(&self, ty: Ty) -> bool {
        matches!(
            self.ctx.ty_kind(ty),
            InternedTyKind::S8
                | InternedTyKind::S16
                | InternedTyKind::S32
                | InternedTyKind::S64
                | InternedTyKind::U8
                | InternedTyKind::U16
                | InternedTyKind::U32
                | InternedTyKind::U64
        )
    }

    fn is_float_type(&self, ty: Ty) -> bool {
        matches!(
            self.ctx.ty_kind(ty),
            InternedTyKind::F32 | InternedTyKind::F64
        )
    }

    fn is_numeric_type(&self, ty: Ty) -> bool {
        self.is_integer_type(ty) || self.is_float_type(ty)
    }

    fn types_compatible(&self, actual: Ty, expected: Ty) -> bool {
        if actual == expected {
            return true;
        }

        // Handle error type
        if actual == Ty::ERROR || expected == Ty::ERROR {
            return true;
        }

        // Handle unknown
        if matches!(
            self.ctx.ty_kind(actual),
            InternedTyKind::Unknown
        ) || matches!(
            self.ctx.ty_kind(expected),
            InternedTyKind::Unknown
        ) {
            return true;
        }

        match (self.ctx.ty_kind(actual), self.ctx.ty_kind(expected)) {
            // Integer widening
            (InternedTyKind::S8, InternedTyKind::S16 | InternedTyKind::S32 | InternedTyKind::S64) => true,
            (InternedTyKind::S16, InternedTyKind::S32 | InternedTyKind::S64) => true,
            (InternedTyKind::S32, InternedTyKind::S64) => true,
            (InternedTyKind::U8, InternedTyKind::U16 | InternedTyKind::U32 | InternedTyKind::U64) => true,
            (InternedTyKind::U16, InternedTyKind::U32 | InternedTyKind::U64) => true,
            (InternedTyKind::U32, InternedTyKind::U64) => true,

            // Float widening
            (InternedTyKind::F32, InternedTyKind::F64) => true,

            // Int to float
            (InternedTyKind::S32, InternedTyKind::F32 | InternedTyKind::F64) => true,

            // Color -> Brush coercion
            (InternedTyKind::Color, InternedTyKind::Brush) => true,

            // Recursive cases
            (InternedTyKind::List(a), InternedTyKind::List(b)) => self.types_compatible(*a, *b),
            (InternedTyKind::Option(a), InternedTyKind::Option(b)) => self.types_compatible(*a, *b),

            _ => false,
        }
    }

    /// Get the appropriate to-string conversion function for a given type.
    /// Primitives get type-specific functions, complex types get object_to_string.
    fn get_to_string_func_for_type(&self, ty: Ty) -> DefId {
        let ty_kind = self.ctx.ty_kind(ty);
        match ty_kind {
            InternedTyKind::Bool => self.ctx.known.functions.bool_to_string(),
            InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 | InternedTyKind::S64 => {
                self.ctx.known.functions.s32_to_string()
            }
            InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 | InternedTyKind::U64 => {
                self.ctx.known.functions.u32_to_string()
            }
            InternedTyKind::F32 => self.ctx.known.functions.f32_to_string(),
            InternedTyKind::F64 => self.ctx.known.functions.f64_to_string(),
            InternedTyKind::Char => self.ctx.known.functions.char_to_string(),
            // String doesn't need conversion, but return object_to_string as fallback
            InternedTyKind::String => self.ctx.known.functions.object_to_string(),
            // All complex types use the generic object_to_string
            _ => self.ctx.known.functions.object_to_string(),
        }
    }

    fn type_to_string(&self, ty: Ty) -> String {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Bool => "bool".to_string(),
            InternedTyKind::S8 => "s8".to_string(),
            InternedTyKind::S16 => "s16".to_string(),
            InternedTyKind::S32 => "s32".to_string(),
            InternedTyKind::S64 => "s64".to_string(),
            InternedTyKind::U8 => "u8".to_string(),
            InternedTyKind::U16 => "u16".to_string(),
            InternedTyKind::U32 => "u32".to_string(),
            InternedTyKind::U64 => "u64".to_string(),
            InternedTyKind::F32 => "f32".to_string(),
            InternedTyKind::F64 => "f64".to_string(),
            InternedTyKind::Char => "char".to_string(),
            InternedTyKind::String => "string".to_string(),
            InternedTyKind::List(elem) => format!("list<{}>", self.type_to_string(*elem)),
            InternedTyKind::Option(inner) => format!("option<{}>", self.type_to_string(*inner)),
            InternedTyKind::Tuple(elems) => {
                let inner: Vec<_> = elems.iter().map(|e| self.type_to_string(*e)).collect();
                format!("({})", inner.join(", "))
            }
            InternedTyKind::Adt(def_id) => {
                let name = self.ctx.defs.name(*def_id);
                self.ctx.str(name)
            }
            InternedTyKind::Func { params, ret } => {
                let param_strs: Vec<_> = params.iter().map(|p| self.type_to_string(*p)).collect();
                let ret_str = ret.map(|r| self.type_to_string(r)).unwrap_or_else(|| "()".to_string());
                format!("func({}) -> {}", param_strs.join(", "), ret_str)
            }
            InternedTyKind::Length => "length".to_string(),
            InternedTyKind::PhysicalLength => "physical-length".to_string(),
            InternedTyKind::Angle => "angle".to_string(),
            InternedTyKind::Duration => "duration".to_string(),
            InternedTyKind::Percent => "percent".to_string(),
            InternedTyKind::RelativeFontSize => "relative-font-size".to_string(),
            InternedTyKind::Color => "color".to_string(),
            InternedTyKind::Brush => "brush".to_string(),
            InternedTyKind::Image => "image".to_string(),
            InternedTyKind::Easing => "easing".to_string(),
            InternedTyKind::Result { ok, err } => {
                let ok_str = ok.map(|t| self.type_to_string(t)).unwrap_or_else(|| "_".to_string());
                let err_str = err.map(|t| self.type_to_string(t)).unwrap_or_else(|| "_".to_string());
                format!("result<{}, {}>", ok_str, err_str)
            }
            InternedTyKind::Error => "<error>".to_string(),
            InternedTyKind::Unknown => "<unknown>".to_string(),
            InternedTyKind::Unit => "()".to_string(),
        }
    }
    // ========================================================================
    // Setter-writes-to-getter analysis
    // ========================================================================

    /// Collect all signal DefIds that an expression reads from.
    fn collect_expr_reads(&self, expr: &ThirExpr, reads: &mut std::collections::HashSet<DefId>) {
        match &expr.kind {
            ThirExprKind::Def(def_id) => {
                // Check if this is a signal/property
                if self.ctx.defs.is_signal(*def_id) {
                    reads.insert(*def_id);
                }
            }
            ThirExprKind::Local(local_id) => {
                // Check if this local maps to a signal
                if let Some(def_id) = self.locals.get(*local_id).def_id {
                    if self.ctx.defs.is_signal(def_id) {
                        reads.insert(def_id);
                    }
                }
            }
            ThirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_expr_reads(lhs, reads);
                self.collect_expr_reads(rhs, reads);
            }
            ThirExprKind::Unary { operand, .. } => {
                self.collect_expr_reads(operand, reads);
            }
            ThirExprKind::Field { base, .. } | ThirExprKind::OptionalField { base, .. } => {
                self.collect_expr_reads(base, reads);
            }
            ThirExprKind::Index { base, index } => {
                self.collect_expr_reads(base, reads);
                self.collect_expr_reads(index, reads);
            }
            ThirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_expr_reads(arg, reads);
                }
            }
            ThirExprKind::Range { start, end, .. } => {
                self.collect_expr_reads(start, reads);
                self.collect_expr_reads(end, reads);
            }
            ThirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.collect_expr_reads(condition, reads);
                self.collect_expr_reads(then_expr, reads);
                self.collect_expr_reads(else_expr, reads);
            }
            ThirExprKind::Interpolation(parts) => {
                for part in parts {
                    if let ThirInterpolationPart::Expr(e) = part {
                        self.collect_expr_reads(e, reads);
                    }
                }
            }
            ThirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_expr_reads(p, reads);
                }
            }
            ThirExprKind::ListLiteral { elements, .. } => {
                for e in elements {
                    self.collect_expr_reads(e, reads);
                }
            }
            ThirExprKind::TupleLiteral { elements } => {
                for e in elements {
                    self.collect_expr_reads(e, reads);
                }
            }
            ThirExprKind::RecordLiteral { fields, .. } => {
                for f in fields {
                    self.collect_expr_reads(f, reads);
                }
            }
            _ => {}
        }
    }

    /// Collect all signal DefIds that statements write to.
    fn collect_stmt_writes(&self, stmts: &[ThirStatement], writes: &mut std::collections::HashSet<DefId>) {
        for stmt in stmts {
            match stmt {
                ThirStatement::Assign { target, .. } => {
                    // Get the signal being assigned to
                    match &target.kind {
                        ThirExprKind::Def(def_id) => {
                            if self.ctx.defs.is_signal(*def_id) {
                                writes.insert(*def_id);
                            }
                        }
                        ThirExprKind::Local(local_id) => {
                            if let Some(def_id) = self.locals.get(*local_id).def_id {
                                if self.ctx.defs.is_signal(def_id) {
                                    writes.insert(def_id);
                                }
                            }
                        }
                        _ => {}
                    }
                }
                ThirStatement::If { then_branch, else_branch, .. } => {
                    self.collect_stmt_writes(then_branch, writes);
                    if let Some(else_stmts) = else_branch {
                        self.collect_stmt_writes(else_stmts, writes);
                    }
                }
                ThirStatement::Expr(_) => {}
            }
        }
    }

    /// Check if a binding's setter writes to any signal that the getter reads.
    fn check_setter_overwrites_getter(
        &mut self,
        binding_name: &str,
        binding_span: Span,
        getter: &ThirExpr,
        setter: &[ThirStatement],
    ) {
        let mut getter_reads = std::collections::HashSet::new();
        let mut setter_writes = std::collections::HashSet::new();

        self.collect_expr_reads(getter, &mut getter_reads);
        self.collect_stmt_writes(setter, &mut setter_writes);

        // Find signals that are both read by getter and written by setter
        for def_id in getter_reads.intersection(&setter_writes) {
            let signal_name = self.ctx.str(self.ctx.defs.name(*def_id));
            self.ctx.diagnostics.warning(
                binding_span,
                format!(
                    "setter for `{}` writes to signal `{}` which is also read by the getter; \
                     this will overwrite user input. Consider updating a different signal instead.",
                    binding_name, signal_name
                ),
            );
        }
    }
}

// Add helper methods to BinOp
impl BinOp {
    pub fn is_comparison(&self) -> bool {
        matches!(self, BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge)
    }

    pub fn is_logical(&self) -> bool {
        matches!(self, BinOp::And | BinOp::Or)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;

    #[test]
    fn test_type_check_simple_component() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Counter {
                count: s32 = 0;
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);

        assert_eq!(hir.len(), 1);

        let thir = compiler.type_check(&hir[0]);
        assert_eq!(compiler.context().str(thir.name), "Counter");
    }

    #[test]
    fn test_type_check_with_text() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Hello {
                name: string = "World";

                Text { "Hello {name}!" }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);
        let thir = compiler.type_check(&hir[0]);

        assert_eq!(thir.body.len(), 1);
    }

    #[test]
    fn test_type_check_if_condition() {
        let mut compiler = Compiler::new();

        let source = r#"
            component Test {
                show: bool = true;

                if show {
                    Text { "visible" }
                }
            }
        "#;

        let file = compiler.parse(source).unwrap();
        let hir = compiler.lower_to_hir(&file);
        let thir = compiler.type_check(&hir[0]);

        assert_eq!(thir.body.len(), 1);
        assert!(!compiler.has_errors());
    }
}
