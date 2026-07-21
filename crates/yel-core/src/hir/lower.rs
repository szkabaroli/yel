//! AST to HIR lowering.

use std::collections::HashMap;

use crate::context::CompilerContext;
use crate::definitions::{
    ComponentDef, DefKind, ElementDef, EnumDef, FieldDef, FunctionDef, GlobalDef,
    GlobalPropDirection, ImportComponentDef, Namespace, ParameterDef, RecordDef, SignalDef,
    VariantCaseDef, VariantDef,
};
use crate::syntax::ast::PropertyDirection;
use crate::diagnostic::{Diagnostic, ErrorCode};
use crate::ids::{DefId, FieldIdx, NodeId, VariantIdx};
use crate::source::Span;
use crate::syntax::ast::{self, PropModifier};
use crate::syntax::Spanned;
use crate::types::{InternedTyKind, Ty};

use super::expr::{BinOp, HirExpr, HirExprKind, HirInterpolationPart, HirLiteral, HirStatement, UnaryOp};
use super::local_scope::LocalScope;
use super::node::{
    HirBinding, HirComponent, HirGlobal, HirHandler, HirItem, HirNode, HirNodeKind,
};

/// Parse a `#rrggbb` or `#rrggbbaa` color hex literal into (r, g, b, a) bytes.
/// Invalid input yields fully-transparent black.
fn parse_color_hex(hex: &str) -> (u8, u8, u8, u8) {
    if hex.len() < 7 {
        return (0, 0, 0, 0);
    }
    let r = u8::from_str_radix(&hex[1..3], 16).unwrap_or(0);
    let g = u8::from_str_radix(&hex[3..5], 16).unwrap_or(0);
    let b = u8::from_str_radix(&hex[5..7], 16).unwrap_or(0);
    let a = if hex.len() >= 9 {
        u8::from_str_radix(&hex[7..9], 16).unwrap_or(255)
    } else {
        255
    };
    (r, g, b, a)
}

/// Lower an AST file, populating definitions in ctx.
pub fn lower_file(file: &ast::File, ctx: &mut CompilerContext) -> Vec<HirItem> {
    let mut lowering = HirLowering::new(ctx);
    lowering.lower_file(file)
}

/// Walk a list of AST nodes and collect the spans of every `@children`
/// marker found. Recurses into `if`/`for` bodies so a slot placed inside
/// control flow is still detected (and counts as the single allowed
/// slot for `has_children_slot` bookkeeping).
fn collect_children_slots(nodes: &[Spanned<ast::Node>], out: &mut Vec<Span>) {
    for spanned in nodes {
        match &spanned.node {
            ast::Node::Children => out.push(spanned.span),
            ast::Node::Element(elem) => collect_children_slots(&elem.children, out),
            ast::Node::If(if_node) => {
                collect_children_slots(&if_node.then_branch, out);
                for (_, body) in &if_node.else_if_branches {
                    collect_children_slots(body, out);
                }
                if let Some(else_body) = &if_node.else_branch {
                    collect_children_slots(else_body, out);
                }
            }
            ast::Node::For(for_node) => collect_children_slots(&for_node.body, out),
            ast::Node::Text(_) => {}
        }
    }
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

    /// Report a duplicate name error.
    fn report_duplicate(&mut self, name: &str, new_span: Span, existing_def_id: DefId) {
        let existing_span = self.ctx.defs.span(existing_def_id);
        let source_name = self.ctx.source_map
            .get(existing_span.source)
            .map(|s| s.name().to_string())
            .unwrap_or_else(|| "<unknown>".to_string());
        let line = self.ctx.source_map
            .get(existing_span.source)
            .map(|s| s.line_col(existing_span.start).0)
            .unwrap_or(0);
        self.ctx.diagnostics.push(
            Diagnostic::error(format!("duplicate definition of `{}`", name))
                .with_span(new_span)
                .with_code(ErrorCode::DuplicateDefinition)
                .with_note(format!("previously defined at {}:{}", source_name, line)),
        );
    }

    fn lower_file(&mut self, file: &ast::File) -> Vec<HirItem> {
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

        // Phase 1b: Register element and import component declarations
        for element in &file.elements {
            self.register_element(&element.node, element.span);
        }

        for import_comp in &file.import_components {
            self.register_import_component(&import_comp.node, import_comp.span);
        }

        for global in &file.globals {
            self.register_global(&global.node, global.span);
        }

        // Phase 2: Register component definitions (without bodies)
        for component in &file.components {
            self.register_component(&component.node, component.span);
        }

        // Phase 3: Lower component bodies, then surface globals as items.
        // Components come first so the type-check order (and therefore
        // diagnostic order) matches the previous components-then-globals
        // pipeline.
        let mut items = Vec::new();
        for component in &file.components {
            if let Some(hir) = self.lower_component(&component.node, component.span) {
                items.push(HirItem::Component(hir));
            }
        }
        for global in &file.globals {
            // The global was registered in phase 1b; emit a handle so it
            // flows through the shared item pipeline. Its name/export flag
            // and defaults already live in the `GlobalDef`.
            let name = self.ctx.intern(&global.node.name);
            if let Some(def_id) = self.ctx.defs.lookup(name, Namespace::Global) {
                let is_export = self
                    .ctx
                    .defs
                    .as_global(def_id)
                    .map(|g| g.is_export)
                    .unwrap_or(false);
                items.push(HirItem::Global(HirGlobal {
                    def_id,
                    name,
                    span: global.span,
                    is_export,
                }));
            }
        }

        items
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

        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Type, def_id) {
            self.report_duplicate(&record.name, span, existing);
        }

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

        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Type, def_id) {
            self.report_duplicate(&enum_decl.name, span, existing);
        }

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

        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Type, def_id) {
            self.report_duplicate(&variant.name, span, existing);
        }

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

    fn register_element(&mut self, element: &ast::Element, span: Span) {
        let name = self.ctx.intern(&element.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Element(ElementDef {
                def_id: DefId::INVALID,
                name,
                properties: vec![],
            }),
            span,
        );

        // Register in Component namespace (elements are used like components)
        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Component, def_id) {
            self.report_duplicate(&element.name, span, existing);
        }

        // Register properties
        let mut prop_ids = Vec::new();
        for (idx, prop) in element.properties.iter().enumerate() {
            let prop_name = self.ctx.intern(&prop.node.name);
            let prop_ty = self.ctx.intern_ast_ty(&prop.node.ty.kind);
            let prop_id = self.ctx.defs.alloc(
                prop_name,
                DefKind::Field(FieldDef {
                    owner: def_id,
                    name: prop_name,
                    ty: prop_ty,
                    idx: FieldIdx::new(idx as u32),
                }),
                prop.span,
            );
            self.ctx.defs.set_type(prop_id, prop_ty);
            prop_ids.push(prop_id);
        }

        // Update element with property IDs
        if let Some(e) = self.ctx.defs.as_element_mut(def_id) {
            e.def_id = def_id;
            e.properties = prop_ids;
        }
    }

    fn register_import_component(&mut self, import_comp: &ast::ImportComponent, span: Span) {
        let name = self.ctx.intern(&import_comp.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::ImportComponent(ImportComponentDef {
                def_id: DefId::INVALID,
                name,
                properties: vec![],
                methods: vec![],
                has_children_slot: import_comp.has_children_slot,
            }),
            span,
        );

        // Register in Component namespace
        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Component, def_id) {
            self.report_duplicate(&import_comp.name, span, existing);
        }

        // Register properties
        let mut prop_ids = Vec::new();
        for (idx, prop) in import_comp.properties.iter().enumerate() {
            let prop_name = self.ctx.intern(&prop.node.name);
            let prop_ty = self.ctx.intern_ast_ty(&prop.node.ty.kind);
            let prop_id = self.ctx.defs.alloc(
                prop_name,
                DefKind::Field(FieldDef {
                    owner: def_id,
                    name: prop_name,
                    ty: prop_ty,
                    idx: FieldIdx::new(idx as u32),
                }),
                prop.span,
            );
            self.ctx.defs.set_type(prop_id, prop_ty);
            prop_ids.push(prop_id);
        }

        // Register methods
        let mut method_ids = Vec::new();
        for method in &import_comp.methods {
            let method_name = self.ctx.intern(&method.node.name);
            let params: Vec<DefId> = method
                .node
                .params
                .iter()
                .enumerate()
                .map(|(idx, (param_name, param_ty))| {
                    let pname = self.ctx.intern(param_name);
                    let pty = self.ctx.intern_ast_ty(&param_ty.kind);
                    let param_id = self.ctx.defs.alloc(
                        pname,
                        DefKind::Parameter(ParameterDef {
                            owner: DefId::INVALID, // will update
                            name: pname,
                            ty: pty,
                            idx: crate::ids::ParamIdx::new(idx as u32),
                        }),
                        method.span,
                    );
                    self.ctx.defs.set_type(param_id, pty);
                    param_id
                })
                .collect();

            let ret_ty = method
                .node
                .return_type
                .as_ref()
                .map(|ty| self.ctx.intern_ast_ty(&ty.kind))
                .unwrap_or(Ty::UNIT);

            let method_id = self.ctx.defs.alloc(
                method_name,
                DefKind::Function(FunctionDef {
                    def_id: DefId::INVALID,
                    name: method_name,
                    params: params.clone(),
                    ret_ty,
                    is_export: false,
                }),
                method.span,
            );

            // Update param owners
            for param_id in &params {
                if let DefKind::Parameter(p) = &mut self.ctx.defs.get_mut(*param_id).kind {
                    p.owner = method_id;
                }
            }

            // Update function def_id
            if let Some(f) = self.ctx.defs.as_function_mut(method_id) {
                f.def_id = method_id;
            }

            method_ids.push(method_id);
        }

        // Update import component with property and method IDs
        if let Some(c) = self.ctx.defs.as_import_component_mut(def_id) {
            c.def_id = def_id;
            c.properties = prop_ids;
            c.methods = method_ids;
        }
    }

    /// Register a global singleton declaration.
    ///
    /// Properties become `Field` defs (owned by the global). Callbacks are
    /// `Function` defs with `is_export = false` (host implements → component
    /// imports). Property defaults are ignored at registration time — they'll
    /// be type-checked in the THIR pass, mirroring how component signals are
    /// handled.
    fn register_global(&mut self, global: &ast::Global, span: Span) {
        let name = self.ctx.intern(&global.name);
        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Global(GlobalDef {
                def_id: DefId::INVALID,
                name,
                is_export: global.is_export,
                properties: vec![],
                property_directions: vec![],
                property_defaults: vec![],
                callbacks: vec![],
                package: None,
            }),
            span,
        );

        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Global, def_id) {
            self.report_duplicate(&global.name, span, existing);
        }

        // Properties.
        // The direction decides what crosses the host boundary. A property
        // with a default value *and* no explicit direction is inline shared
        // state — it emits no WIT, just a module-scoped reactive slot.
        // Without a default it still has no WIT, but the slot starts zero-ish.
        let mut prop_ids = Vec::new();
        let mut directions = Vec::new();
        let mut defaults: Vec<Option<crate::hir::HirExpr>> = Vec::new();
        let mut func_from_props = Vec::new();
        let mut field_idx = 0u32;
        for prop in global.properties.iter() {
            let prop_name = self.ctx.intern(&prop.node.name);
            let prop_ty = self.ctx.intern_ast_ty(&prop.node.ty.kind);

            // func-typed properties are functions, same as components
            if let InternedTyKind::Func { params: _, ret } = self.ctx.ty_kind(prop_ty) {
                let func_id = self.ctx.defs.alloc(
                    prop_name,
                    DefKind::Function(FunctionDef {
                        def_id: DefId::INVALID,
                        name: prop_name,
                        params: vec![],
                        ret_ty: ret.unwrap_or(Ty::UNIT),
                        is_export: false,
                    }),
                    prop.span,
                );
                if let DefKind::Function(f) = &mut self.ctx.defs.get_mut(func_id).kind {
                    f.def_id = func_id;
                }
                self.ctx.defs.set_type(func_id, prop_ty);
                func_from_props.push(func_id);
                continue;
            }

            let prop_id = self.ctx.defs.alloc(
                prop_name,
                DefKind::Field(FieldDef {
                    owner: def_id,
                    name: prop_name,
                    ty: prop_ty,
                    idx: FieldIdx::new(field_idx),
                }),
                prop.span,
            );
            self.ctx.defs.set_type(prop_id, prop_ty);
            prop_ids.push(prop_id);
            field_idx += 1;

            let direction = match prop.node.direction {
                PropertyDirection::InOut => GlobalPropDirection::InOut,
                PropertyDirection::Out => GlobalPropDirection::Out,
                PropertyDirection::In => {
                    if prop.node.default.is_some() {
                        GlobalPropDirection::Inline
                    } else {
                        GlobalPropDirection::In
                    }
                }
                PropertyDirection::Private => GlobalPropDirection::Inline,
            };
            directions.push(direction);

            let default_hir = prop
                .node
                .default
                .as_ref()
                .map(|d| self.lower_expr(&d.node, d.span));
            defaults.push(default_hir);
        }

        // Explicit callback declarations + func-typed properties
        let mut callback_ids: Vec<DefId> = global
            .callbacks
            .iter()
            .map(|cb| self.lower_global_fn_sig(def_id, &cb.node, cb.span, /* is_export */ false))
            .collect();
        callback_ids.extend(func_from_props);

        if let Some(g) = self.ctx.defs.as_global_mut(def_id) {
            g.def_id = def_id;
            g.properties = prop_ids;
            g.property_directions = directions;
            g.property_defaults = defaults;
            g.callbacks = callback_ids;
        }
    }

    /// Allocate a `Function` def for a global callback/public-func signature.
    fn lower_global_fn_sig(
        &mut self,
        _owner: DefId,
        decl: &ast::FunctionDecl,
        span: Span,
        is_export: bool,
    ) -> DefId {
        let fn_name = self.ctx.intern(&decl.name);
        let param_ids: Vec<DefId> = decl
            .params
            .iter()
            .enumerate()
            .map(|(idx, (pname, pty))| {
                let interned = self.ctx.intern(pname);
                let ty = self.ctx.intern_ast_ty(&pty.kind);
                let pid = self.ctx.defs.alloc(
                    interned,
                    DefKind::Parameter(ParameterDef {
                        owner: DefId::INVALID,
                        name: interned,
                        ty,
                        idx: crate::ids::ParamIdx::new(idx as u32),
                    }),
                    span,
                );
                self.ctx.defs.set_type(pid, ty);
                pid
            })
            .collect();

        let ret_ty = decl
            .return_type
            .as_ref()
            .map(|ty| self.ctx.intern_ast_ty(&ty.kind))
            .unwrap_or(Ty::UNIT);

        let fn_id = self.ctx.defs.alloc(
            fn_name,
            DefKind::Function(FunctionDef {
                def_id: DefId::INVALID,
                name: fn_name,
                params: param_ids.clone(),
                ret_ty,
                is_export,
            }),
            span,
        );
        for pid in &param_ids {
            if let DefKind::Parameter(p) = &mut self.ctx.defs.get_mut(*pid).kind {
                p.owner = fn_id;
            }
        }
        if let Some(f) = self.ctx.defs.as_function_mut(fn_id) {
            f.def_id = fn_id;
        }
        fn_id
    }

    fn register_component(&mut self, component: &ast::Component, span: Span) {
        let name = self.ctx.intern(&component.name);

        // Scan the body for `@children` slots. Count > 1 is a diagnostic
        // (one slot per component for v1); count == 1 flips the component
        // into a container. Recurses through if/for so slots anywhere in
        // the tree are detected.
        let mut slot_spans: Vec<Span> = Vec::new();
        collect_children_slots(&component.body, &mut slot_spans);
        let has_children_slot = !slot_spans.is_empty();
        if slot_spans.len() > 1 {
            let first = slot_spans[0];
            for dup_span in slot_spans.iter().skip(1) {
                self.ctx.diagnostics.push(
                    Diagnostic::error(
                        "component already has a `@children` slot — only one is allowed"
                            .to_string(),
                    )
                    .with_span(*dup_span)
                    .with_code(ErrorCode::DuplicateChildrenSlot)
                    .with_note(format!(
                        "first slot declared at {}",
                        self.ctx
                            .source_map
                            .get(first.source)
                            .map(|s| format!("{}:{}", s.name(), s.line_col(first.start).0))
                            .unwrap_or_else(|| "<unknown>".to_string())
                    )),
                );
            }
        }

        let def_id = self.ctx.defs.alloc(
            name,
            DefKind::Component(ComponentDef {
                def_id: DefId::INVALID,
                name,
                properties: vec![],
                callbacks: vec![],
                is_export: component.is_export,
                has_children_slot,
            }),
            span,
        );

        if let Some(existing) = self.ctx.defs.register_name(name, Namespace::Component, def_id) {
            self.report_duplicate(&component.name, span, existing);
        }

        // Register properties as signals (data properties) or functions (callback properties)
        // Default expressions are lowered later in lower_component.
        let mut prop_ids = Vec::new();
        let mut callback_prop_ids = Vec::new();
        let mut signal_idx = 0u32;
        for prop in component.properties.iter() {
            let prop_name = self.ctx.intern(&prop.node.name);
            let prop_ty = self.ctx.intern_ast_ty(&prop.node.ty.kind);

            // Function-typed properties are callbacks (imported from host)
            if let InternedTyKind::Func { params: _, ret } = self.ctx.ty_kind(prop_ty) {
                // Create as DefKind::Function, not Signal
                let func_id = self.ctx.defs.alloc(
                    prop_name,
                    DefKind::Function(FunctionDef {
                        def_id: DefId::INVALID,
                        name: prop_name,
                        params: vec![], // Callback params are implicit in the type
                        ret_ty: ret.unwrap_or(Ty::UNIT),
                        is_export: true, // Callbacks are imported from host
                    }),
                    prop.span,
                );
                // Update function def_id
                if let DefKind::Function(f) = &mut self.ctx.defs.get_mut(func_id).kind {
                    f.def_id = func_id;
                }
                self.ctx.defs.set_type(func_id, prop_ty);
                callback_prop_ids.push(func_id);
            } else {
                // Regular data property - create as Signal
                let prop_id = self.ctx.defs.alloc(
                    prop_name,
                    DefKind::Signal(SignalDef {
                        owner: def_id,
                        name: prop_name,
                        ty: prop_ty,
                        idx: FieldIdx::new(signal_idx),
                        default: None, // Will be set in lower_component
                    }),
                    prop.span,
                );
                self.ctx.defs.set_type(prop_id, prop_ty);
                prop_ids.push(prop_id);
                signal_idx += 1;
            }
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
                        idx: crate::ids::ParamIdx::new(idx as u32),
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

        // Merge function-typed properties with explicit function callbacks
        callback_prop_ids.extend(func_ids);

        if let DefKind::Component(c) = &mut self.ctx.defs.get_mut(def_id).kind {
            c.def_id = def_id;
            c.properties = prop_ids;
            c.callbacks = callback_prop_ids;
        }

        let comp_ty = self.ctx.mk_adt(def_id);
        self.ctx.defs.set_type(def_id, comp_ty);
    }

    fn lower_component(&mut self, component: &ast::Component, span: Span) -> Option<HirComponent> {
        let name = self.ctx.intern(&component.name);
        let def_id = self.ctx.defs.lookup(name, Namespace::Component)?;

        // Reset local scope for this component
        self.locals = LocalScope::new();

        // Add properties (signals) to local scope (with their def_ids for signal tracking)
        let prop_ids = if let Some(comp_def) = self.ctx.defs.as_component(def_id) {
            let ids = comp_def.properties.clone();
            for &prop_id in &ids {
                let prop_name = self.ctx.defs.name(prop_id);
                let prop_ty = self.ctx.defs.type_of(prop_id).unwrap_or(Ty::ERROR);
                let prop_span = self.ctx.defs.span(prop_id);
                self.locals.define_with_def_id(prop_name, prop_ty, prop_span, Some(prop_id));
            }
            // Also add callbacks to local scope so they can be called
            for &cb_id in &comp_def.callbacks {
                let cb_name = self.ctx.defs.name(cb_id);
                let cb_ty = self.ctx.defs.type_of(cb_id).unwrap_or(Ty::ERROR);
                let cb_span = self.ctx.defs.span(cb_id);
                self.locals.define_with_def_id(cb_name, cb_ty, cb_span, Some(cb_id));
            }
            ids
        } else {
            vec![]
        };

        // Now lower default expressions for signal properties (local scope is ready)
        // Note: prop_ids only contains signals, not function-typed properties (callbacks)
        // So we need to match by name, not by position
        for ast_prop in component.properties.iter() {
            if let Some(default_ast) = &ast_prop.node.default {
                let prop_name = self.ctx.intern(&ast_prop.node.name);
                // Find the matching signal by name
                if let Some(&prop_id) = prop_ids.iter().find(|&&id| {
                    self.ctx.defs.name(id) == prop_name
                }) {
                    let default_expr = self.lower_expr(&default_ast.node, default_ast.span);
                    // Update the signal def with the lowered default expression
                    if let DefKind::Signal(signal_def) = &mut self.ctx.defs.get_mut(prop_id).kind {
                        signal_def.default = Some(default_expr);
                    }
                }
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
            ast::Node::Children => HirNodeKind::ChildrenSlot,
        };

        Some(HirNode::new(id, kind, span))
    }

    fn lower_element(&mut self, elem: &ast::ElementNode) -> HirNodeKind {
        // Merge AST bindings with the same name into HirBindings.
        // Tracked as a parallel (order Vec, lookup HashMap) pair so downstream
        // phases — THIR, LIR, effect-id assignment, DOT output — see a
        // deterministic order matching source order. A bare HashMap would
        // randomise effect IDs across runs and break snapshot comparisons.
        // (name_span, getter_expr, setter_body)
        type BindingEntry = (Span, Option<HirExpr>, Option<Vec<HirStatement>>);
        let mut binding_order: Vec<String> = Vec::new();
        let mut binding_map: HashMap<String, BindingEntry> = HashMap::new();

        for b in &elem.bindings {
            let name = b.node.name.clone();
            if !binding_map.contains_key(&name) {
                binding_order.push(name.clone());
            }
            let entry = binding_map
                .entry(name)
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
                PropModifier::Bind => {
                    // Two-way binding: `bind value: x` is equivalent to
                    // `value: x` + `set value: { }` — the getter publishes
                    // the signal to the DOM, and the empty setter enables
                    // auto-sync from DOM input events back into the signal.
                    entry.1 = Some(self.lower_expr(&b.node.value.node, b.node.value.span));
                    entry.2 = Some(Vec::new());
                }
            }
        }

        let bindings: Vec<HirBinding> = binding_order
            .into_iter()
            .map(|name| {
                let (name_span, value, setter) = binding_map.remove(&name).expect(
                    "binding_order and binding_map populated together; key must be present",
                );
                (name, name_span, value, setter)
            })
            .map(|(name, name_span, value, setter)| {
                // Error if setter is defined without a getter
                if setter.is_some() && value.is_none() {
                    self.ctx.diagnostics.error(
                        name_span,
                        ErrorCode::InvalidValueBinding,
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

        let mut handlers: Vec<HirHandler> = Vec::with_capacity(elem.handlers.len());
        for h in &elem.handlers {
            // A bound payload parameter (`drop: { payload -> … }`) is a
            // body-scoped local of type `string`, defined before the body
            // is lowered so its references resolve. The event fixes the
            // type (drop payload / dragenter media-type are both strings).
            // We store only (name, span); typeck re-defines the local to
            // produce the THIR `LocalId` with matching arena parity.
            let param = h.node.param.as_ref().map(|p| {
                let name = self.ctx.interner.intern(&p.node);
                (name, p.span)
            });
            if let Some((name, span)) = param {
                self.locals.push_scope();
                self.locals.define(name, crate::types::Ty::STRING, span);
            }
            let body: Vec<HirStatement> = h
                .node
                .body
                .iter()
                .map(|s| self.lower_statement(&s.node, s.span))
                .collect();
            if param.is_some() {
                self.locals.pop_scope();
            }
            handlers.push(HirHandler {
                name: h.node.name.clone(),
                name_span: h.node.name_span,
                param,
                body,
            });
        }

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

            ast::Expr::Literal(ast::Literal::Color(hex)) => {
                // Color hex literals desugar to a `Color.rgba((r, g, b, a))`
                // variant constructor call so the rest of the pipeline only
                // has to deal with variants — no dedicated primitive repr.
                let (r, g, b, a) = parse_color_hex(hex);
                let mk_byte = |v: u8| HirExpr::new(
                    HirExprKind::Literal(HirLiteral::Int(v as i64)),
                    span,
                );
                let payload_tuple = HirExpr::new(
                    HirExprKind::Literal(HirLiteral::Tuple(vec![
                        mk_byte(r),
                        mk_byte(g),
                        mk_byte(b),
                        mk_byte(a),
                    ])),
                    span,
                );
                HirExprKind::PathCall {
                    base: "Color".to_string(),
                    member: "rgba".to_string(),
                    args: vec![payload_tuple],
                }
            }

            ast::Expr::Literal(lit) => HirExprKind::Literal(self.lower_literal(lit)),

            ast::Expr::Binary(lhs, op, rhs) => {
                let bin_op = BinOp::parse(op).unwrap_or(BinOp::Add);
                HirExprKind::Binary {
                    op: bin_op,
                    lhs: Box::new(self.lower_expr(&lhs.node, lhs.span)),
                    rhs: Box::new(self.lower_expr(&rhs.node, rhs.span)),
                }
            }

            ast::Expr::Unary(op, operand) => {
                let unary_op = UnaryOp::parse(op).unwrap_or(UnaryOp::Neg);
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
                // Handle Type.case(args) for variant constructors OR method calls
                if let ast::Expr::Ident(type_name) = &base.node {
                    // Check if this is a known type (for variant constructors)
                    let interned = self.ctx.intern(type_name);
                    let is_type = self.ctx.defs.lookup(interned, Namespace::Type).is_some();
                    let is_global = self.ctx.defs.lookup(interned, Namespace::Global).is_some();
                    if is_type || is_global {
                        // Type.case(args) or Global.method(args) — both
                        // resolved downstream via the two-segment path. typeck
                        // dispatches on the first segment's DefKind.
                        HirExprKind::PathCall {
                            base: type_name.clone(),
                            member: member.clone(),
                            args: args
                                .iter()
                                .map(|a| self.lower_expr(&a.node, a.span))
                                .collect(),
                        }
                    } else {
                        // It's a variable - use MethodCall
                        HirExprKind::MethodCall {
                            receiver: Box::new(self.lower_expr(&base.node, base.span)),
                            method: member.clone(),
                            args: args
                                .iter()
                                .map(|a| self.lower_expr(&a.node, a.span))
                                .collect(),
                        }
                    }
                } else {
                    // Complex expression base - always use MethodCall
                    HirExprKind::MethodCall {
                        receiver: Box::new(self.lower_expr(&base.node, base.span)),
                        method: member.clone(),
                        args: args
                            .iter()
                            .map(|a| self.lower_expr(&a.node, a.span))
                            .collect(),
                    }
                }
            }

            ast::Expr::Member(base, field) => {
                // Check if base is a type name (enum, variant) or a global
                if let ast::Expr::Ident(name) = &base.node {
                    let interned = self.ctx.intern(name);
                    // Enum/variant case: Enum.case, Variant.ctor
                    if let Some(def_id) = self.ctx.defs.lookup(interned, Namespace::Type)
                        && (self.ctx.defs.as_enum(def_id).is_some()
                            || self.ctx.defs.as_variant(def_id).is_some())
                        {
                            return HirExpr::new(
                                HirExprKind::Path {
                                    segments: vec![name.clone(), field.clone()],
                                },
                                span,
                            );
                        }
                    // Global property read: MailStore.items
                    if self.ctx.defs.lookup(interned, Namespace::Global).is_some() {
                        return HirExpr::new(
                            HirExprKind::Path {
                                segments: vec![name.clone(), field.clone()],
                            },
                            span,
                        );
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
                // Push scope for closure parameters
                self.locals.push_scope();

                // Define params in the local scope so they can be resolved in the body
                let lowered_params: Vec<(String, Ty)> = params
                    .iter()
                    .map(|(name, ty)| {
                        let param_ty = self.ctx.intern_ast_ty(&ty.kind);
                        let name_interned = self.ctx.intern(name);
                        // Define the parameter as a local (the LocalId will be recalculated during type checking)
                        self.locals.define(name_interned, param_ty, span);
                        (name.clone(), param_ty)
                    })
                    .collect();

                let lowered_body: Vec<HirStatement> = body
                    .iter()
                    .map(|s| self.lower_statement(&s.node, s.span))
                    .collect();

                // Pop the closure scope
                self.locals.pop_scope();

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

            ast::Expr::MethodCall {
                receiver,
                method,
                method_span: _,
                args,
            } => HirExprKind::MethodCall {
                receiver: Box::new(self.lower_expr(&receiver.node, receiver.span)),
                method: method.clone(),
                args: args
                    .iter()
                    .map(|a| self.lower_expr(&a.node, a.span))
                    .collect(),
            },
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
            ast::Literal::Color(_) => {
                // Color literals are desugared to Color.rgba(...) in lower_expr;
                // lower_literal should never see them.
                unreachable!("Color literal reached lower_literal — must be desugared at lower_expr")
            }
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

            ast::Statement::Let {
                name,
                name_span,
                ty,
                value,
            } => {
                let lowered_value = self.lower_expr(&value.node, value.span);
                let lowered_ty = ty.as_ref().map(|t| self.ctx.intern_ast_ty(&t.kind));

                // Register the local so subsequent statements can reference it
                let name_interned = self.ctx.intern(name);
                let local_ty = lowered_ty.unwrap_or(Ty::ERROR); // Type will be refined during type checking
                self.locals.define(name_interned, local_ty, *name_span);

                HirStatement::Let {
                    name: name.clone(),
                    ty: lowered_ty,
                    value: lowered_value,
                }
            }
        }
    }
}
