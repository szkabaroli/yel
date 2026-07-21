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
use crate::diagnostic::{Diagnostic, ErrorCode};
use crate::hir::expr::{
    BinOp, HirExpr, HirExprKind, HirInterpolationPart, HirLiteral, HirStatement, UnaryOp,
};
use crate::hir::local_scope::LocalScope;
use crate::hir::node::{
    HirBinding, HirComponent, HirGlobal, HirHandler, HirItem, HirNode, HirNodeKind,
};
use crate::ids::{DefId, ExprId, FieldIdx, NodeId, VariantIdx};
use crate::interner::Name;
use crate::source::Span;
use crate::types::{InternedTyKind, Ty};

use super::expr::{ThirExpr, ThirExprKind, ThirInterpolationPart, ThirStatement};
use super::node::{ThirBinding, ThirComponent, ThirHandler, ThirItem, ThirNode, ThirNodeKind};

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

/// Type check one top-level HIR item, producing THIR.
///
/// The single type-checking entry for both components and globals: it
/// dispatches on the item kind, runs signal-dependency analysis, and
/// stashes the result in the context side table (keyed by the item's
/// `DefId`). There is deliberately no separate globals pass — the driver
/// runs one item list through here.
pub fn type_check(item: &HirItem, ctx: &mut CompilerContext) -> ThirItem {
    match item {
        HirItem::Component(component) => {
            let thir = {
                let mut checker = TypeChecker::new(ctx);
                checker.check_component(component)
            };
            let deps = {
                let is_signal = |d: DefId| ctx.defs.is_signal(d);
                super::signalck::check_component(&thir, &is_signal)
            };
            ctx.set_signal_deps(thir.def_id, deps);
            ThirItem::Component(thir)
        }
        HirItem::Global(global) => ThirItem::Global(type_check_global(global, ctx)),
    }
}

/// Type-check a single global declaration into a [`ThirGlobal`]. A global
/// is a host-boundary singleton with no UI body, so this only checks its
/// property-default expressions; the result carries the same
/// signal-defaults contract as a component so the same lowering drives it.
/// Private — globals reach type-checking only through [`type_check`].
fn type_check_global(global: &HirGlobal, ctx: &mut CompilerContext) -> super::node::ThirGlobal {
    use std::collections::HashMap;

    let gid = global.def_id;
    let (name, span, is_export, prop_ids, defaults) = match ctx.defs.as_global(gid) {
        Some(g) => (
            g.name,
            ctx.defs.span(gid),
            g.is_export,
            g.properties.clone(),
            g.property_defaults.clone(),
        ),
        // An item only exists for a registered global.
        None => unreachable!("HirItem::Global wraps non-global DefId {gid:?}"),
    };

    let mut signal_defaults: HashMap<DefId, ThirExpr> = HashMap::new();
    {
        let mut checker = TypeChecker::new(ctx);
        for (prop_id, default) in prop_ids.iter().copied().zip(defaults) {
            let Some(default_hir) = default else { continue };
            let prop_ty = checker
                .ctx
                .defs
                .type_of(prop_id)
                .unwrap_or(crate::types::Ty::ERROR);
            let thir = checker.type_check_expr(&default_hir, Mode::Check(prop_ty));
            signal_defaults.insert(prop_id, thir);
        }
    }

    let thir_global = super::node::ThirGlobal {
        def_id: gid,
        name,
        span,
        is_export,
        signals: prop_ids,
        signal_defaults,
    };
    let deps = {
        let is_signal = |d: DefId| ctx.defs.is_signal(d);
        super::signalck::check_global(&thir_global, &is_signal)
    };
    ctx.set_signal_deps(thir_global.def_id, deps);
    thir_global
}

/// Type check a component and get both result and type map.
pub fn type_check_with_map(component: &HirComponent, ctx: &mut CompilerContext) -> TypeCheckResult {
    let (thir, type_map) = {
        let mut checker = TypeChecker::new(ctx);
        let thir = checker.check_component(component);
        (thir, checker.type_map)
    };
    let deps = {
        let is_signal = |d: DefId| ctx.defs.is_signal(d);
        super::signalck::check_component(&thir, &is_signal)
    };
    ctx.set_signal_deps(thir.def_id, deps);
    TypeCheckResult {
        component: thir,
        type_map,
    }
}

/// Inclusive `[min, max]` range (widened to `i128`) an integer literal must
/// satisfy for the given target type, plus the type's display name. Returns
/// `None` for non-integer targets (e.g. floats), where an int literal is
/// coerced and no bounds check applies.
fn int_literal_bounds(kind: &InternedTyKind) -> Option<(i128, i128, &'static str)> {
    Some(match kind {
        InternedTyKind::S8 => (i128::from(i8::MIN), i128::from(i8::MAX), "s8"),
        InternedTyKind::S16 => (i128::from(i16::MIN), i128::from(i16::MAX), "s16"),
        InternedTyKind::S32 => (i128::from(i32::MIN), i128::from(i32::MAX), "s32"),
        InternedTyKind::S64 => (i128::from(i64::MIN), i128::from(i64::MAX), "s64"),
        InternedTyKind::U8 => (0, i128::from(u8::MAX), "u8"),
        InternedTyKind::U16 => (0, i128::from(u16::MAX), "u16"),
        InternedTyKind::U32 => (0, i128::from(u32::MAX), "u32"),
        InternedTyKind::U64 => (0, i128::from(u64::MAX), "u64"),
        _ => return None,
    })
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
        use std::collections::HashMap;

        self.current_component = component.def_id;
        self.locals = LocalScope::new();

        // Store type-checked signal defaults
        let mut signal_defaults: HashMap<DefId, ThirExpr> = HashMap::new();

        // Phase 1: Add ALL component properties to local scope first
        // This matches HIR lowering order, ensuring LocalIds are consistent
        let prop_ids = if let Some(comp_def) = self.ctx.defs.as_component(component.def_id) {
            let ids = comp_def.properties.clone();
            for &prop_id in &ids {
                let prop_name = self.ctx.defs.name(prop_id);
                let prop_ty = self.ctx.defs.type_of(prop_id).unwrap_or(Ty::ERROR);
                let prop_span = self.ctx.defs.span(prop_id);
                self.locals
                    .define_with_def_id(prop_name, prop_ty, prop_span, Some(prop_id));
            }
            // Also add callbacks to local scope so they can be called
            // This matches HIR lowering which also adds callbacks to locals
            for &cb_id in &comp_def.callbacks {
                let cb_name = self.ctx.defs.name(cb_id);
                let cb_ty = self.ctx.defs.type_of(cb_id).unwrap_or(Ty::ERROR);
                let cb_span = self.ctx.defs.span(cb_id);
                self.locals
                    .define_with_def_id(cb_name, cb_ty, cb_span, Some(cb_id));
            }
            ids
        } else {
            vec![]
        };

        // Phase 2: Type check default values (after all properties are in scope)
        for &prop_id in &prop_ids {
            let prop_ty = self.ctx.defs.type_of(prop_id).unwrap_or(Ty::ERROR);
            if let Some(signal_def) = self.ctx.defs.as_signal(prop_id)
                && let Some(ref default_expr) = signal_def.default.clone() {
                    let thir_default = self.type_check_expr(default_expr, Mode::Check(prop_ty));
                    signal_defaults.insert(prop_id, thir_default);
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
            signal_defaults,
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
                let component_def = if name.chars().next().is_some_and(|c| c.is_uppercase()) {
                    let name_interned = self.ctx.intern(name);
                    self.ctx.defs.lookup(name_interned, Namespace::Component)
                } else {
                    None
                };

                // Check for recursive component instantiation (component using itself)
                if let Some(def_id) = component_def
                    && def_id == self.current_component
                        && !self.ctx.known.elements.is_builtin(def_id)
                    {
                        self.ctx.diagnostics.push(
                            Diagnostic::error(format!(
                                "recursive component instantiation: '{}' cannot use itself",
                                name
                            ))
                            .with_span(node.span)
                            .with_code(ErrorCode::RecursiveInstantiation),
                        );
                    }

                let thir_bindings: Vec<ThirBinding> = bindings
                    .iter()
                    .map(|b| self.check_binding(b, component_def))
                    .collect();
                // Validate `set value:` setters here, where diagnostics are
                // available, so LIR lowering can treat them as invariants.
                self.check_value_binding_setters(name, &thir_bindings, node.span);
                let thir_handlers = handlers.iter().map(|h| self.check_handler(h)).collect();
                let thir_children = children.iter().map(|n| self.check_node(n)).collect();

                // Container-component contract: if the caller passes child
                // nodes and the target is a user-defined component, it must
                // declare a `@children` slot. Built-in elements (Text,
                // VStack, Button, …) and non-component names route through
                // the DOM child-propagation path unchanged.
                if let Some(def_id) = component_def
                    && !children.is_empty() && !self.ctx.known.elements.is_builtin(def_id) {
                        let accepts = match self.ctx.defs.kind(def_id) {
                            DefKind::Component(c) => c.has_children_slot,
                            DefKind::ImportComponent(ic) => ic.has_children_slot,
                            _ => false,
                        };
                        if !accepts {
                            self.ctx.diagnostics.push(
                                Diagnostic::error(format!(
                                    "component `{}` does not declare `@children`; \
                                     cannot accept caller-supplied child nodes",
                                    name
                                ))
                                .with_span(node.span)
                                .with_code(ErrorCode::MissingChildrenSlot)
                                .with_note(
                                    "add `@children` inside the component body to make it a container"
                                        .to_string(),
                                ),
                            );
                        }
                    }

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
                // Push scope and define the loop variable BEFORE lowering the
                // iterable so LocalId allocation matches HIR lowering order
                // (HIR defines `item` before lowering the iterable; any
                // closures inside the iterable will reference LocalIds that
                // assume `item` has already been defined here).
                self.locals.push_scope();
                let new_item = self.locals.define(*item_name, Ty::ERROR, *item_span);

                // Infer iterable type and extract element type
                let thir_iterable = self.type_check_expr(iterable, Mode::Infer);

                let item_ty = match self.ctx.ty_kind(thir_iterable.ty) {
                    InternedTyKind::List(elem) => *elem,
                    _ => {
                        self.ctx.diagnostics.error(
                            iterable.span,
                            ErrorCode::TypeMismatch,
                            format!(
                                "for loop requires a list type, found `{}`",
                                self.type_to_string(thir_iterable.ty)
                            ),
                        );
                        Ty::ERROR
                    }
                };

                // Patch the loop variable's type now that we know the element type.
                self.locals.set_ty(new_item, item_ty);

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
            HirNodeKind::ChildrenSlot => {
                // No validation at this node — the `@children` marker is
                // meaningful only inside a component body. HIR registration
                // is where we bump `ComponentDef::has_children_slot`.
                // Duplicate-slot detection + "children passed to slot-less
                // component" diagnostics land in Phase 2.
                ThirNodeKind::ChildrenSlot
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
        if let (Some(getter), Some(setter)) = (&thir_value, &thir_setter) {
            self.check_setter_overwrites_getter(&binding.name, binding.name_span, getter, setter);
        }

        let prop_def = component.and_then(|comp_def| {
            let prop_name = self.ctx.intern(&binding.name);
            self.ctx
                .defs
                .find_field(comp_def, prop_name)
                .map(|(_, id)| id)
        });

        ThirBinding {
            name: binding.name.clone(),
            name_span: binding.name_span,
            prop_def,
            value: thir_value,
            setter: thir_setter,
        }
    }

    /// Validate `set value:` binding setters. LIR lowering can only express a
    /// value-binding setter on an Input-family element whose getter is a bare
    /// signal reference, and previously enforced this with `panic!`s. Checking
    /// it here — where diagnostics are available — turns those user errors into
    /// proper diagnostics and lets the LIR layer treat the constraints as
    /// already-validated invariants.
    fn check_value_binding_setters(&mut self, tag: &str, bindings: &[ThirBinding], span: Span) {
        for binding in bindings {
            // Only setters (`set <prop>: { ... }`) are constrained.
            if binding.setter.is_none() {
                continue;
            }
            if binding.name != "value" {
                self.ctx.diagnostics.push(
                    Diagnostic::error(format!(
                        "unsupported binding setter `set {}:` — only `set value:` is implemented",
                        binding.name
                    ))
                    .with_span(binding.name_span)
                    .with_code(ErrorCode::InvalidValueBinding),
                );
                continue;
            }
            if !matches!(tag, "TextInput" | "IntegerInput" | "FloatInput") {
                self.ctx.diagnostics.push(
                    Diagnostic::error(format!(
                        "`set value:` is only supported on `TextInput`, `IntegerInput`, or \
                         `FloatInput`, not `{tag}`"
                    ))
                    .with_span(span)
                    .with_code(ErrorCode::InvalidValueBinding),
                );
                continue;
            }
            // The getter is the write-back target, so it must be a bare signal
            // reference (`value: <signal>`), not a computed expression.
            let getter_is_signal = match binding.value.as_ref().map(|g| &g.kind) {
                Some(ThirExprKind::Def(def_id)) => self.ctx.defs.is_signal(*def_id),
                Some(ThirExprKind::Local(local_id)) => {
                    let def_id = self.locals.get(*local_id).def_id;
                    def_id.is_some_and(|d| self.ctx.defs.is_signal(d))
                }
                _ => false,
            };
            if !getter_is_signal {
                self.ctx.diagnostics.push(
                    Diagnostic::error(
                        "`set value:` requires a companion getter `value: <signal>` that is a \
                         bare signal reference"
                            .to_string(),
                    )
                    .with_span(binding.name_span)
                    .with_code(ErrorCode::InvalidValueBinding),
                );
            }
        }
    }

    fn check_handler(&mut self, handler: &HirHandler) -> ThirHandler {
        // Mirror HIR's payload-param define (same order → LocalId parity, so
        // `Local` references lowered in HIR resolve against this arena). The
        // event fixes the type to `string`.
        let param = handler.param.map(|(name, span)| {
            self.locals.push_scope();
            self.locals.define(name, Ty::STRING, span)
        });
        let thir_body = handler.body.iter().map(|s| self.check_stmt(s)).collect();
        if param.is_some() {
            self.locals.pop_scope();
        }

        ThirHandler {
            name: handler.name.clone(),
            name_span: handler.name_span,
            param,
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

            HirStatement::Let { name, ty, value } => {
                // Determine the type: use explicit annotation if provided, otherwise infer
                let mode = if let Some(expected_ty) = ty {
                    Mode::Check(*expected_ty)
                } else {
                    Mode::Infer
                };
                let thir_value = self.type_check_expr(value, mode);
                let actual_ty = ty.unwrap_or(thir_value.ty);

                // Create a new local for this let binding and add to scope
                let name_interned = self.ctx.intern(name);
                let local_id = self
                    .locals
                    .define(name_interned, actual_ty, thir_value.span);

                ThirStatement::Let {
                    local_id,
                    name: name.clone(),
                    ty: actual_ty,
                    value: thir_value,
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
                (
                    ThirExprKind::ListLiteral {
                        elements: thir_elements,
                        element_ty: elem_ty,
                    },
                    expected,
                )
            }

            // Tuple checked against tuple type - propagate element types
            (
                HirExprKind::Literal(HirLiteral::Tuple(elements)),
                InternedTyKind::Tuple(elem_tys),
            ) => {
                let elem_tys = elem_tys.clone();
                let thir_elements: Vec<_> = elements
                    .iter()
                    .zip(elem_tys.iter())
                    .map(|(e, ty)| self.type_check_expr(e, Mode::Check(*ty)))
                    .collect();
                (
                    ThirExprKind::TupleLiteral {
                        elements: thir_elements,
                    },
                    expected,
                )
            }

            // Record checked against ADT type - match fields
            (
                HirExprKind::Literal(HirLiteral::Record { fields }),
                InternedTyKind::Adt(record_def),
            ) => {
                if let Some(record) = self.ctx.defs.as_record(*record_def) {
                    // Reorder fields to match definition order
                    let record_fields = record.fields.clone();
                    let mut thir_fields = Vec::new();
                    for &field_def_id in &record_fields {
                        let field = self.ctx.defs.as_field(field_def_id);
                        if let Some(field) = field {
                            let field_name = self.ctx.str(field.name);
                            // Find the corresponding field in the literal
                            if let Some((_, expr)) =
                                fields.iter().find(|(name, _)| name.as_str() == &*field_name)
                            {
                                let thir_expr = self.type_check_expr(expr, Mode::Check(field.ty));
                                thir_fields.push(thir_expr);
                            } else {
                                // Field not provided in literal
                                self.ctx.diagnostics.error(
                                    span,
                                    ErrorCode::MissingField,
                                    format!("missing field `{}` in record literal", field_name),
                                );
                                thir_fields.push(ThirExpr::error(self.fresh_expr_id(), span));
                            }
                        }
                    }
                    (
                        ThirExprKind::RecordLiteral {
                            record_def: *record_def,
                            fields: thir_fields,
                        },
                        expected,
                    )
                } else {
                    // The expected ADT isn't a record — report instead of
                    // poisoning silently.
                    self.ctx
                        .diagnostics
                        .error(span, ErrorCode::NotARecord, "record literal requires a record type");
                    (ThirExprKind::Error, Ty::ERROR)
                }
            }

            // Integer literal - polymorphic over all numeric types (int and
            // float). A bare `2` in f32 context is valid (matches Rust /
            // Swift / Go semantics for untyped numeric literals). LIR
            // lowering handles the int→float cast when the target is f32/f64.
            // This applies only to literals; variables of a different
            // numeric type still require an explicit conversion (enforced
            // by the mixed-numeric-types diagnostic in the Infer Binary arm).
            (HirExprKind::Literal(lit @ HirLiteral::Int(v)), _)
                if self.is_numeric_type(expected) =>
            {
                // The literal is polymorphic, but the target width is now
                // known: reject values that don't fit before LIR lowering
                // narrows them with an unchecked `as` cast (e.g. `count: u8
                // = 300` would otherwise silently become 44).
                if let Some((min, max, name)) = int_literal_bounds(&expected_kind) {
                    let val = i128::from(*v);
                    if val < min || val > max {
                        self.ctx.diagnostics.error(
                            span,
                            ErrorCode::IntLiteralOutOfRange,
                            format!(
                                "integer literal `{v}` is out of range for type `{name}` (valid range {min}..={max})"
                            ),
                        );
                    }
                }
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
            (
                HirExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand,
                },
                _,
            ) if self.is_numeric_type(expected) => {
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
                        ErrorCode::TypeMismatch,
                        format!(
                            "type mismatch: expected `{}`, found `{}`",
                            self.type_to_string(expected),
                            self.type_to_string(inferred)
                        ),
                    );
                }
                (kind, expected)
            }

            // Closure checked against function type - infer parameter types
            (
                HirExprKind::Closure { params, body },
                InternedTyKind::Func {
                    params: expected_params,
                    ret: _,
                },
            ) => {
                let expected_params = expected_params.clone();

                self.locals.push_scope();

                let mut thir_params = Vec::new();
                for (i, (name, ty)) in params.iter().enumerate() {
                    let name_interned = self.ctx.intern(name);
                    // Use expected param type if available, otherwise use declared type
                    let param_ty = if matches!(self.ctx.ty_kind(*ty), InternedTyKind::Unknown) {
                        // Infer from expected type
                        expected_params.get(i).copied().unwrap_or(Ty::ERROR)
                    } else {
                        *ty
                    };
                    let local_id = self.locals.define(name_interned, param_ty, span);
                    thir_params.push((local_id, param_ty));
                }

                let thir_body: Vec<_> = body.iter().map(|s| self.check_stmt(s)).collect();

                self.locals.pop_scope();

                // TODO: capture analysis
                let captures = vec![];

                (
                    ThirExprKind::Closure(Box::new(super::expr::ThirClosure {
                        params: thir_params,
                        body: thir_body,
                        captures,
                    })),
                    expected,
                )
            }

            // Default: infer then check compatibility
            _ => {
                let (kind, inferred) = self.infer_expr(expr);
                if !self.types_compatible(inferred, expected) {
                    self.ctx.diagnostics.error(
                        span,
                        ErrorCode::TypeMismatch,
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
                        let elem_ty = match thir_elements.first() {
                            Some(e) => e.ty,
                            None => {
                                // Don't silently poison: an empty list with no
                                // expected type can't be inferred — say so.
                                self.ctx.diagnostics.error(
                                    expr.span,
                                    ErrorCode::CannotInferType,
                                    "cannot infer the element type of an empty list; add a type annotation",
                                );
                                Ty::ERROR
                            }
                        };
                        let list_ty = self.ctx.mk_list(elem_ty);
                        (
                            ThirExprKind::ListLiteral {
                                elements: thir_elements,
                                element_ty: elem_ty,
                            },
                            list_ty,
                        )
                    }
                    HirLiteral::Tuple(elements) => {
                        let thir_elements: Vec<_> = elements
                            .iter()
                            .map(|e| self.type_check_expr(e, Mode::Infer))
                            .collect();
                        let elem_tys: Vec<_> = thir_elements.iter().map(|e| e.ty).collect();
                        let tuple_ty = self.ctx.types.intern(InternedTyKind::Tuple(elem_tys));
                        (
                            ThirExprKind::TupleLiteral {
                                elements: thir_elements,
                            },
                            tuple_ty,
                        )
                    }
                    HirLiteral::Record { fields: _ } => {
                        // Anonymous record - needs expected type context
                        self.ctx.diagnostics.error(
                            expr.span,
                            ErrorCode::CannotInferType,
                            "cannot infer type of anonymous record literal; add type annotation",
                        );
                        (ThirExprKind::Error, Ty::ERROR)
                    }
                    _ => {
                        let ty = self.infer_literal_type(lit, expr.span);
                        (ThirExprKind::Literal(lit.clone()), ty)
                    }
                }
            }

            HirExprKind::Binary { op, lhs, rhs } => {
                // Untyped integer / float literals are polymorphic — they
                // can take any numeric type their context needs. In
                // inference mode, if one operand is a bare numeric
                // literal and the other resolves to a concrete numeric
                // type, re-check the literal against that type so
                // `i == 0` typechecks cleanly when `i: u32`. Without
                // this, every `u32 == integer-literal` (or modulo, etc.)
                // would hit the mixed-numeric diagnostic below even
                // though the user wrote code whose intent is obvious.
                let lhs_is_num_lit = matches!(
                    &lhs.kind,
                    HirExprKind::Literal(HirLiteral::Int(_))
                        | HirExprKind::Literal(HirLiteral::Float(_))
                );
                let rhs_is_num_lit = matches!(
                    &rhs.kind,
                    HirExprKind::Literal(HirLiteral::Int(_))
                        | HirExprKind::Literal(HirLiteral::Float(_))
                );

                let (lhs_thir, rhs_thir) = match (lhs_is_num_lit, rhs_is_num_lit) {
                    (true, false) => {
                        let rhs_thir = self.type_check_expr(rhs, Mode::Infer);
                        let lhs_thir = if self.is_numeric_type(rhs_thir.ty) {
                            self.type_check_expr(lhs, Mode::Check(rhs_thir.ty))
                        } else {
                            self.type_check_expr(lhs, Mode::Infer)
                        };
                        (lhs_thir, rhs_thir)
                    }
                    (false, true) => {
                        let lhs_thir = self.type_check_expr(lhs, Mode::Infer);
                        let rhs_thir = if self.is_numeric_type(lhs_thir.ty) {
                            self.type_check_expr(rhs, Mode::Check(lhs_thir.ty))
                        } else {
                            self.type_check_expr(rhs, Mode::Infer)
                        };
                        (lhs_thir, rhs_thir)
                    }
                    _ => {
                        let lhs_thir = self.type_check_expr(lhs, Mode::Infer);
                        let rhs_thir = self.type_check_expr(rhs, Mode::Infer);
                        (lhs_thir, rhs_thir)
                    }
                };

                // After literal polymorphism, reject any remaining mixed
                // numeric types. Non-literal `u32 == s32` still needs
                // an explicit conversion — matches Rust / Swift / Go
                // semantics exactly: literals are polymorphic, variables
                // are not.
                if lhs_thir.ty != Ty::ERROR
                    && rhs_thir.ty != Ty::ERROR
                    && self.is_numeric_type(lhs_thir.ty)
                    && self.is_numeric_type(rhs_thir.ty)
                    && lhs_thir.ty != rhs_thir.ty
                {
                    self.ctx.diagnostics.error(
                        expr.span,
                        ErrorCode::TypeMismatch,
                        format!(
                            "mixed numeric types in binary `{:?}`: `{}` and `{}` \
                             have no implicit coercion — convert one side explicitly",
                            op,
                            self.type_to_string(lhs_thir.ty),
                            self.type_to_string(rhs_thir.ty),
                        ),
                    );
                }

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
                let (field_ty, field_idx, field_def) =
                    self.resolve_field(base_thir.ty, field, expr.span);

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
                            ErrorCode::TypeMismatch,
                            format!(
                                "optional chaining requires option type, found `{}`",
                                self.type_to_string(base_thir.ty)
                            ),
                        );
                        Ty::ERROR
                    }
                };

                let (field_ty, field_idx, field_def) =
                    self.resolve_field(inner_ty, field, expr.span);

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
                            ErrorCode::TypeMismatch,
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
                if func_def.is_none()
                    && let Some(comp) = self.ctx.defs.as_component(self.current_component) {
                        for &callback_id in &comp.callbacks.clone() {
                            if self.ctx.defs.name(callback_id) == func_name {
                                func_def = Some(callback_id);
                                break;
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

                // Check if it's a local variable of function type (e.g., callback property)
                if let Some(local_id) = self.locals.lookup(func_name) {
                    let local_info = self.locals.get(local_id);
                    let local_ty = local_info.ty;

                    if let InternedTyKind::Func { params, ret } = self.ctx.ty_kind(local_ty) {
                        // Properties have a DefId - use regular Call
                        if let Some(def_id) = local_info.def_id {
                            let param_tys = params.clone();
                            let ret_ty = ret.unwrap_or(Ty::UNIT);

                            let thir_args: Vec<_> = args
                                .iter()
                                .zip(param_tys.iter().chain(std::iter::repeat(&Ty::ERROR)))
                                .map(|(arg, &param_ty)| {
                                    self.type_check_expr(arg, Mode::Check(param_ty))
                                })
                                .collect();

                            return (
                                ThirExprKind::Call {
                                    func: def_id,
                                    args: thir_args,
                                },
                                ret_ty,
                            );
                        }
                        // Local without DefId (e.g., let binding of function type) - not supported
                    }
                }

                // Unknown function - emit error
                self.ctx
                    .diagnostics
                    .error(expr.span, ErrorCode::UnresolvedName, format!("unknown function `{}`", func));
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
                                self.ctx
                                    .diagnostics
                                    .error(expr.span, ErrorCode::WrongArgCount, "len() takes no arguments".to_string());
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
                                self.ctx
                                    .diagnostics
                                    .error(expr.span, ErrorCode::WrongArgCount, "len() takes no arguments".to_string());
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
                                    ErrorCode::WrongArgCount,
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
                            let option_ty =
                                self.ctx.types.intern(InternedTyKind::Option(element_ty));

                            return (
                                ThirExprKind::Call {
                                    func: list_get_func,
                                    args: vec![base_expr, index_expr],
                                },
                                option_ty,
                            );
                        }
                        _ => {
                            self.ctx
                                .diagnostics
                                .error(expr.span, ErrorCode::NoSuchMember, format!("unknown method `{}` on type", member));
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                    }
                }

                // Global function call: MailStore.mark-read(id)
                if let Some(global_id) = self.ctx.defs.lookup(base_name, Namespace::Global) {
                    if let Some(fn_id) = self.ctx.defs.find_global_function(global_id, member_name)
                    {
                        let (param_tys, ret_ty) =
                            if let Some(fdef) = self.ctx.defs.as_function(fn_id) {
                                let params: Vec<Ty> = fdef
                                    .params
                                    .iter()
                                    .map(|pid| self.ctx.defs.type_of(*pid).unwrap_or(Ty::ERROR))
                                    .collect();
                                (params, fdef.ret_ty)
                            } else {
                                (Vec::new(), Ty::ERROR)
                            };

                        if args.len() != param_tys.len() {
                            self.ctx.diagnostics.error(
                                expr.span,
                                ErrorCode::WrongArgCount,
                                format!(
                                    "`{}.{}` expects {} argument(s), found {}",
                                    base,
                                    member,
                                    param_tys.len(),
                                    args.len()
                                ),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }

                        let thir_args: Vec<ThirExpr> = args
                            .iter()
                            .zip(param_tys.iter())
                            .map(|(arg, &pty)| self.type_check_expr(arg, Mode::Check(pty)))
                            .collect();

                        return (
                            ThirExprKind::GlobalCall {
                                global: global_id,
                                function: fn_id,
                                args: thir_args,
                            },
                            ret_ty,
                        );
                    }
                    self.ctx.diagnostics.error(
                        expr.span,
                        ErrorCode::NoSuchMember,
                        format!("no function `{}` on global `{}`", member, base),
                    );
                    return (ThirExprKind::Error, Ty::ERROR);
                }

                // Not a variable - try type lookup for variant constructor
                if let Some(type_def) = self.ctx.defs.lookup(base_name, Namespace::Type) {
                    // Check if it's a variant
                    if let Some(variant) = self.ctx.defs.as_variant(type_def) {
                        // Find the case
                        for &case_def_id in &variant.cases.clone() {
                            if let DefKind::VariantCase(case) =
                                self.ctx.defs.kind(case_def_id).clone()
                                && case.name == member_name {
                                    // Found the case - build the variant constructor
                                    let payload = if let Some(payload_ty) = case.payload {
                                        if args.len() != 1 {
                                            self.ctx.diagnostics.error(
                                                expr.span,
                                                ErrorCode::WrongArgCount,
                                                format!("variant case `{}` expects 1 argument, found {}", member, args.len()),
                                            );
                                            return (ThirExprKind::Error, Ty::ERROR);
                                        }
                                        Some(Box::new(
                                            self.type_check_expr(&args[0], Mode::Check(payload_ty)),
                                        ))
                                    } else {
                                        if !args.is_empty() {
                                            self.ctx.diagnostics.error(
                                                expr.span,
                                                ErrorCode::WrongArgCount,
                                                format!(
                                                    "variant case `{}` takes no arguments",
                                                    member
                                                ),
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
                        // Case not found
                        self.ctx.diagnostics.error(
                            expr.span,
                            ErrorCode::NoSuchCase,
                            format!("variant `{}` has no case `{}`", base, member),
                        );
                    } else {
                        self.ctx
                            .diagnostics
                            .error(expr.span, ErrorCode::UnresolvedName, format!("`{}` is not a variant type", base));
                    }
                } else {
                    self.ctx
                        .diagnostics
                        .error(expr.span, ErrorCode::UnresolvedName, format!("unknown type or variable `{}`", base));
                }
                (ThirExprKind::Error, Ty::ERROR)
            }

            HirExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                // Infer range element type from whichever side isn't a
                // bare integer literal. Both sides literal → default to
                // s32. This lets `for i in 0..rows` typecheck when
                // `rows: u32` — the literal `0` widens to u32.
                let start_is_num_lit = matches!(
                    &start.kind,
                    HirExprKind::Literal(HirLiteral::Int(_))
                        | HirExprKind::Literal(HirLiteral::Float(_))
                );
                let end_is_num_lit = matches!(
                    &end.kind,
                    HirExprKind::Literal(HirLiteral::Int(_))
                        | HirExprKind::Literal(HirLiteral::Float(_))
                );
                let (start_thir, end_thir, elem_ty) = match (start_is_num_lit, end_is_num_lit) {
                    (true, false) => {
                        let end_thir = self.type_check_expr(end, Mode::Infer);
                        let elem_ty = if self.is_integer_type(end_thir.ty) {
                            end_thir.ty
                        } else {
                            Ty::S32
                        };
                        let start_thir = self.type_check_expr(start, Mode::Check(elem_ty));
                        (start_thir, end_thir, elem_ty)
                    }
                    (false, true) => {
                        let start_thir = self.type_check_expr(start, Mode::Infer);
                        let elem_ty = if self.is_integer_type(start_thir.ty) {
                            start_thir.ty
                        } else {
                            Ty::S32
                        };
                        let end_thir = self.type_check_expr(end, Mode::Check(elem_ty));
                        (start_thir, end_thir, elem_ty)
                    }
                    _ => {
                        let start_thir = self.type_check_expr(start, Mode::Check(Ty::S32));
                        let end_thir = self.type_check_expr(end, Mode::Check(Ty::S32));
                        (start_thir, end_thir, Ty::S32)
                    }
                };

                let list_ty = self.ctx.mk_list(elem_ty);

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

                // TODO: infer function type from params and body. Until that
                // lands, report rather than silently poisoning with Ty::ERROR.
                self.ctx.diagnostics.error(
                    expr.span,
                    ErrorCode::CannotInferType,
                    "cannot infer the type of this closure without an expected type",
                );
                let func_ty = Ty::ERROR;

                (
                    ThirExprKind::Closure(Box::new(super::expr::ThirClosure {
                        params: thir_params,
                        body: thir_body,
                        captures,
                    })),
                    func_ty,
                )
            }

            HirExprKind::Interpolation(parts) => {
                let thir_parts: Vec<_> = parts
                    .iter()
                    .map(|p| match p {
                        HirInterpolationPart::Literal(s) => {
                            ThirInterpolationPart::Literal(s.clone())
                        }
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
                // Path should be Type.case or Global.property format (2 segments)
                if segments.len() == 2 {
                    let type_name = &segments[0];
                    let case_name = &segments[1];
                    let type_name_interned = self.ctx.intern(type_name);
                    let case_name_interned = self.ctx.intern(case_name);

                    // Global property read: Global.property -> FieldRead
                    if let Some(global_id) =
                        self.ctx.defs.lookup(type_name_interned, Namespace::Global)
                    {
                        if let Some((field_idx, prop_id)) = self
                            .ctx
                            .defs
                            .find_global_property(global_id, case_name_interned)
                        {
                            let prop_ty = self.ctx.defs.type_of(prop_id).unwrap_or(Ty::ERROR);
                            // Reuse the existing Def reference shape — property
                            // reads at THIR level lookups the type by DefId.
                            return (
                                ThirExprKind::GlobalRead {
                                    global: global_id,
                                    field: field_idx,
                                    prop: prop_id,
                                },
                                prop_ty,
                            );
                        }
                        self.ctx.diagnostics.error(
                            expr.span,
                            ErrorCode::NoSuchMember,
                            format!("no property `{}` on global `{}`", case_name, type_name),
                        );
                        return (ThirExprKind::Error, Ty::ERROR);
                    }

                    if let Some(def_id) = self.ctx.defs.lookup(type_name_interned, Namespace::Type)
                    {
                        // Check if it's an enum
                        if let Some(enum_def) = self.ctx.defs.as_enum(def_id) {
                            for (idx, &case_def_id) in enum_def.cases.iter().enumerate() {
                                if self.ctx.defs.name(case_def_id) == case_name_interned {
                                    let enum_ty =
                                        self.ctx.defs.type_of(def_id).unwrap_or(Ty::ERROR);
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
                                ErrorCode::NoSuchCase,
                                format!("no case `{}` in enum `{}`", case_name, type_name),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }

                        // Check if it's a variant
                        if let Some(variant_def) = self.ctx.defs.as_variant(def_id) {
                            for (idx, &case_def_id) in variant_def.cases.iter().enumerate() {
                                if self.ctx.defs.name(case_def_id) == case_name_interned {
                                    let variant_ty =
                                        self.ctx.defs.type_of(def_id).unwrap_or(Ty::ERROR);
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
                                ErrorCode::NoSuchCase,
                                format!("no case `{}` in variant `{}`", case_name, type_name),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                    }

                    self.ctx
                        .diagnostics
                        .error(expr.span, ErrorCode::UnresolvedName, format!("unknown type `{}` in path", type_name));
                }
                (ThirExprKind::Error, Ty::ERROR)
            }

            HirExprKind::MethodCall {
                receiver,
                method,
                args,
            } => {
                // Type check the receiver
                let receiver_thir = self.type_check_expr(receiver, Mode::Infer);
                let receiver_ty = receiver_thir.ty;

                // Dispatch based on method name and receiver type
                let ty_kind = self.ctx.ty_kind(receiver_ty);
                match (method.as_str(), ty_kind) {
                    ("len", InternedTyKind::List(_)) | ("len", InternedTyKind::String) => {
                        // list.len() or string.len() -> s32
                        if !args.is_empty() {
                            self.ctx
                                .diagnostics
                                .error(expr.span, ErrorCode::WrongArgCount, "len() takes no arguments".to_string());
                        }
                        let len_func = self.ctx.known.functions.len();
                        (
                            ThirExprKind::Call {
                                func: len_func,
                                args: vec![receiver_thir],
                            },
                            Ty::S32,
                        )
                    }
                    ("get", InternedTyKind::List(element_ty)) => {
                        // list.get(idx) -> option<T>
                        let element_ty = *element_ty;
                        if args.len() != 1 {
                            self.ctx.diagnostics.error(
                                expr.span,
                                ErrorCode::WrongArgCount,
                                "get() takes exactly one argument (index)".to_string(),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                        let index_expr = self.type_check_expr(&args[0], Mode::Check(Ty::S32));
                        let list_get_func = self.ctx.known.functions.list_get();
                        let option_ty = self.ctx.types.intern(InternedTyKind::Option(element_ty));
                        (
                            ThirExprKind::Call {
                                func: list_get_func,
                                args: vec![receiver_thir, index_expr],
                            },
                            option_ty,
                        )
                    }
                    ("filter", InternedTyKind::List(element_ty)) => {
                        // list.filter({ p -> bool }) -> list<T>
                        let element_ty = *element_ty;
                        if args.len() != 1 {
                            self.ctx.diagnostics.error(
                                expr.span,
                                ErrorCode::WrongArgCount,
                                "filter() takes exactly one argument (predicate closure)"
                                    .to_string(),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                        // The closure should be: func(element_ty) -> bool
                        let closure_ty = self.ctx.types.intern(InternedTyKind::Func {
                            params: vec![element_ty],
                            ret: Some(Ty::BOOL),
                        });
                        let predicate_expr =
                            self.type_check_expr(&args[0], Mode::Check(closure_ty));
                        let filter_func = self.ctx.known.functions.filter();
                        let result_ty = self.ctx.mk_list(element_ty);
                        (
                            ThirExprKind::Call {
                                func: filter_func,
                                args: vec![receiver_thir, predicate_expr],
                            },
                            result_ty,
                        )
                    }
                    ("append", InternedTyKind::List(element_ty)) => {
                        // list.append(elem) -> list<T>
                        let element_ty = *element_ty;
                        if args.len() != 1 {
                            self.ctx.diagnostics.error(
                                expr.span,
                                ErrorCode::WrongArgCount,
                                "append() takes exactly one argument (element)".to_string(),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                        let elem_expr = self.type_check_expr(&args[0], Mode::Check(element_ty));
                        let append_func = self.ctx.known.functions.append();
                        let result_ty = self.ctx.mk_list(element_ty);
                        (
                            ThirExprKind::Call {
                                func: append_func,
                                args: vec![receiver_thir, elem_expr],
                            },
                            result_ty,
                        )
                    }
                    ("starts-with", InternedTyKind::String) => {
                        // string.starts-with(prefix) -> bool
                        if args.len() != 1 {
                            self.ctx.diagnostics.error(
                                expr.span,
                                ErrorCode::WrongArgCount,
                                "starts-with() takes exactly one argument (prefix)".to_string(),
                            );
                            return (ThirExprKind::Error, Ty::ERROR);
                        }
                        let prefix_expr = self.type_check_expr(&args[0], Mode::Check(Ty::STRING));
                        let starts_with_func = self.ctx.known.functions.starts_with();
                        (
                            ThirExprKind::Call {
                                func: starts_with_func,
                                args: vec![receiver_thir, prefix_expr],
                            },
                            Ty::BOOL,
                        )
                    }
                    _ => {
                        self.ctx.diagnostics.error(
                            expr.span,
                            ErrorCode::NoSuchMember,
                            format!(
                                "unknown method `{}` on type `{}`",
                                method,
                                self.type_to_string(receiver_ty)
                            ),
                        );
                        (ThirExprKind::Error, Ty::ERROR)
                    }
                }
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
        if func_def.is_none()
            && let Some(comp) = self.ctx.defs.as_component(self.current_component) {
                for &callback_id in &comp.callbacks.clone() {
                    if self.ctx.defs.name(callback_id) == func_name {
                        func_def = Some(callback_id);
                        break;
                    }
                }
            }

        if let Some(func_def) = func_def {
            let func_ty = self.ctx.defs.type_of(func_def).unwrap_or(Ty::ERROR);

            let (param_tys, ret_ty) = match self.ctx.ty_kind(func_ty) {
                InternedTyKind::Func { params, ret } => (params.clone(), ret.unwrap_or(Ty::UNIT)),
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
        self.ctx
            .diagnostics
            .error(span, ErrorCode::UnresolvedName, format!("unknown function `{}`", func));
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
            if let InternedTyKind::Adt(expected_def) = self.ctx.ty_kind(expected_ty)
                && let Some(variant) = self.ctx.defs.as_variant(*expected_def) {
                    for &case_def_id in &variant.cases.clone() {
                        if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id).clone()
                            && case.name == case_name {
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
            if let InternedTyKind::Result {
                ok: ok_ty,
                err: err_ty,
            } = self.ctx.ty_kind(expected_ty)
            {
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
                    if let DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id).clone()
                        && case.name == case_name && case.payload.is_some() {
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
                    ErrorCode::WrongArgCount,
                    format!(
                        "variant constructor expects 1 argument, found {}",
                        args.len()
                    ),
                );
                return Some((ThirExprKind::Error, Ty::ERROR));
            }
            Some(Box::new(
                self.type_check_expr(&args[0], Mode::Check(expected_payload_ty)),
            ))
        } else {
            if !args.is_empty() {
                self.ctx
                    .diagnostics
                    .error(span, ErrorCode::WrongArgCount, "variant case takes no arguments");
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
            self.ctx
                .diagnostics
                .error(span, ErrorCode::WrongArgCount, "some expects exactly 1 argument");
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
            self.ctx.diagnostics.error(span, ErrorCode::WrongArgCount, "none takes no arguments");
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
            self.ctx
                .diagnostics
                .error(span, ErrorCode::WrongArgCount, "ok expects exactly 1 argument");
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
            self.ctx
                .diagnostics
                .error(span, ErrorCode::WrongArgCount, "err expects exactly 1 argument");
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
                        .error(span, ErrorCode::WrongArgCount, "to-string expects exactly 1 argument");
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
                        .error(span, ErrorCode::WrongArgCount, format!("{} expects exactly 1 argument", name));
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
                    ErrorCode::NoSuchField,
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
                        self.ctx
                            .diagnostics
                            .error(span, ErrorCode::NoSuchField, format!("no field `{}` on string", field));
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
                        self.ctx
                            .diagnostics
                            .error(span, ErrorCode::NoSuchField, format!("no field `{}` on list", field));
                        (Ty::ERROR, FieldIdx::new(0), DefId::INVALID)
                    }
                }
            }

            _ => {
                self.ctx.diagnostics.error(
                    span,
                    ErrorCode::NoSuchField,
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

    fn infer_literal_type(&mut self, lit: &HirLiteral, span: Span) -> Ty {
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
                other => {
                    self.ctx
                        .diagnostics
                        .error(span, ErrorCode::UnknownUnitSuffix, format!("unknown unit suffix `{other}`"));
                    Ty::ERROR
                }
            },
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
        if matches!(self.ctx.ty_kind(actual), InternedTyKind::Unknown)
            || matches!(self.ctx.ty_kind(expected), InternedTyKind::Unknown)
        {
            return true;
        }

        match (self.ctx.ty_kind(actual), self.ctx.ty_kind(expected)) {
            // Integer widening
            (
                InternedTyKind::S8,
                InternedTyKind::S16 | InternedTyKind::S32 | InternedTyKind::S64,
            ) => true,
            (InternedTyKind::S16, InternedTyKind::S32 | InternedTyKind::S64) => true,
            (InternedTyKind::S32, InternedTyKind::S64) => true,
            (
                InternedTyKind::U8,
                InternedTyKind::U16 | InternedTyKind::U32 | InternedTyKind::U64,
            ) => true,
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
            InternedTyKind::S8
            | InternedTyKind::S16
            | InternedTyKind::S32
            | InternedTyKind::S64 => self.ctx.known.functions.s32_to_string(),
            InternedTyKind::U8
            | InternedTyKind::U16
            | InternedTyKind::U32
            | InternedTyKind::U64 => self.ctx.known.functions.u32_to_string(),
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
                self.ctx.str(name).to_string()
            }
            InternedTyKind::Func { params, ret } => {
                let param_strs: Vec<_> = params.iter().map(|p| self.type_to_string(*p)).collect();
                let ret_str = ret
                    .map(|r| self.type_to_string(r))
                    .unwrap_or_else(|| "()".to_string());
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
                let ok_str = ok
                    .map(|t| self.type_to_string(t))
                    .unwrap_or_else(|| "_".to_string());
                let err_str = err
                    .map(|t| self.type_to_string(t))
                    .unwrap_or_else(|| "_".to_string());
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

    // The signal read/write walkers live in `super::signalck` as one shared
    // implementation — see `check_setter_overwrites_getter` below. They used to
    // be hand-copied here and had silently diverged (this copy dropped
    // `Closure` / `GlobalCall` reads), so the lint missed signals read inside a
    // closure or global call.

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

        let is_signal = |d: DefId| self.ctx.defs.is_signal(d);
        super::signalck::collect_expr_reads(getter, &self.locals, &is_signal, &mut getter_reads);
        super::signalck::collect_stmt_writes(setter, &self.locals, &is_signal, &mut setter_writes);

        // Find signals that are both read by getter and written by setter
        for def_id in getter_reads.intersection(&setter_writes) {
            let signal_name = self.ctx.str(self.ctx.defs.name(*def_id));
            self.ctx.diagnostics.warning(
                binding_span,
                ErrorCode::SetterOverwritesGetter,
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
        matches!(
            self,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
        )
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

        let thir = compiler.type_check(&hir[0]).into_component().expect("component");
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
        let thir = compiler.type_check(&hir[0]).into_component().expect("component");

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
        let thir = compiler.type_check(&hir[0]).into_component().expect("component");

        assert_eq!(thir.body.len(), 1);
        assert!(!compiler.has_errors());
    }
}
