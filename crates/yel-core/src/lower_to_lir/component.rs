//! THIR to LIR lowering.
//!
//! This phase:
//! 1. Extracts reactive signals from component properties
//! 2. Identifies reactive bindings and creates effects
//! 3. Lowers expressions, statements, and nodes
//! 4. Separates static vs dynamic bindings
//! 5. Converts to block-based representation for codegen

use std::cell::RefCell;
use std::collections::HashMap;

use crate::context::CompilerContext;
use crate::definitions::DefKind;
use crate::hir::expr::HirLiteral;
use crate::ids::{DefId, LocalId, NodeId};
use crate::interner::Name;
use super::blocks::BlockLowering;
use crate::source::Span;
use crate::thir::{
    ThirBinding, ThirComponent, ThirExpr, ThirExprKind, ThirHandler, ThirInterpolationPart,
    ThirNode, ThirNodeKind, ThirStatement,
};
use crate::types::{InternedTyKind, Ty};

use crate::lir::expr::{LirExpr, LirExprKind, LirLiteral, LirStatement};
use crate::lir::layout::LirLayoutContext;

/// Convert a primitive HirLiteral to LirLiteral.
/// Panics for compound types (List, Tuple, Record) which should be handled as separate constructs.
fn lower_primitive_literal(lit: &HirLiteral, ty: Ty, ctx: &CompilerContext) -> LirLiteral {
    match lit {
        HirLiteral::Int(v) => {
            // Integer literals are polymorphic over all numeric types
            // (typeck authorises the coercion in the Check arm for
            // `HirLiteral::Int` + any numeric expected type). Emit as the
            // exact target representation so downstream codegen pushes the
            // right valtype; in particular, `y: f32 = 2` must lower to
            // `LirLiteral::F32(2.0)`, not `LirLiteral::S32(2)`.
            match ctx.ty_kind(ty) {
                InternedTyKind::S8 => LirLiteral::S8(*v as i8),
                InternedTyKind::S16 => LirLiteral::S16(*v as i16),
                InternedTyKind::S32 => LirLiteral::S32(*v as i32),
                InternedTyKind::S64 => LirLiteral::S64(*v),
                InternedTyKind::U8 => LirLiteral::U8(*v as u8),
                InternedTyKind::U16 => LirLiteral::U16(*v as u16),
                InternedTyKind::U32 => LirLiteral::U32(*v as u32),
                InternedTyKind::U64 => LirLiteral::U64(*v as u64),
                InternedTyKind::F32 => LirLiteral::F32(*v as f32),
                InternedTyKind::F64 => LirLiteral::F64(*v as f64),
                _ => LirLiteral::S32(*v as i32), // Default to s32
            }
        }
        HirLiteral::Float(v) => match ctx.ty_kind(ty) {
            InternedTyKind::F64 => LirLiteral::F64(*v),
            _ => LirLiteral::F32(*v as f32),
        },
        HirLiteral::String(s) => LirLiteral::String(s.clone()),
        HirLiteral::Char(c) => LirLiteral::Char(*c),
        HirLiteral::Bool(b) => LirLiteral::Bool(*b),
        HirLiteral::Unit(value, unit) => {
            // Normalize unit values to canonical representations (all f32).
            // Mirrors the conversion logic used in lir_rust codegen.
            let normalized = match unit.as_str() {
                "%" => (*value as f32) / 100.0,
                "deg" => (*value as f32) * std::f32::consts::PI / 180.0,
                "turn" => (*value as f32) * 2.0 * std::f32::consts::PI,
                "s" => (*value as f32) * 1000.0,
                _ => *value as f32,
            };
            LirLiteral::F32(normalized)
        }
        HirLiteral::List(_) | HirLiteral::Tuple(_) | HirLiteral::Record { .. } => {
            panic!("Compound literals should be handled as ListConstruct/TupleConstruct/RecordConstruct")
        }
    }
}
use crate::lir::node::{LirBinding, LirResource, LirHandler, LirNode, LirNodeKind};
use crate::lir::signal::{LirEffect, LirSignal, UpdateKind};

/// Internal tree-based representation used during lowering.
/// This is converted to block-based `LirResource` at the end.
pub(crate) struct TreeLirResource {
    pub def_id: DefId,
    pub name: Name,
    pub span: Span,
    pub is_export: bool,
    pub signals: Vec<LirSignal>,
    pub effects: Vec<LirEffect>,
    pub body: Vec<LirNode>,
}

/// Lower a THIR component to block-based LIR (ready for codegen).
pub fn lower_component(component: &ThirComponent, ctx: &CompilerContext) -> LirResource {
    let mut lowering = LirLowering::for_component(ctx, component.def_id, &component.locals);
    // First create tree-based representation
    let tree = lowering.lower_component_to_tree(component);
    let mut lowering = BlockLowering::new(ctx, &tree);
    lowering.lower_component(&tree)
}

/// Lower type-checked global property default expressions to LIR.
///
/// Global defaults are module-scoped (not attached to any single component),
/// so they use their own lowering pass. Returns a map from property DefId to
/// the LIR expression that should seed its backing memory slot at module start.
pub fn lower_globals(
    thir_defaults: &HashMap<DefId, crate::thir::ThirExpr>,
    ctx: &CompilerContext,
) -> HashMap<DefId, LirExpr> {
    let empty_locals = crate::hir::local_scope::LocalScope::new();
    let lowering = LirLowering::for_module(ctx, &empty_locals);
    thir_defaults
        .iter()
        .map(|(def_id, thir_expr)| (*def_id, lowering.lower_expr(thir_expr)))
        .collect()
}

/// Scope for LIR lowering: either a specific component or module-scoped
/// (for global-singleton defaults and other module-level artifacts).
///
/// Kept local to this module — callers use the `for_component` /
/// `for_module` constructors rather than constructing this directly.
enum LoweringScope {
    /// Lowering an expression inside a component. The `DefId` anchors signal
    /// resolution: the component's own properties are treated as SignalReads.
    Component(DefId),
    /// Lowering a module-scope expression (e.g. a global property default).
    /// No component-local signals are in scope; only global properties are.
    Module,
}

/// LIR lowering state.
struct LirLowering<'ctx, 'comp> {
    ctx: &'ctx CompilerContext,
    /// Scope being lowered.
    scope: LoweringScope,
    /// Next effect ID to assign.
    next_effect_id: u32,
    /// Collected effects.
    effects: Vec<LirEffect>,
    /// Signal DefIds visible in this scope (for dependency tracking).
    /// Module scope: every global property. Component scope: component's own
    /// properties + every global property.
    signal_def_ids: Vec<DefId>,
    /// Reference to locals (for resolving Local -> SignalRead). An empty
    /// scope is used for module-scope lowering.
    locals: &'comp crate::hir::local_scope::LocalScope,
    /// Layout context for computing type sizes (uses RefCell for interior mutability).
    layout_ctx: RefCell<LirLayoutContext<'ctx>>,
    /// Monotonic counter for minting `ForId`s. Each `ThirNodeKind::For` we
    /// lower mints one id; the id is pushed onto `for_stack` for the
    /// duration of the body lowering so effects registered inside pick
    /// up their enclosing-for context.
    next_for_id: u32,
    /// Lexical stack of enclosing for-loops. Top is the innermost for.
    /// Drives node-level metadata (e.g. nested-for diff inputs).
    /// Fan-out happens in the per-(boundary, signal) walker, not as a
    /// per-effect iteration context.
    for_stack: Vec<crate::ids::ForId>,
}

impl<'ctx, 'comp> LirLowering<'ctx, 'comp> {
    /// Construct a lowerer anchored to a specific component.
    fn for_component(
        ctx: &'ctx CompilerContext,
        component_def_id: DefId,
        locals: &'comp crate::hir::local_scope::LocalScope,
    ) -> Self {
        let mut signal_def_ids = ctx
            .defs
            .as_component(component_def_id)
            .map(|c| c.properties.clone())
            .unwrap_or_default();
        Self::extend_with_globals(ctx, &mut signal_def_ids);
        Self {
            ctx,
            scope: LoweringScope::Component(component_def_id),
            next_effect_id: 0,
            effects: Vec::new(),
            signal_def_ids,
            locals,
            layout_ctx: RefCell::new(LirLayoutContext::new(ctx)),
            next_for_id: 0,
            for_stack: Vec::new(),
        }
    }

    /// Construct a module-scoped lowerer. Used for global-singleton defaults
    /// and any other expression that isn't tied to a component.
    fn for_module(
        ctx: &'ctx CompilerContext,
        locals: &'comp crate::hir::local_scope::LocalScope,
    ) -> Self {
        let mut signal_def_ids: Vec<DefId> = Vec::new();
        Self::extend_with_globals(ctx, &mut signal_def_ids);
        Self {
            ctx,
            scope: LoweringScope::Module,
            next_effect_id: 0,
            effects: Vec::new(),
            signal_def_ids,
            locals,
            layout_ctx: RefCell::new(LirLayoutContext::new(ctx)),
            next_for_id: 0,
            for_stack: Vec::new(),
        }
    }

    /// Append every global-singleton property's DefId to `signal_def_ids` so
    /// SignalRead/SignalWrite resolution treats globals uniformly with
    /// component signals.
    fn extend_with_globals(ctx: &'ctx CompilerContext, signal_def_ids: &mut Vec<DefId>) {
        for global_id in ctx.defs.globals().collect::<Vec<_>>() {
            if let Some(g) = ctx.defs.as_global(global_id) {
                signal_def_ids.extend(g.properties.iter().copied());
            }
        }
    }

    fn fresh_effect_id(&mut self) -> u32 {
        let id = self.next_effect_id;
        self.next_effect_id += 1;
        id
    }

    fn lower_component_to_tree(&mut self, component: &ThirComponent) -> TreeLirResource {
        // Extract signals from component properties, using type-checked defaults
        let signals = self.lower_signals(&component.signal_defaults);

        // Lower body nodes
        let body: Vec<LirNode> = component
            .body
            .iter()
            .map(|node| self.lower_node(node))
            .collect();

        TreeLirResource {
            def_id: component.def_id,
            name: component.name,
            span: component.span,
            is_export: component.is_export,
            signals,
            effects: std::mem::take(&mut self.effects),
            body,
        }
    }

    /// Extract signals from the component's properties.
    /// Uses type-checked defaults from ThirComponent.signal_defaults.
    fn lower_signals(
        &mut self,
        signal_defaults: &HashMap<DefId, ThirExpr>,
    ) -> Vec<LirSignal> {
        // Only meaningful in component scope — module-scope lowering has no
        // owning component to extract signals from.
        let component_def_id = match self.scope {
            LoweringScope::Component(id) => id,
            LoweringScope::Module => return vec![],
        };
        let comp_def = match self.ctx.defs.as_component(component_def_id) {
            Some(def) => def,
            None => return vec![],
        };
        let properties = comp_def.properties.clone();

        let mut signals = Vec::new();
        for prop_def_id in properties {
            let DefKind::Signal(signal_def) = self.ctx.defs.kind(prop_def_id) else {
                continue;
            };
            let signal_ty = signal_def.ty;

            // Derived signals: when a property's default expression reads
            // other signals, register an effect that re-runs the expression
            // and writes the result to this signal's slot whenever any
            // source dep changes. The `default` stays populated so the
            // constructor also seeds the initial value.
            let default = match signal_defaults.get(&prop_def_id) {
                Some(thir_expr) => {
                    let deps = self.collect_dependencies(thir_expr);
                    // Filter out self-references — a signal whose default
                    // refers to itself (direct or transitive) would loop
                    // forever. The simplest guard: drop the self-edge at
                    // registration time. Typeck can promote this to an
                    // error in a future pass.
                    let deps: Vec<DefId> = deps.into_iter().filter(|&d| d != prop_def_id).collect();
                    let lir_expr = self.lower_expr(thir_expr);
                    if !deps.is_empty() {
                        let effect_id = self.fresh_effect_id();
                        self.effects.push(LirEffect {
                            id: effect_id,
                            dependencies: deps,
                            // NodeId is irrelevant for signal-to-signal
                            // effects — derived-signal update blocks don't
                            // touch the DOM. Use a stable synthetic id.
                            target_node: NodeId::new(0),
                            update_kind: UpdateKind::DerivedSignal(prop_def_id),
                            expr: lir_expr.clone(),
                        });
                    }
                    Some(lir_expr)
                }
                None => None,
            };

            signals.push(LirSignal {
                def_id: prop_def_id,
                ty: signal_ty,
                default,
            });
        }

        signals
    }

    // ========== Node lowering ==========

    fn lower_node(&mut self, node: &ThirNode) -> LirNode {
        let kind = match &node.kind {
            ThirNodeKind::Element {
                component,
                tag,
                bindings,
                handlers,
                children,
            } => self.lower_element(node.id, *component, tag, bindings, handlers, children),

            ThirNodeKind::Text(expr) => self.lower_text(node.id, expr),

            ThirNodeKind::If {
                condition,
                then_branch,
                else_if_branches,
                else_branch,
            } => {
                let cond_expr = self.lower_expr(condition);
                let then_nodes: Vec<LirNode> =
                    then_branch.iter().map(|n| self.lower_node(n)).collect();
                let else_if_nodes: Vec<(LirExpr, Vec<LirNode>)> = else_if_branches
                    .iter()
                    .map(|(cond, nodes)| {
                        let c = self.lower_expr(cond);
                        let n: Vec<LirNode> =
                            nodes.iter().map(|node| self.lower_node(node)).collect();
                        (c, n)
                    })
                    .collect();
                let else_nodes: Option<Vec<LirNode>> = else_branch
                    .as_ref()
                    .map(|nodes| nodes.iter().map(|n| self.lower_node(n)).collect());

                LirNodeKind::If {
                    condition: cond_expr,
                    then_branch: then_nodes,
                    else_if_branches: else_if_nodes,
                    else_branch: else_nodes,
                }
            }

            ThirNodeKind::For {
                item,
                item_name,
                item_span,
                item_ty,
                iterable,
                key,
                body,
            } => {
                // Mint a ForId and push it onto the stack before lowering
                // the body. The id travels with LirNodeKind::For so
                // block_lower can thread it into ForContext without
                // re-minting.
                let for_id = crate::ids::ForId::new(self.next_for_id);
                self.next_for_id += 1;

                let iter_expr = self.lower_expr(iterable);
                let key_expr = key.as_ref().map(|k| self.lower_expr(k));

                self.for_stack.push(for_id);
                let body_nodes: Vec<LirNode> = body.iter().map(|n| self.lower_node(n)).collect();
                let popped = self.for_stack.pop();
                debug_assert_eq!(popped, Some(for_id), "for_stack push/pop mismatch");

                LirNodeKind::For {
                    for_id,
                    item: *item,
                    item_name: *item_name,
                    item_span: *item_span,
                    item_ty: *item_ty,
                    iterable: iter_expr,
                    key: key_expr,
                    body: body_nodes,
                }
            }
            ThirNodeKind::ChildrenSlot => LirNodeKind::ChildrenSlot,
        };

        LirNode::new(node.id, kind, node.span)
    }

    fn lower_element(
        &mut self,
        node_id: NodeId,
        component: Option<DefId>,
        tag: &str,
        bindings: &[ThirBinding],
        handlers: &[ThirHandler],
        children: &[ThirNode],
    ) -> LirNodeKind {
        let mut static_bindings = Vec::new();
        let mut dynamic_binding_ids = Vec::new();

        for binding in bindings {
            // Only process bindings with a getter (value)
            if let Some(ref value) = binding.value {
                let expr = self.lower_expr(value);
                let deps = self.collect_dependencies(value);

                if deps.is_empty() {
                    // Static binding
                    static_bindings.push(LirBinding {
                        name: binding.name.clone(),
                        value: expr,
                    });
                } else {
                    // Dynamic binding - create an effect
                    let effect_id = self.fresh_effect_id();
                    self.effects.push(LirEffect {
                        id: effect_id,
                        dependencies: deps,
                        target_node: node_id,
                        update_kind: UpdateKind::Property(binding.name.clone()),
                        expr,
                    });
                    dynamic_binding_ids.push(effect_id);
                }
            }
            // Note: Setters are handled separately in the block lowering phase
            // where they become part of event handler logic
        }

        let mut lir_handlers: Vec<LirHandler> = handlers
            .iter()
            .map(|h| LirHandler {
                event: h.name.clone(),
                body: h.body.iter().map(|s| self.lower_statement(s)).collect(),
                input_binding_target: None,
            })
            .collect();

        // Lower `set value: { ... }` binding setters on Input-family
        // elements into synthesised `input`-event handlers. The getter
        // expression identifies the target signal (must be a bare
        // signal ref — Svelte's `value={c}` shape); the runtime then
        // writes the DOM-reported value into that signal before
        // running the user-authored body.
        for binding in bindings {
            let Some(setter) = &binding.setter else {
                continue;
            };
            // Only `set value:` on an Input-family element is supported
            // in the current plan. Other element/property pairings are
            // loud errors — silent-fallback guard per CLAUDE.md.
            if binding.name != "value" {
                panic!(
                    "unsupported binding setter `set {}:` — only `set value:` \
                     on bindable elements is implemented",
                    binding.name
                );
            }
            if !matches!(tag, "TextInput" | "IntegerInput" | "FloatInput") {
                panic!(
                    "unsupported binding setter `set value:` on element `{}` \
                     — only `TextInput`/`IntegerInput`/`FloatInput` support value-binding setters",
                    tag
                );
            }
            // The getter's target must be a bare signal read. Anything
            // else (e.g. `value: c + 1`) has no unambiguous write-back
            // target.
            let getter = binding.value.as_ref().unwrap_or_else(|| {
                panic!("binding `set value:` on input element must have a companion getter `value: <signal>`")
            });
            let target = match &getter.kind {
                ThirExprKind::Def(def_id) if self.signal_def_ids.contains(def_id) => *def_id,
                ThirExprKind::Local(local_id) => {
                    let info = self.locals.get(*local_id);
                    info.def_id
                        .filter(|d| self.signal_def_ids.contains(d))
                        .unwrap_or_else(|| {
                            panic!("binding `set value:` getter must be a bare signal reference")
                        })
                }
                _ => panic!(
                    "binding `set value:` getter must be a bare signal reference \
                     (got {:?})",
                    getter.kind
                ),
            };
            let body: Vec<LirStatement> = setter.iter().map(|s| self.lower_statement(s)).collect();
            lir_handlers.push(LirHandler {
                event: "input".to_string(),
                body,
                input_binding_target: Some(target),
            });
        }

        let lir_children: Vec<LirNode> = children.iter().map(|n| self.lower_node(n)).collect();

        LirNodeKind::Element {
            component,
            tag: tag.to_string(),
            static_bindings,
            dynamic_binding_ids,
            handlers: lir_handlers,
            children: lir_children,
        }
    }

    fn lower_text(&mut self, node_id: NodeId, expr: &ThirExpr) -> LirNodeKind {
        let deps = self.collect_dependencies(expr);

        if deps.is_empty() {
            // Static text - try to extract literal string
            if let ThirExprKind::Literal(HirLiteral::String(s)) = &expr.kind {
                return LirNodeKind::StaticText(s.clone());
            }
            // Non-string literal or expression that evaluates to string at runtime
            // but has no dependencies - still treat as static
            let lir_expr = self.lower_expr(expr);
            // For now, if it's a literal that can be converted to string, do it
            if let LirExprKind::Literal(LirLiteral::String(s)) = &lir_expr.kind {
                return LirNodeKind::StaticText(s.clone());
            }
            // Otherwise create a "static" effect (effect_id 0 is special for initial render)
            let effect_id = self.fresh_effect_id();
            self.effects.push(LirEffect {
                id: effect_id,
                dependencies: vec![],
                target_node: node_id,
                update_kind: UpdateKind::TextContent,
                expr: lir_expr,
            });
            LirNodeKind::DynamicText { effect_id }
        } else {
            // Dynamic text - create an effect
            let effect_id = self.fresh_effect_id();
            let lir_expr = self.lower_expr(expr);
            self.effects.push(LirEffect {
                id: effect_id,
                dependencies: deps,
                target_node: node_id,
                update_kind: UpdateKind::TextContent,
                expr: lir_expr,
            });
            LirNodeKind::DynamicText { effect_id }
        }
    }

    // ========== Expression lowering ==========

    fn lower_expr(&self, expr: &ThirExpr) -> LirExpr {
        let kind = match &expr.kind {
            ThirExprKind::Local(local_id) => {
                // Check if this local corresponds to a signal property
                let local_info = self.locals.get(*local_id);
                if let Some(def_id) = local_info.def_id {
                    if self.signal_def_ids.contains(&def_id) {
                        LirExprKind::SignalRead(def_id)
                    } else {
                        LirExprKind::Local(*local_id)
                    }
                } else {
                    LirExprKind::Local(*local_id)
                }
            }

            ThirExprKind::Def(def_id) => {
                // Check if this is a signal read
                if self.signal_def_ids.contains(def_id) {
                    LirExprKind::SignalRead(*def_id)
                } else {
                    LirExprKind::Def(*def_id)
                }
            }

            ThirExprKind::Literal(lit) => {
                // Compound literals (List, Tuple, Record) should be converted to
                // specialized THIR kinds (ListLiteral, TupleLiteral, RecordLiteral)
                // by typeck. Only primitive literals should reach here.
                match lit {
                    HirLiteral::List(_) | HirLiteral::Tuple(_) | HirLiteral::Record { .. } => {
                        panic!("Compound literal should be converted to specialized THIR kind by typeck")
                    }
                    _ => LirExprKind::Literal(lower_primitive_literal(lit, expr.ty, self.ctx)),
                }
            }

            ThirExprKind::Binary { op, lhs, rhs } => LirExprKind::Binary {
                op: *op,
                lhs: Box::new(self.lower_expr(lhs)),
                rhs: Box::new(self.lower_expr(rhs)),
            },

            ThirExprKind::Unary { op, operand } => LirExprKind::Unary {
                op: *op,
                operand: Box::new(self.lower_expr(operand)),
            },

            ThirExprKind::Field {
                base,
                field_idx,
                field_def: _,
            } => LirExprKind::Field {
                base: Box::new(self.lower_expr(base)),
                field_idx: *field_idx,
            },

            ThirExprKind::OptionalField {
                base,
                field_idx,
                field_def: _,
            } => {
                // For now, treat optional field same as regular field
                // TODO: Desugar to match expression
                LirExprKind::Field {
                    base: Box::new(self.lower_expr(base)),
                    field_idx: *field_idx,
                }
            }

            ThirExprKind::Index { base, index } => LirExprKind::Index {
                base: Box::new(self.lower_expr(base)),
                index: Box::new(self.lower_expr(index)),
            },

            ThirExprKind::Call { func, args } => {
                // All function calls (including builtins) are lowered to Call nodes.
                // Codegen will handle known functions specially based on DefId.
                LirExprKind::Call {
                    func: *func,
                    args: args.iter().map(|a| self.lower_expr(a)).collect(),
                }
            }

            ThirExprKind::Range {
                start,
                end,
                inclusive,
            } => LirExprKind::Range {
                start: Box::new(self.lower_expr(start)),
                end: Box::new(self.lower_expr(end)),
                inclusive: *inclusive,
            },

            ThirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => LirExprKind::Ternary {
                condition: Box::new(self.lower_expr(condition)),
                then_expr: Box::new(self.lower_expr(then_expr)),
                else_expr: Box::new(self.lower_expr(else_expr)),
            },

            ThirExprKind::Closure { params, body, .. } => {
                // Lower closure to LirExprKind::Closure for use in filter/map/etc.
                let lowered_params: Vec<(LocalId, Ty)> = params.clone();
                let lowered_body: Vec<LirStatement> =
                    body.iter().map(|s| self.lower_statement(s)).collect();
                LirExprKind::Closure {
                    params: lowered_params,
                    body: lowered_body,
                }
            }

            ThirExprKind::Interpolation(parts) => {
                // Convert interpolation to a call to the known `concat` function.
                // Non-string parts are wrapped in calls to type-specific conversion functions.
                let concat_func = self.ctx.known.functions.concat();

                let exprs: Vec<LirExpr> = parts
                    .iter()
                    .map(|part| match part {
                        ThirInterpolationPart::Literal(s) => LirExpr::new(
                            LirExprKind::Literal(LirLiteral::String(s.clone())),
                            Ty::STRING,
                        ),
                        ThirInterpolationPart::Expr(e) => {
                            let lowered = self.lower_expr(e);
                            // Wrap non-string expressions in type-specific conversion
                            if e.ty != Ty::STRING {
                                let conv_func = self.get_to_string_func_for_type(e.ty);
                                LirExpr::new(
                                    LirExprKind::Call {
                                        func: conv_func,
                                        args: vec![lowered],
                                    },
                                    Ty::STRING,
                                )
                            } else {
                                lowered
                            }
                        }
                    })
                    .collect();

                // Single-part optimization: don't wrap in concat
                if exprs.len() == 1 {
                    return exprs.into_iter().next().unwrap();
                }

                // Multi-part: wrap in a call to concat
                LirExprKind::Call {
                    func: concat_func,
                    args: exprs,
                }
            }

            ThirExprKind::EnumCase { ty_def, case_idx } => LirExprKind::EnumCase {
                ty_def: *ty_def,
                discriminant: case_idx.0,
            },

            ThirExprKind::VariantCtor {
                ty_def,
                case_idx,
                payload,
            } => LirExprKind::VariantCtor {
                ty_def: *ty_def,
                case_idx: case_idx.0,
                payload: payload.as_ref().map(|p| Box::new(self.lower_expr(p))),
            },

            // ========== List/Record/Tuple Literals ==========
            ThirExprKind::ListLiteral {
                elements,
                element_ty,
            } => {
                // Compute element size using layout
                let element_size = self.layout_ctx.borrow_mut().layout_of(*element_ty).size;

                // Lower all element expressions
                let lowered_elements: Vec<_> =
                    elements.iter().map(|e| self.lower_expr(e)).collect();

                LirExprKind::ListConstruct {
                    elements: lowered_elements,
                    element_size,
                }
            }

            ThirExprKind::RecordLiteral { record_def, fields } => {
                // Compute total record size using layout
                let record_layout = self.layout_ctx.borrow_mut().layout_of(expr.ty);

                // Lower all field expressions
                let lowered_fields: Vec<_> = fields.iter().map(|f| self.lower_expr(f)).collect();

                LirExprKind::RecordConstruct {
                    record_def: *record_def,
                    fields: lowered_fields,
                    total_size: record_layout.size,
                }
            }

            ThirExprKind::TupleLiteral { elements } => {
                // Compute total tuple size using layout
                let tuple_layout = self.layout_ctx.borrow_mut().layout_of(expr.ty);

                // Lower all element expressions
                let lowered_elements: Vec<_> =
                    elements.iter().map(|e| self.lower_expr(e)).collect();

                LirExprKind::TupleConstruct {
                    elements: lowered_elements,
                    total_size: tuple_layout.size,
                }
            }

            ThirExprKind::GlobalRead { prop, .. } => LirExprKind::SignalRead(*prop),

            ThirExprKind::GlobalCall { function, args, .. } => LirExprKind::GlobalCall {
                function: *function,
                args: args.iter().map(|a| self.lower_expr(a)).collect(),
            },

            ThirExprKind::Error => {
                todo!("Error expression reached LIR lowering (ty={:?})", expr.ty)
            }
        };

        LirExpr::new(kind, expr.ty)
    }

    // ========== Statement lowering ==========

    fn lower_statement(&self, stmt: &ThirStatement) -> LirStatement {
        match stmt {
            ThirStatement::Expr(e) => LirStatement::Expr(self.lower_expr(e)),

            ThirStatement::Assign { target, value } => {
                // Check if target is a signal write
                let signal_def_id = match &target.kind {
                    ThirExprKind::Def(def_id) if self.signal_def_ids.contains(def_id) => {
                        Some(*def_id)
                    }
                    ThirExprKind::Local(local_id) => {
                        let local_info = self.locals.get(*local_id);
                        local_info
                            .def_id
                            .filter(|def_id| self.signal_def_ids.contains(def_id))
                    }
                    _ => None,
                };

                if let Some(def_id) = signal_def_id {
                    return LirStatement::SignalWrite {
                        signal: def_id,
                        value: self.lower_expr(value),
                    };
                }

                if let ThirExprKind::GlobalRead { prop, .. } = &target.kind {
                    return LirStatement::SignalWrite {
                        signal: *prop,
                        value: self.lower_expr(value),
                    };
                }

                LirStatement::Expr(self.lower_expr(value))
            }

            ThirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => LirStatement::If {
                condition: self.lower_expr(condition),
                then_branch: then_branch
                    .iter()
                    .map(|s| self.lower_statement(s))
                    .collect(),
                else_branch: else_branch
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.lower_statement(s)).collect()),
            },

            ThirStatement::Let {
                local_id, value, ..
            } => LirStatement::Let {
                local_id: *local_id,
                value: self.lower_expr(value),
            },
        }
    }

    // ========== Type-specific conversion helpers ==========

    /// Get the appropriate to-string conversion function for a given type.
    /// Primitives get type-specific functions, complex types get object_to_string.
    fn get_to_string_func_for_type(&self, ty: Ty) -> DefId {
        let ty_kind = self.ctx.types.kind(ty);
        match ty_kind {
            InternedTyKind::Bool => self.ctx.known.functions.bool_to_string(),
            InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 => {
                self.ctx.known.functions.s32_to_string()
            }
            InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 => {
                self.ctx.known.functions.u32_to_string()
            }
            InternedTyKind::S64 => self.ctx.known.functions.s64_to_string(),
            InternedTyKind::U64 => self.ctx.known.functions.u64_to_string(),
            InternedTyKind::F32 => self.ctx.known.functions.f32_to_string(),
            InternedTyKind::F64 => self.ctx.known.functions.f64_to_string(),
            InternedTyKind::Char => self.ctx.known.functions.char_to_string(),
            // All complex types use the generic object_to_string
            InternedTyKind::String => {
                // Should not happen - string doesn't need conversion
                // But return object_to_string as fallback
                self.ctx.known.functions.object_to_string()
            }
            InternedTyKind::List(_)
            | InternedTyKind::Option(_)
            | InternedTyKind::Result { .. }
            | InternedTyKind::Tuple(_)
            | InternedTyKind::Adt(_)
            | InternedTyKind::Func { .. }
            | InternedTyKind::Length
            | InternedTyKind::PhysicalLength
            | InternedTyKind::Angle
            | InternedTyKind::Duration
            | InternedTyKind::Percent
            | InternedTyKind::RelativeFontSize
            | InternedTyKind::Color
            | InternedTyKind::Brush
            | InternedTyKind::Image
            | InternedTyKind::Easing
            | InternedTyKind::Error
            | InternedTyKind::Unknown
            | InternedTyKind::Unit => self.ctx.known.functions.object_to_string(),
        }
    }

    // ========== Dependency tracking ==========

    /// Collect signal dependencies from an expression.
    fn collect_dependencies(&self, expr: &ThirExpr) -> Vec<DefId> {
        let mut deps = Vec::new();
        self.collect_dependencies_inner(expr, &mut deps);
        deps.sort_by_key(|d| d.0);
        deps.dedup();
        deps
    }

    fn collect_dependencies_inner(&self, expr: &ThirExpr, deps: &mut Vec<DefId>) {
        match &expr.kind {
            ThirExprKind::Def(def_id) => {
                if self.signal_def_ids.contains(def_id) {
                    deps.push(*def_id);
                }
            }

            ThirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_dependencies_inner(lhs, deps);
                self.collect_dependencies_inner(rhs, deps);
            }

            ThirExprKind::Unary { operand, .. } => {
                self.collect_dependencies_inner(operand, deps);
            }

            ThirExprKind::Field { base, .. } | ThirExprKind::OptionalField { base, .. } => {
                self.collect_dependencies_inner(base, deps);
            }

            ThirExprKind::Index { base, index } => {
                self.collect_dependencies_inner(base, deps);
                self.collect_dependencies_inner(index, deps);
            }

            ThirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_dependencies_inner(arg, deps);
                }
            }

            ThirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_dependencies_inner(condition, deps);
                self.collect_dependencies_inner(then_expr, deps);
                self.collect_dependencies_inner(else_expr, deps);
            }

            ThirExprKind::Interpolation(parts) => {
                for part in parts {
                    if let ThirInterpolationPart::Expr(e) = part {
                        self.collect_dependencies_inner(e, deps);
                    }
                }
            }

            ThirExprKind::Range { start, end, .. } => {
                self.collect_dependencies_inner(start, deps);
                self.collect_dependencies_inner(end, deps);
            }

            ThirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_dependencies_inner(p, deps);
                }
            }

            ThirExprKind::Local(local_id) => {
                // Check if this local corresponds to a signal
                let local_info = self.locals.get(*local_id);
                if let Some(def_id) = local_info.def_id {
                    if self.signal_def_ids.contains(&def_id) {
                        deps.push(def_id);
                    }
                }
            }

            // List/Record/Tuple literals - collect deps from elements/fields
            ThirExprKind::ListLiteral { elements, .. } => {
                for elem in elements {
                    self.collect_dependencies_inner(elem, deps);
                }
            }

            ThirExprKind::RecordLiteral { fields, .. } => {
                for field in fields {
                    self.collect_dependencies_inner(field, deps);
                }
            }

            ThirExprKind::TupleLiteral { elements } => {
                for elem in elements {
                    self.collect_dependencies_inner(elem, deps);
                }
            }

            // No dependencies
            ThirExprKind::Literal(_)
            | ThirExprKind::EnumCase { .. }
            | ThirExprKind::Closure { .. }
            | ThirExprKind::Error => {}

            ThirExprKind::GlobalRead { prop, .. } => {
                if self.signal_def_ids.contains(prop) {
                    deps.push(*prop);
                }
            }
            ThirExprKind::GlobalCall { args, .. } => {
                for arg in args {
                    self.collect_dependencies_inner(arg, deps);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::definitions::{ComponentDef, DefKind};
    use crate::hir::local_scope::LocalScope;
    use crate::interner::Name;
    use crate::lir::LirOp;
    use crate::source::{SourceId, Span};

    fn dummy_span() -> Span {
        Span::new(SourceId(0), 0, 0)
    }

    fn create_test_ctx_with_component(name: &str) -> (CompilerContext, DefId) {
        let mut ctx = CompilerContext::new();
        // Phase 2.2: lowering reads `ctx.dom_imports()` to construct
        // `LirOp::CallFunction` for DOM ops. Register them on the test
        // ctx so the helper produces a ctx ready for the lowering pass.
        let dom = crate::dom_imports::register_dom_imports(&mut ctx);
        ctx.set_dom_imports(dom);
        let comp_name = ctx.intern(name);
        // Pre-allocate a DefId for the component
        let def_id = DefId::new(ctx.defs.len() as u32);
        ctx.defs.alloc(
            comp_name,
            DefKind::Component(ComponentDef {
                def_id,
                name: comp_name,
                properties: vec![],
                callbacks: vec![],
                is_export: true,
                has_children_slot: false,
            }),
            dummy_span(),
        );
        (ctx, def_id)
    }

    #[test]
    fn test_lower_simple_component() {
        let (ctx, def_id) = create_test_ctx_with_component("TestComponent");

        let component = ThirComponent {
            def_id,
            name: Name(0),
            span: dummy_span(),
            is_export: true,
            body: vec![],
            locals: LocalScope::new(),
            signal_defaults: HashMap::new(),
            signal_deps: crate::thir::signalck::SignalDependencies::default(),
        };

        let lir = lower_component(&component, &ctx);
        assert!(lir.is_export);
        // Phase 2.3: DOM ops are now CallFunction against dom_imports
        // DefIds. An empty component should not emit any
        // create_element / create_text dispatch calls.
        let mount_block = &lir.blocks[lir.mount_block.0 as usize];
        let create_element_def = ctx.dom_imports().create_element;
        let create_text_def = ctx.dom_imports().create_text;
        assert!(mount_block.ops.iter().all(|op| !matches!(
            op,
            LirOp::CallFunction { func, .. }
                if *func == create_element_def || *func == create_text_def
        )));
        assert!(lir.effects.is_empty());
    }

    #[test]
    fn test_lower_static_text() {
        let (ctx, def_id) = create_test_ctx_with_component("TestComponent");

        let component = ThirComponent {
            def_id,
            name: Name(0),
            span: dummy_span(),
            is_export: true,
            signal_defaults: HashMap::new(),
            signal_deps: crate::thir::signalck::SignalDependencies::default(),
            body: vec![ThirNode::new(
                NodeId::new(0),
                ThirNodeKind::Text(ThirExpr::new(
                    crate::ids::ExprId::new(0),
                    ThirExprKind::Literal(HirLiteral::String("Hello".to_string())),
                    Ty::STRING,
                    dummy_span(),
                )),
                dummy_span(),
            )],
            locals: LocalScope::new(),
        };

        let lir = lower_component(&component, &ctx);

        // Block-based: check that "Hello" string is interned
        assert!(
            lir.strings.contains(&"Hello".to_string()),
            "String 'Hello' should be interned"
        );

        // Check that mount block contains a CreateText-equivalent
        // call. Phase 2.2b switched static-text lowering to
        // `LirOp::CallFunction { func: dom_imports.create_text, … }`
        // prefixed by `PushStringPtr` / `PushStringLen`; the legacy
        // `LirOp::CreateText` variant is no longer emitted.
        let mount_block = &lir.blocks[lir.mount_block.0 as usize];
        let create_text_def = ctx.dom_imports().create_text;
        let has_create_text = mount_block.ops.iter().any(|op| {
            matches!(
                op,
                LirOp::CallFunction { func, .. } if *func == create_text_def
            )
        });
        assert!(
            has_create_text,
            "Mount block should contain a CallFunction to dom_imports.create_text"
        );
    }
}
