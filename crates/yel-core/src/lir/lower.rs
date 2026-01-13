//! THIR to LIR lowering.
//!
//! This phase:
//! 1. Extracts reactive signals from component properties
//! 2. Identifies reactive bindings and creates effects
//! 3. Lowers expressions, statements, and nodes
//! 4. Separates static vs dynamic bindings
//! 5. Converts to block-based representation for codegen

use std::cell::RefCell;

use crate::context::CompilerContext;
use crate::definitions::DefKind;
use crate::hir::expr::HirLiteral;
use crate::ids::{DefId, NodeId};
use crate::interner::Name;
use crate::source::Span;
use crate::thir::{
    ThirBinding, ThirComponent, ThirExpr, ThirExprKind, ThirHandler, ThirInterpolationPart,
    ThirNode, ThirNodeKind, ThirStatement,
};
use crate::types::{InternedTyKind, Ty};
use crate::lir::block_lower::BlockLowering;

use super::layout::LayoutContext;
use super::expr::{LirExpr, LirExprKind, LirLiteral, LirStatement};

/// Convert a primitive HirLiteral to LirLiteral.
/// Panics for compound types (List, Tuple, Record) which should be handled as separate constructs.
fn lower_primitive_literal(lit: &HirLiteral, ty: Ty, ctx: &CompilerContext) -> LirLiteral {
    match lit {
        HirLiteral::Int(v) => {
            // Use type to determine exact integer variant
            match ctx.ty_kind(ty) {
                InternedTyKind::S8 => LirLiteral::S8(*v as i8),
                InternedTyKind::S16 => LirLiteral::S16(*v as i16),
                InternedTyKind::S32 => LirLiteral::S32(*v as i32),
                InternedTyKind::S64 => LirLiteral::S64(*v),
                InternedTyKind::U8 => LirLiteral::U8(*v as u8),
                InternedTyKind::U16 => LirLiteral::U16(*v as u16),
                InternedTyKind::U32 => LirLiteral::U32(*v as u32),
                InternedTyKind::U64 => LirLiteral::U64(*v as u64),
                _ => LirLiteral::S32(*v as i32), // Default to s32
            }
        }
        HirLiteral::Float(v) => {
            match ctx.ty_kind(ty) {
                InternedTyKind::F64 => LirLiteral::F64(*v),
                _ => LirLiteral::F32(*v as f32),
            }
        }
        HirLiteral::String(s) => LirLiteral::String(s.clone()),
        HirLiteral::Char(c) => LirLiteral::Char(*c),
        HirLiteral::Bool(b) => LirLiteral::Bool(*b),
        HirLiteral::Unit(_, _) | HirLiteral::Color(_) => {
            panic!("Unit and Color literals should be lowered to specific types during THIR->LIR")
        }
        HirLiteral::List(_) | HirLiteral::Tuple(_) | HirLiteral::Record { .. } => {
            panic!("Compound literals should be handled as ListConstruct/TupleConstruct/RecordConstruct")
        }
    }
}
use super::node::{LirBinding, LirComponent, LirHandler, LirNode, LirNodeKind};
use super::signal::{LirEffect, LirSignal, UpdateKind};

/// Internal tree-based representation used during lowering.
/// This is converted to block-based `LirComponent` at the end.
pub(crate) struct TreeLirComponent {
    pub def_id: DefId,
    pub name: Name,
    pub span: Span,
    pub is_export: bool,
    pub signals: Vec<LirSignal>,
    pub effects: Vec<LirEffect>,
    pub body: Vec<LirNode>,
}

/// Lower a THIR component to block-based LIR (ready for codegen).
pub fn lower_component(component: &ThirComponent, ctx: &CompilerContext) -> LirComponent {
    let mut lowering = LirLowering::new(ctx, component.def_id, &component.locals);
    // First create tree-based representation
    let tree = lowering.lower_component_to_tree(component);
    let mut lowering = BlockLowering::new(ctx, &tree);
    lowering.lower_component(&tree)
}

/// LIR lowering state.
struct LirLowering<'ctx, 'comp> {
    ctx: &'ctx CompilerContext,
    /// Component DefId being lowered.
    component_def_id: DefId,
    /// Next effect ID to assign.
    next_effect_id: u32,
    /// Collected effects.
    effects: Vec<LirEffect>,
    /// Signal DefIds for this component (for dependency tracking).
    signal_def_ids: Vec<DefId>,
    /// Reference to component locals (for resolving Local -> SignalRead).
    locals: &'comp crate::hir::local_scope::LocalScope,
    /// Layout context for computing type sizes (uses RefCell for interior mutability).
    layout_ctx: RefCell<LayoutContext<'ctx>>,
}

impl<'ctx, 'comp> LirLowering<'ctx, 'comp> {
    fn new(ctx: &'ctx CompilerContext, component_def_id: DefId, locals: &'comp crate::hir::local_scope::LocalScope) -> Self {
        // Collect signal DefIds from component properties
        let signal_def_ids = ctx
            .defs
            .as_component(component_def_id)
            .map(|c| c.properties.clone())
            .unwrap_or_default();

        Self {
            ctx,
            component_def_id,
            next_effect_id: 0,
            effects: Vec::new(),
            signal_def_ids,
            locals,
            layout_ctx: RefCell::new(LayoutContext::new(ctx)),
        }
    }

    fn fresh_effect_id(&mut self) -> u32 {
        let id = self.next_effect_id;
        self.next_effect_id += 1;
        id
    }

    fn lower_component_to_tree(&mut self, component: &ThirComponent) -> TreeLirComponent {
        // Extract signals from component properties
        let signals = self.lower_signals();

        // Lower body nodes
        let body: Vec<LirNode> = component
            .body
            .iter()
            .map(|node| self.lower_node(node))
            .collect();

        TreeLirComponent {
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
    fn lower_signals(&self) -> Vec<LirSignal> {
        let comp_def = match self.ctx.defs.as_component(self.component_def_id) {
            Some(def) => def,
            None => return vec![],
        };

        let mut signals = Vec::new();
        for &prop_def_id in &comp_def.properties {
            if let DefKind::Signal(signal_def) = self.ctx.defs.kind(prop_def_id) {
                // Convert HirExpr default to LirExpr
                let default = signal_def.default.as_ref().map(|hir_expr| {
                    self.lower_hir_expr_to_lir(hir_expr, signal_def.ty)
                });

                signals.push(LirSignal {
                    def_id: prop_def_id,
                    ty: signal_def.ty,
                    default,
                });
            }
        }

        signals
    }

    /// Convert a simple HirExpr to LirExpr (for signal defaults).
    /// Uses the declared signal type since HIR expressions aren't type-checked.
    fn lower_hir_expr_to_lir(&self, expr: &crate::hir::HirExpr, declared_ty: Ty) -> LirExpr {
        use crate::hir::HirExprKind;
        use crate::hir::expr::HirLiteral;

        let kind = match &expr.kind {
            HirExprKind::Literal(lit) => {
                // Handle list/record/tuple literals specially
                match lit {
                    HirLiteral::List(elements) => {
                        // Get element type from declared list type
                        let element_ty = match self.ctx.ty_kind(declared_ty) {
                            crate::types::InternedTyKind::List(elem_ty) => *elem_ty,
                            _ => Ty::S32, // fallback
                        };
                        let element_size = self.layout_ctx.borrow_mut().layout_of(element_ty).size;

                        // Lower each element
                        let lowered_elements: Vec<_> = elements
                            .iter()
                            .map(|e| self.lower_hir_expr_to_lir(e, element_ty))
                            .collect();

                        LirExprKind::ListConstruct {
                            elements: lowered_elements,
                            element_size,
                        }
                    }
                    HirLiteral::Tuple(elements) => {
                        // Get element types from declared tuple type
                        let elem_tys = match self.ctx.ty_kind(declared_ty) {
                            crate::types::InternedTyKind::Tuple(tys) => tys.clone(),
                            _ => vec![Ty::S32; elements.len()], // fallback
                        };
                        let total_size = self.layout_ctx.borrow_mut().layout_of(declared_ty).size;

                        let lowered_elements: Vec<_> = elements
                            .iter()
                            .zip(elem_tys.iter())
                            .map(|(e, ty)| self.lower_hir_expr_to_lir(e, *ty))
                            .collect();

                        LirExprKind::TupleConstruct {
                            elements: lowered_elements,
                            total_size,
                        }
                    }
                    HirLiteral::Record { fields } => {
                        // Get record definition from declared type
                        let record_def = match self.ctx.ty_kind(declared_ty) {
                            crate::types::InternedTyKind::Adt(def_id) => *def_id,
                            _ => panic!("Expected ADT type for record literal, got {:?}", declared_ty),
                        };

                        let total_size = self.layout_ctx.borrow_mut().layout_of(declared_ty).size;

                        // Get field types from record definition
                        let rec_def = self.ctx.defs.as_record(record_def)
                            .expect("Expected record definition");
                        let field_tys: Vec<_> = rec_def.fields.iter()
                            .map(|&field_def_id| {
                                match self.ctx.defs.kind(field_def_id) {
                                    crate::definitions::DefKind::Field(f) => f.ty,
                                    _ => panic!("Expected field definition"),
                                }
                            })
                            .collect();

                        // Lower each field expression in definition order
                        let lowered_fields: Vec<_> = fields.iter()
                            .enumerate()
                            .map(|(i, (_, expr))| {
                                let field_ty = field_tys[i];
                                self.lower_hir_expr_to_lir(expr, field_ty)
                            })
                            .collect();

                        LirExprKind::RecordConstruct {
                            record_def,
                            fields: lowered_fields,
                            total_size,
                        }
                    }
                    // Primitive literals - convert to LirLiteral
                    _ => LirExprKind::Literal(lower_primitive_literal(lit, declared_ty, self.ctx)),
                }
            }
            HirExprKind::Def(def_id) => LirExprKind::Def(*def_id),
            HirExprKind::Local(local_id) => LirExprKind::Local(*local_id),
            // For more complex expressions, fall back to a placeholder
            // These should ideally be type-checked first
            _ => {
                // Return a default value based on type
                match self.ctx.ty_kind(declared_ty) {
                    InternedTyKind::S32 | InternedTyKind::U32 => {
                        LirExprKind::Literal(LirLiteral::S32(0))
                    }
                    InternedTyKind::String => {
                        LirExprKind::Literal(LirLiteral::String(String::new()))
                    }
                    InternedTyKind::Bool => {
                        LirExprKind::Literal(LirLiteral::Bool(false))
                    }
                    _ => LirExprKind::Literal(LirLiteral::S32(0)),
                }
            }
        };

        LirExpr::new(kind, declared_ty)
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
                        let n: Vec<LirNode> = nodes.iter().map(|node| self.lower_node(node)).collect();
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
                let iter_expr = self.lower_expr(iterable);
                let key_expr = key.as_ref().map(|k| self.lower_expr(k));
                let body_nodes: Vec<LirNode> = body.iter().map(|n| self.lower_node(n)).collect();

                LirNodeKind::For {
                    item: *item,
                    item_name: *item_name,
                    item_span: *item_span,
                    item_ty: *item_ty,
                    iterable: iter_expr,
                    key: key_expr,
                    body: body_nodes,
                }
            }
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

        let lir_handlers: Vec<LirHandler> = handlers
            .iter()
            .map(|h| LirHandler {
                event: h.name.clone(),
                body: h.body.iter().map(|s| self.lower_statement(s)).collect(),
            })
            .collect();

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
                // Handle compound literals specially
                match lit {
                    HirLiteral::List(elements) => {
                        let element_ty = match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::List(elem_ty) => *elem_ty,
                            _ => Ty::S32,
                        };
                        let element_size = self.layout_ctx.borrow_mut().layout_of(element_ty).size;
                        LirExprKind::ListConstruct {
                            elements: elements.iter().map(|e| {
                                self.lower_hir_expr_to_lir(e, element_ty)
                            }).collect(),
                            element_size,
                        }
                    }
                    HirLiteral::Tuple(elements) => {
                        let elem_tys = match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::Tuple(tys) => tys.clone(),
                            _ => vec![Ty::S32; elements.len()],
                        };
                        let total_size = self.layout_ctx.borrow_mut().layout_of(expr.ty).size;
                        LirExprKind::TupleConstruct {
                            elements: elements.iter().zip(elem_tys.iter()).map(|(e, ty)| {
                                self.lower_hir_expr_to_lir(e, *ty)
                            }).collect(),
                            total_size,
                        }
                    }
                    HirLiteral::Record { fields } => {
                        let record_def = match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::Adt(def_id) => *def_id,
                            _ => panic!("Expected ADT type for record literal"),
                        };
                        let total_size = self.layout_ctx.borrow_mut().layout_of(expr.ty).size;
                        let rec_def = self.ctx.defs.as_record(record_def).expect("Expected record");
                        let field_tys: Vec<_> = rec_def.fields.iter().map(|&field_def_id| {
                            match self.ctx.defs.kind(field_def_id) {
                                crate::definitions::DefKind::Field(f) => f.ty,
                                _ => panic!("Expected field definition"),
                            }
                        }).collect();
                        LirExprKind::RecordConstruct {
                            record_def,
                            fields: fields.iter().zip(field_tys.iter()).map(|((_, e), ty)| {
                                self.lower_hir_expr_to_lir(e, *ty)
                            }).collect(),
                            total_size,
                        }
                    }
                    // Primitive literals
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

            ThirExprKind::Range { .. } => {
                // Range expressions are typically used in for loops
                // For now, just return a placeholder
                LirExprKind::Literal(LirLiteral::S32(0))
            }

            ThirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => LirExprKind::Ternary {
                condition: Box::new(self.lower_expr(condition)),
                then_expr: Box::new(self.lower_expr(then_expr)),
                else_expr: Box::new(self.lower_expr(else_expr)),
            },

            ThirExprKind::Closure { .. } => {
                // Closures are handled inline in handlers
                LirExprKind::Literal(LirLiteral::S32(0))
            }

            ThirExprKind::Interpolation(parts) => {
                // Convert interpolation to a call to the known `concat` function.
                // Non-string parts are wrapped in calls to type-specific conversion functions.
                let concat_func = self.ctx.known.functions.concat();

                let exprs: Vec<LirExpr> = parts
                    .iter()
                    .map(|part| match part {
                        ThirInterpolationPart::Literal(s) => {
                            LirExpr::new(LirExprKind::Literal(LirLiteral::String(s.clone())), Ty::STRING)
                        }
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

            ThirExprKind::ListLiteral { elements, element_ty } => {
                // Compute element size using layout
                let element_size = self.layout_ctx.borrow_mut().layout_of(*element_ty).size;

                // Lower all element expressions
                let lowered_elements: Vec<_> = elements
                    .iter()
                    .map(|e| self.lower_expr(e))
                    .collect();

                LirExprKind::ListConstruct {
                    elements: lowered_elements,
                    element_size,
                }
            }

            ThirExprKind::RecordLiteral { record_def, fields } => {
                // Compute total record size using layout
                let record_layout = self.layout_ctx.borrow_mut().layout_of(expr.ty);

                // Lower all field expressions
                let lowered_fields: Vec<_> = fields
                    .iter()
                    .map(|f| self.lower_expr(f))
                    .collect();

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
                let lowered_elements: Vec<_> = elements
                    .iter()
                    .map(|e| self.lower_expr(e))
                    .collect();

                LirExprKind::TupleConstruct {
                    elements: lowered_elements,
                    total_size: tuple_layout.size,
                }
            }

            ThirExprKind::Error => LirExprKind::Literal(LirLiteral::Bool(false)),
        };

        LirExpr::new(kind, expr.ty)
    }

    // ========== Statement lowering ==========

    fn lower_statement(&self, stmt: &ThirStatement) -> LirStatement {
        match stmt {
            ThirStatement::Expr(e) => LirStatement::Expr(self.lower_expr(e)),

            ThirStatement::Assign { target, value } => {
                // Check if target is a signal write - can be Def or Local
                let signal_def_id = match &target.kind {
                    ThirExprKind::Def(def_id) if self.signal_def_ids.contains(def_id) => {
                        Some(*def_id)
                    }
                    ThirExprKind::Local(local_id) => {
                        // Check if this local corresponds to a signal
                        let local_info = self.locals.get(*local_id);
                        local_info.def_id.filter(|def_id| self.signal_def_ids.contains(def_id))
                    }
                    _ => None,
                };

                if let Some(def_id) = signal_def_id {
                    return LirStatement::SignalWrite {
                        signal: def_id,
                        value: self.lower_expr(value),
                    };
                }
                // Regular assignment (not yet supported in LIR)
                LirStatement::Expr(self.lower_expr(value))
            }

            ThirStatement::If {
                condition,
                then_branch,
                else_branch,
            } => LirStatement::If {
                condition: self.lower_expr(condition),
                then_branch: then_branch.iter().map(|s| self.lower_statement(s)).collect(),
                else_branch: else_branch
                    .as_ref()
                    .map(|stmts| stmts.iter().map(|s| self.lower_statement(s)).collect()),
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
            InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 | InternedTyKind::S64 => {
                self.ctx.known.functions.s32_to_string()
            }
            InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 | InternedTyKind::U64 => {
                self.ctx.known.functions.u32_to_string()
            }
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
        };

        let lir = lower_component(&component, &ctx);
        assert!(lir.is_export);
        // Block-based: check that mount block has no CreateElement/CreateText ops
        let mount_block = &lir.blocks[lir.mount_block.0 as usize];
        // An empty component should have minimal ops (just return or nothing)
        assert!(mount_block.ops.iter().all(|op| !matches!(op, LirOp::CreateElement { .. } | LirOp::CreateText { .. })));
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
        assert!(lir.strings.contains(&"Hello".to_string()), "String 'Hello' should be interned");

        // Check that mount block contains a CreateText op
        let mount_block = &lir.blocks[lir.mount_block.0 as usize];
        let has_create_text = mount_block.ops.iter().any(|op| matches!(op, LirOp::CreateText { .. }));
        assert!(has_create_text, "Mount block should contain CreateText op");
    }
}
