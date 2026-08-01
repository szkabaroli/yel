//! Lowers a component's UI tree to builder expressions.
//!
//! An element becomes an [`Instantiate`](HirExprKind::Instantiate) — a call
//! whose target resolves like any name, whose props are one uniform list
//! merged in source order, and whose children are builder expressions. The
//! dynamic template forms become [`Boundary`](HirExprKind::Boundary) regions:
//! a UI `if` chain is one [`Conditional`](HirBoundary::Conditional) holding a
//! `match`, a UI `for` is a [`Repeat`](HirBoundary::Repeat), `@children` is a
//! [`Children`](HirBoundary::Children) mount point. The component's whole
//! tree is one **build body** whose tail is a
//! [`Fragment`](HirExprKind::Fragment) of the roots.
//!
//! What is *not* here, on purpose: tree-shape flattening (anchors, mount
//! layout — codegen's business), and prop **classification** (handler vs
//! binding is a `Definitions` lookup wherever it is needed; storing it here
//! would duplicate analysis results onto nodes).

use yelc_base::ErrorCode;
use yelc_sema::DefId;
use yelc_syntax::{ParsedFile, ast};

use super::LoweringContext;
use super::bodies::BodyLowering;
use crate::expr::{
    HirBlock, HirBoundary, HirClosure, HirExpr, HirExprKind, HirInstantiate, HirMatch, HirMatchArm,
    HirPattern, HirProp, HirRepeat,
};
use crate::ids::BodyId;

/// The component's UI tree as one parameterless body.
pub(super) fn lower_tree(
    lcx: &mut LoweringContext,
    file: &ParsedFile,
    def: DefId,
    decl: &ast::ComponentDecl,
) -> BodyId {
    let origin = SourceNodeIdOf(file, decl.id);
    let mut body = BodyLowering::new(lcx, file, origin.get(), Some(def));

    let roots: Vec<HirExpr> = decl
        .members
        .iter()
        .filter_map(|member| match member {
            ast::ComponentMember::Node(node) => Some(lower_node(&mut body, node)),
            ast::ComponentMember::Property(_)
            | ast::ComponentMember::Function(_)
            | ast::ComponentMember::Error { .. } => None,
        })
        .collect();

    // The single-slot rule, checked off this walk's own count — the frozen
    // tree re-walked the tree with `collect_children_slots` to ask, and that
    // second walker is the A3 case this phase must not reproduce.
    if body.slots.len() > 1 {
        let first = body.slots[0];
        let location = body
            .lcx
            .sema
            .sources
            .get(first.source)
            .map(|source| format!("{}:{}", source.name(), source.line_col(first.start).0))
            .unwrap_or_else(|| "<unknown>".to_string());
        // By index, not by iterator: `report` takes `&mut body`, so a borrow of
        // `body.slots` cannot be held across the loop body.
        for index in 1..body.slots.len() {
            let duplicate = body.slots[index];
            body.report(
                duplicate,
                ErrorCode::DuplicateChildrenSlot,
                "component already has a `@children` slot — only one is allowed".to_string(),
                Some(format!("first slot declared at {location}")),
            );
        }
    }

    let tail = HirExpr {
        hir_id: body.lcx.synthesize(origin.get()),
        kind: HirExprKind::Fragment(roots),
    };
    body.finish(
        0,
        HirBlock {
            stmts: Vec::new(),
            tail: Some(tail),
        },
    )
}

/// Tiny adaptor so `lower_tree` can name its origin before constructing the
/// `BodyLowering` that owns the file reference.
struct SourceNodeIdOf<'a>(&'a ParsedFile, yelc_syntax::NodeId);

impl SourceNodeIdOf<'_> {
    fn get(&self) -> crate::ids::SourceNodeId {
        crate::ids::SourceNodeId::new(self.0.source, self.1)
    }
}

fn lower_node(body: &mut BodyLowering, node: &ast::UiNode) -> HirExpr {
    match node {
        ast::UiNode::Element(element) => lower_element(body, element),
        ast::UiNode::Text(text) => HirExpr {
            hir_id: body.primary_id(text.id),
            kind: HirExprKind::UiText(Box::new(body.lower_expr(&text.content))),
        },
        ast::UiNode::If(node) => lower_ui_if(body, node),
        ast::UiNode::For(node) => lower_ui_for(body, node),
        ast::UiNode::Children { id, span } => {
            body.slots.push(*span);
            HirExpr {
                hir_id: body.primary_id(*id),
                kind: HirExprKind::Boundary(Box::new(HirBoundary::Children)),
            }
        }
        ast::UiNode::Error { id, .. } => HirExpr {
            hir_id: body.primary_id(*id),
            kind: HirExprKind::Error,
        },
    }
}

fn lower_nodes(body: &mut BodyLowering, nodes: &ast::Braced<ast::UiNode>) -> Vec<HirExpr> {
    match nodes.present() {
        Some(nodes) => nodes.iter().map(|node| lower_node(body, node)).collect(),
        // The `{` itself was missing — reported; an empty run is the lowering.
        None => Vec::new(),
    }
}

fn lower_element(body: &mut BodyLowering, element: &ast::ElementNode) -> HirExpr {
    let hir_id = body.primary_id(element.id);

    // The target resolves like any name. What resolves to nothing *stays*
    // unresolved (H4) — which today includes every builtin element, since the
    // builtin inventory arrives from Yel source later.
    let target = match element.name.present() {
        Some(ident) => body.resolve_callee(ident.name),
        None => {
            // An element with a hole for a name still lowers its props and
            // children so their errors surface; the target is the hole.
            crate::expr::HirCallee::Unresolved(body.lcx.sema.names.intern("<missing>"))
        }
    };

    // F13: same-named props fold into one entity, first occurrence's position.
    let mut props: Vec<HirProp> = Vec::new();
    for prop in &element.props {
        let Some(ident) = prop.name.present() else {
            // A prop with no name: reported; its value still lowers so
            // expression errors inside it surface, then it has nowhere to go.
            body.lower_expr(&prop.value);
            continue;
        };
        if !props.iter().any(|merged| merged.name == ident.name) {
            let hir_id = body.primary_id(prop.id);
            props.push(HirProp {
                hir_id,
                name: ident.name,
                getter: None,
                setter: None,
            });
        }
        match prop.modifier {
            ast::PropModifier::None => {
                let getter = body.lower_expr(&prop.value);
                let entry = props
                    .iter_mut()
                    .find(|merged| merged.name == ident.name)
                    .expect("entry exists");
                entry.getter = Some(getter);
            }
            ast::PropModifier::Set => {
                // The frozen tree extracts the closure body and silently
                // drops any other value shape — an S5 violation it lived
                // with. Reported here instead; the value still lowers.
                match &prop.value.kind {
                    ast::ExprKind::Closure(closure) => {
                        let setter = body.lower_closure(closure);
                        let entry = props
                            .iter_mut()
                            .find(|merged| merged.name == ident.name)
                            .expect("entry exists");
                        entry.setter = Some(setter);
                    }
                    _ => {
                        body.lower_expr(&prop.value);
                        let name = body.lcx.sema.names.str(ident.name).to_string();
                        body.report(
                            prop.span,
                            ErrorCode::InvalidValueBinding,
                            format!("`set {name}:` takes a closure, like `set {name}: {{ … }}`"),
                            None,
                        );
                    }
                }
            }
            ast::PropModifier::Bind => {
                // `bind value: x`  ≡  `value: x` + `set value: { }` — the
                // getter publishes the signal, the empty setter enables
                // DOM→signal auto-sync.
                let getter = body.lower_expr(&prop.value);
                let empty = HirClosure {
                    hir_id: body.invent_id(prop.id),
                    params: Vec::new(),
                    block: HirBlock {
                        stmts: Vec::new(),
                        tail: None,
                    },
                };
                let entry = props
                    .iter_mut()
                    .find(|merged| merged.name == ident.name)
                    .expect("entry exists");
                entry.getter = Some(getter);
                entry.setter = Some(empty);
            }
        }
    }

    // A setter with no getter has nothing to publish — the frozen diagnostic,
    // kept word for word.
    for prop in &props {
        if prop.setter.is_some() && prop.getter.is_none() {
            let name = body.lcx.sema.names.str(prop.name).to_string();
            let span = element
                .props
                .iter()
                .find(|candidate| {
                    candidate
                        .name
                        .present()
                        .is_some_and(|ident| ident.name == prop.name)
                })
                .map(|candidate| candidate.span)
                .unwrap_or(element.span);
            body.report(
                span,
                ErrorCode::InvalidValueBinding,
                format!(
                    "binding `{name}` has a setter but no getter; add a value binding like `{name}: <expr>`"
                ),
                None,
            );
        }
    }

    let children = element
        .children
        .iter()
        .map(|child| lower_node(body, child))
        .collect();

    HirExpr {
        hir_id,
        kind: HirExprKind::Instantiate(Box::new(HirInstantiate {
            target,
            props,
            children,
        })),
    }
}

/// UI `if` → one [`HirBoundary::Conditional`] holding a `match`. The whole
/// `else if` chain is inside it — the chain occupies one anchor in the tree —
/// nesting as plain `Match` expressions in the false arm, each level keyed to
/// its own real AST node (D7's free part: stage 1 gives every `ElseIfBranch`
/// its own id and span, so H2 holds and no diagnostic moves).
fn lower_ui_if(body: &mut BodyLowering, node: &ast::IfNode) -> HirExpr {
    let hir_id = body.primary_id(node.id);
    let scrutinee = body.lower_expr(&node.condition);
    let then_children = lower_nodes(body, &node.then_branch);
    let false_value = lower_else_chain(body, node, 0);
    let arms = vec![
        HirMatchArm {
            hir_id: body.invent_id(node.id),
            pattern: HirPattern::Bool(true),
            value: HirExpr {
                hir_id: body.invent_id(node.id),
                kind: HirExprKind::Fragment(then_children),
            },
        },
        HirMatchArm {
            hir_id: body.invent_id(node.id),
            pattern: HirPattern::Bool(false),
            value: false_value,
        },
    ];
    HirExpr {
        hir_id,
        kind: HirExprKind::Boundary(Box::new(HirBoundary::Conditional(HirMatch {
            scrutinee,
            arms,
        }))),
    }
}

/// The value of "the condition was false" at position `index` of the chain:
/// the next `else if` as a nested match, or the `else` branch, or nothing.
fn lower_else_chain(body: &mut BodyLowering, node: &ast::IfNode, index: usize) -> HirExpr {
    if let Some(branch) = node.else_if_branches.get(index) {
        let hir_id = body.primary_id(branch.id);
        let scrutinee = body.lower_expr(&branch.condition);
        let children = lower_nodes(body, &branch.body);
        let rest = lower_else_chain(body, node, index + 1);
        let arms = vec![
            HirMatchArm {
                hir_id: body.invent_id(branch.id),
                pattern: HirPattern::Bool(true),
                value: HirExpr {
                    hir_id: body.invent_id(branch.id),
                    kind: HirExprKind::Fragment(children),
                },
            },
            HirMatchArm {
                hir_id: body.invent_id(branch.id),
                pattern: HirPattern::Bool(false),
                value: rest,
            },
        ];
        return HirExpr {
            hir_id,
            kind: HirExprKind::Match(Box::new(HirMatch { scrutinee, arms })),
        };
    }
    let children = match &node.else_branch {
        Some(nodes) => lower_nodes(body, nodes),
        None => Vec::new(),
    };
    HirExpr {
        hir_id: body.invent_id(node.id),
        kind: HirExprKind::Fragment(children),
    }
}

fn lower_ui_for(body: &mut BodyLowering, node: &ast::ForNode) -> HirExpr {
    let hir_id = body.primary_id(node.id);
    let iterable = body.lower_expr(&node.iterable);
    body.push_scope();
    let binder = match node.item.present() {
        Some(ident) => body.define(ident, None),
        None => body.define_missing_binder(node.id),
    };
    let key = node.key.as_ref().map(|key| body.lower_expr(key));
    let children = match &node.body {
        ast::ForBody::Nodes(nodes) => lower_nodes(body, nodes),
        // A statement-bodied `for` cannot appear in UI position by
        // construction — the parser picks the body form from the position.
        ast::ForBody::Statements(_) => unreachable!("UI `for` with a statement body"),
    };
    body.pop_scope();
    HirExpr {
        hir_id,
        kind: HirExprKind::Boundary(Box::new(HirBoundary::Repeat(HirRepeat {
            binder,
            iterable,
            key,
            children,
        }))),
    }
}
