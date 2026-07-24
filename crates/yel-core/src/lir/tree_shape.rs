//! Synthesis of the concrete-typed mount-tree shape from a component's
//! body tree. Produces a `ComponentTreeShape` consumed downstream by:
//! - GC type emission (one struct/array type per boundary)
//! - Slot allocation (typed-walk paths to each persistent state field)
//! - Mount/effect emission (typed `struct.new` / `struct.get` sequences)

use rustc_hash::FxHashMap as HashMap;

use crate::ids::{IfId, NodeId, TreeBoundaryId};
use crate::types::Ty;

use super::block::{
    ComponentTreeShape, LirSlotValType, NodeFieldRef, TreeBoundary, TreeBoundaryKind, TreeFieldDecl,
};
use super::expr::{LirExpr, LirExprKind};
use super::node::{LirNode, LirNodeKind};

/// Classifies the source of a `for` loop's iter-body item binding for
/// the type synthesis callback. This lets the callback choose between
/// "byte address into linear memory" (Range / ListMemory — today's
/// behavior) and "typed GC array element" (ListGc — Phase 5b-v.3+).
///
/// 5b-v.1 introduces this enum as pure plumbing — the callback ignores
/// the discriminant. 5b-v.3 will branch on it to flip lists to GC.
#[derive(Debug, Clone, Copy)]
pub enum IterSource {
    /// `0..n` — item bound to a byte address pointing into a shared
    /// scratch buffer (`range_item_buf`).
    Range,
    /// `for x in some_signal` where `some_signal: list<...>` is stored
    /// in linear memory as a fat-ptr `(ptr, len)`. Item bound to a byte
    /// address `ptr + index * elem_size`.
    ListMemory,
    /// `for x in some_signal` where `some_signal: list<...>` is stored
    /// as a typed GC array. Item bound directly to the array element
    /// value (Phase 5b-v.3+).
    ListGc,
}

/// Walk a component's `body_tree` and synthesize the per-position type
/// shape. Returns a `ComponentTreeShape` whose boundaries are the GC
/// struct types that will be emitted for this component.
///
/// `ty_to_slot_val_ty` resolves a loop variable's `Ty` to its iter-body
/// field's `SlotValType` (records / tuples are pointer-passed as `I32`;
/// primitives use their native ValType). The caller threads in their
/// `CompilerContext` to perform the lookup.
pub fn synthesize(
    body_tree: &[LirNode],
    mut ty_to_slot_val_ty: impl FnMut(Ty, IterSource) -> LirSlotValType,
    mut classify_iter_source: impl FnMut(&LirExpr) -> IterSource,
) -> ComponentTreeShape {
    let mut s = Synth::default();
    let root_id = s.alloc_boundary(TreeBoundaryKind::Root);
    s.fill_boundary(
        root_id,
        body_tree,
        &mut ty_to_slot_val_ty,
        &mut classify_iter_source,
    );
    ComponentTreeShape {
        boundaries: s.boundaries,
        root_idx: root_id.0,
        node_field: s.node_field,
    }
}

#[derive(Default)]
struct Synth {
    boundaries: Vec<TreeBoundary>,
    next_if_id: u32,
    /// Monotonic counter for unique field name suffixes within the
    /// component (keeps names deterministic and unambiguous when the
    /// same tag appears multiple times in different boundaries).
    next_field_seq: u32,
    node_field: HashMap<NodeId, NodeFieldRef>,
}

impl Synth {
    fn alloc_boundary(&mut self, kind: TreeBoundaryKind) -> TreeBoundaryId {
        let id = TreeBoundaryId(self.boundaries.len() as u32);
        self.boundaries.push(TreeBoundary {
            id,
            kind,
            fields: Vec::new(),
            parent_link: None,
        });
        id
    }

    fn set_parent_link(
        &mut self,
        boundary_id: TreeBoundaryId,
        parent: TreeBoundaryId,
        field_idx: u32,
    ) {
        self.boundaries[boundary_id.index()].parent_link = Some((parent, field_idx));
    }

    fn push_field(&mut self, b: TreeBoundaryId, f: TreeFieldDecl) {
        self.boundaries[b.index()].fields.push(f);
    }

    fn fresh_seq(&mut self) -> u32 {
        let n = self.next_field_seq;
        self.next_field_seq += 1;
        n
    }

    fn mint_if_id(&mut self) -> IfId {
        let id = IfId(self.next_if_id);
        self.next_if_id += 1;
        id
    }

    /// Walk `nodes` and add their fields to `parent` (a Root, IfBranch, or
    /// ForIterBody boundary). Element children are flattened into the
    /// parent boundary — only `if` / `for` introduce sub-boundaries.
    fn fill_boundary(
        &mut self,
        parent: TreeBoundaryId,
        nodes: &[LirNode],
        ty_to_slot_val_ty: &mut dyn FnMut(Ty, IterSource) -> LirSlotValType,
        classify_iter_source: &mut dyn FnMut(&LirExpr) -> IterSource,
    ) {
        for node in nodes {
            self.lower_node(parent, node, ty_to_slot_val_ty, classify_iter_source);
        }
    }

    fn lower_node(
        &mut self,
        parent: TreeBoundaryId,
        node: &LirNode,
        ty_to_slot_val_ty: &mut dyn FnMut(Ty, IterSource) -> LirSlotValType,
        classify_iter_source: &mut dyn FnMut(&LirExpr) -> IterSource,
    ) {
        match &node.kind {
            LirNodeKind::Element {
                tag,
                children,
                dynamic_binding_ids,
                ..
            } => {
                // Only reserve a DomHandle field when the element needs
                // persistent state in the boundary struct. Today that's
                // any element with reactive (dynamic) bindings — its
                // update effect re-reads the handle to call
                // `set-attribute`. Elements with no dynamic state
                // (purely-static elements like `VStack`, `Text "hi"`,
                // event-only handlers) need only a transient mount-time
                // local, not a struct field. Skipping them removes
                // ~7 unused i32 fields per typical iter-body.
                if !dynamic_binding_ids.is_empty() {
                    let n = self.fresh_seq();
                    let name = format!("{}_{}", tag.to_lowercase(), n);
                    let field_idx = self.boundaries[parent.index()].fields.len() as u32;
                    self.push_field(parent, TreeFieldDecl::DomHandle { name });
                    self.node_field.insert(
                        node.id,
                        NodeFieldRef {
                            owning_boundary: parent,
                            field_idx,
                        },
                    );
                }
                // Element children flatten into the same boundary.
                for child in children {
                    self.lower_node(parent, child, ty_to_slot_val_ty, classify_iter_source);
                }
            }
            LirNodeKind::StaticText(_) => {
                // No persistent state.
            }
            LirNodeKind::DynamicText { .. } => {
                let n = self.fresh_seq();
                let field_idx = self.boundaries[parent.index()].fields.len() as u32;
                self.push_field(
                    parent,
                    TreeFieldDecl::DomHandle {
                        name: format!("text_{}", n),
                    },
                );
                self.node_field.insert(
                    node.id,
                    NodeFieldRef {
                        owning_boundary: parent,
                        field_idx,
                    },
                );
            }
            LirNodeKind::If {
                then_branch,
                else_if_branches,
                else_branch,
                ..
            } => {
                let if_id = self.mint_if_id();
                let anchor_id = self.alloc_boundary(TreeBoundaryKind::IfAnchor {
                    if_id,
                    branches: Vec::new(),
                });

                // Reference the anchor from the parent boundary first.
                let n = self.fresh_seq();
                let parent_field_idx = self.boundaries[parent.index()].fields.len() as u32;
                self.push_field(
                    parent,
                    TreeFieldDecl::SubBoundary {
                        name: format!("if_{}", n),
                        target_idx: anchor_id.0,
                    },
                );
                self.node_field.insert(
                    node.id,
                    NodeFieldRef {
                        owning_boundary: parent,
                        field_idx: parent_field_idx,
                    },
                );
                self.set_parent_link(anchor_id, parent, parent_field_idx);

                // Anchor's own fields: parent handle + anchor handle +
                // active tag. `parent` (field 0) is the if's DOM
                // parent — where branches insert/remove content.
                // `anchor` (field 1) is the comment node used as the
                // insertion-after target. `active` (field 2) is the
                // currently-mounted branch tag.
                self.push_field(
                    anchor_id,
                    TreeFieldDecl::DomHandle {
                        name: "parent".to_string(),
                    },
                );
                self.push_field(
                    anchor_id,
                    TreeFieldDecl::DomHandle {
                        name: "anchor".to_string(),
                    },
                );
                self.push_field(
                    anchor_id,
                    TreeFieldDecl::ActiveTag {
                        name: "active".to_string(),
                    },
                );

                // For each branch: allocate, link via SubBoundary on
                // anchor, push the `content` DomHandle field at index
                // 0 of the branch, then fill body.
                let mut branch_ids: Vec<u32> = Vec::new();
                let alloc_branch = |this: &mut Self,
                                    branch_idx: u32,
                                    name: String,
                                    body: &[LirNode],
                                    ty_to_slot_val_ty: &mut dyn FnMut(
                    Ty,
                    IterSource,
                )
                    -> LirSlotValType,
                                    classify_iter_source: &mut dyn FnMut(
                    &LirExpr,
                )
                    -> IterSource| {
                    let bid = this.alloc_boundary(TreeBoundaryKind::IfBranch { if_id, branch_idx });
                    let field_idx = this.boundaries[anchor_id.index()].fields.len() as u32;
                    this.push_field(
                        anchor_id,
                        TreeFieldDecl::SubBoundary {
                            name,
                            target_idx: bid.0,
                        },
                    );
                    this.set_parent_link(bid, anchor_id, field_idx);
                    this.push_field(
                        bid,
                        TreeFieldDecl::DomHandle {
                            name: "wrapper".to_string(),
                        },
                    );
                    this.fill_boundary(bid, body, ty_to_slot_val_ty, classify_iter_source);
                    bid
                };

                let then_id = alloc_branch(
                    self,
                    0,
                    "branch_then".to_string(),
                    then_branch,
                    ty_to_slot_val_ty,
                    classify_iter_source,
                );
                branch_ids.push(then_id.0);

                for (i, (_, body)) in else_if_branches.iter().enumerate() {
                    let idx = (i as u32) + 1;
                    let bid = alloc_branch(
                        self,
                        idx,
                        format!("branch_else_if_{}", i),
                        body,
                        ty_to_slot_val_ty,
                        classify_iter_source,
                    );
                    branch_ids.push(bid.0);
                }

                if let Some(else_body) = else_branch {
                    let idx = (else_if_branches.len() as u32) + 1;
                    let bid = alloc_branch(
                        self,
                        idx,
                        "branch_else".to_string(),
                        else_body,
                        ty_to_slot_val_ty,
                        classify_iter_source,
                    );
                    branch_ids.push(bid.0);
                }

                // Patch in the now-known branch ids.
                if let TreeBoundaryKind::IfAnchor { branches, .. } =
                    &mut self.boundaries[anchor_id.index()].kind
                {
                    *branches = branch_ids;
                }
            }
            LirNodeKind::For {
                for_id,
                item_name,
                item_ty,
                iterable,
                body,
                ..
            } => {
                // Classify the iter source for the val_ty callback.
                // Phase 5b-v.3: caller-provided closure determines whether
                // a list iterable is GC-backed (ListGc) or memory-backed
                // (ListMemory). Range stays Range.
                let iter_src = match &iterable.kind {
                    LirExprKind::Range { .. } => IterSource::Range,
                    _ => classify_iter_source(iterable),
                };
                // Pre-allocate iter-body boundary; we need its id for
                // the anchor's children-array element type.
                let iter_id =
                    self.alloc_boundary(TreeBoundaryKind::ForIterBody { for_id: *for_id });

                let anchor_id = self.alloc_boundary(TreeBoundaryKind::ForAnchor {
                    for_id: *for_id,
                    iter_body_idx: iter_id.0,
                });

                // Reference the anchor from the parent boundary FIRST
                // so the anchor's parent_walk is recorded.
                let n = self.fresh_seq();
                let parent_field_idx = self.boundaries[parent.index()].fields.len() as u32;
                self.push_field(
                    parent,
                    TreeFieldDecl::SubBoundary {
                        name: format!("for_{}", n),
                        target_idx: anchor_id.0,
                    },
                );
                self.node_field.insert(
                    node.id,
                    NodeFieldRef {
                        owning_boundary: parent,
                        field_idx: parent_field_idx,
                    },
                );
                self.set_parent_link(anchor_id, parent, parent_field_idx);

                // Anchor fields: parent handle, anchor handle, children array.
                self.push_field(
                    anchor_id,
                    TreeFieldDecl::DomHandle {
                        name: "parent".to_string(),
                    },
                );
                self.push_field(
                    anchor_id,
                    TreeFieldDecl::DomHandle {
                        name: "anchor".to_string(),
                    },
                );
                self.push_field(
                    anchor_id,
                    TreeFieldDecl::ChildrenArray {
                        name: "children".to_string(),
                        arr_target_idx: iter_id.0,
                    },
                );

                // Iter-body field layout:
                //   field 0 — `LoopVar`: the per-iter `item_ptr`
                //     (address-of-buf for ranges, pointer-into-list
                //     for lists). Nested fors read this for outer-
                //     item access; the in-body expression layer
                //     treats the value as an address and dereferences
                //     it.
                //   field 1 — `wrapper`: DOM handle of the host
                //     fragment element (`yel-frag`) that the iter
                //     mount block creates and inserts as a sibling of
                //     the for-anchor. All iter content is appended as
                //     children of the wrapper, so a single Remove
                //     cascades — handles every body shape uniformly
                //     (Element-first, DynamicText-first, If-first,
                //     For-first).
                //   field 2 — `loop_var_value`: per-iter VALUE for
                //     range fors only. Fan-out wraps re-seed the
                //     shared `range_item_buf` from this field; lists
                //     leave it default-zero. Always present so field
                //     indices stay shape-uniform.
                //   fields 3+ — body-emitted DomHandle / SubBoundary
                //     entries from `fill_boundary`.
                let val_ty = ty_to_slot_val_ty(*item_ty, iter_src);
                self.push_field(
                    iter_id,
                    TreeFieldDecl::LoopVar {
                        name: format!("item_{}", item_name.0),
                        val_ty,
                    },
                );
                self.push_field(
                    iter_id,
                    TreeFieldDecl::DomHandle {
                        name: "wrapper".to_string(),
                    },
                );
                self.push_field(
                    iter_id,
                    TreeFieldDecl::DomHandle {
                        name: "loop_var_value".to_string(),
                    },
                );
                self.fill_boundary(iter_id, body, ty_to_slot_val_ty, classify_iter_source);
            }
            LirNodeKind::ChildrenSlot => {
                // No persistent state synthesized here — caller-children
                // attach under the ambient parent slot at mount time.
            }
        }
    }
}
