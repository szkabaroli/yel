//! lir-resource-flatten Stage 5e: resolve the lowerer's symbolic
//! `StructField{Get,Set,SetConst}` ops — each carrying a `struct_ty`
//! (a `TreeBoundaryId`) + `field_idx` — into concrete `StructGet` /
//! `StructSet` / `StructSetConst` ops whose `rec` (the boundary's GC
//! struct ref) is resolved at the LIR layer.
//!
//! **Coverage is total, and enforced.** Runs as a post-pass after
//! `lower_component`, which then `debug_assert!`s that
//! [`count_remaining_struct_field_ops`] is 0. Codegen has **no** fallback
//! for an unresolved symbolic op — it `unreachable!`s. The `$self.tree`
//! chain walk (`emit_boundary_ref`) survives only as the codegen of the
//! *explicit* `BoundaryRefFromSelf` op and of the `CallBlock` boundary-
//! param calling convention — never as an implicit fallback.
//!
//! Per block, the rewriter resolves each symbolic op's `struct_ty` to a
//! ref slot by, in order:
//!   1. seeding bindings from the block's `boundary_param_slots` and from
//!      in-flow `BindBoundaryLocal` / `Alloc*Boundary` ops;
//!   2. for an otherwise-unbound boundary reachable from `$self.tree`
//!      (registry `kind == Root` or a static `parent` chain), prepending
//!      a synthesized `BoundaryRefFromSelf` at the block head;
//!   3. for an unbound boundary that descends from an already-bound
//!      ancestor (typically a `boundary_param_slot`), synthesizing a
//!      `StructGet` chain from that ancestor down to it.
//! It recurses into `If` / `Loop` bodies; it does not propagate bindings
//! across `CallBlock` boundaries (the callee receives its own params).
//!
//! Invariant preserved: every op this pass replaces reads/writes the same
//! wasm field with the same value semantics (the resolved op's codegen
//! emits exactly `local.get rec; struct.get/set <ty> <field>`, with `<ty>`
//! recovered from the `rec` slot's `val_ty`).

use std::collections::HashMap;

use crate::ids::TreeBoundaryId;

use super::block::{LirIf, LirOp, LirSlotId, LirSlotInfo, LirSlotKind};
use super::node::LirResource;

/// Walks every block counting unresolved symbolic `StructField{Get,Set,
/// SetConst}` ops. After [`rewrite_struct_field_ops`] runs this
/// must be 0 — every symbolic struct-field access should have lowered to
/// a concrete `StructGet`/`StructSet`. `lower_component` `debug_assert`s
/// this; codegen `unreachable!`s on any survivor.
pub fn count_remaining_struct_field_ops(component: &LirResource) -> usize {
    let mut total = 0usize;
    for block in &component.blocks {
        count_in_ops(&block.ops, &mut total);
    }
    total
}

fn count_in_ops(ops: &[LirOp], total: &mut usize) {
    for op in ops {
        match op {
            LirOp::StructFieldGet { .. }
            | LirOp::StructFieldSet { .. }
            | LirOp::StructFieldSetConst { .. } => {
                *total += 1;
            }
            LirOp::If(if_op) => {
                count_in_ops(&if_op.then_ops, total);
                count_in_ops(&if_op.else_ops, total);
            }
            LirOp::Loop { body_ops, .. } => count_in_ops(body_ops, total),
            _ => {}
        }
    }
}

/// Rewrite every block's ops in-place. Returns the number of
/// LoadHandle/StoreHandle pairs replaced (for telemetry / tests).
pub fn rewrite_struct_field_ops(component: &mut LirResource) -> usize {
    let mut total = 0usize;
    let block_count = component.blocks.len();
    for bi in 0..block_count {
        // Per-block scope: bindings established inside one block do
        // not leak into siblings. Stage 4: seed the map from
        // `boundary_param_slots` so BoundaryField uses whose binding
        // came from a function param resolve at the LIR layer too.
        let mut current = HashMap::new();
        for (b_id, slot) in component.blocks[bi]
            .boundary_params
            .iter()
            .zip(component.blocks[bi].boundary_param_slots.iter())
        {
            current.insert(*b_id, *slot);
        }

        // Stage 5e-1 pre-pass: scan for BoundaryField uses whose
        // boundary id has no in-scope binding. For each unique
        // unbound id, allocate a fresh ref slot and prepend a
        // synthesized `BoundaryRefFromSelf` op at the block head.
        //
        // Filter: only synthesize for boundaries reachable from
        // `$self.tree` via a static parent chain — i.e. those
        // whose registry entry is `Root` or has a `parent` link.
        // `ForIterBody` boundaries are reachable only by indexing
        // into a for-anchor's children array at runtime; the
        // rewriter cannot synthesize a chain walk for them, so it
        // leaves their LoadHandle/StoreHandle as-is. Those uses
        // are expected to come with an in-flow `BindBoundaryLocal`
        // / `boundary_param_slots` binding in any well-formed
        // program — when they don't, codegen's legacy chain walk
        // also fails. The rewriter's job is to reduce surface, not
        // to fix bugs.
        let all_unbound: Vec<TreeBoundaryId> =
            collect_unbound_boundary_ids(&component.blocks[bi].ops, &current);
        // Task #105 B2: walk slots are per-block scratch — allocate them
        // on the block itself (Block-variant ids, per-block local_idx).
        // `struct_types` is read while the block's slots are mutated;
        // destructure so the borrows are disjoint fields.
        let block_id = component.blocks[bi].id;
        let mut prologue: Vec<LirOp> = Vec::new();
        for b_id in &all_unbound {
            // Strategy 1: chain from `$self.tree` (Root) down to
            // `b_id`. Use the existing `BoundaryRefFromSelf` op,
            // which encapsulates the full walk in codegen.
            if boundary_self_walkable(&component.struct_types, *b_id) {
                let block_slots = &mut component.blocks[bi].slots;
                let slot_id = LirSlotId::Block {
                    block: block_id,
                    idx: block_slots.len() as u16,
                };
                let local_idx = next_local_idx(block_slots);
                block_slots.push(crate::lir::block::LirSlotInfo {
                    id: slot_id,
                    kind: crate::lir::block::LirSlotKind::Temp { local_idx },
                    val_ty: crate::lir::block::LirSlotValType::RefNullForBoundary(*b_id),
                    name: Some(format!("self_walk_b{}", b_id.0)),
                });
                current.insert(*b_id, slot_id);
                prologue.push(LirOp::BoundaryRefFromSelf {
                    boundary_id: *b_id,
                    result: slot_id,
                });
                continue;
            }
            // Strategy 2 (task #106): try chaining from an
            // already-bound ancestor (typically a
            // `boundary_param_slot` — descendants of a ForIterBody
            // param). If none of the seeded `current` entries reach
            // `b_id`, the rewriter falls back to lazy in-flow
            // synthesis inside `rewrite_ops` (which sees additional
            // `BindBoundaryLocal` / `Alloc*Boundary` bindings).
            let (blocks, struct_types) =
                (&mut component.blocks, &component.struct_types);
            try_synthesize_ancestor_chain(
                struct_types,
                *b_id,
                &mut current,
                block_id,
                &mut blocks[bi].slots,
                &mut prologue,
            );
        }

        // Lazy ancestor-chain synthesis inside `rewrite_ops` allocates
        // fresh ref slots on the block and reads
        // `component.struct_types` to walk parent links.
        let original_ops = std::mem::take(&mut component.blocks[bi].ops);
        let (blocks, struct_types) = (&mut component.blocks, &component.struct_types);
        let mut new_ops = rewrite_ops(
            original_ops,
            block_id,
            &mut blocks[bi].slots,
            struct_types,
            &mut current,
            &mut total,
        );
        // Prepend prologue.
        if !prologue.is_empty() {
            let mut combined = prologue;
            combined.append(&mut new_ops);
            new_ops = combined;
        }
        component.blocks[bi].ops = new_ops;
    }
    total
}

/// Stage 5e-1 helper: true iff a chain walk from `$self.tree` to
/// `boundary_id` is statically resolvable — i.e. every link from
/// `boundary_id` up to the `Root` exists. ForIterBody / unrooted
/// boundaries fail somewhere along the chain and are not
/// `BoundaryRefFromSelf`-eligible.
fn boundary_self_walkable(
    struct_types: &[crate::lir::struct_types::LirStructTypeDecl],
    boundary_id: TreeBoundaryId,
) -> bool {
    use crate::lir::block::TreeBoundaryKind;
    let mut cur = boundary_id.0;
    // Bounded walk to avoid pathological cycles in malformed input.
    for _ in 0..struct_types.len() + 1 {
        let Some(decl) = struct_types.get(cur as usize) else {
            return false;
        };
        if matches!(decl.kind, TreeBoundaryKind::Root) {
            return true;
        }
        let Some(p) = decl.parent else {
            return false;
        };
        cur = p.parent.0;
    }
    false
}

/// Walk up `boundary_id`'s parent chain looking for an already-bound
/// boundary in `current`. Returns the hop list from `boundary_id`
/// (innermost) up to but not including the bound ancestor, where each
/// entry is `(child_boundary, field_idx_in_parent_struct)`. Returns
/// `None` if the walk hits a missing parent link, a cycle, or reaches
/// the top of the chain without finding a bound boundary.
///
/// Used by task #106 to synthesize a `BoundaryStructGet` chain from
/// an in-scope ancestor (typically a `boundary_param_slot`) down to a
/// statically-named descendant — e.g. an if-anchor nested inside a
/// ForIterBody param.
fn chain_from_bound_ancestor(
    struct_types: &[crate::lir::struct_types::LirStructTypeDecl],
    boundary_id: TreeBoundaryId,
    current: &HashMap<TreeBoundaryId, LirSlotId>,
) -> Option<Vec<(TreeBoundaryId, u32)>> {
    let mut hops: Vec<(TreeBoundaryId, u32)> = Vec::new();
    let mut cur = boundary_id;
    for _ in 0..struct_types.len() + 1 {
        let decl = struct_types.get(cur.index())?;
        let p = decl.parent?;
        hops.push((cur, p.field_idx));
        let parent_id = TreeBoundaryId(p.parent.0);
        if current.contains_key(&parent_id) {
            return Some(hops);
        }
        cur = parent_id;
    }
    None
}

/// Generate a chain of `BoundaryStructGet` ops walking from a bound
/// ancestor down to `b_id`. Each hop allocates a fresh ref slot
/// (registered into `current`) and appends a `BoundaryStructGet` op
/// to `out`. No-op if no ancestor in `current` reaches `b_id`.
///
/// Used both for the eager prologue (boundary_params already seeded
/// into `current` at block entry) and for lazy in-flow synthesis
/// (when `rewrite_ops` encounters an unbound use after additional
/// `BindBoundaryLocal` / `Alloc*Boundary` bindings have populated
/// `current`).
fn try_synthesize_ancestor_chain(
    struct_types: &[crate::lir::struct_types::LirStructTypeDecl],
    b_id: TreeBoundaryId,
    current: &mut HashMap<TreeBoundaryId, LirSlotId>,
    block_id: crate::ids::BlockId,
    slots: &mut Vec<LirSlotInfo>,
    out: &mut Vec<LirOp>,
) {
    let Some(hops) = chain_from_bound_ancestor(struct_types, b_id, current) else {
        return;
    };
    // `hops` is child-first; walk in reverse to step ancestor → target.
    for (b_hop, fidx_in_parent) in hops.iter().rev() {
        let parent_id = TreeBoundaryId(
            struct_types[b_hop.index()]
                .parent
                .expect("chain hop guarantees parent")
                .parent
                .0,
        );
        let rec_slot = *current
            .get(&parent_id)
            .expect("ancestor bound either pre-seed or earlier in this loop");
        // Task #105 B2: walk slots live on the block being rewritten.
        let slot_id = LirSlotId::Block {
            block: block_id,
            idx: slots.len() as u16,
        };
        let local_idx = next_local_idx(slots);
        slots.push(LirSlotInfo {
            id: slot_id,
            kind: LirSlotKind::Temp { local_idx },
            val_ty: crate::lir::block::LirSlotValType::RefNullForBoundary(*b_hop),
            name: Some(format!("ancestor_walk_b{}", b_hop.0)),
        });
        current.insert(*b_hop, slot_id);
        out.push(LirOp::StructGet {
            rec: rec_slot,
            field_idx: *fidx_in_parent,
            result: slot_id,
        });
    }
}

fn next_local_idx(slots: &[LirSlotInfo]) -> u32 {
    slots
        .iter()
        .filter_map(|s| match s.kind {
            LirSlotKind::Temp { local_idx } => Some(local_idx + 1),
            _ => None,
        })
        .max()
        .unwrap_or(0)
}

/// Walk `ops` collecting boundary ids that are read/written via a
/// BoundaryField slot but have no entry in `current`. Returns each
/// unique id once, in first-seen order. Mirrors `rewrite_ops`'s
/// nested traversal for `If`/`Loop` bodies, but does NOT track
/// in-flow `BindBoundaryLocal` / `Alloc*Boundary` bindings — the
/// caller-side `current` already encodes everything resolvable
/// statically; whatever this scan returns is genuinely unbound.
fn collect_unbound_boundary_ids(
    ops: &[LirOp],
    current: &HashMap<TreeBoundaryId, LirSlotId>,
) -> Vec<TreeBoundaryId> {
    let mut seen: Vec<TreeBoundaryId> = Vec::new();
    let mut bound_in_flow: HashMap<TreeBoundaryId, ()> = HashMap::new();
    walk(ops, current, &mut bound_in_flow, &mut seen);
    seen
}

fn walk(
    ops: &[LirOp],
    initial: &HashMap<TreeBoundaryId, LirSlotId>,
    in_flow: &mut HashMap<TreeBoundaryId, ()>,
    seen: &mut Vec<TreeBoundaryId>,
) {
    for op in ops {
        match op {
            LirOp::BindBoundaryLocal { boundary_id, .. }
            | LirOp::AllocBoundary { boundary_id, .. }
            | LirOp::AllocSubBoundary { boundary_id, .. } => {
                in_flow.insert(*boundary_id, ());
            }
            LirOp::StructFieldGet { struct_ty, .. }
            | LirOp::StructFieldSet { struct_ty, .. }
            | LirOp::StructFieldSetConst { struct_ty, .. } => {
                let b_id = *struct_ty;
                let resolvable = initial.contains_key(&b_id) || in_flow.contains_key(&b_id);
                if !resolvable && !seen.contains(&b_id) {
                    seen.push(b_id);
                }
            }
            LirOp::If(if_op) => {
                walk(&if_op.then_ops, initial, in_flow, seen);
                walk(&if_op.else_ops, initial, in_flow, seen);
            }
            LirOp::Loop { body_ops, .. } => walk(body_ops, initial, in_flow, seen),
            _ => {}
        }
    }
}

fn rewrite_ops(
    ops: Vec<LirOp>,
    block_id: crate::ids::BlockId,
    slots: &mut Vec<LirSlotInfo>,
    struct_types: &[crate::lir::struct_types::LirStructTypeDecl],
    current: &mut HashMap<TreeBoundaryId, LirSlotId>,
    total: &mut usize,
) -> Vec<LirOp> {
    // Resolve a BoundaryField use's `rec` slot, lazily synthesizing
    // an ancestor chain into `out` if no direct binding exists in
    // `current`. Returns the rec slot to use, or `None` if no
    // ancestor in scope reaches `boundary_id` (caller should leave
    // the op as-is — codegen will panic, surfacing the bug).
    fn resolve_rec(
        boundary_id: TreeBoundaryId,
        current: &mut HashMap<TreeBoundaryId, LirSlotId>,
        block_id: crate::ids::BlockId,
        slots: &mut Vec<LirSlotInfo>,
        struct_types: &[crate::lir::struct_types::LirStructTypeDecl],
        out: &mut Vec<LirOp>,
    ) -> Option<LirSlotId> {
        if let Some(s) = current.get(&boundary_id).copied() {
            return Some(s);
        }
        try_synthesize_ancestor_chain(struct_types, boundary_id, current, block_id, slots, out);
        current.get(&boundary_id).copied()
    }

    let mut out = Vec::with_capacity(ops.len());
    for op in ops {
        match op {
            // Bindings — track and pass through unchanged. Both
            // BindBoundaryLocal and Alloc*Boundary establish the
            // mapping `boundary_id -> ref_slot`.
            LirOp::BindBoundaryLocal { boundary_id, slot } => {
                current.insert(boundary_id, slot);
                out.push(LirOp::BindBoundaryLocal { boundary_id, slot });
            }
            LirOp::AllocBoundary {
                boundary_id,
                ref_slot,
            } => {
                current.insert(boundary_id, ref_slot);
                out.push(LirOp::AllocBoundary {
                    boundary_id,
                    ref_slot,
                });
            }
            LirOp::AllocSubBoundary {
                boundary_id,
                ref_slot,
            } => {
                current.insert(boundary_id, ref_slot);
                out.push(LirOp::AllocSubBoundary {
                    boundary_id,
                    ref_slot,
                });
            }
            // Symbolic struct-field ops (Stage 5e-4): the lowerer emits
            // these directly with `struct_ty` (= boundary id). Resolve the
            // rec and lower to the generic `Struct{Get,Set,SetConst}`.
            // Unresolvable uses pass through unchanged (counted as remaining).
            LirOp::StructFieldGet {
                struct_ty,
                field_idx,
                result,
            } => {
                if let Some(rec) = resolve_rec(struct_ty, current, block_id, slots, struct_types, &mut out) {
                    *total += 1;
                    out.push(LirOp::StructGet {
                        rec,
                        field_idx,
                        result,
                    });
                    continue;
                }
                out.push(LirOp::StructFieldGet {
                    struct_ty,
                    field_idx,
                    result,
                });
            }
            LirOp::StructFieldSet {
                struct_ty,
                field_idx,
                value,
            } => {
                if let Some(rec) = resolve_rec(struct_ty, current, block_id, slots, struct_types, &mut out) {
                    *total += 1;
                    out.push(LirOp::StructSet {
                        rec,
                        field_idx,
                        value,
                    });
                    continue;
                }
                out.push(LirOp::StructFieldSet {
                    struct_ty,
                    field_idx,
                    value,
                });
            }
            LirOp::StructFieldSetConst {
                struct_ty,
                field_idx,
                value,
            } => {
                if let Some(rec) = resolve_rec(struct_ty, current, block_id, slots, struct_types, &mut out) {
                    *total += 1;
                    out.push(LirOp::StructSetConst {
                        rec,
                        field_idx,
                        value,
                    });
                    continue;
                }
                out.push(LirOp::StructFieldSetConst {
                    struct_ty,
                    field_idx,
                    value,
                });
            }
            // Recurse into compound op bodies. Bindings established
            // inside an `If` arm or `Loop` body are visible to
            // subsequent ops in the same body but should not leak
            // out — except they currently DO leak today (codegen's
            // current_boundary_locals never pops). Match that
            // behaviour: pass `current` through by mutable ref.
            LirOp::If(if_op) => {
                let LirIf {
                    cond,
                    then_ops,
                    else_ops,
                    name,
                } = *if_op;
                let then_ops = rewrite_ops(then_ops, block_id, slots, struct_types, current, total);
                let else_ops = rewrite_ops(else_ops, block_id, slots, struct_types, current, total);
                out.push(LirOp::If(Box::new(LirIf {
                    cond,
                    then_ops,
                    else_ops,
                    name,
                })));
            }
            LirOp::Loop {
                break_cond,
                body_ops,
                name,
            } => {
                let body_ops = rewrite_ops(body_ops, block_id, slots, struct_types, current, total);
                out.push(LirOp::Loop {
                    break_cond,
                    body_ops,
                    name,
                });
            }
            other => out.push(other),
        }
    }
    out
}

/// Returns the boundary-id + field-idx if `slot` is a `BoundaryField`
/// slot, otherwise None.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ids::{BlockId, DefId, TreeBoundaryId};
    use crate::interner::Name;
    use crate::lir::block::{LirBlock, LirSlotKind, LirSlotValType};

    fn mk_slot(id_n: u32, kind: LirSlotKind) -> LirSlotInfo {
        LirSlotInfo {
            id: LirSlotId::resource(id_n),
            kind,
            val_ty: LirSlotValType::I32,
            name: None,
        }
    }

    /// Symbolic `StructFieldGet`/`StructFieldSet` ops rewrite to the
    /// concrete `StructGet`/`StructSet` when a `BindBoundaryLocal`
    /// established the boundary's ref binding earlier.
    #[test]
    fn rewrite_after_bind() {
        let mut comp = LirResource::empty_module_carrier(Name(0));
        comp.def_id = DefId::INVALID;
        // slot 0 = ref slot, slot 1 = result
        comp.slots = vec![
            mk_slot(0, LirSlotKind::Temp { local_idx: 0 }),
            mk_slot(1, LirSlotKind::Temp { local_idx: 1 }),
        ];
        comp.blocks = vec![LirBlock {
            id: BlockId(0),
            ops: vec![
                LirOp::BindBoundaryLocal {
                    boundary_id: TreeBoundaryId(0),
                    slot: LirSlotId::resource(0),
                },
                LirOp::StructFieldGet {
                    struct_ty: TreeBoundaryId(0),
                    field_idx: 3,
                    result: LirSlotId::resource(1),
                },
                LirOp::StructFieldSet {
                    struct_ty: TreeBoundaryId(0),
                    field_idx: 3,
                    value: LirSlotId::resource(1),
                },
            ],
            ..LirBlock::new(BlockId(0))
        }];

        let n = rewrite_struct_field_ops(&mut comp);
        assert_eq!(n, 2, "one Get + one Set rewritten");
        let ops = &comp.blocks[0].ops;
        assert!(matches!(
            ops[1],
            LirOp::StructGet {
                rec: LirSlotId::Resource { idx: 0 },
                field_idx: 3,
                result: LirSlotId::Resource { idx: 1 },
            }
        ));
        assert!(matches!(
            ops[2],
            LirOp::StructSet {
                rec: LirSlotId::Resource { idx: 0 },
                field_idx: 3,
                value: LirSlotId::Resource { idx: 1 },
            }
        ));
    }

    /// A symbolic op with no in-scope binding and an unrooted boundary
    /// stays as-is (counted as remaining; codegen would surface it).
    #[test]
    fn no_rewrite_without_bind() {
        let mut comp = LirResource::empty_module_carrier(Name(0));
        comp.slots = vec![mk_slot(0, LirSlotKind::Temp { local_idx: 0 })];
        comp.blocks = vec![LirBlock {
            id: BlockId(0),
            ops: vec![LirOp::StructFieldGet {
                struct_ty: TreeBoundaryId(0),
                field_idx: 0,
                result: LirSlotId::resource(0),
            }],
            ..LirBlock::new(BlockId(0))
        }];

        let n = rewrite_struct_field_ops(&mut comp);
        assert_eq!(n, 0);
        assert!(matches!(
            comp.blocks[0].ops[0],
            LirOp::StructFieldGet { .. }
        ));
    }
}

#[cfg(test)]
mod stage4_tests {
    use super::*;
    use crate::ids::{BlockId, TreeBoundaryId};
    use crate::interner::Name;
    use crate::lir::block::{LirBlock, LirSlotKind, LirSlotValType};

    #[test]
    fn seed_from_boundary_param_slots() {
        // A block whose only "binding" is a boundary_param + parallel
        // slot — no BindBoundaryLocal or AllocBoundary in the ops.
        // Stage 4's seeding makes the rewrite fire anyway.
        let mut comp = LirResource::empty_module_carrier(Name(0));
        comp.slots = vec![
            // slot 0 = boundary param ref slot
            LirSlotInfo {
                id: LirSlotId::resource(0),
                kind: LirSlotKind::Temp { local_idx: 0 },
                val_ty: LirSlotValType::RefNullForBoundary(TreeBoundaryId(0)),
                name: None,
            },
            // slot 1 = result
            LirSlotInfo {
                id: LirSlotId::resource(1),
                kind: LirSlotKind::Temp { local_idx: 1 },
                val_ty: LirSlotValType::I32,
                name: None,
            },
        ];
        comp.blocks = vec![LirBlock {
            id: BlockId(0),
            ops: vec![LirOp::StructFieldGet {
                struct_ty: TreeBoundaryId(0),
                field_idx: 5,
                result: LirSlotId::resource(1),
            }],
            boundary_params: vec![TreeBoundaryId(0)],
            boundary_param_slots: vec![LirSlotId::resource(0)],
            ..LirBlock::new(BlockId(0))
        }];

        let n = rewrite_struct_field_ops(&mut comp);
        assert_eq!(n, 1, "boundary_param_slot binding seeds the rewrite");
        assert!(matches!(
            comp.blocks[0].ops[0],
            LirOp::StructGet {
                rec: LirSlotId::Resource { idx: 0 },
                field_idx: 5,
                result: LirSlotId::Resource { idx: 1 },
            }
        ));
    }
}
