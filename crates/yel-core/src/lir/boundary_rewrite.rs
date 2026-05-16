//! Stage 3 of lir-resource-flatten: rewrite `LoadHandle` / `StoreHandle`
//! against `BoundaryField` slots into explicit `BoundaryStructGet` /
//! `BoundaryStructSet` ops with the boundary-ref slot resolved
//! statically at lowering time.
//!
//! Runs as a post-pass after `lower_component`. Walks each block's ops
//! linearly, mirroring the same `current_boundary_locals` tracking
//! codegen does today, and rewrites in place. Boundary IDs that come
//! from a block's `boundary_params` (passed in as WASM function params,
//! never materialized via a `BindBoundaryLocal` op) cannot be resolved
//! at the LIR layer — those uses stay as `LoadHandle` / `StoreHandle`
//! and continue to be handled by codegen's fallback chain walk. Stage 4
//! folds boundary_params into typed `block.params` so this fallback
//! disappears; Stage 5 deletes both `BoundaryField` and the codegen
//! fallback entirely.
//!
//! Conservatively only rewrites at the top level of each block plus
//! immediate children inside `If` / `Loop` bodies — we do not propagate
//! bindings across `CallBlock` boundaries.
//!
//! Invariant preserved: every LoadHandle/StoreHandle this pass replaces
//! continues to read/write the same wasm field with the same value
//! semantics (codegen of the new ops mirrors the old chain-walked
//! sequence exactly: `local.get rec; struct.get/set <ty> <field>`).

use std::collections::HashMap;

use crate::ids::TreeBoundaryId;

use super::block::{LirOp, LirSlotId, LirSlotInfo, LirSlotKind};
use super::node::LirResource;

/// Walks every block looking for `LoadHandle` / `StoreHandle` ops
/// against `BoundaryField` slots. Returns the count. After
/// [`rewrite_boundary_field_loadstore`] runs, this should be 0 — if
/// it isn't, the codegen chain walk is still live and Stage 5e
/// can't delete `BoundaryField` / `boundary_params` yet.
pub fn count_remaining_boundary_field_loadstore(component: &LirResource) -> usize {
    let mut total = 0usize;
    for block in &component.blocks {
        count_in_ops(&block.ops, &component.slots, &mut total);
    }
    total
}

fn count_in_ops(ops: &[LirOp], slots: &[LirSlotInfo], total: &mut usize) {
    for op in ops {
        match op {
            LirOp::LoadHandle { slot, .. }
            | LirOp::StoreHandle { slot, .. }
            | LirOp::LoadI32 { slot, .. }
            | LirOp::StoreI32Slot { slot, .. }
            | LirOp::StoreI32 { slot, .. } => {
                if slot_boundary_field(slots, *slot).is_some() {
                    *total += 1;
                }
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                count_in_ops(then_ops, slots, total);
                count_in_ops(else_ops, slots, total);
            }
            LirOp::Loop { body_ops, .. } => count_in_ops(body_ops, slots, total),
            _ => {}
        }
    }
}

/// Rewrite every block's ops in-place. Returns the number of
/// LoadHandle/StoreHandle pairs replaced (for telemetry / tests).
pub fn rewrite_boundary_field_loadstore(component: &mut LirResource) -> usize {
    let slots_snapshot = component.slots.clone();
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
            collect_unbound_boundary_ids(&component.blocks[bi].ops, &slots_snapshot, &current);
        let mut prologue: Vec<LirOp> = Vec::new();
        for b_id in &all_unbound {
            // Strategy 1: chain from `$self.tree` (Root) down to
            // `b_id`. Use the existing `BoundaryRefFromSelf` op,
            // which encapsulates the full walk in codegen.
            if boundary_self_walkable(&component.struct_types, *b_id) {
                let slot_id = LirSlotId::resource(component.slots.len() as u32);
                let local_idx = next_local_idx(&component.slots);
                component.slots.push(crate::lir::block::LirSlotInfo {
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
            try_synthesize_ancestor_chain(
                &component.struct_types,
                *b_id,
                &mut current,
                &mut component.slots,
                &mut prologue,
            );
        }

        // Lazy ancestor-chain synthesis inside `rewrite_ops` needs
        // mutable access to `component.slots` (to allocate fresh
        // ref slots) and read access to `component.struct_types`
        // (to walk parent links). Pass the latter by clone-free
        // immutable borrow alongside the mutable slots vec.
        let original_ops = std::mem::take(&mut component.blocks[bi].ops);
        let struct_types_snapshot = component.struct_types.clone();
        let mut new_ops = rewrite_ops(
            original_ops,
            &mut component.slots,
            &struct_types_snapshot,
            &mut current,
            &mut total,
        );
        // Prepend prologue.
        if !prologue.is_empty() {
            let mut combined = prologue;
            combined.extend(new_ops.drain(..));
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
        let slot_id = LirSlotId::resource(slots.len() as u32);
        let local_idx = next_local_idx(slots);
        slots.push(LirSlotInfo {
            id: slot_id,
            kind: LirSlotKind::Temp { local_idx },
            val_ty: crate::lir::block::LirSlotValType::RefNullForBoundary(*b_hop),
            name: Some(format!("ancestor_walk_b{}", b_hop.0)),
        });
        current.insert(*b_hop, slot_id);
        out.push(LirOp::BoundaryStructGet {
            boundary_id: parent_id,
            field_idx: *fidx_in_parent,
            rec: rec_slot,
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
    slots: &[LirSlotInfo],
    current: &HashMap<TreeBoundaryId, LirSlotId>,
) -> Vec<TreeBoundaryId> {
    let mut seen: Vec<TreeBoundaryId> = Vec::new();
    let mut bound_in_flow: HashMap<TreeBoundaryId, ()> = HashMap::new();
    walk(ops, slots, current, &mut bound_in_flow, &mut seen);
    seen
}

fn walk(
    ops: &[LirOp],
    slots: &[LirSlotInfo],
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
            LirOp::LoadHandle { slot, .. }
            | LirOp::StoreHandle { slot, .. }
            | LirOp::LoadI32 { slot, .. }
            | LirOp::StoreI32Slot { slot, .. }
            | LirOp::StoreI32 { slot, .. } => {
                if let Some((b_id, _)) = slot_boundary_field(slots, *slot) {
                    let resolvable = initial.contains_key(&b_id) || in_flow.contains_key(&b_id);
                    if !resolvable && !seen.contains(&b_id) {
                        seen.push(b_id);
                    }
                }
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                walk(then_ops, slots, initial, in_flow, seen);
                walk(else_ops, slots, initial, in_flow, seen);
            }
            LirOp::Loop { body_ops, .. } => walk(body_ops, slots, initial, in_flow, seen),
            _ => {}
        }
    }
}

fn rewrite_ops(
    ops: Vec<LirOp>,
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
        slots: &mut Vec<LirSlotInfo>,
        struct_types: &[crate::lir::struct_types::LirStructTypeDecl],
        out: &mut Vec<LirOp>,
    ) -> Option<LirSlotId> {
        if let Some(s) = current.get(&boundary_id).copied() {
            return Some(s);
        }
        try_synthesize_ancestor_chain(struct_types, boundary_id, current, slots, out);
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
            // The two rewrite targets.
            LirOp::LoadHandle { slot, to } => {
                if let Some((boundary_id, field_idx)) = slot_boundary_field(slots, slot) {
                    if let Some(rec) = resolve_rec(boundary_id, current, slots, struct_types, &mut out)
                    {
                        *total += 1;
                        out.push(LirOp::BoundaryStructGet {
                            boundary_id,
                            field_idx,
                            rec,
                            result: to,
                        });
                        continue;
                    }
                }
                out.push(LirOp::LoadHandle { slot, to });
            }
            LirOp::StoreHandle { slot, from } => {
                if let Some((boundary_id, field_idx)) = slot_boundary_field(slots, slot) {
                    if let Some(rec) = resolve_rec(boundary_id, current, slots, struct_types, &mut out)
                    {
                        *total += 1;
                        out.push(LirOp::BoundaryStructSet {
                            boundary_id,
                            field_idx,
                            rec,
                            value: from,
                        });
                        continue;
                    }
                }
                out.push(LirOp::StoreHandle { slot, from });
            }
            // Stage 5b: i32-typed reads / writes to BoundaryField
            // slots (active-tag flags, DOM-handle fields written
            // through `LoadI32` / `StoreI32Slot`). Same translation
            // as the handle-typed pair above — codegen uses the
            // same `local.get rec; struct.get/set` sequence either
            // way; the slot's `val_ty` carries the type.
            LirOp::LoadI32 { slot, to } => {
                if let Some((boundary_id, field_idx)) = slot_boundary_field(slots, slot) {
                    if let Some(rec) = resolve_rec(boundary_id, current, slots, struct_types, &mut out)
                    {
                        *total += 1;
                        out.push(LirOp::BoundaryStructGet {
                            boundary_id,
                            field_idx,
                            rec,
                            result: to,
                        });
                        continue;
                    }
                }
                out.push(LirOp::LoadI32 { slot, to });
            }
            LirOp::StoreI32Slot { slot, from } => {
                if let Some((boundary_id, field_idx)) = slot_boundary_field(slots, slot) {
                    if let Some(rec) = resolve_rec(boundary_id, current, slots, struct_types, &mut out)
                    {
                        *total += 1;
                        out.push(LirOp::BoundaryStructSet {
                            boundary_id,
                            field_idx,
                            rec,
                            value: from,
                        });
                        continue;
                    }
                }
                out.push(LirOp::StoreI32Slot { slot, from });
            }
            LirOp::StoreI32 { slot, value } => {
                if let Some((boundary_id, field_idx)) = slot_boundary_field(slots, slot) {
                    if let Some(rec) = resolve_rec(boundary_id, current, slots, struct_types, &mut out)
                    {
                        *total += 1;
                        out.push(LirOp::BoundaryStructSetConst {
                            boundary_id,
                            field_idx,
                            rec,
                            value,
                        });
                        continue;
                    }
                }
                out.push(LirOp::StoreI32 { slot, value });
            }
            // Recurse into compound op bodies. Bindings established
            // inside an `If` arm or `Loop` body are visible to
            // subsequent ops in the same body but should not leak
            // out — except they currently DO leak today (codegen's
            // current_boundary_locals never pops). Match that
            // behaviour: pass `current` through by mutable ref.
            LirOp::If {
                cond,
                then_ops,
                else_ops,
                name,
            } => {
                let then_ops = rewrite_ops(then_ops, slots, struct_types, current, total);
                let else_ops = rewrite_ops(else_ops, slots, struct_types, current, total);
                out.push(LirOp::If {
                    cond,
                    then_ops,
                    else_ops,
                    name,
                });
            }
            LirOp::Loop {
                break_cond,
                body_ops,
                name,
            } => {
                let body_ops = rewrite_ops(body_ops, slots, struct_types, current, total);
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
fn slot_boundary_field(slots: &[LirSlotInfo], slot: LirSlotId) -> Option<(TreeBoundaryId, u32)> {
    let info = slots.get(slot.legacy_u32() as usize)?;
    match info.kind {
        LirSlotKind::BoundaryField {
            boundary_id,
            field_idx,
        } => Some((boundary_id, field_idx)),
        _ => None,
    }
}

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

    /// LoadHandle/StoreHandle on BoundaryField slots inside a block
    /// rewrite to BoundaryStructGet/Set when a BindBoundaryLocal
    /// established the binding earlier.
    #[test]
    fn rewrite_after_bind() {
        let mut comp = LirResource::empty_module_carrier(Name(0));
        comp.def_id = DefId::INVALID;
        // slot 0 = ref slot, slot 1 = BoundaryField, slot 2 = result
        comp.slots = vec![
            mk_slot(0, LirSlotKind::Temp { local_idx: 0 }),
            mk_slot(
                1,
                LirSlotKind::BoundaryField {
                    boundary_id: TreeBoundaryId(0),
                    field_idx: 3,
                },
            ),
            mk_slot(2, LirSlotKind::Temp { local_idx: 1 }),
        ];
        comp.blocks = vec![LirBlock {
            id: BlockId(0),
            ops: vec![
                LirOp::BindBoundaryLocal {
                    boundary_id: TreeBoundaryId(0),
                    slot: LirSlotId::resource(0),
                },
                LirOp::LoadHandle {
                    slot: LirSlotId::resource(1),
                    to: LirSlotId::resource(2),
                },
                LirOp::StoreHandle {
                    slot: LirSlotId::resource(1),
                    from: LirSlotId::resource(2),
                },
            ],
            ..LirBlock::new(BlockId(0))
        }];

        let n = rewrite_boundary_field_loadstore(&mut comp);
        assert_eq!(n, 2, "one Load + one Store rewritten");
        let ops = &comp.blocks[0].ops;
        assert!(matches!(
            ops[1],
            LirOp::BoundaryStructGet {
                boundary_id: TreeBoundaryId(0),
                field_idx: 3,
                rec: LirSlotId::Resource { idx: 0 },
                result: LirSlotId::Resource { idx: 2 },
            }
        ));
        assert!(matches!(
            ops[2],
            LirOp::BoundaryStructSet {
                boundary_id: TreeBoundaryId(0),
                field_idx: 3,
                rec: LirSlotId::Resource { idx: 0 },
                value: LirSlotId::Resource { idx: 2 },
            }
        ));
    }

    /// LoadHandle/StoreHandle without an in-scope binding stays as-is
    /// (codegen's chain walk handles it via the legacy path).
    #[test]
    fn no_rewrite_without_bind() {
        let mut comp = LirResource::empty_module_carrier(Name(0));
        comp.slots = vec![
            mk_slot(
                0,
                LirSlotKind::BoundaryField {
                    boundary_id: TreeBoundaryId(0),
                    field_idx: 0,
                },
            ),
            mk_slot(1, LirSlotKind::Temp { local_idx: 0 }),
        ];
        comp.blocks = vec![LirBlock {
            id: BlockId(0),
            ops: vec![LirOp::LoadHandle {
                slot: LirSlotId::resource(0),
                to: LirSlotId::resource(1),
            }],
            ..LirBlock::new(BlockId(0))
        }];

        let n = rewrite_boundary_field_loadstore(&mut comp);
        assert_eq!(n, 0);
        assert!(matches!(comp.blocks[0].ops[0], LirOp::LoadHandle { .. }));
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
            // slot 1 = BoundaryField(0, 5)
            LirSlotInfo {
                id: LirSlotId::resource(1),
                kind: LirSlotKind::BoundaryField {
                    boundary_id: TreeBoundaryId(0),
                    field_idx: 5,
                },
                val_ty: LirSlotValType::I32,
                name: None,
            },
            // slot 2 = result
            LirSlotInfo {
                id: LirSlotId::resource(2),
                kind: LirSlotKind::Temp { local_idx: 1 },
                val_ty: LirSlotValType::I32,
                name: None,
            },
        ];
        comp.blocks = vec![LirBlock {
            id: BlockId(0),
            ops: vec![LirOp::LoadHandle {
                slot: LirSlotId::resource(1),
                to: LirSlotId::resource(2),
            }],
            boundary_params: vec![TreeBoundaryId(0)],
            boundary_param_slots: vec![LirSlotId::resource(0)],
            ..LirBlock::new(BlockId(0))
        }];

        let n = rewrite_boundary_field_loadstore(&mut comp);
        assert_eq!(n, 1, "boundary_param_slot binding seeds the rewrite");
        assert!(matches!(
            comp.blocks[0].ops[0],
            LirOp::BoundaryStructGet {
                boundary_id: TreeBoundaryId(0),
                field_idx: 5,
                rec: LirSlotId::Resource { idx: 0 },
                result: LirSlotId::Resource { idx: 2 },
            }
        ));
    }
}
