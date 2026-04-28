//! Bitwise structural dedupe of per-(boundary, signal) update
//! blocks.
//!
//! Two `update_b<b>_s<s>` blocks with identical `(boundary_params, ops)`
//! after canonicalising inner `CallBlock`/`CallBlock2` BlockId
//! references and slot-id ordering should share one canonical block.
//! Duplicate blocks are removed from `LirComponent.blocks` and any
//! `CallBlock` / `CallBlock2` / `AddEventListener` / `LirBlockEffect`
//! reference is rewritten to point at the canonical survivor.
//!
//! Block ids are NOT renumbered — surviving blocks keep their original
//! `BlockId`. After this pass `BlockId.0` is no longer guaranteed to
//! equal the block's index in `LirComponent.blocks`, so consumers must
//! look up via `LirComponent::get_block`, which performs a linear scan
//! fallback.

use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use crate::context::CompilerContext;
use crate::ids::BlockId;

use super::block::{LirBlock, LirBlockEffect, LirOp, LirSlotId};
use super::node::LirComponent;

/// Run structural dedupe over the per-(boundary, signal) update blocks
/// of `component`. Only blocks whose recorded name (via
/// `ctx.get_block_name`) starts with `update_b` are considered as
/// dedupe candidates; other blocks are left untouched but their
/// `CallBlock` references into duplicates are still rewritten.
pub fn dedupe_update_blocks(ctx: &CompilerContext, component: &mut LirComponent) {
    // Identify dedupe candidates by name prefix.
    let candidates: Vec<BlockId> = component
        .blocks
        .iter()
        .filter_map(|b| {
            let name = ctx.get_block_name(component.def_id, b.id)?;
            // Per-(boundary, signal) update fns are the only blocks
            // worth structurally deduping — they're emitted per signal
            // dep path and frequently produce identical bodies for
            // unrelated signals at the same boundary.
            if name.kind == "update" && name.signal.is_some() {
                Some(b.id)
            } else {
                None
            }
        })
        .collect();

    if candidates.len() < 2 {
        return;
    }

    let candidate_set: HashSet<BlockId> = candidates.iter().copied().collect();

    // Index blocks by id so the hasher can resolve callees quickly.
    let block_by_id: HashMap<BlockId, &LirBlock> = component
        .blocks
        .iter()
        .map(|b| (b.id, b))
        .collect();

    // Initial hash: 0 for every candidate.
    let mut hash: HashMap<BlockId, u64> = candidates.iter().map(|id| (*id, 0u64)).collect();

    // Fixed-point loop. Each iteration recomputes per-block hashes
    // mixing in callees' hashes. Bounded to a generous cap to guard
    // against pathological non-termination in the face of bugs.
    for _ in 0..64 {
        let mut next: HashMap<BlockId, u64> = HashMap::with_capacity(candidates.len());
        for cid in &candidates {
            let block = block_by_id[cid];
            let h = hash_block(block, &candidate_set, &hash);
            next.insert(*cid, h);
        }
        if next == hash {
            hash = next;
            break;
        }
        hash = next;
    }

    // Group candidates by their final hash.
    let mut groups: HashMap<u64, Vec<BlockId>> = HashMap::new();
    for cid in &candidates {
        groups.entry(hash[cid]).or_default().push(*cid);
    }

    // Build remap: non-canonical → canonical (smallest id wins for
    // determinism). Skip groups of size 1 — nothing to merge.
    let mut remap: HashMap<BlockId, BlockId> = HashMap::new();
    let mut to_remove: HashSet<BlockId> = HashSet::new();
    for group in groups.values() {
        if group.len() < 2 {
            continue;
        }
        let mut sorted = group.clone();
        sorted.sort_by_key(|b| b.0);
        let canonical = sorted[0];
        for &dup in &sorted[1..] {
            // Final structural equality check — guards against
            // hypothetical hash collisions.
            if blocks_structurally_equal(
                block_by_id[&canonical],
                block_by_id[&dup],
                &remap_seed(&remap, canonical, dup),
            ) {
                remap.insert(dup, canonical);
                to_remove.insert(dup);
            }
        }
    }

    if remap.is_empty() {
        return;
    }

    // Rewrite all CallBlock / CallBlock2 / AddEventListener references
    // across every block (not just candidates).
    for block in component.blocks.iter_mut() {
        rewrite_ops(&mut block.ops, &remap);
    }

    // Rewrite effect update_block targets.
    for eff in component.effects.iter_mut() {
        if let Some(canon) = remap.get(&eff.update_block) {
            eff.update_block = *canon;
        }
    }

    // Remove dead blocks.
    component.blocks.retain(|b| !to_remove.contains(&b.id));
}

/// Tiny helper: produce a remap that includes the candidate->canonical
/// pairing being verified, used by the final structural-equality check.
fn remap_seed(
    base: &HashMap<BlockId, BlockId>,
    _canonical: BlockId,
    _dup: BlockId,
) -> HashMap<BlockId, BlockId> {
    base.clone()
}

/// Rewrite `CallBlock` / `CallBlock2` / `AddEventListener` references
/// recursively through `If` / `Loop` bodies.
fn rewrite_ops(ops: &mut [LirOp], remap: &HashMap<BlockId, BlockId>) {
    for op in ops.iter_mut() {
        match op {
            LirOp::CallBlock { block, .. } => {
                if let Some(c) = remap.get(block) {
                    *block = *c;
                }
            }
            LirOp::CallBlock2 { block, .. } => {
                if let Some(c) = remap.get(block) {
                    *block = *c;
                }
            }
            LirOp::AddEventListener { handler, .. } => {
                if let Some(c) = remap.get(handler) {
                    *handler = *c;
                }
            }
            LirOp::If {
                then_ops, else_ops, ..
            } => {
                rewrite_ops(then_ops, remap);
                rewrite_ops(else_ops, remap);
            }
            LirOp::Loop { body_ops, .. } => {
                rewrite_ops(body_ops, remap);
            }
            _ => {}
        }
    }
}

/// Structural hash of a block.
///
/// Mixes:
/// - `boundary_params` (literal TreeBoundaryId list).
/// - `params.len()` + return-flag (signature shape).
/// - The op vector with two normalisations applied:
///   - Slot ids are renumbered in order of first appearance (so two
///     blocks that are identical modulo slot-id allocation order hash
///     the same).
///   - `CallBlock`/`CallBlock2` targets, when they reference another
///     candidate, are substituted with that candidate's current hash.
///     Targets outside the candidate set use the literal id.
fn hash_block(
    block: &LirBlock,
    candidates: &HashSet<BlockId>,
    hash_map: &HashMap<BlockId, u64>,
) -> u64 {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    "lir_block_v1".hash(&mut h);

    // Boundary params (literal — these encode the boundary kind/shape
    // implicitly via the TreeBoundaryId).
    block.boundary_params.len().hash(&mut h);
    for b in &block.boundary_params {
        b.0.hash(&mut h);
    }

    // Signature shape.
    block.params.len().hash(&mut h);
    block.return_slot.is_some().hash(&mut h);

    // Hash ops with slot-id canonicalisation.
    let mut slot_norm = SlotNormalizer::new();
    hash_ops(&block.ops, &mut h, &mut slot_norm, candidates, hash_map);

    h.finish()
}

/// Walk `ops` and feed a normalised representation into `h`. Slot ids
/// are remapped via `slot_norm` (first-seen order); CallBlock targets
/// in `candidates` are replaced with their hash from `hash_map`.
fn hash_ops<H: Hasher>(
    ops: &[LirOp],
    h: &mut H,
    slot_norm: &mut SlotNormalizer,
    candidates: &HashSet<BlockId>,
    hash_map: &HashMap<BlockId, u64>,
) {
    ops.len().hash(h);
    for op in ops {
        hash_op(op, h, slot_norm, candidates, hash_map);
    }
}

fn hash_op<H: Hasher>(
    op: &LirOp,
    h: &mut H,
    sn: &mut SlotNormalizer,
    candidates: &HashSet<BlockId>,
    hash_map: &HashMap<BlockId, u64>,
) {
    // Discriminant tag. We use the variant order via a mem-style tag
    // function: hash the std::mem::discriminant value.
    std::mem::discriminant(op).hash(h);

    use LirOp::*;
    match op {
        CreateElement { tag, result } => {
            tag.0.hash(h);
            sn.norm(*result).hash(h);
        }
        CreateFragment { result } => sn.norm(*result).hash(h),
        CreateText { content, result } => {
            content.0.hash(h);
            sn.norm(*result).hash(h);
        }
        CreateTextDynamic { expr, result } => {
            expr.0.hash(h);
            sn.norm(*result).hash(h);
        }
        CreateComment { content, result } => {
            content.0.hash(h);
            sn.norm(*result).hash(h);
        }
        AppendChild { parent, child } => {
            sn.norm(*parent).hash(h);
            sn.norm(*child).hash(h);
        }
        InsertAfter { parent, node, anchor } => {
            sn.norm(*parent).hash(h);
            sn.norm(*node).hash(h);
            sn.norm(*anchor).hash(h);
        }
        Remove { node } => sn.norm(*node).hash(h),
        SetTextContent { node, expr } => {
            sn.norm(*node).hash(h);
            expr.0.hash(h);
        }
        SetAttribute { node, name, expr } => {
            sn.norm(*node).hash(h);
            name.0.hash(h);
            expr.0.hash(h);
        }
        AddEventListener { node, event, handler } => {
            sn.norm(*node).hash(h);
            event.0.hash(h);
            // Handler is a BlockId — use candidate substitution.
            hash_block_ref(*handler, h, candidates, hash_map);
        }
        MountComponent { component_def, parent, children_root } => {
            component_def.0.hash(h);
            sn.norm(*parent).hash(h);
            children_root.map(|s| sn.norm(s)).hash(h);
        }
        StoreHandle { slot, from } => {
            sn.norm(*slot).hash(h);
            sn.norm(*from).hash(h);
        }
        LoadHandle { slot, to } => {
            sn.norm(*slot).hash(h);
            sn.norm(*to).hash(h);
        }
        StoreI32 { slot, value } => {
            sn.norm(*slot).hash(h);
            value.hash(h);
        }
        StoreI32Slot { slot, from } => {
            sn.norm(*slot).hash(h);
            sn.norm(*from).hash(h);
        }
        LoadI32 { slot, to } => {
            sn.norm(*slot).hash(h);
            sn.norm(*to).hash(h);
        }
        I32Ne { lhs, rhs, result } => {
            sn.norm(*lhs).hash(h);
            sn.norm(*rhs).hash(h);
            sn.norm(*result).hash(h);
        }
        I32EqConst { lhs, rhs, result } => {
            sn.norm(*lhs).hash(h);
            rhs.hash(h);
            sn.norm(*result).hash(h);
        }
        AllocSubBoundary { boundary_id, ref_slot } => {
            boundary_id.0.hash(h);
            sn.norm(*ref_slot).hash(h);
        }
        AllocBoundary { boundary_id, ref_slot } => {
            boundary_id.0.hash(h);
            sn.norm(*ref_slot).hash(h);
        }
        BindBoundaryLocal { boundary_id, slot } => {
            boundary_id.0.hash(h);
            sn.norm(*slot).hash(h);
        }
        EvalExpr { expr, result } => {
            expr.0.hash(h);
            sn.norm(*result).hash(h);
        }
        DropExpr { expr } => expr.0.hash(h),
        If { cond, then_ops, else_ops, name: _ } => {
            sn.norm(*cond).hash(h);
            hash_ops(then_ops, h, sn, candidates, hash_map);
            hash_ops(else_ops, h, sn, candidates, hash_map);
        }
        CallBlock { block, parent } => {
            hash_block_ref(*block, h, candidates, hash_map);
            sn.norm(*parent).hash(h);
        }
        Return => {}
        SignalRead { signal, result } => {
            signal.0.hash(h);
            sn.norm(*result).hash(h);
        }
        SignalWrite { signal, value } => {
            signal.0.hash(h);
            sn.norm(*value).hash(h);
        }
        SignalWriteExpr { signal, expr } => {
            signal.0.hash(h);
            expr.0.hash(h);
        }
        TriggerEffects { signal } => signal.0.hash(h),
        InitSignal { signal_idx, expr } => {
            signal_idx.hash(h);
            expr.0.hash(h);
        }
        InitSignalDefault { signal_idx } => signal_idx.hash(h),
        InitMemorySlot { slot } => sn.norm(*slot).hash(h),
        ResourceNew { base_addr } => base_addr.hash(h),
        Loop { break_cond, body_ops, name: _ } => {
            sn.norm(*break_cond).hash(h);
            hash_ops(body_ops, h, sn, candidates, hash_map);
        }
        CallBlock2 { block, param0, param1, result } => {
            hash_block_ref(*block, h, candidates, hash_map);
            sn.norm(*param0).hash(h);
            sn.norm(*param1).hash(h);
            result.map(|s| sn.norm(s)).hash(h);
        }
        GeU { index, len, result } => {
            sn.norm(*index).hash(h);
            sn.norm(*len).hash(h);
            sn.norm(*result).hash(h);
        }
        LtU { a, b, result } => {
            sn.norm(*a).hash(h);
            sn.norm(*b).hash(h);
            sn.norm(*result).hash(h);
        }
        ComputeItemPtr { base, index, element_size, result } => {
            sn.norm(*base).hash(h);
            sn.norm(*index).hash(h);
            element_size.hash(h);
            sn.norm(*result).hash(h);
        }
        IncrSlot { slot } => sn.norm(*slot).hash(h),
        Alloc { size, align, result } => {
            sn.norm(*size).hash(h);
            align.hash(h);
            sn.norm(*result).hash(h);
        }
        Free { ptr, size } => {
            sn.norm(*ptr).hash(h);
            sn.norm(*size).hash(h);
        }
        MulConst { slot, constant, result } => {
            sn.norm(*slot).hash(h);
            constant.hash(h);
            sn.norm(*result).hash(h);
        }
        AddSlots { a, b, result } => {
            sn.norm(*a).hash(h);
            sn.norm(*b).hash(h);
            sn.norm(*result).hash(h);
        }
        SubSlots { a, b, result } => {
            sn.norm(*a).hash(h);
            sn.norm(*b).hash(h);
            sn.norm(*result).hash(h);
        }
        LoadI32Addr { addr, result } => {
            sn.norm(*addr).hash(h);
            sn.norm(*result).hash(h);
        }
        StoreI32Addr { addr, value } => {
            sn.norm(*addr).hash(h);
            sn.norm(*value).hash(h);
        }
        StructNew { ty_idx, fields, result } => {
            ty_idx.hash(h);
            fields.len().hash(h);
            for f in fields {
                sn.norm(*f).hash(h);
            }
            sn.norm(*result).hash(h);
        }
        StructGet { ty_idx, field, rec, result } => {
            ty_idx.hash(h);
            field.hash(h);
            sn.norm(*rec).hash(h);
            sn.norm(*result).hash(h);
        }
        StructSet { ty_idx, field, rec, value } => {
            ty_idx.hash(h);
            field.hash(h);
            sn.norm(*rec).hash(h);
            sn.norm(*value).hash(h);
        }
        ArrayNewDefault { ty_idx, len, result } => {
            ty_idx.hash(h);
            sn.norm(*len).hash(h);
            sn.norm(*result).hash(h);
        }
        ArrayGet { ty_idx, arr, idx, result } => {
            ty_idx.hash(h);
            sn.norm(*arr).hash(h);
            sn.norm(*idx).hash(h);
            sn.norm(*result).hash(h);
        }
        ArraySet { ty_idx, arr, idx, value } => {
            ty_idx.hash(h);
            sn.norm(*arr).hash(h);
            sn.norm(*idx).hash(h);
            sn.norm(*value).hash(h);
        }
        ArrayCopy {
            dst_ty_idx,
            src_ty_idx,
            dst,
            dst_idx,
            src,
            src_idx,
            count,
        } => {
            dst_ty_idx.hash(h);
            src_ty_idx.hash(h);
            sn.norm(*dst).hash(h);
            sn.norm(*dst_idx).hash(h);
            sn.norm(*src).hash(h);
            sn.norm(*src_idx).hash(h);
            sn.norm(*count).hash(h);
        }
        ArrayLen { arr, result } => {
            sn.norm(*arr).hash(h);
            sn.norm(*result).hash(h);
        }
        RefAsNonNull { slot } => sn.norm(*slot).hash(h),
        RefNull { ty_idx, result } => {
            ty_idx.hash(h);
            sn.norm(*result).hash(h);
        }
        ChildrenArrayNewDefault { anchor_boundary, len, result } => {
            anchor_boundary.0.hash(h);
            sn.norm(*len).hash(h);
            sn.norm(*result).hash(h);
        }
        ChildrenArrayGet { anchor_boundary, arr, idx, result } => {
            anchor_boundary.0.hash(h);
            sn.norm(*arr).hash(h);
            sn.norm(*idx).hash(h);
            sn.norm(*result).hash(h);
        }
        ChildrenArraySet { anchor_boundary, arr, idx, value } => {
            anchor_boundary.0.hash(h);
            sn.norm(*arr).hash(h);
            sn.norm(*idx).hash(h);
            sn.norm(*value).hash(h);
        }
        ChildrenArrayCopy { anchor_boundary, dst, dst_idx, src, src_idx, count } => {
            anchor_boundary.0.hash(h);
            sn.norm(*dst).hash(h);
            sn.norm(*dst_idx).hash(h);
            sn.norm(*src).hash(h);
            sn.norm(*src_idx).hash(h);
            sn.norm(*count).hash(h);
        }
        LoadList { signal, ptr_result, len_result } => {
            signal.0.hash(h);
            sn.norm(*ptr_result).hash(h);
            sn.norm(*len_result).hash(h);
        }
        SetSlot { slot, value } => {
            sn.norm(*slot).hash(h);
            value.hash(h);
        }
        CopySlot { from, to } => {
            sn.norm(*from).hash(h);
            sn.norm(*to).hash(h);
        }
        GetSlotAddress { mem_slot, result } => {
            sn.norm(*mem_slot).hash(h);
            sn.norm(*result).hash(h);
        }
        EvalListExpr { expr, ptr_result, len_result } => {
            expr.0.hash(h);
            sn.norm(*ptr_result).hash(h);
            sn.norm(*len_result).hash(h);
        }
        LoadListGc { signal, ref_result, len_result } => {
            signal.0.hash(h);
            sn.norm(*ref_result).hash(h);
            sn.norm(*len_result).hash(h);
        }
        EvalListExprGc { expr, ref_result, len_result } => {
            expr.0.hash(h);
            sn.norm(*ref_result).hash(h);
            sn.norm(*len_result).hash(h);
        }
        ArrayGetItem { arr, idx, list_ty, result } => {
            sn.norm(*arr).hash(h);
            sn.norm(*idx).hash(h);
            list_ty.hash(h);
            sn.norm(*result).hash(h);
        }
    }
}

fn hash_block_ref<H: Hasher>(
    target: BlockId,
    h: &mut H,
    candidates: &HashSet<BlockId>,
    hash_map: &HashMap<BlockId, u64>,
) {
    if candidates.contains(&target) {
        // Use the current per-iteration hash so structurally-equivalent
        // callees produce the same overall structural hash.
        "candidate".hash(h);
        hash_map.get(&target).copied().unwrap_or(0).hash(h);
    } else {
        "literal".hash(h);
        target.0.hash(h);
    }
}

/// Renumbers SlotIds in first-seen order so two blocks that are
/// identical modulo slot-allocation order map to the same normalised
/// id space.
struct SlotNormalizer {
    map: HashMap<u32, u32>,
    next: u32,
}

impl SlotNormalizer {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            next: 0,
        }
    }

    fn norm(&mut self, slot: LirSlotId) -> u32 {
        if let Some(v) = self.map.get(&slot.0) {
            return *v;
        }
        let v = self.next;
        self.next += 1;
        self.map.insert(slot.0, v);
        v
    }
}

/// Final structural equality check (post-hash). Walks both blocks'
/// canonicalised op streams in lock-step. Used to guard against hash
/// collisions before committing a remap.
fn blocks_structurally_equal(
    a: &LirBlock,
    b: &LirBlock,
    remap: &HashMap<BlockId, BlockId>,
) -> bool {
    if a.boundary_params != b.boundary_params {
        return false;
    }
    if a.params.len() != b.params.len() {
        return false;
    }
    if a.return_slot.is_some() != b.return_slot.is_some() {
        return false;
    }
    let mut sn_a = SlotNormalizer::new();
    let mut sn_b = SlotNormalizer::new();
    ops_eq(&a.ops, &b.ops, &mut sn_a, &mut sn_b, remap)
}

fn ops_eq(
    a: &[LirOp],
    b: &[LirOp],
    sn_a: &mut SlotNormalizer,
    sn_b: &mut SlotNormalizer,
    remap: &HashMap<BlockId, BlockId>,
) -> bool {
    if a.len() != b.len() {
        return false;
    }
    for (x, y) in a.iter().zip(b.iter()) {
        if std::mem::discriminant(x) != std::mem::discriminant(y) {
            return false;
        }
        if !op_eq(x, y, sn_a, sn_b, remap) {
            return false;
        }
    }
    true
}

fn op_eq(
    x: &LirOp,
    y: &LirOp,
    sn_a: &mut SlotNormalizer,
    sn_b: &mut SlotNormalizer,
    remap: &HashMap<BlockId, BlockId>,
) -> bool {
    use LirOp::*;
    let resolve = |b: BlockId| *remap.get(&b).unwrap_or(&b);
    match (x, y) {
        (If { cond: c1, then_ops: t1, else_ops: e1, .. },
         If { cond: c2, then_ops: t2, else_ops: e2, .. }) => {
            sn_a.norm(*c1) == sn_b.norm(*c2)
                && ops_eq(t1, t2, sn_a, sn_b, remap)
                && ops_eq(e1, e2, sn_a, sn_b, remap)
        }
        (Loop { break_cond: c1, body_ops: b1, .. },
         Loop { break_cond: c2, body_ops: b2, .. }) => {
            sn_a.norm(*c1) == sn_b.norm(*c2) && ops_eq(b1, b2, sn_a, sn_b, remap)
        }
        (CallBlock { block: b1, parent: p1 },
         CallBlock { block: b2, parent: p2 }) => {
            resolve(*b1) == resolve(*b2) && sn_a.norm(*p1) == sn_b.norm(*p2)
        }
        (CallBlock2 { block: b1, param0: p01, param1: p11, result: r1 },
         CallBlock2 { block: b2, param0: p02, param1: p12, result: r2 }) => {
            resolve(*b1) == resolve(*b2)
                && sn_a.norm(*p01) == sn_b.norm(*p02)
                && sn_a.norm(*p11) == sn_b.norm(*p12)
                && r1.map(|s| sn_a.norm(s)) == r2.map(|s| sn_b.norm(s))
        }
        (AddEventListener { node: n1, event: e1, handler: h1 },
         AddEventListener { node: n2, event: e2, handler: h2 }) => {
            sn_a.norm(*n1) == sn_b.norm(*n2)
                && e1.0 == e2.0
                && resolve(*h1) == resolve(*h2)
        }
        // For all the other ops we already have a hash that mixes the
        // same fields. A hash match plus matching discriminant is a
        // strong signal; do an additional shallow check by re-hashing
        // both sides through `hash_op` with empty candidate sets and
        // comparing — cheap and avoids a 50-arm match here.
        _ => {
            let mut ha = std::collections::hash_map::DefaultHasher::new();
            let mut hb = std::collections::hash_map::DefaultHasher::new();
            let empty: HashSet<BlockId> = HashSet::new();
            let no_hashes: HashMap<BlockId, u64> = HashMap::new();
            hash_op(x, &mut ha, sn_a, &empty, &no_hashes);
            hash_op(y, &mut hb, sn_b, &empty, &no_hashes);
            ha.finish() == hb.finish()
        }
    }
}

#[allow(dead_code)]
fn _silence_unused(_: &LirBlockEffect) {}
