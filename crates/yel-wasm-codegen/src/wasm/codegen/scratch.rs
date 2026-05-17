//! Free helpers shared across the codegen submodules: per-valtype
//! scratch slot bookkeeping (`compute_slot_locals`, `merge_max_slot_counts`,
//! `push_valtype_locals`, `per_valtype_counts`) plus the small
//! `mem_arg`/`slot_local`/`i32_narrow_store_for` utilities and the
//! mount-retention counter (`compute_mount_retention_counts`).
//!
//! Per-block mount-site counts are precomputed during LIR block lowering
//! (`LirBlock::mount_component_count`); codegen reads that field directly
//! instead of re-walking the op tree.

use wasm_encoder::{Instruction, ValType};

use super::super::CodegenError;
use yel_core::lir::{LirBlock, LirResource, LirSlotId, LirSlotInfo, LirSlotKind};

/// Compute the total `MountComponent` retention count for a component.
/// Every mount site (regardless of whether it lives inside a for-body)
/// gets one `(mut (ref null any))` field appended to `$Comp_<comp_idx>`,
/// so the parent instance keeps every mounted child alive through GC
/// tracing.
pub(super) fn compute_mount_retention_counts(component: &LirResource) -> u32 {
    component
        .blocks
        .iter()
        .map(|b| b.mount_component_count)
        .sum()
}

/// Resolve a slot to its absolute WASM-local index.
///
/// - `Temp { local_idx }` → `local_idx + local_offset` (the standard
///   case: a slot's compacted index is shifted past the function's
///   param locals).
/// - `WasmParam { idx }` → `idx` (the slot *is* a wasm param; the
///   enclosing function's `local_offset` is ignored).
///
/// Panics on Memory / BoundaryField — those must never reach a
/// `LocalGet` / `LocalSet` emission path (they're only addressable via
/// `StoreHandle` / `LoadHandle` or struct-get/set against a tree
/// boundary ref). Hitting the panic indicates a lowering bug.
///
/// Phase 0.3i: callers pass `local_offset` here instead of adding it
/// at the call site, so the `WasmParam` variant can bypass the offset.
#[inline]
pub(crate) fn slot_local(
    component: &LirResource,
    block: &LirBlock,
    slot: LirSlotId,
    local_offset: u32,
) -> u32 {
    match slot_info(slot, block, component).kind {
        LirSlotKind::Temp { local_idx } => match slot {
            // Resource Temps are declared first in the function's local
            // section (after the wasm params), so their local index is
            // `local_offset + local_idx`.
            LirSlotId::Resource { .. } => local_idx + local_offset,
            // Task #105 B2: Block Temps are declared AFTER the
            // component-wide Resource Temps. Their local_idx is
            // per-block (starts at 0), so offset by the count of
            // Resource Temp slots in the component.
            LirSlotId::Block { .. } => {
                let n_resource_temp = component
                    .slots
                    .iter()
                    .filter(|s| matches!(s.kind, LirSlotKind::Temp { .. }))
                    .count() as u32;
                local_offset + n_resource_temp + local_idx
            }
        },
        LirSlotKind::WasmParam { idx } => idx,
        LirSlotKind::Memory { .. } => panic!(
            "slot {:?} is a Memory slot but was used as a WASM local (LocalGet/LocalSet)",
            slot
        ),
        LirSlotKind::BoundaryField { .. } => panic!(
            "slot {:?} is a BoundaryField slot but was used as a WASM local (LocalGet/LocalSet)",
            slot
        ),
    }
}

/// Task #105 (2): unified slot-info lookup. Routes `LirSlotId::Block`
/// to `block.slots` and `LirSlotId::Resource` to `component.slots`.
/// Today `block.slots` is empty (the `Block` variant is never
/// constructed — the allocator still flat-indexes everything into
/// `component.slots`), so the Resource arm is the only one that fires.
/// Safe no-op widening that gives later migration stages a single
/// chokepoint to flip.
#[inline]
pub(crate) fn slot_info<'a>(
    slot: LirSlotId,
    block: &'a LirBlock,
    component: &'a LirResource,
) -> &'a LirSlotInfo {
    match slot {
        LirSlotId::Block { block: bid, idx } => {
            debug_assert_eq!(bid, block.id, "slot ref to different block");
            &block.slots[idx as usize]
        }
        LirSlotId::Resource { idx } => &component.slots[idx as usize],
    }
}

/// Task #105 (2): variant of `slot_local` for the (rare) call sites
/// that emit outside a block context — e.g. signal-emit helpers
/// invoked from setup code where no `LirBlock` is in scope. Asserts
/// the slot must be `Resource`-variant (the only kind reachable from
/// non-block contexts today) and panics on `Block`.
#[inline]
pub(crate) fn slot_local_resource_only(
    component: &LirResource,
    slot: LirSlotId,
    local_offset: u32,
) -> u32 {
    match slot {
        LirSlotId::Resource { idx } => match component.slots[idx as usize].kind {
            LirSlotKind::Temp { local_idx } => local_idx + local_offset,
            LirSlotKind::WasmParam { idx } => idx,
            LirSlotKind::Memory { .. } => panic!(
                "slot {:?} is a Memory slot but was used as a WASM local (LocalGet/LocalSet)",
                slot
            ),
            LirSlotKind::BoundaryField { .. } => panic!(
                "slot {:?} is a BoundaryField slot but was used as a WASM local (LocalGet/LocalSet)",
                slot
            ),
        },
        LirSlotId::Block { .. } => panic!(
            "slot_local_resource_only called with Block-variant slot {:?} (no LirBlock context available)",
            slot
        ),
    }
}

/// Helper to create a MemArg for load/store instructions.
pub(crate) fn mem_arg(offset: u64, align: u32) -> wasm_encoder::MemArg {
    wasm_encoder::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

/// Count the number of flat slots of each valtype required to hold a
/// composite value of `ty`. Used to size the per-valtype scratch local
/// regions on the caller side.
pub(super) fn per_valtype_counts(slots: &[crate::wasm::FlatSlot]) -> (u32, u32, u32, u32) {
    let (mut n_i32, mut n_i64, mut n_f32, mut n_f64) = (0u32, 0u32, 0u32, 0u32);
    for s in slots {
        match s.valtype {
            ValType::I32 => n_i32 += 1,
            ValType::I64 => n_i64 += 1,
            ValType::F32 => n_f32 += 1,
            ValType::F64 => n_f64 += 1,
            _ => {}
        }
    }
    (n_i32, n_i64, n_f32, n_f64)
}

/// Accumulate per-valtype slot counts into a running max tuple.
pub(super) fn merge_max_slot_counts(
    max: &mut (u32, u32, u32, u32),
    slots: &[crate::wasm::FlatSlot],
) {
    let (a, b, c, d) = per_valtype_counts(slots);
    if a > max.0 {
        max.0 = a;
    }
    if b > max.1 {
        max.1 = b;
    }
    if c > max.2 {
        max.2 = c;
    }
    if d > max.3 {
        max.3 = d;
    }
}

/// Emit the right `I32Store{,8,16}` for an i32-backed signed or unsigned
/// narrow integer target (`s8`/`u8`/`s16`/`u16`/`s32`/`u32`). Caller
/// must have pushed `(addr, i32_value)` on the stack.
pub(super) fn i32_narrow_store_for(
    func: &mut wasm_encoder::Function,
    target_kind: &yel_core::types::InternedTyKind,
) {
    use yel_core::types::InternedTyKind;
    let ma = |offset: u64, align: u32| wasm_encoder::MemArg {
        offset,
        align,
        memory_index: 0,
    };
    let instr = match target_kind {
        InternedTyKind::S8 | InternedTyKind::U8 => Instruction::I32Store8(ma(0, 0)),
        InternedTyKind::S16 | InternedTyKind::U16 => Instruction::I32Store16(ma(0, 1)),
        _ => Instruction::I32Store(ma(0, 2)),
    };
    func.instruction(&instr);
}

/// Push `(count, ValType)` entries for any non-zero counts. Order is
/// fixed: i32, i64, f32, f64 — match `FlatScratchBases` layout.
pub(super) fn push_valtype_locals(
    locals: &mut Vec<(u32, wasm_encoder::ValType)>,
    counts: (u32, u32, u32, u32),
) {
    if counts.0 > 0 {
        locals.push((counts.0, ValType::I32));
    }
    if counts.1 > 0 {
        locals.push((counts.1, ValType::I64));
    }
    if counts.2 > 0 {
        locals.push((counts.2, ValType::F32));
    }
    if counts.3 > 0 {
        locals.push((counts.3, ValType::F64));
    }
}

/// Compute absolute scratch local indices for each flat slot under a
/// per-valtype partitioning. Slot i gets `base_of(valtype_i) +
/// index_among_same_valtype_slots_so_far`. Returns an error if any slot
/// would exceed the scratch region reserved by the caller.
pub(super) fn compute_slot_locals(
    slots: &[crate::wasm::FlatSlot],
    scratch: &crate::wasm::FlatScratchBases,
) -> Result<Vec<u32>, CodegenError> {
    let mut out = Vec::with_capacity(slots.len());
    let (mut u_i32, mut u_i64, mut u_f32, mut u_f64) = (0u32, 0u32, 0u32, 0u32);
    for (i, s) in slots.iter().enumerate() {
        let (base, used, cap) = match s.valtype {
            ValType::I32 => (scratch.i32_base, &mut u_i32, scratch.i32_count),
            ValType::I64 => (scratch.i64_base, &mut u_i64, scratch.i64_count),
            ValType::F32 => (scratch.f32_base, &mut u_f32, scratch.f32_count),
            ValType::F64 => (scratch.f64_base, &mut u_f64, scratch.f64_count),
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "emit_flat_slot_store: unsupported scratch valtype {:?} at slot {}",
                    other, i
                )));
            }
        };
        if *used >= cap {
            return Err(CodegenError::InvalidIR(format!(
                "emit_flat_slot_store: scratch capacity for valtype {:?} exhausted at slot {} \
                 (cap={}, used={})",
                s.valtype, i, cap, *used
            )));
        }
        out.push(base + *used);
        *used += 1;
    }
    Ok(out)
}
