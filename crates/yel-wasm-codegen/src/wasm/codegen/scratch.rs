//! Free helpers shared across the codegen submodules: per-valtype
//! scratch slot bookkeeping (`merge_max_slot_counts`,
//! `push_valtype_locals`, `per_valtype_counts`) plus the small
//! `mem_arg`/`slot_local`/`i32_narrow_store_for` utilities and the
//! mount-retention counter (`compute_mount_retention_counts`).
//!
//! Per-block mount-site counts are precomputed during LIR block lowering
//! (`LirBlock::mount_component_count`); codegen reads that field directly
//! instead of re-walking the op tree.

use wasm_encoder::{Function, Instruction, ValType};

use yel_core::lir::arena::LirResourceArena;
use yel_core::lir::{LirBlock, LirResource, LirSlotId, LirSlotInfo, LirSlotKind};

/// Emit `cabi_realloc(0, 0, align, size)`, leaving the freshly-allocated
/// pointer on the stack. The canonical-ABI realloc with `old_ptr = 0` and
/// `old_size = 0` is a plain allocation of `size` bytes at `align`. The caller
/// consumes the pointer (e.g. `local.set`).
pub(super) fn emit_cabi_realloc_fixed(func: &mut Function, align: u32, size: u32, cabi_realloc: u32) {
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(align as i32));
    func.instruction(&Instruction::I32Const(size as i32));
    func.instruction(&Instruction::Call(cabi_realloc));
}

/// Emit `cabi_realloc(0, 0, elem_align, len * elem_size)`, leaving the
/// element-buffer pointer on the stack — a fresh buffer for a `len`-element
/// array of `elem_size`-byte elements. `len` is read from `len_local`.
pub(super) fn emit_cabi_realloc_array(
    func: &mut Function,
    len_local: u32,
    elem_size: u32,
    elem_align: u32,
    cabi_realloc: u32,
) {
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Const(elem_align as i32));
    func.instruction(&Instruction::LocalGet(len_local));
    func.instruction(&Instruction::I32Const(elem_size as i32));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::Call(cabi_realloc));
}

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
/// Panics on Memory — those must never reach a `LocalGet` / `LocalSet`
/// emission path (they're only addressable via `StoreHandle` /
/// `LoadHandle`). Hitting the panic indicates a lowering bug.
///
/// Phase 0.3i: callers pass `local_offset` here instead of adding it
/// at the call site, so the `WasmParam` variant can bypass the offset.
#[inline]
pub(crate) fn slot_local(
    component: &dyn LirResourceArena,
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
                    .slots()
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
    component: &'a dyn LirResourceArena,
) -> &'a LirSlotInfo {
    match slot {
        LirSlotId::Block { block: bid, idx } => {
            // Hard check, not debug_assert: a Block-variant slot referenced
            // while generating a DIFFERENT block has no wasm local in this
            // frame — indexing the current block's slots would silently read
            // the wrong slot info and emit a wrong local index. Cross-block
            // Temp references must either stay Resource-variant or be passed
            // through block params.
            if bid != block.id {
                panic!(
                    "slot {:?} belongs to block {:?} but is referenced while generating \
                     block {:?} — cross-block Temp references have no local in this frame",
                    slot, bid, block.id
                );
            }
            &block.slots[idx as usize]
        }
        LirSlotId::Resource { idx } => &component.slots()[idx as usize],
    }
}

/// Task #105 (2): variant of `slot_local` for the (rare) call sites
/// that emit outside a block context — e.g. signal-emit helpers
/// invoked from setup code where no `LirBlock` is in scope. Asserts
/// the slot must be `Resource`-variant (the only kind reachable from
/// non-block contexts today) and panics on `Block`.
#[inline]
pub(crate) fn slot_local_resource_only(
    component: &dyn LirResourceArena,
    slot: LirSlotId,
    local_offset: u32,
) -> u32 {
    match slot {
        LirSlotId::Resource { idx } => match component.slots()[idx as usize].kind {
            LirSlotKind::Temp { local_idx } => local_idx + local_offset,
            LirSlotKind::WasmParam { idx } => idx,
            LirSlotKind::Memory { .. } => panic!(
                "slot {:?} is a Memory slot but was used as a WASM local (LocalGet/LocalSet)",
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

