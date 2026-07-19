//! Inline expansion of UI signal LirOps to neutral op sequences.
//!
//! Phase 1.1 of the LIR-flattening refactor (see plan
//! `cheeky-gliding-beacon.md`). Each helper takes the LIR builder
//! context plus the signal coordinates and emits the same op sequence
//! today's codegen produces from `LirOp::Signal*`. Call sites switch
//! in Phase 1.2.
//!
//! Today: memory-backed signals lower fully. Struct-backed signals
//! still pass through `LirOp::Signal*` until Phase 1.1c lands the
//! wasm-type-section-index accessor on `LirResource`.
//!
//! See `project_signal_storage_dual.md`: signals can have BOTH a GC
//! and a memory backing. The helper inspects `SignalLayout` and emits
//! whichever path applies for the requested access flavor — they are
//! independent and not mutually exclusive.

use crate::context::CompilerContext;
use crate::lir::block::{LirOp, LirSlotId, LirSlotValType, MemoryValueType, StoreWidth};
use crate::lir::signal_layout::MemSlot;
use crate::types::{InternedTyKind, Ty};

/// Slot-allocator callback used by every helper. Phase 1.2 wires this
/// to `BlockLowering::alloc_temp_slot_typed`; callers from a different
/// context can supply their own allocator that produces fresh
/// `LirSlotId`s with the right `val_ty`.
pub type SlotAlloc<'a> = &'a mut dyn FnMut(LirSlotValType) -> LirSlotId;

/// Result of lowering a single signal op. `Some(ops)` means the helper
/// fully expanded the op to neutral ops; `None` means the caller must
/// emit the original `LirOp::Signal*` (Phase 1.1c will cover the
/// remaining cases — today: struct-backed signals).
pub type LoweredOps = Option<Vec<LirOp>>;

/// Lower a memory-backed `SignalWrite` to a `MemConst` + typed-store
/// sequence. Returns `None` when the signal has no memory backing
/// (struct-backed-only signals fall through to Phase 1.1c).
///
/// `value_slots` is the list of source slots that hold the canonical
/// flat ABI representation of the signal's value. For single-slot
/// types this is one slot; for `option<T>` it's `[discriminant, payload]`;
/// for `string` / `list<T>` it's `[ptr, len]`. The caller is responsible
/// for arranging the slot decomposition (matching today's
/// `slot_local(...) + local_offset + N` walk in `op_emit.rs`).
///
/// Mirrors the `LirOp::SignalWrite` arm in `op_emit.rs:997-1121`
/// (memory path: F32/F64/I64/Option/String/List/narrow-int/default).
pub fn lower_signal_write_to_memory(
    ctx: &CompilerContext,
    signal_ty: Ty,
    mem: MemSlot,
    base_addr: u32,
    value_slots: &[LirSlotId],
    alloc: SlotAlloc<'_>,
) -> LoweredOps {
    let addr = base_addr + mem.offset;
    let mut ops = Vec::new();

    // Emit `MemConst <addr> -> a; StoreAddr a <- value` for `value` at the
    // scalar type/width that backs `signal_ty`.
    let mut store_scalar = |ops: &mut Vec<LirOp>, slot_addr: u32, value: LirSlotId, ty, width| {
        let a = alloc(LirSlotValType::I32);
        ops.push(LirOp::MemConst {
            addr: slot_addr,
            result: a,
        });
        ops.push(LirOp::StoreAddr {
            addr: a,
            value,
            ty,
            width,
        });
    };
    use MemoryValueType::*;
    use StoreWidth::*;

    match ctx.ty_kind(signal_ty) {
        InternedTyKind::F32 => {
            let v = expect_one_slot(value_slots, "F32 signal write")?;
            store_scalar(&mut ops, addr, v, F32, Full);
        }
        InternedTyKind::F64 => {
            let v = expect_one_slot(value_slots, "F64 signal write")?;
            store_scalar(&mut ops, addr, v, F64, Full);
        }
        InternedTyKind::S64 | InternedTyKind::U64 => {
            let v = expect_one_slot(value_slots, "I64 signal write")?;
            store_scalar(&mut ops, addr, v, I64, Full);
        }
        InternedTyKind::Option(_) => {
            // [0]: discriminant byte at addr; [1]: payload i32 at addr+4.
            if value_slots.len() != 2 {
                return None;
            }
            store_scalar(&mut ops, addr, value_slots[0], I32, Narrow8);
            store_scalar(&mut ops, addr + 4, value_slots[1], I32, Full);
        }
        InternedTyKind::String | InternedTyKind::List(_) => {
            // Fat pointer: [ptr at addr, len at addr+4].
            if value_slots.len() != 2 {
                return None;
            }
            store_scalar(&mut ops, addr, value_slots[0], I32, Full);
            store_scalar(&mut ops, addr + 4, value_slots[1], I32, Full);
        }
        InternedTyKind::Bool | InternedTyKind::U8 | InternedTyKind::S8 | InternedTyKind::Char => {
            let v = expect_one_slot(value_slots, "narrow8 signal write")?;
            store_scalar(&mut ops, addr, v, I32, Narrow8);
        }
        InternedTyKind::U16 | InternedTyKind::S16 => {
            let v = expect_one_slot(value_slots, "narrow16 signal write")?;
            store_scalar(&mut ops, addr, v, I32, Narrow16);
        }
        _ => {
            let v = expect_one_slot(value_slots, "default i32 signal write")?;
            store_scalar(&mut ops, addr, v, I32, Full);
        }
    }

    Some(ops)
}

/// Lower a memory-backed `SignalWrite` for a *pointer-typed global*
/// property to a `MemConstGlobalProp` + typed-store sequence. Mirrors
/// [`lower_signal_write_to_memory`] but resolves the base address via
/// the module-level `global_property_addrs` map at codegen time (no
/// per-component memory base addition). Used by Path B in
/// `BlockLowering::try_inline_signal_write` for record/tuple globals.
pub fn lower_signal_write_to_global_memory(
    ctx: &CompilerContext,
    signal_ty: Ty,
    signal_def: crate::ids::DefId,
    size: u32,
    value_slots: &[LirSlotId],
    alloc: SlotAlloc<'_>,
) -> LoweredOps {
    // Reuse the memory-write helper with a synthetic MemSlot { offset: 0 }
    // and base_addr: 0 — every emitted `MemConst { addr }` then carries
    // the *intra-property* byte offset (0, 4, ...). Rewrite each
    // `MemConst` to `MemConstGlobalProp { signal_def, offset: addr }` so
    // codegen resolves the absolute base from `global_property_addrs`.
    let synthetic = MemSlot { offset: 0, size };
    let raw =
        lower_signal_write_to_memory(ctx, signal_ty, synthetic, 0, value_slots, alloc)?;
    let rewritten = raw
        .into_iter()
        .map(|op| match op {
            LirOp::MemConst { addr, result } => LirOp::MemConstGlobalProp {
                signal_def,
                offset: addr,
                result,
            },
            other => other,
        })
        .collect();
    Some(rewritten)
}

/// Lower `InitSignalDefault` for a memory-backed signal — zero the
/// reserved memory cell with a typed store of the right width. Mirrors
/// the `LirOp::InitSignalDefault` non-struct arm in `op_emit.rs:771-799`.
///
/// Returns `None` when the signal has no memory backing OR the type
/// kind isn't handled by the legacy path (today's codegen falls back
/// to a full-width i32 store for "everything else").
pub fn lower_init_signal_default_to_memory(
    ctx: &CompilerContext,
    signal_ty: Ty,
    mem: MemSlot,
    base_addr: u32,
    alloc: SlotAlloc<'_>,
) -> LoweredOps {
    let addr = base_addr + mem.offset;
    let mut ops = Vec::new();
    let a = alloc(LirSlotValType::I32);
    ops.push(LirOp::MemConst { addr, result: a });

    match ctx.ty_kind(signal_ty) {
        InternedTyKind::F32 => {
            // Today's codegen emits `F32Const(0.0); F32Store`. We don't
            // yet have an `F32Const` LirOp; defer to Phase 1.1c when
            // the const-materialize ops land. Bail.
            return None;
        }
        InternedTyKind::F64 => {
            return None;
        }
        InternedTyKind::S64 | InternedTyKind::U64 => {
            // Need `I64Const(0)` materialize → store. Bail until the
            // const-materialize ops are available.
            return None;
        }
        _ => {
            // i32 path. Allocate a zero-valued slot and store it.
            let zero = alloc(LirSlotValType::I32);
            ops.push(LirOp::SetSlot {
                slot: zero,
                value: 0,
            });
            ops.push(LirOp::StoreAddr {
                addr: a,
                value: zero,
                ty: MemoryValueType::I32,
                width: StoreWidth::Full,
            });
        }
    }

    Some(ops)
}

fn expect_one_slot(slots: &[LirSlotId], _what: &str) -> Option<LirSlotId> {
    if slots.len() == 1 {
        Some(slots[0])
    } else {
        None
    }
}

