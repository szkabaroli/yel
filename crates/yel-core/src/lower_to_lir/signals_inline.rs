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
use crate::lir::block::{LirOp, LirSlotId, LirSlotValType};
use crate::lir::signal_layout::{MemSlot, SignalLayout};
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

    match ctx.ty_kind(signal_ty) {
        InternedTyKind::F32 => {
            let v = expect_one_slot(value_slots, "F32 signal write")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::StoreF32Addr { addr: a, value: v });
        }
        InternedTyKind::F64 => {
            let v = expect_one_slot(value_slots, "F64 signal write")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::StoreF64Addr { addr: a, value: v });
        }
        InternedTyKind::S64 | InternedTyKind::U64 => {
            let v = expect_one_slot(value_slots, "I64 signal write")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::StoreI64Addr { addr: a, value: v });
        }
        InternedTyKind::Option(_) => {
            // [0]: discriminant byte at addr; [1]: payload i32 at addr+4.
            if value_slots.len() != 2 {
                return None;
            }
            let disc = value_slots[0];
            let payload = value_slots[1];
            let a0 = alloc(LirSlotValType::I32);
            let a1 = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a0 });
            ops.push(LirOp::StoreI32Narrow8Addr {
                addr: a0,
                value: disc,
            });
            ops.push(LirOp::MemConst {
                addr: addr + 4,
                result: a1,
            });
            ops.push(LirOp::StoreI32Addr {
                addr: a1,
                value: payload,
            });
        }
        InternedTyKind::String | InternedTyKind::List(_) => {
            // Fat pointer: [ptr at addr, len at addr+4].
            if value_slots.len() != 2 {
                return None;
            }
            let ptr = value_slots[0];
            let len = value_slots[1];
            let a0 = alloc(LirSlotValType::I32);
            let a1 = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a0 });
            ops.push(LirOp::StoreI32Addr {
                addr: a0,
                value: ptr,
            });
            ops.push(LirOp::MemConst {
                addr: addr + 4,
                result: a1,
            });
            ops.push(LirOp::StoreI32Addr {
                addr: a1,
                value: len,
            });
        }
        InternedTyKind::Bool | InternedTyKind::U8 | InternedTyKind::S8 | InternedTyKind::Char => {
            let v = expect_one_slot(value_slots, "narrow8 signal write")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::StoreI32Narrow8Addr { addr: a, value: v });
        }
        InternedTyKind::U16 | InternedTyKind::S16 => {
            let v = expect_one_slot(value_slots, "narrow16 signal write")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::StoreI32Narrow16Addr { addr: a, value: v });
        }
        _ => {
            let v = expect_one_slot(value_slots, "default i32 signal write")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::StoreI32Addr { addr: a, value: v });
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

/// Lower a memory-backed `SignalRead` to a `MemConst` + typed-load
/// sequence. Returns `None` when the signal has no memory backing.
///
/// `result_slots` mirrors `value_slots` in `lower_signal_write_to_memory`:
/// one slot per canonical flat ABI valtype. Caller pre-allocates these
/// with the right `val_ty` (i32 for scalars / ptrs / discs; i64 / f32 /
/// f64 for the wide types).
///
/// NOTE: today's codegen never produces `LirOp::SignalRead` (the read
/// side is folded into `LirExprKind::SignalRead` at expression-emit
/// time — see `op_emit.rs:987-995`). This helper exists for parity and
/// for Phase 1.2's future-proofing; Phase 1.1c may relocate it once
/// the call shape settles.
pub fn lower_signal_read_from_memory(
    ctx: &CompilerContext,
    signal_ty: Ty,
    mem: MemSlot,
    base_addr: u32,
    result_slots: &[LirSlotId],
    alloc: SlotAlloc<'_>,
) -> LoweredOps {
    let addr = base_addr + mem.offset;
    let mut ops = Vec::new();

    match ctx.ty_kind(signal_ty) {
        InternedTyKind::F32 => {
            let r = expect_one_slot(result_slots, "F32 signal read")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::LoadF32Addr { addr: a, result: r });
        }
        InternedTyKind::F64 => {
            let r = expect_one_slot(result_slots, "F64 signal read")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::LoadF64Addr { addr: a, result: r });
        }
        InternedTyKind::S64 | InternedTyKind::U64 => {
            let r = expect_one_slot(result_slots, "I64 signal read")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::LoadI64Addr { addr: a, result: r });
        }
        InternedTyKind::Option(_) | InternedTyKind::String | InternedTyKind::List(_) => {
            // Two-slot reads. Today's `LirExprKind::SignalRead` does
            // these with widened i32-loads (the narrow-discriminant
            // case sign/zero-extends from the byte). Mirror that:
            // both halves are loaded as i32 at `addr` / `addr+4`.
            if result_slots.len() != 2 {
                return None;
            }
            let a0 = alloc(LirSlotValType::I32);
            let a1 = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a0 });
            ops.push(LirOp::LoadI32Addr {
                addr: a0,
                result: result_slots[0],
            });
            ops.push(LirOp::MemConst {
                addr: addr + 4,
                result: a1,
            });
            ops.push(LirOp::LoadI32Addr {
                addr: a1,
                result: result_slots[1],
            });
        }
        // Narrow signed/unsigned reads currently round-trip through
        // `LoadI32Addr` because today's codegen uses widened i32-loads
        // for everything <= 4 bytes (sign/zero extension is handled at
        // the expression level). When Phase 1.1c grows narrow-load
        // variants (`LoadI32Narrow8S/U`, `LoadI32Narrow16S/U`), wire
        // them here. Until then bail out so the caller keeps the
        // original `SignalRead` and codegen handles the right load.
        InternedTyKind::Bool
        | InternedTyKind::U8
        | InternedTyKind::S8
        | InternedTyKind::Char
        | InternedTyKind::U16
        | InternedTyKind::S16 => {
            return None;
        }
        _ => {
            let r = expect_one_slot(result_slots, "default i32 signal read")?;
            let a = alloc(LirSlotValType::I32);
            ops.push(LirOp::MemConst { addr, result: a });
            ops.push(LirOp::LoadI32Addr { addr: a, result: r });
        }
    }

    Some(ops)
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
            ops.push(LirOp::StoreI32Addr {
                addr: a,
                value: zero,
            });
        }
    }

    Some(ops)
}

/// Convenience: returns the `MemSlot` for `sig_idx` if the signal has a
/// memory backing in `layout`, else `None`. Callers gate the helper
/// dispatch on this so struct-only signals fall through to the
/// existing `LirOp::Signal*` path.
pub fn signal_mem_slot(layout: &SignalLayout, sig_idx: usize) -> Option<MemSlot> {
    layout.signals.get(sig_idx).and_then(|s| s.mem)
}

fn expect_one_slot(slots: &[LirSlotId], _what: &str) -> Option<LirSlotId> {
    if slots.len() == 1 {
        Some(slots[0])
    } else {
        None
    }
}

// =============================================================================
// Stubs / deferred helpers
// =============================================================================
//
// The following ops have more complex expansions today; Phase 1.1
// reserves their helper names but leaves the body deferred. Each
// `todo!()` carries a precise pointer at the source-of-truth emit arm
// that Phase 1.2 / 1.1c will mirror here.

/// Lower `LirOp::SignalWriteExpr` for memory-backed signals.
///
/// Today's emit arm (`op_emit.rs:699-731`) routes through
/// `emit_signal_store`, which evaluates the expression onto the stack
/// in canonical-ABI flat form then peels each result valtype off in
/// reverse and stores it at its offset. The peel sequence depends on
/// the expression's flat valtype list — neutral LirOp doesn't yet
/// expose a "drain stack into slots" primitive, so this helper is a
/// Phase 1.2-or-later concern.
///
/// Phase 1.2 will likely pre-materialize the expression to slots via
/// `EvalExpr` + a per-flat-valtype scratch and then call
/// `lower_signal_write_to_memory` with the slot list — the same
/// machinery `emit_signal_store` builds at codegen time.
pub fn lower_signal_write_expr_to_memory(
    _ctx: &CompilerContext,
    _signal_ty: Ty,
    _mem: MemSlot,
    _base_addr: u32,
    _expr: crate::lir::block::ExprId,
    _alloc: SlotAlloc<'_>,
) -> LoweredOps {
    todo!(
        "Phase 1.2: lower_signal_write_expr_to_memory — needs an \
         eval-to-flat-slots primitive (see emit_signal_store in \
         op_emit.rs). For Phase 1.1 leave the original LirOp::SignalWriteExpr."
    )
}

/// Lower `LirOp::InitSignal` (memory path) — evaluates the default
/// expression and writes it. Same constraint as
/// `lower_signal_write_expr_to_memory`: needs the eval-to-flat-slots
/// primitive. Phase 1.2 follow-up.
pub fn lower_init_signal_to_memory(
    _ctx: &CompilerContext,
    _signal_ty: Ty,
    _mem: MemSlot,
    _base_addr: u32,
    _expr: crate::lir::block::ExprId,
    _alloc: SlotAlloc<'_>,
) -> LoweredOps {
    todo!(
        "Phase 1.2: lower_init_signal_to_memory — same eval-to-flat-slots \
         dependency as lower_signal_write_expr_to_memory. \
         See emit_signal_store in op_emit.rs."
    )
}

/// Lower `LirOp::TriggerEffects` to a sequence of `CallBlock` ops, one
/// per registered effect for the signal. Phase 1.1 keeps this as a
/// stub because the effect-table lookup requires the per-component
/// effect map (lives on `LirResource.effects`) and the choice of
/// `CallBlock` vs. `CallBlock2` parameters depends on the effect
/// block's signature — both of which need a settled API for the
/// helper to consume. Phase 3 (mount + lifecycle) revisits this with
/// the lifecycle inline helpers.
pub fn lower_trigger_effects(
    _signal: crate::ids::DefId,
    _component_def: crate::ids::DefId,
    _effects: &[crate::lir::block::LirBlockEffect],
    _parent_slot: LirSlotId,
) -> LoweredOps {
    todo!(
        "Phase 3: lower_trigger_effects — fans out to per-effect \
         CallBlock ops. Today's emit arm: op_emit.rs:1123 + \
         emit_trigger_effects helper."
    )
}
