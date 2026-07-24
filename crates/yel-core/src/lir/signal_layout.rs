//! Per-signal storage layout for `LirResource`.
//!
//! Phase 1.1a (LIR-flattening refactor): moves the GC-struct field
//! bookkeeping out of WasmCodegen and into LIR. `compute_signal_layout`
//! walks a finalized `LirResource` once at the end of THIR→LIR lowering
//! and produces the same data the codegen previously rederived itself in
//! `GcTypeLayout::signal_field_paths`.
//!
//! Storage model:
//! - **InStruct**: signal occupies `field_count` consecutive fields on
//!   the component's `$Comp_<i>` GC struct starting at `field_start`.
//!   Most signals are single-slot (`field_count = 1`); non-typed-array
//!   lists are fat pointers (`field_count = 2`).
//! - **Zero**: unit-typed signal — no storage allocated.
//!
//! Every non-unit signal is GC-struct-resident. Records/tuples used to
//! *also* reserve a per-instance linear-memory cell (dual storage); that
//! backing is gone — they now live solely on the struct, and boundary
//! getters/setters lift/lower through a `cabi_realloc` scratch. Per-signal
//! linear memory no longer exists.
//!
//! Field counts mirror `WasmPackageBuilder::signal_storage_valtypes` exactly.

use rustc_hash::FxHashSet as HashSet;

use serde::{Deserialize, Serialize};

use crate::types::{InternedTyKind, Ty};
use super::block::LirSlotValType;
use super::layout::LirLayoutContext;
use super::node::LirResource;

/// Per-signal layout: which GC struct fields each signal occupies.
///
/// Every non-unit signal is GC-struct-resident. Per-signal linear memory
/// no longer exists.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct SignalLayout {
    /// One entry per signal (parallel to `LirResource.signals`).
    pub signals: Vec<SignalStorage>,
}

/// Where a single signal's value lives — always the `$Comp_<i>` GC
/// struct. `gc` is `Some` for every non-unit signal, `None` only for
/// zero-slot (unit-typed) signals.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct SignalStorage {
    /// GC struct fields holding this signal on `$Comp_<i>`. `None` for
    /// signals whose ABI slot count is zero (today only unit-typed).
    pub gc: Option<GcSlot>,
}

/// Consecutive GC struct fields a signal occupies.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct GcSlot {
    /// Index of the first field. Subsequent slots are at
    /// `field_start + 1`, `field_start + 2`, …
    pub field_start: u32,
    /// Number of consecutive fields. Today: 1 for Scalar / GcRef /
    /// GcArrayRef / GcVariant, 2 for FatPointer (non-typed-array list).
    pub field_count: u32,
}

/// Number of ABI slots (GC struct fields) a signal of type `ty` occupies.
///
/// Must match `WasmPackageBuilder::signal_storage_valtypes(ty).len()`
/// exactly:
/// - Unit / Error / Unknown: 0
/// - String: 1 (`(ref $str_bytes)` GC byte array)
/// - List<T> where element is GC-eligible (scalar / record / tuple /
///   string / nested-list / GcVariant): 1 (typed `GcArrayRef`)
/// - List<T> otherwise: 2 (fat pointer fallback)
/// - everything else: 1
///
/// The list classification mirrors `WasmPackageBuilder::is_scalar_list_ty`
/// by reusing `lower_to_lir::blocks::is_scalar_list_ty_struct` (the
/// frontend-neutral copy of the same predicate), so the codegen-side GC
/// struct field layout and the LIR-side `SignalLayout` agree
/// slot-for-slot.
/// Per-field `LirSlotValType` for the scratch slot that backs field
/// `field_idx` of a signal of type `ty`. Mirrors
/// `WasmPackageBuilder::signal_storage_valtypes(ty)[field_idx]` so
/// LIR-layer inline writers can allocate scratch with the same wasm
/// shape codegen will later expect.
///
/// Phase 1.1c-i: used by the unified inline signal-write helper to
/// allocate one correctly-typed scratch slot per GC field instead of
/// the legacy "first slot = natural ty, rest = I32" heuristic. For
/// today's signal repertoire the heuristic happens to match for the
/// only multi-field shape (`FatPointer = [I32, I32]`), but new shapes
/// (typed-ref + companion, ref-typed list element, etc.) need the
/// per-field precision.
pub fn lir_slot_val_ty_for_signal_field(
    ctx: &crate::context::CompilerContext,
    ty: Ty,
    field_idx: u32,
) -> LirSlotValType {
    // Today's signal shapes are all expressible without consulting the
    // wasm type section: scalar / fat-pointer / single-ref. The codegen
    // resolves ref-typed signals to RefNull(<idx>) via `signal_storage_valtypes`
    // — at LIR layer we use the symbolic equivalent the rest of the
    // lowering already speaks, falling back to the natural slot val_ty
    // computed by `ty_to_slot_val_type` (mirrored here as `ty_to_slot`).
    let count = slot_count_for_signal_ty(ctx, ty);
    if field_idx >= count {
        return LirSlotValType::I32;
    }
    // FatPointer signals (non-typed-array list) are always
    // [I32, I32] — both fields are i32.
    if count == 2 {
        return LirSlotValType::I32;
    }
    // Single-field signals: pick the natural slot val_ty for `ty`.
    ty_to_slot_val_type_for_signal(ctx, ty)
}

/// Mirror of `LowerToLirCtx::ty_to_slot_val_type` for the signal-field
/// case. Kept here (lib-side) so callers in `crate::lir` don't need a
/// back-edge to `lower_to_lir`.
fn ty_to_slot_val_type_for_signal(
    ctx: &crate::context::CompilerContext,
    ty: Ty,
) -> LirSlotValType {
    match ctx.ty_kind(ty) {
        InternedTyKind::F32 => LirSlotValType::F32,
        InternedTyKind::F64 => LirSlotValType::F64,
        InternedTyKind::S64 | InternedTyKind::U64 => LirSlotValType::I64,
        _ => LirSlotValType::I32,
    }
}

pub fn slot_count_for_signal_ty(
    ctx: &crate::context::CompilerContext,
    ty: Ty,
) -> u32 {
    match ctx.ty_kind(ty) {
        InternedTyKind::Unit | InternedTyKind::Error | InternedTyKind::Unknown => 0,
        // A GC string is a single `(ref $str_bytes)` slot.
        InternedTyKind::String => 1,
        InternedTyKind::List(_) => {
            let mut seen = HashSet::default();
            if crate::lower_to_lir::blocks::is_scalar_list_ty_struct(ctx, ty, &mut seen) {
                1
            } else {
                2
            }
        }
        _ => 1,
    }
}

/// Compute per-signal storage assignments for `component`. Called once
/// at the end of LIR lowering; result lands in `LirResource.signal_layout`.
pub fn compute_signal_layout(
    component: &LirResource,
    layout_ctx: &mut LirLayoutContext,
) -> SignalLayout {
    let mut signals = Vec::with_capacity(component.signals.len());
    let mut next_struct_field: u32 = 0;

    for signal in &component.signals {
        let ty = signal.ty;
        // GC-struct half: every signal whose ABI slot count is > 0
        // gets one or more consecutive fields on `$Comp_<i>`. Mirrors
        // the codegen-side `signal_storage_valtypes(ty).len()` for
        // every Ty kind reachable today.
        let slot_count = slot_count_for_signal_ty(layout_ctx.ctx(), ty);
        let gc = if slot_count > 0 {
            let field_start = next_struct_field;
            next_struct_field += slot_count;
            Some(GcSlot { field_start, field_count: slot_count })
        } else {
            None
        };
        signals.push(SignalStorage { gc });
    }

    SignalLayout { signals }
}

impl SignalLayout {
    /// True iff signal `sig_idx` has a backing GC-struct slot.
    pub fn signal_in_struct(&self, sig_idx: usize) -> bool {
        self.signals.get(sig_idx).is_some_and(|s| s.gc.is_some())
    }

    /// GC struct field indices (in ABI order) for signal `sig_idx`.
    /// Empty when no GC-struct slot is allocated.
    pub fn signal_field_path(&self, sig_idx: usize) -> Vec<u32> {
        match self.signals.get(sig_idx).and_then(|s| s.gc) {
            Some(GcSlot { field_start, field_count }) => {
                (field_start..field_start + field_count).collect()
            }
            None => Vec::new(),
        }
    }
}
