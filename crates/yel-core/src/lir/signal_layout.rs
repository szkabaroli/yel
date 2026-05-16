//! Per-signal storage layout for `LirResource`.
//!
//! Phase 1.1a (LIR-flattening refactor): moves the GC-struct-vs-memory
//! decision and per-signal field/offset bookkeeping out of WasmCodegen
//! and into LIR. `compute_signal_layout` walks a finalized
//! `LirResource` once at the end of THIR→LIR lowering and produces the
//! same data the codegen previously rederived itself in
//! `GcTypeLayout::signal_field_paths` + `MemoryLayout::signal_offsets`.
//!
//! Storage model:
//! - **InStruct**: signal occupies `field_count` consecutive fields on
//!   the component's `$Comp_<i>` GC struct starting at `field_start`.
//!   Most signals are single-slot (`field_count = 1`); strings and
//!   non-typed-array lists are fat pointers (`field_count = 2`).
//! - **InMemory**: signal lives in linear memory (records / tuples in
//!   the legacy memory path) at `base + offset`, `size` bytes wide.
//! - **Zero**: unit-typed signal — no storage allocated.
//!
//! Field counts mirror `WasmPackageBuilder::signal_storage_valtypes`
//! exactly; in-memory predicate matches `LirLayoutContext::is_pointer_repr`.

use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::types::{InternedTyKind, Ty};
use super::block::LirSlotValType;
use super::layout::LirLayoutContext;
use super::node::LirResource;

/// Per-signal layout: where each signal is stored and how many GC
/// struct fields (or memory bytes) it occupies.
///
/// Note: GC-struct presence and linear-memory presence are **not**
/// mutually exclusive in today's codegen — records/tuples currently
/// have BOTH a `GcRef` field on `$Comp_<i>` AND a linear-memory cell
/// reserved by `is_pointer_repr`. The two pieces of data are tracked
/// independently here so the codegen can keep producing byte-identical
/// output during Phase 1.1a.
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct SignalLayout {
    /// One entry per signal (parallel to `LirResource.signals`).
    pub signals: Vec<SignalStorage>,
    /// Total bytes reserved in linear memory for signals that have an
    /// `InMemory` half. Codegen seeds `MemoryLayout::size` with this.
    pub memory_size: u32,
}

/// Where a single signal's value lives. The GC-struct half and the
/// linear-memory half are independent: a signal may have one, both, or
/// neither.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct SignalStorage {
    /// GC struct fields holding this signal on `$Comp_<i>`. `None` for
    /// signals whose ABI slot count is zero (today only unit-typed).
    pub gc: Option<GcSlot>,
    /// Linear-memory cell reserved for this signal. `None` for signals
    /// that don't reserve memory (Scalar / FatPointer signals — the
    /// `is_pointer_repr == false` arm in `MemoryLayout::new`).
    pub mem: Option<MemSlot>,
}

/// Consecutive GC struct fields a signal occupies.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct GcSlot {
    /// Index of the first field. Subsequent slots are at
    /// `field_start + 1`, `field_start + 2`, …
    pub field_start: u32,
    /// Number of consecutive fields. Today: 1 for Scalar / GcRef /
    /// GcArrayRef / FlatGcStruct, 2 for FatPointer (string / non-
    /// typed-array list).
    pub field_count: u32,
}

/// Linear-memory cell a signal reserves.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct MemSlot {
    /// Offset from the component's memory base.
    pub offset: u32,
    /// Size in bytes.
    pub size: u32,
}

/// Number of ABI slots (GC struct fields) a signal of type `ty` occupies.
///
/// Must match `WasmPackageBuilder::signal_storage_valtypes(ty).len()`
/// exactly:
/// - Unit / Error / Unknown: 0
/// - String: 2 (fat pointer)
/// - List<T> where element is GC-eligible (scalar / record / tuple /
///   string / nested-list / FlatGcStruct): 1 (typed `GcArrayRef`)
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
    // FatPointer signals (string, non-typed-array list) are always
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
        InternedTyKind::String => 2,
        InternedTyKind::List(_) => {
            let mut seen = HashSet::new();
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
    let mut memory_offset: u32 = 0;

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
        // Linear-memory half: pointer-repr signals (records / tuples)
        // also reserve a per-instance memory cell. Mirrors the legacy
        // `MemoryLayout::new` allocation loop.
        let mem = if layout_ctx.is_pointer_repr(ty) {
            let size = layout_ctx.size_of(ty);
            let offset = memory_offset;
            memory_offset += size;
            Some(MemSlot { offset, size })
        } else {
            None
        };
        signals.push(SignalStorage { gc, mem });
    }

    SignalLayout {
        signals,
        memory_size: memory_offset,
    }
}

impl SignalLayout {
    /// True iff signal `sig_idx` has a backing GC-struct slot.
    pub fn signal_in_struct(&self, sig_idx: usize) -> bool {
        self.signals.get(sig_idx).map_or(false, |s| s.gc.is_some())
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

    /// Per-component linear-memory offset of signal `sig_idx`, if any.
    pub fn signal_memory_offset(&self, sig_idx: usize) -> Option<u32> {
        self.signals.get(sig_idx).and_then(|s| s.mem).map(|m| m.offset)
    }
}
