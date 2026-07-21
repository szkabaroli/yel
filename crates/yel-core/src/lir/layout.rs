//! Type layout calculation for Canonical ABI.
//!
//! This module provides layout calculation for WASM code generation.
//! Layouts are computed lazily and cached for efficiency.
//!
//! # Example
//!
//! ```ignore
//! let mut ctx = LayoutContext::new(&compiler_ctx);
//! let layout = ctx.layout_of(Ty::S32);
//! assert_eq!(layout.size, 4);
//! assert_eq!(layout.align, 4);
//! ```

use std::collections::HashMap;

use crate::context::CompilerContext;
use crate::types::{InternedTyKind, Ty};

/// Size and alignment for a type (Canonical ABI).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LirTypeLayout {
    /// Size in bytes.
    pub size: u32,
    /// Alignment in bytes (always power of 2).
    pub align: u32,
}

impl LirTypeLayout {
    /// Create a new type layout.
    pub const fn new(size: u32, align: u32) -> Self {
        Self { size, align }
    }

    /// Layout for a zero-sized type.
    pub const fn zero() -> Self {
        Self { size: 0, align: 1 }
    }
}

/// Layout information for a variant/enum type.
#[derive(Debug, Clone)]
pub struct VariantLayout {
    /// Overall layout of the variant.
    pub layout: LirTypeLayout,
    /// Size of the discriminant (1, 2, or 4 bytes).
    pub discriminant_size: u32,
    /// Offset where payload starts (after discriminant, aligned).
    pub payload_offset: u32,
    /// Maximum payload size across all cases.
    pub max_payload_size: u32,
}

/// Layout information for a record type.
#[derive(Debug, Clone)]
pub struct RecordLayout {
    /// Overall layout of the record.
    pub layout: LirTypeLayout,
    /// Field offsets: (field_name, offset, field_type).
    pub field_offsets: Vec<(String, u32, Ty)>,
    /// GC struct field indices, parallel to `field_offsets`.
    /// Phase 1 of the records-to-GC migration populates this from the
    /// codegen side once per-record `(struct ...)` types are emitted; in
    /// the same order as `field_offsets`. Empty when the record's GC
    /// type has not yet been registered. Phase 2+ consumers will read
    /// these to emit `struct.get` / `struct.set` against the GC struct
    /// instead of the legacy `i32.load` / `i32.store` byte-offset path.
    pub field_gc_indices: Vec<u32>,
}

/// Query-based layout context with caching.
///
/// Pass this to codegen functions that need layout information.
/// Layouts are computed on first query and cached for subsequent lookups.
pub struct LirLayoutContext<'ctx> {
    ctx: &'ctx CompilerContext,
    /// Cache of computed layouts for types.
    cache: HashMap<Ty, LirTypeLayout>,
    /// Cache of record layouts by DefId.
    record_cache: HashMap<crate::ids::DefId, RecordLayout>,
}

impl<'ctx> LirLayoutContext<'ctx> {
    /// Create a new layout context.
    pub fn new(ctx: &'ctx CompilerContext) -> Self {
        Self {
            ctx,
            cache: HashMap::new(),
            record_cache: HashMap::new(),
        }
    }

    /// Query the layout for a type.
    ///
    /// Results are cached, so repeated queries for the same type are O(1).
    pub fn layout_of(&mut self, ty: Ty) -> LirTypeLayout {
        // Check cache first
        if let Some(&layout) = self.cache.get(&ty) {
            return layout;
        }

        // Compute layout
        let layout = self.compute_layout(ty);

        // Cache and return
        self.cache.insert(ty, layout);
        layout
    }

    /// Query the size for a type.
    /// True iff a value of `ty` is internally represented as a single
    /// pointer into linear memory (records, tuples) rather than as flat
    /// stack slots or a fat-pointer pair. Mirrors the codegen-side
    /// `InternalRepr::Pointer` classification but lives in yel-core so
    /// `MemoryLayout::new` can decide whether a signal still needs
    /// per-instance byte allocation: every signal type whose internal
    /// repr is **not** a Pointer is GC-struct-resident and reserves
    /// zero bytes in linear memory.
    /// Expose the underlying `CompilerContext` for callers that need to
    /// inspect `InternedTyKind` alongside layout queries.
    pub fn ctx(&self) -> &'ctx CompilerContext {
        self.ctx
    }

    pub fn is_pointer_repr(&self, ty: Ty) -> bool {
        use crate::definitions::DefKind;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Tuple(_) => true,
            InternedTyKind::Adt(def_id) => {
                matches!(self.ctx.defs.kind(*def_id), DefKind::Record(_))
            }
            _ => false,
        }
    }

    pub fn size_of(&mut self, ty: Ty) -> u32 {
        self.layout_of(ty).size
    }

    /// Query the alignment for a type.
    pub fn align_of(&mut self, ty: Ty) -> u32 {
        self.layout_of(ty).align
    }

    /// Query record layout by DefId.
    pub fn record_layout_by_id(&mut self, def_id: crate::ids::DefId) -> Option<RecordLayout> {
        // Check cache
        if let Some(layout) = self.record_cache.get(&def_id) {
            return Some(layout.clone());
        }

        // Look up record definition
        let record = self.ctx.defs.as_record(def_id)?;

        // Compute layout
        let layout = self.compute_record_layout(&record.fields);
        self.record_cache.insert(def_id, layout.clone());
        Some(layout)
    }

    /// Get the offset of a field within a record.
    pub fn field_offset(&mut self, def_id: crate::ids::DefId, field_name: &str) -> Option<u32> {
        let layout = self.record_layout_by_id(def_id)?;
        layout
            .field_offsets
            .iter()
            .find(|(name, _, _)| name == field_name)
            .map(|(_, offset, _)| *offset)
    }

    /// Compute layout for a record type.
    fn compute_record_layout(&mut self, fields: &[crate::ids::DefId]) -> RecordLayout {
        let mut offset = 0u32;
        let mut max_align = 1u32;
        let mut field_offsets = Vec::new();

        for &field_def_id in fields {
            if let Some(field) = self.ctx.defs.as_field(field_def_id) {
                let field_layout = self.layout_of(field.ty);

                // Align offset for this field
                offset = align_to(offset, field_layout.align);
                max_align = max_align.max(field_layout.align);

                let field_name = self.ctx.str(field.name);
                field_offsets.push((field_name.to_string(), offset, field.ty));

                // Advance offset
                offset += field_layout.size;
            }
        }

        // Final padding
        let total_size = align_to(offset, max_align);

        RecordLayout {
            layout: LirTypeLayout::new(total_size, max_align),
            field_offsets,
            field_gc_indices: Vec::new(),
        }
    }

    // ========================================================================
    // Layout computation
    // ========================================================================

    fn compute_layout(&mut self, ty: Ty) -> LirTypeLayout {
        match self.ctx.ty_kind(ty) {
            // Primitives (Canonical ABI sizes)
            InternedTyKind::Bool => LirTypeLayout::new(1, 1),
            InternedTyKind::S8 | InternedTyKind::U8 => LirTypeLayout::new(1, 1),
            InternedTyKind::S16 | InternedTyKind::U16 => LirTypeLayout::new(2, 2),
            InternedTyKind::S32 | InternedTyKind::U32 => LirTypeLayout::new(4, 4),
            InternedTyKind::S64 | InternedTyKind::U64 => LirTypeLayout::new(8, 8),
            InternedTyKind::F32 => LirTypeLayout::new(4, 4),
            InternedTyKind::F64 => LirTypeLayout::new(8, 8),
            InternedTyKind::Char => LirTypeLayout::new(4, 4), // Unicode scalar
            InternedTyKind::Unit => LirTypeLayout::zero(),

            // Pointer types (ptr, len)
            InternedTyKind::String | InternedTyKind::List(_) => LirTypeLayout::new(8, 4),

            // Option<T>
            InternedTyKind::Option(inner) => {
                let inner_layout = self.layout_of(*inner);
                self.compute_option_layout(inner_layout)
            }

            // Result<O, E>
            InternedTyKind::Result { ok, err } => {
                let ok_layout = ok
                    .map(|t| self.layout_of(t))
                    .unwrap_or(LirTypeLayout::zero());
                let err_layout = err
                    .map(|t| self.layout_of(t))
                    .unwrap_or(LirTypeLayout::zero());
                self.compute_result_layout(ok_layout, err_layout)
            }

            // Tuple
            InternedTyKind::Tuple(elems) => {
                let layouts: Vec<_> = elems.iter().map(|&t| self.layout_of(t)).collect();
                self.compute_tuple_layout(&layouts)
            }

            // ADT (record/enum/variant)
            InternedTyKind::Adt(def_id) => {
                // Check if it's a record
                if let Some(layout) = self.record_layout_by_id(*def_id) {
                    return layout.layout;
                }
                // Check if it's an enum
                if let Some(enum_def) = self.ctx.defs.as_enum(*def_id) {
                    let disc_size = discriminant_size(enum_def.cases.len());
                    return LirTypeLayout::new(disc_size, disc_size.min(4));
                }
                // Check if it's a variant
                if let Some(variant_def) = self.ctx.defs.as_variant(*def_id) {
                    let layout = self.compute_variant_layout_from_def(variant_def);
                    return layout.layout;
                }
                // Unknown - fallback
                LirTypeLayout::new(4, 4)
            }

            // Function reference
            InternedTyKind::Func { .. } => LirTypeLayout::new(4, 4),

            // UI-specific types (f32 representation)
            InternedTyKind::Length
            | InternedTyKind::PhysicalLength
            | InternedTyKind::Angle
            | InternedTyKind::Duration
            | InternedTyKind::Percent
            | InternedTyKind::RelativeFontSize
            | InternedTyKind::Color
            | InternedTyKind::Brush => LirTypeLayout::new(4, 4),

            InternedTyKind::Image => LirTypeLayout::new(4, 4), // handle
            InternedTyKind::Easing => LirTypeLayout::new(4, 4), // enum-like

            // Unknown - fallback
            InternedTyKind::Unknown | InternedTyKind::Error => LirTypeLayout::new(4, 4),
        }
    }

    fn compute_option_layout(&self, inner: LirTypeLayout) -> LirTypeLayout {
        // option<T> = 1-byte discriminant + padding + T
        let payload_offset = align_to(1, inner.align);
        let total_size = payload_offset + inner.size;
        let total_align = inner.align.max(1);
        LirTypeLayout::new(align_to(total_size, total_align), total_align)
    }

    fn compute_result_layout(&self, ok: LirTypeLayout, err: LirTypeLayout) -> LirTypeLayout {
        // result<O, E> = 1-byte discriminant + padding + max(O, E)
        let max_align = ok.align.max(err.align).max(1);
        let max_size = ok.size.max(err.size);
        let payload_offset = align_to(1, max_align);
        let total_size = payload_offset + max_size;
        LirTypeLayout::new(align_to(total_size, max_align), max_align)
    }

    fn compute_tuple_layout(&self, layouts: &[LirTypeLayout]) -> LirTypeLayout {
        if layouts.is_empty() {
            return LirTypeLayout::zero();
        }

        let mut offset = 0u32;
        let mut max_align = 1u32;

        for layout in layouts {
            max_align = max_align.max(layout.align);
            offset = align_to(offset, layout.align);
            offset += layout.size;
        }

        LirTypeLayout::new(align_to(offset, max_align), max_align)
    }

    /// Public wrapper around variant layout computation from a `VariantDef`.
    /// Used by codegen to find `payload_offset` for variant signal setters
    /// without re-deriving the ABI rules inline.
    pub fn compute_variant_layout_from_def_public(
        &mut self,
        variant: &crate::definitions::VariantDef,
    ) -> VariantLayout {
        self.compute_variant_layout_from_def(variant)
    }

    fn compute_variant_layout_from_def(
        &mut self,
        variant: &crate::definitions::VariantDef,
    ) -> VariantLayout {
        let num_cases = variant.cases.len();
        let disc_size = discriminant_size(num_cases);

        let mut max_payload_size = 0u32;
        let mut max_payload_align = 1u32;

        for &case_def_id in &variant.cases {
            if let crate::definitions::DefKind::VariantCase(case) = self.ctx.defs.kind(case_def_id)
                && let Some(payload_ty) = case.payload {
                    let payload_layout = self.layout_of(payload_ty);
                    max_payload_size = max_payload_size.max(payload_layout.size);
                    max_payload_align = max_payload_align.max(payload_layout.align);
                }
        }

        let payload_offset = align_to(disc_size, max_payload_align);
        let total_size = align_to(
            payload_offset + max_payload_size,
            max_payload_align.max(disc_size),
        );
        let total_align = max_payload_align.max(disc_size.min(4));

        VariantLayout {
            layout: LirTypeLayout::new(total_size, total_align),
            discriminant_size: disc_size,
            payload_offset,
            max_payload_size,
        }
    }
}

/// Core WASM valtypes used for flattened canonical-ABI representations.
/// Mirrors `wasm_encoder::ValType` but kept inside `yel-core` so LIR can
/// reason about flat-slot counts without depending on `wasm-encoder`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LirCoreValType {
    I32,
    I64,
    F32,
    F64,
}

/// Per-valtype slot counts for a flattened canonical-ABI value.
/// Order matches the codegen scratch region: i32, i64, f32, f64.
pub type FlatValTypeCounts = (u32, u32, u32, u32);

impl<'ctx> LirLayoutContext<'ctx> {
    /// Compute the canonical-ABI flat core valtypes for a value of `ty`,
    /// matching `yel_wasm_codegen::wasm::flatten_core_valtypes` exactly
    /// (same join rules for Result/Variant). Used by LIR-side passes to
    /// pre-compute scratch-local counts without crossing into the codegen
    /// crate.
    pub fn canonical_flat_valtypes(&mut self, ty: Ty) -> Vec<LirCoreValType> {
        // Snapshot the owned data we need before recursing, since recursion
        // takes `&mut self` and would conflict with `self.ctx.ty_kind(ty)`'s
        // borrow held across the match arms.
        enum Shape {
            Single(LirCoreValType),
            FatPtr,
            Option(Ty),
            Result(Option<Ty>, Option<Ty>),
            Tuple(Vec<Ty>),
            Record(Vec<Ty>),
            Variant(Vec<Option<Ty>>),
            DiscOnly,
        }
        let shape = match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => Shape::Single(LirCoreValType::F32),
            InternedTyKind::F64 => Shape::Single(LirCoreValType::F64),
            InternedTyKind::S64 | InternedTyKind::U64 => Shape::Single(LirCoreValType::I64),
            InternedTyKind::String | InternedTyKind::List(_) => Shape::FatPtr,
            InternedTyKind::Option(inner) => Shape::Option(*inner),
            InternedTyKind::Result { ok, err } => Shape::Result(*ok, *err),
            InternedTyKind::Tuple(elements) => Shape::Tuple(elements.to_vec()),
            InternedTyKind::Adt(def_id) => {
                let def_id = *def_id;
                if let Some(rec_def) = self.ctx.defs.as_record(def_id) {
                    let fields = rec_def.fields.clone();
                    let mut tys = Vec::new();
                    for field_def_id in fields {
                        if let crate::definitions::DefKind::Field(f) =
                            self.ctx.defs.kind(field_def_id)
                        {
                            tys.push(f.ty);
                        }
                    }
                    Shape::Record(tys)
                } else if let Some(var_def) = self.ctx.defs.as_variant(def_id) {
                    let cases = var_def.cases.clone();
                    let mut case_payloads: Vec<Option<Ty>> = Vec::new();
                    for case_def_id in cases {
                        let payload = match self.ctx.defs.kind(case_def_id) {
                            crate::definitions::DefKind::VariantCase(c) => c.payload,
                            _ => None,
                        };
                        case_payloads.push(payload);
                    }
                    Shape::Variant(case_payloads)
                } else {
                    Shape::DiscOnly
                }
            }
            _ => Shape::Single(LirCoreValType::I32),
        };
        match shape {
            Shape::Single(v) => vec![v],
            Shape::FatPtr => vec![LirCoreValType::I32, LirCoreValType::I32],
            Shape::Option(inner) => {
                let mut v = vec![LirCoreValType::I32];
                v.extend(self.canonical_flat_valtypes(inner));
                v
            }
            Shape::Result(ok, err) => {
                let ok_flat = ok
                    .map(|t| self.canonical_flat_valtypes(t))
                    .unwrap_or_default();
                let err_flat = err
                    .map(|t| self.canonical_flat_valtypes(t))
                    .unwrap_or_default();
                let mut v = vec![LirCoreValType::I32];
                v.extend(join_flat_lir_valtypes(&ok_flat, &err_flat));
                v
            }
            Shape::Tuple(elements) => {
                let mut v = Vec::new();
                for t in elements {
                    v.extend(self.canonical_flat_valtypes(t));
                }
                v
            }
            Shape::Record(field_tys) => {
                let mut v = Vec::new();
                for t in field_tys {
                    v.extend(self.canonical_flat_valtypes(t));
                }
                v
            }
            Shape::Variant(case_payloads) => {
                let mut case_flats: Vec<Vec<LirCoreValType>> = Vec::new();
                for payload in case_payloads {
                    let f = payload
                        .map(|t| self.canonical_flat_valtypes(t))
                        .unwrap_or_default();
                    case_flats.push(f);
                }
                let mut joined: Vec<LirCoreValType> = Vec::new();
                for f in &case_flats {
                    joined = join_flat_lir_valtypes(&joined, f);
                }
                let mut v = vec![LirCoreValType::I32];
                v.extend(joined);
                v
            }
            Shape::DiscOnly => vec![LirCoreValType::I32],
        }
    }

    /// Per-valtype slot counts (i32, i64, f32, f64) for a value of `ty`
    /// under canonical ABI flattening.
    pub fn canonical_flat_valtype_counts(&mut self, ty: Ty) -> FlatValTypeCounts {
        let flat = self.canonical_flat_valtypes(ty);
        per_valtype_counts(&flat)
    }
}

/// Per-valtype tally over a flattened valtype list.
pub fn per_valtype_counts(flat: &[LirCoreValType]) -> FlatValTypeCounts {
    let (mut i32c, mut i64c, mut f32c, mut f64c) = (0u32, 0u32, 0u32, 0u32);
    for v in flat {
        match v {
            LirCoreValType::I32 => i32c += 1,
            LirCoreValType::I64 => i64c += 1,
            LirCoreValType::F32 => f32c += 1,
            LirCoreValType::F64 => f64c += 1,
        }
    }
    (i32c, i64c, f32c, f64c)
}

/// Element-wise join of two flat valtype lists under canonical-ABI
/// promotion: any 64-bit slot wins; otherwise i32. Mirrors
/// `yel_wasm_codegen::wasm::join_flat_valtypes`.
fn join_flat_lir_valtypes(a: &[LirCoreValType], b: &[LirCoreValType]) -> Vec<LirCoreValType> {
    let n = a.len().max(b.len());
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        let av = a.get(i).copied();
        let bv = b.get(i).copied();
        let merged = match (av, bv) {
            (Some(x), None) | (None, Some(x)) => x,
            (Some(x), Some(y)) if x == y => x,
            (Some(x), Some(y)) => {
                let is_64 =
                    |v: LirCoreValType| matches!(v, LirCoreValType::I64 | LirCoreValType::F64);
                if is_64(x) || is_64(y) {
                    LirCoreValType::I64
                } else {
                    LirCoreValType::I32
                }
            }
            (None, None) => LirCoreValType::I32,
        };
        out.push(merged);
    }
    out
}

/// Element-wise max over two `FlatValTypeCounts` tuples.
pub fn max_flat_counts(a: FlatValTypeCounts, b: FlatValTypeCounts) -> FlatValTypeCounts {
    (a.0.max(b.0), a.1.max(b.1), a.2.max(b.2), a.3.max(b.3))
}

// ============================================================================
// Helper functions
// ============================================================================

/// Align an offset to the given alignment (must be power of 2).
pub fn align_to(offset: u32, align: u32) -> u32 {
    debug_assert!(align.is_power_of_two(), "alignment must be power of 2");
    (offset + align - 1) & !(align - 1)
}

/// Compute discriminant size based on number of cases.
pub fn discriminant_size(num_cases: usize) -> u32 {
    if num_cases <= 256 {
        1
    } else if num_cases <= 65536 {
        2
    } else {
        4
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_layouts() {
        let ctx = CompilerContext::new();
        let mut layout_ctx = LirLayoutContext::new(&ctx);

        // Test with constants that exist
        assert_eq!(layout_ctx.layout_of(Ty::BOOL), LirTypeLayout::new(1, 1));
        assert_eq!(layout_ctx.layout_of(Ty::S32), LirTypeLayout::new(4, 4));
    }

    #[test]
    fn test_string_layout() {
        let ctx = CompilerContext::new();
        let mut layout_ctx = LirLayoutContext::new(&ctx);
        // String is (ptr, len) = 8 bytes, 4-byte aligned
        assert_eq!(layout_ctx.layout_of(Ty::STRING), LirTypeLayout::new(8, 4));
    }

    #[test]
    fn test_layout_caching() {
        let ctx = CompilerContext::new();
        let mut layout_ctx = LirLayoutContext::new(&ctx);

        // First query computes
        let layout1 = layout_ctx.layout_of(Ty::S32);

        // Second query hits cache
        let layout2 = layout_ctx.layout_of(Ty::S32);

        assert_eq!(layout1, layout2);
        assert_eq!(layout_ctx.cache.len(), 1);
    }

    #[test]
    fn test_align_to() {
        assert_eq!(align_to(0, 4), 0);
        assert_eq!(align_to(1, 4), 4);
        assert_eq!(align_to(4, 4), 4);
        assert_eq!(align_to(5, 4), 8);
        assert_eq!(align_to(7, 8), 8);
        assert_eq!(align_to(8, 8), 8);
    }

    #[test]
    fn test_discriminant_size() {
        assert_eq!(discriminant_size(2), 1);
        assert_eq!(discriminant_size(256), 1);
        assert_eq!(discriminant_size(257), 2);
        assert_eq!(discriminant_size(65536), 2);
        assert_eq!(discriminant_size(65537), 4);
    }
}
