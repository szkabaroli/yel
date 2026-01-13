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
pub struct TypeLayout {
    /// Size in bytes.
    pub size: u32,
    /// Alignment in bytes (always power of 2).
    pub align: u32,
}

impl TypeLayout {
    /// Create a new type layout.
    pub const fn new(size: u32, align: u32) -> Self {
        Self { size, align }
    }

    /// Layout for a zero-sized type.
    pub const fn zero() -> Self {
        Self { size: 0, align: 1 }
    }

    /// Align an offset to this layout's alignment.
    pub fn align_offset(&self, offset: u32) -> u32 {
        align_to(offset, self.align)
    }
}

/// Layout information for a variant/enum type.
#[derive(Debug, Clone)]
pub struct VariantLayout {
    /// Overall layout of the variant.
    pub layout: TypeLayout,
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
    pub layout: TypeLayout,
    /// Field offsets: (field_name, offset, field_type).
    pub field_offsets: Vec<(String, u32, Ty)>,
}

/// Query-based layout context with caching.
///
/// Pass this to codegen functions that need layout information.
/// Layouts are computed on first query and cached for subsequent lookups.
pub struct LayoutContext<'ctx> {
    ctx: &'ctx CompilerContext,
    /// Cache of computed layouts for types.
    cache: HashMap<Ty, TypeLayout>,
    /// Cache of variant layouts.
    variant_cache: HashMap<String, VariantLayout>,
    /// Cache of record layouts by DefId.
    record_cache: HashMap<crate::ids::DefId, RecordLayout>,
}

impl<'ctx> LayoutContext<'ctx> {
    /// Create a new layout context.
    pub fn new(ctx: &'ctx CompilerContext) -> Self {
        Self {
            ctx,
            cache: HashMap::new(),
            variant_cache: HashMap::new(),
            record_cache: HashMap::new(),
        }
    }

    /// Query the layout for a type.
    ///
    /// Results are cached, so repeated queries for the same type are O(1).
    pub fn layout_of(&mut self, ty: Ty) -> TypeLayout {
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
    pub fn size_of(&mut self, ty: Ty) -> u32 {
        self.layout_of(ty).size
    }

    /// Query the alignment for a type.
    pub fn align_of(&mut self, ty: Ty) -> u32 {
        self.layout_of(ty).align
    }

    /// Query variant layout (discriminant size, payload offset, etc.).
    pub fn variant_layout(&mut self, ty_name: &str) -> Option<VariantLayout> {
        // Check cache
        if let Some(layout) = self.variant_cache.get(ty_name) {
            return Some(layout.clone());
        }

        // TODO: Compute for user-defined variants
        None
    }

    /// Get discriminant value for a variant case.
    pub fn variant_discriminant(&self, _ty_name: &str, _case: &str) -> Option<u32> {
        // TODO: Look up from definitions
        None
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
            layout: TypeLayout::new(total_size, max_align),
            field_offsets,
        }
    }

    // ========================================================================
    // Layout computation
    // ========================================================================

    fn compute_layout(&mut self, ty: Ty) -> TypeLayout {
        match self.ctx.ty_kind(ty) {
            // Primitives (Canonical ABI sizes)
            InternedTyKind::Bool => TypeLayout::new(1, 1),
            InternedTyKind::S8 | InternedTyKind::U8 => TypeLayout::new(1, 1),
            InternedTyKind::S16 | InternedTyKind::U16 => TypeLayout::new(2, 2),
            InternedTyKind::S32 | InternedTyKind::U32 => TypeLayout::new(4, 4),
            InternedTyKind::S64 | InternedTyKind::U64 => TypeLayout::new(8, 8),
            InternedTyKind::F32 => TypeLayout::new(4, 4),
            InternedTyKind::F64 => TypeLayout::new(8, 8),
            InternedTyKind::Char => TypeLayout::new(4, 4), // Unicode scalar
            InternedTyKind::Unit => TypeLayout::zero(),

            // Pointer types (ptr, len)
            InternedTyKind::String | InternedTyKind::List(_) => TypeLayout::new(8, 4),

            // Option<T>
            InternedTyKind::Option(inner) => {
                let inner_layout = self.layout_of(*inner);
                self.compute_option_layout(inner_layout)
            }

            // Result<O, E>
            InternedTyKind::Result { ok, err } => {
                let ok_layout = ok
                    .map(|t| self.layout_of(t))
                    .unwrap_or(TypeLayout::zero());
                let err_layout = err
                    .map(|t| self.layout_of(t))
                    .unwrap_or(TypeLayout::zero());
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
                    return TypeLayout::new(disc_size, disc_size.min(4));
                }
                // Check if it's a variant
                if let Some(variant_def) = self.ctx.defs.as_variant(*def_id) {
                    let layout = self.compute_variant_layout_from_def(variant_def);
                    return layout.layout;
                }
                // Unknown - fallback
                TypeLayout::new(4, 4)
            }

            // Function reference
            InternedTyKind::Func { .. } => TypeLayout::new(4, 4),

            // UI-specific types (f32 representation)
            InternedTyKind::Length
            | InternedTyKind::PhysicalLength
            | InternedTyKind::Angle
            | InternedTyKind::Duration
            | InternedTyKind::Percent
            | InternedTyKind::RelativeFontSize
            | InternedTyKind::Color
            | InternedTyKind::Brush => TypeLayout::new(4, 4),

            InternedTyKind::Image => TypeLayout::new(4, 4), // handle
            InternedTyKind::Easing => TypeLayout::new(4, 4), // enum-like

            // Unknown - fallback
            InternedTyKind::Unknown | InternedTyKind::Error => TypeLayout::new(4, 4),
        }
    }

    fn compute_option_layout(&self, inner: TypeLayout) -> TypeLayout {
        // option<T> = 1-byte discriminant + padding + T
        let payload_offset = align_to(1, inner.align);
        let total_size = payload_offset + inner.size;
        let total_align = inner.align.max(1);
        TypeLayout::new(align_to(total_size, total_align), total_align)
    }

    fn compute_result_layout(&self, ok: TypeLayout, err: TypeLayout) -> TypeLayout {
        // result<O, E> = 1-byte discriminant + padding + max(O, E)
        let max_align = ok.align.max(err.align).max(1);
        let max_size = ok.size.max(err.size);
        let payload_offset = align_to(1, max_align);
        let total_size = payload_offset + max_size;
        TypeLayout::new(align_to(total_size, max_align), max_align)
    }

    fn compute_tuple_layout(&self, layouts: &[TypeLayout]) -> TypeLayout {
        if layouts.is_empty() {
            return TypeLayout::zero();
        }

        let mut offset = 0u32;
        let mut max_align = 1u32;

        for layout in layouts {
            max_align = max_align.max(layout.align);
            offset = align_to(offset, layout.align);
            offset += layout.size;
        }

        TypeLayout::new(align_to(offset, max_align), max_align)
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
            {
                if let Some(payload_ty) = case.payload {
                    let payload_layout = self.layout_of(payload_ty);
                    max_payload_size = max_payload_size.max(payload_layout.size);
                    max_payload_align = max_payload_align.max(payload_layout.align);
                }
            }
        }

        let payload_offset = align_to(disc_size, max_payload_align);
        let total_size = align_to(
            payload_offset + max_payload_size,
            max_payload_align.max(disc_size),
        );
        let total_align = max_payload_align.max(disc_size.min(4));

        VariantLayout {
            layout: TypeLayout::new(total_size, total_align),
            discriminant_size: disc_size,
            payload_offset,
            max_payload_size,
        }
    }
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
        let mut layout_ctx = LayoutContext::new(&ctx);

        // Test with constants that exist
        assert_eq!(layout_ctx.layout_of(Ty::BOOL), TypeLayout::new(1, 1));
        assert_eq!(layout_ctx.layout_of(Ty::S32), TypeLayout::new(4, 4));
    }

    #[test]
    fn test_string_layout() {
        let ctx = CompilerContext::new();
        let mut layout_ctx = LayoutContext::new(&ctx);
        // String is (ptr, len) = 8 bytes, 4-byte aligned
        assert_eq!(layout_ctx.layout_of(Ty::STRING), TypeLayout::new(8, 4));
    }

    #[test]
    fn test_layout_caching() {
        let ctx = CompilerContext::new();
        let mut layout_ctx = LayoutContext::new(&ctx);

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
