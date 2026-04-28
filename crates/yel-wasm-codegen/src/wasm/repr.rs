//! Canonical WASM representation helpers.
//!
//! Single source of truth for "how is a Yel value represented on the
//! WASM stack / in WASM blocks". Every emit site that used to
//! independently decompose a type into flat slots must funnel through
//! this module — when a representation question has one answer, it
//! can't become inconsistent between producer and consumer.
//!
//! Non-goals right now: this module doesn't yet wrap the full
//! push/pop/store/load emission surface. It starts with the narrow
//! slice that's biting us (ternary / if block types) and will grow as
//! more emit sites migrate. See
//! `.claude/plans/uniform-pointer-passing.md` for context on why we
//! went with centralised-helpers instead of a full representation
//! refactor — the latter pays runtime cost this doesn't.
//!
//! Rule enforced by convention (not yet type-system): the only call
//! site of `crate::wasm::WasmPackageBuilder::flatten_core_valtypes`
//! outside this module should be the boundary-shim generator (WIT
//! export lifts/lowers). If you find yourself calling it in a new
//! internal emit path, add a helper here instead.
use std::collections::HashMap;

use wasm_encoder::{BlockType, ValType};
use yel_core::lir::{LirExpr, LirExprKind};
use yel_core::Ty;

use crate::CodegenError;

use super::WasmPackageBuilder;

/// Classify the internal WASM-stack representation of a Yel type.
///
/// This is **not** the canonical ABI flattening. The internal
/// representation is what lives on the WASM stack between opcodes
/// inside the module; canonical ABI only matters at WIT boundaries.
/// The two diverge for records and tuples: canonical ABI flattens
/// their fields, but internally we always pass them as a single `i32`
/// pointer into linear memory.
///
/// Rule of thumb:
///   - primitive scalar → its matching `ValType` (1 slot)
///   - `string`, `list<T>` → fat pointer (`i32`, `i32`) — 2 slots
///   - record / tuple → single `i32` pointer — 1 slot
///   - option / result / variant / enum → flat (same as canonical)
///   - unit → zero slots
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum InternalRepr {
    /// Not valid for emit: Unit / error / unknown types that shouldn't
    /// reach internal code paths. Return zero stack slots.
    Zero,
    /// Exactly one stack slot of the given valtype.
    Scalar(wasm_encoder::ValType),
    /// `(i32, i32)` fat pointer — used for strings, lists.
    FatPointer,
    /// A single `i32` pointer to memory — used for records and tuples
    /// that are pointer-passed internally.
    Pointer,
    /// Phase 2 GC migration: a primitive-only record stored as a
    /// `(ref null $<rec>_record)` GC ref (1 stack slot). The contained
    /// `u32` is the record's GC struct type index. Replaces the
    /// `Pointer` repr for records that are eligible for the new path.
    GcRef(u32),
    /// Phase 5b-v.3: a `list<scalar>` stored as a `(ref null
    /// $<elem>_list)` GC array ref (1 stack slot). The contained `u32`
    /// is the list array GC type index from
    /// `RecordGcTypes::list_array_type_idx`. Replaces `FatPointer` for
    /// migrated scalar lists.
    GcArrayRef(u32),
    /// Multi-slot canonical-ABI shape — option / result / variant /
    /// enum. Internal representation matches canonical exactly for
    /// these, so flat-slot push/pop works.
    Flat,
}

impl WasmPackageBuilder<'_> {
    /// Classify how a Yel type is represented on the **internal** WASM
    /// stack. Every emit site that needs to know "how many values does
    /// this type push" should query this instead of
    /// `flatten_core_valtypes` — the latter is canonical ABI
    /// (boundary-facing) and disagrees with internal representation
    /// for record/tuple (pointer-passed) values.
    pub(super) fn internal_repr(&self, ty: Ty) -> InternalRepr {
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        use wasm_encoder::ValType;
        // Phase 2/3: single-level records (POR + records with string /
        // list<scalar> fields) have a GC-ref internal repr.
        if let Some(type_idx) = self.por_record_type_idx(ty) {
            return InternalRepr::GcRef(type_idx);
        }
        // Phase 5e.3: tuples migrate to typed GC struct refs when a
        // `tuple_struct_type_idx` is registered (always, post-5a).
        if let yel_core::types::InternedTyKind::Tuple(_) = self.ctx.ty_kind(ty) {
            if let Some(&tup_idx) = self.record_gc_types.tuple_struct_type_idx.get(&ty) {
                return InternalRepr::GcRef(tup_idx);
            }
        }
        // Phase 5b-v.3: scalar lists migrate to a typed GC array ref.
        if self.is_scalar_list_ty(ty) {
            if let Some(&arr_idx) = self.record_gc_types.list_array_type_idx.get(&ty) {
                return InternalRepr::GcArrayRef(arr_idx);
            }
        }
        // Option-of-ref collapse: option<T> where T has a ref repr is
        // itself just a nullable ref of T's heap type (none = null,
        // some(v) = v). No discriminant slot internally.
        if let yel_core::types::InternedTyKind::Option(inner_ty) = self.ctx.ty_kind(ty) {
            let inner_ty = *inner_ty;
            match self.internal_repr(inner_ty) {
                InternalRepr::GcRef(idx) => return InternalRepr::GcRef(idx),
                InternalRepr::GcArrayRef(idx) => return InternalRepr::GcArrayRef(idx),
                _ => {}
            }
        }
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Unit | InternedTyKind::Error | InternedTyKind::Unknown => {
                InternalRepr::Zero
            }
            InternedTyKind::F32 => InternalRepr::Scalar(ValType::F32),
            InternedTyKind::F64 => InternalRepr::Scalar(ValType::F64),
            InternedTyKind::S64 | InternedTyKind::U64 => InternalRepr::Scalar(ValType::I64),
            InternedTyKind::String | InternedTyKind::List(_) => InternalRepr::FatPointer,
            InternedTyKind::Tuple(_) => InternalRepr::Pointer,
            InternedTyKind::Adt(def_id) => match self.ctx.defs.kind(*def_id) {
                DefKind::Record(_) => InternalRepr::Pointer,
                DefKind::Variant(_) | DefKind::Enum(_) => InternalRepr::Flat,
                _ => InternalRepr::Flat,
            },
            InternedTyKind::Option(_) | InternedTyKind::Result { .. } => InternalRepr::Flat,
            // Everything else (bool / narrow ints / char / length units / …)
            // is a single i32 slot.
            _ => InternalRepr::Scalar(ValType::I32),
        }
    }

    /// Per-struct-field valtypes used to back a signal of `ty` in the
    /// component's `$Comp_<i>` GC struct. Returns an empty vec for
    /// types that are **not yet** migrated off linear memory — today
    /// that's `Pointer` types (records, tuples) which have a
    /// canonical-ABI flattened external representation (multi-slot)
    /// but a single-pointer internal representation, so callers can
    /// detect "skip GC struct, keep memory" by checking
    /// `is_empty()`.
    /// - Scalar(vt) → `[vt]` (1 field; covers F32/F64/S64/U64/bool/
    ///   narrow ints widened to i32 — fields are full-width, the
    ///   narrow-store/load dance is no longer needed since each field
    ///   has its own slot).
    /// - FatPointer → `[I32, I32]` for strings/lists (ptr, len).
    /// - Flat → canonical-ABI flattening for option/result/variant/enum.
    /// - Pointer → `[]` (sentinel: keep linear-memory storage).
    /// - Zero → `[]` (no value).
    pub(crate) fn signal_storage_valtypes(&self, ty: Ty) -> Vec<wasm_encoder::ValType> {
        use wasm_encoder::{HeapType, RefType, ValType};
        match self.internal_repr(ty) {
            InternalRepr::Zero => Vec::new(),
            InternalRepr::Scalar(vt) => vec![vt],
            InternalRepr::FatPointer => vec![ValType::I32, ValType::I32],
            InternalRepr::Pointer => Vec::new(),
            // Phase 2: primitive-only records migrate to a single
            // `(ref null $<rec>_record)` field on the component
            // struct. Non-POR records (mixed with strings/lists/
            // nested types) keep `InternalRepr::Pointer` and stay on
            // the legacy memory path until later phases.
            InternalRepr::GcRef(type_idx) => vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(type_idx),
            })],
            InternalRepr::GcArrayRef(arr_idx) => vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(arr_idx),
            })],
            InternalRepr::Flat => self.flatten_core_valtypes(ty),
        }
    }

    /// True if this global-block property lives in its block's
    /// `$globals_<i>` GC struct (callers should use struct.get /
    /// struct.set via the per-block self-global), false if it is
    /// pointer-typed (record/tuple) and still backed by linear memory
    /// via `global_property_addrs`.
    ///
    /// Returns `false` for any DefId that is not a global-block
    /// property (callers should establish the property's owner via
    /// `Definitions::owning_global_block` before deciding which path
    /// to take).
    pub(crate) fn global_in_struct(&self, prop_def_id: yel_core::DefId) -> bool {
        let block_id = match self.ctx.defs.owning_global_block(prop_def_id) {
            Some(b) => b,
            None => return false,
        };
        let layout_idx = match self.global_block_def_to_idx.get(&block_id) {
            Some(&i) => i,
            None => return false,
        };
        let layout = &self.globals_layouts[layout_idx];
        let block = match self.ctx.defs.as_global(block_id) {
            Some(b) => b,
            None => return false,
        };
        let prop_pos = match block.properties.iter().position(|&p| p == prop_def_id) {
            Some(p) => p,
            None => return false,
        };
        layout
            .property_field_paths
            .get(prop_pos)
            .map(|p| !p.is_empty())
            .unwrap_or(false)
    }

    /// True if this component-local signal lives in the `$Comp_<i>`
    /// GC struct (callers should use struct.get/struct.set), false if
    /// it is still backed by linear memory (pointer-typed signals —
    /// records, tuples) and must go through the `emit_signal_store` /
    /// `signal_addr` path.
    pub(crate) fn signal_in_struct(&self, comp_idx: usize, sig_idx: usize) -> bool {
        self.gc_layouts
            .get(comp_idx)
            .and_then(|l| l.signal_field_paths.get(sig_idx))
            .map(|p| !p.is_empty())
            .unwrap_or(false)
    }

    /// Phase 3 GC migration: true iff `ty` is a "single-level record"
    /// (SLR) — a record whose every declared field is either a
    /// primitive (the Phase-2 POR set), a `string`, or a `list<scalar>`
    /// (where `<scalar>` is a primitive or enum). Nested records,
    /// tuples, options/results with payload, and variants are NOT
    /// allowed; those stay on the legacy memory path until Phase 4+.
    ///
    /// SLR is a strict superset of POR: every POR record is SLR, but
    /// a record like `User { name: string, age: u32 }` is SLR while
    /// not POR. Strings and scalar lists are stored as
    /// `(ref null $fat_value)` boxes inside the record's GC struct;
    /// Field reads unbox via `struct.get $fat_value $ptr/$len`,
    /// RecordConstruct boxes via `struct.new $fat_value`.
    pub(crate) fn is_single_level_record(&self, ty: Ty) -> bool {
        // Phase 4: extended to "deeply-typed records" (DTR) — records
        // whose fields are primitives, strings, list<scalar>, OR nested
        // records that are themselves DTR. Tuples and option/result/
        // variant fields still disqualify the record (those keep the
        // legacy memory path).
        //
        // The name `is_single_level_record` is retained for callsite
        // continuity; semantically it's now `is_dtr` (deeply-typed
        // record). Renaming is deferred to a later cleanup.
        let mut seen = std::collections::HashSet::new();
        self.is_deeply_typed_record_inner(ty, &mut seen)
    }

    fn is_deeply_typed_record_inner(
        &self,
        ty: Ty,
        seen: &mut std::collections::HashSet<yel_core::DefId>,
    ) -> bool {
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        let def_id = match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) => *d,
            _ => return false,
        };
        let record = match self.ctx.defs.kind(def_id) {
            DefKind::Record(r) => r.clone(),
            _ => return false,
        };
        // Cycle guard: if we re-enter the same record def, optimistically
        // accept (the surrounding caller is what determines DTR-ness).
        // In practice, Yel records can't be value-cyclic, so this is a
        // belt-and-braces guard.
        if !seen.insert(def_id) {
            return true;
        }
        let result = (|| {
            for &field_def_id in &record.fields {
                let field_ty = match self.ctx.defs.kind(field_def_id) {
                    DefKind::Field(f) => f.ty,
                    _ => return false,
                };
                if !self.is_dtr_field_ty(field_ty, seen) {
                    return false;
                }
            }
            true
        })();
        seen.remove(&def_id);
        result
    }

    /// Helper for the DTR check: a field type is allowed if it is a
    /// primitive, an enum, a `string`, a `list<scalar>`, or a nested
    /// record that is itself DTR. Tuples and option/result/variant
    /// fields are NOT allowed in Phase 4 — those keep the legacy memory
    /// path.
    fn is_dtr_field_ty(
        &self,
        ty: Ty,
        seen: &mut std::collections::HashSet<yel_core::DefId>,
    ) -> bool {
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        if self.is_primitive_field_ty(ty) {
            return true;
        }
        match self.ctx.ty_kind(ty) {
            InternedTyKind::String => true,
            // Phase 5e.6: extend DTR to any GC-array-eligible list, so
            // `record { …, list<string>, list<record>, … }` records
            // can use a typed `(ref null $<rec>)` field on the
            // component struct instead of two i32 slots.
            InternedTyKind::List(_) => self.is_scalar_list_ty(ty),
            InternedTyKind::Adt(def_id) => match self.ctx.defs.kind(*def_id) {
                DefKind::Record(_) => self.is_deeply_typed_record_inner(ty, seen),
                _ => false,
            },
            _ => false,
        }
    }

    /// Phase 5b-v.3 / 5d preview: for `option<T>` where `T`'s internal
    /// repr is itself a GC ref (GcRef / GcArrayRef), the option
    /// "collapses" to a single nullable ref — `none` is the null ref,
    /// `some(value)` is the non-null ref. No discriminant slot is
    /// emitted internally. Returns the collapsed ref's heap type idx
    /// when the optimisation applies.
    ///
    /// At the WIT canonical-ABI boundary, the discriminant is
    /// synthesised by null-checking the ref (getter) or read from the
    /// caller and used to pick null vs the inner ref (setter).
    pub(crate) fn option_collapses_to_ref(&self, ty: Ty) -> Option<u32> {
        use yel_core::types::InternedTyKind;
        let inner = match self.ctx.ty_kind(ty) {
            InternedTyKind::Option(inner) => *inner,
            _ => return None,
        };
        match self.internal_repr(inner) {
            InternalRepr::GcRef(idx) | InternalRepr::GcArrayRef(idx) => Some(idx),
            _ => None,
        }
    }

    /// Phase 5b-v.3 / 5e.1: true iff `ty` is `list<T>` where `T` migrates
    /// to the typed-GC-array path. Includes primitive scalars and DTR
    /// records (records whose internal repr is a single GcRef).
    /// `list<string>`, `list<list<*>>`, `list<tuple>`, `list<variant>`,
    /// `list<option>` are NOT yet on this path and stay on inline-byte
    /// memory until later sub-phases (5e.2+).
    pub(crate) fn is_scalar_list_ty(&self, ty: Ty) -> bool {
        use yel_core::types::InternedTyKind;
        let elem = match self.ctx.ty_kind(ty) {
            InternedTyKind::List(e) => *e,
            _ => return false,
        };
        if self.is_primitive_field_ty(elem) {
            return true;
        }
        // Phase 5e.1: DTR records have a GcRef internal repr, so an
        // array of them is naturally `(array (ref null $<rec>))`.
        if self.is_single_level_record(elem) {
            return true;
        }
        // Phase 5e.2: nested lists where the inner list is itself
        // GC-eligible — element type is `(ref null $<inner_arr>)`.
        if matches!(self.ctx.ty_kind(elem), InternedTyKind::List(_))
            && self.is_scalar_list_ty(elem)
        {
            return true;
        }
        // Phase 5e.4: strings — element is `(ref null $fat_value)`.
        if matches!(self.ctx.ty_kind(elem), InternedTyKind::String) {
            return true;
        }
        // Phase 5e.5: option<scalar-i32-fits> — reuse $fat_value as the
        // 2-i32 box (disc + payload). Skips option<s64/u64/f64>.
        if let yel_core::types::InternedTyKind::Option(_) = self.ctx.ty_kind(elem) {
            if self.option_collapses_to_ref(elem).is_none() {
                let canonical = self.canonical_flat_valtypes(elem);
                if canonical.iter().all(|vt| matches!(vt, wasm_encoder::ValType::I32))
                    && canonical.len() == 2
                {
                    return true;
                }
            }
        }
        false
    }

    /// Phase 2 GC migration: true iff `ty` is a record whose every
    /// declared field type is a primitive (s8/u8/s16/u16/s32/u32/s64/
    /// u64/bool/char/f32/f64/enum, plus the unit-style scalar types
    /// that collapse to a single i32 in `signal_storage_valtypes`).
    ///
    /// "Primitive-only records" (POR) are the safest first step of the
    /// records-to-GC migration: they have no nested allocation, so
    /// migrating their storage to a `(struct ...)` GC type doesn't
    /// require migrating strings, lists, options/results, variants,
    /// nested records, or tuples in the same step. Records that mix in
    /// any non-primitive field stay on the legacy memory path.
    pub(crate) fn is_primitive_only_record(&self, ty: Ty) -> bool {
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        let def_id = match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) => *d,
            _ => return false,
        };
        let record = match self.ctx.defs.kind(def_id) {
            DefKind::Record(r) => r.clone(),
            _ => return false,
        };
        for &field_def_id in &record.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                DefKind::Field(f) => f.ty,
                _ => return false,
            };
            if !self.is_primitive_field_ty(field_ty) {
                return false;
            }
        }
        true
    }

    /// Helper for `is_primitive_only_record`: true iff `ty` is a
    /// "primitive" in the Phase 2 sense — collapses to a single
    /// scalar slot with no heap allocation.
    fn is_primitive_field_ty(&self, ty: Ty) -> bool {
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Bool
            | InternedTyKind::S8
            | InternedTyKind::S16
            | InternedTyKind::S32
            | InternedTyKind::S64
            | InternedTyKind::U8
            | InternedTyKind::U16
            | InternedTyKind::U32
            | InternedTyKind::U64
            | InternedTyKind::F32
            | InternedTyKind::F64
            | InternedTyKind::Char => true,
            InternedTyKind::Adt(d) => matches!(self.ctx.defs.kind(*d), DefKind::Enum(_)),
            _ => false,
        }
    }

    /// Phase 2 gate: true iff this signal is a primitive-only record
    /// stored as a `(ref null $<rec>_record)` field on the component
    /// struct. Distinct from `signal_in_struct` for migration
    /// debugging — `signal_in_struct` covers all migrated signals
    /// (scalars, fat-ptrs, flat composites, and now POR records);
    /// `signal_is_por_gc` is the narrower Phase-2-specific predicate
    /// that callers use to decide between the new struct.new/get path
    /// and the legacy memory path for Field/RecordConstruct emission.
    #[allow(dead_code)]
    pub(crate) fn signal_is_por_gc(&self, comp_idx: usize, sig_idx: usize) -> bool {
        let component = match self.components.get(comp_idx) {
            Some(c) => c,
            None => return false,
        };
        let sig = match component.signals.get(sig_idx) {
            Some(s) => s,
            None => return false,
        };
        // Phase 3: SLR (string / list<scalar> fields) also routes
        // through the GC path. POR remains a strict subset of SLR, so
        // checking SLR here covers both.
        self.is_single_level_record(sig.ty)
    }

    /// Phase 2: GC type index for a primitive-only record `ty`. Returns
    /// `None` for non-record types, non-POR records, or if the
    /// registry doesn't know the record (shouldn't happen —
    /// `emit_program_record_types` covers every user record). Gating
    /// is intentional: only POR records are migrated in Phase 2;
    /// records with strings/lists/nested types must keep `internal_repr
    /// == Pointer` so the rest of the pipeline routes them through the
    /// legacy memory path.
    pub(crate) fn por_record_type_idx(&self, ty: Ty) -> Option<u32> {
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        // Phase 3: include single-level records (records with string /
        // list<scalar> fields). POR remains the primitive-only subset;
        // SLR is the broader gate for "records on the GC path".
        if !self.is_single_level_record(ty) {
            return None;
        }
        let def_id = match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) => *d,
            _ => return None,
        };
        if !matches!(self.ctx.defs.kind(def_id), DefKind::Record(_)) {
            return None;
        }
        self.record_gc_types.record_type_idx.get(&def_id).copied()
    }

    /// Number of WASM stack slots this type occupies in internal
    /// representation. Callers that used to do
    /// `flatten_core_valtypes(ty).len()` for internal purposes should
    /// use this instead — it returns 1 for pointer-passed
    /// records/tuples, not their field count.
    pub(super) fn internal_stack_slots(&self, ty: Ty) -> usize {
        use wasm_encoder::ValType;
        match self.internal_repr(ty) {
            InternalRepr::Zero => 0,
            InternalRepr::Scalar(_) => 1,
            InternalRepr::FatPointer => 2,
            InternalRepr::Pointer => 1,
            InternalRepr::GcRef(_) => 1,
            InternalRepr::GcArrayRef(_) => 1,
            InternalRepr::Flat => {
                // Flat composites share shape with canonical ABI.
                // Safe to delegate here — this is the one internal
                // call site `flatten_core_valtypes` is allowed (the
                // canonical shape and the internal shape ARE the same
                // for option/result/variant/enum).
                let _ = ValType::I32; // silence unused import
                self.flatten_core_valtypes(ty).len()
            }
        }
    }

    /// Return the block-result `BlockType` for a WASM `if` / nested
    /// block whose body yields a Yel value of type `ty`.
    ///
    /// Rules:
    /// - `Unit` types (no value) → `BlockType::Empty`.
    /// - Single-slot types (primitives, enums, records/tuples passed
    ///   as a single pointer) → `BlockType::Result(valtype)`.
    /// - Multi-slot composites (option/result/variant-with-payload,
    ///   strings, lists) → `BlockType::FunctionType(idx)` looking up
    ///   a pre-registered function type `() -> (slots…)` in
    ///   `self.ternary_block_types`.
    ///
    /// Returns [`CodegenError::InvalidIR`] if the type's flat shape wasn't
    /// pre-registered — the `collect_ternary_block_shapes` pre-pass should
    /// register every ternary/if found in the component's LIR; a missing entry
    /// usually means a new multi-slot-producing construct without a matching
    /// pre-pass entry.
    pub(super) fn block_ty_for(&self, ty: Ty) -> Result<BlockType, CodegenError> {
        match self.internal_repr(ty) {
            InternalRepr::Zero => Ok(BlockType::Empty),
            InternalRepr::Scalar(vt) => Ok(BlockType::Result(vt)),
            InternalRepr::Pointer => Ok(BlockType::Result(ValType::I32)),
            InternalRepr::GcRef(type_idx) => Ok(BlockType::Result(ValType::Ref(
                wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(type_idx),
                },
            ))),
            InternalRepr::GcArrayRef(arr_idx) => Ok(BlockType::Result(ValType::Ref(
                wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(arr_idx),
                },
            ))),
            InternalRepr::FatPointer => {
                // 2-slot (i32, i32) — look up the registered
                // `() -> (i32, i32)` function type.
                let shape = vec![ValType::I32, ValType::I32];
                match self.ternary_block_types.get(&shape) {
                    Some(&idx) => Ok(BlockType::FunctionType(idx)),
                    None => Err(CodegenError::InvalidIR(format!(
                        "repr::block_ty_for: no pre-registered `() -> (i32, i32)` \
                         function type for fat-pointer ternary (ty={:?}). Ensure \
                         `collect_ternary_block_shapes` runs before code emission.",
                        ty,
                    ))),
                }
            }
            InternalRepr::Flat => {
                // Single-slot flat composites (enum, etc.) → direct
                // Result; multi-slot → registered function type.
                let shape = self.flatten_core_valtypes(ty);
                match shape.len() {
                    0 => Ok(BlockType::Empty),
                    1 => Ok(BlockType::Result(shape[0])),
                    _ => match self.ternary_block_types.get(&shape) {
                        Some(&idx) => Ok(BlockType::FunctionType(idx)),
                        None => Err(CodegenError::InvalidIR(format!(
                            "repr::block_ty_for: no pre-registered function type for \
                             multi-slot shape {:?} (ty={:?}). The type section pass \
                             (`collect_ternary_block_shapes`) must visit every \
                             LirExprKind whose evaluation yields a multi-slot value \
                             and intern a matching `() -> shape` function type.",
                            shape, ty,
                        ))),
                    },
                }
            }
        }
    }
}

/// Walk every expression in every component's `exprs` table and
/// collect the flat shapes that ternary / multi-branch `if`
/// expressions produce. Called once during the Type section build so
/// the Type section can intern `() -> (slots…)` function types for
/// every shape observed.
///
/// Single-slot and Unit shapes are skipped — they use
/// `BlockType::Result(_)` / `BlockType::Empty` and don't need a
/// function type.
pub(super) fn collect_ternary_block_shapes(
    builder: &mut WasmPackageBuilder<'_>,
    into: &mut HashMap<Vec<ValType>, ()>,
) {
    use yel_core::types::InternedTyKind;
    // Components own their own expression tables; walk each.
    let expr_tables: Vec<Vec<LirExpr>> = builder
        .components
        .iter()
        .map(|c| c.exprs.clone())
        .collect();
    for exprs in &expr_tables {
        for e in exprs {
            visit_expr(builder, e, into);
        }
    }

    fn visit_expr(
        builder: &mut WasmPackageBuilder<'_>,
        e: &LirExpr,
        into: &mut HashMap<Vec<ValType>, ()>,
    ) {
        // Only Ternary produces a value whose exact stack shape the
        // WASM `if` block type must declare. Every other composite-
        // producing op (VariantCtor, RecordCtor, list ctors, etc.)
        // pushes values without wrapping them in a block — those don't
        // need a block type.
        if let LirExprKind::Ternary { condition, then_expr, else_expr } = &e.kind {
            // Unit ternaries (both branches are expressions with no
            // runtime value) use BlockType::Empty, no registration
            // needed.
            if !matches!(builder.ctx.ty_kind(e.ty), InternedTyKind::Unit) {
                let shape = builder.flatten_core_valtypes(e.ty);
                if shape.len() >= 2 {
                    into.insert(shape, ());
                }
            }
            visit_expr(builder, condition, into);
            visit_expr(builder, then_expr, into);
            visit_expr(builder, else_expr, into);
            return;
        }

        // Recurse into subexpressions. New expression kinds must be
        // added here, but forgetting to doesn't produce a silent bug —
        // a missed ternary would instead trigger `block_ty_for`'s
        // error at emit time with a clear "no pre-registered type
        // for shape X" message, pointing at exactly this function.
        match &e.kind {
            LirExprKind::Binary { lhs, rhs, .. } => {
                visit_expr(builder, lhs, into);
                visit_expr(builder, rhs, into);
            }
            LirExprKind::Unary { operand, .. } => visit_expr(builder, operand, into),
            LirExprKind::Field { base, .. } => visit_expr(builder, base, into),
            LirExprKind::Index { base, index } => {
                visit_expr(builder, base, into);
                visit_expr(builder, index, into);
            }
            LirExprKind::Call { args, .. } | LirExprKind::GlobalCall { args, .. } => {
                for a in args {
                    visit_expr(builder, a, into);
                }
            }
            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                visit_expr(builder, condition, into);
                visit_expr(builder, then_expr, into);
                visit_expr(builder, else_expr, into);
            }
            LirExprKind::VariantCtor { payload: Some(p), .. } => visit_expr(builder, p, into),
            LirExprKind::ListConstruct { elements, .. } => {
                for el in elements {
                    visit_expr(builder, el, into);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for f in fields {
                    visit_expr(builder, f, into);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for el in elements {
                    visit_expr(builder, el, into);
                }
            }
            LirExprKind::Range { start, end, .. } => {
                visit_expr(builder, start, into);
                visit_expr(builder, end, into);
            }
            _ => {}
        }
    }
}
