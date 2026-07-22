//! Canonical WASM representation helpers.
//!
//! Single source of truth for "how is a Yel value represented on the
//! WASM stack / in WASM blocks". Every emit site that used to
//! independently decompose a type into flat slots must funnel through
//! this module — when a representation question has one answer, it
//! can't become inconsistent between producer and consumer.
//!
//! Non-goals: this module doesn't wrap the full push/pop/store/load
//! emission surface — emit sites query the representation and emit
//! their own instructions. See
//! `.claude/plans/uniform-pointer-passing.md` for context on why we
//! went with centralised-helpers instead of a full representation
//! refactor — the latter pays runtime cost this doesn't.
//!
//! Rule enforced by convention (not yet type-system): the only call
//! site of `crate::wasm::WasmPackageBuilder::flatten_core_valtypes`
//! outside this module should be the boundary-shim generator (WIT
//! export lifts/lowers). If you find yourself calling it in a new
//! internal emit path, add a helper here instead.
use std::collections::{HashMap, HashSet};

use wasm_encoder::{BlockType, HeapType, RefType, ValType};
use yel_core::lir::{LirExpr, LirExprKind};
use yel_core::{DefId, DefKind, InternedTyKind, Ty};

use crate::CodegenError;

use super::WasmPackageBuilder;

/// Classify the internal WASM-stack representation of a Yel type.
///
/// This is **not** the canonical ABI flattening. The internal
/// representation is what lives on the WASM stack between opcodes
/// inside the module; canonical ABI only matters at WIT boundaries.
/// Records and tuples diverge from canonical: canonical ABI flattens
/// their fields, internally they're a single typed GC ref.
///
/// Rule of thumb:
///   - primitive scalar → its matching `ValType` (1 slot)
///   - `string` → `(ref $str_bytes)` GC byte array (1 slot)
///   - `list<T>` / record / tuple → single typed GC ref (1 slot)
///   - option / result / variant → typed GcVariant ref (1 slot); an
///     `option<T>` whose payload is a single non-null-ambiguous GC ref
///     collapses to that nullable ref instead
///   - unit → zero slots
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum InternalRepr {
    /// Not valid for emit: Unit / error / unknown types that shouldn't
    /// reach internal code paths. Return zero stack slots.
    Zero,
    /// Exactly one stack slot of the given valtype.
    Scalar(wasm_encoder::ValType),
    /// A record stored as a `(ref null $<rec>_record)` GC ref (1 stack
    /// slot). Contained `u32` is the record's GC struct type index.
    GcRef(u32),
    /// A `list<scalar>` stored as a `(ref null $<elem>_list)` GC array
    /// ref (1 stack slot). Contained `u32` is the list array GC type
    /// index from `RecordGcTypes::list_array_type_idx`.
    GcArrayRef(u32),
    /// An `option<T>` / `result<T,E>` / user `variant` stored as a
    /// single `(ref null $<parent>_super)` (1 stack slot). Contained
    /// `u32` is the parent supertype's GC struct type index from
    /// `RecordGcTypes::gc_variant_super_idx`. Layout follows the W3C
    /// component-model GC ABI proposal (issue #525).
    ///
    /// Discrimination: `ref.test (ref $<parent>_<case>)` — no
    /// discriminant field. Construction: `struct.new $<parent>_<case>`
    /// (or `struct.new_default $<parent>_<none-or-case0>`).
    /// Destructure: `ref.cast (ref $<parent>_<case>); struct.get`.
    ///
    /// WIT-boundary code paths must continue to use `flatten_core_valtypes`
    /// for canonical-ABI lift/lower; never query `signal_storage_valtypes`
    /// for boundary writes.
    GcVariant(u32),
}

/// One leaf field of a (possibly nested) record, paired with how to reach it
/// and where it lands in the canonical-ABI memory layout. Produced by
/// [`WasmPackageBuilder::record_leaf_field_accesses`].
#[derive(Debug, Clone)]
pub(crate) struct LeafFieldAccess {
    /// `(record_type_idx, gc_field_idx)` `struct.get` chain from the record's
    /// root ref down to this leaf field. Callers prepend the prefix that
    /// reaches the record root; the last pair is the leaf's own field.
    pub chain: Vec<(u32, u32)>,
    /// The leaf field's type (never a nested record — those are flattened).
    #[allow(dead_code)]
    pub ty: Ty,
    /// Canonical-ABI byte offset of this field within the record.
    #[allow(dead_code)]
    pub offset: u32,
}

impl WasmPackageBuilder<'_> {
    /// Classify how a Yel type is represented on the **internal** WASM
    /// stack. Every emit site that needs to know "how many values does
    /// this type push" should query this instead of
    /// `flatten_core_valtypes` — the latter is canonical ABI
    /// (boundary-facing) and disagrees with internal representation
    /// for record/tuple (pointer-passed) values.
    pub(super) fn internal_repr(&self, ty: Ty) -> InternalRepr {
        use wasm_encoder::ValType;
        use yel_core::definitions::DefKind;
        use yel_core::types::InternedTyKind;
        if let Some(type_idx) = self.por_record_type_idx(ty) {
            return InternalRepr::GcRef(type_idx);
        }
        if let yel_core::types::InternedTyKind::Tuple(_) = self.ctx.ty_kind(ty)
            && let Some(&tup_idx) = self.record_gc_types.tuple_struct_type_idx.get(&ty) {
                return InternalRepr::GcRef(tup_idx);
            }
        if self.is_scalar_list_ty(ty)
            && let Some(&arr_idx) = self.record_gc_types.list_array_type_idx.get(&ty) {
                return InternalRepr::GcArrayRef(arr_idx);
            }
        // Option-of-ref collapse: option<T> where T has a ref repr is
        // itself just a nullable ref of T's heap type (none = null,
        // some(v) = v). No discriminant slot internally.
        if let yel_core::types::InternedTyKind::Option(inner_ty) = self.ctx.ty_kind(ty) {
            let inner_ty = *inner_ty;
            // strings-to-GC: `option<string>` does NOT collapse to a bare
            // `(ref null $str_bytes)` — a null str_bytes ref is a legitimate
            // empty string, indistinguishable from `none`. Keep it as a
            // GcVariant (`$opt_string` sub-hierarchy) so `some("")` and
            // `none` stay distinct. Other ref inners (records / lists) have
            // no such ambiguity and still collapse.
            let inner_is_gc_string = matches!(self.ctx.ty_kind(inner_ty), InternedTyKind::String);
            if !inner_is_gc_string {
                match self.internal_repr(inner_ty) {
                    InternalRepr::GcRef(idx) => return InternalRepr::GcRef(idx),
                    InternalRepr::GcArrayRef(idx) => return InternalRepr::GcArrayRef(idx),
                    _ => {}
                }
            }
        }
        // The `is_gc_variant` predicate is mirrored in
        // `yel_core::lir::block_lower::is_gc_variant_ty`; both
        // sides MUST agree per Ty.
        if self.is_gc_variant(ty)
            && let Some(&super_idx) = self.record_gc_types.gc_variant_super_idx.get(&ty) {
                return InternalRepr::GcVariant(super_idx);
            }
        // strings-to-GC (`plans/strings-to-gc.md`): a `String` is a GC byte
        // array `(ref $str_bytes)`. `$str_bytes` is always emitted, so the
        // index is present for every program that reaches here.
        if matches!(self.ctx.ty_kind(ty), InternedTyKind::String) {
            // A String's repr KIND is always `GcArrayRef($str_bytes)`. During
            // the early type-computation phase (e.g. list-constructor param
            // typing in `build_core_module`, before `emit_program_record_types`
            // populates the registry) `str_bytes_array_idx` is not yet set;
            // structural callers (`is_scalar_list_ty`) only read the kind, and
            // idx-consuming callers guard on `list_array_type_idx` (empty in
            // that same phase) so this sentinel is never emitted. `u32::MAX`
            // would fail wasm validation loudly if ever wrongly used — not a
            // silent fallback.
            let idx = self.record_gc_types.str_bytes_array_idx.unwrap_or(u32::MAX);
            return InternalRepr::GcArrayRef(idx);
        }
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Unit | InternedTyKind::Error | InternedTyKind::Unknown => {
                InternalRepr::Zero
            }
            InternedTyKind::F32 => InternalRepr::Scalar(ValType::F32),
            InternedTyKind::F64 => InternalRepr::Scalar(ValType::F64),
            InternedTyKind::S64 | InternedTyKind::U64 => InternalRepr::Scalar(ValType::I64),
            // `String` and every `list<T>` are GC refs (handled above:
            // `is_scalar_list_ty` → `GcArrayRef`). Reaching this arm would
            // mean a list whose element is not single-slot — only `unit` /
            // `func`, which are not valid list-element types — so it is
            // unreachable. (Confirmed dead by coverage across tests + fuzz.)
            InternedTyKind::List(_) => unreachable!(
                "internal_repr: list<T> hit the removed fat-pointer fallback — \
                 every list is a typed GC array; a non-single-slot element \
                 (unit/func) is not a valid list element (ty={:?})",
                ty
            ),
            // Tuple / Adt::Record cases are handled at the top of this
            // function via `tuple_struct_type_idx` / `por_record_type_idx`
            // returning the typed `GcRef`. Reaching this match arm means
            // the type registry is missing an entry — collect_program_*
            // didn't see this Ty.
            InternedTyKind::Tuple(_) => unreachable!(
                "internal_repr: tuple {:?} missing tuple_struct_type_idx",
                ty
            ),
            InternedTyKind::Adt(def_id) => match self.ctx.defs.kind(*def_id) {
                DefKind::Record(_) => {
                    unreachable!("internal_repr: record {:?} missing record_type_idx", ty)
                }
                // Enums lower to a single i32 discriminant.
                DefKind::Enum(_) => InternalRepr::Scalar(ValType::I32),
                // Variants are always GcVariant via the
                // `is_gc_variant` gate above. Reaching this arm means
                // the gate rejected the variant — the payload-admissibility
                // rules need to broaden, not a new fallback.
                _ => unreachable!(
                    "internal_repr: variant {:?} rejected by is_gc_variant gate — \
                     widen `gc_variant_payload_admissible` upstream rather than \
                     adding a Flat fallback",
                    ty
                ),
            },
            InternedTyKind::Option(_) | InternedTyKind::Result { .. } => unreachable!(
                "internal_repr: option/result Ty {:?} not registered as GcVariant — \
                 the walker in `gc_types::collect_list_and_tuple_tys` should have caught it. \
                 Add a seed source (signal / record field / variant payload / LirExpr.ty) \
                 and check the option_collapses_to_ref / DefKind admission gates upstream.",
                ty
            ),
            // Everything else (bool / narrow ints / char / length units / …)
            // is a single i32 slot.
            _ => InternalRepr::Scalar(ValType::I32),
        }
    }

    /// Per-struct-field valtypes used to back a signal of `ty` in the
    /// component's `$Comp_<i>` GC struct. Empty vec only for `Zero`
    /// (unit-typed) signals; every value-bearing signal lives in the
    /// component struct.
    /// - Scalar(vt) → `[vt]` (1 field; covers F32/F64/S64/U64/bool/
    ///   narrow ints widened to i32 — fields are full-width, the
    ///   narrow-store/load dance is no longer needed since each field
    ///   has its own slot).
    /// - GcRef / GcArrayRef / GcVariant → 1 typed ref slot.
    /// - Zero → `[]` (no value).
    pub(crate) fn signal_storage_valtypes(&self, ty: Ty) -> Vec<ValType> {
        match self.internal_repr(ty) {
            InternalRepr::Zero => Vec::new(),
            InternalRepr::Scalar(vt) => vec![vt],
            InternalRepr::GcRef(type_idx) => vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(type_idx),
            })],
            InternalRepr::GcArrayRef(arr_idx) => vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(arr_idx),
            })],
            InternalRepr::GcVariant(super_idx) => vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(super_idx),
            })],
        }
    }

    /// True if this global-block property has a backing field on its
    /// block's `$globals_<i>` GC struct (callers use struct.get /
    /// struct.set via the per-block self-global). False for `Zero`
    /// (unit-typed) properties or DefIds that aren't global properties.
    pub(crate) fn global_in_struct(&self, prop_def_id: DefId) -> bool {
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

    /// True if this component-local signal has a backing field on the
    /// `$Comp_<i>` GC struct (callers use struct.get/struct.set).
    /// False for `Zero` (unit-typed) signals or out-of-range indices.
    pub(crate) fn signal_in_struct(&self, comp_idx: usize, sig_idx: usize) -> bool {
        // Source of truth lives on `LirResource.signal_layout`; the
        // mirror on `GcTypeLayout::signal_field_paths` is cross-checked
        // by `debug_assert_eq!` in `emit_component_struct_type`.
        self.components
            .get(comp_idx)
            .map(|c| c.signal_layout.signal_in_struct(sig_idx))
            .unwrap_or(false)
    }

    /// True iff `ty` is an ADT whose definition is a record. Every
    /// record now has a typed GC slot per field, so this collapses to a
    /// single `DefKind::Record` check; kept as a named function so call
    /// sites read clearly.
    pub(crate) fn is_single_level_record(&self, ty: Ty) -> bool {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) => matches!(self.ctx.defs.kind(*d), DefKind::Record(_)),
            _ => false,
        }
    }

    /// Structural mirror of
    /// `yel_core::lir::block_lower::is_gc_variant_ty`. Both
    /// sides MUST agree per Ty so that LIR slot allocation and WASM
    /// codegen pick the same shape (1 ref slot vs N flat slots).
    pub(crate) fn is_gc_variant(&self, ty: Ty) -> bool {
        let mut visiting = HashSet::new();
        self.is_gc_variant_inner(ty, &mut visiting)
    }

    fn is_gc_variant_inner(&self, ty: Ty, visiting: &mut HashSet<DefId>) -> bool {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Option(inner) => {
                let inner = *inner;
                // option<inner> ref-collapses to a single nullable ref
                // when inner already has a single-GC-ref internal repr —
                // scalar lists (typed array), tuples (struct), records
                // (struct). In those cases `option_collapses_to_ref`
                // wins inside `internal_repr` and GcVariant would be
                // redundant; reject here so we don't also register an
                // unused rec group.
                //
                // The yel-core mirror in
                // `block_lower::is_gc_variant_ty_inner` rejects on
                // the same structural shape — keep them in sync.
                if self.is_scalar_list_ty(inner) {
                    return false;
                }
                match self.ctx.ty_kind(inner) {
                    InternedTyKind::Tuple(_) => return false,
                    InternedTyKind::Adt(d)
                        if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) =>
                    {
                        return false;
                    }
                    _ => {}
                }
                self.gc_variant_payload_admissible(inner, visiting)
            }
            InternedTyKind::Result { ok, err } => {
                let ok_ok = match ok {
                    Some(t) => self.gc_variant_payload_admissible(*t, visiting),
                    None => true,
                };
                let err_ok = match err {
                    Some(t) => self.gc_variant_payload_admissible(*t, visiting),
                    None => true,
                };
                ok_ok && err_ok
            }
            InternedTyKind::Adt(def_id) => {
                let def_id = *def_id;
                let cases = match self.ctx.defs.as_variant(def_id) {
                    Some(v) => v.cases.clone(),
                    None => return false,
                };
                if !visiting.insert(def_id) {
                    return true;
                }
                let result = cases.iter().all(|&c| {
                    if let DefKind::VariantCase(case) = self.ctx.defs.kind(c) {
                        match case.payload {
                            None => true,
                            Some(p) => self.gc_variant_payload_admissible(p, visiting),
                        }
                    } else {
                        false
                    }
                });
                visiting.remove(&def_id);
                result
            }
            _ => false,
        }
    }

    fn gc_variant_payload_admissible(&self, ty: Ty, visiting: &mut HashSet<DefId>) -> bool {
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
            | InternedTyKind::Char
            | InternedTyKind::String => true,
            InternedTyKind::List(_) => self.is_scalar_list_ty(ty),
            InternedTyKind::Tuple(_) => {
                self.record_gc_types.tuple_struct_type_idx.contains_key(&ty)
            }
            InternedTyKind::Adt(d) => match self.ctx.defs.kind(*d) {
                DefKind::Enum(_) => true,
                DefKind::Record(_) => self.is_single_level_record(ty),
                DefKind::Variant(_) => self.is_gc_variant_inner(ty, visiting),
                _ => false,
            },
            InternedTyKind::Option(_) | InternedTyKind::Result { .. } => {
                self.is_gc_variant_inner(ty, visiting)
            }
            _ => false,
        }
    }

    pub(crate) fn option_collapses_to_ref(&self, ty: Ty) -> Option<u32> {
        let inner = match self.ctx.ty_kind(ty) {
            InternedTyKind::Option(inner) => *inner,
            _ => return None,
        };

        // Look up registry directly rather than recursing through
        // `internal_repr`. This helper is reachable from
        // `flatten_core_valtypes`, which runs BEFORE the GC type
        // registry is populated (see `collect_ternary_block_shapes`
        // pass at the start of the type-section build). Going through
        // `internal_repr` would trip the unreachable arm for records /
        // tuples whose registry entries aren't filled in yet.
        if let Some(&arr_idx) = self.record_gc_types.list_array_type_idx.get(&inner) {
            return Some(arr_idx);
        }

        // strings-to-GC: `option<string>` deliberately does NOT collapse
        // (a null str_bytes ref would be ambiguous with `none`); it stays a
        // GcVariant. See `internal_repr`'s Option arm.

        match self.ctx.ty_kind(inner) {
            InternedTyKind::Tuple(_) => self
                .record_gc_types
                .tuple_struct_type_idx
                .get(&inner)
                .copied(),
            InternedTyKind::Adt(d) => match self.ctx.defs.kind(*d) {
                DefKind::Record(_) => self.record_gc_types.record_type_idx.get(d).copied(),
                _ => None,
            },
            _ => None,
        }
    }

    /// True iff `ty` is `list<T>` where `T` is stored as a typed GC
    /// array (primitive scalars, records, nested GC-eligible lists,
    /// strings, GcVariant elements, and tuples).
    pub(crate) fn is_scalar_list_ty(&self, ty: Ty) -> bool {
        let elem = match self.ctx.ty_kind(ty) {
            InternedTyKind::List(e) => *e,
            _ => return false,
        };

        // A `list<T>` becomes a typed GC array `(array (mut <elem>))` iff its
        // element occupies a SINGLE wasm slot — i.e. `internal_repr(elem)` is
        // `Scalar` / `GcRef` / `GcArrayRef` / `GcVariant`. This subsumes
        // every single-ref shape at once: primitives, enums, strings,
        // records, tuples, option-of-ref collapse (`option<record>` etc.),
        // gc-variant (option/result/variant), and nested scalar lists.
        //
        // The only elements that are NOT single-slot are `FatPointer` (a
        // non-scalar nested list — the recursion terminates when its own
        // element is not single-slot) and `Zero` (a unit element). Those keep
        // the list on the fat-pointer fallback path.
        matches!(
            self.internal_repr(elem),
            InternalRepr::Scalar(_)
                | InternalRepr::GcRef(_)
                | InternalRepr::GcArrayRef(_)
                | InternalRepr::GcVariant(_)
        )
    }

    /// True iff `ty` is a record whose every declared field type is a
    /// primitive scalar (no nested allocation).
    pub(crate) fn is_primitive_only_record(&self, ty: Ty) -> bool {
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

    /// True iff `ty` collapses to a single scalar slot with no heap
    /// allocation.
    fn is_primitive_field_ty(&self, ty: Ty) -> bool {
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

    /// GC type index for a record `ty`. Returns `None` for non-record
    /// types or if the registry doesn't know the record (shouldn't
    /// happen — `emit_program_record_types` covers every user record).
    /// Every record now has a registered `record_type_idx`; field types
    /// that don't have a typed slot fall through to
    /// `record_field_storage_type`'s anyref fallback. The original POR
    /// vs. SLR vs. DTR distinction is historical — registry membership
    /// alone decides the GC path now.
    pub(crate) fn por_record_type_idx(&self, ty: Ty) -> Option<u32> {
        let def_id = match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) => *d,
            _ => return None,
        };
        if !matches!(self.ctx.defs.kind(def_id), DefKind::Record(_)) {
            return None;
        }
        self.record_gc_types.record_type_idx.get(&def_id).copied()
    }

    /// Flatten a record into its **leaf fields** — every field that is not
    /// itself a nested record (scalar / string / list / tuple / option /
    /// result / variant). Each leaf is returned with the `struct.get` chain
    /// that reaches it from the record's root ref and its canonical-ABI byte
    /// offset within the record. Nested-record fields are walked transparently
    /// (their leaves inherit the extended chain + accumulated offset).
    ///
    /// This is the single source of truth for "canonical slot → field-access
    /// chain": both the by-value single-slot record getter and the multi-slot
    /// record lift consume it, so the nested-record traversal lives in exactly
    /// one place. Chains are `(record_type_idx, gc_field_idx)` pairs, relative
    /// to the record root (callers prepend the prefix that reaches the record).
    pub(crate) fn record_leaf_field_accesses(
        &mut self,
        record_def_id: DefId,
    ) -> Result<Vec<LeafFieldAccess>, CodegenError> {
        let mut out = Vec::new();
        self.collect_record_leaf_accesses(record_def_id, 0, &[], &mut out)?;
        Ok(out)
    }

    fn collect_record_leaf_accesses(
        &mut self,
        record_def_id: DefId,
        base_offset: u32,
        prefix: &[(u32, u32)],
        out: &mut Vec<LeafFieldAccess>,
    ) -> Result<(), CodegenError> {
        let type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("record leaf accesses: missing record_type_idx".into())
            })?;
        let fields: Vec<DefId> = self
            .ctx
            .defs
            .as_record(record_def_id)
            .ok_or_else(|| CodegenError::InvalidIR("record leaf accesses: not a record".into()))?
            .fields
            .clone();
        let field_gc = self
            .record_gc_types
            .field_gc_indices
            .get(&record_def_id)
            .cloned()
            .ok_or_else(|| {
                CodegenError::InvalidIR("record leaf accesses: missing field gc indices".into())
            })?;
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR("record leaf accesses: missing record_layout".into())
            })?
            .clone();
        for (i, &fid) in fields.iter().enumerate() {
            let fty = match self.ctx.defs.kind(fid) {
                DefKind::Field(f) => f.ty,
                _ => continue,
            };
            let (_name, field_offset, _ty) = layout.field_offsets.get(i).cloned().ok_or_else(|| {
                CodegenError::InvalidIR(format!("record leaf accesses: field offset missing for {}", i))
            })?;
            let gc_idx = *field_gc.get(i).ok_or_else(|| {
                CodegenError::InvalidIR("record leaf accesses: gc field index out of range".into())
            })?;
            let mut chain = prefix.to_vec();
            chain.push((type_idx, gc_idx));
            let abs_off = base_offset + field_offset;
            match self.ctx.ty_kind(fty) {
                InternedTyKind::Adt(d)
                    if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) =>
                {
                    // Nested record: recurse, extending the chain + offset.
                    self.collect_record_leaf_accesses(*d, abs_off, &chain, out)?;
                }
                _ => out.push(LeafFieldAccess {
                    chain,
                    ty: fty,
                    offset: abs_off,
                }),
            }
        }
        Ok(())
    }

    /// Number of WASM stack slots this type occupies in internal
    /// representation. Callers that used to do
    /// `flatten_core_valtypes(ty).len()` for internal purposes should
    /// use this instead — it returns 1 for typed-ref records/tuples,
    /// not their field count.
    pub(super) fn internal_stack_slots(&self, ty: Ty) -> usize {
        match self.internal_repr(ty) {
            InternalRepr::Zero => 0,
            InternalRepr::Scalar(_) => 1,
            InternalRepr::GcRef(_) => 1,
            InternalRepr::GcArrayRef(_) => 1,
            InternalRepr::GcVariant(_) => 1,
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
            InternalRepr::GcRef(type_idx) => {
                Ok(BlockType::Result(ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(type_idx),
                })))
            }
            InternalRepr::GcArrayRef(arr_idx) => {
                Ok(BlockType::Result(ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(arr_idx),
                })))
            }
            InternalRepr::GcVariant(super_idx) => {
                Ok(BlockType::Result(ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(super_idx),
                })))
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
    // Components own their own expression tables; walk each.
    let expr_tables: Vec<Vec<LirExpr>> =
        builder.components.iter().map(|c| c.exprs.clone()).collect();

    for exprs in &expr_tables {
        for e in exprs {
            visit_expr(builder, e, exprs, into);
        }
    }

    fn visit_expr(
        builder: &mut WasmPackageBuilder<'_>,
        e: &LirExpr,
        exprs: &[LirExpr],
        into: &mut HashMap<Vec<ValType>, ()>,
    ) {
        // Only Ternary produces a value whose exact stack shape the
        // WASM `if` block type must declare. Every other composite-
        // producing op (VariantCtor, RecordCtor, list ctors, etc.)
        // pushes values without wrapping them in a block — those don't
        // need a block type.
        if let LirExprKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } = &e.kind
        {
            // Unit ternaries (both branches are expressions with no
            // runtime value) use BlockType::Empty, no registration
            // needed.
            if !matches!(builder.ctx.ty_kind(e.ty), InternedTyKind::Unit) {
                let shape = builder.flatten_core_valtypes(e.ty);
                if shape.len() >= 2 {
                    into.insert(shape, ());
                }
            }
            visit_expr(builder, &exprs[condition.0 as usize], exprs, into);
            visit_expr(builder, &exprs[then_expr.0 as usize], exprs, into);
            visit_expr(builder, &exprs[else_expr.0 as usize], exprs, into);
            return;
        }

        // Recurse into subexpressions. New expression kinds must be
        // added here, but forgetting to doesn't produce a silent bug —
        // a missed ternary would instead trigger `block_ty_for`'s
        // error at emit time with a clear "no pre-registered type
        // for shape X" message, pointing at exactly this function.
        match &e.kind {
            LirExprKind::Binary { lhs, rhs, .. } => {
                visit_expr(builder, &exprs[lhs.0 as usize], exprs, into);
                visit_expr(builder, &exprs[rhs.0 as usize], exprs, into);
            }
            LirExprKind::Unary { operand, .. } => {
                visit_expr(builder, &exprs[operand.0 as usize], exprs, into)
            }
            LirExprKind::Field { base, .. } => {
                visit_expr(builder, &exprs[base.0 as usize], exprs, into)
            }
            LirExprKind::Index { base, index } => {
                visit_expr(builder, &exprs[base.0 as usize], exprs, into);
                visit_expr(builder, &exprs[index.0 as usize], exprs, into);
            }
            LirExprKind::Call { args, .. } => {
                for a in args {
                    visit_expr(builder, &exprs[a.0 as usize], exprs, into);
                }
            }
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                visit_expr(builder, &exprs[condition.0 as usize], exprs, into);
                visit_expr(builder, &exprs[then_expr.0 as usize], exprs, into);
                visit_expr(builder, &exprs[else_expr.0 as usize], exprs, into);
            }
            LirExprKind::VariantCtor {
                payload: Some(p), ..
            } => visit_expr(builder, &exprs[p.0 as usize], exprs, into),
            LirExprKind::ListConstruct { elements, .. } => {
                for el in elements {
                    visit_expr(builder, &exprs[el.0 as usize], exprs, into);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for f in fields {
                    visit_expr(builder, &exprs[f.0 as usize], exprs, into);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for el in elements {
                    visit_expr(builder, &exprs[el.0 as usize], exprs, into);
                }
            }
            LirExprKind::Range { start, end, .. } => {
                visit_expr(builder, &exprs[start.0 as usize], exprs, into);
                visit_expr(builder, &exprs[end.0 as usize], exprs, into);
            }
            _ => {}
        }
    }
}
