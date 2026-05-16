//! Wasm function-type construction from LIR `CallingConv` data.
//!
//! Centralises the "what wasm signature does this function have?"
//! decision. Used by:
//!
//! 1. **Block function type registration** in `build.rs` — was inline
//!    until L3-v2 Phase 2 factored it here.
//! 2. **Flow function type registration** (future) — flow's
//!    `LirFunction::FreeFunction` entries register through the same
//!    helper, getting non-i32 returns / no implicit self-ref correctly.
//!
//! The helper takes a [`CallingConv`] + the user-declared param slot
//! list + a slot-info table and produces the wasm `(params, returns)`
//! ValType vectors. All slot-val_ty → wasm-ValType resolution is in
//! [`WasmPackageBuilder::slot_wasm_valtype`].

use wasm_encoder::{TypeSection, ValType};
use yel_core::lir::block::{LirSlotId, LirSlotInfo, LirSlotKind, LirSlotValType};
use yel_core::lir::function::{CallingConv, ImplicitParam};
use yel_core::types::InternedTyKind;

use super::super::gc_types::GcTypeLayout;
use super::super::{CodegenError, WasmPackageBuilder};

impl<'a> WasmPackageBuilder<'a> {
    /// Resolve one `LirSlotValType` into the corresponding wasm
    /// `ValType`. The GC-typed variants consult the per-component
    /// `GcTypeLayout` for boundary/anchor structs and the shared
    /// `record_gc_types` registry for list/record/flat-gc supertypes.
    ///
    /// Errors loudly when a GC type-idx is missing from its registry
    /// (no silent fallback, per `crates/yel-wasm-codegen/CLAUDE.md`).
    pub(crate) fn slot_wasm_valtype(
        &self,
        val_ty: LirSlotValType,
        layout: &GcTypeLayout,
    ) -> Result<ValType, CodegenError> {
        let mk_ref = |idx: u32| {
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(idx),
            })
        };
        Ok(match val_ty {
            LirSlotValType::I32 => ValType::I32,
            LirSlotValType::I64 => ValType::I64,
            LirSlotValType::F32 => ValType::F32,
            LirSlotValType::F64 => ValType::F64,
            LirSlotValType::RefNull(idx) => mk_ref(idx),
            LirSlotValType::RefNullForBoundary(b_id) => {
                let ty_idx = *layout.tree_struct_type_idx.get(&b_id).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "slot wasm val type: missing tree struct type for boundary {:?}",
                        b_id
                    ))
                })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForComponent(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                let ty_idx = self.gc_layouts[j]
                    .component_struct_type_idx
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "slot wasm val type: missing component_struct_type_idx for {:?}",
                            def_id
                        ))
                    })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForChildrenArray(anchor_id) => {
                let ty_idx = *layout
                    .tree_for_arr_type_idx
                    .get(&anchor_id)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "slot wasm val type: missing children-array type for \
                                 anchor {:?}",
                            anchor_id
                        ))
                    })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForListGc(list_ty) => {
                let ty_idx = *self
                    .record_gc_types
                    .list_array_type_idx
                    .get(&list_ty)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "slot wasm val type: missing list_array_type_idx for {:?}",
                            list_ty
                        ))
                    })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForRecord(record_ty) => {
                let def_id = match self.ctx.ty_kind(record_ty) {
                    InternedTyKind::Adt(d) => *d,
                    _ => {
                        return Err(CodegenError::InvalidIR(format!(
                            "slot wasm val type: RefNullForRecord on non-Adt {:?}",
                            record_ty
                        )));
                    }
                };
                let ty_idx = *self
                    .record_gc_types
                    .record_type_idx
                    .get(&def_id)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "slot wasm val type: missing record_type_idx for {:?}",
                            def_id
                        ))
                    })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForFlatGc(parent_ty) => {
                let ty_idx = *self
                    .record_gc_types
                    .flat_gc_super_idx
                    .get(&parent_ty)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "slot wasm val type: missing flat_gc_super_idx for {:?}",
                            parent_ty
                        ))
                    })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForTuple(tuple_ty) => {
                let ty_idx = *self
                    .record_gc_types
                    .tuple_struct_type_idx
                    .get(&tuple_ty)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "slot wasm val type: missing tuple_struct_type_idx for {:?}",
                            tuple_ty
                        ))
                    })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForGlobalBlock(def_id) => {
                let &idx = self.global_block_def_to_idx.get(&def_id).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "slot wasm val type: no globals layout registered for \
                         RefNullForGlobalBlock({:?})",
                        def_id
                    ))
                })?;
                let ty_idx = self.globals_layouts[idx].struct_type_idx;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForSharedHandleArray => {
                let ty_idx = self.shared_handle_arr_type_idx.ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "slot wasm val type: RefNullForSharedHandleArray but shared_handle_arr_type_idx not set"
                            .into(),
                    )
                })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::RefNullForSharedHandle => {
                let ty_idx = self.shared_handle_type_idx.ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "slot wasm val type: RefNullForSharedHandle but shared_handle_type_idx not set"
                            .into(),
                    )
                })?;
                mk_ref(ty_idx)
            }
            LirSlotValType::AnyRef => ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Any,
                },
            }),
        })
    }

    /// Resolve one `ImplicitParam` into the corresponding wasm
    /// `ValType`. The current component's `GcTypeLayout` is required
    /// for typed boundary / self-ref lookups (the component's own
    /// struct lives at `layout.component_struct_type_idx`).
    pub(crate) fn implicit_param_wasm_valtype(
        &self,
        p: &ImplicitParam,
        layout: &GcTypeLayout,
    ) -> Result<ValType, CodegenError> {
        match p {
            ImplicitParam::SelfRef(_def) => {
                // The `_def` field carries the component's DefId; in
                // today's pipeline every block being type-registered is
                // already nested inside its component's pass, so the
                // matching struct type idx lives at
                // `layout.component_struct_type_idx`. We don't cross-
                // check the DefId here — it's an invariant of how
                // build.rs walks `self.components`. If/when codegen
                // gains cross-component dispatch, this lookup should
                // route through a `DefId → GcTypeLayout` map instead.
                let comp_ty_idx = layout.component_struct_type_idx.ok_or_else(|| {
                    CodegenError::InternalError(
                        "implicit SelfRef param: component_struct_type_idx not assigned".into(),
                    )
                })?;
                Ok(ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(comp_ty_idx),
                }))
            }
            ImplicitParam::Boundary(b_id) => {
                self.slot_wasm_valtype(LirSlotValType::RefNullForBoundary(*b_id), layout)
            }
            ImplicitParam::LegacyI32 => Ok(ValType::I32),
            ImplicitParam::ResourceSelf(_def) => {
                // Resource handles cross the component-model boundary
                // as opaque `i32`s. Unlike `SelfRef` we don't need the
                // resource's struct layout — the canonical ABI uses
                // raw integers for resource references.
                Ok(ValType::I32)
            }
        }
    }

    /// Build the wasm `(params, returns)` vectors from a calling
    /// convention + the user-declared parameter slot list.
    ///
    /// `slots` is the slot-info table that owns `user_param_slots`'
    /// entries (typically the component's slot table for UI blocks, or
    /// the flow function's own table). `layout` is the per-component
    /// GC layout used to resolve typed boundary / self-ref params; for
    /// flow functions with no implicit params, the layout is consulted
    /// only if any user param has a GC-typed val_ty.
    pub(crate) fn wasm_function_type_for_conv(
        &self,
        conv: &CallingConv,
        user_param_slots: &[LirSlotId],
        slots: &[LirSlotInfo],
        layout: &GcTypeLayout,
    ) -> Result<(Vec<ValType>, Vec<ValType>), CodegenError> {
        // Parameter order: [implicit_pre...] [user params...]
        // [implicit_post...]. The UI block convention places user
        // params between the leading self-ref and trailing boundary
        // refs; free / flow functions leave both implicit lists empty.
        let cap = conv.implicit_pre.len() + user_param_slots.len() + conv.implicit_post.len();
        let mut params: Vec<ValType> = Vec::with_capacity(cap);
        for p in &conv.implicit_pre {
            params.push(self.implicit_param_wasm_valtype(p, layout)?);
        }
        for ps in user_param_slots {
            let val_ty = slots
                .get(ps.legacy_u32() as usize)
                .map(|s| s.val_ty)
                .unwrap_or(LirSlotValType::I32);
            params.push(self.slot_wasm_valtype(val_ty, layout)?);
        }
        for p in &conv.implicit_post {
            params.push(self.implicit_param_wasm_valtype(p, layout)?);
        }
        let mut returns: Vec<ValType> = Vec::with_capacity(conv.returns.len());
        for r in &conv.returns {
            returns.push(self.slot_wasm_valtype(*r, layout)?);
        }
        Ok((params, returns))
    }

    /// Register a wasm function type derived from a calling convention
    /// + user param slots, appending it to `types` and returning the
    /// type index (`cursor`).
    pub(crate) fn register_wasm_function_type(
        &self,
        types: &mut TypeSection,
        cursor: &mut u32,
        conv: &CallingConv,
        user_param_slots: &[LirSlotId],
        slots: &[LirSlotInfo],
        layout: &GcTypeLayout,
    ) -> Result<u32, CodegenError> {
        let (params, returns) =
            self.wasm_function_type_for_conv(conv, user_param_slots, slots, layout)?;
        types.ty().function(params, returns);
        let idx = *cursor;
        *cursor += 1;
        Ok(idx)
    }

    /// Collect the Temp slots from `slots` in compacted `local_idx`
    /// order and produce the `Vec<(u32, ValType)>` argument
    /// `wasm_encoder::Function::new` wants. Skips the first
    /// `skip_params` Temp slots — those are already wasm-level params
    /// and don't need an extra local declaration.
    ///
    /// `WasmParam` slots are skipped entirely — they live in the
    /// function's wasm parameter locals (declared via the wasm
    /// signature), not in the `Function::new(locals)` block.
    ///
    /// Both UI's `block_fn::generate_block_function` and the non-UI
    /// `wasm::functions::emit_function` route through here so the
    /// slot-local emission order stays identical across callers.
    pub(crate) fn declare_function_locals(
        &self,
        slots: &[LirSlotInfo],
        skip_params: usize,
        layout: &GcTypeLayout,
    ) -> Result<Vec<(u32, ValType)>, CodegenError> {
        let mut temp_slots: Vec<&LirSlotInfo> = slots
            .iter()
            .filter(|s| matches!(s.kind, LirSlotKind::Temp { .. }))
            .collect();
        temp_slots.sort_by_key(|s| match s.kind {
            LirSlotKind::Temp { local_idx } => local_idx,
            _ => u32::MAX,
        });
        let mut locals: Vec<(u32, ValType)> = Vec::new();
        for s in temp_slots.iter().skip(skip_params) {
            let vt = self.slot_wasm_valtype(s.val_ty, layout)?;
            locals.push((1, vt));
        }
        Ok(locals)
    }
}
