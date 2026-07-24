//! Standalone dispatch function emission + value-coercion / input-binding
//! helpers used by dispatch when threading event payloads back to setters.

use rustc_hash::FxHashMap as HashMap;

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::Ty;
use yel_core::ids::BlockId;

use super::super::CodegenError;
use super::super::{MemoryLayout, WasmPackageBuilder};
use super::constants::{HANDLER_ID_HANDLE_SHIFT, HANDLER_ID_LOCAL_MASK};
use super::scratch::{i32_narrow_store_for, mem_arg};

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn generate_dispatch(
        &mut self,
        layouts: &[MemoryLayout],
    ) -> Result<Function, CodegenError> {
        // Param 0: encoded handler_id `(handle << 16) | local_id` —
        // upper 16 bits identify the host resource handle (registry
        // index), lower 16 bits the per-component AddEventListener
        // ordinal. Dispatch decodes both, looks up the typed self ref
        // through the owning component's registry, and tail-calls the
        // matching handler block with `(ref Comp, parent=0, ...)`.
        //
        // Param 1: event-value discriminant (see arm map below).
        // Param 2/3: payload joined slots.
        //   event_disc arms (1:1 with WIT `event-value` declaration order):
        //     0 = none                (no-op preamble)
        //     1 = input-text(string)  — slot0=ptr zext, slot1=len
        //     2 = input-f64(f64)      — slot0 holds f64 bit pattern
        //     3 = input-f32(f32)      — slot0 low32 holds f32 bit pattern
        //     4 = input-s32(s32)      — slot0 low32 holds the s32
        //     5 = input-bool(bool)    — slot0 low32 holds 0/1
        //
        // For binding-setter handlers, the preamble parses the arm,
        // writes the target signal, and triggers its effects — THEN the
        // user body runs.
        const PARAM_HANDLER_ID: u32 = 0;
        const PARAM_EVENT_DISC: u32 = 1;
        const PARAM_SLOT0_I64: u32 = 2;
        const PARAM_SLOT1_LEN: u32 = 3;

        // Clone handlers up-front: the body emission below mutates
        // `self.current_self_local` around each branch so emit_self_ref
        // (binding-setter struct.set, trigger fan-out) sees the
        // registry-resolved ref. Iterating an immutable borrow of
        // `self.global_handler_map` while doing that would conflict.
        let handlers: Vec<(u32, usize, BlockId)> = self.global_handler_map.clone();

        if handlers.is_empty() {
            let mut func = Function::new([]);
            func.instruction(&Instruction::End);
            return Ok(func);
        }

        // Reserve scratch locals: one i32 for the decoded `handle`
        // (registry index) plus one typed `(ref null $Comp_<i>)` per
        // distinct owner component appearing in `handlers`. The typed
        // locals are used by `emit_registry_lookup` to stash the
        // resolved self ref before the call.
        let mut owner_comps: Vec<usize> = handlers
            .iter()
            .map(|(_, ci, _)| *ci)
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        owner_comps.sort();

        // Local layout: WASM params 0..3 (4 of them), then locals start
        // at 4: [handle_local: i32], then one ref local per owner comp.
        let handle_local: u32 = 4;
        let mut owner_self_local: HashMap<usize, u32> = HashMap::default();
        let mut local_decls: Vec<(u32, ValType)> = vec![(1, ValType::I32)];
        for (next_local, &ci) in (5_u32..).zip(owner_comps.iter()) {
            let struct_ty = self.gc_layouts[ci]
                .component_struct_type_idx
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "dispatch: component {} has no struct type — handler resolution \
                         requires registry-lookup which produces a typed ref",
                        ci
                    ))
                })?;
            local_decls.push((
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                }),
            ));
            owner_self_local.insert(ci, next_local);
        }

        let mut func = Function::new(local_decls);

        // handle_local = encoded_handler_id >> HANDLER_ID_HANDLE_SHIFT (logical, unsigned)
        func.instruction(&Instruction::LocalGet(PARAM_HANDLER_ID));
        func.instruction(&Instruction::I32Const(HANDLER_ID_HANDLE_SHIFT));
        func.instruction(&Instruction::I32ShrU);
        func.instruction(&Instruction::LocalSet(handle_local));

        for (local_id, owner_comp_idx, block_id) in handlers.iter() {
            let component = &self.components[*owner_comp_idx];
            let input_binding_target = component.input_binding_handlers.get(block_id).copied();

            // Match on lower 16 bits of encoded handler_id — the
            // per-component AddEventListener ordinal. Upper 16 bits
            // (the handle) feed registry-lookup once we're inside the
            // matching branch.
            func.instruction(&Instruction::LocalGet(PARAM_HANDLER_ID));
            func.instruction(&Instruction::I32Const(HANDLER_ID_LOCAL_MASK));
            func.instruction(&Instruction::I32And);
            func.instruction(&Instruction::I32Const(*local_id as i32));
            func.instruction(&Instruction::I32Eq);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // Resolve `handle_local` → typed self ref via the owner
            // component's registry, into the pre-reserved typed local.
            let self_ref_local = *owner_self_local.get(owner_comp_idx).ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "dispatch: missing owner_self_local for owner component {}",
                    owner_comp_idx
                ))
            })?;
            self.emit_registry_lookup(&mut func, *owner_comp_idx, handle_local, self_ref_local)?;
            // Set `current_self_local` so `emit_self_ref` calls inside
            // the binding-setter struct.set + trigger fan-out source
            // self from the registry-resolved ref, not the singleton.
            let prev_self_local = self.current_self_local;
            let prev_self_comp_idx = self.current_self_comp_idx;
            self.current_self_local = Some(self_ref_local);
            self.current_self_comp_idx = Some(*owner_comp_idx);

            // Binding-setter preamble: extract payload, coerce to
            // target signal's type, store, trigger effects. Skipped
            // entirely for non-input dispatches (discriminant ≠ 2 for
            // input-f64, etc.) so the body still runs if the host
            // misroutes a handler — but no signal mutation leaks.
            if let Some(target_def_id) = input_binding_target {
                let _layout = &layouts[*owner_comp_idx];
                // For migrated global targets, the (target_addr, target_ty)
                // pair below is unused — we go through the per-block GC
                // struct setter directly. We still need `target_ty` to
                // pick the right f64-coercion narrowing.
                let (target_addr, target_ty) =
                    if let Some(sig_idx) = self.signal_index_in(component, target_def_id) {
                        // Every non-unit signal is GC-struct-resident, so
                        // the `target_in_comp_struct` branch below routes
                        // via struct.set and never emits `target_addr`. Only
                        // `target_ty` is live here (f64-coercion narrowing);
                        // reuse the same `-1` poison sentinel the global arm
                        // uses — a wild store if ever emitted.
                        (-1, component.signals[sig_idx].ty)
                    } else if self.ctx.defs.owning_global_block(target_def_id).is_some() {
                        let ty = self
                            .ctx
                            .defs
                            .type_of(target_def_id)
                            .unwrap_or(yel_core::types::Ty::ERROR);
                        // §1.5: globals have no memory address — the
                        // dispatch below routes around target_addr via
                        // the per-block struct.set path. -1 is a poison
                        // sentinel that would surface as a wild store
                        // if ever emitted.
                        (-1, ty)
                    } else {
                        return Err(CodegenError::InvalidIR(format!(
                            "binding-setter handler: no address for target signal {:?}",
                            target_def_id
                        )));
                    };

                // Emit: if event_disc == 2 (input-f64), extract f64
                // from slot0, coerce to target_ty, store at target_addr,
                // trigger effects. Covers every numeric target reachable
                // via `<input type="number">` — floats (identity /
                // demote) and integers (trunc).
                use yel_core::types::InternedTyKind;
                let target_kind = self.ctx.ty_kind(target_ty);
                let supported_numeric_target = matches!(
                    target_kind,
                    InternedTyKind::F32
                        | InternedTyKind::F64
                        | InternedTyKind::S8
                        | InternedTyKind::S16
                        | InternedTyKind::S32
                        | InternedTyKind::S64
                        | InternedTyKind::U8
                        | InternedTyKind::U16
                        | InternedTyKind::U32
                        | InternedTyKind::U64
                );
                if supported_numeric_target {
                    func.instruction(&Instruction::LocalGet(PARAM_EVENT_DISC));
                    func.instruction(&Instruction::I32Const(2)); // input-f64
                    func.instruction(&Instruction::I32Eq);
                    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                    // Decide whether the target signal lives in the
                    // GC struct (component or per-block global) or in
                    // linear memory. Only single-slot numeric signals
                    // reach here, so a migrated target always has a
                    // 1-element field path.
                    let target_in_comp_struct = self
                        .signal_index_in(component, target_def_id)
                        .map(|si| self.signal_in_struct(*owner_comp_idx, si))
                        .unwrap_or(false);
                    let target_in_global_struct =
                        self.ctx.defs.owning_global_block(target_def_id).is_some()
                            && self.global_in_struct(target_def_id);
                    if target_in_comp_struct {
                        let sig_idx =
                            self.signal_index_in(component, target_def_id)
                                .ok_or_else(|| {
                                    CodegenError::InternalError(
                                    "dispatch: target_in_comp_struct but signal_index_in missing"
                                        .into(),
                                )
                                })?;
                        let gc = &self.gc_layouts[*owner_comp_idx];
                        let struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                            CodegenError::InternalError(format!(
                                "dispatch: component {} missing component_struct_type_idx",
                                owner_comp_idx
                            ))
                        })?;
                        let field_idx = self.components[*owner_comp_idx]
                            .signal_layout
                            .signal_field_path(sig_idx)[0];
                        // Stack: <self_ref>, <coerced value> → struct.set
                        self.emit_self_ref(&mut func, *owner_comp_idx)?;
                        self.emit_coerce_f64_to_value(&mut func, target_ty, PARAM_SLOT0_I64)?;
                        func.instruction(&Instruction::StructSet {
                            struct_type_index: struct_ty,
                            field_index: field_idx,
                        });
                    } else if target_in_global_struct {
                        // Per-block global GC struct setter: source the
                        // self ref from the block's
                        // `(mut (ref null $globals_<i>))` global.
                        let block_id = self
                            .ctx
                            .defs
                            .owning_global_block(target_def_id)
                            .ok_or_else(|| {
                                CodegenError::InternalError(
                                    "dispatch: target_in_global_struct but owning_global_block missing"
                                        .into(),
                                )
                            })?;
                        let &layout_idx =
                            self.global_block_def_to_idx.get(&block_id).ok_or_else(|| {
                                CodegenError::InternalError(format!(
                                    "dispatch: global block {:?} has no globals layout entry",
                                    block_id
                                ))
                            })?;
                        let gl = &self.globals_layouts[layout_idx];
                        let block = self.ctx.defs.as_global(block_id).ok_or_else(|| {
                            CodegenError::InternalError(format!(
                                "dispatch: {:?} is not a GlobalDef",
                                block_id
                            ))
                        })?;
                        let prop_pos = block
                            .properties
                            .iter()
                            .position(|&p| p == target_def_id)
                            .ok_or_else(|| {
                                CodegenError::InternalError(format!(
                                    "dispatch: target {:?} not in global block {:?}",
                                    target_def_id, block_id
                                ))
                            })?;
                        let field_idx = gl.property_field_paths[prop_pos][0];
                        let core_global = gl.field_core_globals[field_idx as usize];
                        self.emit_coerce_f64_to_value(&mut func, target_ty, PARAM_SLOT0_I64)?;
                        func.instruction(&Instruction::GlobalSet(core_global));
                    } else {
                        self.emit_coerce_f64_and_store(
                            &mut func,
                            target_addr,
                            target_ty,
                            PARAM_SLOT0_I64,
                        )?;
                    }

                    // Trigger effects watching this signal.
                    self.emit_trigger_effects(&mut func, target_def_id, *owner_comp_idx)?;

                    func.instruction(&Instruction::End); // end disc==2 guard
                }
            }

            // Payload-binding handler: write the fired event's string
            // payload `(ptr, len)` into the block's scratch buffer so the
            // body's `Ptr`-mode param local reads it back via
            // `load_fat_ptr`. Generic over the event — every
            // string-carrying `event-value` variant (`input-text`, `drop`,
            // `drag-enter`, …) flattens to the same params: PARAM_SLOT0_I64
            // holds the ptr (zero-extended), PARAM_SLOT1_LEN the byte len.
            // No discriminant gate: reaching this arm means the host fired
            // the event that owns this handler, so its payload is present.
            if let Some(&offset) = component.payload_binding_handlers.get(block_id) {
                let store_fat_ptr_idx = self
                    .runtime_funcs
                    .as_ref()
                    .and_then(|r| r.store_fat_ptr)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "dispatch: payload-binding handler needs store_fat_ptr but the \
                             runtime helper was not emitted (runtime_needs scan missed it?)"
                                .to_string(),
                        )
                    })?;
                func.instruction(&Instruction::I32Const(offset));
                func.instruction(&Instruction::LocalGet(PARAM_SLOT0_I64));
                func.instruction(&Instruction::I32WrapI64);
                func.instruction(&Instruction::LocalGet(PARAM_SLOT1_LEN));
                func.instruction(&Instruction::Call(store_fat_ptr_idx));
            }

            // Run the user-authored body. Pass the registry-resolved
            // typed self ref directly. Multi-instance-correct: the
            // handle in the encoded handler_id picks the right
            // instance, dispatch routes its ref into the handler.
            if let Some(&func_idx) = self.block_func_indices.get(block_id) {
                func.instruction(&Instruction::LocalGet(self_ref_local));
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::Call(func_idx));
            }
            // Restore prev self-local context before exiting the if-arm.
            self.current_self_local = prev_self_local;
            self.current_self_comp_idx = prev_self_comp_idx;
            func.instruction(&Instruction::Return);
            func.instruction(&Instruction::End);
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Coerce a `f64` (bit-reinterpreted from the joined i64 slot) to
    /// `target_ty` and store it at `target_addr`. `trunc_sat_f64_*`
    /// matches the browser's `<input type="number">` clamping for
    /// finite inputs.
    /// Coerce the f64-bit-shaped i64 input-payload param to a value
    /// of `target_ty` and leave it on the stack. Does **not** store —
    /// callers wrap with their preferred destination (memory store
    /// for legacy signals, struct.set for migrated signals). Mirrors
    /// the coercion table of `emit_coerce_f64_and_store` but stops
    /// just before the typed store.
    pub(super) fn emit_coerce_f64_to_value(
        &self,
        func: &mut Function,
        target_ty: Ty,
        param_s1: u32,
    ) -> Result<(), CodegenError> {
        use yel_core::types::InternedTyKind;
        func.instruction(&Instruction::LocalGet(param_s1));
        func.instruction(&Instruction::F64ReinterpretI64);
        match self.ctx.ty_kind(target_ty) {
            InternedTyKind::F64 => {}
            InternedTyKind::F32 => {
                func.instruction(&Instruction::F32DemoteF64);
            }
            InternedTyKind::S64 => {
                func.instruction(&Instruction::I64TruncSatF64S);
            }
            InternedTyKind::U64 => {
                func.instruction(&Instruction::I64TruncSatF64U);
            }
            InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 => {
                func.instruction(&Instruction::I32TruncSatF64S);
            }
            InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 => {
                func.instruction(&Instruction::I32TruncSatF64U);
            }
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "binding-setter (struct path): unsupported target type {:?} for number(f64) input payload",
                    other
                )));
            }
        }
        Ok(())
    }

    pub(super) fn emit_coerce_f64_and_store(
        &self,
        func: &mut Function,
        target_addr: i32,
        target_ty: Ty,
        param_s1: u32,
    ) -> Result<(), CodegenError> {
        use yel_core::types::InternedTyKind;
        func.instruction(&Instruction::I32Const(target_addr));
        func.instruction(&Instruction::LocalGet(param_s1));
        // Bit-reinterpret i64 -> f64. In the canonical ABI the joined
        // slot for the number arm carries the f64 value bits verbatim
        // in the low 64 bits of the i64 slot.
        func.instruction(&Instruction::F64ReinterpretI64);
        match self.ctx.ty_kind(target_ty) {
            InternedTyKind::F64 => {
                func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
            }
            InternedTyKind::F32 => {
                func.instruction(&Instruction::F32DemoteF64);
                func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
            }
            InternedTyKind::S64 => {
                func.instruction(&Instruction::I64TruncSatF64S);
                func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
            }
            InternedTyKind::U64 => {
                func.instruction(&Instruction::I64TruncSatF64U);
                func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
            }
            InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 => {
                func.instruction(&Instruction::I32TruncSatF64S);
                i32_narrow_store_for(func, self.ctx.ty_kind(target_ty));
            }
            InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 => {
                func.instruction(&Instruction::I32TruncSatF64U);
                i32_narrow_store_for(func, self.ctx.ty_kind(target_ty));
            }
            InternedTyKind::String => {
                return Err(CodegenError::InvalidIR(
                    "binding-setter: string target cannot accept number(f64) input payload".into(),
                ));
            }
            InternedTyKind::Bool => {
                return Err(CodegenError::InvalidIR(
                    "binding-setter: bool target cannot accept number(f64) input payload".into(),
                ));
            }
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "binding-setter: unsupported target type {:?} for number(f64) input payload",
                    other
                )));
            }
        }
        Ok(())
    }
}
