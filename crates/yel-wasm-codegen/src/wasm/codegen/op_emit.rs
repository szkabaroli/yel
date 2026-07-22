//! `LirOp` -> WASM instruction emission.
//!
//! The `emit_op` method is the single match-on-`LirOp` site for the
//! whole codegen pass. Each arm consumes a constructor / mount / block
//! statement and emits the corresponding WASM. Lives on
//! `WasmPackageBuilder<'a>` via an additional impl block.

use wasm_encoder::{BlockType, Function, Instruction};
use yel_core::{DefId, Ty};
use yel_core::lir::{
    ArithOp, ArrayItemRepr, BinOperand, CompareOp, LirExprKind, LirGlobalRef, LirOp, LirSlotKind, LirSlotValType, LirTypeRef,
    MemoryValueType, StoreWidth,
};
use yel_core::types::InternedTyKind;

use super::super::CodegenError;
use super::super::{MemoryLayout, WasmPackageBuilder};
use super::constants::{HANDLER_ID_HANDLE_SHIFT, MAX_HANDLERS_PER_COMPONENT};
use super::scratch::{mem_arg, slot_info, slot_local};
use yel_core::lir::{LirBlock, LirResource, LirSlotId};

impl<'a> WasmPackageBuilder<'a> {
    /// Emit a single block operation as WASM instructions.
    /// `local_offset` is added to slot indices for local variable access:
    /// - Mount function: 2 (for self, root params)
    /// - Block functions: 1 (for parent param) or 2 (for parent, item_ptr params)
    ///
    /// Emit a single LIR op into `func` against the component at
    /// `comp_idx`. The flow-frontend codegen calls this directly (via
    /// `crate::flow`), so it's `pub(crate)` rather than `pub(super)`
    /// — keeps the codegen layering visible without forcing flow to
    /// duplicate ~1700 lines of op-dispatch.
    pub(crate) fn emit_op(
        &mut self,
        func: &mut Function,
        op: &LirOp,
        comp_idx: usize,
        block: &LirBlock,
        local_offset: u32,
    ) -> Result<(), CodegenError> {
        let component = &self.components[comp_idx];
        let layout = self.layouts.get(comp_idx).cloned().unwrap_or_else(|| {
            // Fallback layout for a component missing from `self.layouts`.
            // Per-signal linear memory was removed (every non-unit signal is
            // GC-struct-resident), so there is no signal-sized region to
            // reserve here.
            MemoryLayout {
                base: 324,
                size: 0,
            }
        });

        match op {
            // Phase 3.3: `LirOp::MountComponent` and `LirOp::ResourceNew`
            // were deleted. Mount expansion is now a neutral op sequence
            // emitted by `lower_mount_component` — see lifecycle_inline.rs.
            LirOp::StoreHandle { slot, from } => {
                if let Some(slot_info) = Some(slot_info(*slot, block, component)) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::LocalGet(slot_local(
                                component, block,
                                *from,
                                local_offset,
                            )));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::Temp { .. } | LirSlotKind::WasmParam { .. } => {}
                    }
                }
            }
            LirOp::LoadHandle { slot, to } => {
                if let Some(slot_info) = Some(slot_info(*slot, block, component)) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            func.instruction(&Instruction::LocalSet(slot_local(
                                component, block,
                                *to,
                                local_offset,
                            )));
                        }
                        LirSlotKind::Temp { .. } | LirSlotKind::WasmParam { .. } => {}
                    }
                }
            }
            LirOp::StoreI32 { slot, value } => {
                if let Some(slot_info) = Some(slot_info(*slot, block, component)) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::I32Const(*value));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::Temp { .. } | LirSlotKind::WasmParam { .. } => {
                            // Temp / WasmParam target: `i32.const <value>;
                            // local.set <abs_idx>`. The if-update block in
                            // `create_if_update_block_flat` allocates a
                            // Temp slot for `upd_target` and writes 0/1/2
                            // into it via `StoreI32`; without this arm
                            // those writes silently no-op'd, so the
                            // update dispatch always ran with the
                            // default-zero target and never mounted a
                            // newly-active branch.
                            func.instruction(&Instruction::I32Const(*value));
                            func.instruction(&Instruction::LocalSet(slot_local(
                                component, block,
                                *slot,
                                local_offset,
                            )));
                        }
                    }
                }
            }
            LirOp::StoreI32Slot { slot, from } => {
                if let Some(slot_info) = Some(slot_info(*slot, block, component)) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::LocalGet(slot_local(
                                component, block,
                                *from,
                                local_offset,
                            )));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::Temp { .. } | LirSlotKind::WasmParam { .. } => {
                            // Temp-to-Temp copy is a plain local.set/get.
                            func.instruction(&Instruction::LocalGet(slot_local(
                                component, block,
                                *from,
                                local_offset,
                            )));
                            func.instruction(&Instruction::LocalSet(slot_local(
                                component, block,
                                *slot,
                                local_offset,
                            )));
                        }
                    }
                }
            }
            LirOp::Compare {
                op,
                lhs,
                rhs,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *lhs,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *rhs,
                    local_offset,
                )));
                func.instruction(&match op {
                    CompareOp::GeU => Instruction::I32GeU,
                    CompareOp::LtU => Instruction::I32LtU,
                    CompareOp::Ne => Instruction::I32Ne,
                });
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
            LirOp::I32EqConst { lhs, rhs, result } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *lhs,
                    local_offset,
                )));
                func.instruction(&Instruction::I32Const(*rhs));
                func.instruction(&Instruction::I32Eq);
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
            LirOp::LoadI32 { slot, to } => {
                if let Some(slot_info) = Some(slot_info(*slot, block, component)) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            func.instruction(&Instruction::LocalSet(slot_local(
                                component, block,
                                *to,
                                local_offset,
                            )));
                        }
                        LirSlotKind::Temp { .. } | LirSlotKind::WasmParam { .. } => {}
                    }
                }
            }
            LirOp::EvalExpr { expr, result } => {
                let lir_expr = component.get_expr(*expr);

                // Payload-less user-variant constructor targeting a
                // single-i32 slot: `emit_variant_ctor_flat` would push
                // (discriminant + joined payload zero-pads), but the
                // surrounding EvalExpr only stores a single i32. Emit just
                // the discriminant to keep the stack balanced — matches the
                // existing shortcut in `emit_flat_slot_store` and mirrors
                // how LIR SignalWrite models variant signals as a single
                // i32 slot for the discriminant today.
                if let LirExprKind::VariantCtor {
                    case_idx,
                    payload: None,
                    ..
                } = &lir_expr.kind
                    && matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::Adt(_))
                {
                    func.instruction(&Instruction::I32Const(*case_idx as i32));
                    func.instruction(&Instruction::LocalSet(slot_local(
                        component, block,
                        *result,
                        local_offset,
                    )));
                    return Ok(());
                }

                self.emit_expr(func, lir_expr, component)?;

                // Skip storing result if expression returns void (unit type)
                // This happens with callback calls that return nothing
                if lir_expr.ty == Ty::UNIT {
                    // No value on stack to store
                } else {
                    // Check if expression type is a "fat pointer" (list or string)
                    // Fat pointers return (ptr, len) on stack and need two consecutive slots.
                    // Phase 5b-v.3+: GC-eligible lists (typed array refs) push a
                    // single ref instead of (ptr, len), so they spill to one slot.
                    let is_fat_ptr = matches!(
                        self.ctx.ty_kind(lir_expr.ty),
                        InternedTyKind::List(_) | InternedTyKind::String
                    ) && !matches!(
                        self.internal_repr(lir_expr.ty),
                        super::super::repr::InternalRepr::GcArrayRef(_)
                    );
                    // Check if expression type is Option (returns discriminant, value)
                    let is_option =
                        matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::Option(_));
                    if is_fat_ptr {
                        // Stack has [ptr, len], store len first (top of stack), then ptr
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, block, *result, local_offset) + 1,
                        )); // len -> slot+1
                        func.instruction(&Instruction::LocalSet(slot_local(
                            component, block,
                            *result,
                            local_offset,
                        ))); // ptr -> slot
                    } else if is_option {
                        // Option with payload: Stack has [discriminant, value]
                        // For `none`, VariantCtor only pushes discriminant
                        // For `some(v)`, VariantCtor pushes (discriminant, value)
                        // Check if it's a none variant (no payload)
                        if let LirExprKind::VariantCtor { payload: None, .. } = &lir_expr.kind {
                            // `none` - only discriminant on stack
                            func.instruction(&Instruction::LocalSet(slot_local(
                                component, block,
                                *result,
                                local_offset,
                            ))); // discriminant -> slot
                        // No value to store - slot+1 will be undefined/zero
                        } else {
                            // `some(v)` - (discriminant, value) on stack
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, block, *result, local_offset) + 1,
                            )); // value -> slot+1
                            func.instruction(&Instruction::LocalSet(slot_local(
                                component, block,
                                *result,
                                local_offset,
                            ))); // discriminant -> slot
                        }
                    } else {
                        func.instruction(&Instruction::LocalSet(slot_local(
                            component, block,
                            *result,
                            local_offset,
                        )));
                    }
                }
            }
            LirOp::EvalExprToSlots {
                expr,
                dest_first_slot,
            } => {
                let lir_expr = component.get_expr(*expr);

                // Payload-less user-variant constructor shortcut, same
                // as the EvalExpr arm: push a single i32 discriminant.
                if let LirExprKind::VariantCtor {
                    case_idx,
                    payload: None,
                    ..
                } = &lir_expr.kind
                    && matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::Adt(_))
                {
                    func.instruction(&Instruction::I32Const(*case_idx as i32));
                    func.instruction(&Instruction::LocalSet(slot_local(
                        component, block,
                        *dest_first_slot,
                        local_offset,
                    )));
                    return Ok(());
                }

                self.emit_expr(func, lir_expr, component)?;

                if lir_expr.ty == Ty::UNIT {
                    return Ok(());
                }

                // Determine stack arity from the expression's storage
                // valtypes (matches the SignalWriteExpr → store path):
                // 1 for Scalar / GcRef / GcArrayRef / GcVariant,
                // 2 for FatPointer (string / non-typed-array list).
                // For Option / payload-less variant ctor, `emit_expr`
                // pushes the partial set — handle the `none` case.
                if let LirExprKind::VariantCtor { payload: None, .. } = &lir_expr.kind
                    && matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::Option(_))
                {
                    // `none` — only discriminant on stack.
                    func.instruction(&Instruction::LocalSet(slot_local(
                        component, block,
                        *dest_first_slot,
                        local_offset,
                    )));
                    return Ok(());
                }

                let n = self.signal_storage_valtypes(lir_expr.ty).len();
                let base = slot_local(component, block, *dest_first_slot, local_offset);
                for i in (0..n).rev() {
                    func.instruction(&Instruction::LocalSet(base + i as u32));
                }
            }
            LirOp::DropExpr { expr } => {
                let lir_expr = component.get_expr(*expr);
                self.emit_expr(func, lir_expr, component)?;
                // Drop exactly the number of values the expression pushed.
                // Unit-typed expressions (e.g. callbacks returning nothing)
                // push zero values on the stack, so no drops are emitted in
                // that case. `flatten_core_valtypes` treats unknown primitives
                // as a single i32, so we special-case Unit explicitly here.
                if !matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::Unit) {
                    // GcVariant produces a single supertype ref, not
                    // canonical-flat slots, so drop count must follow
                    // internal stack-slot count for those Tys.
                    let drop_count = match self.internal_repr(lir_expr.ty) {
                        crate::wasm::repr::InternalRepr::GcVariant(_) => {
                            self.internal_stack_slots(lir_expr.ty)
                        }
                        // strings-to-GC: a string is a single $str_bytes ref
                        // internally, not canonical (ptr, len).
                        crate::wasm::repr::InternalRepr::GcArrayRef(_)
                            if matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::String) =>
                        {
                            self.internal_stack_slots(lir_expr.ty)
                        }
                        _ => self.flatten_core_valtypes(lir_expr.ty).len(),
                    };
                    for _ in 0..drop_count {
                        func.instruction(&Instruction::Drop);
                    }
                }
            }
            LirOp::If(if_op) => {
                // Mint a label index for this `if` structural op before
                // emitting (preorder walk). Nested ifs/loops inside the
                // branches will get subsequent indices as they're visited.
                let if_label_idx = self.current_label_counter;
                self.current_label_counter += 1;
                if let Some(n) = &if_op.name {
                    self.current_function_labels.push((if_label_idx, n.clone()));
                }
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    if_op.cond,
                    local_offset,
                )));
                func.instruction(&Instruction::If(BlockType::Empty));

                for nested_op in &if_op.then_ops {
                    self.emit_op(func, nested_op, comp_idx, block, local_offset)?;
                }

                if !if_op.else_ops.is_empty() {
                    func.instruction(&Instruction::Else);
                    for nested_op in &if_op.else_ops {
                        self.emit_op(func, nested_op, comp_idx, block, local_offset)?;
                    }
                }

                func.instruction(&Instruction::End);
            }
            LirOp::PushSlot { slot } => {
                // Stack-push primitive: emit `local.get` so the
                // following `CallFunction { args: vec![] }` consumes
                // this slot's value as a positional arg.
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *slot,
                    local_offset,
                )));
            }
            LirOp::PushStringPtr { string_id } => {
                // Stack-push primitive: resolve `string_id` via the
                // component's string pool and emit `i32.const <data_ptr>`.
                let s = component.get_string(*string_id);
                let (ptr, _len) = self.get_string_info(s).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "PushStringPtr: string {:?} (\"{}\") not in string pool",
                        string_id, s
                    ))
                })?;
                func.instruction(&Instruction::I32Const(ptr as i32));
            }
            LirOp::PushStringLen { string_id } => {
                let s = component.get_string(*string_id);
                let (_ptr, len) = self.get_string_info(s).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "PushStringLen: string {:?} (\"{}\") not in string pool",
                        string_id, s
                    ))
                })?;
                func.instruction(&Instruction::I32Const(len as i32));
            }
            LirOp::PushExpr { expr } => {
                // Generic stack-push for a host-call argument: emit the
                // value's canonical-ABI flat representation. A `VariantCtor`
                // (set-attribute's `attribute-value`) must use the FLAT
                // boundary lowering even when the type's in-memory repr is a
                // GC struct — `emit_expr` would otherwise dispatch to the GC
                // ctor and push a ref. Other exprs flatten via `emit_expr`.
                let lir_expr = component.get_expr(*expr);
                if let LirExprKind::VariantCtor {
                    case_idx, payload, ..
                } = &lir_expr.kind
                {
                    let payload_expr = payload.map(|p| component.get_expr(p));
                    self.emit_variant_ctor_flat(
                        func,
                        lir_expr.ty,
                        *case_idx,
                        payload_expr.as_deref(),
                        component,
                    )?;
                } else if matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::String) {
                    // strings-to-GC (`plans/strings-to-gc.md`): PushExpr is a
                    // host-call boundary. A GC-native string is a `$str_bytes`
                    // ref internally — materialize it to canonical (ptr, len)
                    // right here before the host consumes it.
                    self.emit_expr(func, lir_expr, component)?;
                    self.emit_str_bytes_materialize(func)?;
                } else {
                    self.emit_expr(func, lir_expr, component)?;
                }
            }
            LirOp::PushHandlerId { handler } => {
                // Stack-push primitive: replicate the legacy
                // `AddEventListener` arm's handler-id encoding —
                // allocate a per-component local ordinal, register it
                // in `global_handler_map`, and emit
                // `global.get current_handle; i32.const SHIFT; i32.shl;
                //  i32.const local_id; i32.or`. The result (the encoded
                // id, an i32) is left on the stack for the following
                // CallFunction to consume.
                let local_id = *self.next_handler_local_id.entry(comp_idx).or_insert(0);
                if local_id >= MAX_HANDLERS_PER_COMPONENT {
                    return Err(CodegenError::InvalidIR(format!(
                        "component {} exceeds the {}-handler-site limit \
                         imposed by the (handle<<{})|local_id encoding",
                        comp_idx, MAX_HANDLERS_PER_COMPONENT, HANDLER_ID_HANDLE_SHIFT
                    )));
                }
                self.next_handler_local_id.insert(comp_idx, local_id + 1);
                self.global_handler_map.push((local_id, comp_idx, *handler));

                let handle_g =
                    self.gc_layouts[comp_idx]
                        .current_handle_global
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "PushHandlerId: missing current_handle_global \
                             for component — every component must reserve one \
                             for handle-aware dispatch encoding."
                                    .into(),
                            )
                        })?;
                func.instruction(&Instruction::GlobalGet(handle_g));
                func.instruction(&Instruction::I32Const(HANDLER_ID_HANDLE_SHIFT));
                func.instruction(&Instruction::I32Shl);
                func.instruction(&Instruction::I32Const(local_id as i32));
                func.instruction(&Instruction::I32Or);
            }
            LirOp::CallFunction {
                func: callee_def,
                args,
                result,
            } => {
                // Host imports (DOM functions, and later global
                // callbacks) resolve through the single import registry
                // on `ImportLayout`. The lowering emits `CallFunction {
                // func: <import DefId>, … }`; the registry maps it back
                // to the wasm import index. `import_layout` is `None` on
                // the flow-frontend function-module path, which never
                // targets host imports and instead populates
                // `def_id_to_func_idx`.
                let callee_idx = if let Some(import_idx) = self
                    .import_layout
                    .as_ref()
                    .and_then(|l| l.import_index(*callee_def))
                {
                    import_idx
                } else {
                    // Resolve callee via the externally-populated
                    // `def_id_to_func_idx`. Empty for pure-UI builds (which
                    // never emit this op), populated by flow-frontend
                    // codegen before op-emit runs.
                    self.def_id_to_func_idx
                        .get(callee_def)
                        .copied()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "LirOp::CallFunction references DefId({}) but no wasm \
                             function index is registered for it. Callers must \
                             populate `WasmPackageBuilder::def_id_to_func_idx` \
                             before emit.",
                                callee_def.0
                            ))
                        })?
                };
                // Push args in order. Each arg is a Temp slot whose
                // wasm local index is resolved through the standard
                // `slot_local + local_offset` rule used everywhere
                // else in op-emit.
                for arg in args {
                    func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *arg,
                        local_offset,
                    )));
                }
                func.instruction(&Instruction::Call(callee_idx));
                // Sink the return value (if any) into the result slot.
                // No return → callee's wasm type ends with `-> ()`; the
                // stack stays balanced naturally.
                if let Some(r) = result {
                    func.instruction(&Instruction::LocalSet(slot_local(
                        component, block,
                        *r,
                        local_offset,
                    )));
                }
            }
            LirOp::CallBlock {
                block: callee_block,
                args,
                result,
            } => {
                if let Some(&func_idx) = self.block_func_indices.get(callee_block) {
                    // Phase 0.3p / 0.3q: callee's wasm sig is
                    // `[user params from block.params]
                    // + [boundary refs from boundary_param_slots] -> [return?]`.
                    // BlockIds are now module-wide unique, so the callee
                    // may live in a different component (cross-component
                    // lifecycle calls). Locate the owning component to
                    // resolve the callee's `LirBlock` and its boundary
                    // params correctly.
                    let owner_idx = self
                        .components
                        .iter()
                        .position(|c| c.blocks.iter().any(|b| b.id == *callee_block))
                        .ok_or_else(|| {
                            CodegenError::InternalError(format!(
                                "CallBlock: no component owns block {:?}",
                                callee_block
                            ))
                        })?;
                    let owner = &self.components[owner_idx];
                    let callee = owner.get_block(*callee_block);
                    for arg in args {
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *arg,
                            local_offset,
                        )));
                    }
                    // Boundary refs are only meaningful for intra-component
                    // calls — cross-component lifecycle blocks have empty
                    // `boundary_params`. Push refs from the caller's
                    // (current) component when intra; skip otherwise.
                    if owner_idx == comp_idx {
                        let bp_ids: Vec<_> =
                            callee.boundary_param_ids_from_slots(&owner.slots).collect();
                        for b_id in bp_ids {
                            self.emit_boundary_ref(func, comp_idx, b_id)?;
                        }
                    } else {
                        debug_assert!(
                            callee.boundary_params.is_empty(),
                            "cross-component CallBlock target must have empty boundary_params"
                        );
                    }
                    func.instruction(&Instruction::Call(func_idx));
                    if let Some(r) = result {
                        func.instruction(&Instruction::LocalSet(slot_local(
                            component, block,
                            *r,
                            local_offset,
                        )));
                    } else if callee.return_slot.is_some() {
                        // Callee returns a value but caller doesn't want
                        // it — drop so the wasm stack stays balanced.
                        func.instruction(&Instruction::Drop);
                    }
                }
            }

            // Phase 0.3m: registry ops + resource-new dispatch.
            LirOp::RegistryLookupToSelfRef {
                component: comp_def,
                handle,
                result,
            } => {
                let ci = self.comp_idx_by_def_id(*comp_def).map_err(|_| {
                    CodegenError::InvalidIR(format!(
                        "RegistryLookupToSelfRef: no component for {:?}",
                        comp_def
                    ))
                })?;
                let handle_local = slot_local(component, block, *handle, local_offset);
                let result_local = slot_local(component, block, *result, local_offset);
                self.emit_registry_lookup(func, ci, handle_local, result_local)?;
            }
            LirOp::RegistryAlloc {
                component: comp_def,
                ref_slot,
                idx_scratch,
                arr_scratch,
                result_handle,
            } => {
                let ci = self.comp_idx_by_def_id(*comp_def).map_err(|_| {
                    CodegenError::InvalidIR(format!(
                        "RegistryAlloc: no component for {:?}",
                        comp_def
                    ))
                })?;
                let inst_local = slot_local(component, block, *ref_slot, local_offset);
                let idx_local = slot_local(component, block, *idx_scratch, local_offset);
                let arr_local = slot_local(component, block, *arr_scratch, local_offset);
                self.emit_registry_alloc(func, ci, inst_local, idx_local, arr_local)?;
                // emit_registry_alloc leaves the i32 handle on stack — pop into result.
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result_handle,
                    local_offset,
                )));
            }
            LirOp::CallResourceNew {
                component: comp_def,
                handle,
                result,
            } => {
                let ci = self.comp_idx_by_def_id(*comp_def).map_err(|_| {
                    CodegenError::InvalidIR(format!(
                        "CallResourceNew: no component for {:?}",
                        comp_def
                    ))
                })?;
                let import_layout = self.import_layout.as_ref().ok_or_else(|| {
                    CodegenError::InternalError(
                        "CallResourceNew: import_layout not populated".into(),
                    )
                })?;
                let resource_new_idx = import_layout
                    .components
                    .get(ci)
                    .and_then(|c| c.resource_new)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "CallResourceNew: component {:?} has no [resource-new] import",
                            comp_def
                        ))
                    })?;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *handle,
                    local_offset,
                )));
                func.instruction(&Instruction::Call(resource_new_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
            // === Constructor Operations ===
            LirOp::InitSignal { signal_idx, expr } => {
                let default_expr = component.get_expr(*expr);
                let scratch = self.current_flat_scratch.unwrap_or_default();
                let sig_idx = *signal_idx as usize;
                if self.signal_in_struct(comp_idx, sig_idx) {
                    self.emit_signal_struct_store_from_expr(
                        func,
                        comp_idx,
                        sig_idx,
                        default_expr,
                        component,
                        scratch,
                    )?;
                } else {
                    unreachable!(
                        "InitSignal: non-struct signal is unreachable — \
                         every non-unit signal is GC-struct-resident"
                    )
                }
            }

            LirOp::SignalWriteExpr { signal, expr } => {
                let lir_expr = component.get_expr(*expr);
                let scratch = self.current_flat_scratch.unwrap_or_default();
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    if self.signal_in_struct(comp_idx, sig_idx) {
                        self.emit_signal_struct_store_from_expr(
                            func, comp_idx, sig_idx, lir_expr, component, scratch,
                        )?;
                    } else {
                        unreachable!(
                            "SignalWriteExpr: non-struct signal is unreachable — \
                             every non-unit signal is GC-struct-resident"
                        )
                    }
                } else if self.ctx.defs.owning_global_block(*signal).is_some() {
                    if self.global_in_struct(*signal) {
                        self.emit_global_struct_store_from_expr(
                            func, *signal, lir_expr, component, scratch,
                        )?;
                    } else if let Some(&addr) = self.global_property_addrs.get(signal) {
                        self.emit_signal_store(func, addr, lir_expr, component, scratch)?;
                    } else {
                        return Err(CodegenError::InvalidIR(format!(
                            "SignalWriteExpr: pointer-typed global property {:?} has no \
                             memory address",
                            signal
                        )));
                    }
                } else {
                    return Err(CodegenError::InvalidIR(format!(
                        "SignalWriteExpr: no address for signal {:?}",
                        signal
                    )));
                }
            }

            LirOp::InitSignalDefault { signal_idx } => {
                let sig_idx = *signal_idx as usize;
                // Struct-migrated signals: `struct.new_default` in the
                // constructor already initialised every field to its
                // type's zero/null default, so nothing to do here —
                // EXCEPT for GcVariant, whose null default is
                // semantically "no active case" rather than "case 0".
                // Materialize case 0 explicitly so `ref.test` returns
                // true for case 0 (matching legacy zero-byte memory
                // init that produced "disc=0, payload=zeros").
                if self.signal_in_struct(comp_idx, sig_idx) {
                    let signal_ty = component.signals[sig_idx].ty;
                    if let super::super::repr::InternalRepr::GcVariant(_) =
                        self.internal_repr(signal_ty)
                    {
                        let case0_sub_idx = *self
                            .record_gc_types
                            .gc_variant_case_idx
                            .get(&(signal_ty, 0))
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "InitSignalDefault: missing gc_variant_case_idx \
                                     for ty={:?} case=0",
                                    signal_ty
                                ))
                            })?;
                        // self ref → struct.new_default $<sup>_<case0>
                        // → struct.set on the component field.
                        self.emit_signal_struct_store_const_default(
                            func,
                            comp_idx,
                            sig_idx,
                            case0_sub_idx,
                        )?;
                    }
                    // Other struct-migrated signals (records, lists,
                    // tuples, etc.): null default from `struct.new_default
                    // $Comp` is already correct.
                } else {
                    unreachable!(
                        "InitSignalDefault: non-struct signal is unreachable — \
                         every non-unit signal is GC-struct-resident"
                    )
                }
            }

            LirOp::StructGet {
                rec,
                field_idx,
                result,
            } => {
                // Generic struct.get. The wasm struct-type index is
                // resolved from `rec`'s val_ty (RefNullForBoundary /
                // RefNullForComponent / RefNull) — the op carries no
                // frontend concept. The ref is passed directly; no
                // `current_boundary_locals` chain walk needed.
                let struct_ty = self.struct_ty_idx_from_rec(comp_idx, component, block, *rec)?;
                let rec_local = slot_local(component, block, *rec, local_offset);
                func.instruction(&Instruction::LocalGet(rec_local));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: struct_ty,
                    field_index: *field_idx,
                });
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::StructSet {
                rec,
                field_idx,
                value,
            } => {
                let struct_ty = self.struct_ty_idx_from_rec(comp_idx, component, block, *rec)?;
                let rec_local = slot_local(component, block, *rec, local_offset);
                func.instruction(&Instruction::LocalGet(rec_local));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: *field_idx,
                });
            }

            LirOp::StructSetConst {
                rec,
                field_idx,
                value,
            } => {
                let struct_ty = self.struct_ty_idx_from_rec(comp_idx, component, block, *rec)?;
                let rec_local = slot_local(component, block, *rec, local_offset);
                func.instruction(&Instruction::LocalGet(rec_local));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::I32Const(*value));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: *field_idx,
                });
            }

            LirOp::BoundaryRefFromSelf {
                boundary_id,
                result,
            } => {
                // Stage 5e-1: walk `$self.tree → ... → boundary_id`
                // at LIR layer. Reuses the same chain-walk logic
                // codegen used to do implicitly via
                // `emit_boundary_ref`'s fallback path; the result
                // is now stashed into a slot the rewriter has
                // bound for subsequent StructGet/StructSet ops.
                self.emit_boundary_ref(func, comp_idx, *boundary_id)?;
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            // Pre-rewrite symbolic struct-field ops (Stage 5e-4). The
            // `boundary_rewrite` pass resolves these into generic
            // `Struct{Get,Set,SetConst}` ops before codegen. Reaching here
            // means the pass missed one — same invariant the LIR-layer
            // `debug_assert` in `lower_component` guards.
            LirOp::StructFieldGet { struct_ty, field_idx, .. }
            | LirOp::StructFieldSet { struct_ty, field_idx, .. }
            | LirOp::StructFieldSetConst { struct_ty, field_idx, .. } => {
                unreachable!(
                    "symbolic StructField op (struct_ty {struct_ty:?} field {field_idx}) \
                     reached codegen — boundary_rewrite pass missed it"
                );
            }

            LirOp::BindBoundaryLocal { boundary_id, slot } => {
                // Pure compile-time scope-tracking: record the slot's
                // local index against the boundary so subsequent
                // BoundaryField accesses resolve via local.get. No
                // WASM instructions emitted.
                let local_idx = slot_local(component, block, *slot, local_offset);
                self.current_boundary_locals.insert(*boundary_id, local_idx);
            }

            LirOp::AllocBoundary {
                boundary_id,
                ref_slot,
            } => {
                let component = &self.components[comp_idx];
                let gc = &self.gc_layouts[comp_idx];
                let new_struct_ty = *gc.tree_struct_type_idx.get(boundary_id).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "AllocBoundary: missing struct type for boundary {:?}",
                        boundary_id
                    ))
                })?;
                let ref_local = slot_local(component, block, *ref_slot, local_offset);
                func.instruction(&Instruction::StructNewDefault(new_struct_ty));
                func.instruction(&Instruction::LocalSet(ref_local));
                self.current_boundary_locals.insert(*boundary_id, ref_local);
            }

            LirOp::AllocSubBoundary {
                boundary_id,
                ref_slot,
            } => {
                // Stage 5d: parent link from the registry.
                let component = &self.components[comp_idx];
                let parent_link = component
                    .struct_types
                    .get(boundary_id.index())
                    .and_then(|s| s.parent)
                    .map(|p| (yel_core::ids::TreeBoundaryId(p.parent.0), p.field_idx))
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "AllocSubBoundary: boundary {:?} has no parent_link \
                             (root or for-iter-body cannot be alloc'd via this op)",
                            boundary_id
                        ))
                    })?;
                let (parent_boundary, parent_field_idx) = parent_link;

                let gc = &self.gc_layouts[comp_idx];
                let new_struct_ty = *gc.tree_struct_type_idx.get(boundary_id).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "AllocSubBoundary: missing struct type for boundary {:?}",
                        boundary_id
                    ))
                })?;
                let parent_struct_ty =
                    *gc.tree_struct_type_idx
                        .get(&parent_boundary)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "AllocSubBoundary: missing struct type for parent {:?}",
                                parent_boundary
                            ))
                        })?;

                let ref_local = slot_local(component, block, *ref_slot, local_offset);

                // 1) Alloc the new boundary struct, stash in ref_slot.
                func.instruction(&Instruction::StructNewDefault(new_struct_ty));
                func.instruction(&Instruction::LocalSet(ref_local));

                // 2) Store ref on parent.SubBoundary field.
                self.emit_boundary_ref(func, comp_idx, parent_boundary)?;
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::LocalGet(ref_local));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: parent_struct_ty,
                    field_index: parent_field_idx,
                });

                // 3) Register the new boundary's ref_local for the
                //    rest of this emit scope.
                self.current_boundary_locals.insert(*boundary_id, ref_local);
            }

            LirOp::InitMemorySlot { slot } => {
                if let Some(slot_info) = Some(slot_info(*slot, block, component)) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(layout.base + offset as i32));
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::Temp { .. } | LirSlotKind::WasmParam { .. } => {}
                    }
                }
            }

            // Phase 3.3: LirOp::ResourceNew deleted (never emitted from
            // yel-core lowering; legacy `i32.const base_addr` body was
            // only used by the non-exported constructor wrapper which
            // is now codegen-inline-only).
            LirOp::SignalWrite { signal, value } => {
                // Component-local struct-migrated signal — struct.set
                // each ABI slot from the consecutive value locals.
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    if self.signal_in_struct(comp_idx, sig_idx) {
                        self.emit_signal_struct_store_from_slot(
                            func,
                            comp_idx,
                            sig_idx,
                            component,
                            block,
                            *value,
                            local_offset,
                        )?;
                        return Ok(());
                    }
                    // Pointer-typed signal: fall through to the
                    // legacy memory-write path below.
                } else if self.ctx.defs.owning_global_block(*signal).is_some()
                    && self.global_in_struct(*signal)
                {
                    // Migrated global property — write via struct.set
                    // sourcing values from consecutive WASM locals.
                    self.emit_global_struct_store_from_slot(
                        func,
                        *signal,
                        component,
                        block,
                        *value,
                        local_offset,
                    )?;
                    return Ok(());
                }
                // Global property or non-migrated signal — keep
                // linear-memory write.
                let (addr, signal_ty) =
                    if let Some(_sig_idx) = self.signal_index_in(component, *signal) {
                        unreachable!(
                            "SignalWrite: memory path is globals-only; \
                             signals are GC-struct-resident"
                        )
                    } else if let Some(&a) = self.global_property_addrs.get(signal) {
                        let ty = self
                            .ctx
                            .defs
                            .type_of(*signal)
                            .unwrap_or(yel_core::types::Ty::ERROR);
                        (a, ty)
                    } else {
                        return Err(CodegenError::InvalidIR(format!(
                            "SignalWrite: no address for signal {:?}",
                            signal
                        )));
                    };
                match self.ctx.ty_kind(signal_ty) {
                    InternedTyKind::F32 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                    }
                    InternedTyKind::F64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                    }
                    InternedTyKind::S64 | InternedTyKind::U64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                    }
                    InternedTyKind::Option(_) => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, block, *value, local_offset) + 1,
                        ));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                    InternedTyKind::String | InternedTyKind::List(_) => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, block, *value, local_offset) + 1,
                        ));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                    // Narrow types are packed 1/2 bytes wide — a full
                    // i32.store would clobber the adjacent signal's bytes
                    // (e.g. a bool at offset 0 next to a string ptr at
                    // offset 1 would corrupt the ptr on every toggle).
                    InternedTyKind::Bool
                    | InternedTyKind::U8
                    | InternedTyKind::S8
                    | InternedTyKind::Char => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
                    }
                    InternedTyKind::U16 | InternedTyKind::S16 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::I32Store16(mem_arg(0, 1)));
                    }
                    _ => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(slot_local(
                            component, block,
                            *value,
                            local_offset,
                        )));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                }
            }

            LirOp::TriggerEffects { signal } => {
                self.emit_trigger_effects(func, *signal, comp_idx)?;
            }

            LirOp::Return => {
                func.instruction(&Instruction::Return);
            }

            LirOp::ReturnValue { value } => {
                // Push the value slot's wasm local, then `return` —
                // satisfies the function's declared typed return on
                // the stack. Used by flat top-level functions whose
                // lowering emits early returns from inside If / Loop
                // arms (UI blocks instead use `block_fn.rs`'s
                // trailing-`local.get` convention and never emit
                // this op).
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                func.instruction(&Instruction::Return);
            }

            // === Loop Operations ===
            LirOp::Loop {
                break_cond,
                body_ops,
                name,
            } => {
                // `LirOp::Loop` emits TWO structural ops (outer `block`
                // then inner `loop`); each consumes its own label index.
                // Suffix the names so WAT dumps distinguish `${name}_block`
                // from `${name}_loop`.
                let block_label_idx = self.current_label_counter;
                self.current_label_counter += 1;
                let loop_label_idx = self.current_label_counter;
                self.current_label_counter += 1;
                if let Some(n) = name {
                    self.current_function_labels
                        .push((block_label_idx, format!("{}_block", n)));
                    self.current_function_labels
                        .push((loop_label_idx, format!("{}_loop", n)));
                }
                func.instruction(&Instruction::Block(BlockType::Empty));
                func.instruction(&Instruction::Loop(BlockType::Empty));

                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *break_cond,
                    local_offset,
                )));
                func.instruction(&Instruction::BrIf(1));

                for nested_op in body_ops {
                    self.emit_op(func, nested_op, comp_idx, block, local_offset)?;
                }

                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End);
                func.instruction(&Instruction::End);
            }

            LirOp::BinaryOp {
                op,
                lhs,
                rhs,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *lhs,
                    local_offset,
                )));
                match rhs {
                    BinOperand::Slot(s) => func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *s,
                        local_offset,
                    ))),
                    BinOperand::Const(c) => func.instruction(&Instruction::I32Const(*c as i32)),
                };
                func.instruction(&match op {
                    ArithOp::Add => Instruction::I32Add,
                    ArithOp::Sub => Instruction::I32Sub,
                    ArithOp::Mul => Instruction::I32Mul,
                });
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::Alloc {
                size,
                align,
                result,
            } => {
                if let Some(alloc_funcs) = &self.alloc_funcs {
                    func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *size,
                        local_offset,
                    )));
                    func.instruction(&Instruction::I32Const(*align as i32));
                    func.instruction(&Instruction::Call(alloc_funcs.alloc));
                    func.instruction(&Instruction::LocalSet(slot_local(
                        component, block,
                        *result,
                        local_offset,
                    )));
                }
            }

            LirOp::Free { ptr, size } => {
                if let Some(alloc_funcs) = &self.alloc_funcs {
                    func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *ptr,
                        local_offset,
                    )));
                    func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *size,
                        local_offset,
                    )));
                    func.instruction(&Instruction::Call(alloc_funcs.free));
                }
            }

            LirOp::LoadAddr { addr, result, ty } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *addr,
                    local_offset,
                )));
                // Natural alignment: i32/f32 → 4 (align=2), i64/f64 → 8 (align=3).
                func.instruction(&match ty {
                    MemoryValueType::I32 => Instruction::I32Load(mem_arg(0, 2)),
                    MemoryValueType::I64 => Instruction::I64Load(mem_arg(0, 3)),
                    MemoryValueType::F32 => Instruction::F32Load(mem_arg(0, 2)),
                    MemoryValueType::F64 => Instruction::F64Load(mem_arg(0, 3)),
                });
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::StoreAddr {
                addr,
                value,
                ty,
                width,
            } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *addr,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                func.instruction(&match (ty, width) {
                    // Narrow stores are i32-valued; ty is always I32 here.
                    (_, StoreWidth::Narrow8) => Instruction::I32Store8(mem_arg(0, 0)),
                    (_, StoreWidth::Narrow16) => Instruction::I32Store16(mem_arg(0, 1)),
                    (MemoryValueType::I32, StoreWidth::Full) => Instruction::I32Store(mem_arg(0, 2)),
                    (MemoryValueType::I64, StoreWidth::Full) => Instruction::I64Store(mem_arg(0, 3)),
                    (MemoryValueType::F32, StoreWidth::Full) => Instruction::F32Store(mem_arg(0, 2)),
                    (MemoryValueType::F64, StoreWidth::Full) => Instruction::F64Store(mem_arg(0, 3)),
                });
            }

            LirOp::MemConst { addr, result } => {
                // Phase 1.2: `addr` is a per-component *relative* offset
                // (from `signal_layout.signals[i].mem.offset`). Codegen
                // applies the per-component memory base here so the
                // emitted `i32.const` matches the legacy `signal_addr()`
                // arithmetic used by `LirOp::Signal*` — preserving
                // byte-identical wasm output across the switch.
                //
                // Today MemConst is **only** emitted from the inline
                // signal-write helpers in `signals_inline.rs`, so this
                // unconditional base addition is safe. If a future
                // caller wants an absolute address it will need a
                // dedicated `MemConstAbs` variant.
                let abs_addr = (*addr as i32).wrapping_add(layout.base);
                func.instruction(&Instruction::I32Const(abs_addr));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::MemConstGlobalProp { signal_def, offset, result } => {
                // Resolve the pointer-typed global property's absolute
                // memory base via the module-level `global_property_addrs`
                // map, then add the static `offset`. Mirrors the legacy
                // `emit_signal_store` path that read this map directly.
                let base = *self.global_property_addrs.get(signal_def).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "MemConstGlobalProp: no global_property_addrs entry for {:?} \
                         (pointer-typed global expected)",
                        signal_def
                    ))
                })?;
                let abs_addr = base.wrapping_add(*offset as i32);
                func.instruction(&Instruction::I32Const(abs_addr));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            // === GC ops ===
            LirOp::StructNew {
                ty_idx,
                fields,
                result,
            } => {
                for field_slot in fields {
                    func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *field_slot,
                        local_offset,
                    )));
                }
                func.instruction(&Instruction::StructNew(*ty_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            // Phase 0.2: symbolic-ty GC struct ops. `ty_ref` resolves
            // to a wasm type-section index via `gc_layouts` at emit
            // time; thereafter the emit shape mirrors the concrete
            // `StructNew` / `Get` / `Set` arms above.
            LirOp::StructNewSym {
                ty_ref,
                fields,
                result,
            } => {
                let ty_idx = self.resolve_lir_type_ref(comp_idx, *ty_ref)?;
                for field_slot in fields {
                    func.instruction(&Instruction::LocalGet(slot_local(
                        component, block,
                        *field_slot,
                        local_offset,
                    )));
                }
                func.instruction(&Instruction::StructNew(ty_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::StructGetSym {
                ty_ref,
                field,
                rec,
                result,
            } => {
                let ty_idx = self.resolve_lir_type_ref(comp_idx, *ty_ref)?;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *rec,
                    local_offset,
                )));
                func.instruction(&Instruction::StructGet {
                    struct_type_index: ty_idx,
                    field_index: *field,
                });
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::StructSetSym {
                ty_ref,
                field,
                rec,
                value,
            } => {
                let ty_idx = self.resolve_lir_type_ref(comp_idx, *ty_ref)?;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *rec,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: ty_idx,
                    field_index: *field,
                });
            }

            LirOp::StructNewDefaultSym { ty_ref, result } => {
                let ty_idx = self.resolve_lir_type_ref(comp_idx, *ty_ref)?;
                func.instruction(&Instruction::StructNewDefault(ty_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::StructSetNewDefault {
                struct_ty,
                field,
                rec,
                field_ty,
            } => {
                let struct_ty_idx = self.resolve_lir_type_ref(comp_idx, *struct_ty)?;
                let field_ty_idx = self.resolve_lir_type_ref(comp_idx, *field_ty)?;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *rec,
                    local_offset,
                )));
                func.instruction(&Instruction::StructNewDefault(field_ty_idx));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty_idx,
                    field_index: *field,
                });
            }

            LirOp::ZeroI32Mem { addr } => {
                let abs_addr = (*addr as i32).wrapping_add(layout.base);
                func.instruction(&Instruction::I32Const(abs_addr));
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }

            LirOp::I32Const { value, result } => {
                func.instruction(&Instruction::I32Const(*value));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            // Phase 0.3c: symbolic wasm-global accessors. `gref`
            // resolves to a concrete wasm global index via
            // `gc_layouts` at emit time. Mirrors the LirTypeRef
            // pattern.
            LirOp::GlobalGet { gref, result } => {
                let idx = self.resolve_lir_global_ref(*gref)?;
                func.instruction(&Instruction::GlobalGet(idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
            LirOp::GlobalSet { gref, value } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                let idx = self.resolve_lir_global_ref(*gref)?;
                func.instruction(&Instruction::GlobalSet(idx));
            }

            // Global-field write — the single representation of a
            // global-property store. Resolves `(block, field)` to the
            // per-field core wasm global and emits `global.set`.
            LirOp::GlobalFieldSet {
                block: block_def,
                field,
                value,
            } => {
                let &layout_idx =
                    self.global_block_def_to_idx.get(block_def).ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "GlobalFieldSet: no globals layout for block {:?}",
                            block_def
                        ))
                    })?;
                let g = self.globals_layouts[layout_idx].field_core_globals[*field as usize];
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                func.instruction(&Instruction::GlobalSet(g));
            }

            // Stage 5a: dead `Array{NewDefault,Get,Set,Copy}` arms
            // removed. The only array-write ops emitted by the
            // lowerer today are the `ChildrenArray*` family
            // (per-ForAnchor children arrays).
            LirOp::ArrayLen { arr, result } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *arr,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::RefAsNonNull { slot } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *slot,
                    local_offset,
                )));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *slot,
                    local_offset,
                )));
            }

            LirOp::RefNull { ty_idx, result } => {
                func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                    *ty_idx,
                )));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::ArrayNewDefault {
                array_type,
                len,
                result,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].array_type_base + array_type.0;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *len,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayNewDefault(ty_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::ArrayGet {
                array_type,
                arr,
                idx,
                result,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].array_type_base + array_type.0;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *arr,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *idx,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayGet(ty_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }

            LirOp::ArraySet {
                array_type,
                arr,
                idx,
                value,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].array_type_base + array_type.0;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *arr,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *idx,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *value,
                    local_offset,
                )));
                func.instruction(&Instruction::ArraySet(ty_idx));
            }

            LirOp::ArrayCopy {
                array_type,
                dst,
                dst_idx,
                src,
                src_idx,
                count,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].array_type_base + array_type.0;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *dst,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *dst_idx,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *src,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *src_idx,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *count,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayCopy {
                    array_type_index_dst: ty_idx,
                    array_type_index_src: ty_idx,
                });
            }

            LirOp::SetSlot { slot, value } => {
                func.instruction(&Instruction::I32Const(*value));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *slot,
                    local_offset,
                )));
            }

            LirOp::CopySlot { from, to } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *from,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *to,
                    local_offset,
                )));
            }

            LirOp::GetSlotAddress { mem_slot, result } => {
                // Get the memory offset of a memory slot and store it in result
                if let Some(slot_info) = Some(slot_info(*mem_slot, block, component))
                    && let LirSlotKind::Memory { offset, .. } = slot_info.kind
                {
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::LocalSet(slot_local(
                        component, block,
                        *result,
                        local_offset,
                    )));
                }
            }

            // === Phase 5b-ii: parallel GC-array list ops ===
            //
            // These exist alongside `LoadList` / `EvalListExpr` /
            // `ComputeItemPtr`. No emitter currently produces them; the
            // codegen arms exist so a later sub-phase can flip
            // emission without re-touching this match.
            LirOp::LoadListGc {
                signal,
                ref_result,
                len_result,
            } => {
                // Stage 4a of typed-GC migration: support both
                // component-local AND global typed-list signals. The
                // legacy `LoadList` already had a global branch that
                // called the materializer; here we instead read the
                // typed array ref directly and use array.len so the
                // for-iter body can `array.get` without round-tripping
                // through canonical (ptr, len).
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    self.emit_signal_struct_read(func, comp_idx, sig_idx)?;
                } else if self.ctx.defs.owning_global_block(*signal).is_some() {
                    self.emit_global_struct_read(func, *signal)?;
                } else {
                    return Err(CodegenError::InvalidIR(format!(
                        "LoadListGc: signal {:?} is neither a component-local \
                         signal of `{}` nor a global property",
                        signal,
                        self.ctx.str(component.name)
                    )));
                }
                func.instruction(&Instruction::LocalTee(slot_local(
                    component, block,
                    *ref_result,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *len_result,
                    local_offset,
                )));
            }

            LirOp::EvalListExprGc {
                expr,
                ref_result,
                len_result,
            } => {
                // The expression's emit must leave a single typed array
                // ref on the stack (e.g. `array.new_fixed $<elem>_list
                // ...`). Tee into ref_result; derive len via
                // `array.len`.
                let list_expr = component.get_expr(*expr);
                self.emit_expr(func, list_expr, component)?;
                func.instruction(&Instruction::LocalTee(slot_local(
                    component, block,
                    *ref_result,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *len_result,
                    local_offset,
                )));
            }

            LirOp::ArrayGetItem {
                arr,
                idx,
                list_ty,
                repr,
            } => {
                let ty_idx = self
                    .record_gc_types
                    .list_array_type_idx
                    .get(list_ty)
                    .copied()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "ArrayGetItem: no list_array_type_idx for ty {:?}",
                            list_ty
                        ))
                    })?;
                let arr_l = slot_local(component, block, *arr, local_offset);
                let idx_l = slot_local(component, block, *idx, local_offset);
                // `array.get` of the element ref (or value for scalar lists).
                let get_elem = |func: &mut wasm_encoder::Function| {
                    func.instruction(&Instruction::LocalGet(arr_l));
                    func.instruction(&Instruction::LocalGet(idx_l));
                    func.instruction(&Instruction::ArrayGet(ty_idx));
                };
                // strings-to-GC: a `list<string>` element is a `$str_bytes`
                // ref, not a `$fat_value` box. Materialize it to canonical
                // (ptr, len) at the for-loop item boundary.
                let elem_is_gc_string =
                    matches!(self.ctx.ty_kind(*list_ty), InternedTyKind::List(e)
                        if matches!(self.ctx.ty_kind(*e), InternedTyKind::String));
                match repr {
                    ArrayItemRepr::Scalar { result } => {
                        get_elem(func);
                        func.instruction(&Instruction::LocalSet(slot_local(
                            component, block,
                            *result,
                            local_offset,
                        )));
                    }
                    ArrayItemRepr::Fat {
                        ptr_result,
                        len_result,
                    } if elem_is_gc_string => {
                        // Materialize str_bytes ref → (ptr, len) into slots.
                        let ptr_l = slot_local(component, block, *ptr_result, local_offset);
                        let len_l = slot_local(component, block, *len_result, local_offset);
                        get_elem(func);
                        self.emit_str_bytes_materialize(func)?;
                        func.instruction(&Instruction::LocalSet(len_l));
                        func.instruction(&Instruction::LocalSet(ptr_l));
                    }
                    ArrayItemRepr::FatToMem { buf_addr } if elem_is_gc_string => {
                        // Materialize str_bytes ref → (ptr, len), write to
                        // memory at buf+0/+4 via two i32 scratch temps.
                        let buf_l = slot_local(component, block, *buf_addr, local_offset);
                        let scratch = self.current_flat_scratch.unwrap_or_default();
                        let s_ptr = scratch.i32_base;
                        let s_len = scratch.i32_base + 1;
                        get_elem(func);
                        self.emit_str_bytes_materialize(func)?;
                        func.instruction(&Instruction::LocalSet(s_len));
                        func.instruction(&Instruction::LocalSet(s_ptr));
                        func.instruction(&Instruction::LocalGet(buf_l));
                        func.instruction(&Instruction::LocalGet(s_ptr));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        func.instruction(&Instruction::LocalGet(buf_l));
                        func.instruction(&Instruction::LocalGet(s_len));
                        func.instruction(&Instruction::I32Store(mem_arg(4, 2)));
                    }
                    ArrayItemRepr::Fat { .. } | ArrayItemRepr::FatToMem { .. } => {
                        // A Fat / FatToMem repr for a non-string element
                        // would mean the element is `$fat_value`-boxed, but
                        // every list element is now a scalar, a typed GC ref,
                        // or a `$str_bytes` string (handled by the guarded
                        // arms above). Nothing boxes into `$fat_value`.
                        unreachable!(
                            "ArrayGetItem: Fat/FatToMem repr for non-string element — every \
                             list element is a scalar, typed GC ref, or $str_bytes string; \
                             nothing boxes into $fat_value"
                        );
                    }
                }
            }
            LirOp::RefCast {
                from,
                ty_ref,
                result,
            } => {
                let ty_idx = self.resolve_lir_type_ref(comp_idx, *ty_ref)?;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *from,
                    local_offset,
                )));
                func.instruction(&Instruction::RefCastNullable(
                    wasm_encoder::HeapType::Concrete(ty_idx),
                ));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
            LirOp::RefIsNull { from, result } => {
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *from,
                    local_offset,
                )));
                func.instruction(&Instruction::RefIsNull);
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
            LirOp::ArrayGetTyped {
                ty_ref,
                arr,
                idx,
                result,
            } => {
                let ty_idx = self.resolve_lir_type_ref(comp_idx, *ty_ref)?;
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *arr,
                    local_offset,
                )));
                func.instruction(&Instruction::LocalGet(slot_local(
                    component, block,
                    *idx,
                    local_offset,
                )));
                func.instruction(&Instruction::ArrayGet(ty_idx));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component, block,
                    *result,
                    local_offset,
                )));
            }
        }
        Ok(())
    }

    /// Find the index of a component in `self.components` by its
    /// `DefId`. Used by [`Self::resolve_lir_type_ref`] to route
    /// `LirTypeRef::OtherComponentStruct` lookups into the matching
    /// `gc_layouts` entry. Returns an `InvalidIR` error (rather than
    /// silently 0) when the def-id doesn't name a known component,
    /// per the no-silent-fallback rule.
    pub(crate) fn comp_idx_by_def_id(&self, def_id: DefId) -> Result<usize, CodegenError> {
        self.components
            .iter()
            .position(|c| c.def_id == def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "LirTypeRef::OtherComponentStruct({}) does not name any component in this \
                     resource set",
                    def_id.0
                ))
            })
    }

    /// Resolve the wasm struct-type-section index for a generic
    /// `StructGet` / `StructSet` / `StructSetConst` op from its `rec`
    /// slot's `val_ty`. These ops carry no explicit type — the ref's
    /// declared val_ty names the struct: `RefNullForBoundary` resolves
    /// via the tree-struct map, `RefNullForComponent` via the component
    /// struct, `RefNull` already carries the concrete index. Anything
    /// else is malformed IR (no silent fallback).
    fn struct_ty_idx_from_rec(
        &self,
        comp_idx: usize,
        component: &LirResource,
        block: &LirBlock,
        rec: LirSlotId,
    ) -> Result<u32, CodegenError> {
        match slot_info(rec, block, component).val_ty {
            LirSlotValType::RefNullForBoundary(b) => self.gc_layouts[comp_idx]
                .tree_struct_type_idx
                .get(&b)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "StructGet/Set rec {rec:?}: no tree_struct_type_idx for boundary {b:?}"
                    ))
                }),
            LirSlotValType::RefNullForComponent(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                self.gc_layouts[j].component_struct_type_idx.ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "StructGet/Set rec {rec:?}: component {def_id:?} has no struct type index"
                    ))
                })
            }
            LirSlotValType::RefNull(ty_idx) => Ok(ty_idx),
            other => Err(CodegenError::InvalidIR(format!(
                "StructGet/Set rec {rec:?} has non-struct val_ty {other:?}"
            ))),
        }
    }

    /// Resolve a [`LirTypeRef`] to the concrete wasm type-section
    /// index codegen needs for `struct.new` / `struct.get` /
    /// `struct.set`. Backs the symbolic-ty `LirOp::StructNewSym` /
    /// `StructGetSym` / `StructSetSym` arms added in Phase 0.2.
    ///
    /// Missing layouts produce a descriptive `InternalError` —
    /// silent fallbacks would push a wrong type index and corrupt
    /// the wasm output.
    pub(crate) fn resolve_lir_type_ref(
        &self,
        comp_idx: usize,
        ty_ref: LirTypeRef,
    ) -> Result<u32, CodegenError> {
        match ty_ref {
            LirTypeRef::ComponentStruct => self.gc_layouts[comp_idx]
                .component_struct_type_idx
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirTypeRef::ComponentStruct: component_struct_type_idx not yet \
                         allocated for comp_idx {}",
                        comp_idx
                    ))
                }),
            LirTypeRef::OtherComponentStruct(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                self.gc_layouts[j].component_struct_type_idx.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirTypeRef::OtherComponentStruct({}): component_struct_type_idx not \
                         yet allocated for child comp_idx {}",
                        def_id.0, j
                    ))
                })
            }
            LirTypeRef::TreeBoundary(id) => self.gc_layouts[comp_idx]
                .tree_struct_type_idx
                .get(&id)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirTypeRef::TreeBoundary({:?}): not registered in \
                         gc_layouts[{}].tree_struct_type_idx",
                        id, comp_idx
                    ))
                }),
            LirTypeRef::ForChildrenArray(id) => self.gc_layouts[comp_idx]
                .tree_for_arr_type_idx
                .get(&id)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirTypeRef::ForChildrenArray({:?}): not registered in \
                         gc_layouts[{}].tree_for_arr_type_idx",
                        id, comp_idx
                    ))
                }),
            LirTypeRef::StructDecl(idx) => {
                // Phase 0.3 will wire `LirResource::struct_types` →
                // wasm-type-section-idx resolution. Phase 0.2
                // intentionally traps so the first caller surfaces a
                // clear lifecycle error rather than the wrong idx.
                Err(CodegenError::InternalError(format!(
                    "LirTypeRef::StructDecl({:?}) resolution not yet wired (Phase 0.3 task)",
                    idx
                )))
            }
            LirTypeRef::ArrayDecl(idx) => Err(CodegenError::InternalError(format!(
                "LirTypeRef::ArrayDecl({:?}) resolution not yet wired (Phase 0.3 task)",
                idx
            ))),
            LirTypeRef::SharedHandleStruct => self.shared_handle_type_idx.ok_or_else(|| {
                CodegenError::InternalError(
                    "LirTypeRef::SharedHandleStruct: shared_handle_type_idx not yet emitted by \
                     emit_shared_handle_types"
                        .into(),
                )
            }),
            LirTypeRef::SharedHandleArray => self.shared_handle_arr_type_idx.ok_or_else(|| {
                CodegenError::InternalError(
                    "LirTypeRef::SharedHandleArray: shared_handle_arr_type_idx not yet emitted \
                     by emit_shared_handle_types"
                        .into(),
                )
            }),
            LirTypeRef::GcVariantCase(ty, case_idx) => self
                .record_gc_types
                .gc_variant_case_idx
                .get(&(ty, case_idx))
                .copied()
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirTypeRef::GcVariantCase({:?}, {}): missing gc_variant_case_idx entry",
                        ty, case_idx
                    ))
                }),
            LirTypeRef::TupleStruct(ty) => self
                .record_gc_types
                .tuple_struct_type_idx
                .get(&ty)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirTypeRef::TupleStruct({:?}): missing tuple_struct_type_idx entry",
                        ty
                    ))
                }),
        }
    }

    /// Phase 0.3c: resolve a [`LirGlobalRef`] to the concrete wasm
    /// global index codegen needs for `global.get` / `global.set`.
    /// Backs the symbolic `LirOp::GlobalGet` / `GlobalSet` arms.
    ///
    /// Missing globals produce a descriptive `InternalError` — silent
    /// fallbacks would push a wrong global index and corrupt wasm
    /// output (per yel-wasm-codegen's CLAUDE.md).
    pub(crate) fn resolve_lir_global_ref(&self, gref: LirGlobalRef) -> Result<u32, CodegenError> {
        match gref {
            LirGlobalRef::CurrentHandle(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                self.gc_layouts[j].current_handle_global.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirGlobalRef::CurrentHandle({}): current_handle_global not yet \
                         allocated for comp_idx {}",
                        def_id.0, j
                    ))
                })
            }
            LirGlobalRef::Registry(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                self.gc_layouts[j].registry_global.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirGlobalRef::Registry({}): registry_global not yet allocated for \
                         comp_idx {}",
                        def_id.0, j
                    ))
                })
            }
            LirGlobalRef::RegistryLen(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                self.gc_layouts[j].registry_len_global.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirGlobalRef::RegistryLen({}): registry_len_global not yet allocated \
                         for comp_idx {}",
                        def_id.0, j
                    ))
                })
            }
            LirGlobalRef::RegistryFreeHead(def_id) => {
                let j = self.comp_idx_by_def_id(def_id)?;
                self.gc_layouts[j].registry_free_head_global.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "LirGlobalRef::RegistryFreeHead({}): registry_free_head_global not yet \
                         allocated for comp_idx {}",
                        def_id.0, j
                    ))
                })
            }
        }
    }
}
