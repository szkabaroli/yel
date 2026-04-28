//! Per-block function emission. Each non-mount block in a component
//! becomes a standalone WASM function with a calling convention that
//! matches its declared LIR params + the implicit `(ref null $Comp)`
//! self ref. Methods live on `WasmPackageBuilder<'a>` via an additional
//! impl block.

use std::collections::HashMap;

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::lir::{LirComponent, LirSlotKind, LirSlotValType};
use yel_core::ids::BlockId;

use super::super::CodegenError;
use super::super::WasmPackageBuilder;
use super::scratch::{push_valtype_locals, slot_local};

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn generate_block_function(
        &mut self,
        comp_idx: usize,
        block_id: BlockId,
    ) -> Result<Function, CodegenError> {
        // Fresh label tracking per block function — see the
        // matching reset in `generate_component_mount`.
        self.current_function_labels.clear();
        self.current_label_counter = 0;
        let component: &'a LirComponent = &self.components[comp_idx];
        let block = component.get_block(block_id);

        // Param count comes from `block.params` when set (for-item-mount,
        // for-item-unmount, if-branch mount/unmount, else-if mount);
        // defaults to 1 for legacy blocks (update/handler/etc.) whose
        // callers always pass one i32.
        //
        // Step 4: every block additionally takes `(ref null $Comp)`
        // at WASM param 0 as the implicit self-ref; the LIR-level
        // param_count below excludes it. WASM param indices are thus
        // `0` (self ref) + `1..1+lir_param_count` (LIR params).
        // Legacy fallback: when a block has no explicit `params` AND
        // no `boundary_params`, it uses the fixed `block_1param_type_idx`
        // signature with 1 implicit i32 param. With boundary_params
        // present, the function uses a dynamic type and the actual
        // LIR i32 param count is whatever `block.params.len()` is.
        let lir_param_count: u32 = if !block.params.is_empty() {
            block.params.len() as u32
        } else if block.boundary_params.is_empty() {
            1
        } else {
            0
        };
        // Pre-step: blocks that opt into dynamic per-block function
        // types declare boundary-ref params after the legacy i32 params.
        // These count as additional WASM params (no slot copy — the
        // ref lives directly in the WASM param local and is registered
        // in `current_boundary_locals` for `BoundaryField` accesses).
        let boundary_param_count: u32 = block.boundary_params.len() as u32;
        let param_count: u32 = lir_param_count + boundary_param_count + 1;

        // Gather Temp slots ordered by compacted `local_idx`. Memory slots
        // never become WASM locals. One local is declared per Temp slot;
        // Temp slots reserved as block params are covered here (they're
        // allocated just like any other temp — codegen copies the WASM
        // param into the matching slot-local below).
        let mut temp_slots: Vec<(u32, &yel_core::lir::LirSlotInfo)> = component
            .slots
            .iter()
            .filter_map(|s| {
                if let LirSlotKind::Temp { local_idx } = s.kind {
                    Some((local_idx, s))
                } else {
                    None
                }
            })
            .collect();
        temp_slots.sort_by_key(|(idx, _)| *idx);
        let num_slots = temp_slots.len() as u32;

        // If this block contains InitSignal / SignalWriteExpr ops with
        // composite signal types (Option/Result/Variant-with-payload), the
        // flat-slot store path needs per-valtype scratch locals beyond the
        // block's declared slots.
        let (max_i32_scratch, max_i64_scratch, max_f32_scratch, max_f64_scratch) =
            block.max_flat_scratch_counts;

        // Declare one local per Temp slot in compacted `local_idx` order.
        let mut locals = Vec::new();
        for (_, s) in &temp_slots {
            let val_ty = match s.val_ty {
                LirSlotValType::I32 => ValType::I32,
                LirSlotValType::I64 => ValType::I64,
                LirSlotValType::F32 => ValType::F32,
                LirSlotValType::F64 => ValType::F64,
                LirSlotValType::RefNull(ty_idx) => ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                }),
                LirSlotValType::RefNullForBoundary(boundary_id) => {
                    let ty_idx = self.gc_layouts[comp_idx].tree_struct_type_idx[&boundary_id];
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                    })
                }
                LirSlotValType::RefNullForChildrenArray(anchor_id) => {
                    let ty_idx = self.gc_layouts[comp_idx].tree_for_arr_type_idx[&anchor_id];
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                    })
                }
                LirSlotValType::RefNullForListGc(list_ty) => {
                    let ty_idx = *self
                        .record_gc_types
                        .list_array_type_idx
                        .get(&list_ty)
                        .ok_or_else(|| {
                            CodegenError::InternalError(format!(
                                "block_fn local: missing list_array_type_idx for {:?}",
                                list_ty
                            ))
                        })?;
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                    })
                }
                LirSlotValType::RefNullForRecord(record_ty) => {
                    use yel_core::types::InternedTyKind;
                    let def_id = match self.ctx.ty_kind(record_ty) {
                        InternedTyKind::Adt(d) => *d,
                        _ => {
                            return Err(CodegenError::InternalError(format!(
                                "block_fn local: RefNullForRecord on non-Adt {:?}",
                                record_ty
                            )));
                        }
                    };
                    let ty_idx = *self
                        .record_gc_types
                        .record_type_idx
                        .get(&def_id)
                        .ok_or_else(|| {
                            CodegenError::InternalError(format!(
                                "block_fn local: missing record_type_idx for {:?}",
                                def_id
                            ))
                        })?;
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                    })
                }
            };
            locals.push((1, val_ty));
        }
        // Append per-valtype scratch locals for flat-slot signal stores.
        push_valtype_locals(
            &mut locals,
            (
                max_i32_scratch,
                max_i64_scratch,
                max_f32_scratch,
                max_f64_scratch,
            ),
        );

        // One typed `(ref null $Comp_<child>)` scratch local per
        // distinct child component reachable from a `MountComponent`
        // op in this block. Matches what the mount-internal emitter
        // does for the top-level mount block — block functions can
        // contain `MountComponent` ops too (notably for-item-mount
        // blocks lowered from `<For>` with element children).
        let block_child_indices: Vec<usize> = block
            .mount_component_children
            .iter()
            .filter_map(|def_id| self.components.iter().position(|c| c.def_id == *def_id))
            .collect();
        let mut block_mount_child_locals: HashMap<usize, u32> = HashMap::new();
        let mut block_mount_child_alloc_idx_locals: HashMap<usize, u32> = HashMap::new();
        let mut block_mount_child_alloc_arr_locals: HashMap<usize, u32> = HashMap::new();
        let scratch_total = max_i32_scratch + max_i64_scratch + max_f32_scratch + max_f64_scratch;
        let mut next_block_local = param_count + num_slots + scratch_total;
        for &child_idx in &block_child_indices {
            let child_struct_ty = self.gc_layouts[child_idx]
                .component_struct_type_idx
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "block fn: child component {} missing component_struct_type_idx",
                        child_idx
                    ))
                })?;
            locals.push((
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(child_struct_ty),
                }),
            ));
            block_mount_child_locals.insert(child_idx, next_block_local);
            next_block_local += 1;
            // Mirror mount-internal: reserve i32 idx + typed handle-array
            // scratch locals so MountComponent's emit_registry_alloc has
            // somewhere to write its scratch.
            locals.push((1, ValType::I32));
            block_mount_child_alloc_idx_locals.insert(child_idx, next_block_local);
            next_block_local += 1;
            let _ = child_idx;
            let child_handle_arr_ty = self.shared_handle_arr_type_idx.ok_or_else(|| {
                CodegenError::InternalError("block fn: shared_handle_arr_type_idx not set".into())
            })?;
            locals.push((
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(child_handle_arr_ty),
                }),
            ));
            block_mount_child_alloc_arr_locals.insert(child_idx, next_block_local);
            next_block_local += 1;
        }

        let mut func = Function::new(locals);

        // `local_offset` is the base index into the locals array for
        // slot 0; each LocalGet/LocalSet on a SlotId adds this offset.
        let local_offset = param_count;
        // Copy params into their respective slot locals so subsequent
        // ops that reference a param slot hit freshly-initialised
        // locals. Each param slot is allocated fresh by block_lower
        // (see `LirBlock.params`); here we copy WASM param local `i`
        // into the slot-local at `local_offset + slot_local(component, *slot)`.
        // Param 0 is the implicit self ref — recorded as
        // `current_self_local` so signal struct.get/set ops inside
        // this block source `self` from it.
        let prev_self_local = self.current_self_local;
        let prev_self_comp_idx = self.current_self_comp_idx;
        // Boundary locals are per-function: each emitted block enters
        // with no inner boundaries in scope (the only boundary it can
        // reach for free is the component root via `$self.tree`).
        // Mount blocks may register inner boundaries via
        // `AllocSubBoundary`, but those locals belong to *this*
        // function's frame, so we must not leak them across blocks.
        let prev_boundary_locals = std::mem::take(&mut self.current_boundary_locals);
        self.current_self_local = Some(0);
        self.current_self_comp_idx = Some(comp_idx);
        if block.params.is_empty() {
            if block.boundary_params.is_empty() {
                // Legacy default: 1 implicit i32 LIR param at WASM
                // local 1 maps to slot 0. Preserves behaviour for
                // update/handler/etc. blocks that use the fixed
                // `block_1param_type_idx` signature.
                func.instruction(&Instruction::LocalGet(1));
                func.instruction(&Instruction::LocalSet(local_offset));
            }
            // Else: this block uses a dynamic type with no LIR i32
            // params and only boundary-ref params. WASM param 1 is
            // already a typed boundary ref — leave it in its WASM
            // param local (it's registered in
            // `current_boundary_locals` below); don't try to copy
            // into a non-existent slot.
        } else {
            for (i, param_slot) in block.params.iter().enumerate() {
                // WASM param i+1 (skipping self ref at 0) → slot.
                func.instruction(&Instruction::LocalGet((i as u32) + 1));
                func.instruction(&Instruction::LocalSet(
                    local_offset + slot_local(component, *param_slot),
                ));
            }
        }

        // Pre-step: register every boundary-ref param's WASM local in
        // `current_boundary_locals` so subsequent `BoundaryField` slot
        // accesses on those boundaries resolve to a `local.get` on the
        // param. WASM params: [0]=self, [1..1+lir_param_count]=i32 args,
        // [1+lir_param_count..]=boundary refs (in `block.boundary_params`
        // order).
        for (i, b_id) in block.boundary_params.iter().enumerate() {
            let local = 1 + lir_param_count + (i as u32);
            self.current_boundary_locals.insert(*b_id, local);
        }

        // Set captured locals, local_to_slot, and local_offset for expression emission
        self.current_block_local_offset = Some(local_offset);
        let has_captures = !block.captured_locals.is_empty();
        if has_captures {
            // Resolve each captured SlotId → absolute WASM local index now.
            // emit_expr treats the map as raw absolute indices so it can be
            // shared with the filter-closure path (which reserves raw locals
            // directly instead of going through SlotId).
            let mut resolved = HashMap::with_capacity(block.captured_locals.len());
            for (local_id, slot) in &block.captured_locals {
                resolved.insert(*local_id, slot_local(component, *slot) + local_offset);
            }
            self.current_block_captured_locals = Some(resolved);
        }
        if !block.local_to_slot.is_empty() {
            self.current_block_local_to_slot = Some(block.local_to_slot.clone());
        }
        // Phase 5b-v.2: thread the block's per-LocalId binding-mode map
        // into emission state. The `Local` expr arm consults this to
        // decide whether to follow `local.get` with a typed load (`Ptr`,
        // today's behavior) or leave the value on the stack as-is
        // (`Value`, populated in 5b-v.3 for migrated-list iter bindings).
        if !block.local_modes.is_empty() {
            self.current_block_local_modes = Some(block.local_modes.clone());
        }
        // Scratch locals for flat-slot signal stores live past params + slots.
        let scratch_base = local_offset + num_slots;
        if max_i32_scratch + max_i64_scratch + max_f32_scratch + max_f64_scratch > 0 {
            self.current_init_scratch_start = Some(scratch_base);
        }
        let block_scratch = crate::wasm::FlatScratchBases {
            i32_base: scratch_base,
            i32_count: max_i32_scratch,
            i64_base: scratch_base + max_i32_scratch,
            i64_count: max_i64_scratch,
            f32_base: scratch_base + max_i32_scratch + max_i64_scratch,
            f32_count: max_f32_scratch,
            f64_base: scratch_base + max_i32_scratch + max_i64_scratch + max_f32_scratch,
            f64_count: max_f64_scratch,
        };
        self.current_flat_scratch = Some(block_scratch);

        // Per-child typed scratch locals for any `MountComponent` ops
        // in this block. Set/restore around the body emission so
        // nested block emissions (none today, but defensive) don't
        // accidentally inherit a stale mapping.
        let prev_mount_child_locals = self.current_mount_child_locals.take();
        let prev_mount_child_alloc_idx_locals = self.current_mount_child_alloc_idx_locals.take();
        let prev_mount_child_alloc_arr_locals = self.current_mount_child_alloc_arr_locals.take();
        if !block_mount_child_locals.is_empty() {
            self.current_mount_child_locals = Some(block_mount_child_locals);
            self.current_mount_child_alloc_idx_locals = Some(block_mount_child_alloc_idx_locals);
            self.current_mount_child_alloc_arr_locals = Some(block_mount_child_alloc_arr_locals);
        }

        // Emit block operations
        for op in &block.ops {
            self.emit_op(&mut func, op, comp_idx, local_offset)?;
        }

        // Clear captured locals, local_to_slot, and local_offset
        self.current_block_captured_locals = None;
        self.current_block_local_to_slot = None;
        self.current_block_local_modes = None;
        self.current_block_local_offset = None;
        self.current_init_scratch_start = None;
        self.current_flat_scratch = None;
        self.current_self_local = prev_self_local;
        self.current_self_comp_idx = prev_self_comp_idx;
        self.current_boundary_locals = prev_boundary_locals;
        self.current_mount_child_locals = prev_mount_child_locals;
        self.current_mount_child_alloc_idx_locals = prev_mount_child_alloc_idx_locals;
        self.current_mount_child_alloc_arr_locals = prev_mount_child_alloc_arr_locals;

        // If the block is declared to return i32, push its designated
        // return slot as the final expression before `End`. The slot
        // holds the root DOM handle the caller records in its tracking
        // array for later diff / unmount.
        if let Some(slot) = block.return_slot {
            func.instruction(&Instruction::LocalGet(
                slot_local(component, slot) + local_offset,
            ));
        }

        func.instruction(&Instruction::End);
        // Stash collected structural-op label names under this block's
        // WASM function index for the name section to surface.
        if !self.current_function_labels.is_empty()
            && let Some(&wasm_func_idx) = self.block_func_indices.get(&(comp_idx, block_id))
        {
            let labels = std::mem::take(&mut self.current_function_labels);
            self.function_label_names.insert(wasm_func_idx, labels);
        }
        Ok(func)
    }
}
