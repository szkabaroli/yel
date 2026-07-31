//! Per-block function emission. Each non-mount block in a component
//! becomes a standalone WASM function with a calling convention that
//! matches its declared LIR params + the implicit `(ref null $Comp)`
//! self ref. Methods live on `WasmPackageBuilder<'a>` via an additional
//! impl block.

use rustc_hash::FxHashMap as HashMap;

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::ids::BlockId;
use yel_core::lir::arena::LirResourceArena;
use yel_core::lir::{LirBlock, LirResource, LirSlotKind};

use super::super::CodegenError;
use super::super::WasmPackageBuilder;
use super::scratch::{push_valtype_locals, slot_info, slot_local};

/// Whether `block_id` is one of `component`'s lifecycle / export-wrapper
/// blocks — which use only their declared `params` (no legacy 1-i32 fallback).
/// Reads the component's lifecycle block-ids, which are UI-only fields on the
/// concrete `LirResource`, so it is computed by the caller and passed into the
/// otherwise arena-generic `generate_block_function`.
pub(super) fn block_is_lifecycle(component: &LirResource, block_id: BlockId) -> bool {
    Some(block_id) == component.internal_constructor_block
        || block_id == component.mount_block
        || Some(block_id) == component.internal_unmount_block
        || Some(block_id) == component.export_constructor_block
        || Some(block_id) == component.export_mount_block
        || Some(block_id) == component.export_unmount_block
}

impl<'a> WasmPackageBuilder<'a> {
    /// Emit one block as a standalone wasm function. Arena-generic over the
    /// owning scope: a `LirResource` for a component block (`comp_idx` =
    /// `Some`), or a `ModuleScope` for the module-start globals-init block
    /// (`comp_idx` = `None`). `is_lifecycle` is the caller-computed
    /// resource-only signal (see [`block_is_lifecycle`]); module scope passes
    /// `false` (its block is paramless).
    pub(super) fn generate_block_function(
        &mut self,
        component: &dyn LirResourceArena,
        comp_idx: Option<usize>,
        block: &LirBlock,
        is_lifecycle: bool,
    ) -> Result<Function, CodegenError> {
        // Fresh label tracking per block function — see the
        // matching reset in `generate_component_mount`.
        self.current_function_labels.clear();
        self.current_label_counter = 0;
        let block_id = block.id;

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
        // Stage 5c: read length from `boundary_param_slots`. Each
        // slot's val_ty carries the boundary id; the count matches
        // `boundary_params` by Stage 4 invariant.
        let boundary_param_count: u32 = block.boundary_param_slots.len() as u32;
        // Phase 0.3o: wasm-sig shape is derived uniformly from
        // `block.params`, `block.boundary_param_slots`, and
        // `block.implicit_self`'s slot kind. Lifecycle blocks
        // (ctor/mount/unmount) and user blocks share the same
        // prologue paths — their differences (Temp self vs.
        // WasmParam self, return slot type, legacy-i32 fallback)
        // are all expressible through these fields.
        //
        //   * `self_ref_param_count` is 1 iff `implicit_self` is
        //     `Some` and resolves to a `WasmParam`-backed slot. That
        //     covers mount/unmount and every user block (which all
        //     receive `(ref $Comp)` at wasm local 0). Ctor's
        //     `implicit_self` is a `Temp` slot (allocated by the
        //     body's `StructNewDefaultSym`), so it contributes 0.
        //   * `lir_param_count` is exactly `block.params.len()`.
        //     The legacy "1 implicit i32" fallback fires *only*
        //     when neither user params nor boundary params nor a
        //     lifecycle-shape sig apply — we identify the lifecycle
        //     blocks via `LirResource`'s identity fields (ctor /
        //     mount / unmount block ids), keeping the check
        //     field-driven rather than flag-driven.
        // Phase 0.3m: export-wrapper blocks (host-facing constructor /
        // mount / unmount) join the lifecycle exemption: they have no
        // implicit self ref (`implicit_self: None`) and use only their
        // declared `params`. The legacy-i32 fallback below is a relic
        // for update/handler blocks that pre-date `params`; the export
        // wrappers must opt out so `lir_param_count` matches their
        // declared signature.
        let self_ref_param_count: u32 = match block.implicit_self {
            Some(slot) => match slot_info(slot, block, component).kind {
                LirSlotKind::WasmParam { .. } => 1,
                _ => 0,
            },
            None => 0,
        };
        let lir_param_count: u32 = if !block.params.is_empty() {
            block.params.len() as u32
        } else if boundary_param_count == 0 && !is_lifecycle && comp_idx.is_some() {
            // Legacy "1 implicit i32" fallback — a relic for component
            // update/handler blocks that pre-date explicit `params`. It never
            // applies in module scope (the globals-init block is paramless).
            1
        } else {
            0
        };
        let param_count: u32 = lir_param_count + boundary_param_count + self_ref_param_count;

        // Count Temp slots up-front for later sizing computations
        // (parent-retention region, mount-child scratch locals, etc.).
        // The slot-locals themselves are declared via the shared
        // L3-v2 Phase 2 helper — same helper the non-UI
        // `wasm::functions::emit_function` calls, so both paths
        // produce byte-identical slot orderings + GC val-type
        // resolution.
        // Task #105 B2: Temp slots may live on the component (Resource
        // variant, allocated outside block context) OR on the block
        // itself (Block variant, allocated inside this block). Both
        // need wasm locals declared.
        let num_resource_slots = component
            .slots()
            .iter()
            .filter(|s| matches!(s.kind, LirSlotKind::Temp { .. }))
            .count() as u32;
        let num_block_slots = block
            .slots
            .iter()
            .filter(|s| matches!(s.kind, LirSlotKind::Temp { .. }))
            .count() as u32;
        let num_slots = num_resource_slots + num_block_slots;

        // If this block contains InitSignal / SignalWriteExpr ops with
        // composite signal types (Option/Result/Variant-with-payload), the
        // flat-slot store path needs per-valtype scratch locals beyond the
        // block's declared slots.
        let (max_i32_scratch, max_i64_scratch, max_f32_scratch, max_f64_scratch) =
            block.max_flat_scratch_counts;

        let comp_layout = comp_idx
            .map(|ci| self.gc_layouts[ci].clone())
            .unwrap_or_default();
        // `skip_params: 0` — UI's convention is that every Temp slot
        // (including the ones that back wasm-level params) gets its
        // own wasm local; the prologue copies wasm params into the
        // matching slot-local. Flow uses `skip_params: param_count`
        // because its convention is "Temp slot 0..N IS the wasm
        // param at local 0..N".
        // Task #105 B2: declare Resource-Temp locals first (sorted by
        // their component-wide local_idx), then Block-Temp locals
        // (sorted by per-block local_idx). slot_local's offset math
        // mirrors this order: Block Temps add `num_resource_slots` to
        // their local_idx.
        let mut locals = self.declare_function_locals(component.slots(), 0, &comp_layout)?;
        let block_locals = self.declare_function_locals(&block.slots, 0, &comp_layout)?;
        locals.extend(block_locals);

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
        let mut block_mount_child_locals: HashMap<usize, u32> = HashMap::default();
        let mut block_mount_child_alloc_idx_locals: HashMap<usize, u32> = HashMap::default();
        let mut block_mount_child_alloc_arr_locals: HashMap<usize, u32> = HashMap::default();
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

        // One typed `(ref null $idx)` scratch local per distinct GC
        // struct/ref type used by a composite callback argument in this
        // block. A callback invoked with a composite arg pushes the arg's
        // canonical-ABI flattening directly to the stack; the local holds the
        // arg's GC ref so the lowering can re-read it (per gc-variant case
        // test, per record member). Scalars / lists / strings are handled
        // inline in `emit_callback_arg` and skipped here.
        let mut block_cb_arg_ref_locals: HashMap<u32, u32> = HashMap::default();
        for &ty in &block.callback_arg_composite_types {
            let gc_type_idx = match self.internal_repr(ty) {
                crate::wasm::repr::InternalRepr::GcRef(idx)
                | crate::wasm::repr::InternalRepr::GcVariant(idx) => idx,
                _ => continue,
            };
            if block_cb_arg_ref_locals.contains_key(&gc_type_idx) {
                continue;
            }
            locals.push((
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(gc_type_idx),
                }),
            ));
            block_cb_arg_ref_locals.insert(gc_type_idx, next_block_local);
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
        // into the slot-local at `slot_local(component, *slot, local_offset)`.
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
        // Phase 0.3o: `current_self_local` is `block.implicit_self`
        // resolved through `slot_local`. `None` leaves the ambient
        // self-local unset (host export wrappers, flow free funcs).
        // No flag-based gating.
        match block.implicit_self {
            Some(slot) => {
                self.current_self_local = Some(slot_local(component, block, slot, local_offset));
                self.current_self_comp_idx = comp_idx;
            }
            None => {
                self.current_self_local = None;
                self.current_self_comp_idx = None;
            }
        }
        // Reset the parent-retention cursor for the lifecycle bodies
        // (ctor + mount each consume retention slots from index 0).
        // Field-driven via the component's identity fields.
        if is_lifecycle {
            if let Some(ci) = comp_idx {
                self.parent_retention_cursor.insert(ci, 0);
            }
        }
        // Phase 0.3o: user params land at wasm index
        // `self_ref_param_count + i`.
        let user_param_base: u32 = self_ref_param_count;
        if !is_lifecycle && block.params.is_empty() && block.implicit_self.is_some() {
            if block.boundary_param_slots.is_empty() {
                // Legacy default: 1 implicit i32 LIR param at WASM
                // local 1 maps to slot 0. Preserves behaviour for
                // update/handler/etc. blocks that use the fixed
                // `block_1param_type_idx` signature.
                //
                // F32/F64/I64 inline regression guard: this prologue
                // assumes the slot at local_idx 0 is i32 (the legacy
                // "parent" placeholder). The unified inline-signal
                // helper can allocate scalar scratches at local_idx 0
                // first; when that scratch is not i32, copying an i32
                // wasm param into it fails wasm validation. Skip the
                // copy in that case — the slot will be initialized by
                // its own emit op before any read.
                // The first temp local (at `local_offset`) is the first
                // Resource Temp when the component has any, else the first
                // Block Temp (they're declared right after, so both occupy
                // `local_offset + 0`). With no temps at all there is no
                // local to copy into — skip (Task #105 B2: synth-pass temps
                // moved onto their own blocks, so zero-resource-temp
                // components exist now).
                let first_temp_valtype = |slots: &[yel_core::lir::LirSlotInfo]| {
                    slots
                        .iter()
                        .filter_map(|s| match s.kind {
                            LirSlotKind::Temp { local_idx } => Some((local_idx, s.val_ty)),
                            _ => None,
                        })
                        .min_by_key(|(idx, _)| *idx)
                        .map(|(_, vt)| vt)
                };
                let first_temp_is_i32 = first_temp_valtype(component.slots())
                    .or_else(|| first_temp_valtype(&block.slots))
                    .map(|vt| matches!(vt, yel_core::lir::LirSlotValType::I32))
                    .unwrap_or(false);
                if first_temp_is_i32 {
                    func.instruction(&Instruction::LocalGet(1));
                    func.instruction(&Instruction::LocalSet(local_offset));
                }
            }
            // Else: this block uses a dynamic type with no LIR i32
            // params and only boundary-ref params. WASM param 1 is
            // already a typed boundary ref — leave it in its WASM
            // param local (it's registered in
            // `current_boundary_locals` below); don't try to copy
            // into a non-existent slot.
        } else {
            for (i, param_slot) in block.params.iter().enumerate() {
                // WASM param `user_param_base + i` → slot.
                func.instruction(&Instruction::LocalGet(user_param_base + (i as u32)));
                func.instruction(&Instruction::LocalSet(slot_local(
                    component,
                    block,
                    *param_slot,
                    local_offset,
                )));
            }
        }

        // Pre-step: register every boundary-ref param's WASM local in
        // `current_boundary_locals` so subsequent `BoundaryField` slot
        // accesses on those boundaries resolve to a `local.get` on the
        // param. WASM params: [0]=self, [1..1+lir_param_count]=i32 args,
        // [1+lir_param_count..]=boundary refs (in `block.boundary_params`
        // order).
        // Stage 5c: derive boundary-id list from `boundary_param_slots`
        // (slot val_ty carries the id) instead of reading `boundary_params`.
        let bp_ids: Vec<_> = block
            .boundary_param_ids_from_slots(component.slots())
            .collect();
        for (i, b_id) in bp_ids.iter().enumerate() {
            // Phase 0.3n: base is `self_ref_param_count` (0 for
            // no-self blocks, 1 for legacy self-bearing blocks).
            let wasm_local = self_ref_param_count + lir_param_count + (i as u32);
            self.current_boundary_locals.insert(*b_id, wasm_local);
            // Stage 4 of lir-resource-flatten: copy each boundary
            // param's WASM local into its parallel LIR slot
            // (`block.boundary_param_slots[i]`). This is what makes
            // StructGet / StructSet ops emitted by the boundary_rewrite
            // pass resolve to the same boundary ref the legacy
            // chain walk would have used. Costs one extra
            // `local.get; local.set` per boundary param at function
            // entry — wasm-opt's `--remove-unused-locals` /
            // `--simplify-locals` collapses these in the
            // `--release` pipeline.
            if let Some(slot_id) = block.boundary_param_slots.get(i) {
                let slot_wasm = slot_local(component, block, *slot_id, local_offset);
                func.instruction(&Instruction::LocalGet(wasm_local));
                func.instruction(&Instruction::LocalSet(slot_wasm));
            }
        }

        // Set captured locals, local_to_slot, and local_offset for expression emission
        self.current_block_local_offset = Some(local_offset);
        self.current_generated_block_id = Some(block.id);
        let has_captures = !block.captured_locals.is_empty();
        if has_captures {
            // Resolve each captured SlotId → absolute WASM local index now.
            // emit_expr treats the map as raw absolute indices so it can be
            // shared with the filter-closure path (which reserves raw locals
            // directly instead of going through SlotId).
            let mut resolved =
                HashMap::with_capacity_and_hasher(block.captured_locals.len(), Default::default());
            for (local_id, slot) in &block.captured_locals {
                resolved.insert(*local_id, slot_local(component, block, *slot, local_offset));
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

        // Composite callback-argument ref locals; set/restore around the body
        // emission like the mount-child locals above.
        let prev_cb_arg_ref_locals = self.current_cb_arg_ref_locals.take();
        if !block_cb_arg_ref_locals.is_empty() {
            self.current_cb_arg_ref_locals = Some(block_cb_arg_ref_locals);
        }

        // Emit block operations
        for op in &block.ops {
            self.emit_op(&mut func, op, component, comp_idx, block, local_offset)?;
        }

        // Clear captured locals, local_to_slot, and local_offset
        self.current_block_captured_locals = None;
        self.current_block_local_to_slot = None;
        self.current_block_local_modes = None;
        self.current_block_local_offset = None;
        self.current_generated_block_id = None;
        self.current_init_scratch_start = None;
        self.current_flat_scratch = None;
        self.current_self_local = prev_self_local;
        self.current_self_comp_idx = prev_self_comp_idx;
        self.current_boundary_locals = prev_boundary_locals;
        self.current_mount_child_locals = prev_mount_child_locals;
        self.current_mount_child_alloc_idx_locals = prev_mount_child_alloc_idx_locals;
        self.current_mount_child_alloc_arr_locals = prev_mount_child_alloc_arr_locals;
        self.current_cb_arg_ref_locals = prev_cb_arg_ref_locals;

        // If the block is declared to return i32, push its designated
        // return slot as the final expression before `End`. The slot
        // holds the root DOM handle the caller records in its tracking
        // array for later diff / unmount.
        if let Some(slot) = block.return_slot {
            func.instruction(&Instruction::LocalGet(slot_local(
                component,
                block,
                slot,
                local_offset,
            )));
        }

        func.instruction(&Instruction::End);
        // Stash collected structural-op label names under this block's
        // WASM function index for the name section to surface.
        if !self.current_function_labels.is_empty()
            && let Some(&wasm_func_idx) = self.block_func_indices.get(&block_id)
        {
            let labels = std::mem::take(&mut self.current_function_labels);
            self.function_label_names.insert(wasm_func_idx, labels);
        }
        Ok(func)
    }
}
