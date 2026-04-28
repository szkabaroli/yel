//! Component lifecycle function emission: constructor, mount, unmount —
//! both the exported wrappers and the internal-tier entry points. Each
//! method lives on `WasmPackageBuilder<'a>` via an additional impl block.

use std::collections::HashMap;

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::lir::{LirComponent, LirSlotKind, LirSlotValType};
use yel_core::types::InternedTyKind;

use super::super::CodegenError;
use super::super::{IMPORT_REMOVE, MemoryLayout, WasmPackageBuilder};
use super::scratch::{mem_arg, merge_max_slot_counts, push_valtype_locals, slot_local};

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn generate_constructor_internal_for(
        &mut self,
        comp_idx: usize,
    ) -> Result<Function, CodegenError> {
        let component: &'a LirComponent = &self.components[comp_idx];
        let layout = self
            .layouts
            .get(comp_idx)
            .cloned()
            .unwrap_or_else(MemoryLayout::empty_for_module);

        let mut max_counts: (u32, u32, u32, u32) = (0, 0, 0, 0);
        for s in &component.signals {
            if matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }) {
                continue;
            }
            let slots = self.flatten_core_slots(s.ty);
            merge_max_slot_counts(&mut max_counts, &slots);
        }
        let (max_i32, max_i64, max_f32, max_f64) = max_counts;

        // Locals layout (no params):
        //   0..max_i32                             : i32 scratch
        //   max_i32..max_i32+max_i64               : i64 scratch
        //   ... f32, f64 scratch ...
        //   self_ref_local                         : (ref null $Comp_<i>)
        let mut locals: Vec<(u32, ValType)> = Vec::new();
        push_valtype_locals(&mut locals, max_counts);
        let comp_struct_ty = self.gc_layouts[comp_idx]
            .component_struct_type_idx
            .ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "component {}: constructor_internal emission requires component_struct_type_idx",
                    comp_idx
                ))
            })?;
        locals.push((
            1,
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(comp_struct_ty),
            }),
        ));

        let mut func = Function::new(locals);
        let signal_temp_locals = max_i32;
        let ctor_scratch = crate::wasm::FlatScratchBases {
            i32_base: 0,
            i32_count: max_i32,
            i64_base: max_i32,
            i64_count: max_i64,
            f32_base: max_i32 + max_i64,
            f32_count: max_f32,
            f64_base: max_i32 + max_i64 + max_f32,
            f64_count: max_f64,
        };
        let self_ref_local = max_i32 + max_i64 + max_f32 + max_f64;

        // Allocate the per-component GC instance into `$self_ref`.
        // Internal entry — no singleton mirror, no registry handle.
        func.instruction(&Instruction::StructNewDefault(comp_struct_ty));
        func.instruction(&Instruction::LocalSet(self_ref_local));

        // Allocate the typed mount-tree root and store it on `$Comp.tree`.
        // Inner boundaries (if-anchors, if-branches, for-anchors,
        // for-iter-bodies) are NOT pre-allocated here — mount/iter
        // emission allocates them in tree order and threads their
        // typed refs as function parameters down to inner emit
        // scopes (branch mounts, iter mounts, update blocks).
        let gc_for_tree = &self.gc_layouts[comp_idx];
        if let (Some(tree_root_ty), Some(tree_root_field)) = (
            gc_for_tree.tree_root_type_idx,
            gc_for_tree.tree_root_field_idx,
        ) {
            func.instruction(&Instruction::LocalGet(self_ref_local));
            func.instruction(&Instruction::StructNewDefault(tree_root_ty));
            func.instruction(&Instruction::StructSet {
                struct_type_index: comp_struct_ty,
                field_index: tree_root_field,
            });
        }

        self.current_init_scratch_start = if signal_temp_locals > 0 {
            Some(0)
        } else {
            None
        };
        self.current_flat_scratch = Some(ctor_scratch);
        self.current_self_local = Some(self_ref_local);
        self.current_self_comp_idx = Some(comp_idx);
        // Fresh boundary-locals scope — constructor only has
        // `$self_ref` accessible. Save/clear any leaked entries from a
        // prior emit scope.
        let saved_boundary_locals_ctor = std::mem::take(&mut self.current_boundary_locals);
        // Reset the parent-retention cursor for this component before
        // emitting the constructor body. Constructor block ops never
        // contain `MountComponent` today, but keep the bookkeeping
        // invariant that emission of a per-instance body restarts
        // retention slot allocation from 0.
        self.parent_retention_cursor.insert(comp_idx, 0);
        let component_ref: &'a LirComponent = &self.components[comp_idx];
        let constructor_block = component_ref.get_block(component_ref.constructor_block);
        for op in &constructor_block.ops {
            self.emit_op(&mut func, op, comp_idx, 0)?;
        }
        self.current_init_scratch_start = None;
        self.current_flat_scratch = None;
        self.current_self_local = None;
        self.current_self_comp_idx = None;
        self.current_boundary_locals = saved_boundary_locals_ctor;

        // Zero-init memory-backed slots (allocated during mount block
        // lowering). BoundaryField slots are already default-zero by
        // virtue of `struct.new_default` on the boundary structs.
        for slot in &component.slots {
            if let LirSlotKind::Memory { offset, size } = slot.kind {
                func.instruction(&Instruction::I32Const(layout.base + offset as i32));
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                let _ = size;
            }
        }

        // Return the typed self ref.
        func.instruction(&Instruction::LocalGet(self_ref_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Internal mount entry point.
    ///
    /// Signature: `(ref null $Comp_<i>, root: i32) -> () | i32` (the
    /// `i32` return is reserved for container components — the
    /// children-root DOM node id). Param 0 is the typed self ref;
    /// `current_self_local` is set to `0` for the body so signal
    /// struct.get/set ops, block calls, etc. all source self from
    /// the param ref.
    pub(super) fn generate_component_mount_internal(
        &mut self,
        comp_idx: usize,
        _layout: &MemoryLayout,
    ) -> Result<Function, CodegenError> {
        // Fresh label tracking for this function body.
        self.current_function_labels.clear();
        self.current_label_counter = 0;
        let component: &'a LirComponent = &self.components[comp_idx];

        // Same Temp-slot ordering and scratch sizing as the legacy
        // exported mount, just with one fewer param (no `self: i32`)
        // and the typed self ref already in WASM param 0.
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
        let base_i32_locals = temp_slots.len() as u32;

        let mount_block_ref = component.get_block(component.mount_block);
        let (mount_i32, mount_i64, mount_f32, mount_f64) = mount_block_ref.max_flat_scratch_counts;

        let mut locals: Vec<(u32, ValType)> = Vec::new();
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
                    let &ty_idx = self
                        .record_gc_types
                        .list_array_type_idx
                        .get(&list_ty)
                        .unwrap_or_else(|| panic!(
                            "lifecycle local: missing list_array_type_idx for ty {:?} kind={:?}",
                            list_ty,
                            self.ctx.ty_kind(list_ty)
                        ));
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                    })
                }
                LirSlotValType::RefNullForRecord(record_ty) => {
                    use yel_core::types::InternedTyKind;
                    let def_id = match self.ctx.ty_kind(record_ty) {
                        InternedTyKind::Adt(d) => *d,
                        _ => panic!("lifecycle local: RefNullForRecord on non-Adt"),
                    };
                    let &ty_idx = self
                        .record_gc_types
                        .record_type_idx
                        .get(&def_id)
                        .expect("lifecycle local: missing record_type_idx");
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                    })
                }
            };
            locals.push((1, val_ty));
        }
        if mount_i32 > 0 {
            locals.push((mount_i32, ValType::I32));
        }
        if mount_i64 > 0 {
            locals.push((mount_i64, ValType::I64));
        }
        if mount_f32 > 0 {
            locals.push((mount_f32, ValType::F32));
        }
        if mount_f64 > 0 {
            locals.push((mount_f64, ValType::F64));
        }

        // One typed `(ref null $Comp_<child>)` scratch local per
        // distinct child component reachable from a `MountComponent`
        // op. Used by the op emitter to stash the child ref returned
        // by the internal constructor across the matching internal
        // mount call and the parent-retention `struct.set`.
        //
        // Plus two scratch locals for the on-mount registry-alloc each
        // child needs: an i32 idx scratch and a typed handle-array
        // scratch. These let `emit_registry_alloc` produce a handle for
        // the child (so its event handlers route correctly through
        // dispatch) without requiring the child to be host-exported.
        let mount_block_ref = component.get_block(component.mount_block);
        let child_indices: Vec<usize> = mount_block_ref
            .mount_component_children
            .iter()
            .filter_map(|def_id| self.components.iter().position(|c| c.def_id == *def_id))
            .collect();
        let mut mount_child_locals: HashMap<usize, u32> = HashMap::new();
        let mut mount_child_alloc_idx_locals: HashMap<usize, u32> = HashMap::new();
        let mut mount_child_alloc_arr_locals: HashMap<usize, u32> = HashMap::new();
        let mut next_local = 2 + base_i32_locals + mount_i32 + mount_i64 + mount_f32 + mount_f64;
        for &child_idx in &child_indices {
            let child_struct_ty = self.gc_layouts[child_idx]
                .component_struct_type_idx
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "mount-internal: child component {} missing component_struct_type_idx",
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
            mount_child_locals.insert(child_idx, next_local);
            next_local += 1;
            // i32 idx scratch
            locals.push((1, ValType::I32));
            mount_child_alloc_idx_locals.insert(child_idx, next_local);
            next_local += 1;
            // typed handle-array scratch
            let _ = child_idx;
            let child_handle_arr_ty = self.shared_handle_arr_type_idx.ok_or_else(|| {
                CodegenError::InternalError(
                    "mount-internal: shared_handle_arr_type_idx not set".into(),
                )
            })?;
            locals.push((
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(child_handle_arr_ty),
                }),
            ));
            mount_child_alloc_arr_locals.insert(child_idx, next_local);
            next_local += 1;
        }

        let mut func = Function::new(locals);

        // WASM params: 0 = (ref null $Comp), 1 = root: i32. Slots
        // start at WASM local 2 (mirrors the legacy export's 2-param
        // layout — keeps `slot_local + 2` lookups identical).
        // Copy root (param 1) into slot 0 (local 2).
        func.instruction(&Instruction::LocalGet(1));
        func.instruction(&Instruction::LocalSet(2));

        let scratch_base = 2 + base_i32_locals;
        let mount_scratch = crate::wasm::FlatScratchBases {
            i32_base: scratch_base,
            i32_count: mount_i32,
            i64_base: scratch_base + mount_i32,
            i64_count: mount_i64,
            f32_base: scratch_base + mount_i32 + mount_i64,
            f32_count: mount_f32,
            f64_base: scratch_base + mount_i32 + mount_i64 + mount_f32,
            f64_count: mount_f64,
        };
        let had_scratch = mount_i32 + mount_i64 + mount_f32 + mount_f64 > 0;
        if had_scratch {
            self.current_flat_scratch = Some(mount_scratch);
        }

        // Self ref is WASM param 0 — typed `(ref null $Comp_<i>)`.
        let prev_self_local = self.current_self_local;
        let prev_self_comp_idx = self.current_self_comp_idx;
        let prev_mount_child_locals = self.current_mount_child_locals.take();
        let prev_mount_child_alloc_idx_locals = self.current_mount_child_alloc_idx_locals.take();
        let prev_mount_child_alloc_arr_locals = self.current_mount_child_alloc_arr_locals.take();
        self.current_self_local = Some(0);
        self.current_self_comp_idx = Some(comp_idx);
        if !mount_child_locals.is_empty() {
            self.current_mount_child_locals = Some(mount_child_locals);
            self.current_mount_child_alloc_idx_locals = Some(mount_child_alloc_idx_locals);
            self.current_mount_child_alloc_arr_locals = Some(mount_child_alloc_arr_locals);
        }
        // Fresh boundary-locals scope: any inner-boundary refs the
        // mount block needs are allocated locally via
        // `LirOp::AllocSubBoundary` (which registers entries) and
        // must not leak between component mount emissions.
        let prev_boundary_locals = std::mem::take(&mut self.current_boundary_locals);
        // Reset parent-retention cursor — this is the start of a fresh
        // per-instance body. Each `MountComponent` op encountered
        // during emit consumes one retention slot from
        // `parent_retention_field_base`.
        self.parent_retention_cursor.insert(comp_idx, 0);

        let mount_block = component.get_block(component.mount_block);
        for op in &mount_block.ops {
            self.emit_op(&mut func, op, comp_idx, 2)?;
        }

        self.current_self_local = prev_self_local;
        self.current_self_comp_idx = prev_self_comp_idx;
        self.current_mount_child_locals = prev_mount_child_locals;
        self.current_mount_child_alloc_idx_locals = prev_mount_child_alloc_idx_locals;
        self.current_mount_child_alloc_arr_locals = prev_mount_child_alloc_arr_locals;
        self.current_boundary_locals = prev_boundary_locals;
        if had_scratch {
            self.current_flat_scratch = None;
        }

        // Container components return the children-root DOM node id.
        if let Some(children_slot) = component.children_root_slot {
            func.instruction(&Instruction::LocalGet(
                slot_local(component, children_slot) + 2,
            ));
        }

        func.instruction(&Instruction::End);
        // Internal mount's WASM index is `comp_func_base + 3 + 2*signals + 1`.
        if !self.current_function_labels.is_empty()
            && let Some(&base) = self.component_func_bases.get(comp_idx)
        {
            let data_signal_count = self.components[comp_idx]
                .signals
                .iter()
                .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
                .count() as u32;
            let internal_mount_idx = base + 3 + 2 * data_signal_count + 1;
            let labels = std::mem::take(&mut self.current_function_labels);
            self.function_label_names.insert(internal_mount_idx, labels);
        }
        Ok(func)
    }

    /// Internal unmount entry point.
    ///
    /// Signature: `(ref null $Comp_<i>) -> ()`. Walks memory-backed
    /// slots and removes the DOM nodes they hold. The exported
    /// `[dtor]` wrapper does the registry-handle resolution + drop and
    /// then calls this internal entry — keeping the GC traversal
    /// uniform whether unmount is invoked by the host or by an
    /// internal cleanup site.
    pub(super) fn generate_unmount_internal_for(
        &mut self,
        comp_idx: usize,
    ) -> Result<Function, CodegenError> {
        let component: &'a LirComponent = &self.components[comp_idx];
        let mut func = Function::new([]);

        // Establish self-ref scope so boundary-field reads can
        // source `$self.tree` for the typed walk.
        self.current_self_local = Some(0);
        self.current_self_comp_idx = Some(comp_idx);
        // Fresh boundary-locals scope — unmount has only `$self` as
        // a param; no inner boundaries are passed in. Save and clear
        // so any stale entries from a prior emit scope can't leak.
        let saved_boundary_locals = std::mem::take(&mut self.current_boundary_locals);

        for slot in &component.slots {
            match slot.kind {
                LirSlotKind::Memory { offset, .. } => {
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::Call(IMPORT_REMOVE));
                }
                LirSlotKind::BoundaryField {
                    boundary_id,
                    field_idx,
                } => {
                    // Only DomHandle fields hold i32 DOM handles to
                    // detach. Skip ChildrenArray / SubBoundary /
                    // ActiveTag / LoopVar fields — those carry refs or
                    // tracking metadata, not detachable DOM nodes.
                    let component = &self.components[comp_idx];
                    let boundary = &component.tree_shape.boundaries[boundary_id.index()];
                    let is_dom_handle = matches!(
                        boundary.fields.get(field_idx as usize),
                        Some(yel_core::lir::block::TreeFieldDecl::DomHandle { .. })
                    );
                    if !is_dom_handle {
                        continue;
                    }
                    // Skip slots reachable only through a ForIterBody
                    // (or any boundary whose parent_link is None and
                    // that isn't the root). Per-iteration state lives
                    // on dynamically-allocated iter-body structs cleaned
                    // up via the for-anchor's children-array unmount
                    // path inside `create_for_update_block_reactive`,
                    // not this static self-walk.
                    let mut cur = boundary_id;
                    let mut skip = false;
                    loop {
                        if cur.0 == component.tree_shape.root_idx {
                            break;
                        }
                        let b = &component.tree_shape.boundaries[cur.index()];
                        match b.parent_link {
                            Some((parent, _)) => cur = parent,
                            None => {
                                skip = true;
                                break;
                            }
                        }
                    }
                    if skip {
                        continue;
                    }
                    self.emit_boundary_field_load(&mut func, comp_idx, boundary_id, field_idx)?;
                    func.instruction(&Instruction::Call(IMPORT_REMOVE));
                }
                LirSlotKind::Temp { .. } => {}
            }
        }

        self.current_self_local = None;
        self.current_self_comp_idx = None;
        self.current_boundary_locals = saved_boundary_locals;

        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Exported constructor wrapper.
    ///
    /// Signature: `() -> i32`. Thin delegate over the internal entry
    /// (`generate_constructor_internal_for`):
    ///   1. Call `$ctor_internal` → typed `(ref null $Comp_<i>)` ref.
    ///   2. Stash the ref in `self_ref_local`.
    ///   3. `emit_registry_alloc(self_ref) -> handle_idx` — write the
    ///      ref into a registry handle, returning its i32 index (the
    ///      rep).
    ///   4. `call [resource-new]X` to wrap the rep into a host
    ///      resource handle.
    ///   5. Stash the handle in the trailing `$self_handle (mut i32)`
    ///      field on `$Comp_<i>` (callback emit sites read it via
    ///      `struct.get` for the `borrow<Self>` lift) and return it.
    ///
    /// Non-exported components (no `import_resource_new`) skip the
    /// registry/resource-new dance and just call the internal — they
    /// have no host contract and the returned ref is dropped.
    pub(super) fn generate_constructor_for(
        &mut self,
        _component: &LirComponent,
        layout: &MemoryLayout,
        import_resource_new: Option<u32>,
        comp_idx: usize,
    ) -> Result<Function, CodegenError> {
        let gc_layout = &self.gc_layouts[comp_idx];
        let comp_struct_ty = gc_layout.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InternalError(format!(
                "component {}: exported ctor wrapper requires component_struct_type_idx",
                comp_idx
            ))
        })?;
        let handle_arr_ty = self.shared_handle_arr_type_idx;

        // Compute the internal constructor's WASM function index:
        //   base + 3 + 2 * data_signal_count
        let data_signal_count = self.components[comp_idx]
            .signals
            .iter()
            .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
            .count() as u32;
        let internal_ctor_idx = self.component_func_bases[comp_idx] + 3 + 2 * data_signal_count;

        let need_handle_local = import_resource_new.is_some();

        // Local layout for the export wrapper:
        //   0: self_ref ((ref null $Comp_<i>))
        //   1: handle_local (i32)            — only when exported
        //   2: alloc_scratch_idx_local (i32) — only when exported
        //   3: alloc_scratch_arr_local ((ref null $CompHandleArr_<i>)) — only when exported
        let mut locals: Vec<(u32, ValType)> = Vec::new();
        locals.push((
            1,
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(comp_struct_ty),
            }),
        ));
        let self_ref_local = 0u32;
        let handle_local;
        let alloc_scratch_idx_local;
        let alloc_scratch_arr_local;
        if need_handle_local {
            // i32 handle scratch + i32 alloc-idx scratch (two i32s)
            locals.push((2, ValType::I32));
            handle_local = Some(1u32);
            alloc_scratch_idx_local = Some(2u32);
            // Typed array scratch for the registry-grow path.
            locals.push((
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(handle_arr_ty.ok_or_else(
                        || {
                            CodegenError::InternalError(
                                "exported component ctor: shared handle-array type idx missing"
                                    .into(),
                            )
                        },
                    )?),
                }),
            ));
            alloc_scratch_arr_local = Some(3u32);
        } else {
            handle_local = None;
            alloc_scratch_idx_local = None;
            alloc_scratch_arr_local = None;
        }

        let mut func = Function::new(locals);

        // 1. call internal ctor → ref on stack
        func.instruction(&Instruction::Call(internal_ctor_idx));
        // 2. stash into self_ref_local. Every helper sources self via
        //    current_self_local or via a registry lookup; no singleton
        //    mirror exists.
        func.instruction(&Instruction::LocalSet(self_ref_local));

        // 3-5. Exported: registry alloc + resource-new round-trip.
        if let Some(resource_new_idx) = import_resource_new {
            let scratch_idx = alloc_scratch_idx_local.ok_or_else(|| {
                CodegenError::InternalError("exported ctor: alloc_scratch_idx_local missing".into())
            })?;
            let scratch_arr = alloc_scratch_arr_local.ok_or_else(|| {
                CodegenError::InternalError("exported ctor: alloc_scratch_arr_local missing".into())
            })?;
            self.emit_registry_alloc(
                &mut func,
                comp_idx,
                self_ref_local,
                scratch_idx,
                scratch_arr,
            )?;
            // Stack: [rep (registry idx)] → [host handle]
            func.instruction(&Instruction::Call(resource_new_idx));
            let handle_local_idx = handle_local.ok_or_else(|| {
                CodegenError::InternalError(
                    "exported ctor: handle_local missing with import_resource_new".into(),
                )
            })?;
            func.instruction(&Instruction::LocalSet(handle_local_idx));
            // Stash the host handle in `$Comp_<i>.$self_handle` (a
            // trailing `(mut i32)` field) so callback callsites can
            // recover it via `struct.get` for the `borrow<Self>` lift.
            let gc = &self.gc_layouts[comp_idx];
            let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "component {}: resource-new stash requires component_struct_type_idx",
                    comp_idx
                ))
            })?;
            let self_handle_field = gc.self_handle_field_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "component {}: exported ctor requires self_handle_field_idx",
                    comp_idx
                ))
            })?;
            func.instruction(&Instruction::LocalGet(self_ref_local));
            func.instruction(&Instruction::LocalGet(handle_local_idx));
            func.instruction(&Instruction::StructSet {
                struct_type_index: comp_struct_ty,
                field_index: self_handle_field,
            });
            func.instruction(&Instruction::LocalGet(handle_local_idx));
        } else {
            // Non-exported: legacy "rep is layout.base" — internal-only
            // path, no host registry interaction.
            func.instruction(&Instruction::I32Const(layout.base));
        }
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Exported mount wrapper.
    ///
    /// Signature: `(self: i32, root: i32) -> () | i32`. Resolves the
    /// host handle `self` through the registry into a typed
    /// `(ref null $Comp_<i>)`, stashes the host handle in the
    /// component's `current_handle_global` for AddEventListener
    /// encoding, and delegates to `generate_component_mount_internal`.
    /// The internal entry runs the mount-block body; this wrapper
    /// just routes the call.
    pub(super) fn generate_component_mount(
        &mut self,
        comp_idx: usize,
        _layout: &MemoryLayout,
    ) -> Result<Function, CodegenError> {
        let component: &'a LirComponent = &self.components[comp_idx];
        let gc = &self.gc_layouts[comp_idx];
        let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InternalError(format!(
                "component {}: exported mount wrapper requires component_struct_type_idx",
                comp_idx
            ))
        })?;

        // Compute the internal mount's WASM function index:
        //   base + 3 + 2 * data_signal_count + 1
        let data_signal_count = component
            .signals
            .iter()
            .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
            .count() as u32;
        let internal_mount_idx =
            self.component_func_bases[comp_idx] + 3 + 2 * data_signal_count + 1;

        // Locals: WASM params 0 (self: i32) and 1 (root: i32) are
        // declared by the function type. We reserve one extra typed
        // local for the resolved `(ref null $Comp_<i>)`.
        let self_ref_local = 2u32;
        let locals: Vec<(u32, ValType)> = vec![(
            1,
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(comp_struct_ty),
            }),
        )];
        let mut func = Function::new(locals);

        // Resolve the host handle into a typed self ref. Only exported
        // components have valid registry entries — non-exported child
        // components keep their legacy `layout.base` rep + linear-memory
        // path, so we skip the lookup for them and call internal mount
        // with a null ref. This branch only fires for the exported
        // wrapper, which is only ever invoked by the host (with a real
        // handle).
        if component.is_export {
            self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;
            // Stash the host handle for AddEventListener encoding inside
            // mount-internal.
            if let Some(handle_g) = gc.current_handle_global {
                func.instruction(&Instruction::LocalGet(0));
                func.instruction(&Instruction::GlobalSet(handle_g));
            }
        } else {
            // Non-exported components shouldn't have their exported
            // mount wrapper called externally — `MountComponent` routes
            // children through the internal entry point. Body still
            // feeds a null ref to the internal call so the wrapper
            // validates; if this ever fires at runtime that's a
            // host-side bug worth a trap.
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                comp_struct_ty,
            )));
            func.instruction(&Instruction::LocalSet(self_ref_local));
        }

        // Delegate to the internal mount: (ref, root) -> () | i32.
        func.instruction(&Instruction::LocalGet(self_ref_local));
        func.instruction(&Instruction::LocalGet(1));
        func.instruction(&Instruction::Call(internal_mount_idx));
        // Internal returns i32 for container components; that matches
        // the export's signature, so just leave it on the stack.

        func.instruction(&Instruction::End);
        Ok(func)
    }

    // Emit a single block operation as WASM instructions.
    // `local_offset` is added to slot indices for local variable access:
    // - Mount function: 2 (for self, root params)
    // - Block functions: 1 (for parent param) or 2 (for parent, item_ptr params)

    // Expression emission functions moved to expr.rs

    /// Exported unmount wrapper.
    ///
    /// Signature: `(self: i32) -> ()`. Resolves the host handle
    /// through the registry and delegates to the internal unmount
    /// entry. Body extraction (the actual memory-slot removal) lives
    /// in `generate_unmount_internal_for`.
    pub(super) fn generate_unmount_for(
        &self,
        component: &LirComponent,
        _layout: &MemoryLayout,
        comp_idx: usize,
    ) -> Result<Function, CodegenError> {
        let gc = &self.gc_layouts[comp_idx];
        let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InternalError(format!(
                "component {}: exported unmount wrapper requires component_struct_type_idx",
                comp_idx
            ))
        })?;

        let data_signal_count = component
            .signals
            .iter()
            .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
            .count() as u32;
        let internal_unmount_idx =
            self.component_func_bases[comp_idx] + 3 + 2 * data_signal_count + 2;

        let self_ref_local = 1u32;
        let locals: Vec<(u32, ValType)> = vec![(
            1,
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(comp_struct_ty),
            }),
        )];
        let mut func = Function::new(locals);

        if component.is_export {
            self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;
        } else {
            // Non-exported: dead path; leave a null ref for type-correctness.
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                comp_struct_ty,
            )));
            func.instruction(&Instruction::LocalSet(self_ref_local));
        }
        func.instruction(&Instruction::LocalGet(self_ref_local));
        func.instruction(&Instruction::Call(internal_unmount_idx));

        func.instruction(&Instruction::End);
        Ok(func)
    }
}
