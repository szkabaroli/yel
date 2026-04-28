//! `LirOp` -> WASM instruction emission.
//!
//! The `emit_op` method is the single match-on-`LirOp` site for the
//! whole codegen pass. Each arm consumes a constructor / mount / block
//! statement and emits the corresponding WASM. Lives on
//! `WasmPackageBuilder<'a>` via an additional impl block.

use wasm_encoder::{BlockType, Function, Instruction};
use yel_core::Ty;
use yel_core::lir::{LirExprKind, LirOp, LirSlotKind};
use yel_core::types::InternedTyKind;

use super::super::CodegenError;
use super::super::{
    IMPORT_ADD_EVENT_LISTENER, IMPORT_APPEND_CHILD, IMPORT_CREATE_COMMENT, IMPORT_CREATE_ELEMENT,
    IMPORT_CREATE_FRAGMENT, IMPORT_CREATE_TEXT, IMPORT_INSERT_AFTER, IMPORT_REMOVE,
    IMPORT_SET_ATTRIBUTE, IMPORT_SET_TEXT_CONTENT, MemoryLayout, WasmPackageBuilder,
};
use super::constants::{HANDLER_ID_HANDLE_SHIFT, MAX_HANDLERS_PER_COMPONENT};
use super::scratch::{mem_arg, slot_local};

impl<'a> WasmPackageBuilder<'a> {
    /// Emit a single block operation as WASM instructions.
    /// `local_offset` is added to slot indices for local variable access:
    /// - Mount function: 2 (for self, root params)
    /// - Block functions: 1 (for parent param) or 2 (for parent, item_ptr params)
    pub(super) fn emit_op(
        &mut self,
        func: &mut Function,
        op: &LirOp,
        comp_idx: usize,
        local_offset: u32,
    ) -> Result<(), CodegenError> {
        let component = &self.components[comp_idx];
        let layout = self.layouts.get(comp_idx).cloned().unwrap_or_else(|| {
            let signal_offsets: Vec<i32> = component
                .signals
                .iter()
                .enumerate()
                .map(|(i, _)| (i as i32) * 4)
                .collect();
            let tail = signal_offsets.last().map(|o| o + 4).unwrap_or(0);
            let aligned_tail = (tail + 3) & !3;
            MemoryLayout {
                base: 324,
                signal_offsets,
                size: aligned_tail,
            }
        });

        match op {
            LirOp::MountComponent {
                component_def,
                parent,
                children_root,
            } => {
                // Find the child component index by its DefId
                let child_idx = self
                    .components
                    .iter()
                    .position(|c| c.def_id == *component_def);

                if let Some(child_idx) = child_idx {
                    // Step 5: route the mount through the **internal**
                    // entry points so the typed `(ref null $Comp_<child>)`
                    // never round-trips through `[resource-new]` /
                    // registry handle. Each `MountComponent` site has a
                    // pre-reserved typed local (declared by the
                    // surrounding function emitter) to hold the child
                    // ref across the matching internal mount call and
                    // the parent-retention `struct.set`.
                    let child_func_base = self.component_func_bases[child_idx];
                    let child_data_signal_count = self.components[child_idx]
                        .signals
                        .iter()
                        .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
                        .count() as u32;
                    let child_internal_ctor_idx = child_func_base + 3 + 2 * child_data_signal_count;
                    let child_internal_mount_idx =
                        child_func_base + 3 + 2 * child_data_signal_count + 1;

                    let child_local = self
                        .current_mount_child_locals
                        .as_ref()
                        .and_then(|m| m.get(&child_idx).copied())
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "MountComponent: no typed scratch local reserved for child \
                                 component {} — surrounding function emitter must call \
                                 collect_mount_component_children and reserve one local per \
                                 child",
                                child_idx
                            ))
                        })?;

                    // 1. Call internal ctor → typed `(ref null $Comp_<child>)`.
                    func.instruction(&Instruction::Call(child_internal_ctor_idx));
                    // 2. Stash in the typed scratch so we can both pass
                    //    it to mount and write it into the retention field.
                    func.instruction(&Instruction::LocalSet(child_local));
                    // 2a. Allocate a registry handle for the child so its
                    //     `AddEventListener` ops can encode `(handle <<
                    //     16) | local_id` and dispatch can recover the
                    //     typed self ref via the child's registry — no
                    //     singleton involvement, even for non-exported
                    //     children. The handle is also written into the
                    //     child's transient `current_handle_global` so
                    //     mount-internal's AddEventListener sites pick
                    //     it up.
                    let alloc_idx_local = self
                        .current_mount_child_alloc_idx_locals
                        .as_ref()
                        .and_then(|m| m.get(&child_idx).copied())
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "MountComponent: no alloc-idx scratch local reserved for child \
                                 component {} — surrounding emitter must reserve i32 + arr \
                                 scratch alongside the typed child-ref local",
                                child_idx
                            ))
                        })?;
                    let alloc_arr_local = self
                        .current_mount_child_alloc_arr_locals
                        .as_ref()
                        .and_then(|m| m.get(&child_idx).copied())
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "MountComponent: no alloc-arr scratch local reserved for child \
                                 component {}",
                                child_idx
                            ))
                        })?;
                    self.emit_registry_alloc(
                        func,
                        child_idx,
                        child_local,
                        alloc_idx_local,
                        alloc_arr_local,
                    )?;
                    // Stack: [child_handle: i32]. Stash to child's
                    // current_handle_global before calling mount-internal
                    // so AddEventListener emissions inside read the
                    // right handle.
                    let child_handle_g = self.gc_layouts[child_idx]
                        .current_handle_global
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "MountComponent: child component {} has no current_handle_global",
                                child_idx
                            ))
                        })?;
                    func.instruction(&Instruction::GlobalSet(child_handle_g));
                    // 3. Write retention. `next_mount_retention_target`
                    //    returns either the surrounding component
                    //    instance (parent-retention) or the for-iter
                    //    record (when we're inside a for body). The
                    //    field is `(ref null any)`, so the typed
                    //    child ref upcasts implicitly via `local.get`.
                    if let Some((target_struct_ty, target_local, field_idx)) =
                        self.next_mount_retention_target(comp_idx)?
                    {
                        func.instruction(&Instruction::LocalGet(target_local));
                        func.instruction(&Instruction::LocalGet(child_local));
                        func.instruction(&Instruction::StructSet {
                            struct_type_index: target_struct_ty,
                            field_index: field_idx,
                        });
                    }
                    // 4. Call internal mount: (ref null $Comp_<child>, root: i32) -> () | i32.
                    func.instruction(&Instruction::LocalGet(child_local));
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *parent) + local_offset,
                    ));
                    func.instruction(&Instruction::Call(child_internal_mount_idx));
                    if let Some(cr) = children_root {
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *cr) + local_offset,
                        ));
                    }
                }
            }
            LirOp::CreateElement { tag, result } => {
                let tag_str = component.get_string(*tag);
                if let Some((ptr, len)) = self.get_string_info(tag_str) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    func.instruction(&Instruction::Call(IMPORT_CREATE_ELEMENT));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *result) + local_offset,
                    ));
                }
            }
            LirOp::CreateFragment { result } => {
                func.instruction(&Instruction::Call(IMPORT_CREATE_FRAGMENT));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }
            LirOp::CreateText { content, result } => {
                let text = component.get_string(*content);
                if let Some((ptr, len)) = self.get_string_info(text) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    func.instruction(&Instruction::Call(IMPORT_CREATE_TEXT));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *result) + local_offset,
                    ));
                }
            }
            LirOp::AppendChild { parent, child } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *parent) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *child) + local_offset,
                ));
                func.instruction(&Instruction::Call(IMPORT_APPEND_CHILD));
            }
            LirOp::SetAttribute { node, name, expr } => {
                let attr_name = component.get_string(*name);
                let attr_expr = component.get_expr(*expr);
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *node) + local_offset,
                ));
                if let Some((name_ptr, name_len)) = self.get_string_info(attr_name) {
                    func.instruction(&Instruction::I32Const(name_ptr as i32));
                    func.instruction(&Instruction::I32Const(name_len as i32));
                }
                // Emit attribute-value variant: (discrim, p0: i32, p1: i32, p2: i64, p3: f32, p4: f64)
                self.emit_expr_as_attr_value(func, attr_expr, component, &layout)?;
                func.instruction(&Instruction::Call(IMPORT_SET_ATTRIBUTE));
            }
            LirOp::CreateTextDynamic { expr, result } => {
                let text_expr = component.get_expr(*expr);
                self.emit_expr_as_string(func, text_expr, component, &layout)?;
                func.instruction(&Instruction::Call(IMPORT_CREATE_TEXT));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }
            LirOp::StoreHandle { slot, from } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::LocalGet(
                                slot_local(component, *from) + local_offset,
                            ));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::BoundaryField {
                            boundary_id,
                            field_idx,
                        } => {
                            let value_local = slot_local(component, *from) + local_offset;
                            self.emit_boundary_field_store(
                                func,
                                comp_idx,
                                boundary_id,
                                field_idx,
                                value_local,
                            )?;
                        }
                        LirSlotKind::Temp { .. } => {}
                    }
                }
            }
            LirOp::LoadHandle { slot, to } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *to) + local_offset,
                            ));
                        }
                        LirSlotKind::BoundaryField {
                            boundary_id,
                            field_idx,
                        } => {
                            self.emit_boundary_field_load(func, comp_idx, boundary_id, field_idx)?;
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *to) + local_offset,
                            ));
                        }
                        LirSlotKind::Temp { .. } => {}
                    }
                }
            }
            LirOp::SetTextContent { node, expr } => {
                let text_expr = component.get_expr(*expr);
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *node) + local_offset,
                ));
                self.emit_expr_as_string(func, text_expr, component, &layout)?;
                func.instruction(&Instruction::Call(IMPORT_SET_TEXT_CONTENT));
            }
            LirOp::CreateComment { content, result } => {
                let text = component.get_string(*content);
                if let Some((ptr, len)) = self.get_string_info(text) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    func.instruction(&Instruction::Call(IMPORT_CREATE_COMMENT));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *result) + local_offset,
                    ));
                }
            }
            LirOp::Remove { node } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *node) + local_offset,
                ));
                func.instruction(&Instruction::Call(IMPORT_REMOVE));
            }
            LirOp::InsertAfter {
                parent,
                node,
                anchor,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *parent) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *node) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *anchor) + local_offset,
                ));
                func.instruction(&Instruction::Call(IMPORT_INSERT_AFTER));
            }
            LirOp::StoreI32 { slot, value } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::I32Const(*value));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::BoundaryField {
                            boundary_id,
                            field_idx,
                        } => {
                            self.emit_boundary_ref(func, comp_idx, boundary_id)?;
                            func.instruction(&Instruction::RefAsNonNull);
                            func.instruction(&Instruction::I32Const(*value));
                            let struct_ty =
                                self.gc_layouts[comp_idx].tree_struct_type_idx[&boundary_id];
                            func.instruction(&Instruction::StructSet {
                                struct_type_index: struct_ty,
                                field_index: field_idx,
                            });
                        }
                        LirSlotKind::Temp { local_idx } => {
                            // Temp target: `i32.const <value>; local.set
                            // <abs_idx>`. The if-update block in
                            // `create_if_update_block_flat` allocates a
                            // Temp slot for `upd_target` and writes 0/1/2
                            // into it via `StoreI32`; without this arm
                            // those writes silently no-op'd, so the
                            // update dispatch always ran with the
                            // default-zero target and never mounted a
                            // newly-active branch.
                            func.instruction(&Instruction::I32Const(*value));
                            func.instruction(&Instruction::LocalSet(local_idx + local_offset));
                        }
                    }
                }
            }
            LirOp::StoreI32Slot { slot, from } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::LocalGet(
                                slot_local(component, *from) + local_offset,
                            ));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::BoundaryField {
                            boundary_id,
                            field_idx,
                        } => {
                            self.emit_boundary_ref(func, comp_idx, boundary_id)?;
                            func.instruction(&Instruction::RefAsNonNull);
                            func.instruction(&Instruction::LocalGet(
                                slot_local(component, *from) + local_offset,
                            ));
                            let struct_ty =
                                self.gc_layouts[comp_idx].tree_struct_type_idx[&boundary_id];
                            func.instruction(&Instruction::StructSet {
                                struct_type_index: struct_ty,
                                field_index: field_idx,
                            });
                        }
                        LirSlotKind::Temp { .. } => {
                            // Temp-to-Temp copy is a plain local.set/get.
                            func.instruction(&Instruction::LocalGet(
                                slot_local(component, *from) + local_offset,
                            ));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *slot) + local_offset,
                            ));
                        }
                    }
                }
            }
            LirOp::I32Ne { lhs, rhs, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *lhs) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *rhs) + local_offset,
                ));
                func.instruction(&Instruction::I32Ne);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }
            LirOp::I32EqConst { lhs, rhs, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *lhs) + local_offset,
                ));
                func.instruction(&Instruction::I32Const(*rhs));
                func.instruction(&Instruction::I32Eq);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }
            LirOp::LoadI32 { slot, to } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(offset as i32));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *to) + local_offset,
                            ));
                        }
                        LirSlotKind::BoundaryField {
                            boundary_id,
                            field_idx,
                        } => {
                            self.emit_boundary_field_load(func, comp_idx, boundary_id, field_idx)?;
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *to) + local_offset,
                            ));
                        }
                        LirSlotKind::Temp { .. } => {}
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
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *result) + local_offset,
                    ));
                    return Ok(());
                }

                self.emit_expr(func, lir_expr, component, &layout)?;

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
                            slot_local(component, *result) + local_offset + 1,
                        )); // len -> slot+1
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *result) + local_offset,
                        )); // ptr -> slot
                    } else if is_option {
                        // Option with payload: Stack has [discriminant, value]
                        // For `none`, VariantCtor only pushes discriminant
                        // For `some(v)`, VariantCtor pushes (discriminant, value)
                        // Check if it's a none variant (no payload)
                        if let LirExprKind::VariantCtor { payload: None, .. } = &lir_expr.kind {
                            // `none` - only discriminant on stack
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *result) + local_offset,
                            )); // discriminant -> slot
                        // No value to store - slot+1 will be undefined/zero
                        } else {
                            // `some(v)` - (discriminant, value) on stack
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *result) + local_offset + 1,
                            )); // value -> slot+1
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *result) + local_offset,
                            )); // discriminant -> slot
                        }
                    } else {
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *result) + local_offset,
                        ));
                    }
                }
            }
            LirOp::DropExpr { expr } => {
                let lir_expr = component.get_expr(*expr);
                self.emit_expr(func, lir_expr, component, &layout)?;
                // Drop exactly the number of values the expression pushed.
                // Unit-typed expressions (e.g. callbacks returning nothing)
                // push zero values on the stack, so no drops are emitted in
                // that case. `flatten_core_valtypes` treats unknown primitives
                // as a single i32, so we special-case Unit explicitly here.
                if !matches!(self.ctx.ty_kind(lir_expr.ty), InternedTyKind::Unit) {
                    let flat = self.flatten_core_valtypes(lir_expr.ty);
                    for _ in 0..flat.len() {
                        func.instruction(&Instruction::Drop);
                    }
                }
            }
            LirOp::If {
                cond,
                then_ops,
                else_ops,
                name,
            } => {
                // Mint a label index for this `if` structural op before
                // emitting (preorder walk). Nested ifs/loops inside the
                // branches will get subsequent indices as they're visited.
                let if_label_idx = self.current_label_counter;
                self.current_label_counter += 1;
                if let Some(n) = name {
                    self.current_function_labels.push((if_label_idx, n.clone()));
                }
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *cond) + local_offset,
                ));
                func.instruction(&Instruction::If(BlockType::Empty));

                for nested_op in then_ops {
                    self.emit_op(func, nested_op, comp_idx, local_offset)?;
                }

                if !else_ops.is_empty() {
                    func.instruction(&Instruction::Else);
                    for nested_op in else_ops {
                        self.emit_op(func, nested_op, comp_idx, local_offset)?;
                    }
                }

                func.instruction(&Instruction::End);
            }
            LirOp::CallBlock { block, parent } => {
                if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, *block)) {
                    // Prepend the typed self ref so the callee can route
                    // signal struct.get/set through it.
                    self.emit_self_ref(func, comp_idx)?;
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *parent) + local_offset,
                    ));
                    // Step 2: if the callee opts into boundary-ref params
                    // (`LirBlock.boundary_params` non-empty), the caller
                    // must push the boundary refs after the i32 args, in
                    // declared order. `emit_boundary_ref` resolves each
                    // via the in-scope local (mount-block alloc, parent
                    // function param) or root via `$self.tree`.
                    let callee = component.get_block(*block);
                    for &b_id in &callee.boundary_params {
                        self.emit_boundary_ref(func, comp_idx, b_id)?;
                    }
                    func.instruction(&Instruction::Call(func_idx));
                }
            }
            LirOp::AddEventListener {
                node,
                event,
                handler,
            } => {
                // Per-component ordinal — capped at 16 bits, leaving the
                // upper 16 bits of the encoded handler-id for the host
                // resource handle. 65536 listeners per component is well
                // beyond any realistic UI tree; if that ever becomes a
                // limit, widen the encoding.
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

                let event_str = component.get_string(*event);
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *node) + local_offset,
                ));
                if let Some((ptr, len)) = self.get_string_info(event_str) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                }
                // Encoded handler_id = (handle << 16) | local_id. Read
                // the current host handle from the per-component
                // transient `current_handle` global, which mount-export
                // sets on entry from its `self: i32` param. Dispatch
                // decodes the upper 16 bits back into a registry index.
                let handle_g =
                    self.gc_layouts[comp_idx]
                        .current_handle_global
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "AddEventListener: missing current_handle_global \
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
                func.instruction(&Instruction::Call(IMPORT_ADD_EVENT_LISTENER));
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
                        &layout,
                        scratch,
                    )?;
                } else {
                    // Pointer-typed signals (records, tuples) still
                    // live in linear memory until a later phase
                    // bridges their canonical-ABI flat layout into
                    // GC struct fields.
                    let addr = layout.signal_addr(sig_idx);
                    self.emit_signal_store(func, addr, default_expr, component, &layout, scratch)?;
                }
            }

            LirOp::SignalWriteExpr { signal, expr } => {
                let lir_expr = component.get_expr(*expr);
                let scratch = self.current_flat_scratch.unwrap_or_default();
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    if self.signal_in_struct(comp_idx, sig_idx) {
                        self.emit_signal_struct_store_from_expr(
                            func, comp_idx, sig_idx, lir_expr, component, &layout, scratch,
                        )?;
                    } else {
                        let addr = layout.signal_addr(sig_idx);
                        self.emit_signal_store(func, addr, lir_expr, component, &layout, scratch)?;
                    }
                } else if self.ctx.defs.owning_global_block(*signal).is_some() {
                    if self.global_in_struct(*signal) {
                        self.emit_global_struct_store_from_expr(
                            func, *signal, lir_expr, component, &layout, scratch,
                        )?;
                    } else if let Some(&addr) = self.global_property_addrs.get(signal) {
                        self.emit_signal_store(func, addr, lir_expr, component, &layout, scratch)?;
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
                // type's zero/null default, so nothing to do here.
                if self.signal_in_struct(comp_idx, sig_idx) {
                    // no-op
                } else {
                    // Pointer-typed signals still live in linear
                    // memory — zero-init at signal_addr so subsequent
                    // reads see deterministic state.
                    let signal = &component.signals[sig_idx];
                    let addr = layout.signal_addr(sig_idx);
                    match self.ctx.ty_kind(signal.ty) {
                        InternedTyKind::F32 => {
                            func.instruction(&Instruction::I32Const(addr));
                            func.instruction(&Instruction::F32Const(0.0));
                            func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                        }
                        InternedTyKind::F64 => {
                            func.instruction(&Instruction::I32Const(addr));
                            func.instruction(&Instruction::F64Const(0.0));
                            func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                        }
                        InternedTyKind::S64 | InternedTyKind::U64 => {
                            func.instruction(&Instruction::I32Const(addr));
                            func.instruction(&Instruction::I64Const(0));
                            func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                        }
                        _ => {
                            func.instruction(&Instruction::I32Const(addr));
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                    }
                }
            }

            LirOp::BindBoundaryLocal { boundary_id, slot } => {
                // Pure compile-time scope-tracking: record the slot's
                // local index against the boundary so subsequent
                // BoundaryField accesses resolve via local.get. No
                // WASM instructions emitted.
                let local_idx = slot_local(component, *slot) + local_offset;
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
                let ref_local = slot_local(component, *ref_slot) + local_offset;
                func.instruction(&Instruction::StructNewDefault(new_struct_ty));
                func.instruction(&Instruction::LocalSet(ref_local));
                self.current_boundary_locals.insert(*boundary_id, ref_local);
            }

            LirOp::AllocSubBoundary {
                boundary_id,
                ref_slot,
            } => {
                let component = &self.components[comp_idx];
                let parent_link = component
                    .tree_shape
                    .boundaries
                    .iter()
                    .find(|b| b.id == *boundary_id)
                    .and_then(|b| b.parent_link)
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

                let ref_local = slot_local(component, *ref_slot) + local_offset;

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
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    match slot_info.kind {
                        LirSlotKind::Memory { offset, .. } => {
                            func.instruction(&Instruction::I32Const(layout.base + offset as i32));
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                        LirSlotKind::BoundaryField { .. } => {
                            // BoundaryField slots are zero-initialized
                            // by `struct.new_default` at constructor time
                            // (DOM-handle / active-tag fields are i32, so
                            // their default is 0). No explicit init op
                            // needed.
                        }
                        LirSlotKind::Temp { .. } => {}
                    }
                }
            }

            LirOp::ResourceNew { base_addr } => {
                func.instruction(&Instruction::I32Const(*base_addr));
            }

            LirOp::SignalRead { .. } => {
                // `LirOp::SignalRead` is never produced by the lowering — signal
                // reads happen via `LirExprKind::SignalRead` inside expressions.
                // Reaching this arm means the variant leaked from somewhere new.
                return Err(CodegenError::InvalidIR(format!(
                    "SignalRead op at IR-emit position not handled: {:?}",
                    op
                )));
            }

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
                        *value,
                        local_offset,
                    )?;
                    return Ok(());
                }
                // Global property or non-migrated signal — keep
                // linear-memory write.
                let (addr, signal_ty) =
                    if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                        (layout.signal_addr(sig_idx), component.signals[sig_idx].ty)
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
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                    }
                    InternedTyKind::F64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                    }
                    InternedTyKind::S64 | InternedTyKind::U64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                    }
                    InternedTyKind::Option(_) => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset + 1,
                        ));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                    InternedTyKind::String | InternedTyKind::List(_) => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset + 1,
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
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
                    }
                    InternedTyKind::U16 | InternedTyKind::S16 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
                        func.instruction(&Instruction::I32Store16(mem_arg(0, 1)));
                    }
                    _ => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::LocalGet(
                            slot_local(component, *value) + local_offset,
                        ));
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

                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *break_cond) + local_offset,
                ));
                func.instruction(&Instruction::BrIf(1));

                for nested_op in body_ops {
                    self.emit_op(func, nested_op, comp_idx, local_offset)?;
                }

                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End);
                func.instruction(&Instruction::End);
            }

            LirOp::CallBlock2 {
                block,
                param0,
                param1,
                result,
            } => {
                if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, *block)) {
                    // Step 4: prepend self ref.
                    self.emit_self_ref(func, comp_idx)?;
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *param0) + local_offset,
                    ));
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *param1) + local_offset,
                    ));
                    // If the callee opts into boundary-ref params (e.g.
                    // for-item-mount blocks plumbing the iter-body chain
                    // via `boundary_params`), push each boundary ref
                    // after the i32 args in declared order.
                    let callee = component.get_block(*block);
                    for &b_id in &callee.boundary_params {
                        self.emit_boundary_ref(func, comp_idx, b_id)?;
                    }
                    func.instruction(&Instruction::Call(func_idx));
                    match result {
                        Some(slot) => {
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *slot) + local_offset,
                            ));
                        }
                        None => {
                            // Callee returns i32 if the target block has a
                            // return_slot, but the caller doesn't want it —
                            // drop so the stack stays balanced.
                            let callee_returns = self
                                .components
                                .iter()
                                .find_map(|c| {
                                    c.blocks
                                        .iter()
                                        .find(|b| b.id == *block)
                                        .map(|b| b.return_slot.is_some())
                                })
                                .unwrap_or(false);
                            if callee_returns {
                                func.instruction(&Instruction::Drop);
                            }
                        }
                    }
                }
            }

            LirOp::GeU { index, len, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *index) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *len) + local_offset,
                ));
                func.instruction(&Instruction::I32GeU);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::LtU { a, b, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *a) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *b) + local_offset,
                ));
                func.instruction(&Instruction::I32LtU);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::ComputeItemPtr {
                base,
                index,
                element_size,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *base) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *index) + local_offset,
                ));
                func.instruction(&Instruction::I32Const(*element_size as i32));
                func.instruction(&Instruction::I32Mul);
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::IncrSlot { slot } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *slot) + local_offset,
                ));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *slot) + local_offset,
                ));
            }

            LirOp::Alloc {
                size,
                align,
                result,
            } => {
                if let Some(alloc_funcs) = &self.alloc_funcs {
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *size) + local_offset,
                    ));
                    func.instruction(&Instruction::I32Const(*align as i32));
                    func.instruction(&Instruction::Call(alloc_funcs.alloc));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *result) + local_offset,
                    ));
                }
            }

            LirOp::Free { ptr, size } => {
                if let Some(alloc_funcs) = &self.alloc_funcs {
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *ptr) + local_offset,
                    ));
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *size) + local_offset,
                    ));
                    func.instruction(&Instruction::Call(alloc_funcs.free));
                }
            }

            LirOp::MulConst {
                slot,
                constant,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *slot) + local_offset,
                ));
                func.instruction(&Instruction::I32Const(*constant as i32));
                func.instruction(&Instruction::I32Mul);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::AddSlots { a, b, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *a) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *b) + local_offset,
                ));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::SubSlots { a, b, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *a) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *b) + local_offset,
                ));
                func.instruction(&Instruction::I32Sub);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::LoadI32Addr { addr, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *addr) + local_offset,
                ));
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::StoreI32Addr { addr, value } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *addr) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *value) + local_offset,
                ));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }

            // === GC ops ===
            LirOp::StructNew {
                ty_idx,
                fields,
                result,
            } => {
                for field_slot in fields {
                    func.instruction(&Instruction::LocalGet(
                        slot_local(component, *field_slot) + local_offset,
                    ));
                }
                func.instruction(&Instruction::StructNew(*ty_idx));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::StructGet {
                ty_idx,
                field,
                rec,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *rec) + local_offset,
                ));
                func.instruction(&Instruction::StructGet {
                    struct_type_index: *ty_idx,
                    field_index: *field,
                });
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::StructSet {
                ty_idx,
                field,
                rec,
                value,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *rec) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *value) + local_offset,
                ));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: *ty_idx,
                    field_index: *field,
                });
            }

            LirOp::ArrayNewDefault {
                ty_idx,
                len,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *len) + local_offset,
                ));
                func.instruction(&Instruction::ArrayNewDefault(*ty_idx));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::ArrayGet {
                ty_idx,
                arr,
                idx,
                result,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *arr) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *idx) + local_offset,
                ));
                func.instruction(&Instruction::ArrayGet(*ty_idx));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::ArraySet {
                ty_idx,
                arr,
                idx,
                value,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *arr) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *idx) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *value) + local_offset,
                ));
                func.instruction(&Instruction::ArraySet(*ty_idx));
            }

            LirOp::ArrayCopy {
                dst_ty_idx,
                src_ty_idx,
                dst,
                dst_idx,
                src,
                src_idx,
                count,
            } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *dst) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *dst_idx) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *src) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *src_idx) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *count) + local_offset,
                ));
                func.instruction(&Instruction::ArrayCopy {
                    array_type_index_dst: *dst_ty_idx,
                    array_type_index_src: *src_ty_idx,
                });
            }

            LirOp::ArrayLen { arr, result } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *arr) + local_offset,
                ));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::RefAsNonNull { slot } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *slot) + local_offset,
                ));
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *slot) + local_offset,
                ));
            }

            LirOp::RefNull { ty_idx, result } => {
                func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                    *ty_idx,
                )));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::ChildrenArrayNewDefault {
                anchor_boundary,
                len,
                result,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].tree_for_arr_type_idx[anchor_boundary];
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *len) + local_offset,
                ));
                func.instruction(&Instruction::ArrayNewDefault(ty_idx));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::ChildrenArrayGet {
                anchor_boundary,
                arr,
                idx,
                result,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].tree_for_arr_type_idx[anchor_boundary];
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *arr) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *idx) + local_offset,
                ));
                func.instruction(&Instruction::ArrayGet(ty_idx));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }

            LirOp::ChildrenArraySet {
                anchor_boundary,
                arr,
                idx,
                value,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].tree_for_arr_type_idx[anchor_boundary];
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *arr) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *idx) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *value) + local_offset,
                ));
                func.instruction(&Instruction::ArraySet(ty_idx));
            }

            LirOp::ChildrenArrayCopy {
                anchor_boundary,
                dst,
                dst_idx,
                src,
                src_idx,
                count,
            } => {
                let ty_idx = self.gc_layouts[comp_idx].tree_for_arr_type_idx[anchor_boundary];
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *dst) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *dst_idx) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *src) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *src_idx) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *count) + local_offset,
                ));
                func.instruction(&Instruction::ArrayCopy {
                    array_type_index_dst: ty_idx,
                    array_type_index_src: ty_idx,
                });
            }

            LirOp::LoadList {
                signal,
                ptr_result,
                len_result,
            } => {
                // Resolve the backing fat-pointer storage. List-typed
                // signals may live in:
                //   1. component-local memory (legacy path),
                //   2. component-local `$Comp_<i>` GC struct (migrated),
                //   3. global singleton property in a per-block GC
                //      struct (migrated; lists are FatPointer = always
                //      migrated),
                //   4. global singleton property in linear memory
                //      (legacy path; only happens for not-yet-migrated
                //      pointer types — lists shouldn't fall here, but
                //      the dispatch is symmetric for safety).
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    if self.signal_in_struct(comp_idx, sig_idx) {
                        // Phase 5e.4: when the signal is GC-array-
                        // stored (list<string> etc.), materialize it
                        // via the per-array materializer to get
                        // (ptr, len) in canonical memory.
                        let signal_ty = component.signals[sig_idx].ty;
                        if let super::super::repr::InternalRepr::GcArrayRef(arr_idx) =
                            self.internal_repr(signal_ty)
                        {
                            let mat_fn = *self
                                .gc_list_materializer_fn_indices
                                .get(&arr_idx)
                                .ok_or_else(|| CodegenError::InvalidIR(
                                    "LoadList: missing materializer for GC list".into(),
                                ))?;
                            // Push the array ref via signal_struct_read,
                            // then call materializer (returns ptr, len).
                            self.emit_signal_struct_read(func, comp_idx, sig_idx)?;
                            func.instruction(&Instruction::Call(mat_fn));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *len_result) + local_offset,
                            ));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *ptr_result) + local_offset,
                            ));
                            return Ok(());
                        }
                        // Component-local migrated list: struct.get pushes
                        // (ptr, len) in order; pop into result locals
                        // (len on top, ptr underneath).
                        self.emit_signal_struct_read(func, comp_idx, sig_idx)?;
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *len_result) + local_offset,
                        ));
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *ptr_result) + local_offset,
                        ));
                        return Ok(());
                    }
                    let addr = layout.signal_addr(sig_idx);
                    func.instruction(&Instruction::I32Const(addr));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *ptr_result) + local_offset,
                    ));
                    func.instruction(&Instruction::I32Const(addr + 4));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *len_result) + local_offset,
                    ));
                } else if self.ctx.defs.owning_global_block(*signal).is_some() {
                    if self.global_in_struct(*signal) {
                        // Phase 6: typed-array global property — call the
                        // per-array materializer to get (ptr, len), then
                        // store into result locals. Mirrors the component
                        // GC-array-signal path above.
                        let signal_ty = self
                            .ctx
                            .defs
                            .type_of(*signal)
                            .unwrap_or(yel_core::types::Ty::ERROR);
                        if let super::super::repr::InternalRepr::GcArrayRef(arr_idx) =
                            self.internal_repr(signal_ty)
                        {
                            let mat_fn = *self
                                .gc_list_materializer_fn_indices
                                .get(&arr_idx)
                                .ok_or_else(|| CodegenError::InvalidIR(
                                    "LoadList (global): missing materializer for GC list".into(),
                                ))?;
                            self.emit_global_struct_read(func, *signal)?;
                            func.instruction(&Instruction::Call(mat_fn));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *len_result) + local_offset,
                            ));
                            func.instruction(&Instruction::LocalSet(
                                slot_local(component, *ptr_result) + local_offset,
                            ));
                            return Ok(());
                        }
                        // 2-slot fat-pointer in struct (legacy migrated).
                        self.emit_global_struct_read(func, *signal)?;
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *len_result) + local_offset,
                        ));
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *ptr_result) + local_offset,
                        ));
                    } else if let Some(&global_addr) = self.global_property_addrs.get(signal) {
                        let addr = global_addr;
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *ptr_result) + local_offset,
                        ));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        func.instruction(&Instruction::LocalSet(
                            slot_local(component, *len_result) + local_offset,
                        ));
                    } else {
                        return Err(CodegenError::InvalidIR(format!(
                            "LoadList: pointer-typed global property {:?} has no \
                             memory address",
                            signal
                        )));
                    }
                } else {
                    return Err(CodegenError::InvalidIR(format!(
                        "LoadList: signal {:?} is neither a local signal \
                         of component `{}` nor a known global property",
                        signal,
                        self.ctx.str(component.name)
                    )));
                }
            }

            LirOp::SetSlot { slot, value } => {
                func.instruction(&Instruction::I32Const(*value));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *slot) + local_offset,
                ));
            }

            LirOp::CopySlot { from, to } => {
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *from) + local_offset,
                ));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *to) + local_offset,
                ));
            }

            LirOp::GetSlotAddress { mem_slot, result } => {
                // Get the memory offset of a memory slot and store it in result
                if let Some(slot_info) = component.slots.get(mem_slot.0 as usize)
                    && let LirSlotKind::Memory { offset, .. } = slot_info.kind
                {
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::LocalSet(
                        slot_local(component, *result) + local_offset,
                    ));
                }
            }

            LirOp::EvalListExpr {
                expr,
                ptr_result,
                len_result,
            } => {
                // Evaluate the list expression - it leaves (ptr, len) on the stack
                let list_expr = component.get_expr(*expr);
                self.emit_expr(func, list_expr, component, &layout)?;
                // Stack now has: [ptr, len]
                // Store len first (it's on top), then ptr
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *len_result) + local_offset,
                ));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *ptr_result) + local_offset,
                ));
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
                let sig_idx = self.signal_index_in(component, *signal).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "LoadListGc: signal {:?} is not a local signal of component `{}`",
                        signal,
                        self.ctx.str(component.name)
                    ))
                })?;
                // Push the typed array ref via the component-struct field
                // path. The helper emits `local.get $self; struct.get
                // $comp_<name> $<sig>` (potentially chained for nested
                // sub-structs).
                self.emit_signal_struct_read(func, comp_idx, sig_idx)?;
                // Tee into ref_result so we can also feed array.len.
                func.instruction(&Instruction::LocalTee(
                    slot_local(component, *ref_result) + local_offset,
                ));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *len_result) + local_offset,
                ));
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
                self.emit_expr(func, list_expr, component, &layout)?;
                func.instruction(&Instruction::LocalTee(
                    slot_local(component, *ref_result) + local_offset,
                ));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *len_result) + local_offset,
                ));
            }

            LirOp::ArrayGetItem {
                arr,
                idx,
                list_ty,
                result,
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
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *arr) + local_offset,
                ));
                func.instruction(&Instruction::LocalGet(
                    slot_local(component, *idx) + local_offset,
                ));
                func.instruction(&Instruction::ArrayGet(ty_idx));
                func.instruction(&Instruction::LocalSet(
                    slot_local(component, *result) + local_offset,
                ));
            }
        }
        Ok(())
    }
}
