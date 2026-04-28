//! Record / list / filter constructor function emission. These helpers
//! are emitted into the runtime-functions region of the code section
//! and shared across all components that need them.

use wasm_encoder::{BlockType, Function, Instruction, ValType};
use yel_core::lir::{LirComponent, LirExpr, LirExprKind};
use yel_core::types::InternedTyKind;
use yel_core::ids::LocalId;
use yel_core::{DefId, DefKind, Ty};

use super::super::CodegenError;
use super::super::{MemoryLayout, WasmPackageBuilder};
use super::scratch::mem_arg;

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn generate_record_ctor_at(
        &mut self,
        record_def: DefId,
        _alloc_idx: u32,
    ) -> Result<Function, CodegenError> {
        let rec_def = self.ctx.defs.as_record(record_def).ok_or_else(|| {
            CodegenError::InvalidIR(format!("Expected record definition for {:?}", record_def))
        })?;

        let mut func = Function::new([]);

        // Parameter layout: param 0 is the dest address; subsequent params are
        // the canonical-ABI flat slots of each field in declaration order.
        // For each field we compute its flat_slots (offsets relative to the
        // field's own base) and store each slot at (dest + field_offset +
        // slot.offset) using the store width recorded for the slot.
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!("record layout missing for {:?}", record_def))
            })?;
        let field_count = rec_def.fields.len();
        let mut param_idx = 1u32;
        for i in 0..field_count {
            let (_, field_off, field_ty) = layout.field_offsets[i].clone();
            let slots = self.flatten_core_slots(field_ty);
            for slot in &slots {
                func.instruction(&Instruction::LocalGet(0)); // dest
                let slot_abs = field_off + slot.offset;
                if slot_abs > 0 {
                    func.instruction(&Instruction::I32Const(slot_abs as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(param_idx));
                slot.store.emit_store(&mut func);
                param_idx += 1;
            }
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Generate record constructor that allocates and returns a pointer.
    ///
    /// Signature: (...fields) -> ptr: i32
    ///
    /// This is the CONVENIENCE variant - allocates memory, calls ctor_at, returns ptr.
    pub(super) fn generate_record_ctor(
        &mut self,
        record_def: DefId,
        alloc_idx: u32,
    ) -> Result<Function, CodegenError> {
        let rec_def = self.ctx.defs.as_record(record_def).ok_or_else(|| {
            CodegenError::InvalidIR(format!("Expected record definition for {:?}", record_def))
        })?;

        // Need one local for the allocated pointer
        let mut func = Function::new([(1, ValType::I32)]);

        // Calculate total size
        let mut total_size = 0u32;
        for &field_def_id in &rec_def.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                DefKind::Field(f) => f.ty,
                _ => continue,
            };
            total_size += self.layout_ctx.size_of(field_ty);
        }

        // Allocate memory
        func.instruction(&Instruction::I32Const(total_size as i32));
        func.instruction(&Instruction::I32Const(4)); // alignment
        func.instruction(&Instruction::Call(alloc_idx));

        // Get param count to know local index for ptr
        let param_count = self.count_record_wasm_params(record_def) as u32;
        let ptr_local = param_count; // First local after params

        func.instruction(&Instruction::LocalTee(ptr_local));

        // Push all params for ctor_at call
        for i in 0..param_count {
            func.instruction(&Instruction::LocalGet(i));
        }

        // Call ctor_at
        let ctor_at_idx = self
            .runtime_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("Runtime functions not initialized".to_string())
            })?
            .record_ctor_at(record_def)
            .ok_or_else(|| CodegenError::InvalidIR(format!("No ctor_at for {:?}", record_def)))?;
        func.instruction(&Instruction::Call(ctor_at_idx));

        // Return the pointer
        func.instruction(&Instruction::LocalGet(ptr_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Generate a list constructor helper function.
    /// Takes element values as params, allocates memory, stores elements, returns (ptr, len).
    pub(super) fn generate_list_ctor(
        &mut self,
        elem_ty: Ty,
        count: usize,
        alloc_idx: u32,
    ) -> Result<Function, CodegenError> {
        // Get element size and param count
        let elem_size = self.layout_ctx.size_of(elem_ty);
        let total_size = elem_size * count as u32;
        let params_per_elem = self.count_type_wasm_params(elem_ty);
        let total_params = count * params_per_elem;

        // Need one local for the allocated pointer
        let mut func = Function::new([(1, ValType::I32)]);
        let ptr_local = total_params as u32; // First local after params

        // Allocate memory for the list
        func.instruction(&Instruction::I32Const(total_size as i32));
        func.instruction(&Instruction::I32Const(4)); // alignment
        func.instruction(&Instruction::Call(alloc_idx));
        func.instruction(&Instruction::LocalSet(ptr_local));

        // Store each element
        let mut param_idx = 0u32;
        for i in 0..count {
            let elem_offset = (i as u32) * elem_size;

            match self.ctx.ty_kind(elem_ty) {
                InternedTyKind::String | InternedTyKind::List(_) => {
                    // Fat pointer: store ptr at offset, len at offset+4
                    func.instruction(&Instruction::LocalGet(ptr_local));
                    func.instruction(&Instruction::LocalGet(param_idx));
                    func.instruction(&Instruction::I32Store(mem_arg(elem_offset as u64, 2)));
                    func.instruction(&Instruction::LocalGet(ptr_local));
                    func.instruction(&Instruction::LocalGet(param_idx + 1));
                    func.instruction(&Instruction::I32Store(mem_arg((elem_offset + 4) as u64, 2)));
                    param_idx += 2;
                }
                InternedTyKind::Adt(def_id) => {
                    // Record: store each field at its offset within the element
                    if let Some(rec_def) = self.ctx.defs.as_record(*def_id) {
                        let mut field_offset = 0u32;
                        for &field_def_id in &rec_def.fields {
                            let field_ty = match self.ctx.defs.kind(field_def_id) {
                                DefKind::Field(f) => f.ty,
                                _ => continue,
                            };
                            let field_size = self.layout_ctx.size_of(field_ty);

                            match self.ctx.ty_kind(field_ty) {
                                InternedTyKind::String | InternedTyKind::List(_) => {
                                    // Fat pointer field
                                    func.instruction(&Instruction::LocalGet(ptr_local));
                                    func.instruction(&Instruction::LocalGet(param_idx));
                                    func.instruction(&Instruction::I32Store(mem_arg(
                                        (elem_offset + field_offset) as u64,
                                        2,
                                    )));
                                    func.instruction(&Instruction::LocalGet(ptr_local));
                                    func.instruction(&Instruction::LocalGet(param_idx + 1));
                                    func.instruction(&Instruction::I32Store(mem_arg(
                                        (elem_offset + field_offset + 4) as u64,
                                        2,
                                    )));
                                    param_idx += 2;
                                }
                                _ => {
                                    // Simple field
                                    func.instruction(&Instruction::LocalGet(ptr_local));
                                    func.instruction(&Instruction::LocalGet(param_idx));
                                    func.instruction(&Instruction::I32Store(mem_arg(
                                        (elem_offset + field_offset) as u64,
                                        2,
                                    )));
                                    param_idx += 1;
                                }
                            }
                            field_offset += field_size;
                        }
                    } else {
                        // Non-record ADT (enum/variant) - single i32
                        func.instruction(&Instruction::LocalGet(ptr_local));
                        func.instruction(&Instruction::LocalGet(param_idx));
                        func.instruction(&Instruction::I32Store(mem_arg(elem_offset as u64, 2)));
                        param_idx += 1;
                    }
                }
                _ => {
                    // Primitive element: emit the store matching the WASM
                    // value type so f32/f64/i64 elements validate. The store
                    // width uses the element size layout (size_of(elem_ty))
                    // so narrow integer types (i8/i16) are stored compactly.
                    func.instruction(&Instruction::LocalGet(ptr_local));
                    func.instruction(&Instruction::LocalGet(param_idx));
                    match self.ctx.ty_kind(elem_ty) {
                        InternedTyKind::F32
                        | InternedTyKind::Length
                        | InternedTyKind::PhysicalLength
                        | InternedTyKind::Angle
                        | InternedTyKind::Duration
                        | InternedTyKind::Percent
                        | InternedTyKind::RelativeFontSize => {
                            func.instruction(&Instruction::F32Store(mem_arg(
                                elem_offset as u64,
                                2,
                            )));
                        }
                        InternedTyKind::F64 => {
                            func.instruction(&Instruction::F64Store(mem_arg(
                                elem_offset as u64,
                                3,
                            )));
                        }
                        InternedTyKind::S64 | InternedTyKind::U64 => {
                            func.instruction(&Instruction::I64Store(mem_arg(
                                elem_offset as u64,
                                3,
                            )));
                        }
                        InternedTyKind::Bool | InternedTyKind::S8 | InternedTyKind::U8 => {
                            func.instruction(&Instruction::I32Store8(mem_arg(
                                elem_offset as u64,
                                0,
                            )));
                        }
                        InternedTyKind::S16 | InternedTyKind::U16 => {
                            func.instruction(&Instruction::I32Store16(mem_arg(
                                elem_offset as u64,
                                1,
                            )));
                        }
                        _ => {
                            func.instruction(&Instruction::I32Store(mem_arg(
                                elem_offset as u64,
                                2,
                            )));
                        }
                    }
                    param_idx += 1;
                }
            }
        }

        // Return (ptr, len)
        func.instruction(&Instruction::LocalGet(ptr_local));
        func.instruction(&Instruction::I32Const(count as i32));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Extract all SignalRead DefIds from an expression (used for filter captured signals).
    pub(crate) fn extract_signal_reads(&self, expr: &LirExpr, signals: &mut Vec<(DefId, Ty)>) {
        match &expr.kind {
            LirExprKind::SignalRead(def_id) => {
                if !signals.iter().any(|(id, _)| id == def_id) {
                    signals.push((*def_id, expr.ty));
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.extract_signal_reads(lhs, signals);
                self.extract_signal_reads(rhs, signals);
            }
            LirExprKind::Unary { operand, .. } => {
                self.extract_signal_reads(operand, signals);
            }
            LirExprKind::Field { base, .. } => {
                self.extract_signal_reads(base, signals);
            }
            LirExprKind::Call { args, .. } => {
                for arg in args {
                    self.extract_signal_reads(arg, signals);
                }
            }
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.extract_signal_reads(condition, signals);
                self.extract_signal_reads(then_expr, signals);
                self.extract_signal_reads(else_expr, signals);
            }
            _ => {}
        }
    }

    /// Generate a specialized filter function for a specific call site.
    /// Takes (src_ptr, src_len, [captured_signals...]) and returns (result_ptr, result_len).
    /// The predicate expression is inlined into the loop.
    // Args span unrelated layers (filter id, element type/size, predicate AST,
    // alloc helper idx, owning component, memory layout). They don't form a
    // single coherent group, so a wrapper struct would not aid readability.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn generate_filter_function(
        &mut self,
        _filter_id: usize,
        _elem_ty: Ty,
        elem_size: u32,
        param: (LocalId, Ty),
        predicate: LirExpr,
        alloc_idx: u32,
        component: &LirComponent,
        layout: &MemoryLayout,
    ) -> Result<Function, CodegenError> {
        // Extract captured signals from predicate
        let mut captured_signals: Vec<(DefId, Ty)> = Vec::new();
        self.extract_signal_reads(&predicate, &mut captured_signals);

        // Calculate param count: src_ptr, src_len + captured signals (strings/lists need 2)
        let mut next_param_idx: u32 = 2; // After src_ptr, src_len
        let mut captured_signal_map = std::collections::HashMap::new();
        for (def_id, ty) in &captured_signals {
            let is_fat_ptr = matches!(
                self.ctx.ty_kind(*ty),
                InternedTyKind::String | InternedTyKind::List(_)
            );
            captured_signal_map.insert(*def_id, (next_param_idx, is_fat_ptr));
            next_param_idx += if is_fat_ptr { 2 } else { 1 };
        }

        // Locals start after all params: result_ptr, result_count, loop_index, item_ptr
        let result_ptr_local = next_param_idx;
        let result_count_local = next_param_idx + 1;
        let loop_index_local = next_param_idx + 2;
        let item_ptr_local = next_param_idx + 3;

        let mut func = Function::new([(4, ValType::I32)]);

        // Allocate result buffer: src_len * elem_size
        func.instruction(&Instruction::LocalGet(1)); // src_len
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Const(4)); // alignment
        func.instruction(&Instruction::Call(alloc_idx));
        func.instruction(&Instruction::LocalSet(result_ptr_local));

        // Initialize result_count = 0, loop_index = 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(result_count_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(loop_index_local));

        // Loop: for each element in source list
        func.instruction(&Instruction::Block(BlockType::Empty));
        func.instruction(&Instruction::Loop(BlockType::Empty));

        // Check break condition: loop_index >= src_len
        func.instruction(&Instruction::LocalGet(loop_index_local));
        func.instruction(&Instruction::LocalGet(1)); // src_len
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));

        // Compute item_ptr = src_ptr + loop_index * elem_size
        func.instruction(&Instruction::LocalGet(0)); // src_ptr
        func.instruction(&Instruction::LocalGet(loop_index_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(item_ptr_local));

        // Set up captured locals for predicate emission
        let (param_local_id, _param_ty) = param;
        let old_captured = self.current_block_captured_locals.take();
        let mut captured_map = std::collections::HashMap::new();
        captured_map.insert(param_local_id, item_ptr_local);
        self.current_block_captured_locals = Some(captured_map);

        // Filter closures route the item param via the captured-locals
        // map and the slot holds an item-ptr address — `BindingMode::Ptr`
        // (the default when no mode is registered) preserves today's
        // typed-load behavior. Stash and clear `current_block_local_modes`
        // so any outer-scope `Value` entries don't accidentally apply
        // here.
        let old_modes = self.current_block_local_modes.take();

        // Set up captured signals mapping
        let old_signal_map = self.current_filter_captured_signals.take();
        self.current_filter_captured_signals = Some(captured_signal_map);

        // Emit predicate expression using the full emit_expr (result on stack: 0 or 1)
        self.emit_expr(&mut func, &predicate, component, layout)?;

        // Restore state
        self.current_block_captured_locals = old_captured;
        self.current_block_local_modes = old_modes;
        self.current_filter_captured_signals = old_signal_map;

        // If predicate is true, copy item to result
        func.instruction(&Instruction::If(BlockType::Empty));

        // dest_ptr = result_ptr + result_count * elem_size
        func.instruction(&Instruction::LocalGet(result_ptr_local));
        func.instruction(&Instruction::LocalGet(result_count_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);

        // Copy elem_size bytes from item_ptr to dest_ptr
        func.instruction(&Instruction::LocalGet(item_ptr_local)); // src
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });

        // result_count++
        func.instruction(&Instruction::LocalGet(result_count_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(result_count_local));

        func.instruction(&Instruction::End); // end if

        // loop_index++
        func.instruction(&Instruction::LocalGet(loop_index_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(loop_index_local));

        // Continue loop
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // end loop
        func.instruction(&Instruction::End); // end block

        // Return (result_ptr, result_count)
        func.instruction(&Instruction::LocalGet(result_ptr_local));
        func.instruction(&Instruction::LocalGet(result_count_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }
}
