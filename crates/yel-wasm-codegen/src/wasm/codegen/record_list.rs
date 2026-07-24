//! Record / list / filter constructor function emission. These helpers
//! are emitted into the runtime-functions region of the code section
//! and shared across all components that need them.

use wasm_encoder::{BlockType, Function, Instruction, ValType};
use yel_core::lir::arena::LirResourceArena;
use yel_core::lir::{LirExpr, LirExprKind};
use yel_core::types::InternedTyKind;
use yel_core::ids::LocalId;
use yel_core::{DefId, DefKind, Ty};

use super::super::CodegenError;
use super::super::WasmPackageBuilder;
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
    pub(crate) fn extract_signal_reads(
        &self,
        expr: &LirExpr,
        exprs: &[LirExpr],
        signals: &mut Vec<(DefId, Ty)>,
    ) {
        match &expr.kind {
            LirExprKind::SignalRead(def_id) => {
                if !signals.iter().any(|(id, _)| id == def_id) {
                    signals.push((*def_id, expr.ty));
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.extract_signal_reads(&exprs[lhs.0 as usize], exprs, signals);
                self.extract_signal_reads(&exprs[rhs.0 as usize], exprs, signals);
            }
            LirExprKind::Unary { operand, .. } => {
                self.extract_signal_reads(&exprs[operand.0 as usize], exprs, signals);
            }
            LirExprKind::Field { base, .. } => {
                self.extract_signal_reads(&exprs[base.0 as usize], exprs, signals);
            }
            LirExprKind::Call { args, .. } => {
                for arg in args {
                    self.extract_signal_reads(&exprs[arg.0 as usize], exprs, signals);
                }
            }
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.extract_signal_reads(&exprs[condition.0 as usize], exprs, signals);
                self.extract_signal_reads(&exprs[then_expr.0 as usize], exprs, signals);
                self.extract_signal_reads(&exprs[else_expr.0 as usize], exprs, signals);
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
        list_ty: Ty,
        _elem_size: u32,
        param: (LocalId, Ty),
        predicate: LirExpr,
        _alloc_idx: u32,
        component: &dyn LirResourceArena,
    ) -> Result<Function, CodegenError> {
        // Stage 6 of typed-GC migration: filter operates entirely in
        // typed-GC space. Param 0 is `(ref null $list_arr)` (was
        // src_ptr+src_len), captured signals are passed in their
        // `signal_storage_valtypes` shape (was always coerced to
        // canonical (ptr, len) i32s for fat types). Result is one
        // `(ref null $list_arr)` (was result_ptr+result_count).
        let elem_ty = match self.ctx.ty_kind(list_ty) {
            InternedTyKind::List(e) => *e,
            _ => {
                return Err(CodegenError::InvalidIR(format!(
                    "filter: list_ty {:?} is not a list type",
                    list_ty
                )));
            }
        };
        let arr_type_idx = *self
            .record_gc_types
            .list_array_type_idx
            .get(&list_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "filter: missing list_array_type_idx for {:?}",
                    list_ty
                ))
            })?;
        let arr_ref_ty = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
        });

        // Extract captured signals from predicate.
        let mut captured_signals: Vec<(DefId, Ty)> = Vec::new();
        self.extract_signal_reads(&predicate, component.exprs(), &mut captured_signals);

        // Build param slot map: param 0 is src_arr; captured signals
        // start at param 1, each consuming `signal_storage_valtypes`
        // slots. `is_fat_ptr` in the captured-signal map means "occupies
        // 2 consecutive locals", which post Stage 6 only happens for
        // String signals (FatPointer repr). Typed list signals collapse
        // to 1 typed ref slot.
        let mut next_param_idx: u32 = 1; // After src_arr
        let mut captured_signal_map = rustc_hash::FxHashMap::default();
        for (def_id, ty) in &captured_signals {
            let storage = self.signal_storage_valtypes(*ty);
            let is_fat_ptr = storage.len() == 2;
            captured_signal_map.insert(*def_id, (next_param_idx, is_fat_ptr));
            next_param_idx += storage.len() as u32;
        }

        // Stage 6: typed-GC body. Item is `array.get`'d directly to its
        // element type (typed ref for records / tuples / GcVariant
        // / nested lists / strings-as-$fat_value, scalar for prims).
        // Locals: src_len i32, scratch_arr (ref), result_count i32,
        // loop_index i32, item_local (typed elem), final_arr (ref).
        let src_len_local = next_param_idx;
        let scratch_arr_local = next_param_idx + 1;
        let result_count_local = next_param_idx + 2;
        let loop_index_local = next_param_idx + 3;
        let item_local = next_param_idx + 4;
        let final_arr_local = next_param_idx + 5;

        // Element local type matches the array element storage type.
        let item_val_ty = super::super::gc_types::list_element_storage_type_pub(
            self.ctx,
            elem_ty,
            &self.record_gc_types,
        );

        let local_decls: Vec<(u32, ValType)> = vec![
            (1, ValType::I32),       // src_len
            (1, arr_ref_ty),         // scratch_arr
            (1, ValType::I32),       // result_count
            (1, ValType::I32),       // loop_index
            (1, item_val_ty),        // item
            (1, arr_ref_ty),         // final_arr
        ];
        let mut func = Function::new(local_decls);

        // src_len = array.len(src_arr)
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(src_len_local));

        // scratch_arr = array.new_default $list_arr (src_len) — full
        // capacity. Survivors fill [0..result_count]; final tightening
        // copies just that prefix into a right-sized array via
        // array.copy.
        func.instruction(&Instruction::LocalGet(src_len_local));
        func.instruction(&Instruction::ArrayNewDefault(arr_type_idx));
        func.instruction(&Instruction::LocalSet(scratch_arr_local));

        // result_count = 0; loop_index = 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(result_count_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(loop_index_local));

        // Loop: for each element in source list
        func.instruction(&Instruction::Block(BlockType::Empty));
        func.instruction(&Instruction::Loop(BlockType::Empty));

        // break: loop_index >= src_len
        func.instruction(&Instruction::LocalGet(loop_index_local));
        func.instruction(&Instruction::LocalGet(src_len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));

        // item = array.get $list_arr (src_arr, loop_index)
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::LocalGet(loop_index_local));
        func.instruction(&Instruction::ArrayGet(arr_type_idx));
        func.instruction(&Instruction::LocalSet(item_local));

        // Set up captured locals for predicate emission. Bind the iter
        // param to `item_local` with `Value` mode — the predicate's
        // accesses (Index, Field, IsCase, etc.) work against the typed
        // ref / unboxed scalar directly.
        let (param_local_id, _param_ty) = param;
        let old_captured = self.current_block_captured_locals.take();
        let old_modes = self.current_block_local_modes.take();
        let mut captured_map = rustc_hash::FxHashMap::default();
        let mut local_modes = rustc_hash::FxHashMap::default();
        captured_map.insert(param_local_id, item_local);
        local_modes.insert(param_local_id, yel_core::lir::LirBindingMode::Value);
        self.current_block_captured_locals = Some(captured_map);
        self.current_block_local_modes = Some(local_modes);

        // Captured signals mapping (typed-shape, see above).
        let old_signal_map = self.current_filter_captured_signals.take();
        self.current_filter_captured_signals = Some(captured_signal_map);

        // Emit predicate (result on stack: i32 0 or 1).
        self.emit_expr(&mut func, &predicate, component)?;

        self.current_block_captured_locals = old_captured;
        self.current_block_local_modes = old_modes;
        self.current_filter_captured_signals = old_signal_map;

        // if predicate { array.set scratch_arr result_count item; result_count++ }
        func.instruction(&Instruction::If(BlockType::Empty));
        func.instruction(&Instruction::LocalGet(scratch_arr_local));
        func.instruction(&Instruction::LocalGet(result_count_local));
        func.instruction(&Instruction::LocalGet(item_local));
        func.instruction(&Instruction::ArraySet(arr_type_idx));
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

        // Tighten: final_arr = array.new_default(result_count);
        // array.copy(final_arr, 0, scratch_arr, 0, result_count);
        // return final_arr.
        func.instruction(&Instruction::LocalGet(result_count_local));
        func.instruction(&Instruction::ArrayNewDefault(arr_type_idx));
        func.instruction(&Instruction::LocalSet(final_arr_local));
        // array.copy (dst, dst_idx, src, src_idx, count)
        func.instruction(&Instruction::LocalGet(final_arr_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(scratch_arr_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(result_count_local));
        func.instruction(&Instruction::ArrayCopy {
            array_type_index_dst: arr_type_idx,
            array_type_index_src: arr_type_idx,
        });
        // Return the typed final array.
        func.instruction(&Instruction::LocalGet(final_arr_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Generate `list_append_$listTy(src, elem) -> new_list`.
    ///
    /// Immutable append: allocates a new GC array of length src.len()+1,
    /// copies the source elements into [0..src.len()), writes `elem`
    /// into index src.len(), and returns the new array.
    ///
    /// Signature: `(ref null $list_arr, <elem-storage>) -> (ref null $list_arr)`.
    /// `src` is unwrapped via `array.len` directly — a null receiver
    /// traps inside `array.len`, matching the rest of the typed-array
    /// path (filter, index, etc.). Source code today never produces a
    /// null list (every list signal is initialized to `[]` or a default).
    pub(super) fn generate_list_append_function(
        &mut self,
        list_ty: Ty,
    ) -> Result<Function, CodegenError> {
        let elem_ty = match self.ctx.ty_kind(list_ty) {
            InternedTyKind::List(e) => *e,
            _ => {
                return Err(CodegenError::InvalidIR(format!(
                    "append: list_ty {:?} is not a list type",
                    list_ty
                )));
            }
        };
        let arr_type_idx = *self
            .record_gc_types
            .list_array_type_idx
            .get(&list_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "append: missing list_array_type_idx for {:?}",
                    list_ty
                ))
            })?;
        let arr_ref_ty = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
        });

        // Params: 0 = src (ref null $arr), 1 = elem (<elem-storage>).
        // Locals: src_len i32, new_arr (ref null $arr).
        let src_len_local = 2u32;
        let new_arr_local = 3u32;
        let local_decls: Vec<(u32, ValType)> = vec![
            (1, ValType::I32),
            (1, arr_ref_ty),
        ];
        let mut func = Function::new(local_decls);

        // src_len = array.len(src)
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(src_len_local));

        // new_arr = array.new_default $arr (src_len + 1)
        func.instruction(&Instruction::LocalGet(src_len_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::ArrayNewDefault(arr_type_idx));
        func.instruction(&Instruction::LocalSet(new_arr_local));

        // array.copy new_arr [0 ..] <- src [0 .. src_len]
        func.instruction(&Instruction::LocalGet(new_arr_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalGet(src_len_local));
        func.instruction(&Instruction::ArrayCopy {
            array_type_index_dst: arr_type_idx,
            array_type_index_src: arr_type_idx,
        });

        // array.set new_arr[src_len] = elem (param 1)
        func.instruction(&Instruction::LocalGet(new_arr_local));
        func.instruction(&Instruction::LocalGet(src_len_local));
        func.instruction(&Instruction::LocalGet(1));
        func.instruction(&Instruction::ArraySet(arr_type_idx));

        // return new_arr
        func.instruction(&Instruction::LocalGet(new_arr_local));
        func.instruction(&Instruction::End);
        let _ = elem_ty; // referenced only for the type-section param shape upstream
        Ok(func)
    }

    /// Generate `list_get_$listTy(src, idx) -> option<T>`.
    ///
    /// Bounds-checked read: `some(src[idx])` when `0 <= idx < len`, else
    /// `none`. The unsigned compare `(u32)idx < len` folds the negative case
    /// into out-of-range with one instruction. A null `src` traps in
    /// `array.len`, matching append / index.
    ///
    /// Signature: `(ref null $list_arr, i32) -> <option repr valtype>`.
    /// The option is built in whichever representation `internal_repr` assigns
    /// it: a `GcVariant` (struct.new of the some/none case subtype) or a
    /// nullable-ref collapse (the element ref itself for some, ref.null for
    /// none).
    pub(super) fn generate_list_get_function(
        &mut self,
        list_ty: Ty,
        option_ty: Ty,
    ) -> Result<Function, CodegenError> {
        use super::super::repr::InternalRepr;

        let arr_type_idx = *self
            .record_gc_types
            .list_array_type_idx
            .get(&list_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "list-get: missing list_array_type_idx for {:?}",
                    list_ty
                ))
            })?;

        let opt_repr = self.internal_repr(option_ty);
        let result_vt = match self.signal_storage_valtypes(option_ty).as_slice() {
            [vt] => *vt,
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "list-get: option result {:?} is not a single-slot ref (got {} slots)",
                    option_ty,
                    other.len()
                )));
            }
        };

        // Params: 0 = src (ref null $arr), 1 = idx (i32). No extra locals.
        let mut func = Function::new(Vec::new());

        // if ((u32)idx < array.len(src)) { some } else { none }
        func.instruction(&Instruction::LocalGet(1)); // idx
        func.instruction(&Instruction::LocalGet(0)); // src
        func.instruction(&Instruction::ArrayLen); // len
        func.instruction(&Instruction::I32LtU);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(result_vt)));

        // some branch
        match opt_repr {
            InternalRepr::GcVariant(_) => {
                // some case (idx 0) carries the element as its single field.
                let some_sub = *self
                    .record_gc_types
                    .gc_variant_case_idx
                    .get(&(option_ty, 0))
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "list-get: missing some-case subtype for {:?}",
                            option_ty
                        ))
                    })?;
                func.instruction(&Instruction::LocalGet(0));
                func.instruction(&Instruction::LocalGet(1));
                func.instruction(&Instruction::ArrayGet(arr_type_idx));
                func.instruction(&Instruction::StructNew(some_sub));
            }
            InternalRepr::GcRef(_) | InternalRepr::GcArrayRef(_) => {
                // Nullable-ref collapse: some(x) == x, so the element ref is
                // the option value directly.
                func.instruction(&Instruction::LocalGet(0));
                func.instruction(&Instruction::LocalGet(1));
                func.instruction(&Instruction::ArrayGet(arr_type_idx));
            }
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "list-get: unexpected option repr {:?} for {:?}",
                    other, option_ty
                )));
            }
        }

        func.instruction(&Instruction::Else);

        // none branch
        match opt_repr {
            InternalRepr::GcVariant(_) => {
                let none_sub = *self
                    .record_gc_types
                    .gc_variant_case_idx
                    .get(&(option_ty, 1))
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "list-get: missing none-case subtype for {:?}",
                            option_ty
                        ))
                    })?;
                func.instruction(&Instruction::StructNewDefault(none_sub));
            }
            InternalRepr::GcRef(idx) | InternalRepr::GcArrayRef(idx) => {
                func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(idx)));
            }
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "list-get: unexpected option repr {:?} for {:?}",
                    other, option_ty
                )));
            }
        }

        func.instruction(&Instruction::End); // end if
        func.instruction(&Instruction::End); // end function
        Ok(func)
    }
}
