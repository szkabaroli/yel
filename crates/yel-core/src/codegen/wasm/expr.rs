//! Expression emission for WASM codegen.
//!
//! This module contains all expression emission functions used by the core module
//! code generator. These are implemented as methods on `WasmPackageBuilder`.

use wasm_encoder::{BlockType, Function, Instruction};

use crate::codegen::CodegenError;
use crate::hir::expr::{BinOp, UnaryOp};
use crate::lir::{LirComponent, LirExpr, LirExprKind, LirLiteral};
use crate::types::InternedTyKind;

use super::core_module::mem_arg;
use super::{MemoryLayout, WasmPackageBuilder};

impl WasmPackageBuilder<'_> {
    pub(super) fn emit_expr(
        &mut self,
        func: &mut Function,
        expr: &LirExpr,
        component: &LirComponent,
        layout: &MemoryLayout,
    ) -> Result<(), CodegenError> {
        match &expr.kind {
            LirExprKind::Literal(lit) => {
                self.emit_literal(func, lit, expr.ty);
            }

            LirExprKind::Local(local_id) => {
                let local_offset = self.current_block_local_offset.unwrap_or(0);

                // Check if this local is captured in the current block (passed as parameter)
                // For for-loop items, captured locals are pointers to list items
                if let Some(captured_map) = &self.current_block_captured_locals {
                    if let Some(&slot) = captured_map.get(local_id) {
                        // Local is captured - the slot contains a pointer to the item
                        let local_idx = slot + local_offset;
                        func.instruction(&Instruction::LocalGet(local_idx));

                        // Dereference based on type
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::Bool | InternedTyKind::Char => {
                                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            }
                            InternedTyKind::S8 | InternedTyKind::U8 => {
                                func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                            }
                            InternedTyKind::S16 | InternedTyKind::U16 => {
                                func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                            }
                            InternedTyKind::S64 | InternedTyKind::U64 => {
                                func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                            }
                            InternedTyKind::F32 => {
                                func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                            }
                            InternedTyKind::F64 => {
                                func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                            }
                            // String/List: load fat pointer (ptr, len)
                            InternedTyKind::String | InternedTyKind::List(_) => {
                                let runtime_funcs = self.runtime_funcs.as_ref()
                                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                                func.instruction(&Instruction::Call(runtime_funcs.load_fat_ptr));
                            }
                            // Record types (Adt) - return pointer as-is (field access will use it)
                            _ => {}
                        }
                        return Ok(());
                    }
                }

                // Check if this local is computed inline (e.g., for-loop item ptr)
                if let Some(local_to_slot) = &self.current_block_local_to_slot {
                    if let Some(slot_id) = local_to_slot.get(local_id) {
                        // Local is in an inline-computed slot - this is a pointer to the item
                        let local_idx = slot_id.0 + local_offset;
                        func.instruction(&Instruction::LocalGet(local_idx));

                        // Dereference based on type
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::Bool | InternedTyKind::Char => {
                                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            }
                            InternedTyKind::S8 | InternedTyKind::U8 => {
                                func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                            }
                            InternedTyKind::S16 | InternedTyKind::U16 => {
                                func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                            }
                            InternedTyKind::S64 | InternedTyKind::U64 => {
                                func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                            }
                            InternedTyKind::F32 => {
                                func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                            }
                            InternedTyKind::F64 => {
                                func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                            }
                            // String/List: load fat pointer (ptr, len)
                            InternedTyKind::String | InternedTyKind::List(_) => {
                                let runtime_funcs = self.runtime_funcs.as_ref()
                                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                                func.instruction(&Instruction::Call(runtime_funcs.load_fat_ptr));
                            }
                            // Record types (Adt) - return pointer as-is (field access will use it)
                            _ => {}
                        }
                        return Ok(());
                    }
                }

                // Fallback: Local not found in captured locals or local_to_slot
                // For string types (e.g., loop variables over list<string>), emit (ptr, len) pair
                let is_string = matches!(self.ctx.ty_kind(expr.ty), InternedTyKind::String);
                if is_string {
                    self.add_string("");
                    if let Some((ptr, len)) = self.get_string_info("") {
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                    }
                } else {
                    todo!("Local not found in captured locals or local_to_slot: {:?}", expr.kind)
                }
            }

            LirExprKind::Def(def_id) => {
                if let Some(sig_idx) = self.signal_index_in(component, *def_id) {
                    let addr = layout.signal_addr(sig_idx);
                    func.instruction(&Instruction::I32Const(addr));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                } else {
                    todo!("Def not found in signal_index_in: {:?}", expr.kind)
                }
            }

            LirExprKind::SignalRead(def_id) => {
                if let Some(sig_idx) = self.signal_index_in(component, *def_id) {
                    let addr = layout.signal_addr(sig_idx);
                    func.instruction(&Instruction::I32Const(addr));
                    // Use the appropriate load instruction based on signal type
                    match self.ctx.ty_kind(expr.ty) {
                        InternedTyKind::String | InternedTyKind::List(_) => {
                            // Load ptr from addr
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            // Load len from addr+4
                            func.instruction(&Instruction::I32Const(addr + 4));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        }
                        InternedTyKind::F32 => {
                            func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                        }
                        InternedTyKind::F64 => {
                            func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                        }
                        InternedTyKind::S64 | InternedTyKind::U64 => {
                            func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                        }
                        _ => {
                            // Default: i32 for bool, s32, u32, etc.
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        }
                    }
                } else {
                    todo!("SignalRead not found in signal_index_in: {:?}", expr.kind)
                }
            }

            LirExprKind::Binary { op, lhs, rhs } => {
                self.emit_expr(func, lhs, component, layout)?;
                self.emit_expr(func, rhs, component, layout)?;
                self.emit_binary_op(func, op, lhs.ty);
            }

            LirExprKind::Unary { op, operand } => {
                self.emit_expr(func, operand, component, layout)?;
                self.emit_unary_op(func, op, operand.ty);
            }

            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.emit_expr(func, condition, component, layout)?;
                let result_ty = super::ty_to_wasm_valtype(expr.ty, self.ctx);
                func.instruction(&Instruction::If(BlockType::Result(result_ty)));
                self.emit_expr(func, then_expr, component, layout)?;
                func.instruction(&Instruction::Else);
                self.emit_expr(func, else_expr, component, layout)?;
                func.instruction(&Instruction::End);
            }

            LirExprKind::Call { func: func_def_id, args } => {
                let func_name = self.ctx.str(self.ctx.defs.name(*func_def_id));

                // Handle builtin functions by name
                match func_name.as_str() {
                    "s32-to-string" => {
                        // Call s32_to_string runtime function
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, arg, component, layout)?;
                        } else {
                            todo!("s32-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.s32_to_string));
                        }
                        // Returns (ptr, len)
                    }
                    "bool-to-string" => {
                        // Call bool_to_string runtime function
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, arg, component, layout)?;
                        } else {
                            todo!("bool-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.bool_to_string));
                        }
                        // Returns (ptr, len)
                    }
                    "u32-to-string" => {
                        // u32 can be converted using s32_to_string (values fit in positive i32 range)
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, arg, component, layout)?;
                        } else {
                            todo!("u32-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.s32_to_string));
                        }
                        // Returns (ptr, len)
                    }
                    "f32-to-string" | "f64-to-string" | "char-to-string" => {
                        // Placeholder: return "[number]" string (proper impl would convert)
                        if let Some(arg) = args.first() {
                            let count = self.emit_expr_count(func, arg, component, layout)?;
                            for _ in 0..count {
                                func.instruction(&Instruction::Drop);
                            }
                        }
                        let (ptr, len) = self.add_string("[number]");
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                    }
                    "object-to-string" => {
                        // Return "[object]" string
                        if let Some(arg) = args.first() {
                            let count = self.emit_expr_count(func, arg, component, layout)?;
                            for _ in 0..count {
                                func.instruction(&Instruction::Drop);
                            }
                        }
                        let (ptr, len) = self.add_string("[object]");
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                    }
                    "concat" => {
                        // String concatenation using concat<n> runtime function
                        let arity = args.len();
                        if arity == 0 {
                            // Empty concat returns empty string
                            let (ptr, len) = self.add_string("");
                            func.instruction(&Instruction::I32Const(ptr as i32));
                            func.instruction(&Instruction::I32Const(len as i32));
                        } else if arity == 1 {
                            // Single arg - just emit it directly
                            self.emit_expr(func, &args[0], component, layout)?;
                        } else {
                            // Emit all args (each produces ptr, len)
                            for arg in args {
                                self.emit_expr(func, arg, component, layout)?;
                            }
                            // Call concat<n>
                            if let Some(ref runtime_funcs) = self.runtime_funcs {
                                if let Some(concat_fn) = runtime_funcs.concat(arity) {
                                    func.instruction(&Instruction::Call(concat_fn));
                                }
                            }
                        }
                    }
                    "len" => {
                        // Length of list or string
                        if let Some(arg) = args.first() {
                            // Check if arg is a signal - if so, load len directly from memory
                            if let LirExprKind::SignalRead(def_id) = &arg.kind {
                                if let Some(sig_idx) = component.signals.iter().position(|s| s.def_id == *def_id) {
                                    // Direct signal access - load just the len (at addr+4)
                                    let addr = layout.signal_addr(sig_idx);
                                    func.instruction(&Instruction::I32Const((addr + 4) as i32));
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                } else {
                                    // Not a signal - emit full expr
                                    // Stack: [ptr, len] - need to keep len, drop ptr
                                    // Use slot local 2 (after params 0,1) as temp
                                    self.emit_expr(func, arg, component, layout)?;
                                    func.instruction(&Instruction::LocalSet(2)); // len -> local 2
                                    func.instruction(&Instruction::Drop);        // drop ptr
                                    func.instruction(&Instruction::LocalGet(2)); // push len
                                }
                            } else {
                                // Complex expression
                                self.emit_expr(func, arg, component, layout)?;
                                func.instruction(&Instruction::LocalSet(2));
                                func.instruction(&Instruction::Drop);
                                func.instruction(&Instruction::LocalGet(2));
                            }
                        }
                    }
                    "list-get" => {
                        // Safe list element access: list.get(idx) -> option<T>
                        // Returns (is_some: i32, value) where value depends on element type.
                        if args.len() != 2 {
                            return Err(CodegenError::InvalidIR(
                                "list-get requires 2 args: list, index".to_string(),
                            ));
                        }

                        // First arg is the list (produces ptr, len on stack)
                        self.emit_expr(func, &args[0], component, layout)?;
                        // Stack: [ptr, len]

                        // Second arg is the index
                        self.emit_expr(func, &args[1], component, layout)?;
                        // Stack: [ptr, len, idx]

                        // Determine element size from the expression's result type
                        // expr.ty is option<element_ty>, so we need to extract element_ty
                        let element_size = match self.ctx.ty_kind(expr.ty) {
                            crate::types::InternedTyKind::Option(elem_ty) => {
                                self.layout_ctx.size_of(*elem_ty)
                            }
                            _ => 4, // fallback to 4 bytes
                        };
                        func.instruction(&Instruction::I32Const(element_size as i32));
                        // Stack: [ptr, len, idx, elem_size]

                        // Call list_get_opt(ptr, len, idx, elem_size) -> (is_some, elem_ptr)
                        let runtime_funcs = self.runtime_funcs.as_ref()
                            .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                        func.instruction(&Instruction::Call(runtime_funcs.list_get_opt));
                        // Stack: [is_some, elem_ptr]

                        // Now we need to load the actual value from elem_ptr if is_some=1
                        // For simplicity, we use a conditional:
                        // if is_some { load_value } else { default_value }
                        //
                        // For now, we just return (is_some, elem_ptr) and let the consumer
                        // load the value based on context. This matches how options are typically
                        // represented as (discriminant, payload_ptr/value).
                        //
                        // The caller will need to check is_some and load from elem_ptr as needed.
                    }
                    _ => {
                        // Check if this is a callback call
                        if let Some(import_layout) = &self.import_layout {
                            if let Some(cb_func_idx) = import_layout.find_callback_index(*func_def_id) {
                                // It's a callback - emit args then call the imported callback function
                                for arg in args {
                                    self.emit_expr(func, arg, component, layout)?;
                                }
                                func.instruction(&Instruction::Call(cb_func_idx));
                                // Callbacks return void, but we may need a result on the stack
                                // Push a dummy value if the expression expects a result
                                todo!("Callbacks return void, but we may need a result on the stack: {:?}", expr.kind)
                            } else {
                                // Not a known callback - emit first arg or 0
                                if let Some(arg) = args.first() {
                                    self.emit_expr(func, arg, component, layout)?;
                                } else {
                                    func.instruction(&Instruction::I32Const(0));
                                }
                            }
                        } else {
                            // No import layout - fallback
                            if let Some(arg) = args.first() {
                                self.emit_expr(func, arg, component, layout)?;
                            } else {
                                todo!("Callback not found in imports: {:?}", expr.kind)
                            }
                        }
                    }
                }
            }

            LirExprKind::Field { base, field_idx } => {
                // Field access on a record
                // First, emit the base expression which should leave a record pointer on stack
                self.emit_expr(func, base, component, layout)?;

                // Get the record type from base expression
                if let InternedTyKind::Adt(record_def_id) = self.ctx.ty_kind(base.ty) {
                    // Get field offset from layout
                    if let Some(record_layout) = self.layout_ctx.record_layout_by_id(*record_def_id) {
                        if let Some((_, field_offset, field_ty)) = record_layout.field_offsets.get(field_idx.0 as usize) {
                            let field_offset = *field_offset;

                            // Check what type the field is
                            match self.ctx.ty_kind(*field_ty) {
                                InternedTyKind::String => {
                                    // String field: load (ptr, len) using load_fat_ptr helper
                                    // Stack has record_ptr, add field offset to get string addr
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    // Call load_fat_ptr to get (ptr, len) without using scratch locals
                                    let load_fat_ptr_idx = self.runtime_funcs.as_ref()
                                        .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?
                                        .load_fat_ptr;
                                    func.instruction(&Instruction::Call(load_fat_ptr_idx));
                                }
                                InternedTyKind::F32 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                                }
                                InternedTyKind::F64 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                                }
                                InternedTyKind::S64 | InternedTyKind::U64 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                                }
                                InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::Bool => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                }
                                InternedTyKind::S16 | InternedTyKind::U16 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                                }
                                InternedTyKind::S8 | InternedTyKind::U8 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                                }
                                InternedTyKind::Char => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                }
                                InternedTyKind::List(_) => {
                                    // List field: load (ptr, len) like string
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    let load_fat_ptr_idx = self.runtime_funcs.as_ref()
                                        .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?
                                        .load_fat_ptr;
                                    func.instruction(&Instruction::Call(load_fat_ptr_idx));
                                }
                                _ => todo!("Implement {:?}", self.ctx.ty_kind(*field_ty)),
                            }
                        } else {
                            // Field not found - emit placeholder
                            func.instruction(&Instruction::Drop);
                            todo!("Field not found in record_layout: {:?}", expr.kind)
                        }
                    } else {
                        // No record layout - emit placeholder
                        func.instruction(&Instruction::Drop);
                        todo!("No record layout - emit placeholder: {:?}", expr.kind)
                    }
                } else {
                    // Base is not a record type - emit placeholder
                    func.instruction(&Instruction::Drop);
                    todo!("Base is not a record type - emit placeholder: {:?}", expr.kind)
                }
            }

            LirExprKind::Index { base, index } => {
                // Emit base expression - for a list this gives (ptr, len)
                self.emit_expr(func, base, component, layout)?;
                // Stack: [ptr, len]

                // Emit index
                self.emit_expr(func, index, component, layout)?;
                // Stack: [ptr, len, idx]

                // Get element size from result type
                let element_size = self.layout_ctx.size_of(expr.ty);
                func.instruction(&Instruction::I32Const(element_size as i32));
                // Stack: [ptr, len, idx, elem_size]

                // Call list_get(ptr, len, idx, elem_size) -> elem_ptr
                // This performs bounds checking and traps on out-of-bounds access
                let runtime_funcs = self.runtime_funcs.as_ref()
                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                func.instruction(&Instruction::Call(runtime_funcs.list_get));
                // Result: elem_ptr on stack
            }

            LirExprKind::EnumCase { discriminant, .. } => {
                func.instruction(&Instruction::I32Const(*discriminant as i32));
            }

            LirExprKind::VariantCtor { case_idx, payload, .. } => {
                func.instruction(&Instruction::I32Const(*case_idx as i32));
                if let Some(p) = payload {
                    self.emit_expr(func, p, component, layout)?;
                }
            }

            // List/Record/Tuple constructs - placeholder implementations
            LirExprKind::ListStatic { data_offset, len, .. } => {
                // Return (ptr, len) for static list
                func.instruction(&Instruction::I32Const(*data_offset as i32));
                func.instruction(&Instruction::I32Const(*len as i32));
            }

            LirExprKind::ListConstruct { elements, .. } => {
                // Use list constructor helper - no local conflicts!
                // Each element is emitted to the stack, then we call list_ctor
                if elements.is_empty() {
                    // Empty list: just return (0, 0)
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(0));
                } else {
                    let elem_ty = elements[0].ty;
                    let count = elements.len();

                    // Emit all element values onto the stack
                    for elem in elements {
                        // For RecordConstruct elements, emit field values directly (not calling ctor)
                        // This is because list_ctor stores fields inline
                        if let LirExprKind::RecordConstruct { fields, .. } = &elem.kind {
                            for field in fields {
                                self.emit_expr(func, field, component, layout)?;
                            }
                        } else {
                            // Other elements: emit normally
                            self.emit_expr(func, elem, component, layout)?;
                        }
                    }

                    // Call the list constructor helper
                    let runtime_funcs = self.runtime_funcs.as_ref()
                        .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                    let list_ctor_idx = runtime_funcs.list_ctor(elem_ty, count)
                        .ok_or_else(|| CodegenError::InvalidIR(
                            format!("No list constructor for {:?} with {} elements", elem_ty, count)
                        ))?;
                    func.instruction(&Instruction::Call(list_ctor_idx));
                }
            }

            LirExprKind::RecordConstruct { record_def, fields, .. } => {
                // Use record constructor helper - no local conflicts!
                // Emit all field values onto the stack, then call $ctor_X
                for field in fields {
                    self.emit_expr(func, field, component, layout)?;
                }

                // Call the record constructor helper
                let runtime_funcs = self.runtime_funcs.as_ref()
                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                let ctor_idx = runtime_funcs.record_ctor(*record_def)
                    .ok_or_else(|| CodegenError::InvalidIR(
                        format!("No record constructor for {:?}. Make sure record types are collected.", record_def)
                    ))?;
                func.instruction(&Instruction::Call(ctor_idx));
            }

            LirExprKind::TupleConstruct { elements, total_size } => {
                // Similar to record
                func.instruction(&Instruction::I32Const(*total_size as i32));
                func.instruction(&Instruction::I32Const(4));
                func.instruction(&Instruction::Call(self.alloc_funcs.as_ref().unwrap().alloc));
                func.instruction(&Instruction::LocalSet(0));

                let mut offset = 0u32;
                for elem in elements {
                    func.instruction(&Instruction::LocalGet(0));
                    if offset > 0 {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    self.emit_expr(func, elem, component, layout)?;
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    offset += 4;
                }

                func.instruction(&Instruction::LocalGet(0));
            }
        }

        Ok(())
    }

    /// Like emit_expr but returns the number of values pushed onto the stack.
    pub(super) fn emit_expr_count(
        &mut self,
        func: &mut Function,
        expr: &LirExpr,
        component: &LirComponent,
        layout: &MemoryLayout,
    ) -> Result<usize, CodegenError> {
        match &expr.kind {
            LirExprKind::Literal(lit) => {
                Ok(self.emit_literal_count(func, lit, expr.ty))
            }

            LirExprKind::Local(local_id) => {
                let local_offset = self.current_block_local_offset.unwrap_or(0);

                // Check if this local is captured in the current block (passed as parameter)
                // For for-loop items, captured locals are pointers to list items
                if let Some(captured_map) = &self.current_block_captured_locals {
                    if let Some(&slot) = captured_map.get(local_id) {
                        // Local is captured - the slot contains a pointer to the item
                        let local_idx = slot + local_offset;
                        func.instruction(&Instruction::LocalGet(local_idx));

                        // Dereference based on type
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::Bool | InternedTyKind::Char => {
                                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                return Ok(1);
                            }
                            InternedTyKind::S8 | InternedTyKind::U8 => {
                                func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                                return Ok(1);
                            }
                            InternedTyKind::S16 | InternedTyKind::U16 => {
                                func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                                return Ok(1);
                            }
                            InternedTyKind::S64 | InternedTyKind::U64 => {
                                func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                                return Ok(1);
                            }
                            InternedTyKind::F32 => {
                                func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                                return Ok(1);
                            }
                            InternedTyKind::F64 => {
                                func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                                return Ok(1);
                            }
                            // String/List: load fat pointer (ptr, len)
                            InternedTyKind::String | InternedTyKind::List(_) => {
                                let runtime_funcs = self.runtime_funcs.as_ref()
                                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                                func.instruction(&Instruction::Call(runtime_funcs.load_fat_ptr));
                                return Ok(2); // Returns 2 values (ptr, len)
                            }
                            // Record types (Adt) - return pointer as-is (field access will use it)
                            _ => {
                                return Ok(1);
                            }
                        }
                    }
                }

                // Check if this local is computed inline (e.g., for-loop item ptr)
                if let Some(local_to_slot) = &self.current_block_local_to_slot {
                    if let Some(slot_id) = local_to_slot.get(local_id) {
                        // Local is in an inline-computed slot - this is a pointer to the item
                        let local_idx = slot_id.0 + local_offset;
                        func.instruction(&Instruction::LocalGet(local_idx));

                        // Dereference based on type
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::Bool | InternedTyKind::Char => {
                                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                return Ok(1);
                            }
                            InternedTyKind::S8 | InternedTyKind::U8 => {
                                func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                                return Ok(1);
                            }
                            InternedTyKind::S16 | InternedTyKind::U16 => {
                                func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                                return Ok(1);
                            }
                            InternedTyKind::S64 | InternedTyKind::U64 => {
                                func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                                return Ok(1);
                            }
                            InternedTyKind::F32 => {
                                func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                                return Ok(1);
                            }
                            InternedTyKind::F64 => {
                                func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                                return Ok(1);
                            }
                            // String/List: load fat pointer (ptr, len)
                            InternedTyKind::String | InternedTyKind::List(_) => {
                                let runtime_funcs = self.runtime_funcs.as_ref()
                                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                                func.instruction(&Instruction::Call(runtime_funcs.load_fat_ptr));
                                return Ok(2); // Returns 2 values (ptr, len)
                            }
                            // Record types (Adt) - return pointer as-is (field access will use it)
                            _ => {
                                return Ok(1);
                            }
                        }
                    }
                }

                // Fallback: Local not found in captured locals or local_to_slot
                todo!("Local not found in captured locals or local_to_slot: {:?}", expr.kind);
            }

            LirExprKind::Def(def_id) => {
                if let Some(sig_idx) = self.signal_index_in(component, *def_id) {
                    let addr = layout.signal_addr(sig_idx);
                    func.instruction(&Instruction::I32Const(addr));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                } else {
                    todo!("Def not found in signal_index_in: {:?}", expr.kind)
                }
                Ok(1)
            }

            LirExprKind::SignalRead(def_id) => {
                if let Some(sig_idx) = self.signal_index_in(component, *def_id) {
                    let addr = layout.signal_addr(sig_idx);
                    func.instruction(&Instruction::I32Const(addr));
                    // Use the appropriate load instruction based on signal type
                    match self.ctx.ty_kind(expr.ty) {
                        InternedTyKind::String | InternedTyKind::List(_) => {
                            // Load ptr from addr
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            // Load len from addr+4
                            func.instruction(&Instruction::I32Const(addr + 4));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            Ok(2)
                        }
                        InternedTyKind::F32 => {
                            func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                            Ok(1)
                        }
                        InternedTyKind::F64 => {
                            func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                            Ok(1)
                        }
                        InternedTyKind::S64 | InternedTyKind::U64 => {
                            func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                            Ok(1)
                        }
                        _ => {
                            // Default: i32 for bool, s32, u32, etc.
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            Ok(1)
                        }
                    }
                } else {
                    todo!("SignalRead not found in signal_index_in: {:?}", expr.kind);
                }
            }

            LirExprKind::Binary { op, lhs, rhs } => {
                self.emit_expr(func, lhs, component, layout)?;
                self.emit_expr(func, rhs, component, layout)?;
                self.emit_binary_op(func, op, lhs.ty);
                Ok(1)
            }

            LirExprKind::Unary { op, operand } => {
                self.emit_expr(func, operand, component, layout)?;
                self.emit_unary_op(func, op, operand.ty);
                Ok(1)
            }

            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.emit_expr(func, condition, component, layout)?;
                let result_ty = super::ty_to_wasm_valtype(expr.ty, self.ctx);
                func.instruction(&Instruction::If(BlockType::Result(result_ty)));
                self.emit_expr(func, then_expr, component, layout)?;
                func.instruction(&Instruction::Else);
                self.emit_expr(func, else_expr, component, layout)?;
                func.instruction(&Instruction::End);
                Ok(1)
            }

            LirExprKind::Call { func: func_def_id, args } => {
                let func_name = self.ctx.str(self.ctx.defs.name(*func_def_id));

                // Handle builtin functions by name
                match func_name.as_str() {
                    "s32-to-string" => {
                        // Call s32_to_string runtime function
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, arg, component, layout)?;
                        } else {
                            todo!("s32-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.s32_to_string));
                        }
                        Ok(2) // Returns (ptr, len)
                    }
                    "bool-to-string" => {
                        // Call bool_to_string runtime function
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, arg, component, layout)?;
                        } else {
                            todo!("bool-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.bool_to_string));
                        }
                        Ok(2) // Returns (ptr, len)
                    }
                    "u32-to-string" | "f32-to-string" | "f64-to-string" | "char-to-string" => {
                        // Placeholder: return "[number]" string
                        if let Some(arg) = args.first() {
                            let count = self.emit_expr_count(func, arg, component, layout)?;
                            for _ in 0..count {
                                func.instruction(&Instruction::Drop);
                            }
                        }
                        let (ptr, len) = self.add_string("[number]");
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                        Ok(2)
                    }
                    "object-to-string" => {
                        if let Some(arg) = args.first() {
                            let count = self.emit_expr_count(func, arg, component, layout)?;
                            for _ in 0..count {
                                func.instruction(&Instruction::Drop);
                            }
                        }
                        let (ptr, len) = self.add_string("[object]");
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                        Ok(2) // ptr + len
                    }
                    "concat" => {
                        // String concatenation using concat<n> runtime function
                        let arity = args.len();
                        if arity == 0 {
                            let (ptr, len) = self.add_string("");
                            func.instruction(&Instruction::I32Const(ptr as i32));
                            func.instruction(&Instruction::I32Const(len as i32));
                            Ok(2)
                        } else if arity == 1 {
                            self.emit_expr_count(func, &args[0], component, layout)
                        } else {
                            for arg in args {
                                self.emit_expr(func, arg, component, layout)?;
                            }
                            if let Some(ref runtime_funcs) = self.runtime_funcs {
                                if let Some(concat_fn) = runtime_funcs.concat(arity) {
                                    func.instruction(&Instruction::Call(concat_fn));
                                }
                            }
                            Ok(2) // concat returns (ptr, len)
                        }
                    }
                    "len" => {
                        // Returns single i32 - length of list or string
                        if let Some(arg) = args.first() {
                            // Check if arg is a signal - if so, load len directly from memory
                            if let LirExprKind::SignalRead(def_id) = &arg.kind {
                                if let Some(sig_idx) = component.signals.iter().position(|s| s.def_id == *def_id) {
                                    // Direct signal access - load just the len (at addr+4)
                                    let addr = layout.signal_addr(sig_idx);
                                    func.instruction(&Instruction::I32Const((addr + 4) as i32));
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                } else {
                                    // Not a signal - emit full expr
                                    self.emit_expr(func, arg, component, layout)?;
                                    func.instruction(&Instruction::LocalSet(2));
                                    func.instruction(&Instruction::Drop);
                                    func.instruction(&Instruction::LocalGet(2));
                                }
                            } else {
                                // Complex expression
                                self.emit_expr(func, arg, component, layout)?;
                                func.instruction(&Instruction::LocalSet(2));
                                func.instruction(&Instruction::Drop);
                                func.instruction(&Instruction::LocalGet(2));
                            }
                        }
                        Ok(1)
                    }
                    _ => {
                        // Check if this is a callback call
                        if let Some(import_layout) = &self.import_layout {
                            if let Some(cb_func_idx) = import_layout.find_callback_index(*func_def_id) {
                                for arg in args {
                                    self.emit_expr(func, arg, component, layout)?;
                                }
                                func.instruction(&Instruction::Call(cb_func_idx));
                                todo!("Callback not found in imports: {:?}", expr.kind)
                            }
                        }
                        if let Some(arg) = args.first() {
                            self.emit_expr_count(func, arg, component, layout)
                        } else {
                            todo!("len requires 1 arg: {:?}", expr.kind)
                        }
                    }
                }
            }

            LirExprKind::Field { base, field_idx } => {
                // Field access on a record - emit base, handle multi-value correctly
                let base_count = self.emit_expr_count(func, base, component, layout)?;
                // If base returned more than 1 value, drop the extra ones
                // (e.g., for SignalRead of list type which returns (ptr, len), we'd keep ptr and drop len)
                // But for Index or single-value expressions, base_count is 1, so no drop needed
                for _ in 1..base_count {
                    func.instruction(&Instruction::Drop);
                }

                if let InternedTyKind::Adt(record_def_id) = self.ctx.ty_kind(base.ty) {
                    if let Some(record_layout) = self.layout_ctx.record_layout_by_id(*record_def_id) {
                        if let Some((_, field_offset, field_ty)) = record_layout.field_offsets.get(field_idx.0 as usize) {
                            let field_offset = *field_offset;

                            match self.ctx.ty_kind(*field_ty) {
                                InternedTyKind::String | InternedTyKind::List(_) => {
                                    // String field: use load_fat_ptr helper
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    let load_fat_ptr_idx = self.runtime_funcs.as_ref()
                                        .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?
                                        .load_fat_ptr;
                                    func.instruction(&Instruction::Call(load_fat_ptr_idx));
                                    return Ok(2);
                                }
                                InternedTyKind::F32 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                                    return Ok(1);
                                }
                                InternedTyKind::F64 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(field_offset as i32));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                                    return Ok(1);
                                }
                                _ => todo!("load field: {:?}", field_ty)
                            }
                        }
                    }
                }
                // Fallback
                todo!("Field not found in record_layout: {:?}", expr.kind)
            }

            LirExprKind::Index { base, index } => {
                // Emit base (list gives ptr, len)
                self.emit_expr_count(func, base, component, layout)?;
                // Stack: [ptr, len]

                // Emit index
                self.emit_expr_count(func, index, component, layout)?;
                // Stack: [ptr, len, idx]

                // Get element size from result type
                let element_size = self.layout_ctx.size_of(expr.ty);
                func.instruction(&Instruction::I32Const(element_size as i32));
                // Stack: [ptr, len, idx, elem_size]

                // Call list_get(ptr, len, idx, elem_size) -> elem_ptr
                // This performs bounds checking and traps on out-of-bounds access
                let runtime_funcs = self.runtime_funcs.as_ref()
                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                func.instruction(&Instruction::Call(runtime_funcs.list_get));
                Ok(1) // returns elem_ptr
            }

            LirExprKind::EnumCase { discriminant, .. } => {
                func.instruction(&Instruction::I32Const(*discriminant as i32));
                Ok(1)
            }

            LirExprKind::VariantCtor { case_idx, payload, .. } => {
                func.instruction(&Instruction::I32Const(*case_idx as i32));
                if let Some(p) = payload {
                    let payload_count = self.emit_expr_count(func, p, component, layout)?;
                    Ok(1 + payload_count) // 1 for discriminant + payload values
                } else {
                    Ok(1)
                }
            }

            // List/Record/Tuple constructs return (ptr, len) for lists, ptr for records/tuples
            LirExprKind::ListStatic { data_offset, len, .. } => {
                func.instruction(&Instruction::I32Const(*data_offset as i32));
                func.instruction(&Instruction::I32Const(*len as i32));
                Ok(2) // (ptr, len)
            }

            LirExprKind::ListConstruct { elements, .. } => {
                // Use list constructor helper - no local conflicts!
                // Each element is emitted to the stack, then we call list_ctor
                if elements.is_empty() {
                    // Empty list: just return (0, 0)
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(0));
                } else {
                    let elem_ty = elements[0].ty;
                    let count = elements.len();

                    // Emit all element values onto the stack
                    for elem in elements {
                        // For RecordConstruct elements, emit field values directly (not calling ctor)
                        // This is because list_ctor stores fields inline
                        if let LirExprKind::RecordConstruct { fields, .. } = &elem.kind {
                            for field in fields {
                                self.emit_expr(func, field, component, layout)?;
                            }
                        } else {
                            // Other elements: emit normally
                            self.emit_expr(func, elem, component, layout)?;
                        }
                    }

                    // Call the list constructor helper
                    let runtime_funcs = self.runtime_funcs.as_ref()
                        .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                    let list_ctor_idx = runtime_funcs.list_ctor(elem_ty, count)
                        .ok_or_else(|| CodegenError::InvalidIR(
                            format!("No list constructor for {:?} with {} elements", elem_ty, count)
                        ))?;
                    func.instruction(&Instruction::Call(list_ctor_idx));
                }
                Ok(2) // (ptr, len)
            }

            LirExprKind::RecordConstruct { record_def, fields, .. } => {
                // Use record constructor helper - no local conflicts!
                // Emit all field values onto the stack, then call $ctor_X
                for field in fields {
                    self.emit_expr(func, field, component, layout)?;
                }

                // Call the record constructor helper
                let runtime_funcs = self.runtime_funcs.as_ref()
                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?;
                let ctor_idx = runtime_funcs.record_ctor(*record_def)
                    .ok_or_else(|| CodegenError::InvalidIR(
                        format!("No record constructor for {:?}. Make sure record types are collected.", record_def)
                    ))?;
                func.instruction(&Instruction::Call(ctor_idx));
                Ok(1) // ptr only
            }

            LirExprKind::TupleConstruct { elements, total_size } => {
                func.instruction(&Instruction::I32Const(*total_size as i32));
                func.instruction(&Instruction::I32Const(4));
                func.instruction(&Instruction::Call(self.alloc_funcs.as_ref().unwrap().alloc));
                func.instruction(&Instruction::LocalSet(0));

                let mut offset = 0u32;
                for elem in elements {
                    func.instruction(&Instruction::LocalGet(0));
                    if offset > 0 {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    self.emit_expr(func, elem, component, layout)?;
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    offset += 4;
                }

                func.instruction(&Instruction::LocalGet(0));
                Ok(1) // ptr only
            }
        }
    }

    pub(super) fn emit_literal(&mut self, func: &mut Function, lit: &LirLiteral, _ty: crate::types::Ty) {
        match lit {
            // Signed integers
            LirLiteral::S8(v) => { func.instruction(&Instruction::I32Const(*v as i32)); }
            LirLiteral::S16(v) => { func.instruction(&Instruction::I32Const(*v as i32)); }
            LirLiteral::S32(v) => { func.instruction(&Instruction::I32Const(*v)); }
            LirLiteral::S64(v) => { func.instruction(&Instruction::I64Const(*v)); }
            // Unsigned integers
            LirLiteral::U8(v) => { func.instruction(&Instruction::I32Const(*v as i32)); }
            LirLiteral::U16(v) => { func.instruction(&Instruction::I32Const(*v as i32)); }
            LirLiteral::U32(v) => { func.instruction(&Instruction::I32Const(*v as i32)); }
            LirLiteral::U64(v) => { func.instruction(&Instruction::I64Const(*v as i64)); }
            // Floats
            LirLiteral::F32(v) => { func.instruction(&Instruction::F32Const(*v)); }
            LirLiteral::F64(v) => { func.instruction(&Instruction::F64Const(*v)); }
            // Other primitives
            LirLiteral::Bool(b) => {
                func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
            }
            LirLiteral::Char(c) => {
                // Char is represented as unicode codepoint (i32)
                func.instruction(&Instruction::I32Const(*c as i32));
            }
            LirLiteral::String(s) => {
                self.add_string(s);
                if let Some((ptr, len)) = self.get_string_info(s) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                }
            }
        }
    }

    /// Like emit_literal but returns the number of values pushed.
    pub(super) fn emit_literal_count(&mut self, func: &mut Function, lit: &LirLiteral, ty: crate::types::Ty) -> usize {
        match lit {
            LirLiteral::String(s) => {
                self.add_string(s);
                if let Some((ptr, len)) = self.get_string_info(s) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    2
                } else {
                    todo!("String not found in get_string_info: {:?}", lit)
                }
            }
            _ => {
                self.emit_literal(func, lit, ty);
                1
            }
        }
    }

    pub(super) fn emit_binary_op(&self, func: &mut Function, op: &BinOp, ty: crate::types::Ty) {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => {
                match op {
                    BinOp::Add => { func.instruction(&Instruction::F32Add); }
                    BinOp::Sub => { func.instruction(&Instruction::F32Sub); }
                    BinOp::Mul => { func.instruction(&Instruction::F32Mul); }
                    BinOp::Div => { func.instruction(&Instruction::F32Div); }
                    BinOp::Mod => {
                        // F32 doesn't have native rem, use: a - trunc(a/b) * b
                        // For simplicity, convert to i32, do mod, convert back
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(0)); // temp store b
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(0));
                        func.instruction(&Instruction::I32RemS);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    BinOp::Eq => { func.instruction(&Instruction::F32Eq); }
                    BinOp::Ne => { func.instruction(&Instruction::F32Ne); }
                    BinOp::Lt => { func.instruction(&Instruction::F32Lt); }
                    BinOp::Gt => { func.instruction(&Instruction::F32Gt); }
                    BinOp::Le => { func.instruction(&Instruction::F32Le); }
                    BinOp::Ge => { func.instruction(&Instruction::F32Ge); }
                    // Logical ops convert to i32, operate, convert back
                    BinOp::And | BinOp::BitAnd => {
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(0));
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(0));
                        func.instruction(&Instruction::I32And);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    BinOp::Or | BinOp::BitOr => {
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(0));
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(0));
                        func.instruction(&Instruction::I32Or);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    BinOp::BitXor => {
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(0));
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(0));
                        func.instruction(&Instruction::I32Xor);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                }
            }
            InternedTyKind::F64 => {
                match op {
                    BinOp::Add => { func.instruction(&Instruction::F64Add); }
                    BinOp::Sub => { func.instruction(&Instruction::F64Sub); }
                    BinOp::Mul => { func.instruction(&Instruction::F64Mul); }
                    BinOp::Div => { func.instruction(&Instruction::F64Div); }
                    BinOp::Mod => {
                        // F64 doesn't have native rem - convert to i64, do mod, convert back
                        func.instruction(&Instruction::I64TruncF64S);
                        func.instruction(&Instruction::LocalSet(0)); // temp store b (needs i64 local)
                        func.instruction(&Instruction::I64TruncF64S);
                        func.instruction(&Instruction::LocalGet(0));
                        func.instruction(&Instruction::I64RemS);
                        func.instruction(&Instruction::F64ConvertI64S);
                    }
                    BinOp::Eq => { func.instruction(&Instruction::F64Eq); }
                    BinOp::Ne => { func.instruction(&Instruction::F64Ne); }
                    BinOp::Lt => { func.instruction(&Instruction::F64Lt); }
                    BinOp::Gt => { func.instruction(&Instruction::F64Gt); }
                    BinOp::Le => { func.instruction(&Instruction::F64Le); }
                    BinOp::Ge => { func.instruction(&Instruction::F64Ge); }
                    // Logical/bit ops - just use i32 ops since these return bool anyway
                    BinOp::And | BinOp::BitAnd => { func.instruction(&Instruction::I32And); }
                    BinOp::Or | BinOp::BitOr => { func.instruction(&Instruction::I32Or); }
                    BinOp::BitXor => { func.instruction(&Instruction::I32Xor); }
                }
            }
            _ => {
                // Default: i32 operations
                match op {
                    BinOp::Add => func.instruction(&Instruction::I32Add),
                    BinOp::Sub => func.instruction(&Instruction::I32Sub),
                    BinOp::Mul => func.instruction(&Instruction::I32Mul),
                    BinOp::Div => func.instruction(&Instruction::I32DivS),
                    BinOp::Mod => func.instruction(&Instruction::I32RemS),
                    BinOp::Eq => func.instruction(&Instruction::I32Eq),
                    BinOp::Ne => func.instruction(&Instruction::I32Ne),
                    BinOp::Lt => func.instruction(&Instruction::I32LtS),
                    BinOp::Gt => func.instruction(&Instruction::I32GtS),
                    BinOp::Le => func.instruction(&Instruction::I32LeS),
                    BinOp::Ge => func.instruction(&Instruction::I32GeS),
                    BinOp::And => func.instruction(&Instruction::I32And),
                    BinOp::Or => func.instruction(&Instruction::I32Or),
                    BinOp::BitAnd => func.instruction(&Instruction::I32And),
                    BinOp::BitOr => func.instruction(&Instruction::I32Or),
                    BinOp::BitXor => func.instruction(&Instruction::I32Xor),
                };
            }
        }
    }

    pub(super) fn emit_unary_op(&self, func: &mut Function, op: &UnaryOp, ty: crate::types::Ty) {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => {
                match op {
                    UnaryOp::Not => {
                        // !x for f32: x == 0.0 ? 1.0 : 0.0
                        func.instruction(&Instruction::F32Const(0.0));
                        func.instruction(&Instruction::F32Eq);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    UnaryOp::Neg => {
                        func.instruction(&Instruction::F32Neg);
                    }
                }
            }
            InternedTyKind::F64 => {
                match op {
                    UnaryOp::Not => {
                        func.instruction(&Instruction::F64Const(0.0));
                        func.instruction(&Instruction::F64Eq);
                        func.instruction(&Instruction::F64ConvertI64S);
                    }
                    UnaryOp::Neg => {
                        func.instruction(&Instruction::F64Neg);
                    }
                }
            }
            _ => {
                match op {
                    UnaryOp::Not => {
                        func.instruction(&Instruction::I32Eqz);
                    }
                    UnaryOp::Neg => {
                        func.instruction(&Instruction::I32Const(-1));
                        func.instruction(&Instruction::I32Mul);
                    }
                }
            }
        }
    }

    pub(super) fn emit_expr_as_string(
        &mut self,
        func: &mut Function,
        expr: &LirExpr,
        component: &LirComponent,
        layout: &MemoryLayout,
    ) -> Result<(), CodegenError> {
        let runtime_funcs = self.runtime_funcs.as_ref()
            .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?
            .clone();

        match self.ctx.ty_kind(expr.ty) {
            InternedTyKind::String => {
                self.emit_expr(func, expr, component, layout)?;
            }
            InternedTyKind::S32 | InternedTyKind::U32 => {
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::Call(runtime_funcs.s32_to_string));
            }
            InternedTyKind::S64 | InternedTyKind::U64 => {
                todo!("s64/u64 to string conversion");
            }
            InternedTyKind::Bool => {
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::Call(runtime_funcs.bool_to_string));
            }
            InternedTyKind::F32 => {
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::Call(runtime_funcs.f32_to_string));
            }
            _ => {
                // Unsupported type - emit empty string
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::Drop);
                self.add_string("");
                if let Some((ptr, len)) = self.get_string_info("") {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                }
            }
        }
        Ok(())
    }

    /// Emit an expression as an attribute-value variant using canonical ABI flattening.
    /// The canonical ABI flattens this variant to: (discrim: i32, payload_i64: i64, payload_i32: i32)
    /// - payload_i64: Used for string (ptr<<32|len), i64/u64 values, or f64 reinterpreted as i64
    /// - payload_i32: Used for i32/u32/bool/s8-s32/u8-u32/char values, or f32 reinterpreted as i32
    /// Variant cases: 0=str, 1=bool, 2=s8, 3=s16, 4=s32, 5=s64, 6=u8, 7=u16, 8=u32, 9=u64, 10=f32, 11=f64, 12=char
    pub(super) fn emit_expr_as_attr_value(
        &mut self,
        func: &mut Function,
        expr: &LirExpr,
        component: &LirComponent,
        layout: &MemoryLayout,
    ) -> Result<(), CodegenError> {
        // Determine discriminant and emit variant based on type
        // Canonical ABI: (discrim: i32, payload_i64: i64, payload_i32: i32)
        match self.ctx.ty_kind(expr.ty) {
            InternedTyKind::String => {
                // discrim=0, payload_i64 = ptr (extended), payload_i32 = len
                // Canonical ABI "join" puts ptr in i64 slot, len in i32 slot
                func.instruction(&Instruction::I32Const(0)); // discrim
                self.emit_expr(func, expr, component, layout)?; // pushes ptr, len
                // Call helper to promote (ptr, len) -> (ptr_i64, len)
                if let Some(runtime_funcs) = &self.runtime_funcs {
                    func.instruction(&Instruction::Call(runtime_funcs.pack_fat_ptr_to_i64));
                }
            }
            InternedTyKind::Bool | InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 |
            InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 | InternedTyKind::Char => {
                // These all use payload_i32, payload_i64 = 0
                let discrim = match self.ctx.ty_kind(expr.ty) {
                    InternedTyKind::Bool => 1,
                    InternedTyKind::S8 => 2,
                    InternedTyKind::S16 => 3,
                    InternedTyKind::S32 => 4,
                    InternedTyKind::U8 => 6,
                    InternedTyKind::U16 => 7,
                    InternedTyKind::U32 => 8,
                    InternedTyKind::Char => 12,
                    _ => unreachable!(),
                };
                func.instruction(&Instruction::I32Const(discrim)); // discrim
                func.instruction(&Instruction::I64Const(0)); // payload_i64 = 0
                self.emit_expr(func, expr, component, layout)?; // payload_i32 = value
            }
            InternedTyKind::S64 | InternedTyKind::U64 => {
                // These use payload_i64, payload_i32 = 0
                let discrim = if matches!(self.ctx.ty_kind(expr.ty), InternedTyKind::S64) { 5 } else { 9 };
                func.instruction(&Instruction::I32Const(discrim)); // discrim
                self.emit_expr(func, expr, component, layout)?; // payload_i64 = value (i64)
                func.instruction(&Instruction::I32Const(0)); // payload_i32 = 0
            }
            InternedTyKind::F32 => {
                // f32 goes in payload_i64 (reinterpreted as i32, then extended to i64)
                // This matches the canonical ABI "join" rules for variants
                func.instruction(&Instruction::I32Const(10)); // discrim
                self.emit_expr(func, expr, component, layout)?; // f32 value
                func.instruction(&Instruction::I32ReinterpretF32); // reinterpret as i32
                func.instruction(&Instruction::I64ExtendI32U); // extend to i64 for payload_i64 slot
                func.instruction(&Instruction::I32Const(0)); // payload_i32 = 0
            }
            InternedTyKind::F64 => {
                // f64 uses payload_i64 with reinterpret, payload_i32 = 0
                func.instruction(&Instruction::I32Const(11)); // discrim
                self.emit_expr(func, expr, component, layout)?; // f64 value
                func.instruction(&Instruction::I64ReinterpretF64); // reinterpret as i64
                func.instruction(&Instruction::I32Const(0)); // payload_i32 = 0
            }
            other => {
                todo!("emit_expr_as_attr_value: unsupported type {:?}", other)
            }
        }
        Ok(())
    }
}
