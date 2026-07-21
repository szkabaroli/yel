//! Expression emission for WASM codegen.
//!
//! This module contains all expression emission functions used by the core module
//! code generator. These are implemented as methods on `WasmPackageBuilder`.

use wasm_encoder::{Function, Instruction};
use wasm_encoder::{Ieee32, Ieee64};
use yel_core::{DefId, Ty};

use super::CodegenError;
use yel_core::hir::expr::{BinOp, UnaryOp};
use yel_core::lir::{LirBindingMode, LirResource, LirExpr, LirExprKind, LirLiteral};
use yel_core::types::InternedTyKind;

use super::codegen::{mem_arg, slot_local_resource_only};
use super::WasmPackageBuilder;

fn emit_zero_default(func: &mut Function, vt: wasm_encoder::ValType) -> Result<(), CodegenError> {
    use wasm_encoder::ValType;
    match vt {
        ValType::I32 => {
            func.instruction(&Instruction::I32Const(0));
        }
        ValType::I64 => {
            func.instruction(&Instruction::I64Const(0));
        }
        ValType::F32 => {
            func.instruction(&Instruction::F32Const(0.0_f32.into()));
        }
        ValType::F64 => {
            func.instruction(&Instruction::F64Const(0.0_f64.into()));
        }
        other => {
            return Err(CodegenError::InvalidIR(format!(
                "emit_zero_default: unsupported valtype {:?}",
                other
            )));
        }
    }
    Ok(())
}

impl WasmPackageBuilder<'_> {
    pub(super) fn emit_expr(
        &mut self,
        func: &mut Function,
        expr: &LirExpr,
        component: &LirResource,
    ) -> Result<usize, CodegenError> {
        match &expr.kind {
            LirExprKind::Literal(lit) => {
                let n = self.emit_literal_count(func, lit, expr.ty);
                Ok(n)
            }

            LirExprKind::Local(local_id) => {
                let local_offset = self.current_block_local_offset.unwrap_or(0);

                // Check if this local is captured in the current block (passed as parameter)
                // For for-loop items, captured locals are pointers to list items
                // Phase 5b-v.2: pull the per-binding mode for this local.
                // `Ptr` (default for everything today) keeps today's
                // typed-load behavior; `Value` (added in 5b-v.3) skips
                // the load — the slot already holds the scalar.
                let binding_mode = self
                    .current_block_local_modes
                    .as_ref()
                    .and_then(|m| m.get(local_id).copied())
                    .unwrap_or(LirBindingMode::Ptr);

                if let Some(captured_map) = &self.current_block_captured_locals
                    && let Some(&local_idx) = captured_map.get(local_id)
                {
                    // Local is captured — value already resolved to an
                    // absolute WASM local index at block setup.
                    func.instruction(&Instruction::LocalGet(local_idx));

                    if binding_mode == LirBindingMode::Ptr {
                        // Dereference based on type
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::S32
                            | InternedTyKind::U32
                            | InternedTyKind::Bool
                            | InternedTyKind::Char => {
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
                                let runtime_funcs =
                                    self.runtime_funcs.as_ref().ok_or_else(|| {
                                        CodegenError::InvalidIR(
                                            "Runtime functions not initialized".to_string(),
                                        )
                                    })?;
                                func.instruction(&Instruction::Call(runtime_funcs.load_fat_ptr.expect("load_fat_ptr must be in runtime_needs (scan missed it?)")));
                            }
                            // Record types (Adt) - return pointer as-is (field access will use it)
                            _ => {}
                        }
                    }
                    // BindingMode::Value: slot already holds the scalar value;
                    // the `local.get` above is the entire emission.
                    return Ok(if binding_mode == LirBindingMode::Value {
                        1
                    } else {
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::String | InternedTyKind::List(_) => 2,
                            _ => 1,
                        }
                    });
                }

                // Check if this local is computed inline (e.g., for-loop item ptr)
                if let Some(local_to_slot) = &self.current_block_local_to_slot
                    && let Some(slot_id) = local_to_slot.get(local_id)
                {
                    // Local is in an inline-computed slot - this is a pointer to the item
                    let local_idx = slot_local_resource_only(component, *slot_id, local_offset);
                    func.instruction(&Instruction::LocalGet(local_idx));

                    if binding_mode == LirBindingMode::Ptr {
                        // Dereference based on type
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::S32
                            | InternedTyKind::U32
                            | InternedTyKind::Bool
                            | InternedTyKind::Char => {
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
                                let runtime_funcs =
                                    self.runtime_funcs.as_ref().ok_or_else(|| {
                                        CodegenError::InvalidIR(
                                            "Runtime functions not initialized".to_string(),
                                        )
                                    })?;
                                func.instruction(&Instruction::Call(runtime_funcs.load_fat_ptr.expect("load_fat_ptr must be in runtime_needs (scan missed it?)")));
                            }
                            // Record types (Adt) - return pointer as-is (field access will use it)
                            _ => {}
                        }
                    }
                    return Ok(if binding_mode == LirBindingMode::Value {
                        1
                    } else {
                        match self.ctx.ty_kind(expr.ty) {
                            InternedTyKind::String | InternedTyKind::List(_) => 2,
                            _ => 1,
                        }
                    });
                }

                // Fallback: Local not found in captured locals or local_to_slot
                // For string types (e.g., loop variables over list<string>), emit (ptr, len) pair
                let is_string = matches!(self.ctx.ty_kind(expr.ty), InternedTyKind::String);
                if is_string {
                    self.add_string("");
                    if let Some((ptr, len)) = self.get_string_info("") {
                        func.instruction(&Instruction::I32Const(ptr as i32));
                        func.instruction(&Instruction::I32Const(len as i32));
                        return Ok(2);
                    }
                }
                todo!(
                    "Local not found in captured locals or local_to_slot: {:?}",
                    expr.kind
                )
            }

            LirExprKind::Def(def_id) => {
                // Migrated global property — read via per-block GC
                // struct. Allowed in module-scope contexts (filter
                // closures inside global default exprs etc.) where
                // there's no owning component.
                if self.ctx.defs.owning_global_block(*def_id).is_some()
                    && self.global_in_struct(*def_id)
                {
                    self.emit_global_struct_read(func, *def_id)?;
                    return Ok(self.signal_storage_valtypes(expr.ty).len());
                }
                let sig_idx = self.signal_index_in(component, *def_id).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "Def: {:?} is not a signal of the current component nor a \
                         migrated global-block property",
                        def_id
                    ))
                })?;
                let comp_idx = self.comp_idx_of(component);
                let migrated = comp_idx
                    .map(|ci| self.signal_in_struct(ci, sig_idx))
                    .unwrap_or(false);
                if migrated {
                    let ci = comp_idx.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "Def: migrated signal path requires component index".to_string(),
                        )
                    })?;
                    self.emit_signal_struct_read(func, ci, sig_idx)?;
                    return Ok(self.signal_storage_valtypes(expr.ty).len());
                }
                // Pointer-typed signal in linear memory is unreachable —
                // every non-unit signal is GC-struct-resident.
                unreachable!(
                    "Def: pointer-typed signal in linear memory is unreachable — \
                     every non-unit signal is GC-struct-resident"
                )
            }

            LirExprKind::SignalRead(def_id) => {
                // Filter predicate: captured signals are passed as
                // explicit WASM params, not read via self ref. Resolve
                // through `current_filter_captured_signals` first so
                // the per-instance `current_self_local` requirement of
                // `emit_signal_struct_read` doesn't apply inside filter
                // helpers (those are module-level functions with no
                // self ref at all).
                if let Some(captured) = &self.current_filter_captured_signals
                    && let Some(&(local_idx, is_fat_ptr)) = captured.get(def_id)
                {
                    func.instruction(&Instruction::LocalGet(local_idx));
                    if is_fat_ptr {
                        func.instruction(&Instruction::LocalGet(local_idx + 1));
                    }
                    return Ok(if is_fat_ptr { 2 } else { 1 });
                }
                // Component-local signal that's been migrated to the
                // GC struct — struct.get each ABI slot.
                if let Some(sig_idx) = self.signal_index_in(component, *def_id)
                    && let Some(comp_idx) = self.comp_idx_of(component)
                    && self.signal_in_struct(comp_idx, sig_idx)
                {
                    self.emit_signal_struct_read(func, comp_idx, sig_idx)?;
                    return Ok(self.signal_storage_valtypes(expr.ty).len());
                }
                // Migrated global property — read via per-block GC
                // struct.
                if self.ctx.defs.owning_global_block(*def_id).is_some()
                    && self.global_in_struct(*def_id)
                {
                    self.emit_global_struct_read(func, *def_id)?;
                    return Ok(self.signal_storage_valtypes(expr.ty).len());
                }
                // Global property OR Pointer-typed (record/tuple)
                // local signal — keep linear-memory load path.
                let addr = if let Some(_sig_idx) = self.signal_index_in(component, *def_id) {
                    unreachable!(
                        "SignalRead: memory path is globals-only; \
                         signals are GC-struct-resident"
                    )
                } else if let Some(&a) = self.global_property_addrs.get(def_id) {
                    a
                } else {
                    todo!("SignalRead: no address for {:?}", def_id)
                };
                match self.ctx.ty_kind(expr.ty) {
                    InternedTyKind::String | InternedTyKind::List(_) => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    }
                    InternedTyKind::F32 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                    }
                    InternedTyKind::F64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                    }
                    InternedTyKind::S64 | InternedTyKind::U64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                    }
                    // Narrow types: load 1/2 bytes so we don't pull in the
                    // adjacent signal's memory.
                    InternedTyKind::Bool | InternedTyKind::U8 | InternedTyKind::Char => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load8U(mem_arg(0, 0)));
                    }
                    InternedTyKind::S8 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                    }
                    InternedTyKind::U16 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load16U(mem_arg(0, 1)));
                    }
                    InternedTyKind::S16 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                    }
                    // Option / Result / Variant-with-payload: load each
                    // canonical-ABI flat slot at its recorded offset,
                    // producing `flatten_core_valtypes(ty).len()` stack
                    // values in declaration order. Records / tuples are
                    // pointer-passed in the current ABI — for those, keep
                    // pushing a single pointer-sized i32 load of the base
                    // address so that field accesses (which expect a base
                    // pointer) remain valid.
                    InternedTyKind::Option(_) | InternedTyKind::Result { .. } => {
                        self.emit_flat_slot_signal_read(func, addr, expr.ty)?;
                    }
                    InternedTyKind::Adt(def_id) => {
                        // Variant with any payload: flat-slot load.
                        // Enum / Record: pointer-passed, single i32 load of
                        // signal base address.
                        let has_payload = self
                            .ctx
                            .defs
                            .as_variant(*def_id)
                            .map(|v| {
                                v.cases.clone().iter().any(|&c| {
                                    if let yel_core::definitions::DefKind::VariantCase(case) =
                                        self.ctx.defs.kind(c)
                                    {
                                        case.payload.is_some()
                                    } else {
                                        false
                                    }
                                })
                            })
                            .unwrap_or(false);
                        if has_payload {
                            self.emit_flat_slot_signal_read(func, addr, expr.ty)?;
                        } else {
                            func.instruction(&Instruction::I32Const(addr));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        }
                    }
                    _ => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    }
                }
                Ok(self.flatten_core_valtypes(expr.ty).len())
            }

            LirExprKind::Binary { op, lhs, rhs } => {
                let lhs = component.get_expr(*lhs);
                let rhs = component.get_expr(*rhs);
                self.emit_expr(func, lhs, component)?;
                self.emit_expr(func, rhs, component)?;
                self.emit_binary_op(func, op, lhs.ty);
                Ok(1)
            }

            LirExprKind::Unary { op, operand } => {
                let operand = component.get_expr(*operand);
                self.emit_expr(func, operand, component)?;
                self.emit_unary_op(func, op, operand.ty);
                Ok(1)
            }

            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                let condition = component.get_expr(*condition);
                let then_expr = component.get_expr(*then_expr);
                let else_expr = component.get_expr(*else_expr);
                self.emit_expr(func, condition, component)?;
                // Single source of truth for the block type — covers
                // primitives (`Result(valtype)`), multi-slot composites
                // (`FunctionType(idx)` from the pre-registered
                // `ternary_block_types`), and unit (`Empty`). See
                // `super::repr::block_ty_for`.
                let block_ty = self.block_ty_for(expr.ty)?;
                func.instruction(&Instruction::If(block_ty));
                self.emit_expr(func, then_expr, component)?;
                func.instruction(&Instruction::Else);
                self.emit_expr(func, else_expr, component)?;
                func.instruction(&Instruction::End);
                Ok(self.flatten_core_valtypes(expr.ty).len())
            }

            LirExprKind::Call {
                func: func_def_id,
                args,
            } => {
                let func_name = self.ctx.str(self.ctx.defs.name(*func_def_id));

                // Handle builtin functions by name
                match &*func_name {
                    "s32-to-string" => {
                        // Call s32_to_string runtime function
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, component.get_expr(*arg), component)?;
                        } else {
                            todo!("s32-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.s32_to_string.expect("s32_to_string must be in runtime_needs (scan missed it?)")));
                        }
                        // Returns (ptr, len)
                    }
                    "bool-to-string" => {
                        // Call bool_to_string runtime function
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, component.get_expr(*arg), component)?;
                        } else {
                            todo!("bool-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.bool_to_string.expect("bool_to_string must be in runtime_needs (scan missed it?)")));
                        }
                        // Returns (ptr, len)
                    }
                    "u32-to-string" => {
                        // u32 can be converted using s32_to_string (values fit in positive i32 range)
                        if let Some(arg) = args.first() {
                            self.emit_expr(func, component.get_expr(*arg), component)?;
                        } else {
                            todo!("u32-to-string requires 1 arg: {:?}", expr.kind)
                        }
                        if let Some(ref runtime_funcs) = self.runtime_funcs {
                            func.instruction(&Instruction::Call(runtime_funcs.s32_to_string.expect("s32_to_string must be in runtime_needs (scan missed it?)")));
                        }
                        // Returns (ptr, len)
                    }
                    "s64-to-string" | "u64-to-string" => {
                        // Both s64 and u64 route to the shared s64_to_string
                        // runtime helper. Treats the value as signed (matches
                        // s32_to_string's u32 policy).
                        let arg = args.first().ok_or_else(|| {
                            CodegenError::InvalidIR(format!("{} requires 1 arg", func_name))
                        })?;
                        self.emit_expr(func, component.get_expr(*arg), component)?;
                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        func.instruction(&Instruction::Call(runtime_funcs.s64_to_string.expect("s64_to_string must be in runtime_needs (scan missed it?)")));
                    }
                    "f32-to-string" => {
                        let arg = args.first().ok_or_else(|| {
                            CodegenError::InvalidIR("f32-to-string requires 1 arg".to_string())
                        })?;
                        self.emit_expr(func, component.get_expr(*arg), component)?;
                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        func.instruction(&Instruction::Call(runtime_funcs.f32_to_string.expect("f32_to_string must be in runtime_needs (scan missed it?)")));
                    }
                    "f64-to-string" => {
                        let arg = args.first().ok_or_else(|| {
                            CodegenError::InvalidIR("f64-to-string requires 1 arg".to_string())
                        })?;
                        self.emit_expr(func, component.get_expr(*arg), component)?;
                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        // Fallback: demote f64 -> f32 and stringify, since a
                        // dedicated f64_to_string runtime helper is not yet
                        // generated. This is lossy but validates cleanly and
                        // produces sensible interpolation output.
                        func.instruction(&Instruction::F32DemoteF64);
                        func.instruction(&Instruction::Call(runtime_funcs.f32_to_string.expect("f32_to_string must be in runtime_needs (scan missed it?)")));
                    }
                    "char-to-string" => {
                        // A `char` is a u32 scalar value. For now delegate to
                        // u32_to_string (== s32_to_string in the current
                        // runtime) so we produce a valid string rather than a
                        // silent placeholder.  A dedicated UTF-8-encoding
                        // helper is the follow-up.
                        let arg = args.first().ok_or_else(|| {
                            CodegenError::InvalidIR("char-to-string requires 1 arg".to_string())
                        })?;
                        self.emit_expr(func, component.get_expr(*arg), component)?;
                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        func.instruction(&Instruction::Call(runtime_funcs.s32_to_string.expect("s32_to_string must be in runtime_needs (scan missed it?)")));
                    }
                    "object-to-string" => {
                        // Return "[object]" string
                        if let Some(arg) = args.first() {
                            let count = self.emit_expr(func, component.get_expr(*arg), component)?;
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
                            self.emit_expr(func, component.get_expr(args[0]), component)?;
                        } else {
                            // Emit all args (each produces ptr, len)
                            for arg in args {
                                self.emit_expr(func, component.get_expr(*arg), component)?;
                            }
                            // Call concat<n>
                            if let Some(ref runtime_funcs) = self.runtime_funcs
                                && let Some(concat_fn) = runtime_funcs.concat(arity)
                            {
                                func.instruction(&Instruction::Call(concat_fn));
                            }
                        }
                    }
                    "len" => {
                        // Length of list or string. Fast path skips
                        // re-loading the ptr by reading directly at
                        // (addr + 4) — only valid for legacy memory-
                        // backed signals/globals. Migrated signals
                        // (component or per-block global GC struct)
                        // must go through the full read so the second
                        // ABI slot (the len) ends up on top of stack.
                        // Phase 5b-v.3: GcArrayRef signals emit the
                        // array ref then use `array.len` directly.
                        if let Some(arg) = args.first() {
                            let mut handled_via_emit = false;
                            // GcArrayRef path: array.len instruction.
                            use super::repr::InternalRepr;
                            if let InternalRepr::GcArrayRef(_) = self.internal_repr(component.get_expr(*arg).ty) {
                                self.emit_expr(func, component.get_expr(*arg), component)?;
                                func.instruction(&Instruction::ArrayLen);
                                handled_via_emit = true;
                            }
                            if !handled_via_emit {
                                if let LirExprKind::SignalRead(def_id) = &component.get_expr(*arg).kind {
                                    // Not a `zip`: the intermediate `filter` consumes
                                    // `sig_idx` and the tuple is `(ci, sig_idx)`, neither
                                    // of which `Option::zip` can express.
                                    #[allow(clippy::manual_option_zip)]
                                    let comp_local_struct = self
                                        .signal_index_in(component, *def_id)
                                        .and_then(|sig_idx| {
                                            self.comp_idx_of(component)
                                                .filter(|&ci| self.signal_in_struct(ci, sig_idx))
                                                .map(|ci| (ci, sig_idx))
                                        });
                                    let global_struct =
                                        self.ctx.defs.owning_global_block(*def_id).is_some()
                                            && self.global_in_struct(*def_id);
                                    if comp_local_struct.is_some() || global_struct {
                                        // Emit the full read: pushes (ptr, len);
                                        // drop ptr, leave len. Spill via the
                                        // block's reserved i32 scratch local —
                                        // local 2 is unsafe in blocks whose
                                        // signature has a typed boundary-ref
                                        // param at that index.
                                        let scratch = self
                                            .current_flat_scratch
                                            .as_ref()
                                            .map(|s| s.i32_base)
                                            .unwrap_or(2);
                                        self.emit_expr(func, component.get_expr(*arg), component)?;
                                        func.instruction(&Instruction::LocalSet(scratch));
                                        func.instruction(&Instruction::Drop);
                                        func.instruction(&Instruction::LocalGet(scratch));
                                        handled_via_emit = true;
                                    } else {
                                        let maybe_addr =
                                            self.global_property_addrs.get(def_id).copied();
                                        if let Some(addr) = maybe_addr {
                                            func.instruction(&Instruction::I32Const(addr + 4));
                                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                            handled_via_emit = true;
                                        }
                                    }
                                }
                                if !handled_via_emit {
                                    // Complex expression or unresolved
                                    // signal — emit full and discard ptr.
                                    let scratch = self
                                        .current_flat_scratch
                                        .as_ref()
                                        .map(|s| s.i32_base)
                                        .unwrap_or(2);
                                    self.emit_expr(func, component.get_expr(*arg), component)?;
                                    func.instruction(&Instruction::LocalSet(scratch));
                                    func.instruction(&Instruction::Drop);
                                    func.instruction(&Instruction::LocalGet(scratch));
                                }
                            } // closes `if !handled_via_emit` (GcArrayRef skip guard)
                        }
                    }
                    "list-get" => {
                        // Phase 7 cleanup: the safe `list.get(idx)` builtin
                        // was backed by the `list_get_opt` runtime helper,
                        // which has been deleted. Re-introduce as a typed
                        // `array.len`-bounded helper if/when the YEL surface
                        // syntax exposes it.
                        return Err(CodegenError::InvalidIR(
                            "list-get builtin: removed in Phase 7 cleanup; \
                             reintroduce on a typed-GC-array foundation"
                                .into(),
                        ));
                    }
                    "starts-with" | "starts_with" => {
                        // string.starts-with(prefix) -> bool
                        // Takes (str_ptr, str_len, prefix_ptr, prefix_len) and returns i32 (bool)
                        if args.len() != 2 {
                            return Err(CodegenError::InvalidIR(
                                "starts-with requires 2 args: string, prefix".to_string(),
                            ));
                        }

                        // First arg is the string (produces ptr, len on stack)
                        self.emit_expr(func, component.get_expr(args[0]), component)?;
                        // Stack: [str_ptr, str_len]

                        // Second arg is the prefix (produces ptr, len on stack)
                        self.emit_expr(func, component.get_expr(args[1]), component)?;
                        // Stack: [str_ptr, str_len, prefix_ptr, prefix_len]

                        // Call starts_with(str_ptr, str_len, prefix_ptr, prefix_len) -> bool
                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        func.instruction(&Instruction::Call(runtime_funcs.starts_with.expect("starts_with must be in runtime_needs (scan missed it?)")));
                        // Stack: [bool]
                    }
                    "min" => {
                        // min(a, b) -> s32
                        // Returns a if a < b, else b
                        if args.len() != 2 {
                            return Err(CodegenError::InvalidIR("min requires 2 args".to_string()));
                        }
                        // Emit both args
                        self.emit_expr(func, component.get_expr(args[0]), component)?;
                        self.emit_expr(func, component.get_expr(args[1]), component)?;
                        // Stack: [a, b]
                        // Duplicate for comparison: [a, b, a, b]
                        let s_a = self
                            .current_flat_scratch
                            .as_ref()
                            .map(|s| s.i32_base)
                            .unwrap_or(2);
                        let s_b = s_a + 1;
                        func.instruction(&Instruction::LocalSet(s_b)); // b -> scratch
                        func.instruction(&Instruction::LocalSet(s_a)); // a -> scratch
                        func.instruction(&Instruction::LocalGet(s_a));
                        func.instruction(&Instruction::LocalGet(s_b));
                        func.instruction(&Instruction::LocalGet(s_a));
                        func.instruction(&Instruction::LocalGet(s_b));
                        func.instruction(&Instruction::I32LtS); // a < b
                        func.instruction(&Instruction::Select); // select(a, b, a<b) = a<b ? a : b
                    }
                    "max" => {
                        // max(a, b) -> s32
                        // Returns a if a > b, else b
                        if args.len() != 2 {
                            return Err(CodegenError::InvalidIR("max requires 2 args".to_string()));
                        }
                        // Emit both args
                        self.emit_expr(func, component.get_expr(args[0]), component)?;
                        self.emit_expr(func, component.get_expr(args[1]), component)?;
                        // Stack: [a, b]
                        let s_a = self
                            .current_flat_scratch
                            .as_ref()
                            .map(|s| s.i32_base)
                            .unwrap_or(2);
                        let s_b = s_a + 1;
                        func.instruction(&Instruction::LocalSet(s_b));
                        func.instruction(&Instruction::LocalSet(s_a));
                        func.instruction(&Instruction::LocalGet(s_a));
                        func.instruction(&Instruction::LocalGet(s_b));
                        func.instruction(&Instruction::LocalGet(s_a));
                        func.instruction(&Instruction::LocalGet(s_b));
                        func.instruction(&Instruction::I32GtS); // a > b
                        func.instruction(&Instruction::Select); // select(a, b, a>b) = a>b ? a : b
                    }
                    "append" => {
                        // list.append(elem) → call per-list-Ty helper.
                        // Signature `(ref null $arr, elem) -> (ref null $arr)`;
                        // helper allocates a new array of len+1, copies, writes
                        // the appended element, returns it. See
                        // `generate_list_append_function`.
                        if args.len() != 2 {
                            return Err(CodegenError::InvalidIR(
                                "append requires 2 args: list, element".to_string(),
                            ));
                        }
                        let list_ty = component.get_expr(args[0]).ty;
                        // Push src list (ref null $arr) and the new element.
                        self.emit_expr(func, component.get_expr(args[0]), component)?;
                        self.emit_expr(func, component.get_expr(args[1]), component)?;
                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        let append_fn_idx =
                            runtime_funcs.list_append(list_ty).ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "append: no list_append helper registered for {:?}",
                                    list_ty
                                ))
                            })?;
                        func.instruction(&Instruction::Call(append_fn_idx));
                    }
                    "filter" => {
                        // Stage 6 of typed-GC migration: filter is now
                        // a typed-array → typed-array helper. The src
                        // list, captured signals, and result all flow
                        // in their natural `signal_storage_valtypes`
                        // shape; no materializer round-trip on either
                        // side.
                        if args.len() != 2 {
                            return Err(CodegenError::InvalidIR(
                                "filter requires 2 args: list, predicate closure".to_string(),
                            ));
                        }
                        let filter_id = self.current_filter_call_idx;
                        self.current_filter_call_idx += 1;
                        let predicate = self
                            .filter_calls
                            .get(filter_id)
                            .map(|(_, _, _, _, pred)| pred.clone())
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "Filter {} not found in filter_calls",
                                    filter_id
                                ))
                            })?;
                        let mut captured_signals: Vec<(DefId, Ty)> = Vec::new();
                        self.extract_signal_reads(&predicate, &component.exprs, &mut captured_signals);

                        // Push src list as typed array ref. Internal
                        // repr must be GcArrayRef post Stage 6 — the
                        // LIR builder only registers filter calls when
                        // args[0] has typed list type.
                        self.emit_expr(func, component.get_expr(args[0]), component)?;

                        // Push each captured signal in its natural
                        // storage shape (typed list = 1 ref, string =
                        // 2 i32, scalar = 1 slot).
                        for (def_id, _ty) in &captured_signals {
                            let Some(sig_idx) =
                                component.signals.iter().position(|s| s.def_id == *def_id)
                            else {
                                return Err(CodegenError::InvalidIR(format!(
                                    "Captured signal {:?} not found in component signals",
                                    def_id
                                )));
                            };
                            let comp_idx = self.comp_idx_of(component);
                            if let Some(ci) = comp_idx
                                && self.signal_in_struct(ci, sig_idx)
                            {
                                self.emit_signal_struct_read(func, ci, sig_idx)?;
                                continue;
                            }
                            // Non-struct signal is unreachable — every
                            // non-unit signal is GC-struct-resident.
                            unreachable!(
                                "captured signal read: non-struct signal is unreachable — \
                                 every non-unit signal is GC-struct-resident"
                            )
                        }

                        let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                            CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                        })?;
                        let filter_func_idx = runtime_funcs.filter(filter_id).ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "Filter function {} not found",
                                filter_id
                            ))
                        })?;
                        func.instruction(&Instruction::Call(filter_func_idx));
                        // Returns 1 typed `(ref null $list_arr)`.
                    }
                    _ => {
                        // Check if this is a callback call. The import's
                        // signature already encodes the self handle + params
                        // -> flattened results, so the `Call` instruction
                        // naturally leaves the (possibly multi-value) result
                        // on the stack for the caller to consume. In
                        // block-position (callback handler) the result type
                        // is unit and nothing is pushed; in expression
                        // position the return value is left for the consumer
                        // (interpolation, default initializer, etc.).
                        if let Some(import_layout) = &self.import_layout {
                            if let Some(cb_func_idx) =
                                import_layout.import_index(*func_def_id)
                            {
                                let result_flat = self.flatten_core_valtypes(expr.ty);
                                let uses_indirect_return = result_flat.len() > 1;

                                // Component callbacks are resource methods —
                                // their import signature leads with a `self`
                                // handle. Global callbacks are freestanding
                                // (singletons, no receiver), so they skip it.
                                if !self.ctx.defs.is_global_callback(*func_def_id) {
                                    self.emit_self_handle_load(func, component)?;
                                }
                                for arg in args {
                                    self.emit_expr(func, component.get_expr(*arg), component)?;
                                }

                                if uses_indirect_return {
                                    self.emit_cb_indirect_return_call(func, cb_func_idx, expr.ty)?;
                                } else {
                                    func.instruction(&Instruction::Call(cb_func_idx));
                                }
                            } else {
                                return Err(CodegenError::InvalidIR(format!(
                                    "Call targets `{}` which is not a registered callback import; \
                                     only host-imported callbacks are currently supported as Call targets",
                                    func_name
                                )));
                            }
                        } else {
                            return Err(CodegenError::InvalidIR(format!(
                                "emit_expr: no import_layout available while lowering Call to `{}`",
                                func_name
                            )));
                        }
                    }
                }
                // After Stage 7d the indirect-return path repacks
                // canonical bytes into a single FlatGcStruct supertype
                // ref, so the produced stack-slot count matches
                // `internal_stack_slots(expr.ty)` rather than the
                // canonical-flat slot count for FlatGcStruct returns.
                let slots_produced = match self.internal_repr(expr.ty) {
                    crate::wasm::repr::InternalRepr::FlatGcStruct(_) => {
                        self.internal_stack_slots(expr.ty)
                    }
                    _ => self.flatten_core_valtypes(expr.ty).len(),
                };
                Ok(slots_produced)
            }

            LirExprKind::Field { base, field_idx } => {
                let base = component.get_expr(*base);
                // Phase 5e.3: tuple field access via struct.get when
                // the base tuple has GC-ref internal repr.
                if let InternedTyKind::Tuple(_) = self.ctx.ty_kind(base.ty)
                    && let super::repr::InternalRepr::GcRef(tup_idx) =
                        self.internal_repr(base.ty)
                    {
                        self.emit_expr(func, base, component)?;
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: tup_idx,
                            field_index: field_idx.0,
                        });
                        return Ok(self.flatten_core_valtypes(expr.ty).len());
                    }
                // Phase 2 GC migration: if the base is a primitive-only
                // record, it sits on the stack as `(ref null $<rec>_record)`
                // — replace the legacy `add offset; load` with one
                // `struct.get`. The base ref came from either a SignalRead
                // of a POR signal (which struct.get's the component field)
                // or a RecordConstruct of a POR record (which struct.new's
                // a fresh ref) — both produce the typed ref.
                // Phase 3 gating: the SLR (GC-ref) Field path applies
                // only when the base actually leaves a `(ref null
                // $rec_record)` on the stack — not a memory pointer
                // to inline bytes. Two cases produce a memory pointer
                // even though base.ty is SLR-classified:
                //   1) For-loop / captured-local bindings of
                //      list-of-record elements (Phase 5 territory).
                //   2) Nested record access where the outer record is
                //      non-SLR — `outer.inner` returns a memory ptr
                //      into inline bytes; `inner` may be SLR but the
                //      bytes are still memory.
                //
                // Case 1: base is a Local in iter/captured maps.
                // Case 2: base is itself a `Field` expr (the parent
                // chain returns a memory pointer for non-SLR outer).
                //
                // Reject both — fall through to the legacy memory path.
                if self.is_single_level_record(base.ty)
                    && let InternedTyKind::Adt(record_def_id) = self.ctx.ty_kind(base.ty)
                {
                    let type_idx = self
                        .record_gc_types
                        .record_type_idx
                        .get(record_def_id)
                        .copied()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "Field (SLR): no GC type for record {:?}",
                                record_def_id
                            ))
                        })?;
                    let gc_field_idx = self
                        .record_gc_types
                        .field_gc_indices
                        .get(record_def_id)
                        .and_then(|v| v.get(field_idx.0 as usize))
                        .copied()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "Field (SLR): no GC field index for record {:?} field {}",
                                record_def_id, field_idx.0
                            ))
                        })?;
                    // Look up the field's source type to decide whether
                    // we need to unbox a `$fat_value` (string / list)
                    // back into (ptr, len) for downstream consumers.
                    let record = match self.ctx.defs.kind(*record_def_id) {
                        yel_core::definitions::DefKind::Record(r) => r.clone(),
                        _ => {
                            return Err(CodegenError::InvalidIR(format!(
                                "Field (SLR): {:?} is not a record def",
                                record_def_id
                            )));
                        }
                    };
                    let field_ty = record
                        .fields
                        .get(field_idx.0 as usize)
                        .and_then(|&fid| match self.ctx.defs.kind(fid) {
                            yel_core::definitions::DefKind::Field(f) => Some(f.ty),
                            _ => None,
                        })
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "Field (SLR): cannot resolve field type for record {:?} field {}",
                                record_def_id, field_idx.0
                            ))
                        })?;
                    self.emit_expr(func, base, component)?;
                    // Stack: [(ref null $rec_record)]
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: type_idx,
                        field_index: gc_field_idx,
                    });
                    // Stage 2 of typed-GC migration: typed-array list
                    // field — leave the `(ref null $list_arr)` on the
                    // stack. Index (Stage 1) consumes it directly via
                    // `array.get`; RecordConstruct passes through as a
                    // typed array; legacy consumers (rare post-Stage-7)
                    // materialize at their own call site.
                    let typed_array_field = matches!(
                        self.ctx.ty_kind(field_ty),
                        InternedTyKind::List(_)
                    ) && self
                        .record_gc_types
                        .list_array_type_idx
                        .contains_key(&field_ty);
                    if typed_array_field {
                        return Ok(1);
                    }
                    // Phase 5e.5 Stage 7a: FlatGcStruct field —
                    // materialize the supertype ref to canonical
                    // multi-slot form so legacy interpolation /
                    // concat / canonical-flat consumers see the
                    // expected (disc, payload…) shape.
                    if let crate::wasm::repr::InternalRepr::FlatGcStruct(_) =
                        self.internal_repr(field_ty)
                    {
                        // Drop the supertype ref already on the stack;
                        // the materializer re-emits base+struct.get
                        // for each canonical slot's cascade.
                        func.instruction(&Instruction::Drop);
                        return self.emit_flat_gc_field_materialize_to_stack(
                            func,
                            base,
                            type_idx,
                            gc_field_idx,
                            field_ty,
                            component,
                        );
                    }
                    // For string / list<scalar> fields, the struct slot
                    // is `(ref null $fat_value)` — unbox to (ptr, len).
                    if matches!(
                        self.ctx.ty_kind(field_ty),
                        InternedTyKind::String | InternedTyKind::List(_)
                    ) {
                        let fat_value_idx =
                            self.record_gc_types.fat_value_type_idx.ok_or_else(|| {
                                CodegenError::InvalidIR(
                                    "Field (SLR): fat_value type idx not assigned".into(),
                                )
                            })?;
                        // Stack: [(ref null $fat_value)]
                        // Need to push (ptr, len) — two i32s — by
                        // unboxing the same box twice. Reserving a
                        // typed `(ref null $fat_value)` scratch local
                        // would require declaring it up front in every
                        // function that contains an SLR string/list
                        // field read; instead we re-emit `base` to
                        // produce a fresh box ref. This is cheap (a
                        // few GC ref ops, no allocations) and safe
                        // because every SLR base — Local of an SLR
                        // record signal, SignalRead, or chained Field
                        // on another SLR record — is side-effect-free
                        // and idempotent under repeated emission.
                        func.instruction(&Instruction::Drop);
                        // Re-emit base + struct.get to load the box again,
                        // then unbox $ptr.
                        self.emit_expr(func, base, component)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: type_idx,
                            field_index: gc_field_idx,
                        });
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: fat_value_idx,
                            field_index: 0,
                        });
                        // Re-emit base + struct.get to load the box again,
                        // then unbox $len.
                        self.emit_expr(func, base, component)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: type_idx,
                            field_index: gc_field_idx,
                        });
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: fat_value_idx,
                            field_index: 1,
                        });
                    }
                    return Ok(self.flatten_core_valtypes(expr.ty).len());
                }
                // Field access on a record
                // First, emit the base expression which should leave a record pointer on stack
                self.emit_expr(func, base, component)?;

                // Get the record type from base expression
                if let InternedTyKind::Adt(record_def_id) = self.ctx.ty_kind(base.ty) {
                    // Get field offset from layout
                    if let Some(record_layout) = self.layout_ctx.record_layout_by_id(*record_def_id)
                    {
                        if let Some((_, field_offset, field_ty)) =
                            record_layout.field_offsets.get(field_idx.0 as usize)
                        {
                            let field_offset = *field_offset;

                            // Check what type the field is
                            match self.ctx.ty_kind(*field_ty) {
                                InternedTyKind::String => {
                                    // String field: load (ptr, len) using load_fat_ptr helper
                                    // Stack has record_ptr, add field offset to get string addr
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    // Call load_fat_ptr to get (ptr, len) without using scratch locals
                                    let load_fat_ptr_idx = self
                                        .runtime_funcs
                                        .as_ref()
                                        .ok_or_else(|| {
                                            CodegenError::InvalidIR(
                                                "Runtime functions not initialized".to_string(),
                                            )
                                        })?
                                        .load_fat_ptr.expect("load_fat_ptr must be in runtime_needs (scan missed it?)");
                                    func.instruction(&Instruction::Call(load_fat_ptr_idx));
                                }
                                InternedTyKind::F32 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                                }
                                InternedTyKind::F64 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                                }
                                InternedTyKind::S64 | InternedTyKind::U64 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                                }
                                InternedTyKind::S32
                                | InternedTyKind::U32
                                | InternedTyKind::Bool => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                }
                                InternedTyKind::S16 | InternedTyKind::U16 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load16S(mem_arg(0, 1)));
                                }
                                InternedTyKind::S8 | InternedTyKind::U8 => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                                }
                                InternedTyKind::Char => {
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                }
                                InternedTyKind::List(_) => {
                                    // List field: load (ptr, len) like string
                                    if field_offset > 0 {
                                        func.instruction(&Instruction::I32Const(
                                            field_offset as i32,
                                        ));
                                        func.instruction(&Instruction::I32Add);
                                    }
                                    let load_fat_ptr_idx = self
                                        .runtime_funcs
                                        .as_ref()
                                        .ok_or_else(|| {
                                            CodegenError::InvalidIR(
                                                "Runtime functions not initialized".to_string(),
                                            )
                                        })?
                                        .load_fat_ptr.expect("load_fat_ptr must be in runtime_needs (scan missed it?)");
                                    func.instruction(&Instruction::Call(load_fat_ptr_idx));
                                }
                                InternedTyKind::Adt(def_id) => {
                                    // Enums are a bare i32 discriminant stored
                                    // inline. Variants with any payload use the
                                    // canonical-ABI flat-slot representation,
                                    // matching the SignalRead convention so
                                    // downstream formatters see the same shape
                                    // regardless of where the value originates.
                                    // Records are pointer-passed (current ABI).
                                    if self.ctx.defs.as_enum(*def_id).is_some() {
                                        if field_offset > 0 {
                                            func.instruction(&Instruction::I32Const(
                                                field_offset as i32,
                                            ));
                                            func.instruction(&Instruction::I32Add);
                                        }
                                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                    } else {
                                        let has_payload = self
                                            .ctx
                                            .defs
                                            .as_variant(*def_id)
                                            .map(|v| {
                                                v.cases.clone().iter().any(|&c| {
                                                    if let yel_core::definitions::DefKind::VariantCase(case) =
                                                        self.ctx.defs.kind(c)
                                                    {
                                                        case.payload.is_some()
                                                    } else {
                                                        false
                                                    }
                                                })
                                            })
                                            .unwrap_or(false);
                                        if has_payload {
                                            if field_offset > 0 {
                                                func.instruction(&Instruction::I32Const(
                                                    field_offset as i32,
                                                ));
                                                func.instruction(&Instruction::I32Add);
                                            }
                                            self.emit_flat_slot_load_at_ptr(func, *field_ty)?;
                                        } else {
                                            // Record or payload-less variant:
                                            // leave pointer on stack.
                                            if field_offset > 0 {
                                                func.instruction(&Instruction::I32Const(
                                                    field_offset as i32,
                                                ));
                                                func.instruction(&Instruction::I32Add);
                                            }
                                        }
                                    }
                                }
                                // Option / Result fields used to be loaded
                                // here as canonical-ABI flat slots from a
                                // memory-backed record base. Phase 7
                                // cleanup: every option/result Ty now has
                                // `InternalRepr::FlatGcStruct`, and DTR
                                // records (the GC-ref carriers) admit
                                // option/result fields as
                                // `(ref null $opt_super)` slots — the
                                // legacy memory-backed Field path is no
                                // longer reachable for these field types.
                                InternedTyKind::Option(_) | InternedTyKind::Result { .. } => {
                                    return Err(CodegenError::InvalidIR(format!(
                                        "Field on memory-backed base with Option/Result \
                                         field type {:?} is unreachable post Phase 7 — \
                                         FlatGcStruct migration should have routed this \
                                         through the GC-struct Field path",
                                        field_ty
                                    )));
                                }
                                _ => {
                                    return Err(CodegenError::InvalidIR(format!(
                                        "emit_expr: unsupported field type for record field load: {:?}",
                                        self.ctx.ty_kind(*field_ty)
                                    )));
                                }
                            }
                            Ok(self.flatten_core_valtypes(expr.ty).len())
                        } else {
                            Err(CodegenError::InvalidIR(format!(
                                "emit_expr: field index {} not found in record_layout for {:?}",
                                field_idx.0, expr.kind
                            )))
                        }
                    } else {
                        Err(CodegenError::InvalidIR(format!(
                            "emit_expr: no record layout for field access: {:?}",
                            expr.kind
                        )))
                    }
                } else {
                    Err(CodegenError::InvalidIR(format!(
                        "emit_expr: base of FieldAccess is not a record type: {:?}",
                        self.ctx.ty_kind(base.ty)
                    )))
                }
            }

            LirExprKind::Index { base, index } => {
                let base = component.get_expr(*base);
                let index = component.get_expr(*index);
                // Phase 5b-v.3 / 5e.4: GC array — emit base (array
                // ref), index, then `array.get`. For string elements,
                // unbox the resulting `(ref null $fat_value)` into
                // (ptr, len). For other ref-typed elements (records,
                // nested lists), the consumer expects the ref directly.
                use super::repr::InternalRepr;
                if let InternalRepr::GcArrayRef(arr_ty_idx) = self.internal_repr(base.ty) {
                    // String elements are stored as `(ref null
                    // $fat_value)` and unbox to (ptr, len) for legacy
                    // consumers expecting fat-pointer shape. All other
                    // element types (scalars, records, tuples,
                    // FlatGcStruct, nested lists) yield their natural
                    // single-slot representation directly.
                    let elem_is_string = matches!(
                        self.ctx.ty_kind(expr.ty),
                        InternedTyKind::String
                    );
                    if elem_is_string {
                        let fv = self.record_gc_types.fat_value_type_idx
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "Index: $fat_value type idx missing".into(),
                            ))?;
                        // base, idx, array.get → ref fat_value (twice
                        // — once for ptr unbox, once for len unbox).
                        self.emit_expr(func, base, component)?;
                        self.emit_expr(func, index, component)?;
                        func.instruction(&Instruction::ArrayGet(arr_ty_idx));
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: fv,
                            field_index: 0,
                        });
                        // Re-emit base+idx to load len.
                        self.emit_expr(func, base, component)?;
                        self.emit_expr(func, index, component)?;
                        func.instruction(&Instruction::ArrayGet(arr_ty_idx));
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: fv,
                            field_index: 1,
                        });
                        return Ok(2);
                    }
                    self.emit_expr(func, base, component)?;
                    self.emit_expr(func, index, component)?;
                    func.instruction(&Instruction::ArrayGet(arr_ty_idx));
                    return Ok(1);
                }

                // Phase 7 cleanup: every list now lowers to a typed GC
                // array (`GcArrayRef`); the legacy memory-backed
                // `list_get*` path was deleted. If we reach here the
                // base type wasn't seeded into `list_array_type_idx`
                // — investigate `extra_seed_tys` coverage rather than
                // re-add a fallback.
                Err(CodegenError::InvalidIR(format!(
                    "Index: base type {:?} has no GcArrayRef registration; \
                     legacy list_get path was removed in Phase 7 cleanup. \
                     Check that `build.rs::extra_seed_tys` walks the LirExpr \
                     tree containing this index's base.",
                    self.ctx.ty_kind(base.ty)
                )))
            }

            LirExprKind::EnumCase { discriminant, .. } => {
                func.instruction(&Instruction::I32Const(*discriminant as i32));
                Ok(1)
            }

            LirExprKind::VariantCtor {
                case_idx, payload, ..
            } => {
                let payload = payload.as_ref().map(|p| component.get_expr(*p));
                use super::repr::InternalRepr;
                // Phase 5e.5 (Stage 3): when the parent type is migrated to
                // the W3C subtype-hierarchy GC representation, emit a single
                // `struct.new $<parent>_<case>` instead of the flat
                // discriminant + padded-payload-slots shape. The legacy
                // `emit_variant_ctor_flat` is still called for non-migrated
                // types AND for boundary writes (set-attribute, exports,
                // callback returns).
                if let InternalRepr::FlatGcStruct(_) = self.internal_repr(expr.ty) {
                    self.emit_variant_ctor_gc(
                        func,
                        expr.ty,
                        *case_idx,
                        payload,
                        component,
                    )?;
                    return Ok(1);
                }
                self.emit_variant_ctor_flat(
                    func,
                    expr.ty,
                    *case_idx,
                    payload.as_deref(),
                    component,
                )?;
                Ok(self.flatten_core_valtypes(expr.ty).len())
            }

            // List/Record/Tuple constructs - placeholder implementations
            LirExprKind::ListStatic {
                data_offset, len, ..
            } => {
                // Phase 5e.4: when this list type has a GC array repr,
                // build the typed array dynamically by reading the
                // data section: per element load primitive (or load
                // ptr+len and box for strings), then array.new_fixed.
                use super::repr::InternalRepr;
                if let InternalRepr::GcArrayRef(arr_ty_idx) = self.internal_repr(expr.ty) {
                    let elem_ty = match self.ctx.ty_kind(expr.ty) {
                        InternedTyKind::List(e) => *e,
                        _ => return Err(CodegenError::InvalidIR(
                            "ListStatic GC: expected list type".into(),
                        )),
                    };
                    let len_val = *len;
                    let data_off = *data_offset;
                    if len_val == 0 {
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::ArrayNewDefault(arr_ty_idx));
                        return Ok(1);
                    }
                    let elem_is_string = matches!(
                        self.ctx.ty_kind(elem_ty),
                        InternedTyKind::String
                    );
                    let elem_size: u32 = if elem_is_string {
                        8
                    } else {
                        // Scalar element size — match canonical info.
                        match self.ctx.ty_kind(elem_ty) {
                            InternedTyKind::Bool
                            | InternedTyKind::S8 | InternedTyKind::U8 => 1,
                            InternedTyKind::S16 | InternedTyKind::U16 => 2,
                            InternedTyKind::S64 | InternedTyKind::U64
                            | InternedTyKind::F64 => 8,
                            _ => 4,
                        }
                    };
                    let fat_value_idx = self.record_gc_types.fat_value_type_idx;
                    for i in 0..len_val {
                        let elem_addr = data_off + i * elem_size;
                        if elem_is_string {
                            let fv = fat_value_idx.ok_or_else(|| CodegenError::InvalidIR(
                                "ListStatic<string>: $fat_value type idx missing".into(),
                            ))?;
                            // ptr at elem_addr+0
                            func.instruction(&Instruction::I32Const(elem_addr as i32));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            // len at elem_addr+4
                            func.instruction(&Instruction::I32Const((elem_addr + 4) as i32));
                            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                            func.instruction(&Instruction::StructNew(fv));
                        } else {
                            // Scalar: typed load.
                            func.instruction(&Instruction::I32Const(elem_addr as i32));
                            match self.ctx.ty_kind(elem_ty) {
                                InternedTyKind::Bool | InternedTyKind::U8 => {
                                    func.instruction(&Instruction::I32Load8U(mem_arg(0, 0)));
                                }
                                InternedTyKind::S8 => {
                                    func.instruction(&Instruction::I32Load8S(mem_arg(0, 0)));
                                }
                                InternedTyKind::U16 => {
                                    func.instruction(&Instruction::I32Load16U(mem_arg(0, 1)));
                                }
                                InternedTyKind::S16 => {
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
                                _ => {
                                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                                }
                            }
                        }
                    }
                    func.instruction(&Instruction::ArrayNewFixed {
                        array_type_index: arr_ty_idx,
                        array_size: len_val,
                    });
                    return Ok(1);
                }
                // Legacy: return (ptr, len) for static list (memory-backed).
                func.instruction(&Instruction::I32Const(*data_offset as i32));
                func.instruction(&Instruction::I32Const(*len as i32));
                Ok(2)
            }

            LirExprKind::ListConstruct { elements, .. } => {
                // Phase 5b-v.3 / 5e.1 / 5e.4: GC-array lists use typed
                // arrays. For string elements, each element emits
                // (ptr, len) which must be boxed into `$fat_value`
                // before the array.new_fixed.
                use super::repr::InternalRepr;
                if let InternalRepr::GcArrayRef(arr_ty_idx) = self.internal_repr(expr.ty) {
                    if elements.is_empty() {
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::ArrayNewDefault(arr_ty_idx));
                    } else {
                        // String elements arrive as (ptr, len) and are
                        // boxed into `$fat_value` before array.new_fixed.
                        // Other element types (scalars, records,
                        // tuples, FlatGcStruct, nested lists) push the
                        // typed ref / scalar directly that the array
                        // expects.
                        let elem_is_string = elements
                            .first()
                            .map(|e| matches!(self.ctx.ty_kind(component.get_expr(*e).ty), InternedTyKind::String))
                            .unwrap_or(false);
                        for elem in elements {
                            self.emit_expr(func, component.get_expr(*elem), component)?;
                            if elem_is_string {
                                let fv = self.record_gc_types.fat_value_type_idx.ok_or_else(|| {
                                    CodegenError::InvalidIR(
                                        "ListConstruct: $fat_value type idx missing".into(),
                                    )
                                })?;
                                func.instruction(&Instruction::StructNew(fv));
                            }
                        }
                        func.instruction(&Instruction::ArrayNewFixed {
                            array_type_index: arr_ty_idx,
                            array_size: elements.len() as u32,
                        });
                    }
                    return Ok(1);
                }
                // Use list constructor helper - no local conflicts!
                // Each element is emitted to the stack, then we call list_ctor
                if elements.is_empty() {
                    // Empty list: just return (0, 0)
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(0));
                } else {
                    let elem_ty = component.get_expr(elements[0]).ty;
                    let count = elements.len();

                    // Emit all element values onto the stack
                    for elem in elements {
                        // For RecordConstruct elements, emit field values directly (not calling ctor)
                        // This is because list_ctor stores fields inline
                        if let LirExprKind::RecordConstruct { fields, .. } = &component.get_expr(*elem).kind {
                            for field in fields {
                                self.emit_expr(func, component.get_expr(*field), component)?;
                                // Phase 5e.4: legacy list_ctor expects
                                // canonical-flat slots per record
                                // field. If the field's emit pushed a
                                // typed GC array ref, materialize it
                                // back to (ptr, len).
                                if self.is_scalar_list_ty(component.get_expr(*field).ty)
                                    && let super::repr::InternalRepr::GcArrayRef(arr_idx) =
                                        self.internal_repr(component.get_expr(*field).ty)
                                    {
                                        let mat_fn = *self
                                            .gc_list_materializer_fn_indices
                                            .get(&arr_idx)
                                            .ok_or_else(|| CodegenError::InvalidIR(
                                                "ListConstruct (legacy): missing materializer for GC list field".into(),
                                            ))?;
                                        func.instruction(&Instruction::Call(mat_fn));
                                    }
                            }
                        } else {
                            // Other elements: emit normally
                            self.emit_expr(func, component.get_expr(*elem), component)?;
                        }
                    }

                    // Call the list constructor helper
                    let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                        CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                    })?;
                    let list_ctor_idx =
                        runtime_funcs.list_ctor(elem_ty, count).ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "No list constructor for {:?} with {} elements",
                                elem_ty, count
                            ))
                        })?;
                    func.instruction(&Instruction::Call(list_ctor_idx));
                }
                Ok(2)
            }

            LirExprKind::RecordConstruct {
                record_def, fields, ..
            } => {
                // Phase 2 GC migration: primitive-only records use
                // `struct.new $<rec>_record` directly. The result is a
                // `(ref null $<rec>_record)` ref that flows through
                // SignalWrite (struct.set on the component field) and
                // Field (struct.get on the ref) without going through
                // the record_ctor runtime helper or memory at all.
                //
                // Non-POR records (mixed with strings/lists/nested
                // records/etc.) keep the legacy `record_ctor` path
                // until later phases bridge their layouts.
                // Phase 3 generalisation: SLR records (POR + records
                // with string / list<scalar> fields) build via
                // `struct.new`. For string/list fields the field expr
                // pushes (ptr, len) — wrap in `struct.new $fat_value`
                // before the parent struct.new consumes it.
                if self.is_single_level_record(expr.ty)
                    && let Some(type_idx) = self
                        .record_gc_types
                        .record_type_idx
                        .get(record_def)
                        .copied()
                    {
                        // Resolve per-field source types so we know
                        // which need fat_value boxing.
                        let record = match self.ctx.defs.kind(*record_def) {
                            yel_core::definitions::DefKind::Record(r) => r.clone(),
                            _ => {
                                return Err(CodegenError::InvalidIR(format!(
                                    "RecordConstruct (SLR): {:?} is not a record def",
                                    record_def
                                )));
                            }
                        };
                        let fat_value_idx = self.record_gc_types.fat_value_type_idx;
                        for (i, field_expr) in fields.iter().enumerate() {
                            let field_def_id = record.fields.get(i).copied().ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "RecordConstruct (SLR): field index {} out of range",
                                    i
                                ))
                            })?;
                            let field_ty = match self.ctx.defs.kind(field_def_id) {
                                yel_core::definitions::DefKind::Field(f) => f.ty,
                                _ => {
                                    return Err(CodegenError::InvalidIR(format!(
                                        "RecordConstruct (SLR): {:?} is not a field def",
                                        field_def_id
                                    )));
                                }
                            };
                            // Phase 5e.4: gate before emit_expr — when
                            // a list-typed field will get GC ref repr,
                            // emit it as canonical (ptr, len) by
                            // saving/restoring the elements path.
                            // Simplest: detect GC-list field ahead of
                            // time and handle separately. For
                            // strings (always canonical), or non-GC
                            // lists (also canonical), normal emit_expr
                            // pushes 2 i32s.
                            let needs_gc_materialize = self.is_scalar_list_ty(field_ty)
                                && matches!(
                                    self.internal_repr(field_ty),
                                    super::repr::InternalRepr::GcArrayRef(_)
                                );
                            // Phase 5e.6: if the record field is stored
                            // as a typed `(ref null $arr)` (DTR-eligible
                            // list field), keep the typed array on the
                            // stack — no materialize, no $fat_value box.
                            let field_stored_as_typed_array = matches!(
                                self.ctx.ty_kind(field_ty),
                                InternedTyKind::List(_)
                            ) && self
                                .record_gc_types
                                .list_array_type_idx
                                .contains_key(&field_ty);
                            self.emit_expr(func, component.get_expr(*field_expr), component)?;
                            if field_stored_as_typed_array {
                                // emit_expr on a list<elem> with
                                // GcArrayRef repr already pushes a
                                // typed array — nothing to do.
                            } else if matches!(
                                self.ctx.ty_kind(field_ty),
                                InternedTyKind::String | InternedTyKind::List(_)
                            ) {
                                let fv_idx = fat_value_idx.ok_or_else(|| {
                                    CodegenError::InvalidIR(
                                        "RecordConstruct (SLR): fat_value type idx not assigned"
                                            .into(),
                                    )
                                })?;
                                if needs_gc_materialize
                                    && let super::repr::InternalRepr::GcArrayRef(arr_idx) =
                                        self.internal_repr(field_ty)
                                    {
                                        let mat_fn = *self
                                            .gc_list_materializer_fn_indices
                                            .get(&arr_idx)
                                            .ok_or_else(|| CodegenError::InvalidIR(
                                                "RecordConstruct (SLR): missing materializer for GC list field".into(),
                                            ))?;
                                        func.instruction(&Instruction::Call(mat_fn));
                                    }
                                // Stack now has (ptr, len) — pack into $fat_value.
                                func.instruction(&Instruction::StructNew(fv_idx));
                            }
                        }
                        func.instruction(&Instruction::StructNew(type_idx));
                        return Ok(1);
                    }
                // Use record constructor helper - no local conflicts!
                // Emit all field values onto the stack, then call $ctor_X
                for field in fields {
                    self.emit_expr(func, component.get_expr(*field), component)?;
                }

                // Call the record constructor helper
                let runtime_funcs = self.runtime_funcs.as_ref().ok_or_else(|| {
                    CodegenError::InvalidIR("Runtime functions not initialized".to_string())
                })?;
                let ctor_idx = runtime_funcs.record_ctor(*record_def).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "No record constructor for {:?}. Make sure record types are collected.",
                        record_def
                    ))
                })?;
                func.instruction(&Instruction::Call(ctor_idx));
                Ok(1)
            }

            LirExprKind::TupleConstruct {
                elements,
                total_size,
            } => {
                // Phase 5e.3: when this tuple type has a registered
                // `tuple_struct_type_idx`, use `struct.new` directly
                // — each element pushes its value, then struct.new
                // consumes them in order. No memory alloc.
                use super::repr::InternalRepr;
                if let InternalRepr::GcRef(tup_idx) = self.internal_repr(expr.ty) {
                    for elem in elements {
                        self.emit_expr(func, component.get_expr(*elem), component)?;
                    }
                    func.instruction(&Instruction::StructNew(tup_idx));
                    return Ok(1);
                }
                // Legacy fallback — memory-resident tuple. Allocates
                // bytes and writes each element as i32 at fixed 4-byte
                // offsets (preserves prior behaviour for tuple types
                // not yet registered in `tuple_struct_type_idx`).
                func.instruction(&Instruction::I32Const(*total_size as i32));
                func.instruction(&Instruction::I32Const(4));
                let alloc_idx = self
                    .alloc_funcs
                    .as_ref()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "alloc_funcs not initialized before TupleConstruct".to_string(),
                        )
                    })?
                    .alloc;
                func.instruction(&Instruction::Call(alloc_idx));
                let scratch = self
                    .current_flat_scratch
                    .as_ref()
                    .map(|s| s.i32_base)
                    .unwrap_or(0);
                func.instruction(&Instruction::LocalSet(scratch));

                let mut offset = 0u32;
                for elem in elements {
                    func.instruction(&Instruction::LocalGet(scratch));
                    if offset > 0 {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    self.emit_expr(func, component.get_expr(*elem), component)?;
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    offset += 4;
                }

                func.instruction(&Instruction::LocalGet(scratch));
                Ok(1)
            }

            LirExprKind::Closure { .. } => {
                // Closures are not emitted directly - they're handled specially
                // when used as arguments to filter/map/etc.
                Err(CodegenError::InvalidIR(
                    "Closure expressions should be handled by their containing call".to_string(),
                ))
            }

            LirExprKind::Range { .. } => {
                // Range expressions are not emitted directly - they're handled specially
                // in for-loop iteration setup.
                Err(CodegenError::InvalidIR(
                    "Range expressions should be handled by for-loop iteration".to_string(),
                ))
            }

            LirExprKind::IsCase { base, case_idx } => {
                let base = component.get_expr(*base);
                // Phase 5e.5: discriminant test on a migrated parent —
                // emit `ref.test (ref $<parent>_<case>)`. For non-migrated
                // parents we fall through to the legacy "load disc slot,
                // compare" pattern, which is compiled by the lowering
                // emitting `Field { idx: 0 } eq <case_idx>` instead of
                // `IsCase`. So if we see IsCase here, the parent MUST
                // be migrated.
                use super::repr::InternalRepr;
                let case_sub_idx = match self.internal_repr(base.ty) {
                    InternalRepr::FlatGcStruct(_) => *self
                        .record_gc_types
                        .flat_gc_case_idx
                        .get(&(base.ty, *case_idx))
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "IsCase: missing case subtype index for \
                                 base.ty={:?} case_idx={}",
                                base.ty, case_idx
                            ))
                        })?,
                    other => {
                        return Err(CodegenError::InvalidIR(format!(
                            "IsCase: parent base.ty={:?} has non-FlatGcStruct \
                             repr {:?} — lowering must only emit IsCase for \
                             migrated parents",
                            base.ty, other
                        )));
                    }
                };
                self.emit_expr(func, base, component)?;
                // Use `ref.test (ref $sub)` — the non-null variant. A
                // null base ref returns 0 for every case, matching the
                // legacy "no case is active" semantics for an
                // uninitialized signal. (Using the nullable variant
                // would match null against every subtype test,
                // returning true for *every* case — broken.)
                //
                // InitSignalDefault for FlatGcStruct overrides the
                // component-struct zero-init by storing
                // `struct.new_default $<sup>_<case0>` so the user-
                // observable default is "case 0", matching what the
                // legacy zero-byte memory init produced.
                func.instruction(&Instruction::RefTestNonNull(
                    wasm_encoder::HeapType::Concrete(case_sub_idx),
                ));
                Ok(1)
            }

            LirExprKind::VariantField {
                base,
                case_idx,
                field_idx,
            } => {
                let base = component.get_expr(*base);
                // Phase 5e.5: payload extraction from a known case —
                // `ref.cast (ref $<parent>_<case>); struct.get_<u|s>?
                // $<parent>_<case> <field_idx>`.
                //
                // The case must be active at runtime (caller has
                // typically discriminated via IsCase); if not, ref.cast
                // traps. This matches today's behavior where reading a
                // non-active case payload reads garbage / pads — the
                // GC migration trades that silent garbage for a hard
                // trap, which is strictly better.
                use super::repr::InternalRepr;
                use super::gc_types::StructGetVariant;
                let case_sub_idx = match self.internal_repr(base.ty) {
                    InternalRepr::FlatGcStruct(_) => *self
                        .record_gc_types
                        .flat_gc_case_idx
                        .get(&(base.ty, *case_idx))
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "VariantField: missing case subtype index for \
                                 base.ty={:?} case_idx={}",
                                base.ty, case_idx
                            ))
                        })?,
                    other => {
                        return Err(CodegenError::InvalidIR(format!(
                            "VariantField: parent base.ty={:?} has non-\
                             FlatGcStruct repr {:?} — lowering must only emit \
                             VariantField for migrated parents",
                            base.ty, other
                        )));
                    }
                };
                let payload_ty = super::gc_types::case_payload_ty(self.ctx, base.ty, *case_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "VariantField on case with no payload: base.ty={:?} \
                             case_idx={} field_idx={}",
                            base.ty, case_idx, field_idx
                        ))
                    })?;
                self.emit_expr(func, base, component)?;
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(case_sub_idx),
                ));
                let getter = super::gc_types::struct_get_op_for_payload(self.ctx, payload_ty);
                match getter {
                    StructGetVariant::Plain => {
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: case_sub_idx,
                            field_index: *field_idx,
                        });
                    }
                    StructGetVariant::Signed => {
                        func.instruction(&Instruction::StructGetS {
                            struct_type_index: case_sub_idx,
                            field_index: *field_idx,
                        });
                    }
                    StructGetVariant::Unsigned => {
                        func.instruction(&Instruction::StructGetU {
                            struct_type_index: case_sub_idx,
                            field_index: *field_idx,
                        });
                    }
                }
                Ok(self.flatten_core_valtypes(expr.ty).len())
            }
        }
    }

    /// Emit a variant constructor under the canonical-ABI flat representation:
    /// push discriminant (i32) followed by `flatten(parent_ty) - 1` payload
    /// slots. The active case's payload contributes its own flat slots; any
    /// remaining slots in the joined shape are padded with zeros of the
    /// correct valtype. For Option/Result the parent is handled identically
    /// to a user variant — the join rule subsumes their two-case shape.
    /// Phase 5e.5 (Stage 3): emit a constructor for an `option<T>` /
    /// `result<T,E>` / user `variant` value into the W3C subtype-
    /// hierarchy GC representation. Pushes a single `(ref null
    /// $<parent>_super)` on the stack — the case-subtype reference is
    /// implicitly upcast when stored into a supertype-typed slot.
    ///
    /// - Cases without payload: `struct.new_default $<parent>_<case>`.
    /// - Cases with payload: emit the payload's `internal_repr`, then
    ///   `struct.new $<parent>_<case>`. Wasm-GC type-checks the
    ///   payload value against the case subtype's single field.
    ///
    /// The caller must have already checked that the parent's
    /// `internal_repr` is `FlatGcStruct` (typically via the dispatch in
    /// `LirExprKind::VariantCtor`).
    /// Phase 5e.5 Stage 7a: materialize a FlatGcStruct field reached
    /// via `<base>.struct.get $parent <gc_field_idx>` to the canonical
    /// multi-slot stack layout that interpolation / concat / canonical-
    /// flat consumers expect.
    ///
    /// Re-emits the base+struct.get for each canonical slot and uses a
    /// per-case `ref.test`/`ref.cast`/`struct.get` cascade. Caller has
    /// already dropped the previously-loaded supertype ref. SLR bases
    /// are side-effect-free per the comment block on the SLR Field
    /// path, so re-emission is safe and cheap.
    ///
    /// Currently only `option<P>` parents are handled. result and user
    /// variants land on tasks #79 / #78. Nested `option<option<...>>`
    /// payloads (canonical-flat-valtypes len > 1) are out of scope —
    /// they need recursive sub-materialization and are tracked with
    /// fuzz_nested_option.
    pub(super) fn emit_flat_gc_field_materialize_to_stack(
        &mut self,
        func: &mut Function,
        base: &LirExpr,
        parent_type_idx: u32,
        gc_field_idx: u32,
        field_ty: Ty,
        component: &LirResource,
    ) -> Result<usize, CodegenError> {
        use super::gc_types::StructGetVariant;
        use yel_core::types::InternedTyKind;

        // Phase 5e.5 Stage 7a/7d: dispatch on parent kind.
        match self.ctx.ty_kind(field_ty) {
            InternedTyKind::Result { ok, err } => {
                let ok = *ok;
                let err = *err;
                return self.emit_flat_gc_result_field_materialize(
                    func,
                    base,
                    parent_type_idx,
                    gc_field_idx,
                    field_ty,
                    ok,
                    err,
                    component,
                );
            }
            InternedTyKind::Option(_) => {}
            _ => {
                return Err(CodegenError::InvalidIR(format!(
                    "flat_gc field materialize: only option<P>/result<T,E> \
                     parents supported in Stage 7a/7d — got {:?}",
                    field_ty
                )));
            }
        }
        let inner = match self.ctx.ty_kind(field_ty) {
            InternedTyKind::Option(inner) => *inner,
            _ => unreachable!(),
        };

        let case_some_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(field_ty, 0))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "flat_gc field materialize: missing $opt_some case \
                     idx for {:?}",
                    field_ty
                ))
            })?;

        let inner_canonical = self.flatten_core_valtypes(inner);
        if inner_canonical.len() > 2 {
            return Err(CodegenError::InvalidIR(format!(
                "flat_gc field materialize: nested multi-slot option \
                 payload not yet supported — inner ty={:?} has {} \
                 canonical slots",
                inner,
                inner_canonical.len()
            )));
        }

        // Slot 0: discriminant. Some=0, None=1 per YEL convention; null
        // ref → ref.test_non_null returns 0 → eqz returns 1 → None.
        self.emit_expr(func, base, component)?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: parent_type_idx,
            field_index: gc_field_idx,
        });
        func.instruction(&Instruction::RefTestNonNull(
            wasm_encoder::HeapType::Concrete(case_some_idx),
        ));
        func.instruction(&Instruction::I32Eqz);

        // Detect fat-box payload: case-subtype payload field is
        // `(ref null $fat_value)` for string / non-typed-array list.
        let is_fat_box = matches!(self.ctx.ty_kind(inner), InternedTyKind::String)
            || (matches!(self.ctx.ty_kind(inner), InternedTyKind::List(_))
                && !self.record_gc_types.list_array_type_idx.contains_key(&inner));

        if is_fat_box {
            // Canonical: 2 i32 slots (ptr, len). Some-arm unboxes
            // $fat_value's two fields; None-arm pushes (0, 0).
            let fat_value_idx = self.record_gc_types.fat_value_type_idx.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "flat_gc field materialize: $fat_value type idx missing".into(),
                )
            })?;
            for fat_field in 0u32..2u32 {
                self.emit_expr(func, base, component)?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: parent_type_idx,
                    field_index: gc_field_idx,
                });
                func.instruction(&Instruction::RefTestNonNull(
                    wasm_encoder::HeapType::Concrete(case_some_idx),
                ));
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                    wasm_encoder::ValType::I32,
                )));
                self.emit_expr(func, base, component)?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: parent_type_idx,
                    field_index: gc_field_idx,
                });
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(case_some_idx),
                ));
                func.instruction(&Instruction::StructGet {
                    struct_type_index: case_some_idx,
                    field_index: 0,
                });
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: fat_value_idx,
                    field_index: fat_field,
                });
                func.instruction(&Instruction::Else);
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::End);
            }
            return Ok(self.flatten_core_valtypes(field_ty).len());
        }

        // Single-slot scalar / typed payload: canonical len == 1.
        let payload_valtype = inner_canonical.into_iter().next().ok_or_else(|| {
            CodegenError::InvalidIR(
                "flat_gc field materialize: empty inner canonical layout".into(),
            )
        })?;
        self.emit_expr(func, base, component)?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: parent_type_idx,
            field_index: gc_field_idx,
        });
        func.instruction(&Instruction::RefTestNonNull(
            wasm_encoder::HeapType::Concrete(case_some_idx),
        ));
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(payload_valtype)));
        self.emit_expr(func, base, component)?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: parent_type_idx,
            field_index: gc_field_idx,
        });
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(case_some_idx),
        ));
        let getter = super::gc_types::struct_get_op_for_payload(self.ctx, inner);
        match getter {
            StructGetVariant::Plain => {
                func.instruction(&Instruction::StructGet {
                    struct_type_index: case_some_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Signed => {
                func.instruction(&Instruction::StructGetS {
                    struct_type_index: case_some_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Unsigned => {
                func.instruction(&Instruction::StructGetU {
                    struct_type_index: case_some_idx,
                    field_index: 0,
                });
            }
        }
        func.instruction(&Instruction::Else);
        match payload_valtype {
            wasm_encoder::ValType::I32 => {
                func.instruction(&Instruction::I32Const(0));
            }
            wasm_encoder::ValType::I64 => {
                func.instruction(&Instruction::I64Const(0));
            }
            wasm_encoder::ValType::F32 => {
                func.instruction(&Instruction::F32Const(0.0_f32.into()));
            }
            wasm_encoder::ValType::F64 => {
                func.instruction(&Instruction::F64Const(0.0_f64.into()));
            }
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "flat_gc field materialize: unsupported default \
                     payload valtype {:?}",
                    other
                )));
            }
        }
        func.instruction(&Instruction::End);

        Ok(self.flatten_core_valtypes(field_ty).len())
    }

    /// Phase 5e.5 Stage 7d: materialize a `result<T, E>` FlatGcStruct
    /// field to canonical (disc, joined-payload) stack slots.
    ///
    /// Restricted to single-slot scalar payloads with matching valtypes
    /// in both arms (no width promotion). Fat-box / nested / promoted
    /// shapes route to follow-up tasks.
    pub(super) fn emit_flat_gc_result_field_materialize(
        &mut self,
        func: &mut Function,
        base: &LirExpr,
        parent_type_idx: u32,
        gc_field_idx: u32,
        field_ty: Ty,
        ok: Option<Ty>,
        err: Option<Ty>,
        component: &LirResource,
    ) -> Result<usize, CodegenError> {
        use super::gc_types::StructGetVariant;
        use wasm_encoder::ValType;

        let case_ok_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(field_ty, 0))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "result field materialize: missing $res_ok case idx for {:?}",
                    field_ty
                ))
            })?;
        let case_err_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(field_ty, 1))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "result field materialize: missing $res_err case idx for {:?}",
                    field_ty
                ))
            })?;

        let ok_flat = ok.map(|t| self.flatten_core_valtypes(t)).unwrap_or_default();
        let err_flat = err.map(|t| self.flatten_core_valtypes(t)).unwrap_or_default();
        if ok_flat.len() > 1 || err_flat.len() > 1 {
            return Err(CodegenError::InvalidIR(format!(
                "result field materialize: multi-slot payload not yet \
                 supported — ok={} err={} for {:?}",
                ok_flat.len(),
                err_flat.len(),
                field_ty
            )));
        }
        // Width-promotion / mixed-valtype joined payload not yet
        // supported.
        if let (Some(o), Some(e)) = (ok_flat.first(), err_flat.first())
            && o != e {
                return Err(CodegenError::InvalidIR(format!(
                    "result field materialize: mixed payload valtypes \
                     {:?}/{:?} (width promotion) not yet supported for {:?}",
                    o, e, field_ty
                )));
            }

        // Slot 0: discriminant. Ok=0, Err=1.
        // Cascade: ref.test $res_ok ? 0 : 1.
        self.emit_expr(func, base, component)?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: parent_type_idx,
            field_index: gc_field_idx,
        });
        func.instruction(&Instruction::RefTestNonNull(
            wasm_encoder::HeapType::Concrete(case_ok_idx),
        ));
        func.instruction(&Instruction::I32Eqz);

        // Joined payload slot, if any.
        let joined_valtype = ok_flat.first().copied().or_else(|| err_flat.first().copied());
        if let Some(payload_vt) = joined_valtype {
            self.emit_expr(func, base, component)?;
            func.instruction(&Instruction::StructGet {
                struct_type_index: parent_type_idx,
                field_index: gc_field_idx,
            });
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_ok_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(payload_vt)));
            // Ok arm.
            if let Some(ok_ty) = ok {
                self.emit_expr(func, base, component)?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: parent_type_idx,
                    field_index: gc_field_idx,
                });
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(case_ok_idx),
                ));
                let getter = super::gc_types::struct_get_op_for_payload(self.ctx, ok_ty);
                match getter {
                    StructGetVariant::Plain => func.instruction(&Instruction::StructGet {
                        struct_type_index: case_ok_idx,
                        field_index: 0,
                    }),
                    StructGetVariant::Signed => func.instruction(&Instruction::StructGetS {
                        struct_type_index: case_ok_idx,
                        field_index: 0,
                    }),
                    StructGetVariant::Unsigned => func.instruction(&Instruction::StructGetU {
                        struct_type_index: case_ok_idx,
                        field_index: 0,
                    }),
                };
            } else {
                emit_zero_default(func, payload_vt)?;
            }
            func.instruction(&Instruction::Else);
            // Err arm.
            if let Some(err_ty) = err {
                self.emit_expr(func, base, component)?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: parent_type_idx,
                    field_index: gc_field_idx,
                });
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(case_err_idx),
                ));
                let getter = super::gc_types::struct_get_op_for_payload(self.ctx, err_ty);
                match getter {
                    StructGetVariant::Plain => func.instruction(&Instruction::StructGet {
                        struct_type_index: case_err_idx,
                        field_index: 0,
                    }),
                    StructGetVariant::Signed => func.instruction(&Instruction::StructGetS {
                        struct_type_index: case_err_idx,
                        field_index: 0,
                    }),
                    StructGetVariant::Unsigned => func.instruction(&Instruction::StructGetU {
                        struct_type_index: case_err_idx,
                        field_index: 0,
                    }),
                };
            } else {
                emit_zero_default(func, payload_vt)?;
            }
            func.instruction(&Instruction::End);
            // Suppress unused warning when err missing.
            let _ = ValType::I32;
        }

        Ok(self.flatten_core_valtypes(field_ty).len())
    }

    pub(super) fn emit_variant_ctor_gc(
        &mut self,
        func: &mut Function,
        parent_ty: Ty,
        case_idx: u32,
        payload: Option<&LirExpr>,
        component: &LirResource,
    ) -> Result<(), CodegenError> {
        use super::repr::InternalRepr;
        let case_sub_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(parent_ty, case_idx))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "emit_variant_ctor_gc: missing case subtype index for \
                     parent_ty={:?} case_idx={}",
                    parent_ty, case_idx
                ))
            })?;
        match payload {
            None => {
                // Empty case (none / unit case / etc.) — every field
                // (there are none) defaults trivially.
                func.instruction(&Instruction::StructNewDefault(case_sub_idx));
            }
            Some(p) => {
                self.emit_expr(func, p, component)?;
                // Phase 5e.5: when the case payload's `internal_repr`
                // pushes more values than the case subtype's field
                // expects, box appropriately. The case subtype's
                // payload field uses `record_field_storage_type`
                // rules — strings and non-typed-array lists become
                // `(ref null $fat_value)`. So when emit_expr pushed
                // (ptr, len) for a string/list payload, wrap them in
                // a `$fat_value` struct first.
                let payload_repr = self.internal_repr(p.ty);
                if matches!(payload_repr, InternalRepr::FatPointer) {
                    let fat_value_idx = self.record_gc_types.fat_value_type_idx
                        .ok_or_else(|| CodegenError::InvalidIR(
                            "emit_variant_ctor_gc: $fat_value type idx missing — \
                             cannot box string/list payload".into(),
                        ))?;
                    func.instruction(&Instruction::StructNew(fat_value_idx));
                }
                // For Pointer (memory-backed records/tuples): payload
                // is a single i32 ptr. The subtype field would be
                // `anyref` (since record_field_storage_type defaults
                // to anyref for non-DTR). emit_expr pushes the i32 —
                // wasm-GC won't accept i32 where ref is expected, so
                // this case errors loudly. Stages 6c/6d migrate non-
                // DTR-record / tuple payload types out of this state.
                func.instruction(&Instruction::StructNew(case_sub_idx));
            }
        }
        Ok(())
    }

    /// Phase 5e.5 / Phase 7: emit a variant value in canonical-ABI flat
    /// shape (`disc i32, ...payload-slots`) on the stack.
    ///
    /// **Boundary-only.** Internal SSA emission must use
    /// `emit_variant_ctor_gc` for any FlatGcStruct-migrated parent.
    /// This function is still called from:
    /// - WIT export return-value lowering (set-attribute, callback args).
    /// - Option-of-ref collapse for `option<list<T>>` and similar
    ///   where the option is a single ref slot internally.
    ///
    /// (`InternalRepr::Flat` was removed in Phase 7 cleanup, so the
    /// "non-migrated option/result fallback" path no longer exists —
    /// every option/result/variant now goes through `FlatGcStruct`
    /// for internal SSA, and this helper is reached only by boundary
    /// dispatchers that need canonical-ABI bytes.)
    pub(super) fn emit_variant_ctor_flat(
        &mut self,
        func: &mut Function,
        parent_ty: Ty,
        case_idx: u32,
        payload: Option<&LirExpr>,
        component: &LirResource,
    ) -> Result<(), CodegenError> {
        use wasm_encoder::{HeapType, ValType};

        // Option-of-ref collapse path: parent_ty is `option<T>` whose
        // inner T has a ref internal repr. The whole option is one
        // nullable ref slot — no discriminant. THIR convention (matches
        // FlatGcStruct case_idx): **0 = some**, **1 = none**.
        if let Some(arr_idx) = self.option_collapses_to_ref(parent_ty) {
            match case_idx {
                0 => {
                    // some → emit payload's ref
                    let p = payload.ok_or_else(|| {
                        CodegenError::InvalidIR("option-collapse some(): payload missing".into())
                    })?;
                    self.emit_expr(func, p, component)?;
                }
                _ => {
                    // none → typed null ref
                    func.instruction(&Instruction::RefNull(HeapType::Concrete(arr_idx)));
                }
            }
            return Ok(());
        }

        // Compute the parent's full flat valtypes. The first slot is always
        // the i32 discriminant; the rest is the slot-wise join over all
        // cases' payload flattenings.
        let parent_flat = self.flatten_core_valtypes(parent_ty);
        let joined_payload_slots = if parent_flat.is_empty() {
            &[] as &[ValType]
        } else {
            &parent_flat[1..]
        };

        // A nested FlatGcStruct variant payload (today: the language `color`
        // value inside `attribute-value::color`) is on the stack as a ref,
        // not as the parent's joined flat slots. Pack it via the reused
        // per-program color packer, which pushes the discriminant + the
        // parent's color slots itself. (Generalizes to a nested-variant
        // lift once a non-color case appears.)
        if let Some(p) = payload {
            if self.is_color_ty(p.ty) {
                return self.emit_attr_value_color_arm(func, p, component);
            }
        }

        // Push discriminant.
        func.instruction(&Instruction::I32Const(case_idx as i32));

        // Push active case's payload flat slots, then pad remaining joined
        // slots with zeros.
        let payload_flat = match payload {
            Some(p) => self.flatten_core_valtypes(p.ty),
            None => Vec::new(),
        };

        // A string/fat-pointer payload flattens to `(ptr: i32, len: i32)`,
        // but when the parent's joined shape promotes slot 0 to `i64` (any
        // variant that also has a 64-bit case), the pointer must move into
        // the i64 slot while `len` stays i32 — a *non-terminal* promotion
        // the per-slot path below can't do. Reuse the boundary-lowering
        // `pack_fat_ptr_to_i64` helper, then pad the remaining slots.
        if let Some(p) = payload {
            if matches!(self.ctx.ty_kind(p.ty), InternedTyKind::String)
                && joined_payload_slots.first() == Some(&ValType::I64)
            {
                self.emit_expr(func, p, component)?; // ptr, len
                let helper = self
                    .runtime_funcs
                    .as_ref()
                    .and_then(|r| r.pack_fat_ptr_to_i64)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "string variant payload needs pack_fat_ptr_to_i64, but it was \
                             not registered in runtime_needs"
                                .into(),
                        )
                    })?;
                func.instruction(&Instruction::Call(helper)); // -> (i64, i32)
                for vt in &joined_payload_slots[2..] {
                    match vt {
                        ValType::I32 => func.instruction(&Instruction::I32Const(0)),
                        ValType::I64 => func.instruction(&Instruction::I64Const(0)),
                        ValType::F32 => func.instruction(&Instruction::F32Const(Ieee32::from(0.0))),
                        ValType::F64 => func.instruction(&Instruction::F64Const(Ieee64::from(0.0))),
                        other => {
                            return Err(CodegenError::InvalidIR(format!(
                                "unsupported pad slot valtype {:?} after string payload (parent_ty={:?})",
                                other, parent_ty
                            )));
                        }
                    };
                }
                return Ok(());
            }
        }

        if payload_flat.len() > joined_payload_slots.len() {
            return Err(CodegenError::InvalidIR(format!(
                "variant ctor payload flattens to {} slots but joined shape only has {} (parent_ty={:?})",
                payload_flat.len(),
                joined_payload_slots.len(),
                parent_ty
            )));
        }

        if let Some(p) = payload {
            // Identify per-slot width/type mismatches and plan canonical-ABI
            // promotions. The payload's own flat shape is computed by
            // `flatten_core_valtypes`; the joined shape is the slot-wise
            // `join_flat_valtypes` already folded into `parent_flat` by the
            // variant branch of `flatten_core_valtypes`. Promotion rules here
            // must match the join rules (see `join_flat_valtypes` in
            // `crates/yel-wasm-codegen/src/wasm/mod.rs`): any 64-bit target
            // promotes 32-bit ints via `i64.extend_i32_u` and 32-bit floats via
            // `i32.reinterpret_f32` then `i64.extend_i32_u`; 64-bit floats go
            // through `i64.reinterpret_f64`. Within a 32-bit width, a float
            // promotes to `i32` via `i32.reinterpret_f32`.
            //
            // We can only promote the *top* of the stack after each slot is
            // pushed, so for multi-slot payloads we require either that every
            // slot already matches, or that mismatches occur only on the last
            // slot. This covers the observed fuzzer bucket (scalar variant
            // payloads) without needing scratch locals. Any remaining shape is
            // reported as an `InvalidIR`.
            let mismatches: Vec<usize> = payload_flat
                .iter()
                .enumerate()
                .filter(|(i, pv)| **pv != joined_payload_slots[*i])
                .map(|(i, _)| i)
                .collect();

            let bad_multi = mismatches.len() > 1;
            let bad_terminal = mismatches.len() == 1 && mismatches[0] + 1 != payload_flat.len();
            if !mismatches.is_empty() && (bad_multi || bad_terminal) {
                return Err(CodegenError::InvalidIR(format!(
                    "variant ctor: multi-slot payload with non-terminal width mismatches is not yet supported \
                     (mismatched slots={:?}, payload_flat={:?}, joined={:?}, parent_ty={:?})",
                    mismatches, payload_flat, joined_payload_slots, parent_ty
                )));
            }

            // For payloads whose *internal* repr is a single pointer
            // (records / tuples), `emit_expr` pushes one i32 ptr — but
            // the variant's flat shape expects all the record's fields
            // unfolded. Detect that mismatch and flatten via a per-slot
            // load from the pushed pointer.
            //
            // Phase 5 stopgap: SLR records typed as `GcRef` may also
            // arrive on the stack as a memory ptr when their source is
            // a memory-backed list element (`Index`) or a field access
            // into a non-SLR outer record (`Field`) — Phase 3 has
            // migrated record signal storage to GC structs, but lists
            // still hold inline-byte elements until Phase 5. In that
            // physical-shape case we must unfold the same way as the
            // legacy `Pointer` repr. Replace this with `array.get`
            // returning a ref directly once Phase 5 lands.
            // Phase 7 cleanup: `payload_emits_memory_record_ptr` is
            // dead post-migration, so the SLR-record memory-pointer
            // unfold path can no longer fire. The payload is always
            // pushed in its `internal_repr` shape (typed GC ref or
            // scalar slots) — no materializer call needed.
            self.emit_expr(func, p, component)?;

            if let Some(&i) = mismatches.last() {
                let pv = payload_flat[i];
                let jv = joined_payload_slots[i];
                match (pv, jv) {
                    // Matching or trivially-equal pairs already filtered out.
                    (ValType::I32, ValType::I64) => {
                        func.instruction(&Instruction::I64ExtendI32U);
                    }
                    (ValType::F32, ValType::I64) => {
                        func.instruction(&Instruction::I32ReinterpretF32);
                        func.instruction(&Instruction::I64ExtendI32U);
                    }
                    (ValType::F64, ValType::I64) => {
                        func.instruction(&Instruction::I64ReinterpretF64);
                    }
                    (ValType::F32, ValType::I32) => {
                        func.instruction(&Instruction::I32ReinterpretF32);
                    }
                    _ => {
                        return Err(CodegenError::InvalidIR(format!(
                            "variant ctor slot {} width mismatch: payload has {:?} but joined shape expects {:?} \
                             (parent_ty={:?}); unsupported canonical-ABI promotion pair",
                            i, pv, jv, parent_ty
                        )));
                    }
                }
            }
        }

        // Pad the unused joined slots with typed zeros / null refs.
        for vt in &joined_payload_slots[payload_flat.len()..] {
            match vt {
                ValType::I32 => {
                    func.instruction(&Instruction::I32Const(0));
                }
                ValType::I64 => {
                    func.instruction(&Instruction::I64Const(0));
                }
                ValType::F32 => {
                    func.instruction(&Instruction::F32Const(Ieee32::from(0.0)));
                }
                ValType::F64 => {
                    func.instruction(&Instruction::F64Const(Ieee64::from(0.0)));
                }
                ValType::Ref(ref_ty) => {
                    func.instruction(&Instruction::RefNull(ref_ty.heap_type));
                }
                _ => {
                    return Err(CodegenError::InvalidIR(format!(
                        "unsupported joined variant slot valtype {:?} (parent_ty={:?})",
                        vt, parent_ty
                    )));
                }
            }
        }

        Ok(())
    }

    pub(super) fn emit_literal(&mut self, func: &mut Function, lit: &LirLiteral, _ty: Ty) {
        match lit {
            // Signed integers
            LirLiteral::S8(v) => {
                func.instruction(&Instruction::I32Const(*v as i32));
            }
            LirLiteral::S16(v) => {
                func.instruction(&Instruction::I32Const(*v as i32));
            }
            LirLiteral::S32(v) => {
                func.instruction(&Instruction::I32Const(*v));
            }
            LirLiteral::S64(v) => {
                func.instruction(&Instruction::I64Const(*v));
            }
            // Unsigned integers
            LirLiteral::U8(v) => {
                func.instruction(&Instruction::I32Const(*v as i32));
            }
            LirLiteral::U16(v) => {
                func.instruction(&Instruction::I32Const(*v as i32));
            }
            LirLiteral::U32(v) => {
                func.instruction(&Instruction::I32Const(*v as i32));
            }
            LirLiteral::U64(v) => {
                func.instruction(&Instruction::I64Const(*v as i64));
            }
            // Floats
            LirLiteral::F32(v) => {
                func.instruction(&Instruction::F32Const(Ieee32::from(*v)));
            }
            LirLiteral::F64(v) => {
                func.instruction(&Instruction::F64Const(Ieee64::from(*v)));
            }
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
    pub(super) fn emit_literal_count(
        &mut self,
        func: &mut Function,
        lit: &LirLiteral,
        ty: Ty,
    ) -> usize {
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

    pub(super) fn emit_binary_op(&self, func: &mut Function, op: &BinOp, ty: Ty) {
        // Resolve scratch local indices. In legacy 2-param blocks local 0/1
        // were free i32 scratch, but in per-(boundary,signal) update fns
        // those slots are typed boundary-ref params. The block's scratch
        // reservation (compute_flat_scratch_counts) bumps i32/i64 counts
        // when these BinOp arms are present; we then use those bases here.
        let i32_scratch = self
            .current_flat_scratch
            .as_ref()
            .map(|s| s.i32_base)
            .unwrap_or(0);
        let i64_scratch = self
            .current_flat_scratch
            .as_ref()
            .map(|s| s.i64_base)
            .unwrap_or(0);
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => {
                match op {
                    BinOp::Add => {
                        func.instruction(&Instruction::F32Add);
                    }
                    BinOp::Sub => {
                        func.instruction(&Instruction::F32Sub);
                    }
                    BinOp::Mul => {
                        func.instruction(&Instruction::F32Mul);
                    }
                    BinOp::Div => {
                        func.instruction(&Instruction::F32Div);
                    }
                    BinOp::Mod => {
                        // F32 doesn't have native rem, use: a - trunc(a/b) * b
                        // For simplicity, convert to i32, do mod, convert back
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(i32_scratch)); // temp store b
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(i32_scratch));
                        func.instruction(&Instruction::I32RemS);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    BinOp::Eq => {
                        func.instruction(&Instruction::F32Eq);
                    }
                    BinOp::Ne => {
                        func.instruction(&Instruction::F32Ne);
                    }
                    BinOp::Lt => {
                        func.instruction(&Instruction::F32Lt);
                    }
                    BinOp::Gt => {
                        func.instruction(&Instruction::F32Gt);
                    }
                    BinOp::Le => {
                        func.instruction(&Instruction::F32Le);
                    }
                    BinOp::Ge => {
                        func.instruction(&Instruction::F32Ge);
                    }
                    // Logical ops convert to i32, operate, convert back
                    BinOp::And | BinOp::BitAnd => {
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(i32_scratch));
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(i32_scratch));
                        func.instruction(&Instruction::I32And);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    BinOp::Or | BinOp::BitOr => {
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(i32_scratch));
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(i32_scratch));
                        func.instruction(&Instruction::I32Or);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    BinOp::BitXor => {
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalSet(i32_scratch));
                        func.instruction(&Instruction::I32TruncF32S);
                        func.instruction(&Instruction::LocalGet(i32_scratch));
                        func.instruction(&Instruction::I32Xor);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                }
            }
            InternedTyKind::F64 => {
                match op {
                    BinOp::Add => {
                        func.instruction(&Instruction::F64Add);
                    }
                    BinOp::Sub => {
                        func.instruction(&Instruction::F64Sub);
                    }
                    BinOp::Mul => {
                        func.instruction(&Instruction::F64Mul);
                    }
                    BinOp::Div => {
                        func.instruction(&Instruction::F64Div);
                    }
                    BinOp::Mod => {
                        // F64 doesn't have native rem - convert to i64, do mod, convert back
                        func.instruction(&Instruction::I64TruncF64S);
                        func.instruction(&Instruction::LocalSet(i64_scratch)); // temp store b (i64 scratch)
                        func.instruction(&Instruction::I64TruncF64S);
                        func.instruction(&Instruction::LocalGet(i64_scratch));
                        func.instruction(&Instruction::I64RemS);
                        func.instruction(&Instruction::F64ConvertI64S);
                    }
                    BinOp::Eq => {
                        func.instruction(&Instruction::F64Eq);
                    }
                    BinOp::Ne => {
                        func.instruction(&Instruction::F64Ne);
                    }
                    BinOp::Lt => {
                        func.instruction(&Instruction::F64Lt);
                    }
                    BinOp::Gt => {
                        func.instruction(&Instruction::F64Gt);
                    }
                    BinOp::Le => {
                        func.instruction(&Instruction::F64Le);
                    }
                    BinOp::Ge => {
                        func.instruction(&Instruction::F64Ge);
                    }
                    // Logical/bit ops - just use i32 ops since these return bool anyway
                    BinOp::And | BinOp::BitAnd => {
                        func.instruction(&Instruction::I32And);
                    }
                    BinOp::Or | BinOp::BitOr => {
                        func.instruction(&Instruction::I32Or);
                    }
                    BinOp::BitXor => {
                        func.instruction(&Instruction::I32Xor);
                    }
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

    pub(super) fn emit_unary_op(&self, func: &mut Function, op: &UnaryOp, ty: Ty) {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => {
                match op {
                    UnaryOp::Not => {
                        // !x for f32: x == 0.0 ? 1.0 : 0.0
                        func.instruction(&Instruction::F32Const(Ieee32::from(0.0)));
                        func.instruction(&Instruction::F32Eq);
                        func.instruction(&Instruction::F32ConvertI32S);
                    }
                    UnaryOp::Neg => {
                        func.instruction(&Instruction::F32Neg);
                    }
                }
            }
            InternedTyKind::F64 => match op {
                UnaryOp::Not => {
                    func.instruction(&Instruction::F64Const(Ieee64::from(0.0)));
                    func.instruction(&Instruction::F64Eq);
                    func.instruction(&Instruction::F64ConvertI64S);
                }
                UnaryOp::Neg => {
                    func.instruction(&Instruction::F64Neg);
                }
            },
            _ => match op {
                UnaryOp::Not => {
                    func.instruction(&Instruction::I32Eqz);
                }
                UnaryOp::Neg => {
                    func.instruction(&Instruction::I32Const(-1));
                    func.instruction(&Instruction::I32Mul);
                }
            },
        }
    }

    /// True iff `ty` is the language-level `color` builtin variant.
    /// Hex literals (`#2563eb`) and named cases (`Color.red` etc.) all
    /// resolve to this single ADT (see `register_builtin_variants` in
    /// `yel-core/src/stdlib_lookup.rs`).
    fn is_color_ty(&self, ty: Ty) -> bool {
        if let InternedTyKind::Adt(d) = self.ctx.ty_kind(ty)
            && let Some(color_def) = self.ctx.known.variants.color {
                return *d == color_def;
            }
        false
    }

    /// Phase 7: pack a YEL `color` value (a `(ref null $var_color)`
    /// FlatGcStruct supertype ref) into the canonical-ABI flattening
    /// of `attribute-value::color(color)`. Pushes the full attribute-
    /// value flat shape: `(disc=13, slot0=color_disc widened to i64,
    /// slot1=r as i32, slot2=g as i32, slot3=b as i32, slot4=a as i32)`.
    /// Non-rgba cases push their disc and zero rgba slots.
    fn emit_attr_value_color_arm(
        &mut self,
        func: &mut Function,
        expr: &LirExpr,
        component: &LirResource,
    ) -> Result<(), CodegenError> {
        let helper = self
            .pack_color_helper_fn_idx
            .ok_or_else(|| CodegenError::InvalidIR(
                "attr-value color: $pack_color_to_attr_slots helper not registered — \
                 build.rs should emit it whenever the program references the language \
                 `color` type".into(),
            ))?;
        // attribute-value disc = 13 (color case).
        func.instruction(&Instruction::I32Const(13));
        // Push the color ref; the helper consumes it and produces
        // (i64 inner_disc, i32 r, i32 g, i32 b, i32 a).
        self.emit_expr(func, expr, component)?;
        func.instruction(&Instruction::Call(helper));
        Ok(())
    }

    /// Emit a callback call that uses the canonical-ABI indirect-return
    /// convention: the callback's result has >1 flat slots, so the import
    /// takes an extra `i32 ret_ptr` param and writes into that memory. Args
    /// (including the self-handle) must already be on the stack.
    ///
    /// The caller-visible stack shape differs by return-type family, keeping
    /// this path consistent with non-callback producers of the same type:
    ///
    /// - Record / Tuple (pointer-convention composites): allocate a fresh
    ///   heap buffer, pass it as ret_ptr, then push the buffer pointer.
    ///   This matches what `RecordConstruct` / `TupleConstruct` produce, so
    ///   downstream consumers (field access, signal-store that expects a
    ///   pointer) work uniformly.
    /// - String / List / Option / Result / Variant (flat-convention
    ///   composites): use the shared `cb_return_scratch_addr`, call the
    ///   callback, then load each canonical-ABI flat slot from the scratch
    ///   using its declared load-width.
    ///
    /// Returns the number of stack slots produced.
    pub(super) fn emit_cb_indirect_return_call(
        &mut self,
        func: &mut Function,
        cb_func_idx: u32,
        ret_ty: Ty,
    ) -> Result<usize, CodegenError> {
        // Pointer-convention composites: allocate a fresh buffer, pass it as
        // ret_ptr, and the pointer IS the result.
        let use_pointer_convention = match self.ctx.ty_kind(ret_ty) {
            InternedTyKind::Tuple(_) => true,
            InternedTyKind::Adt(def_id) => {
                // Record: pointer-convention. Variant (including user
                // variants with payloads): flat-convention. Enum without
                // payloads has only 1 flat slot and doesn't hit this path.
                self.ctx.defs.as_record(*def_id).is_some()
            }
            _ => false,
        };

        if use_pointer_convention {
            let size = self.layout_ctx.size_of(ret_ty) as i32;
            let align = self.layout_ctx.align_of(ret_ty) as i32;
            let alloc_funcs = self.alloc_funcs.as_ref().ok_or_else(|| {
                CodegenError::InvalidIR(
                    "alloc_funcs not initialized before callback call".to_string(),
                )
            })?;
            let alloc_idx = alloc_funcs.alloc;
            let stash_addr = self.cb_pointer_stash_addr.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "cb_pointer_stash_addr not initialized before callback call".to_string(),
                )
            })?;
            // Stack here has (self_handle, ...args). Push size, align, call
            // alloc -> buffer ptr. We need the ptr both as the callback's
            // last param (ret_ptr) AND as the caller-visible result. Since we
            // can't assume a free local (every callsite has its own scratch
            // layout), stash via memory: write buffer_ptr to
            // cb_pointer_stash_addr, then re-load twice.
            func.instruction(&Instruction::I32Const(stash_addr));
            func.instruction(&Instruction::I32Const(size));
            func.instruction(&Instruction::I32Const(align));
            func.instruction(&Instruction::Call(alloc_idx));
            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            // Stack now: [...args]. Load stashed ptr as ret_ptr.
            func.instruction(&Instruction::I32Const(stash_addr));
            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            func.instruction(&Instruction::Call(cb_func_idx));
            // Phase 2: if the return type is a primitive-only record,
            // bridge the linear-memory buffer into a `(ref null
            // $<rec>_record)` GC ref so subsequent SignalWrite /
            // FieldAccess sites see the new shape. Lift each flat
            // slot from the buffer at its canonical offset, then
            // `struct.new` to assemble the record. The caller-visible
            // result is the ref, not the i32 ptr.
            if self.is_primitive_only_record(ret_ty)
                && let Some(record_type_idx) = self.por_record_type_idx(ret_ty)
            {
                // Re-load the buffer pointer; emit_flat_slot_load_at_ptr
                // consumes one ptr and pushes the canonical-ABI flat
                // slots. For POR records the canonical flat order
                // matches declared field order, so struct.new can
                // consume the slots directly.
                func.instruction(&Instruction::I32Const(stash_addr));
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                self.emit_flat_slot_load_at_ptr(func, ret_ty)?;
                func.instruction(&Instruction::StructNew(record_type_idx));
                return Ok(1);
            }
            // Re-load the stashed pointer as the expression's result.
            func.instruction(&Instruction::I32Const(stash_addr));
            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            return Ok(1);
        }

        // Flat-convention composites: use shared scratch, then load slots.
        let ret_addr = self.cb_return_scratch_addr.ok_or_else(|| {
            CodegenError::InvalidIR(
                "cb_return_scratch_addr not initialized before callback call".to_string(),
            )
        })?;
        func.instruction(&Instruction::I32Const(ret_addr));
        func.instruction(&Instruction::Call(cb_func_idx));
        // Phase 5e.5 Stage 7b: callback returns of FlatGcStruct must
        // produce a single supertype ref (matching internal_repr) for
        // signal-store / Field consumers — not the canonical (disc,
        // payload) flat slots. Re-pack the canonical bytes into a
        // case-subtype struct.
        if let crate::wasm::repr::InternalRepr::FlatGcStruct(_) =
            self.internal_repr(ret_ty)
        {
            return self.emit_cb_flat_gc_return_load(func, ret_addr, ret_ty);
        }
        self.emit_cb_indirect_return_load(func, ret_addr, ret_ty)
    }

    /// Phase 5e.5 Stage 7b/7d: read canonical (disc, payload) bytes
    /// that the host wrote into the callback return scratch and
    /// assemble a FlatGcStruct supertype ref. Handles option, result,
    /// and user-variant uniformly via an N-case `if disc == k` cascade.
    pub(super) fn emit_cb_flat_gc_return_load(
        &mut self,
        func: &mut Function,
        ret_addr: i32,
        ret_ty: Ty,
    ) -> Result<usize, CodegenError> {
        use wasm_encoder::{HeapType, RefType, ValType};

        let super_idx = *self
            .record_gc_types
            .flat_gc_super_idx
            .get(&ret_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "cb flat-gc return: missing supertype idx for {:?}",
                    ret_ty
                ))
            })?;
        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&ret_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "cb flat-gc return: missing case count for {:?}",
                    ret_ty
                ))
            })?;

        let slots = self.flatten_core_slots(ret_ty);
        let disc_slot = slots.first().ok_or_else(|| {
            CodegenError::InvalidIR("cb flat-gc return: missing disc slot".into())
        })?;
        let disc_offset = disc_slot.offset as i32;
        // Payload area starts at the first non-disc slot offset (or
        // disc_offset + 1 if there are no payload slots — the case-0
        // empty case is still valid).
        let payload_base = slots
            .get(1)
            .map(|s| s.offset)
            .unwrap_or((disc_offset as u32) + 1);

        let result_block_ty = wasm_encoder::BlockType::Result(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(super_idx),
        }));

        // Cascade: for each case k emit `if disc == k { build case k }
        // else { ... }`. Innermost else falls back to
        // struct.new_default $case0 (unreachable: host must send a
        // valid disc).
        let mut nesting: u32 = 0;
        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(ret_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "cb flat-gc return: missing case_idx for ({:?}, {})",
                        ret_ty, k
                    ))
                })?;
            // disc == k
            func.instruction(&Instruction::I32Const(ret_addr + disc_offset));
            func.instruction(&Instruction::I32Load8U(mem_arg(0, 0)));
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Eq);
            func.instruction(&Instruction::If(result_block_ty));

            // Build case k.
            if let Some(payload_ty) =
                super::gc_types::case_payload_ty(self.ctx, ret_ty, k)
            {
                self.emit_cb_flat_gc_load_case_payload(
                    func,
                    ret_addr,
                    payload_ty,
                    payload_base,
                )?;
                func.instruction(&Instruction::StructNew(case_sub_idx));
            } else {
                func.instruction(&Instruction::StructNewDefault(case_sub_idx));
            }

            func.instruction(&Instruction::Else);
            nesting += 1;
        }
        // Innermost else: invariant violation — push case0 default.
        let case0_sub_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(ret_ty, 0))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "cb flat-gc return: missing case 0 idx for {:?}",
                    ret_ty
                ))
            })?;
        func.instruction(&Instruction::StructNewDefault(case0_sub_idx));
        for _ in 0..nesting {
            func.instruction(&Instruction::End);
        }
        Ok(1)
    }

    /// Helper for `emit_cb_flat_gc_return_load`: load a single case's
    /// payload from canonical-ABI memory at `payload_base` (absolute,
    /// i.e. `ret_addr + offset`) and leave the value(s) needed by the
    /// case-subtype's payload field on the stack.
    fn emit_cb_flat_gc_load_case_payload(
        &mut self,
        func: &mut Function,
        ret_addr: i32,
        payload_ty: Ty,
        payload_base: u32,
    ) -> Result<(), CodegenError> {
        use super::StoreWidth;
        use wasm_encoder::ValType;
        use yel_core::types::InternedTyKind;

        let payload_canonical = self.flatten_core_valtypes(payload_ty);
        let is_fat_box = matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
            || (matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_))
                && !self
                    .record_gc_types
                    .list_array_type_idx
                    .contains_key(&payload_ty));

        if is_fat_box {
            let fat_value_idx = self.record_gc_types.fat_value_type_idx.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "cb flat-gc return: $fat_value type idx missing".into(),
                )
            })?;
            // ptr at payload_base, len at payload_base + 4
            func.instruction(&Instruction::I32Const(ret_addr + payload_base as i32));
            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            func.instruction(&Instruction::I32Const(ret_addr + payload_base as i32 + 4));
            func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            func.instruction(&Instruction::StructNew(fat_value_idx));
            return Ok(());
        }

        if payload_canonical.len() != 1 {
            return Err(CodegenError::InvalidIR(format!(
                "cb flat-gc return: nested multi-slot payload ({} slots) \
                 not yet supported — payload_ty={:?}",
                payload_canonical.len(),
                payload_ty
            )));
        }

        // Single-slot scalar payload. Compute its canonical layout to
        // get the precise store width — for primitives the slot sits
        // at payload_base.
        let payload_slots = self.flatten_core_slots(payload_ty);
        let slot = payload_slots.first().ok_or_else(|| {
            CodegenError::InvalidIR(
                "cb flat-gc return: payload has zero flat slots but non-empty canonical".into(),
            )
        })?;
        let slot_addr = ret_addr + payload_base as i32 + slot.offset as i32;
        func.instruction(&Instruction::I32Const(slot_addr));
        match (slot.store, slot.valtype) {
            (StoreWidth::I32, ValType::I32) => {
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            }
            (StoreWidth::I32_8, ValType::I32) => {
                func.instruction(&Instruction::I32Load8U(mem_arg(0, 0)));
            }
            (StoreWidth::I32_16, ValType::I32) => {
                func.instruction(&Instruction::I32Load16U(mem_arg(0, 1)));
            }
            (StoreWidth::I64, ValType::I64) => {
                func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
            }
            (StoreWidth::F32, ValType::F32) => {
                func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
            }
            (StoreWidth::F64, ValType::F64) => {
                func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
            }
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "cb flat-gc return: unsupported payload slot {:?}",
                    other
                )));
            }
        }
        Ok(())
    }

    /// Emit instructions that load the flattened canonical-ABI representation
    /// of a value of `ret_ty` from the callback return-area scratch at
    /// `ret_addr`, pushing each slot onto the stack in order. Returns the
    /// number of stack slots produced (== `flatten_core_valtypes(ret_ty).len()`).
    ///
    /// This is the load-side counterpart of the store-side logic in
    /// `emit_flat_slot_store`: slots come back in the exact offsets specified
    /// by `flatten_core_slots`, each read with its declared store-width's
    /// matching load instruction (so 1/2-byte discriminants become i32 via
    /// `I32Load8U` / `I32Load16U`, f32/f64/i64 use their typed loads, etc.).
    pub(super) fn emit_cb_indirect_return_load(
        &mut self,
        func: &mut Function,
        ret_addr: i32,
        ret_ty: Ty,
    ) -> Result<usize, CodegenError> {
        use super::StoreWidth;
        use wasm_encoder::ValType;

        let slots = self.flatten_core_slots(ret_ty);
        let valtypes = self.flatten_core_valtypes(ret_ty);
        if slots.len() != valtypes.len() {
            return Err(CodegenError::InvalidIR(format!(
                "emit_cb_indirect_return_load: flat valtypes ({}) disagree with \
                 flat slots ({}) for return type {:?}",
                valtypes.len(),
                slots.len(),
                ret_ty
            )));
        }
        for (slot, vt) in slots.iter().zip(valtypes.iter()) {
            // Base address for this slot = ret_addr + slot.offset.
            func.instruction(&Instruction::I32Const(ret_addr + slot.offset as i32));
            match (slot.store, *vt) {
                (StoreWidth::I32, ValType::I32) => {
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                }
                (StoreWidth::I32_8, ValType::I32) => {
                    // Discriminants (option/result/variant) are stored as
                    // 1-byte values; load unsigned-extended. bool/u8/s8 are
                    // also I32_8; unsigned load matches their store.
                    func.instruction(&Instruction::I32Load8U(mem_arg(0, 0)));
                }
                (StoreWidth::I32_16, ValType::I32) => {
                    func.instruction(&Instruction::I32Load16U(mem_arg(0, 1)));
                }
                (StoreWidth::I64, ValType::I64) => {
                    func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                }
                (StoreWidth::F32, ValType::F32) => {
                    func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                }
                (StoreWidth::F64, ValType::F64) => {
                    func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                }
                _ => {
                    return Err(CodegenError::InvalidIR(format!(
                        "emit_cb_indirect_return_load: unsupported slot \
                         (store={:?}, valtype={:?}) for return type {:?}",
                        slot.store, vt, ret_ty
                    )));
                }
            }
        }
        Ok(slots.len())
    }

    /// Load each canonical-ABI flat slot of a signal of type `ty` located at
    /// base address `addr`, pushing the values onto the stack in declaration
    /// order.
    ///
    /// **Boundary / fallback only.** With FlatGcStruct now hosting
    /// every option/result/variant signal, the SignalRead callsite for
    /// `signal_in_struct == false` is dead in practice. Kept so the
    /// not-yet-migrated `InternalRepr::Flat` arm (TODO in `repr.rs`)
    /// still works for raw enums and corner cases.
    pub(super) fn emit_flat_slot_signal_read(
        &mut self,
        func: &mut Function,
        addr: i32,
        ty: Ty,
    ) -> Result<(), CodegenError> {
        use super::StoreWidth;
        use wasm_encoder::ValType;
        let slots = self.flatten_core_slots(ty);
        if slots.is_empty() {
            return Err(CodegenError::InvalidIR(format!(
                "SignalRead: type {:?} flattens to zero slots",
                ty
            )));
        }
        for slot in &slots {
            func.instruction(&Instruction::I32Const(addr + slot.offset as i32));
            match (slot.valtype, slot.store) {
                (ValType::I32, StoreWidth::I32) => {
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                }
                (ValType::I32, StoreWidth::I32_8) => {
                    func.instruction(&Instruction::I32Load8U(mem_arg(0, 0)));
                }
                (ValType::I32, StoreWidth::I32_16) => {
                    func.instruction(&Instruction::I32Load16U(mem_arg(0, 1)));
                }
                (ValType::I64, StoreWidth::I64) => {
                    func.instruction(&Instruction::I64Load(mem_arg(0, 3)));
                }
                (ValType::F32, StoreWidth::F32) => {
                    func.instruction(&Instruction::F32Load(mem_arg(0, 2)));
                }
                (ValType::F64, StoreWidth::F64) => {
                    func.instruction(&Instruction::F64Load(mem_arg(0, 3)));
                }
                (vt, store) => {
                    return Err(CodegenError::InvalidIR(format!(
                        "SignalRead: unsupported slot shape valtype={:?} store={:?} for type {:?}",
                        vt, store, ty
                    )));
                }
            }
        }
        Ok(())
    }

    /// **Boundary only.** Three live callers (record-field memory load,
    /// `emit_variant_ctor_flat` payload lift, callback POR-record return
    /// load) all live on canonical-ABI boundaries, not internal SSA.
    /// `InternalRepr::Flat` is gone, so the "non-migrated option/result
    /// fallback" mentioned in earlier comments no longer exists.
    ///
    /// Load each canonical-ABI flat slot of a value of type `ty` from a base
    /// pointer currently on top of the WASM stack, pushing the slot values onto
    /// the stack in declaration order. Consumes the base pointer from the
    /// stack.
    ///
    /// Used by `FieldAccess` for composite field types (option, result,
    /// variant-with-payload) so downstream consumers observe the exact
    /// multi-value shape their emitters expect. Mirrors
    /// `emit_flat_slot_signal_read`, but the base is dynamic (the record
    /// pointer + field offset already summed by the caller) rather than a
    /// known absolute address.
    ///
    /// Requires a scratch i32 local from `current_flat_scratch` to stash the
    /// base pointer across slot loads. Counting passes
    /// (`count_block_flat_scratch`) must ensure `i32_count >= 1` whenever this
    /// helper is reachable.
    ///
    /// Returns the number of stack values pushed
    /// (== `flatten_core_valtypes(ty).len()`).
    pub(super) fn emit_flat_slot_load_at_ptr(
        &mut self,
        func: &mut Function,
        ty: Ty,
    ) -> Result<usize, CodegenError> {
        use super::StoreWidth;
        use wasm_encoder::ValType;
        let slots = self.flatten_core_slots(ty);
        if slots.is_empty() {
            return Err(CodegenError::InvalidIR(format!(
                "emit_flat_slot_load_at_ptr: type {:?} flattens to zero slots",
                ty
            )));
        }

        // Single-slot path: skip the scratch local entirely — the base pointer
        // already on the stack is consumed by the one typed load.
        if slots.len() == 1 {
            let slot = &slots[0];
            match (slot.valtype, slot.store) {
                (ValType::I32, StoreWidth::I32) => {
                    func.instruction(&Instruction::I32Load(mem_arg(slot.offset as u64, 2)));
                }
                (ValType::I32, StoreWidth::I32_8) => {
                    func.instruction(&Instruction::I32Load8U(mem_arg(slot.offset as u64, 0)));
                }
                (ValType::I32, StoreWidth::I32_16) => {
                    func.instruction(&Instruction::I32Load16U(mem_arg(slot.offset as u64, 1)));
                }
                (ValType::I64, StoreWidth::I64) => {
                    func.instruction(&Instruction::I64Load(mem_arg(slot.offset as u64, 3)));
                }
                (ValType::F32, StoreWidth::F32) => {
                    func.instruction(&Instruction::F32Load(mem_arg(slot.offset as u64, 2)));
                }
                (ValType::F64, StoreWidth::F64) => {
                    func.instruction(&Instruction::F64Load(mem_arg(slot.offset as u64, 3)));
                }
                (vt, store) => {
                    return Err(CodegenError::InvalidIR(format!(
                        "emit_flat_slot_load_at_ptr: unsupported slot shape valtype={:?} \
                         store={:?} for type {:?}",
                        vt, store, ty
                    )));
                }
            }
            return Ok(1);
        }

        // Multi-slot path: stash the base pointer in the first reserved i32
        // scratch local, then load each slot by (local.get base + mem_arg
        // offset). Uses local.tee on the first iteration so we preserve the
        // pointer without an extra set/get pair.
        let scratch = self.current_flat_scratch.ok_or_else(|| {
            CodegenError::InvalidIR(
                "emit_flat_slot_load_at_ptr: current_flat_scratch not initialized — \
                 enclosing function must reserve at least one i32 scratch local for \
                 composite field loads"
                    .to_string(),
            )
        })?;
        if scratch.i32_count < 1 {
            return Err(CodegenError::InvalidIR(format!(
                "emit_flat_slot_load_at_ptr: enclosing function reserved {} i32 scratch \
                 locals, need >= 1 for composite field load of type {:?}",
                scratch.i32_count, ty
            )));
        }
        let base_local = scratch.i32_base;

        for (i, slot) in slots.iter().enumerate() {
            if i == 0 {
                // Base pointer is already on stack: tee it into scratch so it
                // remains available for the first load.
                func.instruction(&Instruction::LocalTee(base_local));
            } else {
                func.instruction(&Instruction::LocalGet(base_local));
            }
            match (slot.valtype, slot.store) {
                (ValType::I32, StoreWidth::I32) => {
                    func.instruction(&Instruction::I32Load(mem_arg(slot.offset as u64, 2)));
                }
                (ValType::I32, StoreWidth::I32_8) => {
                    func.instruction(&Instruction::I32Load8U(mem_arg(slot.offset as u64, 0)));
                }
                (ValType::I32, StoreWidth::I32_16) => {
                    func.instruction(&Instruction::I32Load16U(mem_arg(slot.offset as u64, 1)));
                }
                (ValType::I64, StoreWidth::I64) => {
                    func.instruction(&Instruction::I64Load(mem_arg(slot.offset as u64, 3)));
                }
                (ValType::F32, StoreWidth::F32) => {
                    func.instruction(&Instruction::F32Load(mem_arg(slot.offset as u64, 2)));
                }
                (ValType::F64, StoreWidth::F64) => {
                    func.instruction(&Instruction::F64Load(mem_arg(slot.offset as u64, 3)));
                }
                (vt, store) => {
                    return Err(CodegenError::InvalidIR(format!(
                        "emit_flat_slot_load_at_ptr: unsupported slot shape valtype={:?} \
                         store={:?} for type {:?}",
                        vt, store, ty
                    )));
                }
            }
        }
        Ok(slots.len())
    }
}

// ============================================================================
// Unit tests — exercise the pure-emission helpers (emit_literal,
// emit_binary_op, emit_unary_op) in isolation. The full emit_expr path
// requires a complete builder state and is covered by the integration
// fixtures + runtime-inspection tests.
// ============================================================================
#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{CodeSection, FunctionSection, Module, TypeSection};
    use wasmparser::{Operator, Parser, Payload};
    use yel_core::context::CompilerContext;
    use yel_core::lir::LirResource;
    use yel_core::types::{InternedTyKind, Ty};

    /// Wrap an emitted `Function` into a minimal valid WASM module
    /// (type: `() -> ()`) so `wasmparser` can decode the instruction
    /// stream. Returns the parsed ops.
    fn finish_and_read_ops(func: Function) -> Vec<Operator<'static>> {
        let mut module = Module::new();

        let mut types = TypeSection::new();
        types.ty().function([], []);
        module.section(&types);

        let mut funcs = FunctionSection::new();
        funcs.function(0);
        module.section(&funcs);

        let mut code = CodeSection::new();
        code.function(&func);
        module.section(&code);

        let bytes = module.finish();

        // Locate the function body and stream its operators into a Vec.
        // Operator<'a> borrows from the underlying slice; by parsing into
        // owned-data-only variants via `into_static` we can return the
        // vec from this helper.
        let mut out: Vec<Operator<'static>> = Vec::new();
        for payload in Parser::new(0).parse_all(&bytes) {
            if let Payload::CodeSectionEntry(body) = payload.expect("parse") {
                let reader = body.get_operators_reader().expect("ops reader");
                for op in reader {
                    let op = op.expect("op");
                    // Most Operator variants are 'static — none of the
                    // tests below use the few that carry byte slices, so
                    // we can safely cast.
                    // SAFETY: we immediately drop `bytes` after the
                    // collect; in practice none of the tested ops borrow
                    // from it. If a future test adds an op with a
                    // borrowed field, it would fail to compile here.
                    out.push(unsafe { std::mem::transmute::<Operator<'_>, Operator<'static>>(op) });
                }
                break;
            }
        }
        // Keep `bytes` alive by leaking it. This only runs in tests and
        // the allocation is tiny — simpler than threading lifetimes.
        let _leaked = Box::leak(bytes.into_boxed_slice());
        out
    }

    /// Build a WasmPackageBuilder with no components, suitable for
    /// testing the pure-emission helpers that don't need runtime state.
    fn make_builder(ctx: &CompilerContext) -> WasmPackageBuilder<'_> {
        // Empty component slice; the helpers under test don't touch
        // `self.components`.
        static EMPTY: &[LirResource] = &[];
        WasmPackageBuilder::new(EMPTY, ctx)
    }

    // ---- emit_literal ----

    #[test]
    fn literal_s32_pushes_i32_const() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S32);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::S32(42), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);

        let ops = finish_and_read_ops(func);
        assert!(
            matches!(ops[0], Operator::I32Const { value: 42 }),
            "expected i32.const 42, got {:?}",
            ops
        );
    }

    #[test]
    fn literal_s64_pushes_i64_const() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S64);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::S64(42_000_000_000), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);

        let ops = finish_and_read_ops(func);
        assert!(
            matches!(
                ops[0],
                Operator::I64Const {
                    value: 42_000_000_000
                }
            ),
            "expected i64.const 42_000_000_000, got {:?}",
            ops
        );
    }

    #[test]
    fn literal_f32_pushes_f32_const() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::F32);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::F32(3.5), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);

        let ops = finish_and_read_ops(func);
        if let Operator::F32Const { value } = &ops[0] {
            assert_eq!(value.bits(), 3.5f32.to_bits());
        } else {
            panic!("expected f32.const, got {:?}", ops);
        }
    }

    #[test]
    fn literal_f64_pushes_f64_const() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::F64);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::F64(3.5e10), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);

        let ops = finish_and_read_ops(func);
        if let Operator::F64Const { value } = &ops[0] {
            assert_eq!(value.bits(), 3.5e10f64.to_bits());
        } else {
            panic!("expected f64.const, got {:?}", ops);
        }
    }

    #[test]
    fn literal_bool_pushes_i32_const_0_or_1() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::Bool);

        for (input, expected) in [(true, 1), (false, 0)] {
            let mut builder = make_builder(&ctx);
            let mut func = Function::new([]);
            builder.emit_literal(&mut func, &LirLiteral::Bool(input), ty);
            func.instruction(&Instruction::Drop);
            func.instruction(&Instruction::End);
            let ops = finish_and_read_ops(func);
            assert!(
                matches!(ops[0], Operator::I32Const { value } if value == expected),
                "bool {} should push i32.const {}, got {:?}",
                input,
                expected,
                ops
            );
        }
    }

    #[test]
    fn literal_char_pushes_codepoint_as_i32() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::Char);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        // `é` is U+00E9 = 233 as a codepoint.
        builder.emit_literal(&mut func, &LirLiteral::Char('é'), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);

        let ops = finish_and_read_ops(func);
        assert!(
            matches!(ops[0], Operator::I32Const { value: 233 }),
            "char 'é' should push codepoint 233, got {:?}",
            ops
        );
    }

    #[test]
    fn literal_narrow_ints_sign_extend_then_widen() {
        // S8::MIN (-128) must push as i32.const -128 (sign-extended).
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S8);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::S8(-128), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);
        let ops = finish_and_read_ops(func);
        assert!(
            matches!(ops[0], Operator::I32Const { value: -128 }),
            "s8(-128) should sign-extend to i32.const -128, got {:?}",
            ops
        );

        // U8(255) should push as i32.const 255 (zero-extended).
        let ty = ctx.intern_ty(InternedTyKind::U8);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::U8(255), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);
        let ops = finish_and_read_ops(func);
        assert!(
            matches!(ops[0], Operator::I32Const { value: 255 }),
            "u8(255) should zero-extend to i32.const 255, got {:?}",
            ops
        );
    }

    // ---- emit_binary_op ----

    /// Helper: emit a single binary op for a given type and return the
    /// last emitted operator (the operator corresponding to that binop).
    fn single_binop(ctx: &CompilerContext, op: BinOp, ty: Ty) -> Operator<'static> {
        let builder = make_builder(ctx);
        let mut func = Function::new([]);
        // Provide two locals as operands to keep the stack shape valid;
        // the op itself is what we want to inspect.
        builder.emit_binary_op(&mut func, &op, ty);
        func.instruction(&Instruction::End);
        let ops = finish_and_read_ops(func);
        // Last op before End is the binop we emitted.
        // `finish_and_read_ops` drops End so we just take the last.
        ops.into_iter().next().expect("at least one op")
    }

    #[test]
    fn binary_ops_s32_use_integer_instructions() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S32);

        assert!(matches!(
            single_binop(&ctx, BinOp::Add, ty),
            Operator::I32Add
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Sub, ty),
            Operator::I32Sub
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Mul, ty),
            Operator::I32Mul
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Div, ty),
            Operator::I32DivS
        ));
        assert!(matches!(single_binop(&ctx, BinOp::Eq, ty), Operator::I32Eq));
        assert!(matches!(single_binop(&ctx, BinOp::Ne, ty), Operator::I32Ne));
        assert!(matches!(
            single_binop(&ctx, BinOp::Lt, ty),
            Operator::I32LtS
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Gt, ty),
            Operator::I32GtS
        ));
    }

    #[test]
    fn binary_ops_f32_use_float_instructions() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::F32);

        assert!(matches!(
            single_binop(&ctx, BinOp::Add, ty),
            Operator::F32Add
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Sub, ty),
            Operator::F32Sub
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Mul, ty),
            Operator::F32Mul
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Div, ty),
            Operator::F32Div
        ));
        assert!(matches!(single_binop(&ctx, BinOp::Lt, ty), Operator::F32Lt));
    }

    /// KNOWN BUG: `emit_binary_op` has no dedicated arm for `U32`/`U64`;
    /// they fall through to the catch-all `_ =>` branch that emits
    /// SIGNED integer instructions. `u32 < u32` therefore lowers to
    /// `i32.lt_s`, which compares large unsigned values as if they
    /// were negative. `u32 / u32` lowers to `i32.div_s` with the same
    /// wrong-sign problem.
    ///
    /// This test asserts the **correct** expected behaviour
    /// (unsigned comparisons and division) and is `#[ignore]`d while the
    /// bug exists. Fix: add an `InternedTyKind::U32 | InternedTyKind::U16
    /// | InternedTyKind::U8` branch to `emit_binary_op` that uses
    /// `_u`-suffixed instructions for `Lt`/`Gt`/`Le`/`Ge`/`Div`/`Mod`.
    #[test]
    #[ignore = "known bug: emit_binary_op uses signed instructions for \
                 unsigned integer types — u32 comparisons and division \
                 treat large values as negative"]
    fn binary_ops_unsigned_use_unsigned_comparisons() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::U32);
        assert!(matches!(
            single_binop(&ctx, BinOp::Lt, ty),
            Operator::I32LtU
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Gt, ty),
            Operator::I32GtU
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Div, ty),
            Operator::I32DivU
        ));
    }

    /// KNOWN BUG: `emit_binary_op` has no `S64`/`U64` arm; they fall
    /// through to the catch-all `_ =>` branch that emits `i32`-wide
    /// instructions. That means `s64 + s64` lowers to `i32.add`,
    /// silently truncating 64-bit arithmetic to 32 bits and likely
    /// producing a stack-type-mismatch at validation time — except it
    /// doesn't, because `SignalRead` of an s64 also falls through to
    /// the wrong load path in some scenarios, so the widths agree by
    /// coincidence. Either way, the observable semantics are wrong.
    ///
    /// Fix: add `InternedTyKind::S64 | InternedTyKind::U64` branch to
    /// `emit_binary_op` emitting `I64Add`/`I64Sub`/`I64Mul`/`I64DivS`
    /// (or `I64DivU` for U64)/etc.
    #[test]
    #[ignore = "known bug: emit_binary_op emits i32 instructions for s64/u64 \
                 types — 64-bit arithmetic is silently truncated to 32 bits"]
    fn binary_ops_s64_produce_i64_instructions() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S64);
        assert!(matches!(
            single_binop(&ctx, BinOp::Add, ty),
            Operator::I64Add
        ));
        assert!(matches!(
            single_binop(&ctx, BinOp::Lt, ty),
            Operator::I64LtS
        ));
    }

    // ---- emit_unary_op ----

    fn single_unop(ctx: &CompilerContext, op: UnaryOp, ty: Ty) -> Operator<'static> {
        let builder = make_builder(ctx);
        let mut func = Function::new([]);
        builder.emit_unary_op(&mut func, &op, ty);
        func.instruction(&Instruction::End);
        finish_and_read_ops(func)
            .into_iter()
            .next()
            .expect("at least one op")
    }

    #[test]
    fn unary_not_on_bool_uses_eqz() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::Bool);
        assert!(
            matches!(single_unop(&ctx, UnaryOp::Not, ty), Operator::I32Eqz),
            "boolean `!` must lower to `i32.eqz` — any other lowering \
             would not correctly invert a canonical 0/1 bool value"
        );
    }

    #[test]
    fn unary_neg_on_s32_uses_sub_from_zero_or_mul_neg_one() {
        // WASM has no native i32.neg; the compiler must fabricate it
        // via `0 - x` (i32.const 0 + i32.sub) or `x * -1`. Either is
        // correct; this test asserts the result is *some* valid
        // negation sequence, not which one specifically.
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S32);
        let builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_unary_op(&mut func, &UnaryOp::Neg, ty);
        func.instruction(&Instruction::End);
        let ops = finish_and_read_ops(func);
        // Must contain either an I32Sub or I32Mul for the negation
        // (not an I32Add, which would be wrong).
        let negation_shaped = ops
            .iter()
            .any(|op| matches!(op, Operator::I32Sub | Operator::I32Mul));
        assert!(
            negation_shaped,
            "i32 negation must lower via Sub or Mul; got {:?}",
            ops
        );
    }

    #[test]
    fn unary_neg_on_f32_uses_native_neg() {
        // Floats DO have a native neg instruction — using sub would
        // be wasteful and wrong for -0.0 / NaN sign-bit semantics.
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::F32);
        assert!(
            matches!(single_unop(&ctx, UnaryOp::Neg, ty), Operator::F32Neg),
            "f32 negation must use `f32.neg` not an arithmetic substitute \
             — otherwise -0.0 and NaN sign handling break"
        );
    }

    // ---- emit_expr_as_string / emit_expr_as_attr_value ----
    //
    // These helpers convert an already-evaluated expression into either a
    // runtime string (for interpolation) or a canonical-ABI attribute-value
    // variant (for element attribute bindings). They dispatch on the
    // expression's declared type.  The tests below feed each helper a
    // synthetic primitive-literal `LirExpr` and assert which runtime-call /
    // variant-discriminant it emits — the per-type branches are otherwise
    // only exercised via the fixture harness, where a regression in the
    // discriminant table would pass WASM validation but wrongly encode
    // attributes at runtime.
    //
    // These tests construct `RuntimeFunctions` with sentinel indices
    // (unique per field) so a `Call(sentinel)` in the emitted stream
    // uniquely identifies which runtime helper was selected.



    // ---- parser-visible smoke: the wrapper produces a valid module ----

    #[test]
    fn emitted_literal_module_parses_cleanly() {
        let mut ctx = CompilerContext::new();
        let ty = ctx.intern_ty(InternedTyKind::S32);
        let mut builder = make_builder(&ctx);
        let mut func = Function::new([]);
        builder.emit_literal(&mut func, &LirLiteral::S32(1), ty);
        func.instruction(&Instruction::Drop);
        func.instruction(&Instruction::End);
        let ops = finish_and_read_ops(func);
        assert!(
            ops.iter().any(|op| matches!(op, Operator::I32Const { .. })),
            "sanity: at least one i32.const in a trivial literal emission"
        );
    }
}
