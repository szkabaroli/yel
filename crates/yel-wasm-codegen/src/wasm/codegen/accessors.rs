//! Per-signal getter/setter generation + value-coercion helpers.
//!
//! Methods live on `WasmPackageBuilder<'a>` via an additional impl block
//! and are called from `build::build_core_module` during the code section
//! pass.

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::types::InternedTyKind;
use yel_core::{DefId, DefKind, Ty};

use super::super::CodegenError;
use super::super::FlatSlot;
use super::super::WasmPackageBuilder;

/// Phase 5e.5 Stage 7d: when canonical-ABI joined-flat slot type
/// `vt_joined` differs from a case payload's actual valtype
/// `vt_case`, emit the reinterpret bridge. Same-width int/float
/// pairs reinterpret losslessly; same-type pairs no-op; other
/// mismatches return an error so we don't silently miscompile.
fn emit_canonical_reinterpret(
    func: &mut Function,
    vt_joined: ValType,
    vt_case: ValType,
) -> Result<(), CodegenError> {
    if vt_joined == vt_case {
        return Ok(());
    }
    match (vt_joined, vt_case) {
        // Same-width bit reinterprets (join of i32/f32 or i64/f64).
        (ValType::I32, ValType::F32) => {
            func.instruction(&Instruction::F32ReinterpretI32);
        }
        (ValType::F32, ValType::I32) => {
            func.instruction(&Instruction::I32ReinterpretF32);
        }
        (ValType::I64, ValType::F64) => {
            func.instruction(&Instruction::F64ReinterpretI64);
        }
        (ValType::F64, ValType::I64) => {
            func.instruction(&Instruction::I64ReinterpretF64);
        }
        // Width-narrowing joins: the canonical-ABI `join` widens a mixed
        // {i32/f32, i64/f64} slot up to i64, so a case whose payload is 32-bit
        // reads its value out of the low half of the i64 joined slot. The
        // narrow value was stored zero/bit-extended into the low bits, so
        // `i32.wrap_i64` recovers it losslessly (then reinterpret to f32 if
        // the case payload is f32). `join` never produces a slot narrower
        // than a case's payload, so only i64→{i32,f32} occur here.
        (ValType::I64, ValType::I32) => {
            func.instruction(&Instruction::I32WrapI64);
        }
        (ValType::I64, ValType::F32) => {
            func.instruction(&Instruction::I32WrapI64);
            func.instruction(&Instruction::F32ReinterpretI32);
        }
        _ => {
            return Err(CodegenError::InvalidIR(format!(
                "canonical-ABI reinterpret: unsupported joined→case bridge \
                 {:?} → {:?}",
                vt_joined, vt_case
            )));
        }
    }
    Ok(())
}

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn single_slot_getter_type(&self, ty: Ty) -> Result<Option<u32>, CodegenError> {
        use wasm_encoder::ValType;
        let flat = self.canonical_flat_valtypes(ty);
        if flat.len() != 1 {
            return Ok(None);
        }
        Ok(Some(match flat[0] {
            ValType::I32 => self.func_types.getter_i32,
            ValType::F32 => self.func_types.getter_f32,
            ValType::F64 => self.func_types.getter_f64,
            ValType::I64 => self.func_types.getter_i64,
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "single_slot_getter_type: composite type {:?} flattens to \
                     unsupported single slot valtype {:?}",
                    ty, other
                )));
            }
        }))
    }

    /// Whether a canonical value of `ty` transitively contains a `list`,
    /// i.e. whether materialising it into linear memory allocates any fresh
    /// element buffer that a post-return must reclaim. Strings are *aliased*
    /// (their bytes live in persistent / interned storage and are never freshly
    /// copied by a getter), so they do not count; scalars are inline.
    pub(super) fn ty_contains_fresh_list(&self, ty: Ty) -> bool {
        let mut visited = std::collections::HashSet::new();
        self.ty_contains_fresh_list_rec(ty, &mut visited)
    }

    fn ty_contains_fresh_list_rec(
        &self,
        ty: Ty,
        visited: &mut std::collections::HashSet<DefId>,
    ) -> bool {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::List(_) => true,
            // strings-to-GC: a materialized string is a fresh `cabi_realloc`
            // buffer that the getter's post-return must free.
            InternedTyKind::String => true,
            InternedTyKind::Option(inner) => self.ty_contains_fresh_list_rec(*inner, visited),
            InternedTyKind::Result { ok, err } => {
                ok.is_some_and(|t| self.ty_contains_fresh_list_rec(t, visited))
                    || err.is_some_and(|t| self.ty_contains_fresh_list_rec(t, visited))
            }
            InternedTyKind::Tuple(elems) => elems
                .iter()
                .any(|&t| self.ty_contains_fresh_list_rec(t, visited)),
            InternedTyKind::Adt(def_id) => {
                if !visited.insert(*def_id) {
                    return false; // recursive type guard
                }
                if let Some(rec) = self.ctx.defs.as_record(*def_id) {
                    rec.fields.iter().any(|&fid| {
                        matches!(self.ctx.defs.kind(fid), DefKind::Field(f)
                            if self.ty_contains_fresh_list_rec(f.ty, visited))
                    })
                } else if let Some(var) = self.ctx.defs.as_variant(*def_id) {
                    var.cases.iter().any(|&cid| {
                        matches!(self.ctx.defs.kind(cid),
                            yel_core::definitions::DefKind::VariantCase(c)
                            if c.payload.is_some_and(|t| self.ty_contains_fresh_list_rec(t, visited)))
                    })
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Emit the `cabi_post_*` (post-return) body for an aggregate-returning
    /// getter whose result was freshly materialised into linear memory. Only
    /// `signal_in_struct` (GC-migrated) getters reach here — memory-resident
    /// getters return a pointer *into live signal storage* and must never be
    /// freed. Param 0 is the returned pointer. The body frees the freshly
    /// allocated buffer graph: nested `list` element buffers (recursively, via
    /// runtime loops over element counts), then the top-level scratch.
    pub(super) fn generate_cabi_post_getter(
        &mut self,
        ty: Ty,
        free_fn: u32,
    ) -> Result<Function, CodegenError> {
        use wasm_encoder::Instruction;
        let mut out: Vec<Instruction<'static>> = Vec::new();
        let mut locals: u32 = 0;
        let ret_ptr: u32 = 0;
        self.emit_free_region(&mut out, &mut locals, ty, ret_ptr, 0, free_fn)?;
        // Free the top-level scratch itself (size = its canonical in-memory
        // size; `size_of(string|list) == 8`, the fat-pointer pair).
        let size = self.layout_ctx.size_of(ty);
        out.push(Instruction::LocalGet(ret_ptr));
        out.push(Instruction::I32Const(size as i32));
        out.push(Instruction::Call(free_fn));
        out.push(Instruction::End);
        let mut func = Function::new([(locals, ValType::I32)]);
        for ins in &out {
            func.instruction(ins);
        }
        Ok(func)
    }

    /// Recursively emit instructions that free every freshly-allocated `list`
    /// element buffer reachable from a canonical value of `ty` located at
    /// `[base_local] + byte_off`. Does NOT free the region holding the value
    /// itself (the caller owns that). `locals` tracks the count of i32 locals
    /// allocated so far; new loop/scratch locals are handed out at index
    /// `1 + (prior count)` to line up with the function's declared locals.
    #[allow(clippy::too_many_arguments)]
    fn emit_free_region(
        &mut self,
        out: &mut Vec<wasm_encoder::Instruction<'static>>,
        locals: &mut u32,
        ty: Ty,
        base_local: u32,
        byte_off: u32,
        free_fn: u32,
    ) -> Result<(), CodegenError> {
        use wasm_encoder::{BlockType, Instruction, MemArg};
        if !self.ty_contains_fresh_list(ty) {
            return Ok(()); // no fresh buffers under this value
        }
        let ma = |offset: u32, align: u32| MemArg {
            offset: offset as u64,
            align,
            memory_index: 0,
        };
        let alloc_local = |locals: &mut u32| -> u32 {
            *locals += 1;
            *locals
        };
        match self.ctx.ty_kind(ty) {
            InternedTyKind::List(elem) => {
                let elem = *elem;
                let data = alloc_local(locals);
                let len = alloc_local(locals);
                // data = mem[base+byte_off]; len = mem[base+byte_off+4]
                out.push(Instruction::LocalGet(base_local));
                out.push(Instruction::I32Load(ma(byte_off, 2)));
                out.push(Instruction::LocalSet(data));
                out.push(Instruction::LocalGet(base_local));
                out.push(Instruction::I32Load(ma(byte_off + 4, 2)));
                out.push(Instruction::LocalSet(len));
                let (elem_size, _elem_align) =
                    gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem);
                // If elements themselves carry fresh buffers, loop and recurse.
                if self.ty_contains_fresh_list(elem) {
                    let i = alloc_local(locals);
                    let elem_base = alloc_local(locals);
                    out.push(Instruction::I32Const(0));
                    out.push(Instruction::LocalSet(i));
                    out.push(Instruction::Block(BlockType::Empty));
                    out.push(Instruction::Loop(BlockType::Empty));
                    out.push(Instruction::LocalGet(i));
                    out.push(Instruction::LocalGet(len));
                    out.push(Instruction::I32GeU);
                    out.push(Instruction::BrIf(1));
                    // elem_base = data + i * elem_size
                    out.push(Instruction::LocalGet(data));
                    out.push(Instruction::LocalGet(i));
                    out.push(Instruction::I32Const(elem_size as i32));
                    out.push(Instruction::I32Mul);
                    out.push(Instruction::I32Add);
                    out.push(Instruction::LocalSet(elem_base));
                    self.emit_free_region(out, locals, elem, elem_base, 0, free_fn)?;
                    out.push(Instruction::LocalGet(i));
                    out.push(Instruction::I32Const(1));
                    out.push(Instruction::I32Add);
                    out.push(Instruction::LocalSet(i));
                    out.push(Instruction::Br(0));
                    out.push(Instruction::End); // loop
                    out.push(Instruction::End); // block
                }
                // free(data, len * elem_size)
                out.push(Instruction::LocalGet(data));
                out.push(Instruction::LocalGet(len));
                out.push(Instruction::I32Const(elem_size as i32));
                out.push(Instruction::I32Mul);
                out.push(Instruction::Call(free_fn));
            }
            InternedTyKind::Tuple(elems) => {
                let elems: Vec<Ty> = elems.to_vec();
                let mut offset: u32 = 0;
                for elem_ty in elems {
                    let l = self.layout_ctx.layout_of(elem_ty);
                    offset = yel_core::lir::align_to(offset, l.align);
                    self.emit_free_region(out, locals, elem_ty, base_local, byte_off + offset, free_fn)?;
                    offset += l.size;
                }
            }
            InternedTyKind::Option(inner) => {
                let inner = *inner;
                let payload_off = yel_core::lir::align_to(1, self.layout_ctx.align_of(inner));
                // if disc != 0 (some) { free inner }
                out.push(Instruction::LocalGet(base_local));
                out.push(Instruction::I32Load8U(ma(byte_off, 0)));
                out.push(Instruction::If(BlockType::Empty));
                self.emit_free_region(out, locals, inner, base_local, byte_off + payload_off, free_fn)?;
                out.push(Instruction::End);
            }
            InternedTyKind::Result { ok, err } => {
                let (ok, err) = (*ok, *err);
                let a = ok.map(|t| self.layout_ctx.align_of(t)).unwrap_or(1);
                let b = err.map(|t| self.layout_ctx.align_of(t)).unwrap_or(1);
                let payload_off = yel_core::lir::align_to(1, a.max(b).max(1));
                // disc == 0 → ok, else → err
                out.push(Instruction::LocalGet(base_local));
                out.push(Instruction::I32Load8U(ma(byte_off, 0)));
                out.push(Instruction::If(BlockType::Empty)); // disc != 0 → err
                if let Some(err_ty) = err {
                    self.emit_free_region(out, locals, err_ty, base_local, byte_off + payload_off, free_fn)?;
                }
                out.push(Instruction::Else);
                if let Some(ok_ty) = ok {
                    self.emit_free_region(out, locals, ok_ty, base_local, byte_off + payload_off, free_fn)?;
                }
                out.push(Instruction::End);
            }
            InternedTyKind::Adt(def_id) => {
                let def_id = *def_id;
                if let Some(rec) = self.ctx.defs.as_record(def_id) {
                    let fields = rec.fields.clone();
                    let layout = self
                        .layout_ctx
                        .record_layout_by_id(def_id)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "cabi_post: record layout missing for {:?}",
                                def_id
                            ))
                        })?;
                    for (i, _fid) in fields.iter().enumerate() {
                        let (_, foff, fty) = layout.field_offsets[i].clone();
                        self.emit_free_region(out, locals, fty, base_local, byte_off + foff, free_fn)?;
                    }
                } else if let Some(var) = self.ctx.defs.as_variant(def_id) {
                    let vd = var.clone();
                    let var_layout = self.layout_ctx.compute_variant_layout_from_def_public(&vd);
                    let payload_off = var_layout.payload_offset;
                    let discriminant = alloc_local(locals);
                    // Load the discriminant at its canonical width (read
                    // counterpart of `discriminant_store_width`).
                    out.push(Instruction::LocalGet(base_local));
                    out.push(crate::wasm::discriminant_load_instr(
                        var_layout.discriminant_size,
                        byte_off,
                    ));
                    out.push(Instruction::LocalSet(discriminant));
                    for (c, &cid) in vd.cases.iter().enumerate() {
                        let payload = match self.ctx.defs.kind(cid) {
                            yel_core::definitions::DefKind::VariantCase(case) => case.payload,
                            _ => None,
                        };
                        if let Some(pty) = payload {
                            if self.ty_contains_fresh_list(pty) {
                                out.push(Instruction::LocalGet(discriminant));
                                out.push(Instruction::I32Const(c as i32));
                                out.push(Instruction::I32Eq);
                                out.push(Instruction::If(BlockType::Empty));
                                self.emit_free_region(out, locals, pty, base_local, byte_off + payload_off, free_fn)?;
                                out.push(Instruction::End);
                            }
                        }
                    }
                }
                // Enums carry no payload — nothing to free.
            }
            // strings-to-GC: free the string's materialized (ptr, len)
            // buffer allocated by the getter's boundary materialization.
            InternedTyKind::String => {
                let data = alloc_local(locals);
                let len = alloc_local(locals);
                out.push(Instruction::LocalGet(base_local));
                out.push(Instruction::I32Load(ma(byte_off, 2)));
                out.push(Instruction::LocalSet(data));
                out.push(Instruction::LocalGet(base_local));
                out.push(Instruction::I32Load(ma(byte_off + 4, 2)));
                out.push(Instruction::LocalSet(len));
                // free(data, len) — byte array, elem_size 1. Guard len != 0
                // so an empty string (which may carry a 0-size buffer) is a
                // no-op rather than a spurious free of an unowned region.
                out.push(Instruction::LocalGet(len));
                out.push(Instruction::If(BlockType::Empty));
                out.push(Instruction::LocalGet(data));
                out.push(Instruction::LocalGet(len));
                out.push(Instruction::Call(free_fn));
                out.push(Instruction::End);
            }
            // Scalars: no fresh buffer.
            _ => {}
        }
        Ok(())
    }

    /// Gap 3 — pointer-spill trampoline for an exported setter whose flattened
    /// params exceed the canonical-ABI limit (`MAX_FLAT_PARAMS = 16`). The
    /// canonical ABI then passes ALL params via a single pointer to a region
    /// holding the param tuple `(self: borrow, value: T)` in canonical memory
    /// layout. This thin `(ptr) -> ()` shim loads `self` and each of the
    /// value's flat slots from that region and calls the existing wide-signature
    /// setter (`wide_setter_idx`), so the large branch-heavy setter body needs
    /// no changes. `self` occupies offset 0 (a 4-byte handle); the value starts
    /// at its natural alignment after it.
    pub(super) fn generate_setter_spill_trampoline(
        &mut self,
        value_ty: Ty,
        wide_setter_idx: u32,
    ) -> Result<Function, CodegenError> {
        use crate::wasm::StoreWidth;
        use wasm_encoder::{Instruction, MemArg};
        let slots = self.flatten_core_slots(value_ty);
        debug_assert_eq!(
            slots.len(),
            self.canonical_flat_valtypes(value_ty).len(),
            "spill trampoline: flat-slot count must match the wide setter's param count"
        );
        let value_base = yel_core::lir::align_to(4, self.layout_ctx.align_of(value_ty));
        let ma = |off: u32, align: u32| MemArg {
            offset: off as u64,
            align,
            memory_index: 0,
        };
        let mut func = Function::new(Vec::<(u32, ValType)>::new());
        // self handle at region+0
        func.instruction(&Instruction::LocalGet(0));
        func.instruction(&Instruction::I32Load(ma(0, 2)));
        // value's flat slots, loaded from the region at their canonical
        // memory offsets, in the SAME order the wide setter expects. Small
        // ints load zero-extended — the setter narrows on store, so the high
        // bits are irrelevant.
        for s in &slots {
            let off = value_base + s.offset;
            func.instruction(&Instruction::LocalGet(0));
            match s.store {
                StoreWidth::I32 => func.instruction(&Instruction::I32Load(ma(off, 2))),
                StoreWidth::I32_8 => func.instruction(&Instruction::I32Load8U(ma(off, 0))),
                StoreWidth::I32_16 => func.instruction(&Instruction::I32Load16U(ma(off, 1))),
                StoreWidth::I64 => func.instruction(&Instruction::I64Load(ma(off, 3))),
                StoreWidth::F32 => func.instruction(&Instruction::F32Load(ma(off, 2))),
                StoreWidth::F64 => func.instruction(&Instruction::F64Load(ma(off, 3))),
            };
        }
        func.instruction(&Instruction::Call(wide_setter_idx));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// When `comp_idx` is `Some` and the signal has been migrated to
    /// GC-struct storage, materialise
    /// the canonical-ABI return shape from the component's
    /// `$Comp_<i>` struct fields. Multi-slot composites are refreshed
    /// into the legacy `signal_addr` region (now a per-call lift
    /// scratch, not a permanent store) and the scratch pointer is
    /// returned per canonical ABI; primitives bypass memory entirely.
    /// Pointer-typed signals (records/tuples) fall through to the
    /// existing memory-resident path until a later phase migrates
    /// them.
    pub(super) fn generate_getter_for_with_struct(
        &mut self,
        signal_ty: Ty,
        sig_idx: usize,
        comp_idx: Option<usize>,
    ) -> Result<Function, CodegenError> {
        // GC-struct-migrated signal — return value is computed from
        // struct.get instead of memory.load. Single-slot canonical-ABI
        // returns push the value directly; multi-slot returns lift into a
        // `cabi_realloc` scratch and return the scratch pointer. The
        // scalar/memory fallthrough at the tail computes its own `addr`
        // lazily (only reachable for non-struct signals).
        if let Some(ci) = comp_idx
            && self.signal_in_struct(ci, sig_idx)
        {
            // Reserve a `(ref null $Comp_<ci>)` local at index 1
            // (param 0 is `self: i32`) so the registry-lookup
            // sequence has somewhere to store the resolved ref;
            // signal struct.get's then read from `current_self_local`.
            let gc = &self.gc_layouts[ci];
            let struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "getter (struct): component {} missing component_struct_type_idx",
                    ci
                ))
            })?;
            let field_path: Vec<u32> = self.components[ci].signal_layout.signal_field_path(sig_idx);

            // Phase 5b-v.3: GC list getter — copy GC array → canonical ABI (ptr,len).
            // Only matches direct `list<scalar>` signals — option-collapsed
            // signals (option<list<scalar>>) fall through to the
            // multi-slot getter below which materialises the
            // discriminant via null-check.
            if matches!(self.ctx.ty_kind(signal_ty), InternedTyKind::List(_))
                && let super::super::repr::InternalRepr::GcArrayRef(arr_type_idx) =
                    self.internal_repr(signal_ty)
            {
                let cabi_realloc = self
                    .alloc_funcs
                    .as_ref()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("GC list getter requires cabi_realloc".into())
                    })?
                    .cabi_realloc;
                let elem_ty = match self.ctx.ty_kind(signal_ty) {
                    InternedTyKind::List(e) => *e,
                    _ => {
                        return Err(CodegenError::InvalidIR(
                            "GC list getter: signal_ty is not a list".into(),
                        ));
                    }
                };
                let (elem_size, elem_align) =
                    gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
                let elem_record_def: Option<DefId> = match self.ctx.ty_kind(elem_ty) {
                    InternedTyKind::Adt(d)
                        if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) =>
                    {
                        Some(*d)
                    }
                    _ => None,
                };
                // Phase 5e.5 Stage 8a: when element is a
                // FlatGcStruct, the typed-array stores supertype
                // refs — delegate to the dedicated per-Ty
                // materializer function instead of inlining the
                // copy loop. This branch must take precedence
                // over the legacy $fat_value-boxed paths below.
                let elem_is_flat_gc = matches!(
                    self.internal_repr(elem_ty),
                    super::super::repr::InternalRepr::FlatGcStruct(_)
                );
                // strings-to-GC: a `list<string>` element is a `$str_bytes`
                // ref — delegate to the shared materializer (which handles the
                // inner byte-array → (ptr, len) copy) exactly like flat-gc,
                // rather than the legacy inline `$fat_value` unbox below.
                // A collapsed-option element (`list<option<record|tuple|list>>`)
                // likewise needs the dedicated per-element lift.
                let delegate_to_materializer = elem_is_flat_gc
                    || matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String)
                    || self.elem_option_collapses(elem_ty).is_some();
                if delegate_to_materializer {
                    let local_decls: Vec<(u32, ValType)> = vec![
                        (
                            1,
                            ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                            }),
                        ),
                        (1, ValType::I32), // scratch_ptr
                        (1, ValType::I32), // ptr_temp
                        (1, ValType::I32), // len_temp
                    ];
                    let self_ref_local: u32 = 1;
                    let scratch_local: u32 = 2;
                    let ptr_temp: u32 = 3;
                    let len_temp: u32 = 4;
                    let mut func = Function::new(local_decls);
                    self.emit_registry_lookup(&mut func, ci, 0, self_ref_local)?;
                    self.current_self_local = Some(self_ref_local);
                    self.current_self_comp_idx = Some(ci);
                    // Allocate 8-byte canonical scratch (ptr, len).
                    super::scratch::emit_cabi_realloc_fixed(&mut func, 4, 8, cabi_realloc);
                    func.instruction(&Instruction::LocalSet(scratch_local));
                    // Load array ref + call materializer → (ptr, len).
                    self.emit_self_ref(&mut func, ci)?;
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: struct_ty,
                        field_index: field_path[0],
                    });
                    let mat_fn = *self
                        .gc_list_materializer_fn_indices
                        .get(&arr_type_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "list<flat-gc> getter: missing materializer for arr {}",
                                arr_type_idx
                            ))
                        })?;
                    func.instruction(&Instruction::Call(mat_fn));
                    // Stack: (ptr, len). Stash, then write.
                    func.instruction(&Instruction::LocalSet(len_temp));
                    func.instruction(&Instruction::LocalSet(ptr_temp));
                    func.instruction(&Instruction::LocalGet(scratch_local));
                    func.instruction(&Instruction::LocalGet(ptr_temp));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(scratch_local));
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(len_temp));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(scratch_local));
                    self.current_self_local = None;
                    self.current_self_comp_idx = None;
                    func.instruction(&Instruction::End);
                    return Ok(func);
                }
                // Phase 5e.4 / 5e.5: $fat_value-boxed element types
                // (strings, option<scalar-i32-fits>) — share the
                // same per-element copy logic.
                let elem_is_string = matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String)
                    || (!elem_is_flat_gc
                        && matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::Option(_))
                        && {
                            let canonical = self.canonical_flat_valtypes(elem_ty);
                            canonical.len() == 2
                                && canonical.iter().all(|vt| matches!(vt, ValType::I32))
                        });
                // Phase 5e.6: nested-list element — element is itself
                // a typed GC array ref. Recursively call the inner
                // materializer to lower it to (ptr, len) for
                // canonical memory.
                let elem_is_nested_list =
                    matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::List(_))
                        && self
                            .record_gc_types
                            .list_array_type_idx
                            .contains_key(&elem_ty);
                // Locals: 1=self_ref, 2=scratch_ptr, 3=arr_ref, 4=len, 5=data_ptr, 6=idx
                // For record / string elements: 7=elem_addr, 8=elem_ref (typed record ref / fat_value ref)
                let self_ref_local: u32 = 1;
                let scratch_ptr_local: u32 = 2;
                let arr_ref_local: u32 = 3;
                let len_local: u32 = 4;
                let data_ptr_local: u32 = 5;
                let idx_local: u32 = 6;
                let elem_addr_local: u32 = 7;
                let elem_ref_local: u32 = 8;
                let mut local_decls: Vec<(u32, ValType)> = vec![
                    (
                        1,
                        ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                        }),
                    ),
                    (1, ValType::I32), // scratch_ptr
                    (
                        1,
                        ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                        }),
                    ),
                    (1, ValType::I32), // len
                    (1, ValType::I32), // data_ptr
                    (1, ValType::I32), // idx
                ];
                if let Some(record_def_id) = elem_record_def {
                    local_decls.push((1, ValType::I32)); // elem_addr
                    let record_type_idx = self
                        .record_gc_types
                        .record_type_idx
                        .get(&record_def_id)
                        .copied()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "GC list getter: missing record_type_idx".into(),
                            )
                        })?;
                    local_decls.push((
                        1,
                        ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(record_type_idx),
                        }),
                    ));
                    // Phase 5e.6 scratch for typed-array list field
                    // materialization during nested record lift.
                    local_decls.push((1, ValType::I32)); // mat_ptr
                    local_decls.push((1, ValType::I32)); // mat_len
                } else if elem_is_string {
                    local_decls.push((1, ValType::I32)); // elem_addr
                } else if elem_is_nested_list {
                    local_decls.push((1, ValType::I32)); // elem_addr
                    local_decls.push((1, ValType::I32)); // inner_ptr
                    local_decls.push((1, ValType::I32)); // inner_len
                }
                let mut func = Function::new(local_decls);
                self.emit_registry_lookup(&mut func, ci, 0, self_ref_local)?;
                self.current_self_local = Some(self_ref_local);
                self.current_self_comp_idx = Some(ci);
                // Load GC array ref from struct
                self.emit_self_ref(&mut func, ci)?;
                func.instruction(&Instruction::StructGet {
                    struct_type_index: struct_ty,
                    field_index: field_path[0],
                });
                func.instruction(&Instruction::LocalSet(arr_ref_local));
                // len = array.len(arr)
                func.instruction(&Instruction::LocalGet(arr_ref_local));
                func.instruction(&Instruction::ArrayLen);
                func.instruction(&Instruction::LocalSet(len_local));
                // data_ptr = cabi_realloc(0, 0, elem_align, len * elem_size)
                super::scratch::emit_cabi_realloc_array(&mut func, len_local, elem_size, elem_align, cabi_realloc);
                func.instruction(&Instruction::LocalSet(data_ptr_local));
                // Copy loop: for idx in 0..len { data[idx*sz] = arr.get(idx) }
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::LocalSet(idx_local));
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::LocalGet(len_local));
                func.instruction(&Instruction::I32GeU);
                func.instruction(&Instruction::BrIf(1));
                if let Some(record_def_id) = elem_record_def {
                    // elem_addr = data_ptr + idx * elem_size
                    func.instruction(&Instruction::LocalGet(data_ptr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(elem_size as i32));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalSet(elem_addr_local));
                    // elem_ref = arr[idx]
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&Instruction::LocalSet(elem_ref_local));
                    // Lift fields → memory at elem_addr.
                    // mat_ptr/mat_len locals are appended after
                    // elem_ref_local in local_decls above.
                    let mat_ptr_local = elem_ref_local + 1;
                    let mat_len_local = elem_ref_local + 2;
                    self.emit_record_lift_to_memory(
                        &mut func,
                        record_def_id,
                        elem_ref_local,
                        elem_addr_local,
                        0,
                        Some((mat_ptr_local, mat_len_local)),
                    )?;
                } else if elem_is_string {
                    // A `list<string>` element is a `$str_bytes` GC ref, not
                    // a `$fat_value` box; its boundary materialization is
                    // handled by the str_bytes-aware path, never here.
                    unreachable!(
                        "list<string> getter: string element boxed into $fat_value — \
                         strings are $str_bytes GC refs, materialized elsewhere"
                    );
                } else if elem_is_nested_list {
                    let inner_arr_idx = self.record_gc_types.list_array_type_idx[&elem_ty];
                    let inner_mat_fn = *self
                            .gc_list_materializer_fn_indices
                            .get(&inner_arr_idx)
                            .ok_or_else(|| CodegenError::InvalidIR(format!(
                                "GC list getter (nested): missing inner materializer for arr_type_idx={}",
                                inner_arr_idx
                            )))?;
                    // Locals appended after elem_addr_local (=7):
                    //   8 = inner_ptr, 9 = inner_len
                    let inner_ptr_local = elem_addr_local + 1;
                    let inner_len_local = elem_addr_local + 2;
                    // elem_addr = data_ptr + idx * 8
                    func.instruction(&Instruction::LocalGet(data_ptr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(8));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalSet(elem_addr_local));
                    // (inner_ptr, inner_len) = $inner_mat(arr.get(idx))
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&Instruction::Call(inner_mat_fn));
                    func.instruction(&Instruction::LocalSet(inner_len_local));
                    func.instruction(&Instruction::LocalSet(inner_ptr_local));
                    // store inner_ptr at elem_addr+0
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::LocalGet(inner_ptr_local));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    // store inner_len at elem_addr+4
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(inner_len_local));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                } else {
                    // destination address
                    func.instruction(&Instruction::LocalGet(data_ptr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(elem_size as i32));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    // array.get element
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    emit_gc_array_get(&mut func, self.ctx, elem_ty, arr_type_idx);
                    // store to memory
                    emit_gc_list_elem_store(&mut func, self.ctx, elem_ty);
                }
                // idx++
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(idx_local));
                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End); // loop
                func.instruction(&Instruction::End); // block
                // scratch = cabi_realloc(0, 0, 4, 8) — allocate (ptr, len) pair
                super::scratch::emit_cabi_realloc_fixed(&mut func, 4, 8, cabi_realloc);
                func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                // scratch[0] = data_ptr
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                func.instruction(&Instruction::LocalGet(data_ptr_local));
                func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                // scratch[4] = len
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                func.instruction(&Instruction::LocalGet(len_local));
                func.instruction(&Instruction::I32Store(super::scratch::mem_arg(4, 2)));
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                func.instruction(&Instruction::End);
                self.current_self_local = None;
                self.current_self_comp_idx = None;
                return Ok(func);
            }

            let self_ref_local: u32 = 1;
            // Scratch pointer local for the multi-slot lift path. The
            // canonical-ABI return shape for composite types is a
            // pointer to a freshly-allocated buffer (sized to the type's
            // flat layout); we obtain it via cabi_realloc per call so
            // concurrent callers on different instances don't race on
            // shared memory.
            let scratch_ptr_local: u32 = 2;
            // Two extra i32 scratch locals (ptr/len holding) used by
            // the GC-array-ref → canonical (ptr, len) materializer
            // path. Indices 3 and 4 in the function's local space.
            let mat_ptr_local: u32 = 3;
            let mat_len_local: u32 = 4;
            // Anyref scratch for the option-of-collapsed-ref lift
            // path: holds the (possibly null) inner record / list /
            // tuple ref while we test it and lift its payload.
            let inner_ref_local: u32 = 5;
            let mut func = Function::new([
                (
                    1,
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                    }),
                ),
                (3, ValType::I32),
                (
                    1,
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Abstract {
                            shared: false,
                            ty: wasm_encoder::AbstractHeapType::Any,
                        },
                    }),
                ),
            ]);
            let _ = mat_ptr_local;
            let _ = mat_len_local;
            // Look up arr[rep] → ref, store in self-ref local. Every
            // helper sources self via current_self_local, which we
            // set just below.
            self.emit_registry_lookup(&mut func, ci, 0, self_ref_local)?;

            self.current_self_local = Some(self_ref_local);
            self.current_self_comp_idx = Some(ci);
            let flat_valtypes = self.canonical_flat_valtypes(signal_ty);
            // Phase 3: SLR (POR + records with string / list<scalar>
            // fields) all route through the GC-backed getter path.
            let is_por = self.is_single_level_record(signal_ty);
            let result = (|| -> Result<(), CodegenError> {
                // Phase 2: POR record with exactly one flat slot —
                // return that slot's value directly (canonical-ABI
                // says single-slot composites return the value, not
                // a pointer). Read struct.get(comp).get(record).
                if is_por && flat_valtypes.len() == 1 {
                    let record_def_id = match self.ctx.ty_kind(signal_ty) {
                        yel_core::types::InternedTyKind::Adt(d) => *d,
                        _ => {
                            return Err(CodegenError::InvalidIR(
                                "POR getter: signal_ty is not an Adt".into(),
                            ));
                        }
                    };
                    // The single canonical slot is a leaf scalar reached
                    // through a chain of single-slot records (e.g. `record O {
                    // i: I }`, `record I { a: s64 }` → the slot is `o.i.a`).
                    // Read down that chain to the scalar and return it by
                    // value; stopping at the first record ref would return a
                    // `(ref …)` where the getter's flat return type (e.g. i64)
                    // is expected. Same leaf-access primitive the multi-slot
                    // lift uses — here there is exactly one (scalar) leaf.
                    let leaves = self.record_leaf_field_accesses(record_def_id)?;
                    let leaf = match leaves.as_slice() {
                        [l] => l,
                        _ => {
                            return Err(CodegenError::InvalidIR(format!(
                                "single-slot record getter: expected exactly one leaf, \
                                 got {} for {:?}",
                                leaves.len(),
                                record_def_id
                            )));
                        }
                    };
                    let full_chain: Vec<(u32, u32)> = std::iter::once((struct_ty, field_path[0]))
                        .chain(leaf.chain.iter().copied())
                        .collect();
                    self.emit_gc_field_chain(&mut func, ci, &full_chain)?;
                    return Ok(());
                }
                // Option-of-ref collapsed signal: storage is one
                // nullable ref; canonical ABI shape includes a
                // synthesised discriminant + the inner type's
                // canonical slots. Read the ref, null-check, fill
                // a cabi_realloc'd scratch buffer accordingly, and
                // return the scratch pointer. Only handles the
                // option<list<scalar>> sub-case for Phase 5b-v.3
                // (inner is a GC array ref + materializer
                // available); other inner reprs (records, nested
                // option/result) are routed to the legacy path
                // until Phase 5d.
                if let Some(arr_type_idx) = self.option_collapses_to_ref(signal_ty) {
                    let inner_ty = match self.ctx.ty_kind(signal_ty) {
                        InternedTyKind::Option(i) => *i,
                        _ => unreachable!("option_collapses_to_ref non-option"),
                    };
                    if matches!(self.ctx.ty_kind(inner_ty), InternedTyKind::List(_))
                        && let Some(&mat_fn) =
                            self.gc_list_materializer_fn_indices.get(&arr_type_idx)
                    {
                        let layout_info = self.layout_ctx.layout_of(signal_ty);
                        let cabi_realloc = self
                            .alloc_funcs
                            .as_ref()
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(
                                    "option-collapse getter: cabi_realloc missing".into(),
                                )
                            })?
                            .cabi_realloc;
                        let slots = self.flatten_core_slots(signal_ty);
                        // slots = [disc(I32_8 at 0), ptr(I32 at 4), len(I32 at 8)]
                        if slots.len() != 3 {
                            return Err(CodegenError::InvalidIR(format!(
                                "option-collapse getter: expected 3 canonical slots, got {}",
                                slots.len()
                            )));
                        }
                        // Allocate scratch.
                        super::scratch::emit_cabi_realloc_fixed(&mut func, layout_info.align, layout_info.size, cabi_realloc);
                        func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                        // Read the ref, null-check.
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[0],
                        });
                        func.instruction(&Instruction::RefIsNull);
                        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                        // null path: disc=0, ptr=0, len=0.
                        for slot in slots.iter() {
                            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                            if slot.offset != 0 {
                                func.instruction(&Instruction::I32Const(slot.offset as i32));
                                func.instruction(&Instruction::I32Add);
                            }
                            func.instruction(&Instruction::I32Const(0));
                            slot.store.emit_store(&mut func);
                        }
                        func.instruction(&Instruction::Else);
                        // non-null path: disc=1, then materializer for ptr/len.
                        // disc:
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        if slots[0].offset != 0 {
                            func.instruction(&Instruction::I32Const(slots[0].offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::I32Const(1));
                        slots[0].store.emit_store(&mut func);
                        // call materializer with the ref.
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[0],
                        });
                        func.instruction(&Instruction::Call(mat_fn));
                        // stack: ptr, len → save to mat_ptr/mat_len.
                        func.instruction(&Instruction::LocalSet(mat_len_local));
                        func.instruction(&Instruction::LocalSet(mat_ptr_local));
                        // store ptr at slots[1].
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        if slots[1].offset != 0 {
                            func.instruction(&Instruction::I32Const(slots[1].offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::LocalGet(mat_ptr_local));
                        slots[1].store.emit_store(&mut func);
                        // store len at slots[2].
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        if slots[2].offset != 0 {
                            func.instruction(&Instruction::I32Const(slots[2].offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::LocalGet(mat_len_local));
                        slots[2].store.emit_store(&mut func);
                        func.instruction(&Instruction::End); // if/else
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        return Ok(());
                    }
                }
                // Phase 5e.5: FlatGcStruct signal — lift the GC ref
                // to the canonical-ABI (disc, payload) memory layout
                // by testing each case subtype and writing the
                // matching disc + payload bytes into a scratch
                // buffer. The boundary helper expects 1 ref slot
                // in storage, but the canonical shape is multi-slot,
                // so the generic loop below would mis-walk. This MUST
                // precede the `flat_valtypes.len() == 1` direct-return
                // branch below: an all-empty (payload-less) variant —
                // or a `result<(),()>` — has a single canonical i32
                // discriminant slot yet its storage is a GC ref, so
                // the direct `struct.get` would return a ref where the
                // getter's i32 result is expected.
                if let super::super::repr::InternalRepr::FlatGcStruct(super_idx) =
                    self.internal_repr(signal_ty)
                {
                    return self.emit_flat_gc_signal_lift(
                        &mut func,
                        ci,
                        sig_idx,
                        signal_ty,
                        super_idx,
                        scratch_ptr_local,
                    );
                }
                if flat_valtypes.len() == 1 {
                    // Single-slot: read field, return it directly.
                    self.emit_self_ref(&mut func, ci)?;
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: struct_ty,
                        field_index: field_path[0],
                    });
                    return Ok(());
                }
                // Phase 2: primitive-only record getter. The struct
                // field path holds ONE `(ref null $<rec>_record)`,
                // not a sequence of canonical-ABI flat slots. To
                // satisfy the host's canonical-ABI return shape we
                // still need to write flat bytes into a cabi_realloc
                // lift scratch — but each slot is sourced via
                // `struct.get $<rec>_record $field` instead of from
                // a memory load.
                if is_por {
                    let record_type_idx = self.por_record_type_idx(signal_ty).ok_or_else(|| {
                        CodegenError::InvalidIR("SLR getter: record type idx missing".into())
                    })?;
                    let record_def_id = match self.ctx.ty_kind(signal_ty) {
                        yel_core::types::InternedTyKind::Adt(d) => *d,
                        _ => {
                            return Err(CodegenError::InvalidIR(
                                "SLR getter: signal_ty is not an Adt".into(),
                            ));
                        }
                    };
                    let layout_info = self.layout_ctx.layout_of(signal_ty);
                    let cabi_realloc = self
                        .alloc_funcs
                        .as_ref()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "SLR getter: cabi_realloc not initialised".into(),
                            )
                        })?
                        .cabi_realloc;
                    // Allocate lift scratch.
                    super::scratch::emit_cabi_realloc_fixed(&mut func, layout_info.align, layout_info.size, cabi_realloc);
                    func.instruction(&Instruction::LocalSet(scratch_ptr_local));

                    // Phase 4: recurse into the record (and any
                    // nested DTR records) to emit one store per
                    // canonical-ABI flat slot. Each store sources
                    // its value via the chain of GC struct.gets that
                    // reach the corresponding inner field.
                    let _ = record_type_idx;
                    let prefix: Vec<(u32, u32)> = vec![(struct_ty, field_path[0])];
                    self.emit_getter_lift_dtr_record(
                        &mut func,
                        ci,
                        record_def_id,
                        0,
                        scratch_ptr_local,
                        &prefix,
                    )?;
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    return Ok(());
                }
                // Phase 5e.3: tuple-as-signal getter — storage is
                // one tuple struct ref; canonical ABI is the
                // flattening of tuple elements. Allocate scratch,
                // load each tuple field via struct.get, store at
                // the canonical offset.
                if matches!(self.ctx.ty_kind(signal_ty), InternedTyKind::Tuple(_)) {
                    let layout_info = self.layout_ctx.layout_of(signal_ty);
                    let cabi_realloc = self
                        .alloc_funcs
                        .as_ref()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR("tuple getter: cabi_realloc missing".into())
                        })?
                        .cabi_realloc;
                    // Allocate the canonical-ABI lift scratch, then lower the
                    // tuple GC struct into it (recursively — see
                    // `emit_getter_lift_tuple`) and return the scratch pointer.
                    super::scratch::emit_cabi_realloc_fixed(&mut func, layout_info.align, layout_info.size, cabi_realloc);
                    func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                    let prefix: Vec<(u32, u32)> = vec![(struct_ty, field_path[0])];
                    self.emit_getter_lift_tuple(
                        &mut func,
                        ci,
                        signal_ty,
                        0,
                        scratch_ptr_local,
                        mat_ptr_local,
                        mat_len_local,
                        &prefix,
                    )?;
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    return Ok(());
                }

                // option<T> where T's internal repr is a single GC
                // ref (record, list-array, tuple) collapses to a
                // nullable ref slot internally — null = none, non-
                // null = some(value). Storage is 1 ref, but the
                // canonical shape is multi-slot
                // [disc(i32), ...inner_canonical]. Dispatch to a
                // dedicated lift that null-checks the ref, writes
                // disc + payload accordingly.
                if matches!(self.ctx.ty_kind(signal_ty), InternedTyKind::Option(_))
                    && self.option_collapses_to_ref(signal_ty).is_some()
                {
                    return self.emit_option_collapsed_ref_signal_lift(
                        &mut func,
                        ci,
                        sig_idx,
                        signal_ty,
                        scratch_ptr_local,
                        inner_ref_local,
                        mat_ptr_local,
                        mat_len_local,
                    );
                }

                // Multi-slot: allocate a per-call lift scratch via
                // cabi_realloc(0, 0, align, size) sized to the signal
                // type's canonical-ABI flat layout, write each field
                // into it at its byte offset, and return the scratch
                // pointer. The host's lifting machinery takes ownership
                // per the canonical ABI, so no leak.
                let slots = self.flatten_core_slots(signal_ty);
                let storage = self.signal_storage_valtypes(signal_ty);
                if storage.len() != field_path.len() {
                    return Err(CodegenError::InvalidIR(format!(
                        "getter: signal_storage_valtypes ({}) disagrees with struct field path ({}) for signal {}",
                        storage.len(),
                        field_path.len(),
                        sig_idx
                    )));
                }
                let layout_info = self.layout_ctx.layout_of(signal_ty);
                let cabi_realloc = self
                    .alloc_funcs
                    .as_ref()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "multi-slot getter requires alloc_funcs (cabi_realloc) \
                                 to be initialized before generation"
                                .to_string(),
                        )
                    })?
                    .cabi_realloc;
                // cabi_realloc(0, 0, align, size) -> ptr
                super::scratch::emit_cabi_realloc_fixed(&mut func, layout_info.align, layout_info.size, cabi_realloc);
                func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                // Walk struct fields in declaration order. Each non-ref
                // field maps 1:1 to one canonical slot. Each GC array
                // ref field expands to two consecutive canonical slots
                // (ptr, len) via the materializer; if the ref is null
                // (e.g. `none` discriminant case), write 0/0 — the
                // consumer's discriminant check guards reading them.
                let mut canonical_idx = 0usize;
                for (field_i, vt) in storage.iter().enumerate() {
                    if let ValType::Ref(ref_ty) = vt {
                        // Expect GcArrayRef on a list<scalar> field.
                        let arr_type_idx = match ref_ty.heap_type {
                            wasm_encoder::HeapType::Concrete(idx) => idx,
                            _ => {
                                return Err(CodegenError::InvalidIR(format!(
                                    "getter: unexpected non-concrete ref heap type \
                                         in signal_storage_valtypes for signal {}",
                                    sig_idx
                                )));
                            }
                        };
                        let mat_fn = *self
                            .gc_list_materializer_fn_indices
                            .get(&arr_type_idx)
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "getter: no materializer for GC list arr_type_idx={}",
                                    arr_type_idx
                                ))
                            })?;
                        let ptr_slot = &slots[canonical_idx];
                        let len_slot = &slots[canonical_idx + 1];
                        // Null-check the array ref. If null (none case),
                        // write (0, 0); else call materializer, get
                        // (ptr, len), write to ptr_slot/len_slot offsets.
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[field_i],
                        });
                        func.instruction(&Instruction::RefIsNull);
                        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                        // null path: write zeros to both slots.
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::LocalSet(mat_ptr_local));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::LocalSet(mat_len_local));
                        func.instruction(&Instruction::Else);
                        // non-null path: call materializer.
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[field_i],
                        });
                        func.instruction(&Instruction::Call(mat_fn));
                        // stack: (ptr, len)
                        func.instruction(&Instruction::LocalSet(mat_len_local));
                        func.instruction(&Instruction::LocalSet(mat_ptr_local));
                        func.instruction(&Instruction::End); // if/else
                        // Now write mat_ptr_local to ptr_slot, mat_len_local to len_slot.
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        if ptr_slot.offset != 0 {
                            func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::LocalGet(mat_ptr_local));
                        ptr_slot.store.emit_store(&mut func);
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        if len_slot.offset != 0 {
                            func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::LocalGet(mat_len_local));
                        len_slot.store.emit_store(&mut func);
                        canonical_idx += 2;
                    } else {
                        let slot = &slots[canonical_idx];
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        if slot.offset != 0 {
                            func.instruction(&Instruction::I32Const(slot.offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[field_i],
                        });
                        slot.store.emit_store(&mut func);
                        canonical_idx += 1;
                    }
                }
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                Ok(())
            })();
            self.current_self_local = None;
            self.current_self_comp_idx = None;
            result?;
            func.instruction(&Instruction::End);
            return Ok(func);
        }
        // Scalar / memory-resident fallthrough branch — first branch
        // converted to the Phase 0.3k-pre two-phase pattern. The
        // budget is empty (no scratch locals); the caller builds
        // `Function::new(&[])` and the body emitter writes against
        // it. Wasm output is byte-identical to the prior inline form.
        unreachable!(
            "getter: non-struct signal fallthrough is unreachable — \
             every non-unit signal is GC-struct-resident"
        )
    }

    pub(super) fn generate_setter_for(
        &mut self,
        comp_idx: usize,
        sig_idx: usize,
        _import_realloc: u32,
    ) -> Result<Function, CodegenError> {
        let component = &self.components[comp_idx];
        let signal = &component.signals[sig_idx];
        let signal_def_id = signal.def_id;

        let ty = signal.ty;

        // GC-struct-migrated signals: write each canonical-ABI flat
        // param directly into its backing struct field. The setter
        // signature is `(self: i32, flat_0, flat_1, ...)` and the
        // struct schema mirrors `flatten_core_valtypes` exactly for
        // every migrated signal type, so a 1-to-1 param→field copy
        // produces a struct-resident value in the canonical shape
        // any internal reader expects. Pointer-typed signals (records,
        // tuples) keep the existing memory path below.
        if self.signal_in_struct(comp_idx, sig_idx) {
            let gc = &self.gc_layouts[comp_idx];
            let struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "setter (struct): component {} missing component_struct_type_idx",
                    comp_idx
                ))
            })?;
            let field_path: Vec<u32> = component.signal_layout.signal_field_path(sig_idx);

            // Phase 5b-v.3: GC list setter — copy canonical ABI (ptr, len) → GC array.
            // Only handles direct list signals; option-collapsed
            // option<list<scalar>> signals are handled in a dedicated
            // branch further below.
            if matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_))
                && let super::super::repr::InternalRepr::GcArrayRef(arr_type_idx) =
                    self.internal_repr(ty)
            {
                let elem_ty = match self.ctx.ty_kind(ty) {
                    InternedTyKind::List(e) => *e,
                    _ => {
                        return Err(CodegenError::InvalidIR(
                            "GC list setter: signal_ty is not a list".into(),
                        ));
                    }
                };
                let (elem_size, _elem_align) =
                    gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
                let elem_record_def: Option<DefId> = match self.ctx.ty_kind(elem_ty) {
                    InternedTyKind::Adt(d)
                        if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) =>
                    {
                        Some(*d)
                    }
                    _ => None,
                };
                // Phase 5e.5 Stage 8a: when element is FlatGcStruct,
                // delegate per-element packing to the dedicated
                // un-materializer function instead of inlining the
                // legacy $fat_value-boxed path.
                let elem_is_flat_gc = matches!(
                    self.internal_repr(elem_ty),
                    super::super::repr::InternalRepr::FlatGcStruct(_)
                );
                // strings-to-GC: a `list<string>` setter delegates to the
                // shared un-materializer (canonical (ptr,len) → array of
                // $str_bytes refs) just like flat-gc, not the inline copy.
                let delegate_to_unmaterializer = elem_is_flat_gc
                    || matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String)
                    || self.elem_option_collapses(elem_ty).is_some();
                if delegate_to_unmaterializer {
                    let unmat_fn = *self
                        .gc_list_unmaterializer_fn_indices
                        .get(&arr_type_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "list<flat-gc> setter: missing un-materializer for arr {}",
                                arr_type_idx
                            ))
                        })?;
                    // Locals: 3=self_ref. Build via call.
                    let self_ref_local: u32 = 3;
                    let local_decls: Vec<(u32, ValType)> = vec![(
                        1,
                        ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                        }),
                    )];
                    let mut func = Function::new(local_decls);
                    self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;
                    self.current_self_local = Some(self_ref_local);
                    self.current_self_comp_idx = Some(comp_idx);
                    self.emit_self_ref(&mut func, comp_idx)?;
                    func.instruction(&Instruction::LocalGet(1)); // ptr
                    func.instruction(&Instruction::LocalGet(2)); // len
                    func.instruction(&Instruction::Call(unmat_fn));
                    func.instruction(&Instruction::StructSet {
                        struct_type_index: struct_ty,
                        field_index: field_path[0],
                    });
                    self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                    self.current_self_local = None;
                    self.current_self_comp_idx = None;
                    func.instruction(&Instruction::End);
                    return Ok(func);
                }
                let elem_is_string = matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String)
                    || (!elem_is_flat_gc
                        && matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::Option(_))
                        && {
                            let canonical = self.canonical_flat_valtypes(elem_ty);
                            canonical.len() == 2
                                && canonical.iter().all(|vt| matches!(vt, ValType::I32))
                        });
                // Setter params: 0=rep(i32), 1=ptr(i32), 2=len(i32)
                // Locals: 3=self_ref, 4=arr_ref, 5=idx, [6=elem_addr if record/string]
                let self_ref_local: u32 = 3;
                let arr_ref_local: u32 = 4;
                let idx_local: u32 = 5;
                let elem_addr_local: u32 = 6;
                let mut local_decls: Vec<(u32, ValType)> = vec![
                    (
                        1,
                        ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                        }),
                    ),
                    (
                        1,
                        ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                        }),
                    ),
                    (1, ValType::I32), // idx
                ];
                let elem_is_nested_list_setter =
                    matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::List(_))
                        && self
                            .record_gc_types
                            .list_array_type_idx
                            .contains_key(&elem_ty);
                if elem_record_def.is_some() || elem_is_string || elem_is_nested_list_setter {
                    local_decls.push((1, ValType::I32)); // elem_addr
                }
                let mut func = Function::new(local_decls);
                self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;
                self.current_self_local = Some(self_ref_local);
                self.current_self_comp_idx = Some(comp_idx);
                // arr = array.new_default(len)
                func.instruction(&Instruction::LocalGet(2)); // len param
                func.instruction(&Instruction::ArrayNewDefault(arr_type_idx));
                func.instruction(&Instruction::LocalSet(arr_ref_local));
                // Copy loop: for idx in 0..len { arr.set(idx, load(ptr + idx * elem_size)) }
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::LocalSet(idx_local));
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::LocalGet(2)); // len
                func.instruction(&Instruction::I32GeU);
                func.instruction(&Instruction::BrIf(1));
                if let Some(record_def_id) = elem_record_def {
                    // elem_addr = ptr + idx * elem_size
                    func.instruction(&Instruction::LocalGet(1));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(elem_size as i32));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalSet(elem_addr_local));
                    // Build record GC ref from canonical bytes; result on stack.
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    self.emit_record_pack_from_memory(
                        &mut func,
                        record_def_id,
                        elem_addr_local,
                        0,
                    )?;
                    // arr.set(idx, record_ref)
                    func.instruction(&Instruction::ArraySet(arr_type_idx));
                } else if elem_is_string {
                    // A `list<string>` element is a `$str_bytes` GC ref, not
                    // a `$fat_value` box; its setter path is handled by the
                    // str_bytes-aware un-materializer, never here.
                    unreachable!(
                        "list<string> setter: string element boxed into $fat_value — \
                         strings are $str_bytes GC refs, un-materialized elsewhere"
                    );
                } else if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::List(_))
                    && self
                        .record_gc_types
                        .list_array_type_idx
                        .contains_key(&elem_ty)
                {
                    // Phase 5e.6: nested-list element — call the inner
                    // un-materializer to lift canonical (ptr, len) into
                    // a typed GC array, then array.set.
                    let inner_arr_idx = self.record_gc_types.list_array_type_idx[&elem_ty];
                    let inner_unmat_fn = *self
                        .gc_list_unmaterializer_fn_indices
                        .get(&inner_arr_idx)
                        .ok_or_else(|| CodegenError::InvalidIR(format!(
                            "GC list setter (nested): missing inner un-materializer for arr_type_idx={}",
                            inner_arr_idx
                        )))?;
                    // arr_ref, idx for array.set
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    // elem_addr = ptr + idx * 8
                    func.instruction(&Instruction::LocalGet(1));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(8));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalSet(elem_addr_local));
                    // call $inner_unmat(load(elem_addr), load(elem_addr+4))
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::Call(inner_unmat_fn));
                    func.instruction(&Instruction::ArraySet(arr_type_idx));
                } else {
                    // Scalar element: load primitive and array.set.
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    // address: ptr + idx * elem_size
                    func.instruction(&Instruction::LocalGet(1));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(elem_size as i32));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    emit_gc_list_elem_load(&mut func, self.ctx, elem_ty);
                    emit_gc_array_set(&mut func, self.ctx, elem_ty, arr_type_idx);
                }
                // idx++
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(idx_local));
                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End); // loop
                func.instruction(&Instruction::End); // block
                // struct.set the GC array ref
                self.emit_self_ref(&mut func, comp_idx)?;
                func.instruction(&Instruction::LocalGet(arr_ref_local));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: field_path[0],
                });
                self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                self.current_self_local = None;
                self.current_self_comp_idx = Some(comp_idx);
                func.instruction(&Instruction::End);
                return Ok(func);
            }

            // Option-of-list-scalar collapsed setter. Params are
            // canonical (rep:i32, disc:i32, ptr:i32, len:i32). If
            // disc==0, store ref.null; else build a typed GC array
            // from (ptr, len) like the list setter and store the ref.
            if let Some(arr_type_idx) = self.option_collapses_to_ref(ty) {
                let inner_ty = match self.ctx.ty_kind(ty) {
                    InternedTyKind::Option(i) => *i,
                    _ => unreachable!(),
                };
                if let InternedTyKind::List(elem_ty) = self.ctx.ty_kind(inner_ty) {
                    let elem_ty = *elem_ty;
                    // When the inner list's element is stored as a GC ref
                    // (string → `$str_bytes`, flat-gc record, option-collapse,
                    // nested list) the inline copy loop below is wrong — it
                    // would `array.set` a raw i32 into a ref-typed element
                    // ("expected ref, found i32"). Route the whole inner list
                    // through the shared per-list un-materializer instead,
                    // exactly as the plain `list<T>` setter does; only the
                    // discriminant handling is layered on top. Genuine scalar
                    // elements keep the byte-identical inline loop.
                    let elem_needs_unmaterializer = matches!(
                        self.internal_repr(elem_ty),
                        super::super::repr::InternalRepr::FlatGcStruct(_)
                    ) || matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String)
                        || self.elem_option_collapses(elem_ty).is_some()
                        || (matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::List(_))
                            && self
                                .record_gc_types
                                .list_array_type_idx
                                .contains_key(&elem_ty));
                    if elem_needs_unmaterializer {
                        let unmat_fn = *self
                            .gc_list_unmaterializer_fn_indices
                            .get(&arr_type_idx)
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "option<list> setter: missing un-materializer for arr {}",
                                    arr_type_idx
                                ))
                            })?;
                        // Setter params: 0=rep, 1=disc, 2=ptr, 3=len.
                        let self_ref_local: u32 = 4;
                        let mut func = Function::new([(
                            1,
                            ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                            }),
                        )]);
                        self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;
                        self.current_self_local = Some(self_ref_local);
                        self.current_self_comp_idx = Some(comp_idx);
                        self.emit_self_ref(&mut func, comp_idx)?;
                        // disc==0 → none (typed null ref); else build the
                        // array from canonical (ptr, len) via the
                        // un-materializer (boxes each element correctly).
                        func.instruction(&Instruction::LocalGet(1)); // disc
                        func.instruction(&Instruction::I32Eqz);
                        func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                            ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                            }),
                        )));
                        func.instruction(&Instruction::RefNull(
                            wasm_encoder::HeapType::Concrete(arr_type_idx),
                        ));
                        func.instruction(&Instruction::Else);
                        func.instruction(&Instruction::LocalGet(2)); // ptr
                        func.instruction(&Instruction::LocalGet(3)); // len
                        func.instruction(&Instruction::Call(unmat_fn));
                        func.instruction(&Instruction::End);
                        func.instruction(&Instruction::StructSet {
                            struct_type_index: struct_ty,
                            field_index: field_path[0],
                        });
                        self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                        self.current_self_local = None;
                        self.current_self_comp_idx = Some(comp_idx);
                        func.instruction(&Instruction::End);
                        return Ok(func);
                    }
                    let (elem_size, _elem_align) =
                        gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
                    // Setter params: 0=rep, 1=disc, 2=ptr, 3=len.
                    // Locals: 4=self_ref, 5=arr_ref, 6=idx.
                    let self_ref_local: u32 = 4;
                    let arr_ref_local: u32 = 5;
                    let idx_local: u32 = 6;
                    let mut func = Function::new([
                        (
                            1,
                            ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                            }),
                        ),
                        (
                            1,
                            ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                            }),
                        ),
                        (1, ValType::I32), // idx
                    ]);
                    self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;
                    self.current_self_local = Some(self_ref_local);
                    self.current_self_comp_idx = Some(comp_idx);
                    // if disc == 0: arr_ref = null; else build array.
                    func.instruction(&Instruction::LocalGet(1)); // disc
                    func.instruction(&Instruction::I32Eqz);
                    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                    // none → null ref
                    func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                        arr_type_idx,
                    )));
                    func.instruction(&Instruction::LocalSet(arr_ref_local));
                    func.instruction(&Instruction::Else);
                    // some → build array from (ptr, len).
                    func.instruction(&Instruction::LocalGet(3)); // len
                    func.instruction(&Instruction::ArrayNewDefault(arr_type_idx));
                    func.instruction(&Instruction::LocalSet(arr_ref_local));
                    // copy loop
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::LocalSet(idx_local));
                    func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                    func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::LocalGet(3)); // len
                    func.instruction(&Instruction::I32GeU);
                    func.instruction(&Instruction::BrIf(1));
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::LocalGet(2)); // ptr
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(elem_size as i32));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    emit_gc_list_elem_load(&mut func, self.ctx, elem_ty);
                    emit_gc_array_set(&mut func, self.ctx, elem_ty, arr_type_idx);
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(1));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalSet(idx_local));
                    func.instruction(&Instruction::Br(0));
                    func.instruction(&Instruction::End); // loop
                    func.instruction(&Instruction::End); // block
                    func.instruction(&Instruction::End); // if/else
                    // struct.set the GC array ref
                    self.emit_self_ref(&mut func, comp_idx)?;
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::StructSet {
                        struct_type_index: struct_ty,
                        field_index: field_path[0],
                    });
                    self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                    self.current_self_local = None;
                    self.current_self_comp_idx = Some(comp_idx);
                    func.instruction(&Instruction::End);
                    return Ok(func);
                }
            }

            // Phase 2: primitive-only record signal — params are
            // canonical-ABI flat (one per record field), but the
            // struct field is ONE ref slot. Pack the flat params into
            // a `struct.new $<rec>_record`, then `struct.set` on the
            // component field.
            // Phase 3: SLR (POR + string / list<scalar> fields) routes
            // through the GC-backed setter path.
            let is_por = self.is_single_level_record(ty);
            // The setter's actual WASM param count = 1 (self) + flat
            // valtypes count. Reserve the self-ref local right after
            // all params so it's always at the correct index whether
            // we take the POR or the param-mirrored field path.
            let actual_flat_count = self.canonical_flat_valtypes(ty).len() as u32;
            let self_ref_local: u32 = 1 + actual_flat_count;
            let mut func = Function::new([(
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                }),
            )]);
            // Look up the rep (param 0) → ref, into self_ref_local. Every
            // helper sources self via current_self_local, set just below.
            self.emit_registry_lookup(&mut func, comp_idx, 0, self_ref_local)?;

            self.current_self_local = Some(self_ref_local);
            self.current_self_comp_idx = Some(comp_idx);

            // strings-to-GC (`plans/strings-to-gc.md`): a plain string
            // signal. Canonical params are (ptr, len) from the host; the
            // struct field is a single `(ref $str_bytes)`. Un-materialize
            // the (ptr, len) into a GC byte array and `struct.set`. This
            // is the WIT-boundary "string in" site.
            if matches!(self.ctx.ty_kind(ty), InternedTyKind::String)
            {
                let arr_type_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "string setter: $str_bytes array type not registered".into(),
                    )
                })?;
                let unmat_fn = *self
                    .gc_list_unmaterializer_fn_indices
                    .get(&arr_type_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "string setter: missing $str_bytes un-materializer".into(),
                        )
                    })?;
                self.emit_self_ref(&mut func, comp_idx)?;
                func.instruction(&Instruction::LocalGet(1)); // ptr
                func.instruction(&Instruction::LocalGet(2)); // len
                func.instruction(&Instruction::Call(unmat_fn));
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: field_path[0],
                });
                self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                self.current_self_local = None;
                self.current_self_comp_idx = None;
                func.instruction(&Instruction::End);
                return Ok(func);
            }

            // Phase 7: option<T> where T's internal repr is a single GC
            // ref (record / list / tuple) collapses to a nullable ref
            // slot internally. Setter params are canonical:
            // (self, disc, ...inner_canonical). Dispatch on disc:
            // disc=0 → Some, build inner from following params and
            // struct.set; disc=1 → None, struct.set ref.null. YEL
            // convention here matches FlatGcStruct case_idx (0=Some,
            // 1=None) — see `emit_flat_gc_signal_lift`.
            if matches!(self.ctx.ty_kind(ty), InternedTyKind::Option(_))
                && self.option_collapses_to_ref(ty).is_some()
            {
                let inner_ty = match self.ctx.ty_kind(ty) {
                    InternedTyKind::Option(t) => *t,
                    _ => unreachable!(),
                };
                let arr_idx = self.option_collapses_to_ref(ty).unwrap();
                self.emit_self_ref(&mut func, comp_idx)?;
                // disc is param 1 — canonical-ABI option: 1 = some, 0 = none
                // (standard WIT convention). Take the Some branch on disc != 0;
                // the else branch stores a typed null ref for none. (No
                // i32.eqz: that inverts to some=0 and mis-stores every host
                // Some as none.)
                func.instruction(&Instruction::LocalGet(1));
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(arr_idx),
                    }),
                )));
                // disc==1 (Some): build inner from canonical params 2..
                match self.ctx.ty_kind(inner_ty) {
                    InternedTyKind::Adt(d)
                        if matches!(
                            self.ctx.defs.kind(*d),
                            yel_core::definitions::DefKind::Record(_)
                        ) =>
                    {
                        let record_def_id = *d;
                        let mut next_param: u32 = 2;
                        self.emit_setter_pack_dtr_record(
                            &mut func,
                            record_def_id,
                            &mut next_param,
                        )?;
                    }
                    InternedTyKind::List(_)
                        if self
                            .record_gc_types
                            .list_array_type_idx
                            .contains_key(&inner_ty) =>
                    {
                        let inner_arr_idx = self.record_gc_types.list_array_type_idx[&inner_ty];
                        let unmat_fn = *self
                            .gc_list_unmaterializer_fn_indices
                            .get(&inner_arr_idx)
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(format!(
                                    "option<list> setter: missing un-materializer for arr {}",
                                    inner_arr_idx
                                ))
                            })?;
                        // ptr (param 2), len (param 3)
                        func.instruction(&Instruction::LocalGet(2));
                        func.instruction(&Instruction::LocalGet(3));
                        func.instruction(&Instruction::Call(unmat_fn));
                    }
                    InternedTyKind::String => {
                        // strings-to-GC: option<string> Some payload — build
                        // a $str_bytes ref from canonical (ptr, len).
                        func.instruction(&Instruction::LocalGet(2));
                        func.instruction(&Instruction::LocalGet(3));
                        self.emit_str_bytes_unmaterialize(&mut func)?;
                    }
                    InternedTyKind::Tuple(_) => {
                        // option<tuple> Some payload — build the tuple GC
                        // struct from canonical params 2.. (recursively).
                        let mut next_param: u32 = 2;
                        self.emit_setter_pack_tuple(&mut func, inner_ty, &mut next_param)?;
                    }
                    _ => {
                        return Err(CodegenError::InvalidIR(format!(
                            "option-collapsed setter: unsupported inner ty {:?}",
                            inner_ty
                        )));
                    }
                }
                func.instruction(&Instruction::Else);
                // disc!=0 (None): typed null ref.
                func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                    arr_idx,
                )));
                func.instruction(&Instruction::End);
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: field_path[0],
                });
                self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                self.current_self_local = None;
                self.current_self_comp_idx = None;
                func.instruction(&Instruction::End);
                return Ok(func);
            }

            // Phase 5e.3: tuple-as-signal setter — params are flat
            // canonical slots (one per tuple element). Push self, then
            // each flat param (consuming canonical slot count per
            // element via `canonical_flat_valtypes`), `struct.new
            // $tuple_<n>` to build the ref, then `struct.set` into
            // the component field.
            if let InternedTyKind::Tuple(_) = self.ctx.ty_kind(ty) {
                // Build the tuple GC struct from the canonical-ABI flat params
                // (recursively — see `emit_setter_pack_tuple`) and store the
                // resulting ref into the component field.
                self.emit_self_ref(&mut func, comp_idx)?;
                let mut next_param: u32 = 1;
                self.emit_setter_pack_tuple(&mut func, ty, &mut next_param)?;
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: field_path[0],
                });
                self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                self.current_self_local = None;
                self.current_self_comp_idx = None;
                func.instruction(&Instruction::End);
                return Ok(func);
            }
            // Phase 5e.5 (Stage 7 partial): FlatGcStruct setter —
            // params are canonical (rep, disc, ...payload-slots).
            // Dispatch on disc: for each case k, if disc == k, build
            // `struct.new $<sup>_<case_k>(payload)` (or
            // `struct.new_default` for empty payload) and
            // `struct.set` on the component field.
            //
            // Stage 6a only admits `option<scalar>` so payload is
            // 0 or 1 canonical slot. The general loop here also
            // handles any future Stage 6e/f (`result`, user variants)
            // with multi-slot payloads, but those aren't gated on
            // yet; the loop's per-case payload slot count is computed
            // from `canonical_flat_valtypes(case_payload_ty)` so it
            // generalises automatically.
            if let super::super::repr::InternalRepr::FlatGcStruct(_super_idx) =
                self.internal_repr(ty)
            {
                let case_count = *self
                    .record_gc_types
                    .flat_gc_case_count
                    .get(&ty)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "FlatGcStruct setter: missing case count for {:?}",
                            ty
                        ))
                    })?;
                // Disc is canonical param at index 1 (param 0 = rep).
                // Payload slots follow at indices 2.. (canonical-shape
                // joined; per-case payload reads only its own subset).
                //
                // Strategy: chained `if disc == k then build & store`.
                // After all cases, fall through (no-op — should never
                // happen if host obeys the canonical ABI).
                for k in 0..case_count {
                    // disc == k ?
                    func.instruction(&Instruction::LocalGet(1));
                    func.instruction(&Instruction::I32Const(k as i32));
                    func.instruction(&Instruction::I32Eq);
                    func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                    let case_sub_idx = *self
                        .record_gc_types
                        .flat_gc_case_idx
                        .get(&(ty, k))
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "FlatGcStruct setter: missing case_idx for ({:?}, {})",
                                ty, k
                            ))
                        })?;

                    self.emit_self_ref(&mut func, comp_idx)?;

                    if let Some(payload_ty) =
                        super::super::gc_types::case_payload_ty(self.ctx, ty, k)
                    {
                        // Phase 5e.5 Stage 7f: nested FlatGcStruct
                        // payload — recursively build the inner
                        // supertype ref from canonical params before
                        // wrapping in the outer case subtype.
                        if matches!(
                            self.internal_repr(payload_ty),
                            super::super::repr::InternalRepr::FlatGcStruct(_)
                        ) {
                            self.emit_pack_canonical_to_flat_gc(&mut func, payload_ty, 2)?;
                            func.instruction(&Instruction::StructNew(case_sub_idx));
                        } else {
                            // Push canonical-flat slots; box fat-value
                            // for string / non-typed-array list payloads.
                            // Phase 5e.5 Stage 7d: when the parent's
                            // joined canonical slot valtype differs
                            // from this case's payload valtype (width
                            // promotion: e.g. variant<a(s32), b(f32)>
                            // joins to i32, so case B needs
                            // f32.reinterpret_i32), insert the
                            // reinterpret after each LocalGet.
                            // Typed list payload (in list_array_type_idx):
                            // canonical (ptr, len) needs to be converted
                            // to a typed-array ref via the per-list un-
                            // materializer before wrapping in the case
                            // subtype.
                            let is_typed_list =
                                matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_))
                                    && self
                                        .record_gc_types
                                        .list_array_type_idx
                                        .contains_key(&payload_ty);
                            if is_typed_list {
                                let arr_type_idx = *self
                                    .record_gc_types
                                    .list_array_type_idx
                                    .get(&payload_ty)
                                    .unwrap();
                                let unmat_fn = *self
                                    .gc_list_unmaterializer_fn_indices
                                    .get(&arr_type_idx)
                                    .ok_or_else(|| {
                                        CodegenError::InvalidIR(format!(
                                            "FlatGcStruct setter (typed list): missing \
                                             un-materializer for arr {}",
                                            arr_type_idx
                                        ))
                                    })?;
                                // Push canonical (ptr, len) from params 2, 3.
                                func.instruction(&Instruction::LocalGet(2));
                                func.instruction(&Instruction::LocalGet(3));
                                func.instruction(&Instruction::Call(unmat_fn));
                                func.instruction(&Instruction::StructNew(case_sub_idx));
                            } else {
                                let payload_flat = self.canonical_flat_valtypes(payload_ty);
                                let parent_canonical = self.canonical_flat_valtypes(ty);
                                for (next_param, (i, vt_payload)) in
                                    (2_u32..).zip(payload_flat.iter().enumerate())
                                {
                                    func.instruction(&Instruction::LocalGet(next_param));
                                    let vt_joined =
                                        parent_canonical.get(1 + i).copied().unwrap_or(*vt_payload);
                                    emit_canonical_reinterpret(&mut func, vt_joined, *vt_payload)?;
                                }
                                // strings-to-GC: a string payload builds a
                                // `$str_bytes` ref from canonical (ptr, len).
                                // Every valid list is a typed array handled
                                // above, so String is the only ref-built
                                // payload here — nothing boxes into $fat_value.
                                if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String) {
                                    let str_bytes_idx =
                                        self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                                            CodegenError::InvalidIR(
                                                "FlatGcStruct setter: $str_bytes missing".into(),
                                            )
                                        })?;
                                    let unmat_fn = *self
                                        .gc_list_unmaterializer_fn_indices
                                        .get(&str_bytes_idx)
                                        .ok_or_else(|| {
                                            CodegenError::InvalidIR(
                                                "FlatGcStruct setter: missing $str_bytes \
                                                 un-materializer"
                                                    .into(),
                                            )
                                        })?;
                                    func.instruction(&Instruction::Call(unmat_fn));
                                }
                                func.instruction(&Instruction::StructNew(case_sub_idx));
                            }
                        }
                    } else {
                        // No payload: empty case subtype.
                        func.instruction(&Instruction::StructNewDefault(case_sub_idx));
                    }

                    func.instruction(&Instruction::StructSet {
                        struct_type_index: struct_ty,
                        field_index: field_path[0],
                    });
                    func.instruction(&Instruction::End);
                }
                self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
                self.current_self_local = None;
                self.current_self_comp_idx = None;
                func.instruction(&Instruction::End);
                return Ok(func);
            }

            if is_por {
                let record_type_idx = self.por_record_type_idx(ty).ok_or_else(|| {
                    CodegenError::InvalidIR("SLR setter: missing record type idx".into())
                })?;
                let record_def_id = match self.ctx.ty_kind(ty) {
                    InternedTyKind::Adt(d) => *d,
                    _ => {
                        return Err(CodegenError::InvalidIR(
                            "SLR setter: signal_ty is not an Adt".into(),
                        ));
                    }
                };
                let _record_def = match self.ctx.defs.kind(record_def_id) {
                    yel_core::definitions::DefKind::Record(r) => r.clone(),
                    _ => {
                        return Err(CodegenError::InvalidIR(
                            "SLR setter: not a record def".into(),
                        ));
                    }
                };
                // Push self ref, then per-field push the flat params
                // (boxing string/list pairs into $fat_value), then
                // `struct.new $<rec>_record`, then `struct.set` on the
                // component field.
                self.emit_self_ref(&mut func, comp_idx)?;
                let mut next_param: u32 = 1;
                self.emit_setter_pack_dtr_record(
                    &mut func,
                    record_def_id,
                    &mut next_param,
                )?;
                debug_assert_eq!(next_param - 1, actual_flat_count);
                let _ = record_type_idx; // already pushed by recursion
                func.instruction(&Instruction::StructSet {
                    struct_type_index: struct_ty,
                    field_index: field_path[0],
                });
            } else {
                for (i, &field_idx) in field_path.iter().enumerate() {
                    self.emit_self_ref(&mut func, comp_idx)?;
                    // Param 0 is `self: i32`; flat slot params start at 1.
                    func.instruction(&Instruction::LocalGet(1 + i as u32));
                    func.instruction(&Instruction::StructSet {
                        struct_type_index: struct_ty,
                        field_index: field_idx,
                    });
                }
            }
            self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
            self.current_self_local = None;
            self.current_self_comp_idx = None;
            func.instruction(&Instruction::End);
            return Ok(func);
        }
        // Non-struct / variant-in-memory signal path: unreachable — every
        // non-unit signal is GC-struct-resident, so the struct path above
        // always returns before reaching here.
        unreachable!(
            "setter: non-struct/variant-in-memory signal path is unreachable — \
             every non-unit signal is GC-struct-resident"
        )
    }

    /// Phase 5b-v.3: emit a standalone `$gc_list_unbox_<arr_type_idx>`
    /// function that converts a GC array ref → linear-memory fat pointer.
    ///
    /// Signature: `(ref null $arr_type_idx) -> (i32, i32)` — (data_ptr, len).
    /// Used by `SignalRead` when a GC-list signal is consumed in a
    /// Phase 5e.6: emit a per-array un-materializer fn — the inverse of
    /// the materializer. Takes canonical `(ptr, len)` and returns a
    /// typed `(ref null $arr)` GC array. Currently handles only the
    /// `list<string>` element shape (each elem = 8 canonical bytes →
    /// `$fat_value` box). Other element shapes return a placeholder
    /// empty array — this surfaces a clear runtime issue rather than a
    /// compile-time hang while the rest of the migration lands.
    /// Phase 5e.5 (Stage 7 partial): lift a `FlatGcStruct` signal into
    /// the canonical-ABI `(disc, payload-bytes)` memory layout for the
    /// WIT export boundary. Allocates a `cabi_realloc`'d scratch buffer
    /// sized to the signal's canonical layout, writes the disc + active
    /// case's payload bytes, and leaves the scratch pointer on the
    /// stack as the getter's return value.
    ///
    /// Per-case body uses a `block $done; … br $done` cascade so once
    /// a case matches we skip the remaining tests and the fall-through
    /// default. The default writes disc=0 (legacy zero-byte memory
    /// parity) if every `ref.test` fails — only reachable for
    /// uninitialized FlatGcStruct signals (defensive).
    ///
    /// Payload writes follow the case subtype's payload field type:
    /// - Primitive scalar (i32 / i64 / f32 / f64): single typed store.
    /// - String / non-typed-array list: payload field is
    ///   `(ref null $fat_value)`; unbox via two `struct.get $fat_value`s
    ///   and store the (ptr, len) pair at consecutive canonical slot
    ///   offsets.
    fn emit_flat_gc_signal_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
        signal_ty: Ty,
        super_idx: u32,
        scratch_ptr_local: u32,
    ) -> Result<(), CodegenError> {
        use super::scratch::mem_arg;
        let _ = super_idx;

        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("FlatGcStruct lift: cabi_realloc missing".into())
            })?
            .cabi_realloc;
        let layout_info = self.layout_ctx.layout_of(signal_ty);
        let canonical_slots = self.flatten_core_slots(signal_ty);

        let disc_offset = canonical_slots.first().map(|s| s.offset).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "FlatGcStruct lift: canonical layout has zero slots for {:?}",
                signal_ty
            ))
        })?;

        // scratch = cabi_realloc(0, 0, align, size)
        super::scratch::emit_cabi_realloc_fixed(func, layout_info.align, layout_info.size, cabi_realloc);
        func.instruction(&Instruction::LocalSet(scratch_ptr_local));

        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&signal_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct lift: missing case count for {:?}",
                    signal_ty
                ))
            })?;

        // Outer block lets a matching case skip the remaining tests +
        // the fall-through default via `br $done`.
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));

        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(signal_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "FlatGcStruct lift: missing case_idx for ({:?}, {})",
                        signal_ty, k
                    ))
                })?;

            self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // disc = k
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if disc_offset != 0 {
                func.instruction(&Instruction::I32Const(disc_offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

            if let Some(payload_ty) =
                super::super::gc_types::case_payload_ty(self.ctx, signal_ty, k)
            {
                self.emit_flat_gc_payload_lift(
                    func,
                    ci,
                    sig_idx,
                    case_sub_idx,
                    payload_ty,
                    &canonical_slots,
                    scratch_ptr_local,
                )?;
            }

            // Skip remaining cases + fall-through default.
            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
        }

        // Fall-through default (signal field was null): disc=0.
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if disc_offset != 0 {
            func.instruction(&Instruction::I32Const(disc_offset as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

        // End outer block.
        func.instruction(&Instruction::End);

        // Return scratch pointer.
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        Ok(())
    }

    /// Phase 7: lift an option-of-collapsed-ref signal — `option<T>` where
    /// T's internal repr is a single GC ref (record / list-array / tuple)
    /// — to canonical-ABI bytes. Storage is one nullable ref; canonical
    /// shape is `[disc(i32), …inner_canonical_slots]`. Convention here
    /// matches the FlatGcStruct lift: disc=0 means Some (case 0), disc=1
    /// means None (case 1). `ref.is_null` returns 1 when null and 0
    /// otherwise — exactly the disc value we want.
    fn emit_option_collapsed_ref_signal_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
        signal_ty: Ty,
        scratch_ptr_local: u32,
        inner_ref_local: u32,
        mat_ptr_local: u32,
        mat_len_local: u32,
    ) -> Result<(), CodegenError> {
        use super::super::StoreWidth;
        use super::scratch::mem_arg;

        let inner_ty = match self.ctx.ty_kind(signal_ty) {
            InternedTyKind::Option(t) => *t,
            _ => {
                return Err(CodegenError::InvalidIR(
                    "option_collapsed_ref_signal_lift: not an option type".into(),
                ));
            }
        };

        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("option-collapsed lift: cabi_realloc missing".into())
            })?
            .cabi_realloc;
        let layout_info = self.layout_ctx.layout_of(signal_ty);
        let canonical_slots = self.flatten_core_slots(signal_ty);
        let disc_offset = canonical_slots
            .first()
            .map(|s| s.offset as i32)
            .unwrap_or(0);
        let payload_slots: Vec<_> = canonical_slots.iter().skip(1).cloned().collect();

        // Allocate scratch.
        super::scratch::emit_cabi_realloc_fixed(func, layout_info.align, layout_info.size, cabi_realloc);
        func.instruction(&Instruction::LocalSet(scratch_ptr_local));

        // Read collapsed ref → inner_ref_local.
        self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
        func.instruction(&Instruction::LocalSet(inner_ref_local));

        // Canonical-ABI option/result discriminant: 0 = none, 1 = some — the
        // standard WIT convention the host expects (matching the direct
        // GC-list option getter). The collapsed ref is null for none, so
        // `disc = !ref.is_null` (some = non-null = 1). (Do NOT use bare
        // ref.is_null here: that stores the inverted some=0 convention and the
        // host reads every Some back as None.)
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if disc_offset != 0 {
            func.instruction(&Instruction::I32Const(disc_offset));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::LocalGet(inner_ref_local));
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

        // Conditional payload lift.
        func.instruction(&Instruction::LocalGet(inner_ref_local));
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

        // Non-null branch — dispatch on the inner shape.
        match self.ctx.ty_kind(inner_ty) {
            InternedTyKind::Adt(d)
                if matches!(
                    self.ctx.defs.kind(*d),
                    yel_core::definitions::DefKind::Record(_)
                ) =>
            {
                // The collapsed ref IS the record GC struct, stored directly
                // in the signal field. Reach it via the signal-field prefix
                // and lower each field with the complete recursive record lift
                // (handles string / list / nested-record / flat-gc fields) —
                // the older `emit_inline_record_lift_from_anyref` panics on a
                // string field.
                let record_def_id = *d;
                let struct_ty = self.gc_layouts[ci].component_struct_type_idx.ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "option<record> lift: missing component_struct_type_idx".into(),
                    )
                })?;
                let field_path = self.components[ci].signal_layout.signal_field_path(sig_idx);
                let prefix: Vec<(u32, u32)> = vec![(struct_ty, field_path[0])];
                self.emit_getter_lift_dtr_record(
                    func,
                    ci,
                    record_def_id,
                    payload_slots[0].offset,
                    scratch_ptr_local,
                    &prefix,
                )?;
            }
            InternedTyKind::List(_)
                if self
                    .record_gc_types
                    .list_array_type_idx
                    .contains_key(&inner_ty) =>
            {
                let arr_idx = self.record_gc_types.list_array_type_idx[&inner_ty];
                let mat_fn = *self
                    .gc_list_materializer_fn_indices
                    .get(&arr_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "option<list>: missing materializer for arr_type_idx={}",
                            arr_idx
                        ))
                    })?;
                func.instruction(&Instruction::LocalGet(inner_ref_local));
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(arr_idx),
                ));
                func.instruction(&Instruction::Call(mat_fn));
                // (ptr, len) on the stack.
                func.instruction(&Instruction::LocalSet(mat_len_local));
                func.instruction(&Instruction::LocalSet(mat_ptr_local));
                let ptr_slot = &payload_slots[0];
                let len_slot = &payload_slots[1];
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                if ptr_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(mat_ptr_local));
                ptr_slot.store.emit_store(func);
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                if len_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(mat_len_local));
                len_slot.store.emit_store(func);
            }
            InternedTyKind::String => {
                // strings-to-GC: option<string> Some — the collapsed ref is a
                // `$str_bytes`; materialize to (ptr, len) and store at the
                // canonical payload slots.
                let arr_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                    CodegenError::InvalidIR("option<string> lift: $str_bytes not registered".into())
                })?;
                let mat_fn = *self
                    .gc_list_materializer_fn_indices
                    .get(&arr_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "option<string> lift: missing $str_bytes materializer".into(),
                        )
                    })?;
                func.instruction(&Instruction::LocalGet(inner_ref_local));
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(arr_idx),
                ));
                func.instruction(&Instruction::Call(mat_fn));
                func.instruction(&Instruction::LocalSet(mat_len_local));
                func.instruction(&Instruction::LocalSet(mat_ptr_local));
                let ptr_slot = &payload_slots[0];
                let len_slot = &payload_slots[1];
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                if ptr_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(mat_ptr_local));
                ptr_slot.store.emit_store(func);
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                if len_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(mat_len_local));
                len_slot.store.emit_store(func);
            }
            InternedTyKind::Tuple(_) => {
                // The collapsed ref IS the tuple GC struct, stored directly in
                // the signal field. We're already inside the non-null (Some)
                // branch, so reach the tuple via the signal-field prefix and
                // lower each element to canonical at the payload base.
                let struct_ty = self.gc_layouts[ci].component_struct_type_idx.ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "option<tuple> lift: missing component_struct_type_idx".into(),
                    )
                })?;
                let field_path = self.components[ci].signal_layout.signal_field_path(sig_idx);
                let base = payload_slots[0].offset;
                let prefix: Vec<(u32, u32)> = vec![(struct_ty, field_path[0])];
                self.emit_getter_lift_tuple(
                    func,
                    ci,
                    inner_ty,
                    base,
                    scratch_ptr_local,
                    mat_ptr_local,
                    mat_len_local,
                    &prefix,
                )?;
            }
            _ => {
                return Err(CodegenError::InvalidIR(format!(
                    "option-collapsed lift: unsupported inner ty {:?}",
                    inner_ty
                )));
            }
        }

        func.instruction(&Instruction::Else);
        // Null branch — zero-fill payload slots.
        for slot in &payload_slots {
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if slot.offset != 0 {
                func.instruction(&Instruction::I32Const(slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            match slot.store {
                StoreWidth::I64 => {
                    func.instruction(&Instruction::I64Const(0));
                }
                StoreWidth::F32 => {
                    func.instruction(&Instruction::F32Const(0.0.into()));
                }
                StoreWidth::F64 => {
                    func.instruction(&Instruction::F64Const(0.0.into()));
                }
                _ => {
                    func.instruction(&Instruction::I32Const(0));
                }
            }
            slot.store.emit_store(func);
        }
        func.instruction(&Instruction::End);

        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        Ok(())
    }

    /// Walk a record's fields and lift each to canonical-ABI bytes at
    /// `base_addr_local + base_offset`. The record ref is held in
    /// `record_ref_anyref_local` as an anyref; we ref.cast it to the
    /// concrete record type at every use site rather than requiring a
    /// typed record-ref local.
    fn emit_inline_record_lift_from_anyref(
        &mut self,
        func: &mut Function,
        record_def_id: DefId,
        record_ref_anyref_local: u32,
        base_addr_local: u32,
        base_offset: u32,
        mat_ptr_local: u32,
        mat_len_local: u32,
    ) -> Result<(), CodegenError> {
        use super::scratch::mem_arg;

        let record_def = match self.ctx.defs.kind(record_def_id) {
            DefKind::Record(r) => r.clone(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "inline_record_lift: not a record def".into(),
                ));
            }
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("inline_record_lift: missing record_type_idx".into())
            })?;
        let gc_field_indices: Vec<u32> = self
            .record_gc_types
            .field_gc_indices
            .get(&record_def_id)
            .cloned()
            .ok_or_else(|| {
                CodegenError::InvalidIR("inline_record_lift: missing gc field indices".into())
            })?;
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR("inline_record_lift: missing record layout".into())
            })?
            .clone();

        for (i, &field_def_id) in record_def.fields.iter().enumerate() {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => continue,
            };
            let (_n, field_offset, _t) = layout.field_offsets.get(i).cloned().ok_or_else(|| {
                CodegenError::InvalidIR("inline_record_lift: missing field offset".into())
            })?;
            let abs_off = base_offset + field_offset;
            let gc_field_idx = gc_field_indices[i];

            // Helper: load the record ref, cast to typed, struct.get the field.
            let load_field = |f: &mut Function| {
                f.instruction(&Instruction::LocalGet(record_ref_anyref_local));
                f.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(record_type_idx),
                ));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: record_type_idx,
                    field_index: gc_field_idx,
                });
            };

            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::String => {
                    // A string field is a `$str_bytes` GC ref, not a
                    // `$fat_value` box; its inline lift is handled by the
                    // str_bytes-aware path, never here.
                    unreachable!(
                        "inline_record_lift: string field boxed into $fat_value — \
                         strings are $str_bytes GC refs, materialized elsewhere"
                    );
                }
                InternedTyKind::List(_)
                    if self
                        .record_gc_types
                        .list_array_type_idx
                        .contains_key(&field_ty) =>
                {
                    let arr_idx = self.record_gc_types.list_array_type_idx[&field_ty];
                    let mat_fn = *self
                        .gc_list_materializer_fn_indices
                        .get(&arr_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "inline_record_lift: missing materializer for arr_type_idx={}",
                                arr_idx
                            ))
                        })?;
                    load_field(func);
                    func.instruction(&Instruction::Call(mat_fn));
                    func.instruction(&Instruction::LocalSet(mat_len_local));
                    func.instruction(&Instruction::LocalSet(mat_ptr_local));
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    if abs_off != 0 {
                        func.instruction(&Instruction::I32Const(abs_off as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(mat_ptr_local));
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(mat_len_local));
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                }
                InternedTyKind::F32
                | InternedTyKind::F64
                | InternedTyKind::S64
                | InternedTyKind::U64
                | InternedTyKind::S32
                | InternedTyKind::U32
                | InternedTyKind::S16
                | InternedTyKind::U16
                | InternedTyKind::S8
                | InternedTyKind::U8
                | InternedTyKind::Bool
                | InternedTyKind::Char => {
                    // Scalar primitive: load from struct, store at abs_off
                    // using the field's natural width.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    if abs_off != 0 {
                        func.instruction(&Instruction::I32Const(abs_off as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    load_field(func);
                    self.emit_typed_field_store(func, field_ty);
                }
                _ => {
                    return Err(CodegenError::InvalidIR(format!(
                        "inline_record_lift: unsupported field type {:?}",
                        self.ctx.ty_kind(field_ty)
                    )));
                }
            }
        }
        Ok(())
    }

    /// Phase 5e.5: write a single case's payload bytes into the
    /// canonical-ABI scratch. The case subtype's payload field is at
    /// struct index 0; its WASM type depends on payload Ty
    /// (`record_field_storage_type` rules):
    /// - Primitive scalar / packed → typed store.
    /// - String / non-typed-array list → field is `(ref null $fat_value)`;
    ///   unbox via two `struct.get $fat_value`s and write (ptr, len)
    ///   at consecutive canonical slot offsets.
    fn emit_flat_gc_payload_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
        case_sub_idx: u32,
        payload_ty: Ty,
        canonical_slots: &[crate::wasm::FlatSlot],
        scratch_ptr_local: u32,
    ) -> Result<(), CodegenError> {
        use super::super::gc_types::StructGetVariant;
        use yel_core::types::InternedTyKind;

        // Identify whether the case-subtype's payload field is a
        // `$fat_value` ref (string / non-typed-array list) — needs
        // unboxing — or a direct value (primitive scalar / typed ref).
        // Typed list payload (in list_array_type_idx): the case-subtype
        // field is a `(ref null $list_arr)`. Canonical-ABI lowering
        // expects (ptr, len) at canonical_slots[1..3], so call the per-
        // list materializer to lower the GC array to inline-memory
        // bytes, then write ptr/len.
        if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_))
            && self
                .record_gc_types
                .list_array_type_idx
                .contains_key(&payload_ty)
        {
            let arr_type_idx = *self
                .record_gc_types
                .list_array_type_idx
                .get(&payload_ty)
                .unwrap();
            let mat_fn = *self
                .gc_list_materializer_fn_indices
                .get(&arr_type_idx)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "FlatGcStruct payload lift (typed list): missing materializer for arr {}",
                        arr_type_idx
                    ))
                })?;
            let ptr_slot = canonical_slots.get(1).ok_or_else(|| {
                CodegenError::InvalidIR(
                    "FlatGcStruct payload lift (typed list): missing ptr slot".into(),
                )
            })?;
            let len_slot = canonical_slots.get(2).ok_or_else(|| {
                CodegenError::InvalidIR(
                    "FlatGcStruct payload lift (typed list): missing len slot".into(),
                )
            })?;
            // Pre-allocated scratch locals 3 (ptr) and 4 (len) in the
            // getter's local space — see getter setup above.
            let mat_ptr_local: u32 = 3;
            let mat_len_local: u32 = 4;
            // Load case typed array ref, call materializer → (ptr, len).
            self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: case_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::Call(mat_fn));
            func.instruction(&Instruction::LocalSet(mat_len_local));
            func.instruction(&Instruction::LocalSet(mat_ptr_local));
            // Store ptr at scratch + ptr_slot.offset.
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if ptr_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::LocalGet(mat_ptr_local));
            func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            // Store len at scratch + len_slot.offset.
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if len_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::LocalGet(mat_len_local));
            func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            return Ok(());
        }

        // strings-to-GC: a string payload's case-subtype field is a
        // `(ref null $str_bytes)` — materialize to (ptr, len) like a typed
        // list, not an unbox of `$fat_value`.
        if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
        {
            let arr_type_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                CodegenError::InvalidIR("FlatGcStruct string payload lift: $str_bytes missing".into())
            })?;
            let mat_fn = *self
                .gc_list_materializer_fn_indices
                .get(&arr_type_idx)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "FlatGcStruct string payload lift: missing $str_bytes materializer".into(),
                    )
                })?;
            let ptr_slot = canonical_slots.get(1).ok_or_else(|| {
                CodegenError::InvalidIR("FlatGcStruct string payload lift: missing ptr slot".into())
            })?;
            let len_slot = canonical_slots.get(2).ok_or_else(|| {
                CodegenError::InvalidIR("FlatGcStruct string payload lift: missing len slot".into())
            })?;
            let mat_ptr_local: u32 = 3;
            let mat_len_local: u32 = 4;
            self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: case_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::Call(mat_fn));
            func.instruction(&Instruction::LocalSet(mat_len_local));
            func.instruction(&Instruction::LocalSet(mat_ptr_local));
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if ptr_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::LocalGet(mat_ptr_local));
            func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if len_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::LocalGet(mat_len_local));
            func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            return Ok(());
        }

        // Phase 5e.5 Stage 7f: nested FlatGcStruct payload — the
        // case-subtype field is itself a `(ref null $inner_super)`.
        // Cast outer to case_sub, load inner ref, then recursively
        // lift via per-inner-case cascade writing canonical bytes at
        // the slot offsets that follow disc.
        if matches!(
            self.internal_repr(payload_ty),
            super::super::repr::InternalRepr::FlatGcStruct(_)
        ) {
            let inner_super_idx = match self.internal_repr(payload_ty) {
                super::super::repr::InternalRepr::FlatGcStruct(s) => s,
                _ => unreachable!(),
            };
            let _ = inner_super_idx;
            let inner_case_count = *self
                .record_gc_types
                .flat_gc_case_count
                .get(&payload_ty)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "FlatGcStruct payload lift (nested): missing case count for {:?}",
                        payload_ty
                    ))
                })?;
            // Inner canonical slots (with absolute offsets within
            // payload_ty starting at 0). We need to map them onto the
            // outer canonical slots [1..]: outer slot at index k
            // corresponds to inner slot at index k-1 by canonical-ABI
            // construction (option/result join the inner flat into
            // the outer payload region directly when no width
            // promotion).
            let inner_slots = self.flatten_core_slots(payload_ty);
            if canonical_slots.len() < 1 + inner_slots.len() {
                return Err(CodegenError::InvalidIR(format!(
                    "FlatGcStruct payload lift (nested): outer canonical \
                     has {} slots, inner needs {} after disc",
                    canonical_slots.len(),
                    inner_slots.len()
                )));
            }
            // Per-inner-case cascade.
            func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            for inner_k in 0..inner_case_count {
                let inner_case_sub_idx = *self
                    .record_gc_types
                    .flat_gc_case_idx
                    .get(&(payload_ty, inner_k))
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "FlatGcStruct payload lift (nested): missing inner \
                             case_idx for ({:?}, {})",
                            payload_ty, inner_k
                        ))
                    })?;
                // Load inner ref: <signal>; ref.cast outer_case; struct.get 0
                self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(case_sub_idx),
                ));
                func.instruction(&Instruction::StructGet {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
                func.instruction(&Instruction::RefTestNonNull(
                    wasm_encoder::HeapType::Concrete(inner_case_sub_idx),
                ));
                func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Write inner disc=inner_k at outer canonical_slots[1].offset.
                let inner_disc_outer_slot = canonical_slots.get(1).ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "FlatGcStruct payload lift (nested): missing outer slot \
                         for inner disc"
                            .into(),
                    )
                })?;
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                if inner_disc_outer_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(inner_disc_outer_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::I32Const(inner_k as i32));
                func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
                // If inner case has payload, recursively lift it.
                if let Some(inner_payload_ty) =
                    super::super::gc_types::case_payload_ty(self.ctx, payload_ty, inner_k)
                {
                    self.emit_nested_flat_gc_inner_payload_lift(
                        func,
                        ci,
                        sig_idx,
                        case_sub_idx,
                        inner_case_sub_idx,
                        inner_payload_ty,
                        canonical_slots,
                        scratch_ptr_local,
                    )?;
                }
                func.instruction(&Instruction::Br(1));
                func.instruction(&Instruction::End);
            }
            // Default (inner ref is null): inner disc = 0 (case 0).
            let inner_disc_outer_slot = canonical_slots.get(1).ok_or_else(|| {
                CodegenError::InvalidIR(
                    "FlatGcStruct payload lift (nested default): missing outer slot \
                     for inner disc"
                        .into(),
                )
            })?;
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if inner_disc_outer_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(inner_disc_outer_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
            func.instruction(&Instruction::End);
            return Ok(());
        }

        // Primitive scalar or simple typed payload: single canonical
        // slot. Push (addr, value) and use the payload type's natural
        // store width.
        let payload_slot = canonical_slots.get(1).ok_or_else(|| {
            CodegenError::InvalidIR(
                "FlatGcStruct payload lift: missing payload slot in canonical layout".into(),
            )
        })?;
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if payload_slot.offset != 0 {
            func.instruction(&Instruction::I32Const(payload_slot.offset as i32));
            func.instruction(&Instruction::I32Add);
        }
        self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(case_sub_idx),
        ));
        let getter = super::super::gc_types::struct_get_op_for_payload(self.ctx, payload_ty);
        match getter {
            StructGetVariant::Plain => {
                func.instruction(&Instruction::StructGet {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Signed => {
                func.instruction(&Instruction::StructGetS {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Unsigned => {
                func.instruction(&Instruction::StructGetU {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
            }
        }
        self.emit_typed_field_store(func, payload_ty);
        Ok(())
    }

    /// Phase 5e.5: re-emit `<self>.struct.get $Comp $sig` (the FlatGcStruct
    /// supertype-ref load). Centralised because `emit_flat_gc_signal_lift`
    /// re-emits the read for each case test + for the cast/payload load.
    fn emit_signal_struct_read_for_lift(
        &self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
    ) -> Result<(), CodegenError> {
        let gc_layout = &self.gc_layouts[ci];
        let struct_ty = gc_layout.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("FlatGcStruct lift: missing component_struct_type_idx".into())
        })?;
        let field_idx = self.components[ci]
            .signal_layout
            .signal_field_path(sig_idx)
            .first()
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct lift: missing field path for signal {}",
                    sig_idx
                ))
            })?;
        let _ = gc_layout;
        self.emit_self_ref(func, ci)?;
        func.instruction(&Instruction::StructGet {
            struct_type_index: struct_ty,
            field_index: field_idx,
        });
        Ok(())
    }

    /// Phase 7: emit `$pack_color_to_attr_slots` body — the per-program
    /// helper that lifts a `(ref null $var_color)` to the canonical-ABI
    /// flattening of `attribute-value::color(color)`. Signature:
    /// `(ref null $var_color) -> (i64, i32, i32, i32, i32)` where the
    /// i64 is the inner color disc widened, and the four i32s are the
    /// rgba bytes (zero for non-rgba cases).
    pub(super) fn generate_pack_color_to_attr_slots(
        &self,
        color_ty: yel_core::Ty,
    ) -> Result<Function, CodegenError> {
        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&color_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR("pack_color: missing case count for color variant".into())
            })?;
        // Locate rgba — the only payload-bearing case.
        let mut rgba_idx_opt: Option<u32> = None;
        for k in 0..case_count {
            if super::super::gc_types::case_payload_ty(self.ctx, color_ty, k).is_some() {
                rgba_idx_opt = Some(k);
                break;
            }
        }
        let rgba_idx = rgba_idx_opt.ok_or_else(|| {
            CodegenError::InvalidIR("pack_color: color variant has no rgba payload case".into())
        })?;
        let rgba_sub_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(color_ty, rgba_idx))
            .ok_or_else(|| {
                CodegenError::InvalidIR("pack_color: missing rgba case-subtype index".into())
            })?;
        let rgba_payload_ty = super::super::gc_types::case_payload_ty(self.ctx, color_ty, rgba_idx)
            .expect("rgba case has a payload");
        let rgba_tuple_struct_idx = *self
            .record_gc_types
            .tuple_struct_type_idx
            .get(&rgba_payload_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(
                    "pack_color: rgba tuple<u8,u8,u8,u8> struct type not registered".into(),
                )
            })?;

        // Param 0 = (ref null $var_color). No locals needed — we
        // re-use param 0 by `local.get 0` in each branch.
        let mut func = Function::new([]);

        // Compute inner disc as i32 via chained ref.test against each
        // non-rgba case. Final else falls to rgba.
        let i32_block = wasm_encoder::BlockType::Result(ValType::I32);
        let mut nesting: u32 = 0;
        for k in 0..case_count {
            if k == rgba_idx {
                continue;
            }
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(color_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "pack_color: missing case_idx for ({:?}, {})",
                        color_ty, k
                    ))
                })?;
            func.instruction(&Instruction::LocalGet(0));
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::If(i32_block));
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::Else);
            nesting += 1;
        }
        func.instruction(&Instruction::I32Const(rgba_idx as i32));
        for _ in 0..nesting {
            func.instruction(&Instruction::End);
        }
        // Stack: i32 inner_disc → widen to i64 for slot 0.
        func.instruction(&Instruction::I64ExtendI32U);

        // Emit four independent ifs, each yielding 1 i32 byte (or 0
        // for non-rgba cases). Avoids depending on a multi-value
        // block type.
        for elem_field in 0..4u32 {
            func.instruction(&Instruction::LocalGet(0));
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(rgba_sub_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                ValType::I32,
            )));
            func.instruction(&Instruction::LocalGet(0));
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(rgba_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: rgba_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::RefAsNonNull);
            // Tuple struct fields store u8 as a non-packed i32 (per
            // `record_field_storage_type`'s primitive lowering), so use
            // plain `StructGet` rather than `StructGetU`.
            func.instruction(&Instruction::StructGet {
                struct_type_index: rgba_tuple_struct_idx,
                field_index: elem_field,
            });
            func.instruction(&Instruction::Else);
            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::End);
        }
        func.instruction(&Instruction::End);
        Ok(func)
    }

    pub(super) fn generate_gc_list_unmaterializer(
        &mut self,
        arr_type_idx: u32,
        elem_ty: yel_core::Ty,
    ) -> Result<Function, CodegenError> {
        use yel_core::types::InternedTyKind;
        let mut func = Function::new([
            (
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                }),
            ), // arr (local 2)
            (1, ValType::I32), // idx (local 3)
            (1, ValType::I32), // elem_addr (local 4)
        ]);
        // Params: 0 = ptr, 1 = len. Locals: 2 = arr, 3 = idx, 4 = elem_addr.
        let ptr_local: u32 = 0;
        let len_local: u32 = 1;
        let arr_local: u32 = 2;
        let idx_local: u32 = 3;
        let elem_addr_local: u32 = 4;

        // arr = array.new_default $arr (len)
        func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
        func.instruction(&wasm_encoder::Instruction::ArrayNewDefault(arr_type_idx));
        func.instruction(&wasm_encoder::Instruction::LocalSet(arr_local));

        // Option-of-ref collapse element: rebuild the collapsed inner ref
        // from canonical `(disc, payload)`. `none` elements stay the
        // default null ref; `some` builds the inner (record / tuple /
        // scalar-list) and `array.set`s it.
        if let Some(inner_ty) = self.elem_option_collapses(elem_ty) {
            return self.finish_gc_list_collapsed_option_unmaterializer(
                func,
                arr_type_idx,
                elem_ty,
                inner_ty,
                ptr_local,
                len_local,
                arr_local,
                idx_local,
                elem_addr_local,
            );
        }

        // Phase 5e.5 Stage 8a: FlatGcStruct element — for each
        // canonical (disc, payload) record at ptr + idx * elem_size,
        // build a supertype ref via per-case dispatch and store into
        // the typed array.
        if matches!(
            self.internal_repr(elem_ty),
            super::super::repr::InternalRepr::FlatGcStruct(_)
        ) {
            let layout_info = self.layout_ctx.layout_of(elem_ty);
            let elem_size = layout_info.size as i32;
            let canonical_slots = self.flatten_core_slots(elem_ty);
            let case_count = *self
                .record_gc_types
                .flat_gc_case_count
                .get(&elem_ty)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "list un-materializer (flat-gc): missing case count for {:?}",
                        elem_ty
                    ))
                })?;
            let super_idx = *self
                .record_gc_types
                .flat_gc_super_idx
                .get(&elem_ty)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "list un-materializer (flat-gc): missing super idx for {:?}",
                        elem_ty
                    ))
                })?;
            let _ = super_idx;
            let disc_off = canonical_slots
                .first()
                .map(|s| s.offset as i32)
                .unwrap_or(0);

            func.instruction(&Instruction::I32Const(0));
            func.instruction(&Instruction::LocalSet(idx_local));
            func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::LocalGet(len_local));
            func.instruction(&Instruction::I32GeU);
            func.instruction(&Instruction::BrIf(1));
            // elem_addr = ptr + idx * elem_size
            func.instruction(&Instruction::LocalGet(ptr_local));
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::I32Const(elem_size));
            func.instruction(&Instruction::I32Mul);
            func.instruction(&Instruction::I32Add);
            func.instruction(&Instruction::LocalSet(elem_addr_local));
            // disc = load8u(elem_addr + disc_off)
            // arr.set(idx, <build case ref via cascade>)
            func.instruction(&Instruction::LocalGet(arr_local));
            func.instruction(&Instruction::LocalGet(idx_local));

            // Build via chained-if cascade reading disc.
            let result_ty = wasm_encoder::BlockType::Result(wasm_encoder::ValType::Ref(
                wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(super_idx),
                },
            ));
            let mut nesting: u32 = 0;
            for k in 0..case_count {
                let case_sub_idx = *self
                    .record_gc_types
                    .flat_gc_case_idx
                    .get(&(elem_ty, k))
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "list un-materializer (flat-gc): missing case_idx for ({:?}, {})",
                            elem_ty, k
                        ))
                    })?;
                // disc == k ?
                func.instruction(&Instruction::LocalGet(elem_addr_local));
                if disc_off != 0 {
                    func.instruction(&Instruction::I32Const(disc_off));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::I32Load8U(super::scratch::mem_arg(0, 0)));
                func.instruction(&Instruction::I32Const(k as i32));
                func.instruction(&Instruction::I32Eq);
                func.instruction(&Instruction::If(result_ty));
                // Build case k.
                if let Some(payload_ty) =
                    super::super::gc_types::case_payload_ty(self.ctx, elem_ty, k)
                {
                    let is_fat_box = matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
                        || (matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_))
                            && !self
                                .record_gc_types
                                .list_array_type_idx
                                .contains_key(&payload_ty));
                    if is_fat_box {
                        let str_bytes_unmat = if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
                        {
                            self.record_gc_types.str_bytes_array_idx.and_then(|idx| {
                                self.gc_list_unmaterializer_fn_indices.get(&idx).copied()
                            })
                        } else {
                            None
                        };
                        let ptr_slot = canonical_slots.get(1).ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "list un-materializer (flat-gc): missing ptr slot".into(),
                            )
                        })?;
                        let len_slot = canonical_slots.get(2).ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "list un-materializer (flat-gc): missing len slot".into(),
                            )
                        })?;
                        // ptr
                        func.instruction(&Instruction::LocalGet(elem_addr_local));
                        if ptr_slot.offset != 0 {
                            func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                        // len
                        func.instruction(&Instruction::LocalGet(elem_addr_local));
                        if len_slot.offset != 0 {
                            func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                        let unmat_fn = str_bytes_unmat.expect(
                            "list un-materializer (flat-gc): fat-box payload that is not a \
                             $str_bytes string — every valid list is a typed GC array; \
                             nothing boxes into $fat_value",
                        );
                        func.instruction(&Instruction::Call(unmat_fn));
                        func.instruction(&Instruction::StructNew(case_sub_idx));
                    } else {
                        let payload_slot = canonical_slots.get(1).ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "list un-materializer (flat-gc): missing payload slot".into(),
                            )
                        })?;
                        func.instruction(&Instruction::LocalGet(elem_addr_local));
                        if payload_slot.offset != 0 {
                            func.instruction(&Instruction::I32Const(payload_slot.offset as i32));
                            func.instruction(&Instruction::I32Add);
                        }
                        self.emit_typed_field_load(&mut func, payload_ty);
                        func.instruction(&Instruction::StructNew(case_sub_idx));
                    }
                } else {
                    func.instruction(&Instruction::StructNewDefault(case_sub_idx));
                }
                func.instruction(&Instruction::Else);
                nesting += 1;
            }
            // Default: case 0.
            let case0 = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(elem_ty, 0))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "list un-materializer (flat-gc): missing case 0 for {:?}",
                        elem_ty
                    ))
                })?;
            func.instruction(&Instruction::StructNewDefault(case0));
            for _ in 0..nesting {
                func.instruction(&Instruction::End);
            }
            // Stack: arr_ref, idx, case_ref → array.set
            func.instruction(&Instruction::ArraySet(arr_type_idx));
            // idx++
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::I32Const(1));
            func.instruction(&Instruction::I32Add);
            func.instruction(&Instruction::LocalSet(idx_local));
            func.instruction(&Instruction::Br(0));
            func.instruction(&Instruction::End); // loop
            func.instruction(&Instruction::End); // block
            func.instruction(&Instruction::LocalGet(arr_local));
            func.instruction(&Instruction::End);
            return Ok(func);
        }
        if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String) {
            // strings-to-GC: element is a `$str_bytes` ref built from the
            // canonical (ptr, len) at ptr+idx*8 via the str_bytes
            // un-materializer.
            // for idx in 0..len { arr.set(idx, build_elem(load(ptr+idx*8), load(ptr+idx*8+4))) }
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Block(
                wasm_encoder::BlockType::Empty,
            ));
            func.instruction(&wasm_encoder::Instruction::Loop(
                wasm_encoder::BlockType::Empty,
            ));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::I32GeU);
            func.instruction(&wasm_encoder::Instruction::BrIf(1));
            // elem_addr = ptr + idx * 8
            func.instruction(&wasm_encoder::Instruction::LocalGet(ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(8));
            func.instruction(&wasm_encoder::Instruction::I32Mul);
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(elem_addr_local));
            // arr.set(idx, str_bytes_unmaterialize(load ptr, load len))
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            // ptr field
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::I32Load(
                super::scratch::mem_arg(0, 2),
            ));
            // len field
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(4));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::I32Load(
                super::scratch::mem_arg(0, 2),
            ));
            self.emit_str_bytes_unmaterialize(&mut func)?;
            func.instruction(&wasm_encoder::Instruction::ArraySet(arr_type_idx));
            // idx++
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(1));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Br(0));
            func.instruction(&wasm_encoder::Instruction::End);
            func.instruction(&wasm_encoder::Instruction::End);
        }
        // Phase 7: record element — for each canonical-flat record at
        // `ptr + idx * elem_size`, build a typed `(ref null $<rec>)`
        // via `emit_record_pack_from_memory` and `arr.set` it. Without
        // this branch the array is left filled with default (null)
        // refs, which then traps every downstream `struct.get` that
        // tries to read a Person field.
        if let yel_core::types::InternedTyKind::Adt(d) = self.ctx.ty_kind(elem_ty)
            && matches!(
                self.ctx.defs.kind(*d),
                yel_core::definitions::DefKind::Record(_)
            ) && self.record_gc_types.record_type_idx.contains_key(d)
            {
                let record_def_id = *d;
                let elem_size = self.layout_ctx.size_of(elem_ty) as i32;
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::LocalSet(idx_local));
                func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::LocalGet(len_local));
                func.instruction(&Instruction::I32GeU);
                func.instruction(&Instruction::BrIf(1));
                // elem_addr = ptr + idx * elem_size
                func.instruction(&Instruction::LocalGet(ptr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::I32Const(elem_size));
                func.instruction(&Instruction::I32Mul);
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(elem_addr_local));
                // arr.set(idx, pack_record_from_memory(elem_addr, 0))
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                self.emit_record_pack_from_memory(&mut func, record_def_id, elem_addr_local, 0)?;
                func.instruction(&Instruction::ArraySet(arr_type_idx));
                // idx++
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(idx_local));
                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End); // loop
                func.instruction(&Instruction::End); // block
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::End); // function
                return Ok(func);
            }
        // For other element shapes we just return the empty default
        // array. Callers that hit this with a non-string element will
        // see runtime missing-element behavior; the corresponding
        // record_pack_from_memory error will direct them here.
        let _ = (idx_local, elem_addr_local);
        func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
        func.instruction(&wasm_encoder::Instruction::End);
        Ok(func)
    }

    /// strings-to-GC (`plans/strings-to-gc.md`): materialize a `$str_bytes`
    /// GC byte array `(ref null $str_bytes)` into a canonical-ABI
    /// `(ptr, len)` in linear memory. Byte-for-byte copy via a
    /// `cabi_realloc`'d scratch buffer — the only place a GC-native string
    /// touches linear memory (the WIT boundary). Mirror of the
    /// scalar-list materializer, specialized to packed i8 elements.
    pub(super) fn generate_str_bytes_materializer(
        &mut self,
        arr_type_idx: u32,
    ) -> Result<Function, CodegenError> {
        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("str_bytes materializer requires cabi_realloc".into())
            })?
            .cabi_realloc;
        // Params: 0 = arr (ref null $str_bytes). Locals: 1 = len, 2 = data_ptr, 3 = idx.
        let mut func = Function::new([
            (1, ValType::I32), // len
            (1, ValType::I32), // data_ptr
            (1, ValType::I32), // idx
        ]);
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        // len = array.len(arr)
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(len_local));
        // data_ptr = cabi_realloc(0, 0, 1, len * 1)
        super::scratch::emit_cabi_realloc_array(&mut func, len_local, 1, 1, cabi_realloc);
        func.instruction(&Instruction::LocalSet(data_ptr_local));
        // idx = 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(idx_local));
        // while idx < len { mem8[data_ptr + idx] = array.get_u(arr, idx); idx++ }
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));
        // address: data_ptr + idx
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Add);
        // byte: array.get_u(arr, idx)
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::ArrayGetU(arr_type_idx));
        // i32.store8
        func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
        // idx++
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // loop
        func.instruction(&Instruction::End); // block
        // return (data_ptr, len)
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// strings-to-GC: un-materialize a canonical `(ptr, len)` into a
    /// `$str_bytes` GC byte array. Byte-for-byte copy loop. Inverse of
    /// `generate_str_bytes_materializer`.
    pub(super) fn generate_str_bytes_unmaterializer(
        &mut self,
        arr_type_idx: u32,
    ) -> Result<Function, CodegenError> {
        // Params: 0 = ptr, 1 = len. Locals: 2 = arr, 3 = idx.
        let mut func = Function::new([
            (
                1,
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                }),
            ), // arr
            (1, ValType::I32), // idx
        ]);
        let ptr_local: u32 = 0;
        let len_local: u32 = 1;
        let arr_local: u32 = 2;
        let idx_local: u32 = 3;
        // arr = array.new_default $str_bytes (len)
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::ArrayNewDefault(arr_type_idx));
        func.instruction(&Instruction::LocalSet(arr_local));
        // idx = 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(idx_local));
        // while idx < len { arr.set(idx, mem8u[ptr + idx]); idx++ }
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));
        // arr.set(idx, load8u(ptr + idx))
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::I32Load8U(super::scratch::mem_arg(0, 0)));
        func.instruction(&Instruction::ArraySet(arr_type_idx));
        // idx++
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // loop
        func.instruction(&Instruction::End); // block
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// strings-to-GC: materializer for a `list<string>` whose array element
    /// is a `$str_bytes` ref. Per element: materialize the inner byte array
    /// to `(ptr, len)` and store the canonical 8-byte (ptr, len) slot.
    pub(super) fn generate_gc_list_string_materializer(
        &mut self,
        arr_type_idx: u32,
    ) -> Result<Function, CodegenError> {
        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("list<string> materializer requires cabi_realloc".into())
            })?
            .cabi_realloc;
        let mut func = Function::new([
            (1, ValType::I32), // len
            (1, ValType::I32), // data_ptr
            (1, ValType::I32), // idx
            (1, ValType::I32), // elem_addr
            (1, ValType::I32), // inner_ptr
            (1, ValType::I32), // inner_len
        ]);
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        let elem_addr_local: u32 = 4;
        let inner_ptr_local: u32 = 5;
        let inner_len_local: u32 = 6;
        // len = array.len(arr)
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(len_local));
        // data_ptr = cabi_realloc(0, 0, 4, len * 8)
        super::scratch::emit_cabi_realloc_array(&mut func, len_local, 8, 4, cabi_realloc);
        func.instruction(&Instruction::LocalSet(data_ptr_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));
        // elem_addr = data_ptr + idx * 8
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(8));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(elem_addr_local));
        // (inner_ptr, inner_len) = str_bytes_materialize(arr[idx])
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::ArrayGet(arr_type_idx));
        self.emit_str_bytes_materialize(&mut func)?;
        func.instruction(&Instruction::LocalSet(inner_len_local));
        func.instruction(&Instruction::LocalSet(inner_ptr_local));
        // store ptr @ elem_addr+0
        func.instruction(&Instruction::LocalGet(elem_addr_local));
        func.instruction(&Instruction::LocalGet(inner_ptr_local));
        func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
        // store len @ elem_addr+4
        func.instruction(&Instruction::LocalGet(elem_addr_local));
        func.instruction(&Instruction::I32Const(4));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalGet(inner_len_local));
        func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
        // idx++
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // loop
        func.instruction(&Instruction::End); // block
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// If `elem_ty` is an `option<inner>` that collapses to a single nullable
    /// ref (mirroring `internal_repr`'s option-of-ref collapse), return the
    /// collapsed `inner` Ty; else `None`. `option<string>` does NOT collapse.
    pub(super) fn elem_option_collapses(&self, elem_ty: Ty) -> Option<Ty> {
        let inner = match self.ctx.ty_kind(elem_ty) {
            InternedTyKind::Option(i) => *i,
            _ => return None,
        };
        if matches!(self.ctx.ty_kind(inner), InternedTyKind::String) {
            return None;
        }
        match self.internal_repr(inner) {
            super::super::repr::InternalRepr::GcRef(_)
            | super::super::repr::InternalRepr::GcArrayRef(_) => Some(inner),
            _ => None,
        }
    }

    /// Materializer for a `list<option<inner>>` whose element is the collapsed
    /// inner ref (none = null, some = the ref). Per element writes the
    /// canonical `(disc, payload)` bytes: `disc = ref.is_null` (0 = some,
    /// 1 = none); when some, the inner (record / tuple / scalar-list) is
    /// lifted to the payload region.
    pub(super) fn generate_gc_list_collapsed_option_materializer(
        &mut self,
        arr_type_idx: u32,
        elem_ty: Ty,
        inner_ty: Ty,
    ) -> Result<Function, CodegenError> {
        use wasm_encoder::HeapType;
        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("collapsed-option materializer requires cabi_realloc".into())
            })?
            .cabi_realloc;
        let (elem_size, elem_align) =
            gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
        let canonical_slots = self.flatten_core_slots(elem_ty);
        let disc_off = canonical_slots
            .first()
            .map(|s| s.offset)
            .ok_or_else(|| CodegenError::InvalidIR("collapsed-option mat: no disc slot".into()))?;
        let payload_off = canonical_slots
            .get(1)
            .map(|s| s.offset)
            .ok_or_else(|| CodegenError::InvalidIR("collapsed-option mat: no payload slot".into()))?;
        let mut func = Function::new([
            (1, ValType::I32),                        // 1 = len
            (1, ValType::I32),                        // 2 = data_ptr
            (1, ValType::I32),                        // 3 = idx
            (1, ValType::I32),                        // 4 = elem_addr
            (1, ValType::Ref(wasm_encoder::RefType {  // 5 = elem_ref (anyref)
                nullable: true,
                heap_type: HeapType::Abstract { shared: false, ty: wasm_encoder::AbstractHeapType::Any },
            })),
            (1, ValType::I32), // 6 = mat_ptr
            (1, ValType::I32), // 7 = mat_len
        ]);
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        let elem_addr_local: u32 = 4;
        let elem_ref_local: u32 = 5;
        let mat_ptr_local: u32 = 6;
        let mat_len_local: u32 = 7;
        // len = array.len(arr)
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(len_local));
        // data_ptr = cabi_realloc(0, 0, align, len * elem_size)
        super::scratch::emit_cabi_realloc_array(&mut func, len_local, elem_size, elem_align, cabi_realloc);
        func.instruction(&Instruction::LocalSet(data_ptr_local));
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));
        // elem_addr = data_ptr + idx * elem_size
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(elem_addr_local));
        // elem_ref = arr[idx]
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::ArrayGet(arr_type_idx));
        func.instruction(&Instruction::LocalSet(elem_ref_local));
        // disc = ref.is_null(elem_ref)  (0 = some, 1 = none)
        func.instruction(&Instruction::LocalGet(elem_addr_local));
        if disc_off != 0 {
            func.instruction(&Instruction::I32Const(disc_off as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::LocalGet(elem_ref_local));
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
        // if some (ref not null), lift the inner payload.
        func.instruction(&Instruction::LocalGet(elem_ref_local));
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        self.emit_collapsed_inner_lift(
            &mut func,
            inner_ty,
            elem_ref_local,
            elem_addr_local,
            payload_off,
            &canonical_slots,
            mat_ptr_local,
            mat_len_local,
        )?;
        func.instruction(&Instruction::End); // if
        // idx++
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // loop
        func.instruction(&Instruction::End); // block
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Lift a collapsed-option inner value (record / tuple / scalar-list),
    /// whose non-null ref is held in `inner_ref_local` (anyref), into the
    /// canonical payload region at `base_addr_local + payload_off`.
    #[allow(clippy::too_many_arguments)]
    fn emit_collapsed_inner_lift(
        &mut self,
        func: &mut Function,
        inner_ty: Ty,
        inner_ref_local: u32,
        base_addr_local: u32,
        payload_off: u32,
        canonical_slots: &[crate::wasm::FlatSlot],
        mat_ptr_local: u32,
        mat_len_local: u32,
    ) -> Result<(), CodegenError> {
        use wasm_encoder::HeapType;
        match self.ctx.ty_kind(inner_ty) {
            InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) => {
                let record_def_id = *d;
                self.emit_inline_record_lift_from_anyref(
                    func,
                    record_def_id,
                    inner_ref_local,
                    base_addr_local,
                    payload_off,
                    mat_ptr_local,
                    mat_len_local,
                )
            }
            InternedTyKind::Tuple(tuple_elems) => {
                let elems: Vec<Ty> = tuple_elems.to_vec();
                let tup_idx = self
                    .record_gc_types
                    .tuple_struct_type_idx
                    .get(&inner_ty)
                    .copied()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("collapsed-option tuple lift: missing tuple idx".into())
                    })?;
                let mut offset: u32 = 0;
                for (i, &e_ty) in elems.iter().enumerate() {
                    let el = self.layout_ctx.layout_of(e_ty);
                    offset = (offset + el.align - 1) & !(el.align - 1);
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    let abs = payload_off + offset;
                    if abs != 0 {
                        func.instruction(&Instruction::I32Const(abs as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(inner_ref_local));
                    func.instruction(&Instruction::RefCastNonNull(HeapType::Concrete(tup_idx)));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: tup_idx,
                        field_index: i as u32,
                    });
                    self.emit_typed_field_store(func, e_ty);
                    offset += el.size;
                }
                Ok(())
            }
            InternedTyKind::List(_) => {
                // scalar-list inner: materialize the inner array to (ptr,len).
                let arr_idx = *self
                    .record_gc_types
                    .list_array_type_idx
                    .get(&inner_ty)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("collapsed-option list lift: missing inner arr".into())
                    })?;
                let mat_fn = *self
                    .gc_list_materializer_fn_indices
                    .get(&arr_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("collapsed-option list lift: missing inner mat".into())
                    })?;
                let ptr_slot = *canonical_slots.get(1).ok_or_else(|| {
                    CodegenError::InvalidIR("collapsed-option list lift: no ptr slot".into())
                })?;
                let len_slot = *canonical_slots.get(2).ok_or_else(|| {
                    CodegenError::InvalidIR("collapsed-option list lift: no len slot".into())
                })?;
                func.instruction(&Instruction::LocalGet(inner_ref_local));
                func.instruction(&Instruction::RefCastNonNull(HeapType::Concrete(arr_idx)));
                func.instruction(&Instruction::Call(mat_fn));
                func.instruction(&Instruction::LocalSet(mat_len_local));
                func.instruction(&Instruction::LocalSet(mat_ptr_local));
                func.instruction(&Instruction::LocalGet(base_addr_local));
                if ptr_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(mat_ptr_local));
                func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                func.instruction(&Instruction::LocalGet(base_addr_local));
                if len_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(mat_len_local));
                func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                Ok(())
            }
            other => Err(CodegenError::InvalidIR(format!(
                "collapsed-option inner lift: unsupported inner ty {:?}",
                other
            ))),
        }
    }

    /// Finish the un-materializer for a `list<option<inner>>` collapsed-ref
    /// element: for each canonical `(disc, payload)`, a `some` (disc == 0)
    /// rebuilds the inner ref and `array.set`s it; a `none` leaves the
    /// default null ref.
    #[allow(clippy::too_many_arguments)]
    fn finish_gc_list_collapsed_option_unmaterializer(
        &mut self,
        mut func: Function,
        arr_type_idx: u32,
        elem_ty: Ty,
        inner_ty: Ty,
        ptr_local: u32,
        len_local: u32,
        arr_local: u32,
        idx_local: u32,
        elem_addr_local: u32,
    ) -> Result<Function, CodegenError> {
        let elem_size = self.layout_ctx.layout_of(elem_ty).size;
        let canonical_slots = self.flatten_core_slots(elem_ty);
        let disc_off = canonical_slots
            .first()
            .map(|s| s.offset)
            .ok_or_else(|| CodegenError::InvalidIR("collapsed-option unmat: no disc slot".into()))?;
        let payload_off = canonical_slots
            .get(1)
            .map(|s| s.offset)
            .ok_or_else(|| CodegenError::InvalidIR("collapsed-option unmat: no payload slot".into()))?;
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));
        // elem_addr = ptr + idx * elem_size
        func.instruction(&Instruction::LocalGet(ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(elem_addr_local));
        // disc = mem8[elem_addr + disc_off]; if disc == 0 (some) build inner.
        func.instruction(&Instruction::LocalGet(elem_addr_local));
        if disc_off != 0 {
            func.instruction(&Instruction::I32Const(disc_off as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::I32Load8U(super::scratch::mem_arg(0, 0)));
        func.instruction(&Instruction::I32Eqz);
        func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        self.emit_collapsed_inner_pack(
            &mut func,
            inner_ty,
            elem_addr_local,
            payload_off,
            &canonical_slots,
        )?;
        func.instruction(&Instruction::ArraySet(arr_type_idx));
        func.instruction(&Instruction::End); // if
        // idx++
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // loop
        func.instruction(&Instruction::End); // block
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Build a collapsed-option inner ref (record / tuple / scalar-list) from
    /// its canonical payload bytes at `base_addr_local + payload_off`,
    /// leaving the inner ref on the stack.
    fn emit_collapsed_inner_pack(
        &mut self,
        func: &mut Function,
        inner_ty: Ty,
        base_addr_local: u32,
        payload_off: u32,
        canonical_slots: &[crate::wasm::FlatSlot],
    ) -> Result<(), CodegenError> {
        match self.ctx.ty_kind(inner_ty) {
            InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) => {
                let record_def_id = *d;
                self.emit_record_pack_from_memory(func, record_def_id, base_addr_local, payload_off)
            }
            InternedTyKind::Tuple(tuple_elems) => {
                let elems: Vec<Ty> = tuple_elems.to_vec();
                let tup_idx = self
                    .record_gc_types
                    .tuple_struct_type_idx
                    .get(&inner_ty)
                    .copied()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("collapsed-option tuple pack: missing tuple idx".into())
                    })?;
                let mut offset: u32 = 0;
                for &e_ty in &elems {
                    let el = self.layout_ctx.layout_of(e_ty);
                    offset = (offset + el.align - 1) & !(el.align - 1);
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    let abs = payload_off + offset;
                    if abs != 0 {
                        func.instruction(&Instruction::I32Const(abs as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    self.emit_typed_field_load(func, e_ty);
                    offset += el.size;
                }
                func.instruction(&Instruction::StructNew(tup_idx));
                Ok(())
            }
            InternedTyKind::List(_) => {
                let arr_idx = *self
                    .record_gc_types
                    .list_array_type_idx
                    .get(&inner_ty)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("collapsed-option list pack: missing inner arr".into())
                    })?;
                let unmat_fn = *self
                    .gc_list_unmaterializer_fn_indices
                    .get(&arr_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("collapsed-option list pack: missing inner unmat".into())
                    })?;
                let ptr_slot = *canonical_slots.get(1).ok_or_else(|| {
                    CodegenError::InvalidIR("collapsed-option list pack: no ptr slot".into())
                })?;
                let len_slot = *canonical_slots.get(2).ok_or_else(|| {
                    CodegenError::InvalidIR("collapsed-option list pack: no len slot".into())
                })?;
                func.instruction(&Instruction::LocalGet(base_addr_local));
                if ptr_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                func.instruction(&Instruction::LocalGet(base_addr_local));
                if len_slot.offset != 0 {
                    func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                func.instruction(&Instruction::Call(unmat_fn));
                Ok(())
            }
            other => Err(CodegenError::InvalidIR(format!(
                "collapsed-option inner pack: unsupported inner ty {:?}",
                other
            ))),
        }
    }

    /// non-for-loop expression context (e.g. `.filter()` source, method call).
    pub(super) fn generate_gc_list_materializer(
        &mut self,
        arr_type_idx: u32,
        elem_ty: yel_core::Ty,
    ) -> Result<Function, CodegenError> {
        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("gc_list_materializer requires cabi_realloc".into())
            })?
            .cabi_realloc;
        // Phase 5e.1: for record element types we need a typed copy
        // loop that pulls each field out of the record GC ref and
        // stores it at the canonical-ABI offset in memory. Strings /
        // list<scalar> fields unbox `$fat_value`, primitives use a
        // typed store.
        // Phase 5e.5 Stage 8a: when element is FlatGcStruct, the
        // typed-array stores supertype refs — materialize each via a
        // per-case ref.test cascade. Generated below in a dedicated
        // branch; here we only handle string / legacy-option-fat-box.
        let elem_is_flat_gc = matches!(
            self.internal_repr(elem_ty),
            super::super::repr::InternalRepr::FlatGcStruct(_)
        );
        let elem_is_string = matches!(
            self.ctx.ty_kind(elem_ty),
            yel_core::types::InternedTyKind::String
        ) || (!elem_is_flat_gc
            && matches!(
                self.ctx.ty_kind(elem_ty),
                yel_core::types::InternedTyKind::Option(_)
            )
            && {
                let canonical = self.canonical_flat_valtypes(elem_ty);
                canonical.len() == 2 && canonical.iter().all(|vt| matches!(vt, ValType::I32))
            });
        // strings-to-GC: a `list<string>` whose element is a `$str_bytes`
        // ref. Per element: materialize the inner byte array to (ptr, len)
        // and write the canonical 8-byte (ptr, len) slot.
        if matches!(self.ctx.ty_kind(elem_ty), yel_core::types::InternedTyKind::String)
        {
            return self.generate_gc_list_string_materializer(arr_type_idx);
        }
        if elem_is_string {
            // A `list<string>` element is a `$str_bytes` GC ref handled
            // by `generate_gc_list_string_materializer` above; a legacy
            // fat-box `option<scalar>` element no longer boxes into
            // `$fat_value` either. This branch is unreachable.
            unreachable!(
                "list<string> materializer: element boxed into $fat_value — strings are \
                 $str_bytes GC refs and no value boxes into $fat_value"
            );
        }
        // Phase 5e.6: nested-list element — each elem is itself a typed
        // GC array ref. Recursively call its materializer to produce
        // (inner_ptr, inner_len), then store the pair at the canonical
        // 8-byte slot.
        if matches!(
            self.ctx.ty_kind(elem_ty),
            yel_core::types::InternedTyKind::List(_)
        ) {
            let inner_arr_idx = self
                .record_gc_types
                .list_array_type_idx
                .get(&elem_ty)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "gc_list_materializer: nested list element has no typed array idx".into(),
                    )
                })?;
            let inner_mat_fn = self
                .gc_list_materializer_fn_indices
                .get(&inner_arr_idx)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "gc_list_materializer: missing inner materializer for arr_type_idx={}",
                        inner_arr_idx
                    ))
                })?;
            let elem_size: u32 = 8; // canonical (ptr, len)
            let elem_align: u32 = 4;
            let mut func = Function::new([
                (1, ValType::I32), // len
                (1, ValType::I32), // data_ptr
                (1, ValType::I32), // idx
                (1, ValType::I32), // elem_addr
                (1, ValType::I32), // inner_ptr scratch
                (1, ValType::I32), // inner_len scratch
            ]);
            let arr_local: u32 = 0;
            let len_local: u32 = 1;
            let data_ptr_local: u32 = 2;
            let idx_local: u32 = 3;
            let elem_addr_local: u32 = 4;
            let inner_ptr_local: u32 = 5;
            let inner_len_local: u32 = 6;
            // len = array.len(arr)
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::ArrayLen);
            func.instruction(&wasm_encoder::Instruction::LocalSet(len_local));
            // data_ptr = cabi_realloc(0, 0, 4, len * 8)
            super::scratch::emit_cabi_realloc_array(&mut func, len_local, elem_size, elem_align, cabi_realloc);
            func.instruction(&wasm_encoder::Instruction::LocalSet(data_ptr_local));
            // idx = 0
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Block(
                wasm_encoder::BlockType::Empty,
            ));
            func.instruction(&wasm_encoder::Instruction::Loop(
                wasm_encoder::BlockType::Empty,
            ));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::I32GeU);
            func.instruction(&wasm_encoder::Instruction::BrIf(1));
            // elem_addr = data_ptr + idx * 8
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
            func.instruction(&wasm_encoder::Instruction::I32Mul);
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(elem_addr_local));
            // (inner_ptr, inner_len) = $inner_mat(arr.get(idx))
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
            func.instruction(&wasm_encoder::Instruction::Call(inner_mat_fn));
            func.instruction(&wasm_encoder::Instruction::LocalSet(inner_len_local));
            func.instruction(&wasm_encoder::Instruction::LocalSet(inner_ptr_local));
            // *elem_addr = inner_ptr
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(inner_ptr_local));
            func.instruction(&wasm_encoder::Instruction::I32Store(
                super::scratch::mem_arg(0, 2),
            ));
            // *(elem_addr + 4) = inner_len
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(4));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalGet(inner_len_local));
            func.instruction(&wasm_encoder::Instruction::I32Store(
                super::scratch::mem_arg(0, 2),
            ));
            // idx++
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(1));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Br(0));
            func.instruction(&wasm_encoder::Instruction::End);
            func.instruction(&wasm_encoder::Instruction::End);
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::End);
            return Ok(func);
        }
        // Phase 5e.5 Stage 8a: FlatGcStruct element — materialize each
        // ref to canonical bytes via a per-case `ref.test` cascade.
        if elem_is_flat_gc {
            return self.generate_gc_list_materializer_flat_gc(arr_type_idx, elem_ty);
        }
        // Option-of-ref collapse element (`option<record|tuple|scalar-list|
        // collapsing-option>`): the array stores the collapsed inner ref
        // (none = null, some(v) = v). Materialize each to canonical
        // `(disc, payload)`.
        if let Some(inner_ty) = self.elem_option_collapses(elem_ty) {
            return self.generate_gc_list_collapsed_option_materializer(
                arr_type_idx,
                elem_ty,
                inner_ty,
            );
        }
        let elem_record_def: Option<DefId> = match self.ctx.ty_kind(elem_ty) {
            InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) => {
                Some(*d)
            }
            _ => None,
        };
        if let Some(record_def_id) = elem_record_def {
            let (elem_size, elem_align) =
                gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
            // Locals (param 0 = arr_ref):
            //   1 = len, 2 = data_ptr, 3 = idx, 4 = elem_addr,
            //   5 = elem_ref (typed record ref)
            let record_type_idx = self
                .record_gc_types
                .record_type_idx
                .get(&record_def_id)
                .copied()
                .ok_or_else(|| {
                    CodegenError::InvalidIR("gc_list_materializer: missing record_type_idx".into())
                })?;
            let mut func = Function::new([
                (1, ValType::I32), // len
                (1, ValType::I32), // data_ptr
                (1, ValType::I32), // idx
                (1, ValType::I32), // elem_addr
                (
                    1,
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(record_type_idx),
                    }),
                ),
                (1, ValType::I32), // mat_ptr (5e.6 typed-array list field)
                (1, ValType::I32), // mat_len
            ]);
            let arr_local: u32 = 0;
            let len_local: u32 = 1;
            let data_ptr_local: u32 = 2;
            let idx_local: u32 = 3;
            let elem_addr_local: u32 = 4;
            let elem_ref_local: u32 = 5;
            let mat_ptr_local: u32 = 6;
            let mat_len_local: u32 = 7;
            // len = array.len(arr)
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::ArrayLen);
            func.instruction(&wasm_encoder::Instruction::LocalSet(len_local));
            // data_ptr = cabi_realloc(0, 0, elem_align, len * elem_size)
            super::scratch::emit_cabi_realloc_array(&mut func, len_local, elem_size, elem_align, cabi_realloc);
            func.instruction(&wasm_encoder::Instruction::LocalSet(data_ptr_local));
            // idx = 0
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Block(
                wasm_encoder::BlockType::Empty,
            ));
            func.instruction(&wasm_encoder::Instruction::Loop(
                wasm_encoder::BlockType::Empty,
            ));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::I32GeU);
            func.instruction(&wasm_encoder::Instruction::BrIf(1));
            // elem_addr = data_ptr + idx * elem_size
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
            func.instruction(&wasm_encoder::Instruction::I32Mul);
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(elem_addr_local));
            // elem_ref = arr[idx]
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
            func.instruction(&wasm_encoder::Instruction::LocalSet(elem_ref_local));
            // Lift fields → memory at elem_addr.
            self.emit_record_lift_to_memory(
                &mut func,
                record_def_id,
                elem_ref_local,
                elem_addr_local,
                0,
                Some((mat_ptr_local, mat_len_local)),
            )?;
            // idx++
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(1));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Br(0));
            func.instruction(&wasm_encoder::Instruction::End); // loop
            func.instruction(&wasm_encoder::Instruction::End); // block
            // return (data_ptr, len)
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::End);
            return Ok(func);
        }
        let (elem_size, elem_align) =
            gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
        // Locals (param 0 = arr_ref):
        //   1 = len (i32)
        //   2 = data_ptr (i32)
        //   3 = idx (i32)
        let arr_ref_valtype = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
        });
        let mut func = Function::new([
            (1, ValType::I32), // len
            (1, ValType::I32), // data_ptr
            (1, ValType::I32), // idx
        ]);
        // The function's parameter 0 is the arr_ref (already a param, not a local).
        // WASM local indices: 0 = arr_ref (param), 1 = len, 2 = data_ptr, 3 = idx
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        // len = array.len(arr)
        func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
        func.instruction(&wasm_encoder::Instruction::ArrayLen);
        func.instruction(&wasm_encoder::Instruction::LocalSet(len_local));
        // data_ptr = cabi_realloc(0, 0, elem_align, len * elem_size)
        super::scratch::emit_cabi_realloc_array(&mut func, len_local, elem_size, elem_align, cabi_realloc);
        func.instruction(&wasm_encoder::Instruction::LocalSet(data_ptr_local));
        // idx = 0
        func.instruction(&wasm_encoder::Instruction::I32Const(0));
        func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
        // Copy loop: while idx < len { mem[data_ptr + idx*sz] = arr[idx]; idx++ }
        func.instruction(&wasm_encoder::Instruction::Block(
            wasm_encoder::BlockType::Empty,
        ));
        func.instruction(&wasm_encoder::Instruction::Loop(
            wasm_encoder::BlockType::Empty,
        ));
        func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
        func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
        func.instruction(&wasm_encoder::Instruction::I32GeU);
        func.instruction(&wasm_encoder::Instruction::BrIf(1));
        // destination: data_ptr + idx * elem_size
        func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
        func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
        func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
        func.instruction(&wasm_encoder::Instruction::I32Mul);
        func.instruction(&wasm_encoder::Instruction::I32Add);
        // array.get element
        func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
        func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
        emit_gc_array_get(&mut func, self.ctx, elem_ty, arr_type_idx);
        // store to memory
        emit_gc_list_elem_store(&mut func, self.ctx, elem_ty);
        // idx++
        func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
        func.instruction(&wasm_encoder::Instruction::I32Const(1));
        func.instruction(&wasm_encoder::Instruction::I32Add);
        func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
        func.instruction(&wasm_encoder::Instruction::Br(0));
        func.instruction(&wasm_encoder::Instruction::End); // loop
        func.instruction(&wasm_encoder::Instruction::End); // block
        // return (data_ptr, len)
        func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
        func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
        func.instruction(&wasm_encoder::Instruction::End);
        let _ = arr_ref_valtype; // only used to document param type
        Ok(func)
    }

    /// Phase 4: emit per-flat-slot stores for a DTR record (possibly
    /// containing nested DTR records). Each store sources its value by
    /// walking from `self` through `prefix` (a chain of `(struct_type,
    /// field_idx)`) into the outer record's GC ref, then through the
    /// record's GC fields. For string / list<scalar> fields the
    /// `$fat_value` box is unwrapped per-slot.
    ///
    /// `record_def_id` is the record at the *end* of the prefix chain
    /// (the outermost call passes the signal's record def + prefix
    /// `[(comp_struct, comp_field)]`). `base_offset` is the byte offset
    /// of this record's contents within the canonical-ABI scratch.
    /// Phase 5e.5 Stage 8a: materializer for `list<FlatGcStruct>`
    /// — for each `(ref null $sup)` element, write canonical bytes
    /// (disc + payload) at `data_ptr + idx * elem_size` via a
    /// per-case ref.test cascade. Currently restricted to scalar /
    /// fat-box payloads with no width promotion.
    fn generate_gc_list_materializer_flat_gc(
        &mut self,
        arr_type_idx: u32,
        elem_ty: yel_core::Ty,
    ) -> Result<Function, CodegenError> {
        use super::super::gc_types::{
            StructGetVariant, case_payload_ty, struct_get_op_for_payload,
        };
        use yel_core::types::InternedTyKind;

        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR(
                    "gc_list_materializer (flat-gc): cabi_realloc missing".into(),
                )
            })?
            .cabi_realloc;
        let layout_info = self.layout_ctx.layout_of(elem_ty);
        let elem_size = layout_info.size;
        let elem_align = layout_info.align;
        let canonical_slots = self.flatten_core_slots(elem_ty);
        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&elem_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "list materializer (flat-gc): missing case count for {:?}",
                    elem_ty
                ))
            })?;

        let arr_ref_valtype = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
        });
        let _ = arr_ref_valtype;
        let mut func = Function::new([
            (1, ValType::I32), // len (local 1)
            (1, ValType::I32), // data_ptr (local 2)
            (1, ValType::I32), // idx (local 3)
            (1, ValType::I32), // elem_addr (local 4)
            (1, ValType::I32), // str_bytes mat ptr (local 5)
            (1, ValType::I32), // str_bytes mat len (local 6)
        ]);
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        let elem_addr_local: u32 = 4;
        let mat_ptr_local: u32 = 5;
        let mat_len_local: u32 = 6;

        // len = array.len(arr)
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::ArrayLen);
        func.instruction(&Instruction::LocalSet(len_local));
        // data_ptr = cabi_realloc(0, 0, elem_align, len * elem_size)
        super::scratch::emit_cabi_realloc_array(&mut func, len_local, elem_size, elem_align, cabi_realloc);
        func.instruction(&Instruction::LocalSet(data_ptr_local));
        // idx = 0
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::I32GeU);
        func.instruction(&Instruction::BrIf(1));
        // elem_addr = data_ptr + idx * elem_size
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(elem_addr_local));

        // Per-case ref.test cascade. Outer block lets a matching arm
        // skip the rest + the fall-through default.
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(elem_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "list materializer (flat-gc): missing case_idx for ({:?}, {})",
                        elem_ty, k
                    ))
                })?;
            // arr.get(idx); ref.test $case_sub
            func.instruction(&Instruction::LocalGet(arr_local));
            func.instruction(&Instruction::LocalGet(idx_local));
            func.instruction(&Instruction::ArrayGet(arr_type_idx));
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            // disc = k at elem_addr + canonical_slots[0].offset
            let disc_off = canonical_slots.first().map(|s| s.offset).unwrap_or(0) as i32;
            func.instruction(&Instruction::LocalGet(elem_addr_local));
            if disc_off != 0 {
                func.instruction(&Instruction::I32Const(disc_off));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
            // Payload write.
            if let Some(payload_ty) = case_payload_ty(self.ctx, elem_ty, k) {
                // strings-to-GC: string payload field is a $str_bytes ref —
                // materialize once to (ptr, len) and store both canonical slots.
                if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
                {
                    let arr_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                        CodegenError::InvalidIR("list mat (flat-gc) string: $str_bytes missing".into())
                    })?;
                    let str_mat_fn = *self
                        .gc_list_materializer_fn_indices
                        .get(&arr_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR("list mat (flat-gc) string: missing mat".into())
                        })?;
                    let ptr_slot = *canonical_slots.get(1).ok_or_else(|| {
                        CodegenError::InvalidIR("list mat (flat-gc) string: missing ptr slot".into())
                    })?;
                    let len_slot = *canonical_slots.get(2).ok_or_else(|| {
                        CodegenError::InvalidIR("list mat (flat-gc) string: missing len slot".into())
                    })?;
                    func.instruction(&Instruction::LocalGet(arr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&Instruction::RefCastNonNull(
                        wasm_encoder::HeapType::Concrete(case_sub_idx),
                    ));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: case_sub_idx,
                        field_index: 0,
                    });
                    func.instruction(&Instruction::Call(str_mat_fn));
                    func.instruction(&Instruction::LocalSet(mat_len_local));
                    func.instruction(&Instruction::LocalSet(mat_ptr_local));
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    if ptr_slot.offset != 0 {
                        func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(mat_ptr_local));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    if len_slot.offset != 0 {
                        func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(mat_len_local));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                } else {
                    // Scalar payload — single canonical slot.
                    let payload_slot = canonical_slots.get(1).ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "list materializer (flat-gc): missing payload slot".into(),
                        )
                    })?;
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    if payload_slot.offset != 0 {
                        func.instruction(&Instruction::I32Const(payload_slot.offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(arr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&Instruction::RefCastNonNull(
                        wasm_encoder::HeapType::Concrete(case_sub_idx),
                    ));
                    let getter = struct_get_op_for_payload(self.ctx, payload_ty);
                    match getter {
                        StructGetVariant::Plain => {
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: case_sub_idx,
                                field_index: 0,
                            });
                        }
                        StructGetVariant::Signed => {
                            func.instruction(&Instruction::StructGetS {
                                struct_type_index: case_sub_idx,
                                field_index: 0,
                            });
                        }
                        StructGetVariant::Unsigned => {
                            func.instruction(&Instruction::StructGetU {
                                struct_type_index: case_sub_idx,
                                field_index: 0,
                            });
                        }
                    }
                    self.emit_typed_field_store(&mut func, payload_ty);
                }
            }
            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
        }
        // Default (null ref): disc = 0.
        let disc_off = canonical_slots.first().map(|s| s.offset).unwrap_or(0) as i32;
        func.instruction(&Instruction::LocalGet(elem_addr_local));
        if disc_off != 0 {
            func.instruction(&Instruction::I32Const(disc_off));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
        func.instruction(&Instruction::End); // outer per-elem block

        // idx++
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(1));
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(idx_local));
        func.instruction(&Instruction::Br(0));
        func.instruction(&Instruction::End); // loop
        func.instruction(&Instruction::End); // block
        // Return (data_ptr, len)
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Phase 5e.5: lift a FlatGcStruct field of a DTR record into
    /// canonical-ABI scratch. Reads the field ref via the
    /// `<self>.<prefix-chain>.<final struct.get>` pattern (matching
    /// the rest of `emit_getter_lift_dtr_record`'s read shape) and
    /// dispatches on case via `ref.test`.
    fn emit_flat_gc_dtr_field_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        field_ty: Ty,
        record_type_idx: u32,
        gc_field_idx: u32,
        prefix: &[(u32, u32)],
        abs_field_offset: u32,
        scratch_ptr_local: u32,
    ) -> Result<(), CodegenError> {
        use super::scratch::mem_arg;
        let canonical_slots = self.flatten_core_slots(field_ty);
        let disc_offset = canonical_slots.first().map(|s| s.offset).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "FlatGcStruct DTR field lift: empty canonical layout for {:?}",
                field_ty
            ))
        })?;
        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&field_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct DTR field lift: missing case count for {:?}",
                    field_ty
                ))
            })?;

        // Re-emits `<self>.<prefix>.struct.get $rec field` (leaves a
        // (ref null $<sup>) on the stack) across multiple ref.tests — the
        // shared `emit_gc_field_chain` on the full path to this field.
        let field_chain: Vec<(u32, u32)> = prefix
            .iter()
            .copied()
            .chain(std::iter::once((record_type_idx, gc_field_idx)))
            .collect();
        let emit_field_ref = |this: &Self, func: &mut Function| -> Result<(), CodegenError> {
            this.emit_gc_field_chain(func, ci, &field_chain)
        };

        // Outer block lets a matching case skip remaining tests +
        // fall-through default.
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));

        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(field_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "FlatGcStruct DTR field lift: missing case_idx for ({:?}, {})",
                        field_ty, k
                    ))
                })?;

            emit_field_ref(self, func)?;
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // disc = k
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            let disc_abs = abs_field_offset + disc_offset;
            if disc_abs != 0 {
                func.instruction(&Instruction::I32Const(disc_abs as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

            if let Some(payload_ty) = super::super::gc_types::case_payload_ty(self.ctx, field_ty, k)
            {
                use super::super::gc_types::StructGetVariant;
                use yel_core::types::InternedTyKind;

                // Typed-GC-array list payload OR string payload — both are a
                // single GC array ref (`$<elem>_list` / `$str_bytes`) in the
                // case subtype; materialize once to canonical (ptr, len) and
                // store both slots. (Parity with the signal payload lift; the
                // field path previously handled only the string sub-case and
                // stored a list ref as a raw i32.)
                let payload_arr_idx: Option<u32> =
                    if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_)) {
                        self.record_gc_types.list_array_type_idx.get(&payload_ty).copied()
                    } else if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String) {
                        self.record_gc_types.str_bytes_array_idx
                    } else {
                        None
                    };
                if let Some(arr_type_idx) = payload_arr_idx {
                    let mat_fn = *self
                        .gc_list_materializer_fn_indices
                        .get(&arr_type_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "FlatGcStruct DTR array field lift: missing materializer".into(),
                            )
                        })?;
                    let ptr_slot = *canonical_slots.get(1).ok_or_else(|| {
                        CodegenError::InvalidIR("DTR array field lift: missing ptr slot".into())
                    })?;
                    let len_slot = *canonical_slots.get(2).ok_or_else(|| {
                        CodegenError::InvalidIR("DTR array field lift: missing len slot".into())
                    })?;
                    let mat_ptr_local = scratch_ptr_local + 1;
                    let mat_len_local = scratch_ptr_local + 2;
                    emit_field_ref(self, func)?;
                    func.instruction(&Instruction::RefCastNonNull(
                        wasm_encoder::HeapType::Concrete(case_sub_idx),
                    ));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: case_sub_idx,
                        field_index: 0,
                    });
                    func.instruction(&Instruction::Call(mat_fn));
                    func.instruction(&Instruction::LocalSet(mat_len_local));
                    func.instruction(&Instruction::LocalSet(mat_ptr_local));
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    let abs = abs_field_offset + ptr_slot.offset;
                    if abs != 0 {
                        func.instruction(&Instruction::I32Const(abs as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(mat_ptr_local));
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    let abs = abs_field_offset + len_slot.offset;
                    if abs != 0 {
                        func.instruction(&Instruction::I32Const(abs as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(mat_len_local));
                    func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                } else {
                    // Single-slot scalar payload.
                    let payload_slot = canonical_slots.get(1).ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "FlatGcStruct DTR field lift: missing payload slot".into(),
                        )
                    })?;
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    let abs = abs_field_offset + payload_slot.offset;
                    if abs != 0 {
                        func.instruction(&Instruction::I32Const(abs as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    emit_field_ref(self, func)?;
                    func.instruction(&Instruction::RefCastNonNull(
                        wasm_encoder::HeapType::Concrete(case_sub_idx),
                    ));
                    let getter =
                        super::super::gc_types::struct_get_op_for_payload(self.ctx, payload_ty);
                    match getter {
                        StructGetVariant::Plain => {
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: case_sub_idx,
                                field_index: 0,
                            });
                        }
                        StructGetVariant::Signed => {
                            func.instruction(&Instruction::StructGetS {
                                struct_type_index: case_sub_idx,
                                field_index: 0,
                            });
                        }
                        StructGetVariant::Unsigned => {
                            func.instruction(&Instruction::StructGetU {
                                struct_type_index: case_sub_idx,
                                field_index: 0,
                            });
                        }
                    }
                    self.emit_typed_field_store(func, payload_ty);
                }
            }

            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
        }

        // Default: write disc=0 (legacy zero-byte parity).
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        let disc_abs = abs_field_offset + disc_offset;
        if disc_abs != 0 {
            func.instruction(&Instruction::I32Const(disc_abs as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

        // End outer block.
        func.instruction(&Instruction::End);
        Ok(())
    }

    /// Emit `self_ref` then the `struct.get` chain in `chain`, leaving the
    /// value of the final field on the stack. `chain` is the full path from
    /// the component self to the target field — `(struct_type_idx,
    /// gc_field_idx)` per hop — and each intermediate ref is `ref.as_non_null`
    /// before the next `struct.get`. The one place the nested-record /
    /// tuple-element ref walk is emitted (see `record_leaf_field_accesses`).
    fn emit_gc_field_chain(
        &self,
        func: &mut Function,
        ci: usize,
        chain: &[(u32, u32)],
    ) -> Result<(), CodegenError> {
        self.emit_self_ref(func, ci)?;
        for (idx, &(s_ty, f_idx)) in chain.iter().enumerate() {
            if idx > 0 {
                func.instruction(&Instruction::RefAsNonNull);
            }
            func.instruction(&Instruction::StructGet {
                struct_type_index: s_ty,
                field_index: f_idx,
            });
        }
        Ok(())
    }

    fn emit_getter_lift_dtr_record(
        &mut self,
        func: &mut Function,
        ci: usize,
        record_def_id: DefId,
        base_offset: u32,
        scratch_ptr_local: u32,
        prefix: &[(u32, u32)],
    ) -> Result<(), CodegenError> {
        // Flatten the record (transparently through nested records) into its
        // leaf fields, each with the struct.get chain that reaches it and its
        // canonical byte offset. The nested-record traversal lives in
        // `record_leaf_field_accesses`; here we only lower each leaf by type.
        let leaves = self.record_leaf_field_accesses(record_def_id)?;
        for leaf in leaves {
            let abs_field_offset = base_offset + leaf.offset;
            // Full path from self to the leaf field = prefix ++ leaf.chain.
            let full_chain: Vec<(u32, u32)> =
                prefix.iter().copied().chain(leaf.chain.iter().copied()).collect();
            let field_ty = leaf.ty;

            // FlatGcStruct leaf (migrated option / result / variant): dispatch
            // on case. Its lift reads the field via the chain-to-parent + the
            // final (parent_struct_ty, gc_field_idx).
            if self.flat_gc_migrated(field_ty) {
                let (&(parent_ty, gc_field_idx), parent_prefix) =
                    full_chain.split_last().ok_or_else(|| {
                        CodegenError::InvalidIR("DTR getter lift: empty leaf chain".into())
                    })?;
                self.emit_flat_gc_dtr_field_lift(
                    func,
                    ci,
                    field_ty,
                    parent_ty,
                    gc_field_idx,
                    parent_prefix,
                    abs_field_offset,
                    scratch_ptr_local,
                )?;
                continue;
            }

            // Tuple leaf: the field is a nested tuple GC struct. Lower it
            // recursively — `full_chain` reaches the tuple ref, and the tuple
            // lift writes each element at its offset within the field.
            if matches!(self.ctx.ty_kind(field_ty), InternedTyKind::Tuple(_)) {
                let mat_ptr_local = scratch_ptr_local + 1;
                let mat_len_local = scratch_ptr_local + 2;
                self.emit_getter_lift_tuple(
                    func,
                    ci,
                    field_ty,
                    abs_field_offset,
                    scratch_ptr_local,
                    mat_ptr_local,
                    mat_len_local,
                    &full_chain,
                )?;
                continue;
            }

            let field_kind = self.ctx.ty_kind(field_ty).clone();
            let field_slots = self.flatten_core_slots(field_ty);

            // Typed-GC-array list leaf: read the array ref, materialize to
            // canonical (ptr, len), store both slots.
            let typed_arr_idx: Option<u32> =
                if matches!(field_kind, yel_core::types::InternedTyKind::List(_)) {
                    self.record_gc_types.list_array_type_idx.get(&field_ty).copied()
                } else {
                    None
                };
            if let Some(arr_idx) = typed_arr_idx {
                let mat_fn = *self
                    .gc_list_materializer_fn_indices
                    .get(&arr_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "DTR getter lift: missing materializer for arr_type_idx {}",
                            arr_idx
                        ))
                    })?;
                self.emit_gc_field_chain(func, ci, &full_chain)?;
                func.instruction(&Instruction::Call(mat_fn));
                self.store_materialized_ptr_len(
                    func,
                    scratch_ptr_local,
                    abs_field_offset,
                    &field_slots,
                );
                continue;
            }

            // strings-to-GC: a string leaf is a `$str_bytes` ref; materialize
            // to canonical (ptr, len).
            if matches!(field_kind, yel_core::types::InternedTyKind::String) {
                self.emit_gc_field_chain(func, ci, &full_chain)?;
                self.emit_str_bytes_materialize(func)?;
                self.store_materialized_ptr_len(
                    func,
                    scratch_ptr_local,
                    abs_field_offset,
                    &field_slots,
                );
                continue;
            }

            // Scalar leaf: one store per flat slot (a scalar is a single
            // slot; the chain is re-read per slot to keep the value typed).
            for slot in field_slots.iter() {
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                let total_off = abs_field_offset + slot.offset;
                if total_off != 0 {
                    func.instruction(&Instruction::I32Const(total_off as i32));
                    func.instruction(&Instruction::I32Add);
                }
                self.emit_gc_field_chain(func, ci, &full_chain)?;
                slot.store.emit_store(func);
            }
        }
        Ok(())
    }

    /// Given a materialized `(ptr, len)` on top of the stack (len on top),
    /// stash them and store ptr / len at their canonical slot offsets
    /// (`field_slots[0]` / `[1]`) relative to `abs_field_offset` in the lift
    /// scratch. Uses the getter's reserved `scratch_ptr+1 / +2` mat locals.
    fn store_materialized_ptr_len(
        &self,
        func: &mut Function,
        scratch_ptr_local: u32,
        abs_field_offset: u32,
        field_slots: &[FlatSlot],
    ) {
        let mat_len_local = scratch_ptr_local + 2;
        let mat_ptr_local = scratch_ptr_local + 1;
        func.instruction(&Instruction::LocalSet(mat_len_local));
        func.instruction(&Instruction::LocalSet(mat_ptr_local));
        let ptr_slot = &field_slots[0];
        let len_slot = &field_slots[1];
        let ptr_off = abs_field_offset + ptr_slot.offset;
        let len_off = abs_field_offset + len_slot.offset;
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if ptr_off != 0 {
            func.instruction(&Instruction::I32Const(ptr_off as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::LocalGet(mat_ptr_local));
        ptr_slot.store.emit_store(func);
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if len_off != 0 {
            func.instruction(&Instruction::I32Const(len_off as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::LocalGet(mat_len_local));
        len_slot.store.emit_store(func);
    }

    /// Lower a tuple GC struct (reached via `prefix`) to its canonical-ABI
    /// memory representation in the lift scratch, writing each element at its
    /// aligned offset from `base_offset`. The tuple twin of
    /// [`Self::emit_getter_lift_dtr_record`]; recurses for nested tuples and
    /// delegates record / option / result elements to the record/flat-gc
    /// lifts. `prefix` is the chain of `(struct_type, field_index)` struct.gets
    /// that reaches THIS tuple's ref from the component self.
    fn emit_getter_lift_tuple(
        &mut self,
        func: &mut Function,
        ci: usize,
        tuple_ty: Ty,
        base_offset: u32,
        scratch_ptr_local: u32,
        mat_ptr_local: u32,
        mat_len_local: u32,
        prefix: &[(u32, u32)],
    ) -> Result<(), CodegenError> {
        let elements: Vec<Ty> = match self.ctx.ty_kind(tuple_ty) {
            InternedTyKind::Tuple(els) => els.to_vec(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "tuple getter lift: not a tuple ty".into(),
                ));
            }
        };
        let tup_idx = self
            .record_gc_types
            .tuple_struct_type_idx
            .get(&tuple_ty)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("tuple getter lift: missing tuple_struct_type_idx".into())
            })?;
        // Emit the struct.get chain that leaves tuple element `i`'s internal
        // value on the stack: self → walk prefix → the tuple ref → element i,
        // via the shared `emit_gc_field_chain` on `prefix ++ [(tup_idx, i)]`.
        let emit_elem =
            |this: &Self, func: &mut Function, i: u32| -> Result<(), CodegenError> {
                let chain: Vec<(u32, u32)> = prefix
                    .iter()
                    .copied()
                    .chain(std::iter::once((tup_idx, i)))
                    .collect();
                this.emit_gc_field_chain(func, ci, &chain)
            };
        let mut offset: u32 = base_offset;
        for (i, &elem_ty) in elements.iter().enumerate() {
            let elem_layout = self.layout_ctx.layout_of(elem_ty);
            let aligned = (offset + elem_layout.align - 1) & !(elem_layout.align - 1);
            offset = aligned;
            // option / result / variant element.
            if self.flat_gc_migrated(elem_ty) {
                self.emit_flat_gc_dtr_field_lift(
                    func,
                    ci,
                    elem_ty,
                    tup_idx,
                    i as u32,
                    prefix,
                    offset,
                    scratch_ptr_local,
                )?;
                offset += elem_layout.size;
                continue;
            }
            // record element → recurse into the record lift with the element
            // reachable via `prefix + (tup_idx, i)`.
            if let InternedTyKind::Adt(d) = self.ctx.ty_kind(elem_ty)
                && matches!(self.ctx.defs.kind(*d), DefKind::Record(_))
            {
                let rec_def = *d;
                let mut new_prefix = prefix.to_vec();
                new_prefix.push((tup_idx, i as u32));
                self.emit_getter_lift_dtr_record(
                    func,
                    ci,
                    rec_def,
                    offset,
                    scratch_ptr_local,
                    &new_prefix,
                )?;
                offset += elem_layout.size;
                continue;
            }
            // nested tuple element → recurse.
            if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::Tuple(_)) {
                let mut new_prefix = prefix.to_vec();
                new_prefix.push((tup_idx, i as u32));
                self.emit_getter_lift_tuple(
                    func,
                    ci,
                    elem_ty,
                    offset,
                    scratch_ptr_local,
                    mat_ptr_local,
                    mat_len_local,
                    &new_prefix,
                )?;
                offset += elem_layout.size;
                continue;
            }
            match self.internal_repr(elem_ty) {
                super::super::repr::InternalRepr::Scalar(_) => {
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    if offset != 0 {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    emit_elem(self, func, i as u32)?;
                    self.emit_typed_field_store(func, elem_ty);
                }
                super::super::repr::InternalRepr::GcArrayRef(arr_idx) => {
                    let mat_fn = *self
                        .gc_list_materializer_fn_indices
                        .get(&arr_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "tuple getter lift: missing materializer for GC array {}",
                                arr_idx
                            ))
                        })?;
                    emit_elem(self, func, i as u32)?;
                    func.instruction(&Instruction::Call(mat_fn));
                    func.instruction(&Instruction::LocalSet(mat_len_local));
                    func.instruction(&Instruction::LocalSet(mat_ptr_local));
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    if offset != 0 {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(mat_ptr_local));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    func.instruction(&Instruction::I32Const((offset + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(mat_len_local));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                }
                other => {
                    return Err(CodegenError::InvalidIR(format!(
                        "tuple getter lift: element type {:?} (repr {:?}) not yet supported \
                         at the WIT boundary — scalars, strings, scalar lists, records, \
                         tuples and option/result are handled (collapsed option<composite> \
                         pending)",
                        elem_ty, other
                    )));
                }
            }
            offset += elem_layout.size;
        }
        Ok(())
    }

    /// Phase 4: emit the SLR/DTR setter packing for a single record:
    /// consume params at `*next_param..` and leave a `(ref null
    /// $<rec>_record)` on the stack via `struct.new`. Recurses through
    /// nested record fields. Strings / list<scalar> fields box their
    /// (ptr, len) pair into a `$fat_value` via `struct.new $fat_value`.
    fn emit_setter_pack_dtr_record(
        &self,
        func: &mut Function,
        record_def_id: DefId,
        next_param: &mut u32,
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            DefKind::Record(r) => r.clone(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "DTR setter pack: not a record def".into(),
                ));
            }
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("DTR setter pack: missing record type idx".into())
            })?;
        for &field_def_id in &record_def.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => {
                    return Err(CodegenError::InvalidIR(
                        "DTR setter pack: not a field def".into(),
                    ));
                }
            };
            // Phase 5e.5: FlatGcStruct field — read canonical
            // (disc, payload-slots) from params, dispatch on disc to
            // build the matching case subtype, push the (ref null
            // $<sup>) for the parent struct.new.
            if self.flat_gc_migrated(field_ty) {
                self.emit_flat_gc_setter_pack_field(func, field_ty, next_param)?;
                continue;
            }
            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::Adt(field_def) => match self.ctx.defs.kind(*field_def) {
                    yel_core::definitions::DefKind::Record(_) => {
                        // Nested record field: recurse to consume its
                        // flat params and push `(ref null $inner_record)`.
                        self.emit_setter_pack_dtr_record(
                            func,
                            *field_def,
                            next_param,
                        )?;
                    }
                    _ => {
                        // Enum / variant / etc. - single i32 (legacy
                        // SLR path).
                        let field_slots = self.canonical_flat_valtypes(field_ty);
                        for _ in 0..field_slots.len() {
                            func.instruction(&Instruction::LocalGet(*next_param));
                            *next_param += 1;
                        }
                    }
                },
                InternedTyKind::Tuple(_) => {
                    // Nested tuple field: build its GC struct from the field's
                    // canonical params (recursively) and leave the tuple ref
                    // for the parent struct.new.
                    self.emit_setter_pack_tuple(func, field_ty, next_param)?;
                }
                InternedTyKind::String | InternedTyKind::List(_) => {
                    // Typed list field (in list_array_type_idx): the
                    // record-field storage type is `(ref null $list_arr)`,
                    // not `$fat_value`. Convert canonical (ptr, len) to
                    // a typed array ref via the per-list un-materializer.
                    let typed_list_arr_idx =
                        if matches!(self.ctx.ty_kind(field_ty), InternedTyKind::List(_)) {
                            self.record_gc_types
                                .list_array_type_idx
                                .get(&field_ty)
                                .copied()
                        } else {
                            None
                        };
                    let field_slots = self.canonical_flat_valtypes(field_ty);
                    for _ in 0..field_slots.len() {
                        func.instruction(&Instruction::LocalGet(*next_param));
                        *next_param += 1;
                    }
                    // strings-to-GC: a string field is a `$str_bytes` ref
                    // built from canonical (ptr, len) via the str_bytes
                    // un-materializer.
                    let str_bytes_unmat = if matches!(self.ctx.ty_kind(field_ty), InternedTyKind::String)
                    {
                        self.record_gc_types
                            .str_bytes_array_idx
                            .and_then(|idx| self.gc_list_unmaterializer_fn_indices.get(&idx).copied())
                    } else {
                        None
                    };
                    if let Some(unmat_fn) = str_bytes_unmat {
                        func.instruction(&Instruction::Call(unmat_fn));
                    } else if let Some(arr_type_idx) = typed_list_arr_idx {
                        let unmat_fn = *self
                            .gc_list_unmaterializer_fn_indices
                            .get(&arr_type_idx)
                            .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "DTR setter pack (typed list): missing un-materializer for arr {}",
                                arr_type_idx
                            ))
                        })?;
                        func.instruction(&Instruction::Call(unmat_fn));
                    } else {
                        // A `String | List` field with neither a $str_bytes
                        // nor a typed-array un-materializer would be a
                        // fat-pointer-boxed list, which no longer exists —
                        // every valid list is a typed GC array.
                        unreachable!(
                            "DTR setter pack: String/List field with no un-materializer — \
                             strings are $str_bytes and every valid list is a typed GC array; \
                             nothing boxes into $fat_value"
                        );
                    }
                }
                _ => {
                    let field_slots = self.canonical_flat_valtypes(field_ty);
                    for _ in 0..field_slots.len() {
                        func.instruction(&Instruction::LocalGet(*next_param));
                        *next_param += 1;
                    }
                }
            }
        }
        func.instruction(&Instruction::StructNew(record_type_idx));
        Ok(())
    }

    /// Build a tuple GC struct from canonical-ABI flat params, consuming
    /// `*next_param` params in element order and leaving one `(ref $tuple)`
    /// on the stack. The tuple twin of [`Self::emit_setter_pack_dtr_record`];
    /// the two call each other for arbitrarily nested composites (a tuple
    /// element that is a record → `emit_setter_pack_dtr_record`; a nested
    /// tuple → recurse here).
    fn emit_setter_pack_tuple(
        &self,
        func: &mut Function,
        tuple_ty: Ty,
        next_param: &mut u32,
    ) -> Result<(), CodegenError> {
        let elements: Vec<Ty> = match self.ctx.ty_kind(tuple_ty) {
            InternedTyKind::Tuple(els) => els.to_vec(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "tuple setter pack: not a tuple ty".into(),
                ));
            }
        };
        let tup_idx = self
            .record_gc_types
            .tuple_struct_type_idx
            .get(&tuple_ty)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("tuple setter pack: missing tuple_struct_type_idx".into())
            })?;
        for &elem_ty in &elements {
            if self.flat_gc_migrated(elem_ty) {
                self.emit_flat_gc_setter_pack_field(func, elem_ty, next_param)?;
                continue;
            }
            match self.internal_repr(elem_ty) {
                super::super::repr::InternalRepr::Scalar(_) => {
                    func.instruction(&Instruction::LocalGet(*next_param));
                    *next_param += 1;
                }
                super::super::repr::InternalRepr::GcArrayRef(arr_idx) => {
                    let unmat = *self
                        .gc_list_unmaterializer_fn_indices
                        .get(&arr_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "tuple setter pack: missing un-materializer for GC array {}",
                                arr_idx
                            ))
                        })?;
                    func.instruction(&Instruction::LocalGet(*next_param)); // ptr
                    func.instruction(&Instruction::LocalGet(*next_param + 1)); // len
                    func.instruction(&Instruction::Call(unmat));
                    *next_param += 2;
                }
                super::super::repr::InternalRepr::GcRef(_) => {
                    match self.ctx.ty_kind(elem_ty) {
                        InternedTyKind::Adt(d)
                            if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) =>
                        {
                            self.emit_setter_pack_dtr_record(func, *d, next_param)?;
                        }
                        InternedTyKind::Tuple(_) => {
                            // Nested tuple: recurse.
                            self.emit_setter_pack_tuple(func, elem_ty, next_param)?;
                        }
                        _ => {
                            return Err(CodegenError::InvalidIR(format!(
                                "tuple setter pack: element {:?} (GcRef) not yet supported \
                                 at the WIT boundary",
                                elem_ty
                            )));
                        }
                    }
                }
                other => {
                    return Err(CodegenError::InvalidIR(format!(
                        "tuple setter pack: element type {:?} (repr {:?}) not yet supported \
                         at the WIT boundary — scalars, strings, scalar lists, records, \
                         tuples and option/result are handled",
                        elem_ty, other
                    )));
                }
            }
        }
        func.instruction(&Instruction::StructNew(tup_idx));
        Ok(())
    }

    /// Phase 5e.5: read canonical-ABI flat params for a FlatGcStruct
    /// field and push a `(ref null $<sup>)` onto the stack — used by
    /// `emit_setter_pack_dtr_record` when packing a record's field
    /// type that's a migrated option / result / variant.
    ///
    /// Canonical layout: `[disc i32_8, ...payload-slots]`. The
    /// generated code dispatches on the disc param via a chained
    /// `if disc == k` and builds the matching case subtype. After
    /// the cascade, the stack has one supertype ref.
    fn emit_flat_gc_setter_pack_field(
        &self,
        func: &mut Function,
        field_ty: Ty,
        next_param: &mut u32,
    ) -> Result<(), CodegenError> {
        use yel_core::types::InternedTyKind;
        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&field_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct setter pack: missing case count for {:?}",
                    field_ty
                ))
            })?;
        let super_idx = *self
            .record_gc_types
            .flat_gc_super_idx
            .get(&field_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct setter pack: missing flat_gc_super_idx for {:?}",
                    field_ty
                ))
            })?;
        // Disc param index, then payload-slot params follow. Compute
        // the param-index range this field consumes per canonical
        // flattening.
        let canonical = self.canonical_flat_valtypes(field_ty);
        let disc_param = *next_param;
        let payload_start_param = disc_param + 1;
        let payload_count = canonical.len() as u32 - 1; // disc takes 1
        // Build the result via an outer block so each case body can
        // `br` past the remaining tests + final unreachable.
        let block_ty =
            wasm_encoder::BlockType::Result(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(super_idx),
            }));
        func.instruction(&Instruction::Block(block_ty));

        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(field_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "FlatGcStruct setter pack: missing case_idx for ({:?}, {})",
                        field_ty, k
                    ))
                })?;

            // disc == k ?
            func.instruction(&Instruction::LocalGet(disc_param));
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Eq);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // Build the case subtype.
            if let Some(payload_ty) = super::super::gc_types::case_payload_ty(self.ctx, field_ty, k)
            {
                let typed_list_arr_idx =
                    if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_)) {
                        self.record_gc_types
                            .list_array_type_idx
                            .get(&payload_ty)
                            .copied()
                    } else {
                        None
                    };
                let payload_flat = self.canonical_flat_valtypes(payload_ty);
                for (i, vt_payload) in payload_flat.iter().enumerate() {
                    func.instruction(&Instruction::LocalGet(payload_start_param + i as u32));
                    // The param carries the field's JOINED canonical valtype
                    // (the canonical-ABI `join` may have widened it, e.g.
                    // result<s32, s64> joins the payload slot to i64). Bridge
                    // it back to this case's payload valtype before building
                    // the case subtype — otherwise `struct.new $..._<case>`
                    // sees the wrong width (i64 where an i32 field is
                    // expected). Same narrowing the direct FlatGcStruct signal
                    // setter applies.
                    let vt_joined = canonical.get(1 + i).copied().unwrap_or(*vt_payload);
                    emit_canonical_reinterpret(func, vt_joined, *vt_payload)?;
                }
                if let Some(arr_type_idx) = typed_list_arr_idx {
                    let unmat_fn = *self
                        .gc_list_unmaterializer_fn_indices
                        .get(&arr_type_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "FlatGcStruct setter pack (typed list): missing un-materializer \
                                 for arr {}",
                                arr_type_idx
                            ))
                        })?;
                    func.instruction(&Instruction::Call(unmat_fn));
                } else if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
                {
                    // strings-to-GC: build a $str_bytes ref from (ptr, len).
                    let arr_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "FlatGcStruct setter pack: $str_bytes missing".into(),
                        )
                    })?;
                    let unmat_fn = *self
                        .gc_list_unmaterializer_fn_indices
                        .get(&arr_idx)
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(
                                "FlatGcStruct setter pack: missing $str_bytes un-materializer".into(),
                            )
                        })?;
                    func.instruction(&Instruction::Call(unmat_fn));
                }
                // A scalar payload's slots are already on the stack for
                // the case struct.new below. String (str_bytes) and typed
                // lists were handled above; nothing boxes into $fat_value.
                func.instruction(&Instruction::StructNew(case_sub_idx));
            } else {
                func.instruction(&Instruction::StructNewDefault(case_sub_idx));
            }

            // br with the case ref on the stack to exit the outer
            // block with that as the result.
            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
        }

        // Default: unreachable canonical-ABI invariant violation.
        // The host MUST send a valid disc; otherwise our types differ
        // from theirs. Push struct.new_default of case 0 to satisfy
        // the block's result type, then trap.
        let case0_sub_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(field_ty, 0))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct setter pack: missing case_idx for ({:?}, 0)",
                    field_ty
                ))
            })?;
        func.instruction(&Instruction::StructNewDefault(case0_sub_idx));

        // End outer block.
        func.instruction(&Instruction::End);

        // Advance next_param past disc + payload slots.
        *next_param += 1 + payload_count;
        Ok(())
    }

    /// Phase 5e.5 Stage 7f: write the inner case's payload bytes for
    /// a 2-deep nested FlatGcStruct lift (e.g. option<option<P>>).
    /// Inner-case-subtype payload field is loaded by chaining outer
    /// cast + outer struct.get + inner cast + inner struct.get.
    /// Currently only handles non-promoted single-slot scalar payloads
    /// — fat-box and deeper nesting fall to follow-up.
    fn emit_nested_flat_gc_inner_payload_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
        outer_case_sub_idx: u32,
        inner_case_sub_idx: u32,
        inner_payload_ty: Ty,
        canonical_slots: &[crate::wasm::FlatSlot],
        scratch_ptr_local: u32,
    ) -> Result<(), CodegenError> {
        use super::super::gc_types::StructGetVariant;
        use yel_core::types::InternedTyKind;

        // Detect fat-box inner payload: case-subtype field is
        // `(ref null $fat_value)`.
        // strings-to-GC: nested string payload — the inner case-subtype
        // field is a `$str_bytes` ref; materialize to (ptr, len).
        if matches!(self.ctx.ty_kind(inner_payload_ty), InternedTyKind::String)
        {
            let arr_type_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                CodegenError::InvalidIR("nested FlatGcStruct string lift: $str_bytes missing".into())
            })?;
            let mat_fn = *self
                .gc_list_materializer_fn_indices
                .get(&arr_type_idx)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "nested FlatGcStruct string lift: missing materializer".into(),
                    )
                })?;
            let ptr_slot = *canonical_slots.get(2).ok_or_else(|| {
                CodegenError::InvalidIR("nested string lift: missing ptr slot".into())
            })?;
            let len_slot = *canonical_slots.get(3).ok_or_else(|| {
                CodegenError::InvalidIR("nested string lift: missing len slot".into())
            })?;
            let mat_ptr_local = scratch_ptr_local + 1;
            let mat_len_local = scratch_ptr_local + 2;
            self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(outer_case_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: outer_case_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(inner_case_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: inner_case_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::Call(mat_fn));
            func.instruction(&Instruction::LocalSet(mat_len_local));
            func.instruction(&Instruction::LocalSet(mat_ptr_local));
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if ptr_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(ptr_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::LocalGet(mat_ptr_local));
            func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            if len_slot.offset != 0 {
                func.instruction(&Instruction::I32Const(len_slot.offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::LocalGet(mat_len_local));
            func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            return Ok(());
        }

        // Scalar inner payload.
        let inner_payload_outer_slot = canonical_slots.get(2).ok_or_else(|| {
            CodegenError::InvalidIR(
                "nested FlatGcStruct lift: missing outer slot for inner payload".into(),
            )
        })?;
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if inner_payload_outer_slot.offset != 0 {
            func.instruction(&Instruction::I32Const(
                inner_payload_outer_slot.offset as i32,
            ));
            func.instruction(&Instruction::I32Add);
        }
        self.emit_signal_struct_read_for_lift(func, ci, sig_idx)?;
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(outer_case_sub_idx),
        ));
        func.instruction(&Instruction::StructGet {
            struct_type_index: outer_case_sub_idx,
            field_index: 0,
        });
        func.instruction(&Instruction::RefCastNonNull(
            wasm_encoder::HeapType::Concrete(inner_case_sub_idx),
        ));
        let getter = super::super::gc_types::struct_get_op_for_payload(self.ctx, inner_payload_ty);
        match getter {
            StructGetVariant::Plain => {
                func.instruction(&Instruction::StructGet {
                    struct_type_index: inner_case_sub_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Signed => {
                func.instruction(&Instruction::StructGetS {
                    struct_type_index: inner_case_sub_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Unsigned => {
                func.instruction(&Instruction::StructGetU {
                    struct_type_index: inner_case_sub_idx,
                    field_index: 0,
                });
            }
        }
        self.emit_typed_field_store(func, inner_payload_ty);
        Ok(())
    }

    /// Phase 5e.5 Stage 7f: recursively pack canonical-flat params
    /// into a FlatGcStruct supertype ref. Reads canonical slots
    /// starting at param index `base`, builds the matching case
    /// subtype, recurses on FlatGcStruct payloads. Emits an
    /// `if/else if/.../else struct.new_default` cascade that leaves
    /// one `(ref null $sup)` on the stack.
    ///
    /// Currently restricted to non-promoted (no width-join) shapes;
    /// width promotion lands on #79.
    pub(super) fn emit_pack_canonical_to_flat_gc(
        &self,
        func: &mut Function,
        ty: Ty,
        base: u32,
    ) -> Result<(), CodegenError> {
        use yel_core::types::InternedTyKind;
        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "pack canonical→flat-gc: missing case count for {:?}",
                    ty
                ))
            })?;
        let super_idx = *self
            .record_gc_types
            .flat_gc_super_idx
            .get(&ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "pack canonical→flat-gc: missing super idx for {:?}",
                    ty
                ))
            })?;
        let result_ty =
            wasm_encoder::BlockType::Result(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(super_idx),
            }));

        let disc_param = base;
        let payload_start = base + 1;

        // Emit a nested if/else cascade. For each case k:
        //   if disc == k then <build case k> else <next>
        // Final else: struct.new_default $case0 (unreachable invariant).
        let mut nesting: u32 = 0;
        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "pack canonical→flat-gc: missing case_idx for ({:?}, {})",
                        ty, k
                    ))
                })?;
            // Test disc == k
            func.instruction(&Instruction::LocalGet(disc_param));
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Eq);
            func.instruction(&Instruction::If(result_ty));
            // Build case k subtype.
            if let Some(payload_ty) = super::super::gc_types::case_payload_ty(self.ctx, ty, k) {
                let payload_repr = self.internal_repr(payload_ty);
                if matches!(
                    payload_repr,
                    super::super::repr::InternalRepr::FlatGcStruct(_)
                ) {
                    // Recursive pack: payload's canonical slots start
                    // at payload_start (parent shares its joined
                    // payload slot positions with each case's payload
                    // canonical when not width-promoted).
                    self.emit_pack_canonical_to_flat_gc(func, payload_ty, payload_start)?;
                    func.instruction(&Instruction::StructNew(case_sub_idx));
                } else {
                    let payload_flat = self.canonical_flat_valtypes(payload_ty);
                    for i in 0..payload_flat.len() as u32 {
                        func.instruction(&Instruction::LocalGet(payload_start + i));
                    }
                    // strings-to-GC: a string payload builds a `$str_bytes`
                    // ref from canonical (ptr, len). Every valid list is a
                    // typed GC array, so String is the only ref-built payload
                    // here — nothing boxes into $fat_value.
                    if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String) {
                        let str_bytes_idx =
                            self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                                CodegenError::InvalidIR(
                                    "pack canonical→flat-gc: $str_bytes missing".into(),
                                )
                            })?;
                        let unmat_fn = *self
                            .gc_list_unmaterializer_fn_indices
                            .get(&str_bytes_idx)
                            .ok_or_else(|| {
                                CodegenError::InvalidIR(
                                    "pack canonical→flat-gc: missing $str_bytes un-materializer"
                                        .into(),
                                )
                            })?;
                        func.instruction(&Instruction::Call(unmat_fn));
                    }
                    func.instruction(&Instruction::StructNew(case_sub_idx));
                }
            } else {
                func.instruction(&Instruction::StructNewDefault(case_sub_idx));
            }
            // Begin else for the next case.
            func.instruction(&Instruction::Else);
            nesting += 1;
        }
        // Innermost else: invariant violation — host sent invalid disc.
        // Push struct.new_default $case0 to satisfy the block result type.
        let case0_sub_idx = *self
            .record_gc_types
            .flat_gc_case_idx
            .get(&(ty, 0))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "pack canonical→flat-gc: missing case_idx for ({:?}, 0)",
                    ty
                ))
            })?;
        func.instruction(&Instruction::StructNewDefault(case0_sub_idx));
        for _ in 0..nesting {
            func.instruction(&Instruction::End);
        }
        Ok(())
    }

    /// Phase 5e.1: build a `(ref null $<rec>)` from canonical-ABI bytes
    /// at memory address held in `base_addr_local`. For each field:
    /// - primitive: typed load at field offset
    /// - string / list<scalar>: load (ptr, len) at field offset and
    ///   wrap in `struct.new $fat_value`
    /// - nested DTR record: recurse with adjusted base+field_offset
    ///
    /// Ends with `struct.new $<rec>` consuming the pushed field values
    /// and leaving the record GC ref on the stack.
    ///
    /// **Boundary-only.** Lifts canonical-ABI bytes the host wrote into
    /// `cabi_realloc`'d scratch (e.g., a setter param's record value)
    /// into the typed `(ref null $<rec>)`. Internal SSA never calls
    /// this — internal record values live as GC refs end-to-end via
    /// `RecordConstruct → struct.new` and `Field → struct.get`.
    pub(super) fn emit_record_pack_from_memory(
        &mut self,
        func: &mut Function,
        record_def_id: DefId,
        base_addr_local: u32,
        base_offset: u32,
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            DefKind::Record(r) => r.clone(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "record_pack_from_memory: not a record def".into(),
                ));
            }
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("record_pack_from_memory: missing record_type_idx".into())
            })?;
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR("record_pack_from_memory: missing record layout".into())
            })?
            .clone();
        for (i, &field_def_id) in record_def.fields.iter().enumerate() {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => {
                    return Err(CodegenError::InvalidIR(
                        "record_pack_from_memory: not a field def".into(),
                    ));
                }
            };
            let (_name, field_offset, _ty) =
                layout.field_offsets.get(i).cloned().ok_or_else(|| {
                    CodegenError::InvalidIR("record_pack_from_memory: missing field offset".into())
                })?;
            let abs_off = base_offset + field_offset;
            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::Adt(field_def)
                    if matches!(
                        self.ctx.defs.kind(*field_def),
                        yel_core::definitions::DefKind::Record(_)
                    ) =>
                {
                    self.emit_record_pack_from_memory(func, *field_def, base_addr_local, abs_off)?;
                }
                InternedTyKind::List(_)
                    if self
                        .record_gc_types
                        .list_array_type_idx
                        .get(&field_ty)
                        .copied()
                        .is_some() =>
                {
                    // Phase 5e.6: typed-array list field. Call per-array
                    // un-materializer to lift canonical (ptr, len) into a
                    // typed `(ref null $<elem>_list)` GC array.
                    let arr_idx = self.record_gc_types.list_array_type_idx[&field_ty];
                    let unmat_fn = self
                        .gc_list_unmaterializer_fn_indices
                        .get(&arr_idx)
                        .copied()
                        .ok_or_else(|| CodegenError::InvalidIR(format!(
                            "record_pack_from_memory: missing un-materializer for arr_type_idx={}",
                            arr_idx
                        )))?;
                    // call $unmat(ptr, len) → (ref null $arr)
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const(abs_off as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::Call(unmat_fn));
                }
                InternedTyKind::String | InternedTyKind::List(_) => {
                    // Load (ptr, len) from memory.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const(abs_off as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    // strings-to-GC: build a $str_bytes ref instead of boxing.
                    // Typed-array lists matched the arm above; a `List` here
                    // would be a non-typed-array list, which no longer exists.
                    if matches!(self.ctx.ty_kind(field_ty), InternedTyKind::String) {
                        self.emit_str_bytes_unmaterialize(func)?;
                    } else {
                        unreachable!(
                            "record_pack_from_memory: non-typed-array list field — every valid \
                             list is a typed GC array; nothing boxes into $fat_value"
                        );
                    }
                }
                _ => {
                    // Primitive / enum field — typed load.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    if abs_off != 0 {
                        func.instruction(&Instruction::I32Const(abs_off as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    self.emit_typed_field_load(func, field_ty);
                }
            }
        }
        func.instruction(&Instruction::StructNew(record_type_idx));
        Ok(())
    }

    /// Phase 5e.1: write a record GC ref's fields to canonical-ABI
    /// memory at `base_addr_local + base_offset`. The ref must be on
    /// top of the stack on entry; this consumes it. For each field:
    /// - primitive: struct.get + typed store at field offset
    /// - string / list<scalar>: struct.get the $fat_value box, unbox
    ///   (ptr, len) and store both at field offset / +4
    /// - nested DTR record: recurse on the inner ref
    ///
    /// Optional scratch i32 locals used by the typed-array list-field
    /// path to stash (ptr, len) returned by the per-array materializer
    /// before storing them into canonical memory. Pass `None` from
    /// callers that don't yet declare the scratch locals — the lift
    /// will return an error if it actually needs them.
    ///
    /// Phase 5e.5: lift a single FlatGcStruct *field* of a parent
    /// record into the record's canonical-ABI memory layout. Used by
    /// `emit_record_lift_to_memory` when a record contains a migrated
    /// option / result / variant field.
    ///
    /// Reads the supertype ref from `record_ref_local.struct.get
    /// $rec field_idx`, walks each case via `ref.test`, and on match
    /// writes the canonical disc byte + payload bytes at
    /// `base_addr + base_field_offset + slot_offset` per the field's
    /// canonical-flat layout.
    fn emit_flat_gc_field_lift(
        &mut self,
        func: &mut Function,
        record_ref_local: u32,
        record_type_idx: u32,
        gc_field_idx: u32,
        field_ty: Ty,
        base_addr_local: u32,
        base_field_offset: u32,
    ) -> Result<(), CodegenError> {
        use super::scratch::mem_arg;
        let canonical_slots = self.flatten_core_slots(field_ty);
        let disc_offset = canonical_slots.first().map(|s| s.offset).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "FlatGcStruct field lift: empty canonical layout for {:?}",
                field_ty
            ))
        })?;

        let case_count = *self
            .record_gc_types
            .flat_gc_case_count
            .get(&field_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "FlatGcStruct field lift: missing case count for {:?}",
                    field_ty
                ))
            })?;

        // Outer block lets a matching case skip remaining tests +
        // fall-through default.
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));

        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .flat_gc_case_idx
                .get(&(field_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "FlatGcStruct field lift: missing case_idx for ({:?}, {})",
                        field_ty, k
                    ))
                })?;

            // Test: <record>.struct.get $rec field; ref.test (ref $case_k)
            func.instruction(&Instruction::LocalGet(record_ref_local));
            func.instruction(&Instruction::StructGet {
                struct_type_index: record_type_idx,
                field_index: gc_field_idx,
            });
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // disc = k at base_addr + base_field_offset + disc_offset
            func.instruction(&Instruction::LocalGet(base_addr_local));
            let disc_abs = base_field_offset + disc_offset;
            if disc_abs != 0 {
                func.instruction(&Instruction::I32Const(disc_abs as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::I32Const(k as i32));
            func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

            // Payload (if any) — use the same fat_box / typed-slot
            // dispatch as the signal-lift helper, but read the case
            // payload via the record-field path.
            if let Some(payload_ty) = super::super::gc_types::case_payload_ty(self.ctx, field_ty, k)
            {
                self.emit_flat_gc_record_field_payload_lift(
                    func,
                    record_ref_local,
                    record_type_idx,
                    gc_field_idx,
                    case_sub_idx,
                    payload_ty,
                    &canonical_slots,
                    base_addr_local,
                    base_field_offset,
                )?;
            }

            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
        }

        // Default: write disc=0 (legacy zero-byte parity).
        func.instruction(&Instruction::LocalGet(base_addr_local));
        let disc_abs = base_field_offset + disc_offset;
        if disc_abs != 0 {
            func.instruction(&Instruction::I32Const(disc_abs as i32));
            func.instruction(&Instruction::I32Add);
        }
        func.instruction(&Instruction::I32Const(0));
        func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

        // End outer block.
        func.instruction(&Instruction::End);
        Ok(())
    }

    /// Helper: write payload bytes for a FlatGcStruct field's active
    /// case into the parent record's canonical-ABI scratch.
    fn emit_flat_gc_record_field_payload_lift(
        &mut self,
        func: &mut Function,
        record_ref_local: u32,
        record_type_idx: u32,
        gc_field_idx: u32,
        case_sub_idx: u32,
        payload_ty: Ty,
        canonical_slots: &[crate::wasm::FlatSlot],
        base_addr_local: u32,
        base_field_offset: u32,
    ) -> Result<(), CodegenError> {
        use super::super::gc_types::StructGetVariant;

        // Helper closure to push <record>.struct.get $rec field;
        // ref.cast to case subtype.
        let emit_field_ref = |func: &mut Function| {
            func.instruction(&Instruction::LocalGet(record_ref_local));
            func.instruction(&Instruction::StructGet {
                struct_type_index: record_type_idx,
                field_index: gc_field_idx,
            });
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
        };

        // Single-slot scalar payload.
        let payload_slot = canonical_slots.get(1).ok_or_else(|| {
            CodegenError::InvalidIR("FlatGcStruct field payload lift: missing payload slot".into())
        })?;
        func.instruction(&Instruction::LocalGet(base_addr_local));
        let abs_off = base_field_offset + payload_slot.offset;
        if abs_off != 0 {
            func.instruction(&Instruction::I32Const(abs_off as i32));
            func.instruction(&Instruction::I32Add);
        }
        emit_field_ref(func);
        let getter = super::super::gc_types::struct_get_op_for_payload(self.ctx, payload_ty);
        match getter {
            StructGetVariant::Plain => {
                func.instruction(&Instruction::StructGet {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Signed => {
                func.instruction(&Instruction::StructGetS {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
            }
            StructGetVariant::Unsigned => {
                func.instruction(&Instruction::StructGetU {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
            }
        }
        self.emit_typed_field_store(func, payload_ty);
        Ok(())
    }

    /// **Boundary-only.** Lowers a typed `(ref null $<rec>)` to
    /// canonical-ABI bytes in linear memory at `base_addr_local`.
    /// Used by export getters and host materializers (e.g., for
    /// `set-attribute` variant payloads). Internal record values
    /// stay in GC the entire time and never need this.
    fn emit_record_lift_to_memory(
        &mut self,
        func: &mut Function,
        record_def_id: DefId,
        record_ref_local: u32,
        base_addr_local: u32,
        base_offset: u32,
        scratch_ptr_len: Option<(u32, u32)>,
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            DefKind::Record(r) => r.clone(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "record_lift_to_memory: not a record def".into(),
                ));
            }
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("record_lift_to_memory: missing record_type_idx".into())
            })?;
        let gc_field_indices: Vec<u32> = self
            .record_gc_types
            .field_gc_indices
            .get(&record_def_id)
            .cloned()
            .ok_or_else(|| {
                CodegenError::InvalidIR("record_lift_to_memory: missing gc field indices".into())
            })?;
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| {
                CodegenError::InvalidIR("record_lift_to_memory: missing record layout".into())
            })?
            .clone();
        for (i, &field_def_id) in record_def.fields.iter().enumerate() {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => {
                    return Err(CodegenError::InvalidIR(
                        "record_lift_to_memory: not a field def".into(),
                    ));
                }
            };
            let (_name, field_offset, _ty) =
                layout.field_offsets.get(i).cloned().ok_or_else(|| {
                    CodegenError::InvalidIR("record_lift_to_memory: missing field offset".into())
                })?;
            let abs_off = base_offset + field_offset;
            let gc_field_idx = gc_field_indices[i];
            // Phase 5e.5: FlatGcStruct field — read the supertype ref
            // from the record, dispatch on case via ref.test, write
            // disc + payload bytes at the field's canonical layout
            // offset. Mirrors the signal-lift logic in
            // `emit_flat_gc_signal_lift` but reads from a record GC
            // struct field instead of a component-struct field.
            if self.flat_gc_migrated(field_ty) {
                self.emit_flat_gc_field_lift(
                    func,
                    record_ref_local,
                    record_type_idx,
                    gc_field_idx,
                    field_ty,
                    base_addr_local,
                    abs_off,
                )?;
                continue;
            }
            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::Adt(field_def)
                    if matches!(
                        self.ctx.defs.kind(*field_def),
                        yel_core::definitions::DefKind::Record(_)
                    ) =>
                {
                    // Nested record: load the inner ref, then recurse
                    // — but we'd need a fresh local for the inner ref.
                    // Skip nested-record support for the initial 5e.1
                    // landing; emit a clear error so it surfaces if
                    // hit.
                    return Err(CodegenError::InvalidIR(
                        "record_lift_to_memory: nested record fields not yet supported".into(),
                    ));
                }
                InternedTyKind::List(_)
                    if self
                        .record_gc_types
                        .list_array_type_idx
                        .get(&field_ty)
                        .copied()
                        .is_some() =>
                {
                    // Phase 5e.6: typed-array list field. Call the
                    // per-array materializer to lower the GC array back
                    // to canonical (ptr, len), then store both i32s.
                    let arr_idx = self.record_gc_types.list_array_type_idx[&field_ty];
                    let mat_fn = self
                        .gc_list_materializer_fn_indices
                        .get(&arr_idx)
                        .copied()
                        .ok_or_else(|| {
                            CodegenError::InvalidIR(format!(
                                "record_lift_to_memory: missing materializer for arr_type_idx={}",
                                arr_idx
                            ))
                        })?;
                    let (scratch_ptr, scratch_len) = scratch_ptr_len.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "record_lift_to_memory: typed-array list field requires scratch i32 locals".into(),
                        )
                    })?;
                    // (ptr, len) ← call $mat(struct.get field)
                    func.instruction(&Instruction::LocalGet(record_ref_local));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: record_type_idx,
                        field_index: gc_field_idx,
                    });
                    func.instruction(&Instruction::Call(mat_fn));
                    func.instruction(&Instruction::LocalSet(scratch_len));
                    func.instruction(&Instruction::LocalSet(scratch_ptr));
                    // Store ptr at abs_off.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    if abs_off != 0 {
                        func.instruction(&Instruction::I32Const(abs_off as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(scratch_ptr));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    // Store len at abs_off + 4.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(scratch_len));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                }
                InternedTyKind::String => {
                    // strings-to-GC: field is a `$str_bytes` ref. Materialize
                    // to (ptr, len) and store the canonical 8-byte slot.
                    let (scratch_ptr, scratch_len) = scratch_ptr_len.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "record_lift_to_memory: string field requires scratch i32 locals".into(),
                        )
                    })?;
                    func.instruction(&Instruction::LocalGet(record_ref_local));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: record_type_idx,
                        field_index: gc_field_idx,
                    });
                    self.emit_str_bytes_materialize(func)?;
                    func.instruction(&Instruction::LocalSet(scratch_len));
                    func.instruction(&Instruction::LocalSet(scratch_ptr));
                    // Store ptr at abs_off.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    if abs_off != 0 {
                        func.instruction(&Instruction::I32Const(abs_off as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(scratch_ptr));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    // Store len at abs_off + 4.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(scratch_len));
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                }
                // A non-typed-array `list<T>` field would box into
                // `$fat_value`, but every valid list is a typed GC array
                // (matched above), so this arm is unreachable.
                InternedTyKind::List(_) => {
                    unreachable!(
                        "record_lift_to_memory: non-typed-array list field — every valid list \
                         is a typed GC array; nothing boxes into $fat_value"
                    );
                }
                _ => {
                    // Primitive / enum: struct.get → typed store at offset.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    if abs_off != 0 {
                        func.instruction(&Instruction::I32Const(abs_off as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::LocalGet(record_ref_local));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: record_type_idx,
                        field_index: gc_field_idx,
                    });
                    self.emit_typed_field_store(func, field_ty);
                }
            }
        }
        Ok(())
    }

    /// Emit a typed memory load for a primitive/enum field type.
    /// Address is on the stack; result is the loaded value.
    fn emit_typed_field_load(&self, func: &mut Function, ty: yel_core::Ty) {
        use super::scratch::mem_arg;
        match self.ctx.ty_kind(ty) {
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

    /// Emit a typed memory store for a primitive/enum field type.
    /// (address, value) on the stack.
    fn emit_typed_field_store(&self, func: &mut Function, ty: yel_core::Ty) {
        use super::scratch::mem_arg;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Bool | InternedTyKind::S8 | InternedTyKind::U8 => {
                func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
            }
            InternedTyKind::S16 | InternedTyKind::U16 => {
                func.instruction(&Instruction::I32Store16(mem_arg(0, 1)));
            }
            InternedTyKind::S64 | InternedTyKind::U64 => {
                func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
            }
            InternedTyKind::F32 => {
                func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
            }
            InternedTyKind::F64 => {
                func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
            }
            _ => {
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }
        }
    }
}

/// Returns (canonical_byte_size, canonical_align) for a GC list element type.
fn gc_list_elem_canonical_info(
    ctx: &yel_core::context::CompilerContext,
    layout_ctx: &mut yel_core::lir::LirLayoutContext,
    elem_ty: yel_core::Ty,
) -> (u32, u32) {
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(elem_ty) {
        InternedTyKind::Bool | InternedTyKind::S8 | InternedTyKind::U8 => (1, 1),
        InternedTyKind::S16 | InternedTyKind::U16 => (2, 2),
        InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::F32 | InternedTyKind::Char => {
            (4, 4)
        }
        InternedTyKind::S64 | InternedTyKind::U64 | InternedTyKind::F64 => (8, 8),
        // Phase 5e.1: records use their canonical-ABI memory layout
        // size/align (sourced from layout_ctx). Caller passes the
        // record def_id; the layout knows total bytes + max alignment.
        InternedTyKind::Adt(d)
            if matches!(ctx.defs.kind(*d), yel_core::definitions::DefKind::Record(_)) =>
        {
            if let Some(rl) = layout_ctx.record_layout_by_id(*d) {
                (rl.layout.size, rl.layout.align)
            } else {
                (4, 4)
            }
        }
        // Phase 5e.2: lists (and strings) at canonical ABI are
        // 8 bytes (ptr i32 + len i32), align 4.
        InternedTyKind::List(_) | InternedTyKind::String => (8, 4),
        _ => (4, 4),
    }
}

/// Emit `array.get $arr_type_idx`. All scalar element types are stored
/// as unpacked i32/i64/f32/f64 in the GC array, so plain array.get works.
fn emit_gc_array_get(
    func: &mut wasm_encoder::Function,
    _ctx: &yel_core::context::CompilerContext,
    _elem_ty: yel_core::Ty,
    arr_type_idx: u32,
) {
    func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
}

/// Emit a memory store for a GC list element (value already on stack).
fn emit_gc_list_elem_store(
    func: &mut wasm_encoder::Function,
    ctx: &yel_core::context::CompilerContext,
    elem_ty: yel_core::Ty,
) {
    use super::scratch::mem_arg;
    use wasm_encoder::Instruction;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(elem_ty) {
        InternedTyKind::Bool | InternedTyKind::S8 | InternedTyKind::U8 => {
            func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
        }
        InternedTyKind::S16 | InternedTyKind::U16 => {
            func.instruction(&Instruction::I32Store16(mem_arg(0, 1)));
        }
        InternedTyKind::S64 | InternedTyKind::U64 => {
            func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
        }
        InternedTyKind::F32 => {
            func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
        }
        InternedTyKind::F64 => {
            func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
        }
        _ => {
            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
        }
    }
}

/// Emit a memory load for a GC list element (address already on stack).
fn emit_gc_list_elem_load(
    func: &mut wasm_encoder::Function,
    ctx: &yel_core::context::CompilerContext,
    elem_ty: yel_core::Ty,
) {
    use super::scratch::mem_arg;
    use wasm_encoder::Instruction;
    use yel_core::types::InternedTyKind;
    match ctx.ty_kind(elem_ty) {
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

/// Emit `array.set $arr_type_idx` for an element type.
fn emit_gc_array_set(
    func: &mut wasm_encoder::Function,
    _ctx: &yel_core::context::CompilerContext,
    _elem_ty: yel_core::Ty,
    arr_type_idx: u32,
) {
    func.instruction(&wasm_encoder::Instruction::ArraySet(arr_type_idx));
}
