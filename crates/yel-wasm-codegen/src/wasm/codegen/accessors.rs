//! Per-signal getter/setter generation + value-coercion helpers.
//!
//! Methods live on `WasmPackageBuilder<'a>` via an additional impl block
//! and are called from `build::build_core_module` during the code section
//! pass.

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::types::InternedTyKind;
use yel_core::{DefId, DefKind, Ty};

use super::super::CodegenError;
use super::super::WasmPackageBuilder;

/// How the GcVariant supertype ref that a lift reads from is reached on the
/// stack. The composite payload-lift family (`emit_gc_variant_lift` and friends)
/// re-emits this ref once per case in the `ref.test` cascade, so it must be
/// side-effect-free and cheap to re-emit — both variants are.
///
/// - `SelfChain`: a signal / record-field lift walks `self` + a `struct.get`
///   chain (`emit_gc_field_chain`).
/// - `ArrayElem`: a `list<gc-variant>` materializer reads `arr[idx]`.
/// - `PayloadOf`: the gc-variant payload *nested inside* another gc-variant case —
///   `<inner>.ref.cast $case.struct.get 0`. This makes nested-gc-variant lifting
///   recurse to arbitrary depth (`option<result<result<…>>>`, `option<variant
///   with a gc-variant / list payload>`, …) through the one full-featured lift.
#[derive(Clone, Copy)]
pub(in crate::wasm) enum GcRefSource<'a> {
    SelfChain {
        ci: usize,
        chain: &'a [(u32, u32)],
    },
    /// A typed GC ref held in a local, optionally followed by a `struct.get`
    /// chain (`(struct_type_idx, gc_field_idx)` per hop). An empty chain is
    /// the local itself.
    LocalChain {
        ref_local: u32,
        chain: &'a [(u32, u32)],
    },
    ArrayElem {
        arr_local: u32,
        idx_local: u32,
        arr_type_idx: u32,
    },
    PayloadOf {
        inner: &'a GcRefSource<'a>,
        case_sub_idx: u32,
        /// `struct.get` hops applied AFTER reaching the case payload field
        /// (`<inner>.ref.cast $case.struct.get 0`), so a record / tuple
        /// payload's members can be reached through a variant case. Empty =
        /// the payload ref itself.
        chain: &'a [(u32, u32)],
    },
}

/// Where a value's canonical-ABI representation starts when packing it into
/// its GC form. `Params`: the value's flat slots are function params starting
/// at `first_param`. `Memory`: the value's canonical bytes sit at
/// `address_local + offset`. Member positions are derived at generation time
/// (flat param counts / layout offsets), so a source is `Copy` and never
/// mutated — a member's source is computed from its parent's.
#[derive(Clone, Copy)]
enum CanonicalSource {
    Params { first_param: u32 },
    Memory { address_local: u32, offset: u32 },
}

/// One member (record field / tuple element) of a composite GC struct: its
/// type, byte offset within the composite's canonical layout, and the GC
/// struct field index it lives at.
struct CompositeMember {
    ty: yel_core::Ty,
    canonical_offset: u32,
    gc_field_index: u32,
}


/// When canonical-ABI joined-flat slot type
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

/// Widen a value on top of the stack from its own canonical slot valtype
/// (`vt_case`) up to the `join`ed slot valtype (`vt_joined`) that a
/// gc-variant's canonical shape declares at that position. The inverse of
/// [`emit_canonical_reinterpret`] (which goes joined → case). The `join`
/// widens toward i32/i64 (any 64-bit or ref beats 32-bit; integer beats float
/// at equal width), so the only bridges are same-width reinterprets and
/// zero-extends into i64. A ref → i64 stop-gap join is a loud gap.
fn emit_canonical_widen(
    func: &mut Function,
    vt_case: ValType,
    vt_joined: ValType,
) -> Result<(), CodegenError> {
    if vt_case == vt_joined {
        return Ok(());
    }
    match (vt_case, vt_joined) {
        // Same-width int/float reinterpret (join picks the integer).
        (ValType::F32, ValType::I32) => {
            func.instruction(&Instruction::I32ReinterpretF32);
        }
        (ValType::F64, ValType::I64) => {
            func.instruction(&Instruction::I64ReinterpretF64);
        }
        // 32-bit → i64 joined slot: zero-extend into the low half (the host
        // reads only the case's own low bits, so zero-extension is lossless).
        (ValType::I32, ValType::I64) => {
            func.instruction(&Instruction::I64ExtendI32U);
        }
        (ValType::F32, ValType::I64) => {
            func.instruction(&Instruction::I32ReinterpretF32);
            func.instruction(&Instruction::I64ExtendI32U);
        }
        _ => {
            return Err(CodegenError::InvalidIR(format!(
                "canonical-ABI widen: unsupported case→joined bridge {:?} → {:?}",
                vt_case, vt_joined
            )));
        }
    }
    Ok(())
}

/// Push the zero value of `vt` onto the stack (zero-padding a joined payload
/// slot a shorter variant case doesn't cover). A ref slot pads with a typed
/// null.
fn push_zero_valtype(func: &mut Function, vt: ValType) -> Result<(), CodegenError> {
    match vt {
        ValType::I32 => func.instruction(&Instruction::I32Const(0)),
        ValType::I64 => func.instruction(&Instruction::I64Const(0)),
        ValType::F32 => func.instruction(&Instruction::F32Const(0.0.into())),
        ValType::F64 => func.instruction(&Instruction::F64Const(0.0.into())),
        ValType::Ref(rt) => func.instruction(&Instruction::RefNull(rt.heap_type)),
        other => {
            return Err(CodegenError::InvalidIR(format!(
                "canonical-ABI zero-pad: unsupported slot valtype {:?}",
                other
            )));
        }
    };
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
            // A materialized string is a fresh `cabi_realloc`
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
    /// getter whose result was freshly materialised into linear memory.
    /// Param 0 is the returned pointer. The body frees the freshly
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
            // Free the string's materialized (ptr, len)
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

    /// Materialise the canonical-ABI return shape from the component's
    /// `$Comp_<i>` struct fields. Multi-slot composites are written into a
    /// per-call `cabi_realloc` lift scratch and the scratch pointer is
    /// returned per canonical ABI; primitives bypass memory entirely.
    pub(super) fn generate_getter_for_with_struct(
        &mut self,
        signal_ty: Ty,
        sig_idx: usize,
        comp_idx: Option<usize>,
    ) -> Result<Function, CodegenError> {
        // A lossy nested collapsing-option (option<option<record|tuple|list>>)
        // collapses to a single nullable ref that cannot distinguish `none`
        // from `some(none)`. Refuse loudly rather than emit a getter that
        // round-trips a corrupted value (or the misleading "no materializer
        // for GC list" the storage-field walk would otherwise raise).
        if self.is_lossy_nested_collapsing_option(signal_ty) {
            return Err(CodegenError::InvalidIR(format!(
                "getter: {:?} is a nested collapsing option whose single-ref storage \
                 loses `some(none)` vs `none`; a non-collapsing gc-variant repr (needs \
                 gc-variant composite payloads) is required",
                signal_ty
            )));
        }
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

            // GC list getter — delegate the per-element lowering to the
            // shared per-list materializer function (generated for every
            // registered list array type); the getter body just calls it and
            // spills the returned (ptr, len) into an 8-byte scratch. Only
            // matches direct `list<T>` signals — option-collapsed signals
            // (option<list<T>>) fall through to the multi-slot getter below
            // which materialises the discriminant via null-check.
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
                let mat_fn = *self
                    .gc_list_materializer_fn_indices
                    .get(&arr_type_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "GC list getter: missing materializer for arr {}",
                            arr_type_idx
                        ))
                    })?;
                let self_ref_local: u32 = 1;
                let scratch_local: u32 = 2;
                let ptr_temp: u32 = 3;
                let len_temp: u32 = 4;
                let mut func = Function::new([
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
                ]);
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
                func.instruction(&Instruction::Call(mat_fn));
                store_canonical_ptr_len(&mut func, scratch_local, 0, 4, ptr_temp, len_temp);
                func.instruction(&Instruction::LocalGet(scratch_local));
                self.current_self_local = None;
                self.current_self_comp_idx = None;
                func.instruction(&Instruction::End);
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
            let mut func = Function::new([
                (
                    1,
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                    }),
                ),
                (3, ValType::I32),
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
            // SLR (POR + records with string / list<scalar>
            // fields) all route through the GC-backed getter path.
            let is_por = self.is_single_level_record(signal_ty);
            let result = (|| -> Result<(), CodegenError> {
                // POR record with exactly one flat slot —
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
                // GcVariant signal — lift the GC ref to the canonical-ABI
                // (disc, payload) layout in a scratch buffer. MUST precede the
                // `flat_valtypes.len() == 1` direct-return branch below: a
                // payload-less variant (or `result<(),()>`) has a single
                // canonical i32 slot yet its storage is a GC ref, so the
                // direct `struct.get` would return a ref where the getter's
                // i32 result is expected.
                if let super::super::repr::InternalRepr::GcVariant(super_idx) =
                    self.internal_repr(signal_ty)
                {
                    return self.emit_gc_variant_signal_lift(
                        &mut func,
                        ci,
                        sig_idx,
                        signal_ty,
                        super_idx,
                        scratch_ptr_local,
                    );
                }
                // Collapsed option (option<record|tuple|list>): storage is a
                // single nullable ref, but the canonical shape is
                // [disc, ...inner]. Handle BEFORE the generic single-slot
                // direct return below, which would wrongly return the raw ref.
                // An empty inner (e.g. `option<record {}>`) collapses to just
                // [disc]: return `!ref.is_null` (some=1/none=0) by value.
                // Otherwise lift disc+payload into a cabi_realloc scratch.
                if matches!(self.ctx.ty_kind(signal_ty), InternedTyKind::Option(_))
                    && self.option_collapses_to_ref(signal_ty).is_some()
                {
                    if flat_valtypes.len() == 1 {
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[0],
                        });
                        func.instruction(&Instruction::RefIsNull);
                        func.instruction(&Instruction::I32Eqz);
                        return Ok(());
                    }
                    return self.emit_option_collapsed_ref_signal_lift(
                        &mut func,
                        ci,
                        sig_idx,
                        signal_ty,
                        scratch_ptr_local,
                        mat_ptr_local,
                        mat_len_local,
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
                // Primitive-only record getter. The struct
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

                    // Lower the record into the scratch member-by-member;
                    // each member is sourced via the chain of GC struct.gets
                    // that reach it (nested composites extend the chain).
                    let _ = (record_type_idx, record_def_id);
                    let prefix: Vec<(u32, u32)> = vec![(struct_ty, field_path[0])];
                    self.emit_composite_lift_to_memory(
                        &mut func,
                        signal_ty,
                        GcRefSource::SelfChain { ci, chain: &prefix },
                        scratch_ptr_local,
                        0,
                        Some((mat_ptr_local, mat_len_local)),
                    )?;
                    func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                    return Ok(());
                }
                // Tuple-as-signal getter — storage is
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
                    // tuple GC struct into it member-by-member and return the
                    // scratch pointer.
                    super::scratch::emit_cabi_realloc_fixed(&mut func, layout_info.align, layout_info.size, cabi_realloc);
                    func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                    let prefix: Vec<(u32, u32)> = vec![(struct_ty, field_path[0])];
                    self.emit_composite_lift_to_memory(
                        &mut func,
                        signal_ty,
                        GcRefSource::SelfChain { ci, chain: &prefix },
                        scratch_ptr_local,
                        0,
                        Some((mat_ptr_local, mat_len_local)),
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

        // Mirror the getter guard: a lossy nested collapsing-option cannot be
        // stored without losing `some(none)` vs `none`. Refuse loudly.
        if self.is_lossy_nested_collapsing_option(ty) {
            return Err(CodegenError::InvalidIR(format!(
                "setter: {:?} is a nested collapsing option whose single-ref storage \
                 loses `some(none)` vs `none`; a non-collapsing gc-variant repr (needs \
                 gc-variant composite payloads) is required",
                ty
            )));
        }

        // Write each canonical-ABI flat param into its backing struct
        // field. The setter signature is `(self: i32, flat_0, flat_1, ...)`;
        // composite params are un-materialized into GC refs first.
        if self.signal_in_struct(comp_idx, sig_idx) {
            let gc = &self.gc_layouts[comp_idx];
            let struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "setter (struct): component {} missing component_struct_type_idx",
                    comp_idx
                ))
            })?;
            let field_path: Vec<u32> = component.signal_layout.signal_field_path(sig_idx);

            // GC list setter — delegate canonical (ptr, len) → GC array to
            // the shared per-list un-materializer function (generated for
            // every registered list array type). Only handles direct list
            // signals; option-collapsed option<list<T>> signals are handled
            // in a dedicated branch further below.
            if matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_))
                && let super::super::repr::InternalRepr::GcArrayRef(arr_type_idx) =
                    self.internal_repr(ty)
            {
                let unmat_fn = *self
                    .gc_list_unmaterializer_fn_indices
                    .get(&arr_type_idx)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "GC list setter: missing un-materializer for arr {}",
                            arr_type_idx
                        ))
                    })?;
                // Setter params: 0=rep(i32), 1=ptr(i32), 2=len(i32).
                // Locals: 3=self_ref.
                let self_ref_local: u32 = 3;
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

            // Primitive-only record signal — params are
            // canonical-ABI flat (one per record field), but the
            // struct field is ONE ref slot. Pack the flat params into
            // a `struct.new $<rec>_record`, then `struct.set` on the
            // component field.
            // SLR (POR + string / list<scalar> fields) routes
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

            // A plain string
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

            // Option<T> where T's internal repr is a single GC
            // ref (record / list / tuple) collapses to a nullable ref
            // slot internally. Setter params are canonical:
            // (self, disc, ...inner_canonical); disc != 0 builds the inner
            // from the following params, disc == 0 stores a typed null ref.
            if matches!(self.ctx.ty_kind(ty), InternedTyKind::Option(_))
                && self.option_collapses_to_ref(ty).is_some()
            {
                let declared_vts = self.canonical_flat_valtypes(ty);
                self.emit_self_ref(&mut func, comp_idx)?;
                self.emit_member_pack(
                    &mut func,
                    ty,
                    CanonicalSource::Params { first_param: 1 },
                    &declared_vts,
                )?;
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

            // Tuple-as-signal setter — params are flat
            // canonical slots (one per tuple element). Push self, then
            // each flat param (consuming canonical slot count per
            // element via `canonical_flat_valtypes`), `struct.new
            // $tuple_<n>` to build the ref, then `struct.set` into
            // the component field.
            if let InternedTyKind::Tuple(_) = self.ctx.ty_kind(ty) {
                // Build the tuple GC struct from the canonical-ABI flat params
                // (recursively — see `emit_composite_pack`) and store the
                // resulting ref into the component field.
                let declared_vts = self.canonical_flat_valtypes(ty);
                self.emit_self_ref(&mut func, comp_idx)?;
                self.emit_composite_pack(
                    &mut func,
                    ty,
                    CanonicalSource::Params { first_param: 1 },
                    &declared_vts,
                )?;
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
            // GcVariant setter — params are canonical
            // (rep, disc, ...payload-slots). Delegate the whole disc-dispatch
            // cascade (per-case build, width-join reinterprets, string /
            // typed-list un-materializing, nested gc-variant recursion) to the
            // shared pack: disc at param 1, payload slots at 2..
            if matches!(
                self.internal_repr(ty),
                super::super::repr::InternalRepr::GcVariant(_)
            ) {
                self.emit_self_ref(&mut func, comp_idx)?;
                let declared_vts = self.canonical_flat_valtypes(ty);
                self.emit_pack_canonical_to_gc_variant(
                    &mut func,
                    ty,
                    CanonicalSource::Params { first_param: 1 },
                    &declared_vts,
                )?;
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
                // Push self ref, then build the record GC struct from the
                // flat params (un-materializing string/list (ptr, len) pairs
                // into GC refs), then `struct.set` on the component field.
                let declared_vts = self.canonical_flat_valtypes(ty);
                self.emit_self_ref(&mut func, comp_idx)?;
                self.emit_composite_pack(
                    &mut func,
                    ty,
                    CanonicalSource::Params { first_param: 1 },
                    &declared_vts,
                )?;
                let _ = (record_type_idx, record_def_id, actual_flat_count);
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

    /// Lift a `GcVariant` signal into the canonical-ABI
    /// `(disc, payload-bytes)` memory layout for the WIT export boundary.
    /// Allocates a `cabi_realloc`'d scratch buffer sized to the signal's
    /// canonical layout, writes the disc + active case's payload bytes,
    /// and leaves the scratch pointer on the stack as the getter's
    /// return value.
    ///
    /// Per-case body uses a `block $done; … br $done` cascade so once
    /// a case matches we skip the remaining tests and the fall-through
    /// default. The default writes disc=0 if every `ref.test` fails —
    /// only reachable for uninitialized GcVariant signals (defensive).
    ///
    /// Payload writes follow the case subtype's payload field type:
    /// - Primitive scalar (i32 / i64 / f32 / f64): single typed store.
    /// - String: `$str_bytes` ref — materialize to (ptr, len) and store
    ///   the pair at consecutive canonical slot offsets.
    fn emit_gc_variant_signal_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
        signal_ty: Ty,
        super_idx: u32,
        scratch_ptr_local: u32,
    ) -> Result<(), CodegenError> {
        let _ = super_idx;

        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("GcVariant lift: cabi_realloc missing".into())
            })?
            .cabi_realloc;
        let layout_info = self.layout_ctx.layout_of(signal_ty);

        // A gc-variant signal is a gc-variant field of the component struct reached
        // at `field_path[0]`. Allocate the canonical-ABI lift scratch and
        // delegate to the one reach-generic gc-variant lift so the signal and
        // nested-field lifts share a single case-loop + payload
        // implementation. Scratch locals 3 (ptr) and 4 (len) are reserved by
        // the getter for inner materializer returns.
        super::scratch::emit_cabi_realloc_fixed(func, layout_info.align, layout_info.size, cabi_realloc);
        func.instruction(&Instruction::LocalSet(scratch_ptr_local));

        let struct_ty = self.gc_layouts[ci].component_struct_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("GcVariant lift: missing component_struct_type_idx".into())
        })?;
        let field_idx = self.components[ci]
            .signal_layout
            .signal_field_path(sig_idx)
            .first()
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "GcVariant lift: missing field path for signal {}",
                    sig_idx
                ))
            })?;
        let chain = [(struct_ty, field_idx)];
        self.emit_gc_variant_lift(
            func,
            GcRefSource::SelfChain { ci, chain: &chain },
            signal_ty,
            0,
            scratch_ptr_local,
            3,
            4,
        )?;

        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        Ok(())
    }

    /// Lift an option-of-collapsed-ref signal — `option<T>` where T's
    /// internal repr is a single GC ref (record / list-array / tuple / string)
    /// — to canonical-ABI bytes: allocate the lift scratch, delegate the
    /// disc + payload writes to the collapse-aware member lift, and leave the
    /// scratch pointer on the stack as the getter's return value.
    fn emit_option_collapsed_ref_signal_lift(
        &mut self,
        func: &mut Function,
        ci: usize,
        sig_idx: usize,
        signal_ty: Ty,
        scratch_ptr_local: u32,
        mat_ptr_local: u32,
        mat_len_local: u32,
    ) -> Result<(), CodegenError> {
        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR("option-collapsed lift: cabi_realloc missing".into())
            })?
            .cabi_realloc;
        let layout_info = self.layout_ctx.layout_of(signal_ty);
        super::scratch::emit_cabi_realloc_fixed(func, layout_info.align, layout_info.size, cabi_realloc);
        func.instruction(&Instruction::LocalSet(scratch_ptr_local));

        let struct_ty = self.gc_layouts[ci].component_struct_type_idx.ok_or_else(|| {
            CodegenError::InvalidIR("option-collapsed lift: missing component_struct_type_idx".into())
        })?;
        let field_path = self.components[ci].signal_layout.signal_field_path(sig_idx);
        let chain = [(struct_ty, field_path[0])];
        self.emit_member_lift_to_memory(
            func,
            signal_ty,
            GcRefSource::SelfChain { ci, chain: &chain },
            scratch_ptr_local,
            0,
            Some((mat_ptr_local, mat_len_local)),
        )?;
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        Ok(())
    }



    /// Lift one gc-variant case payload (struct index 0 of the case subtype) to
    /// canonical-ABI bytes: scalar → typed store; string / typed list →
    /// materialize to (ptr, len); nested GcVariant → recurse. The supertype
    /// ref is reached via `source`; `canonical_slots` carry ABSOLUTE offsets
    /// (the caller pre-adds any field base offset). Shared by the signal lift
    /// and the field lift.
    fn emit_gc_variant_payload_lift(
        &mut self,
        func: &mut Function,
        source: GcRefSource,
        case_sub_idx: u32,
        payload_ty: Ty,
        canonical_slots: &[crate::wasm::FlatSlot],
        scratch_ptr_local: u32,
        mat_ptr_local: u32,
        mat_len_local: u32,
    ) -> Result<(), CodegenError> {
        use super::super::gc_types::StructGetVariant;
        use yel_core::types::InternedTyKind;

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
                        "GcVariant payload lift (typed list): missing materializer for arr {}",
                        arr_type_idx
                    ))
                })?;
            let ptr_slot = canonical_slots.get(1).ok_or_else(|| {
                CodegenError::InvalidIR(
                    "GcVariant payload lift (typed list): missing ptr slot".into(),
                )
            })?;
            let len_slot = canonical_slots.get(2).ok_or_else(|| {
                CodegenError::InvalidIR(
                    "GcVariant payload lift (typed list): missing len slot".into(),
                )
            })?;
            // Load case typed array ref, call materializer → (ptr, len).
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: case_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::Call(mat_fn));
            store_canonical_ptr_len(
                func,
                scratch_ptr_local,
                ptr_slot.offset,
                len_slot.offset,
                mat_ptr_local,
                mat_len_local,
            );
            return Ok(());
        }

        // A string payload's case-subtype field is a
        // `(ref null $str_bytes)` — materialize to (ptr, len) like a typed
        // list.
        if matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
        {
            let arr_type_idx = self.record_gc_types.str_bytes_array_idx.ok_or_else(|| {
                CodegenError::InvalidIR("GcVariant string payload lift: $str_bytes missing".into())
            })?;
            let mat_fn = *self
                .gc_list_materializer_fn_indices
                .get(&arr_type_idx)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "GcVariant string payload lift: missing $str_bytes materializer".into(),
                    )
                })?;
            let ptr_slot = canonical_slots.get(1).ok_or_else(|| {
                CodegenError::InvalidIR("GcVariant string payload lift: missing ptr slot".into())
            })?;
            let len_slot = canonical_slots.get(2).ok_or_else(|| {
                CodegenError::InvalidIR("GcVariant string payload lift: missing len slot".into())
            })?;
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::StructGet {
                struct_type_index: case_sub_idx,
                field_index: 0,
            });
            func.instruction(&Instruction::Call(mat_fn));
            store_canonical_ptr_len(
                func,
                scratch_ptr_local,
                ptr_slot.offset,
                len_slot.offset,
                mat_ptr_local,
                mat_len_local,
            );
            return Ok(());
        }

        // Nested GcVariant payload — the case-subtype's payload field is
        // itself a `(ref null $inner_super)`. Recurse through
        // `emit_gc_variant_lift` with a `PayloadOf` ref source, which handles
        // arbitrary nesting depth and every inner payload kind.
        //
        // The inner gc-variant's canonical layout is placed at the outer's first
        // post-disc slot; `canonical_slots` here are already absolute, so
        // `canonical_slots[1].offset` is the inner region's base offset.
        if matches!(
            self.internal_repr(payload_ty),
            super::super::repr::InternalRepr::GcVariant(_)
        ) {
            let inner_base = canonical_slots
                .get(1)
                .map(|s| s.offset)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(
                        "GcVariant payload lift (nested): missing outer slot for inner"
                            .into(),
                    )
                })?;
            let payload_source = GcRefSource::PayloadOf {
                inner: &source,
                case_sub_idx,
                chain: &[],
            };
            return self.emit_gc_variant_lift(
                func,
                payload_source,
                payload_ty,
                inner_base,
                scratch_ptr_local,
                mat_ptr_local,
                mat_len_local,
            );
        }

        // Record / tuple / collapsed-option payload: the case-subtype's field 0
        // is a composite ref (record ref / tuple ref / collapsed-inner ref).
        // Delegate to the reach-generic member lift with a `PayloadOf` source
        // (reaches `<source>.ref.cast $case.struct.get 0`), writing the
        // payload's canonical bytes at the payload region base offset.
        if self.composite_gc_members(payload_ty)?.is_some()
            || self.option_collapses_to_ref(payload_ty).is_some()
        {
            let payload_base = canonical_slots.get(1).map(|s| s.offset).ok_or_else(|| {
                CodegenError::InvalidIR(
                    "GcVariant composite payload lift: missing payload region base slot".into(),
                )
            })?;
            let payload_source = GcRefSource::PayloadOf {
                inner: &source,
                case_sub_idx,
                chain: &[],
            };
            return self.emit_member_lift_to_memory(
                func,
                payload_ty,
                payload_source,
                scratch_ptr_local,
                payload_base,
                Some((mat_ptr_local, mat_len_local)),
            );
        }

        // Primitive scalar or simple typed payload: single canonical
        // slot. Push (addr, value) and use the payload type's natural
        // store width.
        let payload_slot = canonical_slots.get(1).ok_or_else(|| {
            CodegenError::InvalidIR(
                "GcVariant payload lift: missing payload slot in canonical layout".into(),
            )
        })?;
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        if payload_slot.offset != 0 {
            func.instruction(&Instruction::I32Const(payload_slot.offset as i32));
            func.instruction(&Instruction::I32Add);
        }
        self.emit_gc_ref(func, source)?;
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
        emit_canonical_scalar_store(func, self.ctx, payload_ty);
        Ok(())
    }



    /// Emit `$pack_color_to_attr_slots` body — the per-program
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
            .gc_variant_case_count
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
            .gc_variant_case_idx
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
                .gc_variant_case_idx
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

        // GcVariant element — for each
        // canonical (disc, payload) record at ptr + idx * elem_size,
        // build a supertype ref via per-case dispatch and store into
        // the typed array.
        if matches!(
            self.internal_repr(elem_ty),
            super::super::repr::InternalRepr::GcVariant(_)
        ) {
            let elem_size = self.layout_ctx.layout_of(elem_ty).size;
            emit_gc_array_unmaterialize_loop(
                &mut func,
                ptr_local,
                len_local,
                idx_local,
                Some(elem_addr_local),
                elem_size,
                |func| {
                    // arr.set(idx, <build supertype ref from canonical bytes
                    // at elem_addr>). The one reach-generic pack (scalar /
                    // string / typed list / nested gc-variant) — the
                    // un-materializer twin of `emit_gc_variant_lift`.
                    func.instruction(&Instruction::LocalGet(arr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    self.emit_pack_canonical_to_gc_variant(
                        func,
                        elem_ty,
                        CanonicalSource::Memory {
                            address_local: elem_addr_local,
                            offset: 0,
                        },
                        // Memory-sourced: each case reads its payload at its
                        // natural width from its own offset — no bridging.
                        &[],
                    )?;
                    func.instruction(&Instruction::ArraySet(arr_type_idx));
                    Ok(())
                },
            )?;
            func.instruction(&Instruction::LocalGet(arr_local));
            func.instruction(&Instruction::End);
            return Ok(func);
        }
        if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String) {
            // Element is a `$str_bytes` ref built from the
            // canonical (ptr, len) at ptr+idx*8 via the str_bytes
            // un-materializer.
            // for idx in 0..len { arr.set(idx, build_elem(load(ptr+idx*8), load(ptr+idx*8+4))) }
            emit_gc_array_unmaterialize_loop(
                &mut func,
                ptr_local,
                len_local,
                idx_local,
                Some(elem_addr_local),
                8,
                |func| {
                    // arr.set(idx, str_bytes_unmaterialize(load ptr, load len))
                    func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
                    func.instruction(&wasm_encoder::Instruction::I32Load(
                        super::scratch::mem_arg(0, 2),
                    ));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
                    func.instruction(&wasm_encoder::Instruction::I32Const(4));
                    func.instruction(&wasm_encoder::Instruction::I32Add);
                    func.instruction(&wasm_encoder::Instruction::I32Load(
                        super::scratch::mem_arg(0, 2),
                    ));
                    self.emit_str_bytes_unmaterialize(func)?;
                    func.instruction(&wasm_encoder::Instruction::ArraySet(arr_type_idx));
                    Ok(())
                },
            )?;
        }
        // Tuple element — build each tuple GC ref from its canonical-flat
        // bytes via the composite pack and `arr.set` it (the tuple
        // twin of the record branch below).
        if matches!(self.ctx.ty_kind(elem_ty), yel_core::types::InternedTyKind::Tuple(_))
            && self.record_gc_types.tuple_struct_type_idx.contains_key(&elem_ty)
        {
            let elem_size = self.layout_ctx.size_of(elem_ty);
            emit_gc_array_unmaterialize_loop(
                &mut func,
                ptr_local,
                len_local,
                idx_local,
                Some(elem_addr_local),
                elem_size,
                |func| {
                    func.instruction(&Instruction::LocalGet(arr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    self.emit_composite_pack(
                        func,
                        elem_ty,
                        CanonicalSource::Memory {
                            address_local: elem_addr_local,
                            offset: 0,
                        },
                        &[],
                    )?;
                    func.instruction(&Instruction::ArraySet(arr_type_idx));
                    Ok(())
                },
            )?;
            func.instruction(&Instruction::LocalGet(arr_local));
            func.instruction(&Instruction::End); // function
            return Ok(func);
        }
        // Record element — for each canonical-flat record at
        // `ptr + idx * elem_size`, build a typed `(ref null $<rec>)`
        // via the composite pack and `arr.set` it. Without this branch
        // the array is left filled with default (null) refs, which then
        // traps every downstream `struct.get` reading a field.
        if let yel_core::types::InternedTyKind::Adt(d) = self.ctx.ty_kind(elem_ty)
            && matches!(
                self.ctx.defs.kind(*d),
                yel_core::definitions::DefKind::Record(_)
            ) && self.record_gc_types.record_type_idx.contains_key(d)
            {
                let elem_size = self.layout_ctx.size_of(elem_ty);
                emit_gc_array_unmaterialize_loop(
                    &mut func,
                    ptr_local,
                    len_local,
                    idx_local,
                    Some(elem_addr_local),
                    elem_size,
                    |func| {
                        // arr.set(idx, <composite pack from canonical bytes at elem_addr>)
                        func.instruction(&Instruction::LocalGet(arr_local));
                        func.instruction(&Instruction::LocalGet(idx_local));
                        self.emit_composite_pack(
                            func,
                            elem_ty,
                            CanonicalSource::Memory {
                                address_local: elem_addr_local,
                                offset: 0,
                            },
                            &[],
                        )?;
                        func.instruction(&Instruction::ArraySet(arr_type_idx));
                        Ok(())
                    },
                )?;
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::End); // function
                return Ok(func);
            }
        // Nested-list element — each elem is itself a typed GC array ref;
        // build it from the canonical (ptr, len) at elem_addr via the inner
        // un-materializer and `array.set` it.
        if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::List(_))
            && let Some(&inner_arr_idx) = self.record_gc_types.list_array_type_idx.get(&elem_ty)
        {
            let inner_unmat_fn = *self
                .gc_list_unmaterializer_fn_indices
                .get(&inner_arr_idx)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "gc_list un-materializer (nested): missing inner un-materializer for arr_type_idx={}",
                        inner_arr_idx
                    ))
                })?;
            emit_gc_array_unmaterialize_loop(
                &mut func,
                ptr_local,
                len_local,
                idx_local,
                Some(elem_addr_local),
                8,
                |func| {
                    // arr.set(idx, inner_unmat(load(elem_addr), load(elem_addr+4)))
                    func.instruction(&Instruction::LocalGet(arr_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::Call(inner_unmat_fn));
                    func.instruction(&Instruction::ArraySet(arr_type_idx));
                    Ok(())
                },
            )?;
            func.instruction(&Instruction::LocalGet(arr_local));
            func.instruction(&Instruction::End); // function
            return Ok(func);
        }
        // Scalar / enum element: copy each canonical value from the (ptr, len)
        // buffer into the typed GC array (`array.new_default` above only
        // zero-filled it). Reached by the gc-variant-payload pack path for e.g. a
        // `list<s32>` nested in `option<result<list<s32>, string>>`. Any
        // non-scalar element that falls through to here keeps the zero-filled
        // default rather than a bogus scalar-load.
        if matches!(
            self.internal_repr(elem_ty),
            super::super::repr::InternalRepr::Scalar(_)
        ) {
            let (elem_size, _elem_align) =
                gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
            emit_gc_array_unmaterialize_loop(
                &mut func,
                ptr_local,
                len_local,
                idx_local,
                None,
                elem_size,
                |func| {
                    // arr.set(idx, <typed load at ptr + idx * elem_size>)
                    func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(ptr_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                    func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
                    func.instruction(&wasm_encoder::Instruction::I32Mul);
                    func.instruction(&wasm_encoder::Instruction::I32Add);
                    emit_canonical_scalar_load(func, self.ctx, elem_ty);
                    func.instruction(&wasm_encoder::Instruction::ArraySet(arr_type_idx));
                    Ok(())
                },
            )?;
        }
        let _ = elem_addr_local;
        func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
        func.instruction(&wasm_encoder::Instruction::End);
        Ok(func)
    }

    /// Materialize a `$str_bytes`
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
        // mem8[data_ptr + idx] = array.get_u(arr, idx)
        emit_gc_array_materialize_loop(
            &mut func,
            arr_local,
            len_local,
            data_ptr_local,
            idx_local,
            None,
            1,
            1,
            cabi_realloc,
            |func| {
                func.instruction(&Instruction::LocalGet(data_ptr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::ArrayGetU(arr_type_idx));
                func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
                Ok(())
            },
        )?;
        // return (data_ptr, len)
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Un-materialize a canonical `(ptr, len)` into a
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
        // arr.set(idx, load8u(ptr + idx))
        emit_gc_array_unmaterialize_loop(
            &mut func,
            ptr_local,
            len_local,
            idx_local,
            None,
            1,
            |func| {
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::LocalGet(ptr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::I32Load8U(super::scratch::mem_arg(0, 0)));
                func.instruction(&Instruction::ArraySet(arr_type_idx));
                Ok(())
            },
        )?;
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Materializer for a `list<string>` whose array element
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
        emit_gc_array_materialize_loop(
            &mut func,
            arr_local,
            len_local,
            data_ptr_local,
            idx_local,
            Some(elem_addr_local),
            8,
            4,
            cabi_realloc,
            |func| {
                // (inner_ptr, inner_len) = str_bytes_materialize(arr[idx])
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::ArrayGet(arr_type_idx));
                self.emit_str_bytes_materialize(func)?;
                store_canonical_ptr_len(
                    func,
                    elem_addr_local,
                    0,
                    4,
                    inner_ptr_local,
                    inner_len_local,
                );
                Ok(())
            },
        )?;
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
        let mut local_decls: Vec<(u32, ValType)> = vec![
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
        ];
        // A record / tuple inner is lifted through
        // `emit_composite_lift_to_memory`, which reads the ref from a typed
        // local — declare one (8).
        let typed_ref_local: Option<u32> =
            if let Some((struct_type_index, _)) = self.composite_gc_members(inner_ty)? {
                local_decls.push((
                    1,
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: HeapType::Concrete(struct_type_index),
                    }),
                ));
                Some(8)
            } else {
                None
            };
        let mut func = Function::new(local_decls);
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        let elem_addr_local: u32 = 4;
        let elem_ref_local: u32 = 5;
        let mat_ptr_local: u32 = 6;
        let mat_len_local: u32 = 7;
        emit_gc_array_materialize_loop(
            &mut func,
            arr_local,
            len_local,
            data_ptr_local,
            idx_local,
            Some(elem_addr_local),
            elem_size,
            elem_align,
            cabi_realloc,
            |func| {
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
                    func,
                    inner_ty,
                    elem_ref_local,
                    elem_addr_local,
                    payload_off,
                    &canonical_slots,
                    mat_ptr_local,
                    mat_len_local,
                    typed_ref_local,
                )?;
                func.instruction(&Instruction::End); // if
                Ok(())
            },
        )?;
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Lift a collapsed-option inner value (record / tuple / scalar-list),
    /// whose non-null ref is held in `inner_ref_local` (anyref), into the
    /// canonical payload region at `base_addr_local + payload_off`. A record /
    /// tuple inner needs `typed_ref_local` — a caller-declared typed
    /// `(ref null $composite)` local the anyref is cast into before delegating
    /// to [`Self::emit_composite_lift_to_memory`].
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
        typed_ref_local: Option<u32>,
    ) -> Result<(), CodegenError> {
        use wasm_encoder::HeapType;
        if let Some((struct_type_index, _)) = self.composite_gc_members(inner_ty)? {
            let typed_ref_local = typed_ref_local.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "collapsed-option composite lift: caller declared no typed ref local".into(),
                )
            })?;
            func.instruction(&Instruction::LocalGet(inner_ref_local));
            func.instruction(&Instruction::RefCastNonNull(HeapType::Concrete(
                struct_type_index,
            )));
            func.instruction(&Instruction::LocalSet(typed_ref_local));
            return self.emit_composite_lift_to_memory(
                func,
                inner_ty,
                GcRefSource::LocalChain {
                    ref_local: typed_ref_local,
                    chain: &[],
                },
                base_addr_local,
                payload_off,
                Some((mat_ptr_local, mat_len_local)),
            );
        }
        match self.ctx.ty_kind(inner_ty) {
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
                store_canonical_ptr_len(
                    func,
                    base_addr_local,
                    ptr_slot.offset,
                    len_slot.offset,
                    mat_ptr_local,
                    mat_len_local,
                );
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
        emit_gc_array_unmaterialize_loop(
            &mut func,
            ptr_local,
            len_local,
            idx_local,
            Some(elem_addr_local),
            elem_size,
            |func| {
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
                self.emit_member_pack(
                    func,
                    inner_ty,
                    CanonicalSource::Memory {
                        address_local: elem_addr_local,
                        offset: payload_off,
                    },
                    &[],
                )?;
                func.instruction(&Instruction::ArraySet(arr_type_idx));
                func.instruction(&Instruction::End); // if
                Ok(())
            },
        )?;
        func.instruction(&Instruction::LocalGet(arr_local));
        func.instruction(&Instruction::End);
        Ok(func)
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
        // For record element types we need a typed copy
        // loop that pulls each field out of the record GC ref and
        // stores it at the canonical-ABI offset in memory.
        // When element is GcVariant, the
        // typed-array stores supertype refs — materialize each via a
        // per-case ref.test cascade in a dedicated branch below.
        let elem_is_gc_variant = matches!(
            self.internal_repr(elem_ty),
            super::super::repr::InternalRepr::GcVariant(_)
        );
        // A `list<string>` whose element is a `$str_bytes`
        // ref. Per element: materialize the inner byte array to (ptr, len)
        // and write the canonical 8-byte (ptr, len) slot.
        if matches!(self.ctx.ty_kind(elem_ty), yel_core::types::InternedTyKind::String)
        {
            return self.generate_gc_list_string_materializer(arr_type_idx);
        }
        // Nested-list element — each elem is itself a typed
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
            emit_gc_array_materialize_loop(
                &mut func,
                arr_local,
                len_local,
                data_ptr_local,
                idx_local,
                Some(elem_addr_local),
                elem_size,
                elem_align,
                cabi_realloc,
                |func| {
                    // (inner_ptr, inner_len) = $inner_mat(arr.get(idx))
                    func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                    func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&wasm_encoder::Instruction::Call(inner_mat_fn));
                    store_canonical_ptr_len(
                        func,
                        elem_addr_local,
                        0,
                        4,
                        inner_ptr_local,
                        inner_len_local,
                    );
                    Ok(())
                },
            )?;
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::End);
            return Ok(func);
        }
        // GcVariant element — materialize each
        // ref to canonical bytes via a per-case `ref.test` cascade.
        if elem_is_gc_variant {
            return self.generate_gc_list_materializer_gc_variant(arr_type_idx, elem_ty);
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
        // Tuple element: each array slot is a tuple GC struct ref. Per element,
        // lift the tuple to canonical memory at its slot (the tuple twin of the
        // record branch below).
        if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::Tuple(_))
            && let Some(&tup_idx) = self.record_gc_types.tuple_struct_type_idx.get(&elem_ty)
        {
            let (elem_size, elem_align) =
                gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
            let mut func = Function::new([
                (1, ValType::I32), // len
                (1, ValType::I32), // data_ptr
                (1, ValType::I32), // idx
                (1, ValType::I32), // elem_addr
                (
                    1,
                    ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(tup_idx),
                    }),
                ), // elem_ref (typed tuple ref)
                (1, ValType::I32), // mat_ptr
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
            emit_gc_array_materialize_loop(
                &mut func,
                arr_local,
                len_local,
                data_ptr_local,
                idx_local,
                Some(elem_addr_local),
                elem_size,
                elem_align,
                cabi_realloc,
                |func| {
                    func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                    func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&wasm_encoder::Instruction::LocalSet(elem_ref_local));
                    self.emit_composite_lift_to_memory(
                        func,
                        elem_ty,
                        GcRefSource::LocalChain {
                            ref_local: elem_ref_local,
                            chain: &[],
                        },
                        elem_addr_local,
                        0,
                        Some((mat_ptr_local, mat_len_local)),
                    )
                },
            )?;
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::End);
            return Ok(func);
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
            emit_gc_array_materialize_loop(
                &mut func,
                arr_local,
                len_local,
                data_ptr_local,
                idx_local,
                Some(elem_addr_local),
                elem_size,
                elem_align,
                cabi_realloc,
                |func| {
                    // elem_ref = arr[idx]; lift fields → memory at elem_addr.
                    func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
                    func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                    func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
                    func.instruction(&wasm_encoder::Instruction::LocalSet(elem_ref_local));
                    self.emit_composite_lift_to_memory(
                        func,
                        elem_ty,
                        GcRefSource::LocalChain {
                            ref_local: elem_ref_local,
                            chain: &[],
                        },
                        elem_addr_local,
                        0,
                        Some((mat_ptr_local, mat_len_local)),
                    )
                },
            )?;
            // return (data_ptr, len)
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::End);
            return Ok(func);
        }
        let (elem_size, elem_align) =
            gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
        // Param 0 = arr `(ref null $arr)`. Locals: 1 = len, 2 = data_ptr, 3 = idx.
        let mut func = Function::new([
            (1, ValType::I32), // len
            (1, ValType::I32), // data_ptr
            (1, ValType::I32), // idx
        ]);
        let arr_local: u32 = 0;
        let len_local: u32 = 1;
        let data_ptr_local: u32 = 2;
        let idx_local: u32 = 3;
        // mem[data_ptr + idx * elem_size] = arr[idx]
        emit_gc_array_materialize_loop(
            &mut func,
            arr_local,
            len_local,
            data_ptr_local,
            idx_local,
            None,
            elem_size,
            elem_align,
            cabi_realloc,
            |func| {
                func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
                func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
                func.instruction(&wasm_encoder::Instruction::I32Mul);
                func.instruction(&wasm_encoder::Instruction::I32Add);
                func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
                func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
                emit_gc_array_get(func, self.ctx, elem_ty, arr_type_idx);
                emit_canonical_scalar_store(func, self.ctx, elem_ty);
                Ok(())
            },
        )?;
        // return (data_ptr, len)
        func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
        func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
        func.instruction(&wasm_encoder::Instruction::End);
        Ok(func)
    }

    /// Materializer for `list<GcVariant>`
    /// — for each `(ref null $sup)` element, write canonical bytes
    /// (disc + payload) at `data_ptr + idx * elem_size` via a
    /// per-case ref.test cascade (delegating payload writes to
    /// `emit_gc_variant_lift`).
    fn generate_gc_list_materializer_gc_variant(
        &mut self,
        arr_type_idx: u32,
        elem_ty: yel_core::Ty,
    ) -> Result<Function, CodegenError> {
        let cabi_realloc = self
            .alloc_funcs
            .as_ref()
            .ok_or_else(|| {
                CodegenError::InvalidIR(
                    "gc_list_materializer (gc-variant): cabi_realloc missing".into(),
                )
            })?
            .cabi_realloc;
        let layout_info = self.layout_ctx.layout_of(elem_ty);
        let elem_size = layout_info.size;
        let elem_align = layout_info.align;

        // Param 0 = arr `(ref null $arr)`.
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

        emit_gc_array_materialize_loop(
            &mut func,
            arr_local,
            len_local,
            data_ptr_local,
            idx_local,
            Some(elem_addr_local),
            elem_size,
            elem_align,
            cabi_realloc,
            |func| {
                // Lift each element's active case (disc + payload) into
                // canonical memory at `elem_addr` via the shared
                // GcVariant lift.
                self.emit_gc_variant_lift(
                    func,
                    GcRefSource::ArrayElem {
                        arr_local,
                        idx_local,
                        arr_type_idx,
                    },
                    elem_ty,
                    0,
                    elem_addr_local,
                    mat_ptr_local,
                    mat_len_local,
                )
            },
        )?;
        // Return (data_ptr, len)
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(len_local));
        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Map a yel gc-variant case index to its canonical-ABI / WIT discriminant.
    /// yel orders `option` as `[some=0, none=1]`, but the WIT/component-model
    /// `option<T>` is `none=0, some=1` — so swap those two at the boundary.
    /// `result` (ok=0, err=1) and user variants already match the WIT case
    /// order, so their index passes through. Applied wherever a case index is
    /// written as (getter) or matched against (setter) the boundary
    /// discriminant.
    fn gc_variant_wit_disc(&self, ty: Ty, case_idx: u32) -> u32 {
        if matches!(self.ctx.ty_kind(ty), InternedTyKind::Option(_)) {
            1 - case_idx
        } else {
            case_idx
        }
    }



    /// Reach-generic GcVariant lift: dispatch on the value's active case via
    /// a `ref.test` cascade and write the canonical-ABI discriminant + payload
    /// bytes at `scratch_ptr_local + base_offset + slot.offset`. The outer ref
    /// is reached through `source` (self-chain field or list array element), so
    /// this one body serves both the signal / record-field getters and the
    /// `list<gc-variant>` materializer. `mat_ptr_local` / `mat_len_local` are
    /// caller-reserved scratch i32s for inner string / list materializer
    /// returns.
    fn emit_gc_variant_lift(
        &mut self,
        func: &mut Function,
        source: GcRefSource,
        gc_variant_ty: Ty,
        base_offset: u32,
        scratch_ptr_local: u32,
        mat_ptr_local: u32,
        mat_len_local: u32,
    ) -> Result<(), CodegenError> {
        use super::scratch::mem_arg;
        let canonical_slots = self.flatten_core_slots(gc_variant_ty);
        let disc_offset = canonical_slots.first().map(|s| s.offset).ok_or_else(|| {
            CodegenError::InvalidIR(format!(
                "GcVariant lift: empty canonical layout for {:?}",
                gc_variant_ty
            ))
        })?;
        let case_count = *self
            .record_gc_types
            .gc_variant_case_count
            .get(&gc_variant_ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "GcVariant lift: missing case count for {:?}",
                    gc_variant_ty
                ))
            })?;

        // Outer block lets a matching case skip remaining tests +
        // fall-through default.
        func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));

        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .gc_variant_case_idx
                .get(&(gc_variant_ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "GcVariant lift: missing case_idx for ({:?}, {})",
                        gc_variant_ty, k
                    ))
                })?;

            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::RefTestNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // disc = k
            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
            let disc_abs = base_offset + disc_offset;
            if disc_abs != 0 {
                func.instruction(&Instruction::I32Const(disc_abs as i32));
                func.instruction(&Instruction::I32Add);
            }
            func.instruction(&Instruction::I32Const(self.gc_variant_wit_disc(gc_variant_ty, k) as i32));
            func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));

            if let Some(payload_ty) =
                super::super::gc_types::case_payload_ty(self.ctx, gc_variant_ty, k)
            {
                // Delegate to the one reach-generic payload lift (scalar /
                // string / list / nested gc-variant). The value's canonical slots
                // carry value-relative offsets, so shift them by `base_offset`
                // to absolute before handing them over.
                let abs_slots: Vec<crate::wasm::FlatSlot> = canonical_slots
                    .iter()
                    .map(|s| crate::wasm::FlatSlot {
                        offset: base_offset + s.offset,
                        ..*s
                    })
                    .collect();
                self.emit_gc_variant_payload_lift(
                    func,
                    source,
                    case_sub_idx,
                    payload_ty,
                    &abs_slots,
                    scratch_ptr_local,
                    mat_ptr_local,
                    mat_len_local,
                )?;
            }

            func.instruction(&Instruction::Br(1));
            func.instruction(&Instruction::End);
        }

        // Default: write disc=0 (defensive — uninitialized ref).
        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
        let disc_abs = base_offset + disc_offset;
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

    /// Push the GcVariant supertype ref described by `source` onto the
    /// stack — the reach-generic entry the composite payload lift re-emits per
    /// case. See [`GcRefSource`].
    fn emit_gc_ref(
        &self,
        func: &mut Function,
        source: GcRefSource,
    ) -> Result<(), CodegenError> {
        match source {
            GcRefSource::SelfChain { ci, chain } => self.emit_gc_field_chain(func, ci, chain),
            GcRefSource::LocalChain { ref_local, chain } => {
                func.instruction(&Instruction::LocalGet(ref_local));
                for (idx, &(struct_type_index, field_index)) in chain.iter().enumerate() {
                    if idx > 0 {
                        func.instruction(&Instruction::RefAsNonNull);
                    }
                    func.instruction(&Instruction::StructGet {
                        struct_type_index,
                        field_index,
                    });
                }
                Ok(())
            }
            GcRefSource::ArrayElem {
                arr_local,
                idx_local,
                arr_type_idx,
            } => {
                func.instruction(&Instruction::LocalGet(arr_local));
                func.instruction(&Instruction::LocalGet(idx_local));
                func.instruction(&Instruction::ArrayGet(arr_type_idx));
                Ok(())
            }
            GcRefSource::PayloadOf {
                inner,
                case_sub_idx,
                chain,
            } => {
                // <inner supertype ref>; ref.cast to the active case; struct.get
                // the payload field (a scalar / nested-gc-variant / record /
                // tuple ref), then walk any trailing member `struct.get` chain
                // to reach a composite payload's field.
                self.emit_gc_ref(func, *inner)?;
                func.instruction(&Instruction::RefCastNonNull(
                    wasm_encoder::HeapType::Concrete(case_sub_idx),
                ));
                func.instruction(&Instruction::StructGet {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                });
                for &(struct_type_index, field_index) in chain {
                    func.instruction(&Instruction::RefAsNonNull);
                    func.instruction(&Instruction::StructGet {
                        struct_type_index,
                        field_index,
                    });
                }
                Ok(())
            }
        }
    }



    /// Describe a composite (record ADT / tuple) as its GC struct type index
    /// plus per-member canonical layout; `None` for non-composite types.
    fn composite_gc_members(
        &mut self,
        ty: Ty,
    ) -> Result<Option<(u32, Vec<CompositeMember>)>, CodegenError> {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::Adt(d) if matches!(self.ctx.defs.kind(*d), DefKind::Record(_)) => {
                let record_def_id = *d;
                let record_def = match self.ctx.defs.kind(record_def_id) {
                    DefKind::Record(r) => r.clone(),
                    _ => unreachable!("guarded by match above"),
                };
                let struct_type_index = self
                    .record_gc_types
                    .record_type_idx
                    .get(&record_def_id)
                    .copied()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("composite members: missing record_type_idx".into())
                    })?;
                let gc_field_indices: Vec<u32> = self
                    .record_gc_types
                    .field_gc_indices
                    .get(&record_def_id)
                    .cloned()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("composite members: missing gc field indices".into())
                    })?;
                let layout = self
                    .layout_ctx
                    .record_layout_by_id(record_def_id)
                    .ok_or_else(|| {
                        CodegenError::InvalidIR("composite members: missing record layout".into())
                    })?
                    .clone();
                let mut members = Vec::with_capacity(record_def.fields.len());
                for (i, &field_def_id) in record_def.fields.iter().enumerate() {
                    let field_ty = match self.ctx.defs.kind(field_def_id) {
                        DefKind::Field(f) => f.ty,
                        _ => {
                            return Err(CodegenError::InvalidIR(
                                "composite members: not a field def".into(),
                            ));
                        }
                    };
                    let (_name, field_offset, _t) =
                        layout.field_offsets.get(i).cloned().ok_or_else(|| {
                            CodegenError::InvalidIR("composite members: missing field offset".into())
                        })?;
                    members.push(CompositeMember {
                        ty: field_ty,
                        canonical_offset: field_offset,
                        gc_field_index: gc_field_indices[i],
                    });
                }
                Ok(Some((struct_type_index, members)))
            }
            InternedTyKind::Tuple(elements) => {
                let elements: Vec<Ty> = elements.to_vec();
                let struct_type_index = self
                    .record_gc_types
                    .tuple_struct_type_idx
                    .get(&ty)
                    .copied()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "composite members: missing tuple_struct_type_idx".into(),
                        )
                    })?;
                let mut members = Vec::with_capacity(elements.len());
                let mut offset = 0u32;
                for (i, &elem_ty) in elements.iter().enumerate() {
                    let elem_layout = self.layout_ctx.layout_of(elem_ty);
                    offset = (offset + elem_layout.align - 1) & !(elem_layout.align - 1);
                    members.push(CompositeMember {
                        ty: elem_ty,
                        canonical_offset: offset,
                        gc_field_index: i as u32,
                    });
                    offset += elem_layout.size;
                }
                Ok(Some((struct_type_index, members)))
            }
            _ => Ok(None),
        }
    }

    /// Push one scalar value from its canonical `source` onto the stack.
    fn push_canonical_scalar(&self, func: &mut Function, source: CanonicalSource, ty: Ty) {
        match source {
            CanonicalSource::Params { first_param } => {
                func.instruction(&Instruction::LocalGet(first_param));
            }
            CanonicalSource::Memory {
                address_local,
                offset,
            } => {
                func.instruction(&Instruction::LocalGet(address_local));
                if offset != 0 {
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                emit_canonical_scalar_load(func, self.ctx, ty);
            }
        }
    }

    /// Push a canonical `(ptr, len)` pair from `source` onto the stack.
    fn push_canonical_ptr_len(&self, func: &mut Function, source: CanonicalSource) {
        match source {
            CanonicalSource::Params { first_param } => {
                func.instruction(&Instruction::LocalGet(first_param));
                func.instruction(&Instruction::LocalGet(first_param + 1));
            }
            CanonicalSource::Memory {
                address_local,
                offset,
            } => {
                for part_offset in [offset, offset + 4] {
                    func.instruction(&Instruction::LocalGet(address_local));
                    if part_offset != 0 {
                        func.instruction(&Instruction::I32Const(part_offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                }
            }
        }
    }

    /// Resolve the un-materializer function for a string (`$str_bytes`) or
    /// typed-list type.
    fn ptr_len_unmaterializer(&self, ty: Ty) -> Result<u32, CodegenError> {
        let arr_idx = if matches!(self.ctx.ty_kind(ty), InternedTyKind::String) {
            self.record_gc_types.str_bytes_array_idx
        } else {
            self.record_gc_types.list_array_type_idx.get(&ty).copied()
        }
        .ok_or_else(|| {
            CodegenError::InvalidIR(format!("canonical pack: no GC array type for {:?}", ty))
        })?;
        self.gc_list_unmaterializer_fn_indices
            .get(&arr_idx)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "canonical pack: missing un-materializer for arr {}",
                    arr_idx
                ))
            })
    }

    /// Resolve the materializer function for a string (`$str_bytes`) or
    /// typed-list type.
    fn ptr_len_materializer(&self, ty: Ty) -> Result<u32, CodegenError> {
        let arr_idx = if matches!(self.ctx.ty_kind(ty), InternedTyKind::String) {
            self.record_gc_types.str_bytes_array_idx
        } else {
            self.record_gc_types.list_array_type_idx.get(&ty).copied()
        }
        .ok_or_else(|| {
            CodegenError::InvalidIR(format!("canonical lift: no GC array type for {:?}", ty))
        })?;
        self.gc_list_materializer_fn_indices
            .get(&arr_idx)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "canonical lift: missing materializer for arr {}",
                    arr_idx
                ))
            })
    }

    /// Build a composite (record / tuple) GC struct from its canonical-ABI
    /// representation, leaving one `(ref null $composite)` on the stack.
    /// Members are read from `source` — flattened params or canonical memory —
    /// and dispatched by shape via [`Self::emit_member_pack`].
    fn emit_composite_pack(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: CanonicalSource,
        declared_vts: &[wasm_encoder::ValType],
    ) -> Result<(), CodegenError> {
        let (struct_type_index, members) = self.composite_gc_members(ty)?.ok_or_else(|| {
            CodegenError::InvalidIR(format!("composite pack: {:?} is not a record/tuple", ty))
        })?;
        let mut flat_param_offset: u32 = 0;
        for member in &members {
            let member_source = match source {
                CanonicalSource::Params { first_param } => CanonicalSource::Params {
                    first_param: first_param + flat_param_offset,
                },
                CanonicalSource::Memory {
                    address_local,
                    offset,
                } => CanonicalSource::Memory {
                    address_local,
                    offset: offset + member.canonical_offset,
                },
            };
            let member_flat = self.canonical_flat_valtypes(member.ty).len() as u32;
            // A record's fields concatenate in canonical order, so this
            // member's declared valtypes are the corresponding window of the
            // parent's declared region.
            let member_declared: &[wasm_encoder::ValType] = declared_vts
                .get(flat_param_offset as usize..(flat_param_offset + member_flat) as usize)
                .unwrap_or(&[]);
            flat_param_offset += member_flat;
            self.emit_member_pack(func, member.ty, member_source, member_declared)?;
        }
        func.instruction(&Instruction::StructNew(struct_type_index));
        Ok(())
    }

    /// Pack one value from its canonical representation to its GC form,
    /// leaving it on the stack (typically for a parent `struct.new`):
    /// gc-variant (migrated option / result / variant) → disc-dispatch pack;
    /// collapsed `option<composite/list>` → null-check + inner pack;
    /// nested record / tuple → recurse; string / typed list → (ptr, len) +
    /// un-materializer; scalar / enum → direct read.
    /// `declared_vts` gives the actual declared valtypes of this value's
    /// canonical slots as they appear in the source (only meaningful for
    /// `Params`). At the top level these equal the value's own canonical
    /// valtypes, but when the value is a variant case payload the parent's
    /// `join` may have widened shared slots — each `Params` read narrows the
    /// declared width down to the value's natural width via
    /// `emit_canonical_reinterpret` (a no-op when they match). `Memory` reads
    /// at natural offsets and ignore `declared_vts`.
    fn emit_member_pack(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: CanonicalSource,
        declared_vts: &[wasm_encoder::ValType],
    ) -> Result<(), CodegenError> {
        if self.is_gc_variant(ty) {
            return self.emit_pack_canonical_to_gc_variant(func, ty, source, declared_vts);
        }
        // Collapsed option: canonical [disc, ...inner]; disc != 0 builds the
        // inner ref, else a typed null.
        if let Some(arr_idx) = self.option_collapses_to_ref(ty) {
            let inner_ty = match self.ctx.ty_kind(ty) {
                InternedTyKind::Option(t) => *t,
                _ => unreachable!("option_collapses_to_ref non-option"),
            };
            let slots = self.flatten_core_slots(ty);
            // An empty inner (e.g. `option<record {}>`) flattens to just
            // `[disc]` — there is no payload slot. `some` still builds the
            // inner ref (an empty `struct.new`, reading no payload params /
            // bytes), so a missing payload slot is fine: the dummy offset is
            // never read. Non-empty inners have a real payload offset here.
            let payload_offset = slots.get(1).map(|s| s.offset).unwrap_or(0);
            match source {
                CanonicalSource::Params { first_param } => {
                    func.instruction(&Instruction::LocalGet(first_param));
                    // disc is naturally i32; narrow a joined-widened param.
                    if let Some(&vt) = declared_vts.first() {
                        emit_canonical_reinterpret(func, vt, ValType::I32)?;
                    }
                }
                CanonicalSource::Memory {
                    address_local,
                    offset,
                } => {
                    let disc_offset = offset + slots[0].offset;
                    func.instruction(&Instruction::LocalGet(address_local));
                    if disc_offset != 0 {
                        func.instruction(&Instruction::I32Const(disc_offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::I32Load8U(super::scratch::mem_arg(0, 0)));
                }
            }
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(arr_idx),
                }),
            )));
            let inner_source = match source {
                CanonicalSource::Params { first_param } => CanonicalSource::Params {
                    first_param: first_param + 1,
                },
                CanonicalSource::Memory {
                    address_local,
                    offset,
                } => CanonicalSource::Memory {
                    address_local,
                    offset: offset + payload_offset,
                },
            };
            self.emit_member_pack(func, inner_ty, inner_source, declared_vts.get(1..).unwrap_or(&[]))?;
            func.instruction(&Instruction::Else);
            func.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
                arr_idx,
            )));
            func.instruction(&Instruction::End);
            return Ok(());
        }
        if self.composite_gc_members(ty)?.is_some() {
            return self.emit_composite_pack(func, ty, source, declared_vts);
        }
        if matches!(self.ctx.ty_kind(ty), InternedTyKind::String)
            || (matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_))
                && self.record_gc_types.list_array_type_idx.contains_key(&ty))
        {
            let unmat_fn = self.ptr_len_unmaterializer(ty)?;
            match source {
                CanonicalSource::Params { first_param } => {
                    // (ptr, len) — two i32 slots; narrow any joined-widened
                    // param before the un-materializer consumes them.
                    for i in 0..2u32 {
                        func.instruction(&Instruction::LocalGet(first_param + i));
                        if let Some(&vt) = declared_vts.get(i as usize) {
                            emit_canonical_reinterpret(func, vt, ValType::I32)?;
                        }
                    }
                }
                CanonicalSource::Memory { .. } => {
                    self.push_canonical_ptr_len(func, source);
                }
            }
            func.instruction(&Instruction::Call(unmat_fn));
            return Ok(());
        }
        if matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_)) {
            return Err(CodegenError::InvalidIR(
                "member pack: list has no typed GC array".into(),
            ));
        }
        // Scalar / enum. Param-sourced values pass every flat slot through,
        // narrowing any joined-widened param to the value's natural width;
        // memory-sourced values are a single typed load.
        match source {
            CanonicalSource::Params { first_param } => {
                let flat = self.canonical_flat_valtypes(ty);
                for (i, &vt_natural) in flat.iter().enumerate() {
                    func.instruction(&Instruction::LocalGet(first_param + i as u32));
                    let vt_declared = declared_vts.get(i).copied().unwrap_or(vt_natural);
                    emit_canonical_reinterpret(func, vt_declared, vt_natural)?;
                }
            }
            CanonicalSource::Memory { .. } => {
                self.push_canonical_scalar(func, source, ty);
            }
        }
        Ok(())
    }

    /// Pack a GcVariant value (migrated option / result / variant) from its
    /// canonical representation `[disc, ...payload]` into a `(ref null $sup)`
    /// left on the stack, via an `if disc == k … else … else
    /// struct.new_default(case0)` cascade.
    ///
    /// Param-sourced packs bridge each canonical slot from the **declared**
    /// param valtype back to this value's own natural width. `declared_vts`
    /// gives the actual declared valtypes of this value's canonical slots
    /// `[disc, ...payload]`. At the top level these equal the value's own
    /// canonical valtypes, but when this value is a **nested flat-gc payload**
    /// of a wider parent variant, the parent's `join` may have widened shared
    /// slots (e.g. `variant { a(result<s32,s32>), b(s64) }` widens slot 0 to
    /// i64, so the nested result's disc param is declared i64 and must be
    /// narrowed to i32). The recursion threads the parent's payload region
    /// (`declared_vts[1..]`) down so every nesting level bridges its own
    /// shared slots. Memory-sourced packs read each case's payload at its
    /// natural width from its own offset, so `declared_vts` is unused there.
    fn emit_pack_canonical_to_gc_variant(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: CanonicalSource,
        declared_vts: &[wasm_encoder::ValType],
    ) -> Result<(), CodegenError> {
        let slots = self.flatten_core_slots(ty);
        let disc_slot_offset = slots.first().map(|s| s.offset).unwrap_or(0);
        let payload_slot_offset = slots.get(1).map(|s| s.offset);
        let case_count = *self
            .record_gc_types
            .gc_variant_case_count
            .get(&ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "pack canonical->gc-variant: missing case count for {:?}",
                    ty
                ))
            })?;
        let super_idx = *self
            .record_gc_types
            .gc_variant_super_idx
            .get(&ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "pack canonical->gc-variant: missing super idx for {:?}",
                    ty
                ))
            })?;
        let result_ty =
            wasm_encoder::BlockType::Result(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(super_idx),
            }));

        let mut nesting: u32 = 0;
        for k in 0..case_count {
            let case_sub_idx = *self
                .record_gc_types
                .gc_variant_case_idx
                .get(&(ty, k))
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "pack canonical->gc-variant: missing case_idx for ({:?}, {})",
                        ty, k
                    ))
                })?;
            // disc == wit_disc(k) ?
            match source {
                CanonicalSource::Params { first_param } => {
                    func.instruction(&Instruction::LocalGet(first_param));
                    // The disc slot is naturally i32, but a wider parent may
                    // have declared this shared slot at a joined width — narrow
                    // it back down before the i32 comparison.
                    if let Some(&vt_declared) = declared_vts.first() {
                        emit_canonical_reinterpret(func, vt_declared, wasm_encoder::ValType::I32)?;
                    }
                }
                CanonicalSource::Memory {
                    address_local,
                    offset,
                } => {
                    let disc_offset = offset + disc_slot_offset;
                    func.instruction(&Instruction::LocalGet(address_local));
                    if disc_offset != 0 {
                        func.instruction(&Instruction::I32Const(disc_offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::I32Load8U(super::scratch::mem_arg(0, 0)));
                }
            }
            func.instruction(&Instruction::I32Const(self.gc_variant_wit_disc(ty, k) as i32));
            func.instruction(&Instruction::I32Eq);
            func.instruction(&Instruction::If(result_ty));
            if let Some(payload_ty) = super::super::gc_types::case_payload_ty(self.ctx, ty, k) {
                let payload_source = match source {
                    CanonicalSource::Params { first_param } => CanonicalSource::Params {
                        first_param: first_param + 1,
                    },
                    CanonicalSource::Memory {
                        address_local,
                        offset,
                    } => CanonicalSource::Memory {
                        address_local,
                        offset: offset
                            + payload_slot_offset.ok_or_else(|| {
                                CodegenError::InvalidIR(
                                    "pack canonical->gc-variant: missing payload slot".into(),
                                )
                            })?,
                    },
                };
                if matches!(
                    self.internal_repr(payload_ty),
                    super::super::repr::InternalRepr::GcVariant(_)
                ) {
                    // Nested gc-variant payload: recurse. Its canonical slots
                    // share the outer payload region, so the actual declared
                    // param valtypes for the child are this variant's payload
                    // region (`declared_vts[1..]`) — pass them so the child
                    // bridges each slot the outer `join` widened.
                    let child_declared: &[wasm_encoder::ValType] =
                        declared_vts.get(1..).unwrap_or(&[]);
                    self.emit_pack_canonical_to_gc_variant(
                        func,
                        payload_ty,
                        payload_source,
                        child_declared,
                    )?;
                    func.instruction(&Instruction::StructNew(case_sub_idx));
                } else if self.composite_gc_members(payload_ty)?.is_some()
                    || self.option_collapses_to_ref(payload_ty).is_some()
                {
                    // Record / tuple / collapsed-option payload: build the
                    // payload ref from the case's payload region, then wrap it
                    // in the case subtype. `declared_vts[1..]` are the actual
                    // (possibly parent-joined) param widths for the payload
                    // region so the pack narrows each shared slot down to the
                    // payload's natural width.
                    let child_declared: &[wasm_encoder::ValType] =
                        declared_vts.get(1..).unwrap_or(&[]);
                    self.emit_member_pack(func, payload_ty, payload_source, child_declared)?;
                    func.instruction(&Instruction::StructNew(case_sub_idx));
                } else {
                    let is_ptr_len_payload = matches!(
                        self.ctx.ty_kind(payload_ty),
                        InternedTyKind::String
                    ) || (matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_))
                        && self
                            .record_gc_types
                            .list_array_type_idx
                            .contains_key(&payload_ty));
                    match payload_source {
                        CanonicalSource::Params { first_param } => {
                            // Push each payload slot, bridging the declared
                            // (possibly parent-joined) width down to the case's
                            // own natural width.
                            let payload_flat = self.canonical_flat_valtypes(payload_ty);
                            for (i, vt_payload) in payload_flat.iter().enumerate() {
                                func.instruction(&Instruction::LocalGet(first_param + i as u32));
                                let vt_declared =
                                    declared_vts.get(1 + i).copied().unwrap_or(*vt_payload);
                                emit_canonical_reinterpret(func, vt_declared, *vt_payload)?;
                            }
                        }
                        CanonicalSource::Memory { .. } => {
                            if is_ptr_len_payload {
                                self.push_canonical_ptr_len(func, payload_source);
                            } else {
                                self.push_canonical_scalar(func, payload_source, payload_ty);
                            }
                        }
                    }
                    if is_ptr_len_payload {
                        // Rebuild the GC ref from the pushed (ptr, len).
                        let unmat_fn = self.ptr_len_unmaterializer(payload_ty)?;
                        func.instruction(&Instruction::Call(unmat_fn));
                    }
                    func.instruction(&Instruction::StructNew(case_sub_idx));
                }
            } else {
                func.instruction(&Instruction::StructNewDefault(case_sub_idx));
            }
            func.instruction(&Instruction::Else);
            nesting += 1;
        }
        // Innermost else: invariant violation (host sent an invalid disc).
        // Push a default case-0 subtype to satisfy the block result type.
        let case0_sub_idx = *self
            .record_gc_types
            .gc_variant_case_idx
            .get(&(ty, 0))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "pack canonical->gc-variant: missing case_idx for ({:?}, 0)",
                    ty
                ))
            })?;
        func.instruction(&Instruction::StructNewDefault(case0_sub_idx));
        for _ in 0..nesting {
            func.instruction(&Instruction::End);
        }
        Ok(())
    }







    /// **Boundary-only.** Lower a composite (record / tuple) GC struct to its
    /// canonical-ABI bytes at `address_local + base_offset`. The composite is
    /// reached through `source` — a `SelfChain` (component signal / nested
    /// field) or a `LocalChain` (typed ref in a local) — and each member is
    /// reached by extending that chain with its GC field, so nested
    /// composites recurse without extra locals. `scratch_ptr_len` are two
    /// caller-reserved i32 locals for materializer (ptr, len) returns,
    /// required for string / list / gc-variant members. Internal values never
    /// take this path — records/tuples stay GC refs end-to-end.
    fn emit_composite_lift_to_memory(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: GcRefSource,
        address_local: u32,
        base_offset: u32,
        scratch_ptr_len: Option<(u32, u32)>,
    ) -> Result<(), CodegenError> {
        let (struct_type_index, members) = self.composite_gc_members(ty)?.ok_or_else(|| {
            CodegenError::InvalidIR(format!("composite lift: {:?} is not a record/tuple", ty))
        })?;
        for member in &members {
            let absolute_offset = base_offset + member.canonical_offset;
            let member_hop = (struct_type_index, member.gc_field_index);
            let member_chain: Vec<(u32, u32)>;
            let member_source = match source {
                GcRefSource::SelfChain { ci, chain } => {
                    member_chain = chain
                        .iter()
                        .copied()
                        .chain(std::iter::once(member_hop))
                        .collect();
                    GcRefSource::SelfChain {
                        ci,
                        chain: &member_chain,
                    }
                }
                GcRefSource::LocalChain { ref_local, chain } => {
                    member_chain = chain
                        .iter()
                        .copied()
                        .chain(std::iter::once(member_hop))
                        .collect();
                    GcRefSource::LocalChain {
                        ref_local,
                        chain: &member_chain,
                    }
                }
                GcRefSource::PayloadOf {
                    inner,
                    case_sub_idx,
                    chain,
                } => {
                    // A record / tuple carried as a variant case payload:
                    // extend the after-payload `struct.get` chain with this
                    // member's hop.
                    member_chain = chain
                        .iter()
                        .copied()
                        .chain(std::iter::once(member_hop))
                        .collect();
                    GcRefSource::PayloadOf {
                        inner,
                        case_sub_idx,
                        chain: &member_chain,
                    }
                }
                _ => {
                    return Err(CodegenError::InvalidIR(
                        "composite lift: source must be a self chain, a typed local, or a \
                         variant payload".into(),
                    ));
                }
            };
            self.emit_member_lift_to_memory(
                func,
                member.ty,
                member_source,
                address_local,
                absolute_offset,
                scratch_ptr_len,
            )?;
        }
        Ok(())
    }

    /// Lower one GC value (reached through `source`) to its canonical-ABI
    /// bytes at `address_local + offset`: gc-variant → case-dispatch lift;
    /// collapsed option → disc from the null-check, inner lift on some,
    /// zero-filled payload on none; nested composite → recurse; string /
    /// typed list → materialize to (ptr, len); scalar / enum → typed store.
    pub(in crate::wasm) fn emit_member_lift_to_memory(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: GcRefSource,
        address_local: u32,
        offset: u32,
        scratch_ptr_len: Option<(u32, u32)>,
    ) -> Result<(), CodegenError> {
        if self.is_gc_variant(ty) {
            let (scratch_ptr, scratch_len) = scratch_ptr_len.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "member lift: gc-variant member requires scratch i32 locals".into(),
                )
            })?;
            return self.emit_gc_variant_lift(
                func,
                source,
                ty,
                offset,
                address_local,
                scratch_ptr,
                scratch_len,
            );
        }
        // Collapsed option: storage is one nullable ref; canonical shape is
        // [disc, ...inner]. disc = !ref.is_null; on some lower the inner at
        // the payload offset, on none zero-fill the payload slots.
        if self.option_collapses_to_ref(ty).is_some() {
            let inner_ty = match self.ctx.ty_kind(ty) {
                InternedTyKind::Option(t) => *t,
                _ => unreachable!("option_collapses_to_ref non-option"),
            };
            let slots = self.flatten_core_slots(ty);
            let disc_offset = offset + slots.first().map(|s| s.offset).unwrap_or(0);
            let payload_offset = offset + slots.get(1).map(|s| s.offset).unwrap_or(0);
            func.instruction(&Instruction::LocalGet(address_local));
            if disc_offset != 0 {
                func.instruction(&Instruction::I32Const(disc_offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::RefIsNull);
            func.instruction(&Instruction::I32Eqz);
            func.instruction(&Instruction::I32Store8(super::scratch::mem_arg(0, 0)));
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::RefIsNull);
            func.instruction(&Instruction::I32Eqz);
            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            self.emit_member_lift_to_memory(
                func,
                inner_ty,
                source,
                address_local,
                payload_offset,
                scratch_ptr_len,
            )?;
            func.instruction(&Instruction::Else);
            for slot in slots.iter().skip(1) {
                func.instruction(&Instruction::LocalGet(address_local));
                let slot_offset = offset + slot.offset;
                if slot_offset != 0 {
                    func.instruction(&Instruction::I32Const(slot_offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                match slot.store {
                    super::super::StoreWidth::I64 => {
                        func.instruction(&Instruction::I64Const(0));
                    }
                    super::super::StoreWidth::F32 => {
                        func.instruction(&Instruction::F32Const(0.0.into()));
                    }
                    super::super::StoreWidth::F64 => {
                        func.instruction(&Instruction::F64Const(0.0.into()));
                    }
                    _ => {
                        func.instruction(&Instruction::I32Const(0));
                    }
                }
                slot.store.emit_store(func);
            }
            func.instruction(&Instruction::End);
            return Ok(());
        }
        if self.composite_gc_members(ty)?.is_some() {
            return self.emit_composite_lift_to_memory(
                func,
                ty,
                source,
                address_local,
                offset,
                scratch_ptr_len,
            );
        }
        if matches!(self.ctx.ty_kind(ty), InternedTyKind::String)
            || (matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_))
                && self.record_gc_types.list_array_type_idx.contains_key(&ty))
        {
            let mat_fn = self.ptr_len_materializer(ty)?;
            let (scratch_ptr, scratch_len) = scratch_ptr_len.ok_or_else(|| {
                CodegenError::InvalidIR(
                    "member lift: string/list member requires scratch i32 locals".into(),
                )
            })?;
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::Call(mat_fn));
            store_canonical_ptr_len(
                func,
                address_local,
                offset,
                offset + 4,
                scratch_ptr,
                scratch_len,
            );
            return Ok(());
        }
        // Scalar / enum: one typed store per canonical slot (a scalar is a
        // single slot; the source is re-read per slot to keep the value typed).
        let slots = self.flatten_core_slots(ty);
        for slot in slots.iter() {
            func.instruction(&Instruction::LocalGet(address_local));
            let slot_offset = offset + slot.offset;
            if slot_offset != 0 {
                func.instruction(&Instruction::I32Const(slot_offset as i32));
                func.instruction(&Instruction::I32Add);
            }
            self.emit_gc_ref(func, source)?;
            slot.store.emit_store(func);
        }
        Ok(())
    }

    /// Direct twin of [`Self::emit_member_lift_to_memory`]: push a value of
    /// type `ty`, reached through `source`, onto the WASM stack as exactly its
    /// canonical-ABI flat slots (`flatten_core_valtypes(ty)`) in declaration
    /// order — **no linear-memory buffer, no alloc/free**. Used to pass a
    /// composite argument by value to a host callback import.
    ///
    /// gc-variant / collapsed-option cases are produced through a WASM `if`
    /// whose block type is the value's own canonical shape (pre-interned in
    /// `ternary_block_types`), so nested composites compose without staging
    /// locals. A cross-case `join` that would widen a **non-top** stack slot
    /// of a multi-slot payload returns a loud [`CodegenError`] rather than
    /// miscompile (staging that slot is out of scope for this path).
    pub(in crate::wasm) fn emit_value_to_canonical_stack(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: GcRefSource,
    ) -> Result<(), CodegenError> {
        use yel_core::types::InternedTyKind;
        // gc-variant (option<scalar> / result / user variant): multi-case
        // dispatch producing [i32 disc, ...joined payload].
        if self.is_gc_variant(ty) {
            return self.emit_gc_variant_to_canonical_stack(func, ty, source);
        }
        // Collapsed option (inner has a ref repr): [i32 disc, ...inner].
        if self.option_collapses_to_ref(ty).is_some() {
            let inner_ty = match self.ctx.ty_kind(ty) {
                InternedTyKind::Option(t) => *t,
                _ => unreachable!("option_collapses_to_ref on non-option"),
            };
            return self.emit_collapsed_option_to_canonical_stack(func, ty, inner_ty, source);
        }
        // Record / tuple: push each member's canonical slots in order.
        if let Some((struct_type_index, members)) = self.composite_gc_members(ty)? {
            for member in &members {
                let hop = (struct_type_index, member.gc_field_index);
                let member_chain: Vec<(u32, u32)>;
                let member_source = match source {
                    GcRefSource::SelfChain { ci, chain } => {
                        member_chain = chain.iter().copied().chain(std::iter::once(hop)).collect();
                        GcRefSource::SelfChain {
                            ci,
                            chain: &member_chain,
                        }
                    }
                    GcRefSource::LocalChain { ref_local, chain } => {
                        member_chain = chain.iter().copied().chain(std::iter::once(hop)).collect();
                        GcRefSource::LocalChain {
                            ref_local,
                            chain: &member_chain,
                        }
                    }
                    GcRefSource::PayloadOf {
                        inner,
                        case_sub_idx,
                        chain,
                    } => {
                        member_chain =
                            chain.iter().copied().chain(std::iter::once(hop)).collect();
                        GcRefSource::PayloadOf {
                            inner,
                            case_sub_idx,
                            chain: &member_chain,
                        }
                    }
                    GcRefSource::ArrayElem { .. } => {
                        return Err(CodegenError::InvalidIR(
                            "value->stack: record/tuple member source must be a self/local/\
                             payload chain"
                                .into(),
                        ));
                    }
                };
                self.emit_value_to_canonical_stack(func, member.ty, member_source)?;
            }
            return Ok(());
        }
        // String / typed list: the materializer leaves (ptr, len) on the stack
        // — already the two canonical slots in order, no scratch needed.
        if matches!(self.ctx.ty_kind(ty), InternedTyKind::String)
            || (matches!(self.ctx.ty_kind(ty), InternedTyKind::List(_))
                && self.record_gc_types.list_array_type_idx.contains_key(&ty))
        {
            let mat_fn = self.ptr_len_materializer(ty)?;
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::Call(mat_fn));
            return Ok(());
        }
        // Scalar / enum: a single canonical slot. Reading the value through
        // `source` (a record/tuple field is stored full-width) yields the
        // canonical slot valtype directly — no join at this level.
        let slots = self.flatten_core_slots(ty);
        if slots.len() != 1 {
            return Err(CodegenError::InvalidIR(format!(
                "value->stack: type {:?} is neither composite nor a single-slot scalar \
                 ({} canonical slots)",
                ty,
                slots.len()
            )));
        }
        self.emit_gc_ref(func, source)?;
        Ok(())
    }

    /// Collapsed `option<T>` (T has a ref repr): push `[i32 disc, ...inner]`.
    /// disc = `!ref.is_null` (some=1, none=0 — the WIT option discriminant);
    /// on some recurse the inner (the option ref *is* the inner ref), on none
    /// zero-pad the inner slots.
    fn emit_collapsed_option_to_canonical_stack(
        &mut self,
        func: &mut Function,
        ty: Ty,
        inner_ty: Ty,
        source: GcRefSource,
    ) -> Result<(), CodegenError> {
        let canon = self.flatten_core_valtypes(ty);
        let block_ty = self.canonical_block_type(ty)?;
        self.emit_gc_ref(func, source)?;
        func.instruction(&Instruction::RefIsNull);
        func.instruction(&Instruction::If(block_ty));
        // NONE (ref is null): disc 0, zero-pad the inner slots.
        func.instruction(&Instruction::I32Const(0));
        for &vt in &canon[1..] {
            push_zero_valtype(func, vt)?;
        }
        func.instruction(&Instruction::Else);
        // SOME: disc 1, then the inner value's canonical slots. A collapsed
        // option has a single payload case, so the inner's canonical slots
        // exactly fill `canon[1..]` — no join widening.
        func.instruction(&Instruction::I32Const(1));
        self.emit_value_to_canonical_stack(func, inner_ty, source)?;
        func.instruction(&Instruction::End);
        Ok(())
    }

    /// gc-variant (non-collapsed option / result / user variant): push
    /// `[i32 disc, ...joined payload]` via a `ref.test` if/else cascade, each
    /// branch producing the full canonical shape (block type = the variant's
    /// canonical shape).
    fn emit_gc_variant_to_canonical_stack(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: GcRefSource,
    ) -> Result<(), CodegenError> {
        let canon_vts = self.flatten_core_valtypes(ty);
        let block_ty = self.canonical_block_type(ty)?;
        let case_count = *self
            .record_gc_types
            .gc_variant_case_count
            .get(&ty)
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "value->stack gc-variant: missing case count for {:?}",
                    ty
                ))
            })?;
        self.emit_variant_case_chain(func, ty, source, block_ty, case_count, 0, &canon_vts)
    }

    /// Recursive if/else spine for [`Self::emit_gc_variant_to_canonical_stack`].
    /// Case `k`: `ref.test` the case subtype; then-branch produces
    /// `[disc(k), ...payload(k) widened/padded]`, else-branch recurses to
    /// `k+1`. After the last case a default branch produces `[0, ...zeros]`.
    #[allow(clippy::too_many_arguments)]
    fn emit_variant_case_chain(
        &mut self,
        func: &mut Function,
        ty: Ty,
        source: GcRefSource,
        block_ty: wasm_encoder::BlockType,
        case_count: u32,
        k: u32,
        canon_vts: &[wasm_encoder::ValType],
    ) -> Result<(), CodegenError> {
        if k == case_count {
            // Default (no case matched — defensive; a valid non-null value
            // always matches one case): disc 0 + zero payload.
            func.instruction(&Instruction::I32Const(0));
            for &vt in &canon_vts[1..] {
                push_zero_valtype(func, vt)?;
            }
            return Ok(());
        }
        let case_sub_idx = *self
            .record_gc_types
            .gc_variant_case_idx
            .get(&(ty, k))
            .ok_or_else(|| {
                CodegenError::InvalidIR(format!(
                    "value->stack gc-variant: missing case_idx for ({:?}, {})",
                    ty, k
                ))
            })?;
        self.emit_gc_ref(func, source)?;
        func.instruction(&Instruction::RefTestNonNull(
            wasm_encoder::HeapType::Concrete(case_sub_idx),
        ));
        func.instruction(&Instruction::If(block_ty));
        func.instruction(&Instruction::I32Const(
            self.gc_variant_wit_disc(ty, k) as i32,
        ));
        if let Some(payload_ty) = super::super::gc_types::case_payload_ty(self.ctx, ty, k) {
            self.emit_case_payload_to_joined_stack(
                func,
                source,
                case_sub_idx,
                payload_ty,
                &canon_vts[1..],
            )?;
        } else {
            for &vt in &canon_vts[1..] {
                push_zero_valtype(func, vt)?;
            }
        }
        func.instruction(&Instruction::Else);
        self.emit_variant_case_chain(func, ty, source, block_ty, case_count, k + 1, canon_vts)?;
        func.instruction(&Instruction::End);
        Ok(())
    }

    /// Push a gc-variant case's payload as the `joined` payload slots (the
    /// variant's canonical `[1..]` valtypes), widening the payload's own slots
    /// up to the joined valtypes and zero-padding any trailing joined slots the
    /// (shorter) payload doesn't cover.
    fn emit_case_payload_to_joined_stack(
        &mut self,
        func: &mut Function,
        source: GcRefSource,
        case_sub_idx: u32,
        payload_ty: Ty,
        joined: &[wasm_encoder::ValType],
    ) -> Result<(), CodegenError> {
        use super::super::gc_types::{struct_get_op_for_payload, StructGetVariant};
        use yel_core::types::InternedTyKind;
        let payload_vts: Vec<wasm_encoder::ValType> = self
            .flatten_core_slots(payload_ty)
            .iter()
            .map(|s| s.valtype)
            .collect();
        let m = payload_vts.len();
        if m > joined.len() {
            return Err(CodegenError::InvalidIR(format!(
                "value->stack payload: {:?} flattens to {} slots but joined payload is {}",
                payload_ty,
                m,
                joined.len()
            )));
        }
        let is_scalar_leaf = m == 1
            && !self.is_gc_variant(payload_ty)
            && self.option_collapses_to_ref(payload_ty).is_none()
            && self.composite_gc_members(payload_ty)?.is_none()
            && !matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::String)
            && !(matches!(self.ctx.ty_kind(payload_ty), InternedTyKind::List(_))
                && self.record_gc_types.list_array_type_idx.contains_key(&payload_ty));
        if is_scalar_leaf {
            // Read the (possibly packed) scalar payload with correct
            // signedness, then widen to the joined slot valtype.
            self.emit_gc_ref(func, source)?;
            func.instruction(&Instruction::RefCastNonNull(
                wasm_encoder::HeapType::Concrete(case_sub_idx),
            ));
            match struct_get_op_for_payload(self.ctx, payload_ty) {
                StructGetVariant::Plain => func.instruction(&Instruction::StructGet {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                }),
                StructGetVariant::Signed => func.instruction(&Instruction::StructGetS {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                }),
                StructGetVariant::Unsigned => func.instruction(&Instruction::StructGetU {
                    struct_type_index: case_sub_idx,
                    field_index: 0,
                }),
            };
            emit_canonical_widen(func, payload_vts[0], joined[0])?;
        } else {
            // string / typed list / nested composite payload: produce its
            // canonical slots directly, reached through a `PayloadOf` source.
            let payload_source = GcRefSource::PayloadOf {
                inner: &source,
                case_sub_idx,
                chain: &[],
            };
            self.emit_value_to_canonical_stack(func, payload_ty, payload_source)?;
            // Reconcile each produced slot with the joined valtype. Only the
            // final (top-of-stack) slot can be widened without staging; a
            // non-top mismatch is a loud gap.
            for i in 0..m {
                if payload_vts[i] != joined[i] {
                    if i == m - 1 {
                        emit_canonical_widen(func, payload_vts[i], joined[i])?;
                    } else {
                        return Err(CodegenError::InvalidIR(format!(
                            "value->stack payload: multi-slot payload {:?} needs a cross-case \
                             join widening at non-top slot {} ({:?} -> {:?}); staging that slot \
                             is not supported on the direct callback-arg path",
                            payload_ty, i, payload_vts[i], joined[i]
                        )));
                    }
                }
            }
        }
        // Zero-pad joined slots this payload doesn't cover.
        for &vt in &joined[m..] {
            push_zero_valtype(func, vt)?;
        }
        Ok(())
    }

    /// The WASM `if`/`block` type that produces a value of `ty`'s canonical-ABI
    /// shape: `Empty` for zero slots, `Result(vt)` for one, else a pre-interned
    /// `() -> (slots…)` function type looked up in `ternary_block_types`.
    fn canonical_block_type(&self, ty: Ty) -> Result<wasm_encoder::BlockType, CodegenError> {
        let vts = self.flatten_core_valtypes(ty);
        match vts.len() {
            0 => Ok(wasm_encoder::BlockType::Empty),
            1 => Ok(wasm_encoder::BlockType::Result(vts[0])),
            _ => {
                let idx = self.ternary_block_types.get(&vts).ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "value->stack: canonical block shape {:?} for {:?} was not pre-interned \
                         (collect_ternary_block_shapes must register composite callback args)",
                        vts, ty
                    ))
                })?;
                Ok(wasm_encoder::BlockType::FunctionType(*idx))
            }
        }
    }

}

/// Store a materialized `(ptr, len)` pair (on the stack, len on top) into
/// canonical memory: ptr at `base_address_local + ptr_offset`, len at
/// `base_address_local + len_offset`. Stashes the pair through the two given
/// scratch locals. Both stores are 4-byte `i32.store`s — canonical ptr/len
/// slots are always i32.
fn store_canonical_ptr_len(
    func: &mut wasm_encoder::Function,
    base_address_local: u32,
    ptr_offset: u32,
    len_offset: u32,
    materialized_ptr_local: u32,
    materialized_len_local: u32,
) {
    use wasm_encoder::Instruction;
    func.instruction(&Instruction::LocalSet(materialized_len_local));
    func.instruction(&Instruction::LocalSet(materialized_ptr_local));
    func.instruction(&Instruction::LocalGet(base_address_local));
    if ptr_offset != 0 {
        func.instruction(&Instruction::I32Const(ptr_offset as i32));
        func.instruction(&Instruction::I32Add);
    }
    func.instruction(&Instruction::LocalGet(materialized_ptr_local));
    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
    func.instruction(&Instruction::LocalGet(base_address_local));
    if len_offset != 0 {
        func.instruction(&Instruction::I32Const(len_offset as i32));
        func.instruction(&Instruction::I32Add);
    }
    func.instruction(&Instruction::LocalGet(materialized_len_local));
    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
}

/// Emit the standard materializer copy-loop skeleton shared by every GC-array
/// → canonical-memory lowering:
/// ```text
/// len      = array.len(arr)
/// data_ptr = cabi_realloc(0, 0, elem_align, len * elem_size)
/// for idx in 0..len {
///     [elem_addr = data_ptr + idx * elem_size]   // when a local is given
///     <emit_element>
/// }
/// ```
/// `emit_element` writes one element's canonical bytes; the enclosing site
/// keeps its own prologue (how `arr` was obtained) and epilogue (returning or
/// spilling the `(data_ptr, len)` pair).
#[allow(clippy::too_many_arguments)]
fn emit_gc_array_materialize_loop(
    func: &mut wasm_encoder::Function,
    arr_local: u32,
    len_local: u32,
    data_ptr_local: u32,
    idx_local: u32,
    elem_addr_local: Option<u32>,
    elem_size: u32,
    elem_align: u32,
    cabi_realloc: u32,
    mut emit_element: impl FnMut(&mut wasm_encoder::Function) -> Result<(), CodegenError>,
) -> Result<(), CodegenError> {
    use wasm_encoder::Instruction;
    func.instruction(&Instruction::LocalGet(arr_local));
    func.instruction(&Instruction::ArrayLen);
    func.instruction(&Instruction::LocalSet(len_local));
    super::scratch::emit_cabi_realloc_array(func, len_local, elem_size, elem_align, cabi_realloc);
    func.instruction(&Instruction::LocalSet(data_ptr_local));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(idx_local));
    func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::LocalGet(idx_local));
    func.instruction(&Instruction::LocalGet(len_local));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));
    if let Some(elem_addr) = elem_addr_local {
        func.instruction(&Instruction::LocalGet(data_ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(elem_addr));
    }
    emit_element(func)?;
    func.instruction(&Instruction::LocalGet(idx_local));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(idx_local));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block
    Ok(())
}

/// Emit the standard un-materializer copy-loop skeleton shared by every
/// canonical-memory → GC-array lifting:
/// ```text
/// for idx in 0..len {
///     [elem_addr = canonical_ptr + idx * elem_size]   // when a local is given
///     <emit_element>
/// }
/// ```
/// The caller creates the target array (`array.new_default`) beforehand;
/// `emit_element` builds one element and `array.set`s it; the enclosing site
/// keeps its own epilogue (returning `arr`).
#[allow(clippy::too_many_arguments)]
fn emit_gc_array_unmaterialize_loop(
    func: &mut wasm_encoder::Function,
    canonical_ptr_local: u32,
    len_local: u32,
    idx_local: u32,
    elem_addr_local: Option<u32>,
    elem_size: u32,
    mut emit_element: impl FnMut(&mut wasm_encoder::Function) -> Result<(), CodegenError>,
) -> Result<(), CodegenError> {
    use wasm_encoder::Instruction;
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(idx_local));
    func.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    func.instruction(&Instruction::LocalGet(idx_local));
    func.instruction(&Instruction::LocalGet(len_local));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));
    if let Some(elem_addr) = elem_addr_local {
        func.instruction(&Instruction::LocalGet(canonical_ptr_local));
        func.instruction(&Instruction::LocalGet(idx_local));
        func.instruction(&Instruction::I32Const(elem_size as i32));
        func.instruction(&Instruction::I32Mul);
        func.instruction(&Instruction::I32Add);
        func.instruction(&Instruction::LocalSet(elem_addr));
    }
    emit_element(func)?;
    func.instruction(&Instruction::LocalGet(idx_local));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(idx_local));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block
    Ok(())
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
        // Records use their canonical-ABI memory layout
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
        // Lists (and strings) at canonical ABI are
        // 8 bytes (ptr i32 + len i32), align 4.
        InternedTyKind::List(_) | InternedTyKind::String => (8, 4),
        // Every other element (tuple, option/result/variant gc-variant, enum, …)
        // uses its canonical-ABI memory layout size/align — the list buffer
        // stride must be the full element width, not the 4-byte scalar
        // fallback. A wrong stride here silently corrupts the post-return
        // dealloc walk (`emit_free_region`), which frees garbage pointers and
        // hangs `$free`.
        _ => {
            let l = layout_ctx.layout_of(elem_ty);
            (l.size, l.align)
        }
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

/// Emit a typed canonical-memory store for a primitive/enum type.
/// (address, value) on the stack.
fn emit_canonical_scalar_store(
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

/// Emit a typed canonical-memory load for a primitive/enum type.
/// Address is on the stack; result is the loaded value.
fn emit_canonical_scalar_load(
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
