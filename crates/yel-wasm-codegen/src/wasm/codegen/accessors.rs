//! Per-signal getter/setter generation + value-coercion helpers.
//!
//! Methods live on `WasmPackageBuilder<'a>` via an additional impl block
//! and are called from `build::build_core_module` during the code section
//! pass.

use wasm_encoder::{Function, Instruction, ValType};
use yel_core::types::InternedTyKind;
use yel_core::Ty;

use super::super::CodegenError;
use super::super::{MemoryLayout, WasmPackageBuilder};
use super::scratch::mem_arg;

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn single_slot_getter_type(&self, ty: Ty) -> Result<Option<u32>, CodegenError> {
        use wasm_encoder::ValType;
        let flat = self.canonical_flat_valtypes(ty);
        if flat.len() != 1 {
            return Ok(None);
        }
        Ok(Some(match flat[0] {
            ValType::I32 => 4,
            ValType::F32 => 10,
            ValType::F64 => 12,
            ValType::I64 => 14,
            other => {
                return Err(CodegenError::InvalidIR(format!(
                    "single_slot_getter_type: composite type {:?} flattens to \
                     unsupported single slot valtype {:?}",
                    ty, other
                )));
            }
        }))
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
        layout: &MemoryLayout,
        sig_idx: usize,
        comp_idx: Option<usize>,
    ) -> Result<Function, CodegenError> {
        let addr = layout.signal_addr(sig_idx);

        // GC-struct-migrated signal — return value is computed from
        // struct.get instead of memory.load. Single-slot canonical-ABI
        // returns push the value directly; multi-slot returns refresh
        // the lift scratch at `addr` and return the scratch pointer.
        if let Some(ci) = comp_idx
            && self.signal_in_struct(ci, sig_idx) {
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
                let field_path: Vec<u32> = gc.signal_field_paths[sig_idx].clone();

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
                        .ok_or_else(|| CodegenError::InvalidIR(
                            "GC list getter requires cabi_realloc".into(),
                        ))?
                        .cabi_realloc;
                    let elem_ty = match self.ctx.ty_kind(signal_ty) {
                        InternedTyKind::List(e) => *e,
                        _ => return Err(CodegenError::InvalidIR(
                            "GC list getter: signal_ty is not a list".into(),
                        )),
                    };
                    let (elem_size, elem_align) = gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
                    let elem_record_def: Option<yel_core::DefId> = match self.ctx.ty_kind(elem_ty) {
                        InternedTyKind::Adt(d) if matches!(
                            self.ctx.defs.kind(*d),
                            yel_core::definitions::DefKind::Record(_)
                        ) => Some(*d),
                        _ => None,
                    };
                    // Phase 5e.4 / 5e.5: $fat_value-boxed element types
                    // (strings, option<scalar-i32-fits>) — share the
                    // same per-element copy logic.
                    let elem_is_string = matches!(
                        self.ctx.ty_kind(elem_ty),
                        InternedTyKind::String
                    ) || (
                        matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::Option(_))
                            && {
                                let canonical = self.canonical_flat_valtypes(elem_ty);
                                canonical.len() == 2
                                    && canonical.iter().all(|vt|
                                        matches!(vt, ValType::I32))
                            }
                    );
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
                        (1, ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                        })),
                        (1, ValType::I32), // scratch_ptr
                        (1, ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                        })),
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
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "GC list getter: missing record_type_idx".into(),
                            ))?;
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
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(elem_align as i32));
                    func.instruction(&Instruction::LocalGet(len_local));
                    func.instruction(&Instruction::I32Const(elem_size as i32));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::Call(cabi_realloc));
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
                        let fv = self.record_gc_types.fat_value_type_idx
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "list<string> getter: $fat_value type idx missing".into(),
                            ))?;
                        // elem_addr = data_ptr + idx * 8
                        func.instruction(&Instruction::LocalGet(data_ptr_local));
                        func.instruction(&Instruction::LocalGet(idx_local));
                        func.instruction(&Instruction::I32Const(8));
                        func.instruction(&Instruction::I32Mul);
                        func.instruction(&Instruction::I32Add);
                        func.instruction(&Instruction::LocalSet(elem_addr_local));
                        // store ptr at elem_addr+0
                        func.instruction(&Instruction::LocalGet(elem_addr_local));
                        func.instruction(&Instruction::LocalGet(arr_ref_local));
                        func.instruction(&Instruction::LocalGet(idx_local));
                        func.instruction(&Instruction::ArrayGet(arr_type_idx));
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: fv,
                            field_index: 0,
                        });
                        func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                        // store len at elem_addr+4
                        func.instruction(&Instruction::LocalGet(elem_addr_local));
                        func.instruction(&Instruction::I32Const(4));
                        func.instruction(&Instruction::I32Add);
                        func.instruction(&Instruction::LocalGet(arr_ref_local));
                        func.instruction(&Instruction::LocalGet(idx_local));
                        func.instruction(&Instruction::ArrayGet(arr_type_idx));
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: fv,
                            field_index: 1,
                        });
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
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Const(8));
                    func.instruction(&Instruction::Call(cabi_realloc));
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
                // Phase 3: SLR (POR + records with string / list<scalar>
                // fields) all route through the GC-backed getter path.
                let is_por = self.is_single_level_record(signal_ty);
                let result = (|| -> Result<(), CodegenError> {
                    // Phase 2: POR record with exactly one flat slot —
                    // return that slot's value directly (canonical-ABI
                    // says single-slot composites return the value, not
                    // a pointer). Read struct.get(comp).get(record).
                    if is_por && flat_valtypes.len() == 1 {
                        let record_type_idx = self
                            .por_record_type_idx(signal_ty)
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "POR getter (1 slot): record type idx missing".into(),
                            ))?;
                        let record_def_id = match self.ctx.ty_kind(signal_ty) {
                            yel_core::types::InternedTyKind::Adt(d) => *d,
                            _ => return Err(CodegenError::InvalidIR(
                                "POR getter: signal_ty is not an Adt".into(),
                            )),
                        };
                        let gc_field_idx = self
                            .record_gc_types
                            .field_gc_indices
                            .get(&record_def_id)
                            .and_then(|v| v.first())
                            .copied()
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "POR getter (1 slot): record GC field index missing".into(),
                            ))?;
                        self.emit_self_ref(&mut func, ci)?;
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: struct_ty,
                            field_index: field_path[0],
                        });
                        func.instruction(&Instruction::RefAsNonNull);
                        func.instruction(&Instruction::StructGet {
                            struct_type_index: record_type_idx,
                            field_index: gc_field_idx,
                        });
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
                            && let Some(&mat_fn) = self
                                .gc_list_materializer_fn_indices
                                .get(&arr_type_idx)
                        {
                            let layout_info = self.layout_ctx.layout_of(signal_ty);
                            let cabi_realloc = self
                                .alloc_funcs
                                .as_ref()
                                .ok_or_else(|| CodegenError::InvalidIR(
                                    "option-collapse getter: cabi_realloc missing".into(),
                                ))?
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
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::I32Const(layout_info.align as i32));
                            func.instruction(&Instruction::I32Const(layout_info.size as i32));
                            func.instruction(&Instruction::Call(cabi_realloc));
                            func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                            // Read the ref, null-check.
                            self.emit_self_ref(&mut func, ci)?;
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: struct_ty,
                                field_index: field_path[0],
                            });
                            func.instruction(&Instruction::RefIsNull);
                            func.instruction(&Instruction::If(
                                wasm_encoder::BlockType::Empty,
                            ));
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
                        let record_type_idx = self
                            .por_record_type_idx(signal_ty)
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "SLR getter: record type idx missing".into(),
                            ))?;
                        let record_def_id = match self.ctx.ty_kind(signal_ty) {
                            yel_core::types::InternedTyKind::Adt(d) => *d,
                            _ => return Err(CodegenError::InvalidIR(
                                "SLR getter: signal_ty is not an Adt".into(),
                            )),
                        };
                        let fat_value_idx = self.record_gc_types.fat_value_type_idx;
                        let layout_info = self.layout_ctx.layout_of(signal_ty);
                        let cabi_realloc = self
                            .alloc_funcs
                            .as_ref()
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "SLR getter: cabi_realloc not initialised".into(),
                            ))?
                            .cabi_realloc;
                        // Allocate lift scratch.
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Const(layout_info.align as i32));
                        func.instruction(&Instruction::I32Const(layout_info.size as i32));
                        func.instruction(&Instruction::Call(cabi_realloc));
                        func.instruction(&Instruction::LocalSet(scratch_ptr_local));

                        // Phase 4: recurse into the record (and any
                        // nested DTR records) to emit one store per
                        // canonical-ABI flat slot. Each store sources
                        // its value via the chain of GC struct.gets that
                        // reach the corresponding inner field.
                        let _ = record_type_idx;
                        let prefix: Vec<(u32, u32)> =
                            vec![(struct_ty, field_path[0])];
                        self.emit_getter_lift_dtr_record(
                            &mut func,
                            ci,
                            record_def_id,
                            0,
                            scratch_ptr_local,
                            fat_value_idx,
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
                    if let InternedTyKind::Tuple(tuple_elems) = self.ctx.ty_kind(signal_ty) {
                        let elements: Vec<yel_core::Ty> = tuple_elems.to_vec();
                        let tup_idx = self
                            .record_gc_types
                            .tuple_struct_type_idx
                            .get(&signal_ty)
                            .copied()
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "tuple getter: missing tuple_struct_type_idx".into(),
                            ))?;
                        let layout_info = self.layout_ctx.layout_of(signal_ty);
                        let cabi_realloc = self
                            .alloc_funcs
                            .as_ref()
                            .ok_or_else(|| CodegenError::InvalidIR(
                                "tuple getter: cabi_realloc missing".into(),
                            ))?
                            .cabi_realloc;
                        // Allocate lift scratch.
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Const(layout_info.align as i32));
                        func.instruction(&Instruction::I32Const(layout_info.size as i32));
                        func.instruction(&Instruction::Call(cabi_realloc));
                        func.instruction(&Instruction::LocalSet(scratch_ptr_local));
                        // For each tuple element, compute canonical offset
                        // and store the field's value.
                        let mut offset: u32 = 0;
                        for (i, &elem_ty) in elements.iter().enumerate() {
                            let elem_layout = self.layout_ctx.layout_of(elem_ty);
                            // Align offset.
                            let aligned = (offset + elem_layout.align - 1)
                                & !(elem_layout.align - 1);
                            offset = aligned;
                            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                            if offset != 0 {
                                func.instruction(&Instruction::I32Const(offset as i32));
                                func.instruction(&Instruction::I32Add);
                            }
                            self.emit_self_ref(&mut func, ci)?;
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: struct_ty,
                                field_index: field_path[0],
                            });
                            func.instruction(&Instruction::RefAsNonNull);
                            func.instruction(&Instruction::StructGet {
                                struct_type_index: tup_idx,
                                field_index: i as u32,
                            });
                            self.emit_typed_field_store(&mut func, elem_ty);
                            offset += elem_layout.size;
                        }
                        func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                        return Ok(());
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
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::I32Const(layout_info.align as i32));
                    func.instruction(&Instruction::I32Const(layout_info.size as i32));
                    func.instruction(&Instruction::Call(cabi_realloc));
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
                                .ok_or_else(|| CodegenError::InvalidIR(format!(
                                    "getter: no materializer for GC list arr_type_idx={}",
                                    arr_type_idx
                                )))?;
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
                            func.instruction(&Instruction::If(
                                wasm_encoder::BlockType::Empty,
                            ));
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
                                func.instruction(&Instruction::I32Const(
                                    ptr_slot.offset as i32,
                                ));
                                func.instruction(&Instruction::I32Add);
                            }
                            func.instruction(&Instruction::LocalGet(mat_ptr_local));
                            ptr_slot.store.emit_store(&mut func);
                            func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                            if len_slot.offset != 0 {
                                func.instruction(&Instruction::I32Const(
                                    len_slot.offset as i32,
                                ));
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
        let mut func = Function::new([]);

        match self.ctx.ty_kind(signal_ty) {
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
            InternedTyKind::String | InternedTyKind::List(_) => {
                // String/List: MAX_FLAT_RESULTS=1 means complex returns use pointer-to-tuple
                // Signature: (self: i32) -> i32 (pointer to (ptr, len) tuple)
                // The signal already stores (ptr, len) at addr, so return addr
                func.instruction(&Instruction::I32Const(addr));
            }
            InternedTyKind::Option(_) => {
                // Option type: MAX_FLAT_RESULTS=1 means we return pointer to (discriminant, value)
                // Memory layout: 1-byte discriminant at addr, value at addr+4 (aligned)
                // Return the address directly - signal already stores (discriminant, value) in memory
                func.instruction(&Instruction::I32Const(addr));
            }
            InternedTyKind::Result { .. } => {
                // Result type: MAX_FLAT_RESULTS=1 means we return pointer to (discriminant, payload)
                // Memory layout: 1-byte discriminant at addr, payload at aligned offset
                // Return the address directly - signal already stores result in memory
                func.instruction(&Instruction::I32Const(addr));
            }
            InternedTyKind::Adt(def_id) => {
                let def_id = *def_id;
                if self.ctx.defs.as_variant(def_id).is_some() {
                    // Variant: return pointer to inline memory. The canonical
                    // ABI lifts composite return values from an out-pointer
                    // into the real shape. Variants always carry a
                    // discriminant + joined payload, so flat arity is >= 1
                    // and only degenerates to 1 slot when there's no payload
                    // (enum-shape) — in that case the single slot is i32 and
                    // pointer-vs-value aliases on i32, so this path stays
                    // correct.
                    func.instruction(&Instruction::I32Const(addr));
                } else if self.ctx.defs.as_record(def_id).is_some() {
                    // Record: if the record flattens to exactly one slot
                    // (MAX_FLAT_RESULTS=1), the canonical ABI says return
                    // the value directly instead of through a pointer. Load
                    // that slot from inline memory with its typed load.
                    // Otherwise use the pointer convention.
                    if self.canonical_flat_valtypes(signal_ty).len() == 1 {
                        self.emit_flat_slot_signal_read(&mut func, addr, signal_ty)?;
                    } else {
                        func.instruction(&Instruction::I32Const(addr));
                    }
                } else {
                    // Enum (no payloads): load discriminant as i32
                    func.instruction(&Instruction::I32Const(addr));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                }
            }
            InternedTyKind::Tuple(_) => {
                // Tuple: same canonical-ABI rule as records. A single-field
                // tuple whose element flattens to one slot returns that slot
                // directly; multi-slot tuples use the pointer convention.
                if self.canonical_flat_valtypes(signal_ty).len() == 1 {
                    self.emit_flat_slot_signal_read(&mut func, addr, signal_ty)?;
                } else {
                    func.instruction(&Instruction::I32Const(addr));
                }
            }
            // Narrow integer + bool types are stored 1 byte wide and packed
            // next to neighbouring signals — load the correct width so we
            // don't bleed bytes from the next slot (an i32.load on a bool
            // at offset 0 would read into the adjacent string pointer and
            // fail jco's bool discriminant check with `invalid variant
            // discriminant for bool`).
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
            _ => {
                func.instruction(&Instruction::I32Const(addr));
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            }
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    pub(super) fn generate_setter_for(
        &mut self,
        comp_idx: usize,
        layout: &MemoryLayout,
        sig_idx: usize,
        _import_realloc: u32,
    ) -> Result<Function, CodegenError> {
        let component = &self.components[comp_idx];
        let signal = &component.signals[sig_idx];
        let signal_def_id = signal.def_id;

        let addr = layout.signal_addr(sig_idx);
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
            let field_path: Vec<u32> = gc.signal_field_paths[sig_idx].clone();

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
                    _ => return Err(CodegenError::InvalidIR(
                        "GC list setter: signal_ty is not a list".into(),
                    )),
                };
                let (elem_size, _elem_align) = gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
                let elem_record_def: Option<yel_core::DefId> = match self.ctx.ty_kind(elem_ty) {
                    InternedTyKind::Adt(d) if matches!(
                        self.ctx.defs.kind(*d),
                        yel_core::definitions::DefKind::Record(_)
                    ) => Some(*d),
                    _ => None,
                };
                let elem_is_string = matches!(
                    self.ctx.ty_kind(elem_ty),
                    InternedTyKind::String
                ) || (
                    matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::Option(_))
                        && {
                            let canonical = self.canonical_flat_valtypes(elem_ty);
                            canonical.len() == 2
                                && canonical.iter().all(|vt|
                                    matches!(vt, ValType::I32))
                        }
                );
                // Setter params: 0=rep(i32), 1=ptr(i32), 2=len(i32)
                // Locals: 3=self_ref, 4=arr_ref, 5=idx, [6=elem_addr if record/string]
                let self_ref_local: u32 = 3;
                let arr_ref_local: u32 = 4;
                let idx_local: u32 = 5;
                let elem_addr_local: u32 = 6;
                let mut local_decls: Vec<(u32, ValType)> = vec![
                    (1, ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                    })),
                    (1, ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                    })),
                    (1, ValType::I32), // idx
                ];
                if elem_record_def.is_some() || elem_is_string {
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
                    // Phase 5e.4: string element — load (ptr, len) at
                    // canonical offset, struct.new $fat_value, array.set.
                    let fv = self.record_gc_types.fat_value_type_idx
                        .ok_or_else(|| CodegenError::InvalidIR(
                            "list<string> setter: $fat_value type idx missing".into(),
                        ))?;
                    // elem_addr = ptr + idx * 8
                    func.instruction(&Instruction::LocalGet(1));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    func.instruction(&Instruction::I32Const(8));
                    func.instruction(&Instruction::I32Mul);
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalSet(elem_addr_local));
                    // arr_ref, idx for the array.set.
                    func.instruction(&Instruction::LocalGet(arr_ref_local));
                    func.instruction(&Instruction::LocalGet(idx_local));
                    // load ptr at +0
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    // load len at +4
                    func.instruction(&Instruction::LocalGet(elem_addr_local));
                    func.instruction(&Instruction::I32Const(4));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    // box into $fat_value
                    func.instruction(&Instruction::StructNew(fv));
                    // array.set
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
                    let (elem_size, _elem_align) =
                        gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
                    // Setter params: 0=rep, 1=disc, 2=ptr, 3=len.
                    // Locals: 4=self_ref, 5=arr_ref, 6=idx.
                    let self_ref_local: u32 = 4;
                    let arr_ref_local: u32 = 5;
                    let idx_local: u32 = 6;
                    let mut func = Function::new([
                        (1, ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(struct_ty),
                        })),
                        (1, ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
                        })),
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
                    func.instruction(&Instruction::RefNull(
                        wasm_encoder::HeapType::Concrete(arr_type_idx),
                    ));
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

            // Phase 5e.3: tuple-as-signal setter — params are flat
            // canonical slots (one per tuple element). Push self, then
            // each flat param (consuming canonical slot count per
            // element via `canonical_flat_valtypes`), `struct.new
            // $tuple_<n>` to build the ref, then `struct.set` into
            // the component field.
            if let InternedTyKind::Tuple(tuple_elems) = self.ctx.ty_kind(ty) {
                let elements: Vec<yel_core::Ty> = tuple_elems.to_vec();
                let tup_idx = self
                    .record_gc_types
                    .tuple_struct_type_idx
                    .get(&ty)
                    .copied()
                    .ok_or_else(|| CodegenError::InvalidIR(
                        "tuple setter: missing tuple_struct_type_idx".into(),
                    ))?;
                self.emit_self_ref(&mut func, comp_idx)?;
                let mut next_param: u32 = 1;
                for &elem_ty in &elements {
                    let elem_slots = self.canonical_flat_valtypes(elem_ty);
                    for _ in 0..elem_slots.len() {
                        func.instruction(&Instruction::LocalGet(next_param));
                        next_param += 1;
                    }
                }
                func.instruction(&Instruction::StructNew(tup_idx));
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
                let fat_value_idx = self.record_gc_types.fat_value_type_idx;
                let record_def_id = match self.ctx.ty_kind(ty) {
                    InternedTyKind::Adt(d) => *d,
                    _ => return Err(CodegenError::InvalidIR(
                        "SLR setter: signal_ty is not an Adt".into(),
                    )),
                };
                let _record_def = match self.ctx.defs.kind(record_def_id) {
                    yel_core::definitions::DefKind::Record(r) => r.clone(),
                    _ => return Err(CodegenError::InvalidIR(
                        "SLR setter: not a record def".into(),
                    )),
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
                    fat_value_idx,
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
        // Variants need special handling because the flat param list for a
        // variant joins payloads slot-wise. For a variant signal the setter
        // params are (self, discriminant, joined_payload_slots...) and we
        // store them raw at (addr, addr + payload_offset, ...): the host
        // will have serialized the active case's payload into the joined
        // slots, so we just copy them into backing memory verbatim.
        let is_variant = matches!(
            self.ctx.ty_kind(ty),
            InternedTyKind::Adt(def_id) if self.ctx.defs.as_variant(*def_id).is_some()
        );
        let is_enum = matches!(
            self.ctx.ty_kind(ty),
            InternedTyKind::Adt(def_id)
                if self.ctx.defs.as_enum(*def_id).is_some()
        );

        // Legacy memory-resident signal (Pointer-typed: records, tuples,
        // options) — store path uses memory addresses, but the trailing
        // `emit_trigger_effects` still routes through (ref Comp, parent)
        // effect blocks, so we must resolve a typed self ref via the
        // registry and stash it in `current_self_local` for
        // `emit_self_ref` to pick up.
        let gc = &self.gc_layouts[comp_idx];
        let comp_struct_ty = gc.component_struct_type_idx.ok_or_else(|| {
            CodegenError::InternalError(format!(
                "setter (memory path): component {} missing component_struct_type_idx",
                comp_idx
            ))
        })?;
        // Variant setters take (rep, disc, joined_payload_slots...). For
        // non-variant signals it's (rep, ...flat). Compute the WASM
        // local index for the typed self ref accordingly: it sits right
        // after every declared param.
        let setter_param_count: u32 = if is_variant {
            // 1 (rep) + 1 (disc) + max joined slots — too dynamic to
            // precompute here without re-doing the variant join
            // walk. Use a generous upper bound: we need the local to
            // come AFTER all params, but Function::new declares locals
            // by count from the end of params. So we just need the
            // count of params; let the variant arm count below.
            //
            // For simplicity, count params via walking the variant
            // joined slots once.
            let mut joined: Vec<ValType> = Vec::new();
            if let InternedTyKind::Adt(def_id) = self.ctx.ty_kind(ty)
                && let Some(v) = self.ctx.defs.as_variant(*def_id) {
                    let cases = v.cases.clone();
                    let mut case_flats: Vec<Vec<ValType>> = Vec::new();
                    for &c in &cases {
                        let payload = match self.ctx.defs.kind(c) {
                            yel_core::definitions::DefKind::VariantCase(case) => case.payload,
                            _ => None,
                        };
                        case_flats.push(
                            payload
                                .map(|t| self.canonical_flat_valtypes(t))
                                .unwrap_or_default(),
                        );
                    }
                    for f in &case_flats {
                        joined = super::super::join_flat_valtypes(&joined, f);
                    }
                }
            // (rep) + (disc) + joined slots
            2 + joined.len() as u32
        } else {
            1 + self.canonical_flat_valtypes(ty).len() as u32
        };
        let self_ref_local_legacy: u32 = setter_param_count;
        let mut func = Function::new([(
            1,
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(comp_struct_ty),
            }),
        )]);
        // Registry lookup: rep (param 0) → typed ref → self_ref_local_legacy.
        self.emit_registry_lookup(
            &mut func,
            comp_idx,
            0,
            self_ref_local_legacy,
        )?;
        self.current_self_local = Some(self_ref_local_legacy);
        self.current_self_comp_idx = Some(comp_idx);

        if is_variant {
            // Discriminant at offset 0 (1 byte).
            func.instruction(&Instruction::I32Const(addr));
            func.instruction(&Instruction::LocalGet(1));
            func.instruction(&Instruction::I32Store8(mem_arg(0, 0)));
            // Joined payload slots start at the variant's payload_offset.
            if let InternedTyKind::Adt(def_id) = self.ctx.ty_kind(ty)
                && let Some(var_def) = self.ctx.defs.as_variant(*def_id) {
                    let var_layout = {
                        let vd = var_def.clone();
                        self.layout_ctx.compute_variant_layout_from_def_public(&vd)
                    };
                    let payload_offset = var_layout.payload_offset;
                    let joined = {
                        // Recompute joined flat valtypes for the variant payloads.
                        use wasm_encoder::ValType;
                        let mut case_flats: Vec<Vec<ValType>> = Vec::new();
                        for &case_def_id in &var_def.cases {
                            let payload = match self.ctx.defs.kind(case_def_id) {
                                yel_core::definitions::DefKind::VariantCase(c) => c.payload,
                                _ => None,
                            };
                            case_flats.push(
                                payload
                                    .map(|t| self.canonical_flat_valtypes(t))
                                    .unwrap_or_default(),
                            );
                        }
                        let mut joined: Vec<ValType> = Vec::new();
                        for f in &case_flats {
                            joined = super::super::join_flat_valtypes(&joined, f);
                        }
                        joined
                    };
                    // Write each joined slot at payload_offset + sequential bumps.
                    // For the raw storage path we lay out slots back-to-back at
                    // their natural sizes starting from payload_offset.
                    let mut slot_off = payload_offset;
                    for (i, vt) in joined.iter().enumerate() {
                        use wasm_encoder::ValType;
                        func.instruction(&Instruction::I32Const(addr + slot_off as i32));
                        func.instruction(&Instruction::LocalGet((i + 2) as u32));
                        match vt {
                            ValType::I32 => {
                                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                                slot_off += 4;
                            }
                            ValType::I64 => {
                                func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                                slot_off += 8;
                            }
                            ValType::F32 => {
                                func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                                slot_off += 4;
                            }
                            ValType::F64 => {
                                func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                                slot_off += 8;
                            }
                            _ => {
                                return Err(CodegenError::InvalidIR(format!(
                                    "Unsupported variant payload slot type {:?}",
                                    vt
                                )));
                            }
                        }
                    }
                }
        } else if is_enum {
            // Enum (no payloads): just store discriminant as i32.
            func.instruction(&Instruction::I32Const(addr));
            func.instruction(&Instruction::LocalGet(1));
            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
        } else {
            // Use the generic flat-slot table. Record/Option/primitives etc.
            let slots = self.flatten_core_slots(ty);
            for (i, slot) in slots.iter().enumerate() {
                func.instruction(&Instruction::I32Const(addr + slot.offset as i32));
                func.instruction(&Instruction::LocalGet((i + 1) as u32));
                slot.store.emit_store(&mut func);
            }
        }

        self.emit_trigger_effects(&mut func, signal_def_id, comp_idx)?;
        self.current_self_local = None;
        self.current_self_comp_idx = None;

        func.instruction(&Instruction::End);
        Ok(func)
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
    pub(super) fn generate_gc_list_unmaterializer(
        &mut self,
        arr_type_idx: u32,
        elem_ty: yel_core::Ty,
    ) -> Result<Function, CodegenError> {
        use yel_core::types::InternedTyKind;
        let mut func = Function::new([
            (1, ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
            })),       // arr (local 2)
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

        if matches!(self.ctx.ty_kind(elem_ty), InternedTyKind::String) {
            let fv = self.record_gc_types.fat_value_type_idx
                .ok_or_else(|| CodegenError::InvalidIR(
                    "list<string> un-materializer: $fat_value type idx missing".into(),
                ))?;
            // for idx in 0..len { arr.set(idx, struct.new $fat_value(load(ptr+idx*8), load(ptr+idx*8+4))) }
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&wasm_encoder::Instruction::Loop(wasm_encoder::BlockType::Empty));
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
            // arr.set(idx, struct.new $fat_value(...))
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            // ptr field
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::I32Load(super::scratch::mem_arg(0, 2)));
            // len field
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(4));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::I32Load(super::scratch::mem_arg(0, 2)));
            func.instruction(&wasm_encoder::Instruction::StructNew(fv));
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
        // For other element shapes we just return the empty default
        // array. Callers that hit this with a non-string element will
        // see runtime missing-element behavior; the corresponding
        // record_pack_from_memory error will direct them here.
        let _ = (idx_local, elem_addr_local);
        func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
        func.instruction(&wasm_encoder::Instruction::End);
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
            .ok_or_else(|| CodegenError::InvalidIR(
                "gc_list_materializer requires cabi_realloc".into(),
            ))?
            .cabi_realloc;
        // Phase 5e.1: for record element types we need a typed copy
        // loop that pulls each field out of the record GC ref and
        // stores it at the canonical-ABI offset in memory. Strings /
        // list<scalar> fields unbox `$fat_value`, primitives use a
        // typed store.
        let elem_is_string = matches!(
            self.ctx.ty_kind(elem_ty),
            yel_core::types::InternedTyKind::String
        ) || (
            matches!(self.ctx.ty_kind(elem_ty), yel_core::types::InternedTyKind::Option(_))
                && {
                    let canonical = self.canonical_flat_valtypes(elem_ty);
                    canonical.len() == 2
                        && canonical.iter().all(|vt|
                            matches!(vt, ValType::I32))
                }
        );
        if elem_is_string {
            // Phase 5e.4: per-element layout = ($fat_value ref → ptr@+0, len@+4).
            let fat_value_idx = self.record_gc_types.fat_value_type_idx
                .ok_or_else(|| CodegenError::InvalidIR(
                    "list<string> materializer: $fat_value type idx missing".into(),
                ))?;
            let elem_size: u32 = 8;
            let elem_align: u32 = 4;
            let mut func = Function::new([
                (1, ValType::I32), // len
                (1, ValType::I32), // data_ptr
                (1, ValType::I32), // idx
                (1, ValType::I32), // elem_addr
            ]);
            let arr_local: u32 = 0;
            let len_local: u32 = 1;
            let data_ptr_local: u32 = 2;
            let idx_local: u32 = 3;
            let elem_addr_local: u32 = 4;
            // len = array.len(arr)
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::ArrayLen);
            func.instruction(&wasm_encoder::Instruction::LocalSet(len_local));
            // data_ptr = cabi_realloc(0, 0, elem_align, len * elem_size)
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::I32Const(elem_align as i32));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
            func.instruction(&wasm_encoder::Instruction::I32Mul);
            func.instruction(&wasm_encoder::Instruction::Call(cabi_realloc));
            func.instruction(&wasm_encoder::Instruction::LocalSet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&wasm_encoder::Instruction::Loop(wasm_encoder::BlockType::Empty));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::I32GeU);
            func.instruction(&wasm_encoder::Instruction::BrIf(1));
            // elem_addr = data_ptr + idx * 8
            func.instruction(&wasm_encoder::Instruction::LocalGet(data_ptr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(8));
            func.instruction(&wasm_encoder::Instruction::I32Mul);
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalSet(elem_addr_local));
            // ptr at elem_addr+0
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
            func.instruction(&wasm_encoder::Instruction::RefAsNonNull);
            func.instruction(&wasm_encoder::Instruction::StructGet {
                struct_type_index: fat_value_idx,
                field_index: 0,
            });
            func.instruction(&wasm_encoder::Instruction::I32Store(super::scratch::mem_arg(0, 2)));
            // len at elem_addr+4
            func.instruction(&wasm_encoder::Instruction::LocalGet(elem_addr_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(4));
            func.instruction(&wasm_encoder::Instruction::I32Add);
            func.instruction(&wasm_encoder::Instruction::LocalGet(arr_local));
            func.instruction(&wasm_encoder::Instruction::LocalGet(idx_local));
            func.instruction(&wasm_encoder::Instruction::ArrayGet(arr_type_idx));
            func.instruction(&wasm_encoder::Instruction::RefAsNonNull);
            func.instruction(&wasm_encoder::Instruction::StructGet {
                struct_type_index: fat_value_idx,
                field_index: 1,
            });
            func.instruction(&wasm_encoder::Instruction::I32Store(super::scratch::mem_arg(0, 2)));
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
        let elem_record_def: Option<yel_core::DefId> = match self.ctx.ty_kind(elem_ty) {
            yel_core::types::InternedTyKind::Adt(d)
                if matches!(self.ctx.defs.kind(*d), yel_core::definitions::DefKind::Record(_)) =>
            {
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
                .ok_or_else(|| CodegenError::InvalidIR(
                    "gc_list_materializer: missing record_type_idx".into(),
                ))?;
            let mut func = Function::new([
                (1, ValType::I32), // len
                (1, ValType::I32), // data_ptr
                (1, ValType::I32), // idx
                (1, ValType::I32), // elem_addr
                (1, ValType::Ref(wasm_encoder::RefType {
                    nullable: true,
                    heap_type: wasm_encoder::HeapType::Concrete(record_type_idx),
                })),
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
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::I32Const(elem_align as i32));
            func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
            func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
            func.instruction(&wasm_encoder::Instruction::I32Mul);
            func.instruction(&wasm_encoder::Instruction::Call(cabi_realloc));
            func.instruction(&wasm_encoder::Instruction::LocalSet(data_ptr_local));
            // idx = 0
            func.instruction(&wasm_encoder::Instruction::I32Const(0));
            func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
            func.instruction(&wasm_encoder::Instruction::Block(wasm_encoder::BlockType::Empty));
            func.instruction(&wasm_encoder::Instruction::Loop(wasm_encoder::BlockType::Empty));
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
        let (elem_size, elem_align) = gc_list_elem_canonical_info(self.ctx, &mut self.layout_ctx, elem_ty);
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
        func.instruction(&wasm_encoder::Instruction::I32Const(0));
        func.instruction(&wasm_encoder::Instruction::I32Const(0));
        func.instruction(&wasm_encoder::Instruction::I32Const(elem_align as i32));
        func.instruction(&wasm_encoder::Instruction::LocalGet(len_local));
        func.instruction(&wasm_encoder::Instruction::I32Const(elem_size as i32));
        func.instruction(&wasm_encoder::Instruction::I32Mul);
        func.instruction(&wasm_encoder::Instruction::Call(cabi_realloc));
        func.instruction(&wasm_encoder::Instruction::LocalSet(data_ptr_local));
        // idx = 0
        func.instruction(&wasm_encoder::Instruction::I32Const(0));
        func.instruction(&wasm_encoder::Instruction::LocalSet(idx_local));
        // Copy loop: while idx < len { mem[data_ptr + idx*sz] = arr[idx]; idx++ }
        func.instruction(&wasm_encoder::Instruction::Block(wasm_encoder::BlockType::Empty));
        func.instruction(&wasm_encoder::Instruction::Loop(wasm_encoder::BlockType::Empty));
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
    fn emit_getter_lift_dtr_record(
        &mut self,
        func: &mut Function,
        ci: usize,
        record_def_id: yel_core::DefId,
        base_offset: u32,
        scratch_ptr_local: u32,
        fat_value_idx: Option<u32>,
        prefix: &[(u32, u32)],
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            yel_core::definitions::DefKind::Record(r) => r.clone(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "DTR getter lift: not a record def".into(),
                ))
            }
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| {
                CodegenError::InvalidIR("DTR getter lift: missing record type idx".into())
            })?;
        let gc_field_indices: Vec<u32> = self
            .record_gc_types
            .field_gc_indices
            .get(&record_def_id)
            .cloned()
            .ok_or_else(|| {
                CodegenError::InvalidIR("DTR getter lift: missing gc field indices".into())
            })?;
        let record_layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| CodegenError::InvalidIR("DTR getter lift: missing record_layout".into()))?
            .clone();
        for (i, &field_def_id) in record_def.fields.iter().enumerate() {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => {
                    return Err(CodegenError::InvalidIR(
                        "DTR getter lift: not a field def".into(),
                    ))
                }
            };
            let (_name, field_offset, _ty) = record_layout
                .field_offsets
                .get(i)
                .cloned()
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "DTR getter lift: field offset missing for field {}",
                        i
                    ))
                })?;
            let gc_field_idx = gc_field_indices[i];
            let abs_field_offset = base_offset + field_offset;
            // Nested record field: recurse with extended prefix.
            if let yel_core::types::InternedTyKind::Adt(field_def) =
                self.ctx.ty_kind(field_ty)
                && let yel_core::definitions::DefKind::Record(_) =
                    self.ctx.defs.kind(*field_def)
            {
                let mut new_prefix: Vec<(u32, u32)> = prefix.to_vec();
                new_prefix.push((record_type_idx, gc_field_idx));
                self.emit_getter_lift_dtr_record(
                    func,
                    ci,
                    *field_def,
                    abs_field_offset,
                    scratch_ptr_local,
                    fat_value_idx,
                    &new_prefix,
                )?;
                continue;
            }
            // Primitive / string / list field. Emit one store per flat slot.
            let field_kind = self.ctx.ty_kind(field_ty).clone();
            let field_slots = self.flatten_core_slots(field_ty);
            for (slot_idx, slot) in field_slots.iter().enumerate() {
                func.instruction(&Instruction::LocalGet(scratch_ptr_local));
                let total_off = abs_field_offset + slot.offset;
                if total_off != 0 {
                    func.instruction(&Instruction::I32Const(total_off as i32));
                    func.instruction(&Instruction::I32Add);
                }
                // Emit ref chain: self, then for each (struct_ty,
                // field_idx) in prefix do struct.get + ref.as_non_null
                // (except final, where we keep nullable for the last
                // struct.get's argument). Actually: we need the value
                // to be `ref` for the next struct.get. The inner record
                // field returns `(ref null $<inner>)` so we ref.as_non_null
                // before chaining further. We always pass nullable refs
                // to struct.get; struct.get accepts nullable refs.
                self.emit_self_ref(func, ci)?;
                for (idx, &(s_ty, f_idx)) in prefix.iter().enumerate() {
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: s_ty,
                        field_index: f_idx,
                    });
                    if idx + 1 < prefix.len() {
                        func.instruction(&Instruction::RefAsNonNull);
                    }
                }
                // After prefix walk: stack top = (ref null $rec_record).
                func.instruction(&Instruction::RefAsNonNull);
                func.instruction(&Instruction::StructGet {
                    struct_type_index: record_type_idx,
                    field_index: gc_field_idx,
                });
                if matches!(
                    field_kind,
                    yel_core::types::InternedTyKind::String
                        | yel_core::types::InternedTyKind::List(_)
                ) {
                    let fv = fat_value_idx.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "DTR getter lift: fat_value type idx missing".into(),
                        )
                    })?;
                    func.instruction(&Instruction::RefAsNonNull);
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: fv,
                        field_index: slot_idx as u32,
                    });
                }
                slot.store.emit_store(func);
            }
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
        record_def_id: yel_core::DefId,
        fat_value_idx: Option<u32>,
        next_param: &mut u32,
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            yel_core::definitions::DefKind::Record(r) => r.clone(),
            _ => {
                return Err(CodegenError::InvalidIR(
                    "DTR setter pack: not a record def".into(),
                ))
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
                    ))
                }
            };
            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::Adt(field_def) => match self.ctx.defs.kind(*field_def) {
                    yel_core::definitions::DefKind::Record(_) => {
                        // Nested record field: recurse to consume its
                        // flat params and push `(ref null $inner_record)`.
                        self.emit_setter_pack_dtr_record(
                            func,
                            *field_def,
                            fat_value_idx,
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
                InternedTyKind::String | InternedTyKind::List(_) => {
                    let field_slots = self.canonical_flat_valtypes(field_ty);
                    for _ in 0..field_slots.len() {
                        func.instruction(&Instruction::LocalGet(*next_param));
                        *next_param += 1;
                    }
                    let fv = fat_value_idx.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "DTR setter pack: fat_value type idx missing".into(),
                        )
                    })?;
                    func.instruction(&Instruction::StructNew(fv));
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

    /// Phase 5e.1: build a `(ref null $<rec>)` from canonical-ABI bytes
    /// at memory address held in `base_addr_local`. For each field:
    /// - primitive: typed load at field offset
    /// - string / list<scalar>: load (ptr, len) at field offset and
    ///   wrap in `struct.new $fat_value`
    /// - nested DTR record: recurse with adjusted base+field_offset
    /// Ends with `struct.new $<rec>` consuming the pushed field values
    /// and leaving the record GC ref on the stack.
    fn emit_record_pack_from_memory(
        &mut self,
        func: &mut Function,
        record_def_id: yel_core::DefId,
        base_addr_local: u32,
        base_offset: u32,
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            yel_core::definitions::DefKind::Record(r) => r.clone(),
            _ => return Err(CodegenError::InvalidIR(
                "record_pack_from_memory: not a record def".into(),
            )),
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| CodegenError::InvalidIR(
                "record_pack_from_memory: missing record_type_idx".into(),
            ))?;
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| CodegenError::InvalidIR(
                "record_pack_from_memory: missing record layout".into(),
            ))?
            .clone();
        let fat_value_idx = self.record_gc_types.fat_value_type_idx;
        for (i, &field_def_id) in record_def.fields.iter().enumerate() {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => return Err(CodegenError::InvalidIR(
                    "record_pack_from_memory: not a field def".into(),
                )),
            };
            let (_name, field_offset, _ty) = layout
                .field_offsets
                .get(i)
                .cloned()
                .ok_or_else(|| CodegenError::InvalidIR(
                    "record_pack_from_memory: missing field offset".into(),
                ))?;
            let abs_off = base_offset + field_offset;
            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::Adt(field_def) if matches!(
                    self.ctx.defs.kind(*field_def),
                    yel_core::definitions::DefKind::Record(_)
                ) => {
                    self.emit_record_pack_from_memory(
                        func,
                        *field_def,
                        base_addr_local,
                        abs_off,
                    )?;
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
                    let arr_idx = self
                        .record_gc_types
                        .list_array_type_idx[&field_ty];
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
                    // Load (ptr, len) from memory and box in $fat_value.
                    let fv = fat_value_idx.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "record_pack_from_memory: fat_value type idx missing".into(),
                        )
                    })?;
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const(abs_off as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::I32Load(super::scratch::mem_arg(0, 2)));
                    func.instruction(&Instruction::StructNew(fv));
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
    /// Optional scratch i32 locals used by the typed-array list-field
    /// path to stash (ptr, len) returned by the per-array materializer
    /// before storing them into canonical memory. Pass `None` from
    /// callers that don't yet declare the scratch locals — the lift
    /// will return an error if it actually needs them.
    fn emit_record_lift_to_memory(
        &mut self,
        func: &mut Function,
        record_def_id: yel_core::DefId,
        record_ref_local: u32,
        base_addr_local: u32,
        base_offset: u32,
        scratch_ptr_len: Option<(u32, u32)>,
    ) -> Result<(), CodegenError> {
        let record_def = match self.ctx.defs.kind(record_def_id) {
            yel_core::definitions::DefKind::Record(r) => r.clone(),
            _ => return Err(CodegenError::InvalidIR(
                "record_lift_to_memory: not a record def".into(),
            )),
        };
        let record_type_idx = self
            .record_gc_types
            .record_type_idx
            .get(&record_def_id)
            .copied()
            .ok_or_else(|| CodegenError::InvalidIR(
                "record_lift_to_memory: missing record_type_idx".into(),
            ))?;
        let gc_field_indices: Vec<u32> = self
            .record_gc_types
            .field_gc_indices
            .get(&record_def_id)
            .cloned()
            .ok_or_else(|| CodegenError::InvalidIR(
                "record_lift_to_memory: missing gc field indices".into(),
            ))?;
        let layout = self
            .layout_ctx
            .record_layout_by_id(record_def_id)
            .ok_or_else(|| CodegenError::InvalidIR(
                "record_lift_to_memory: missing record layout".into(),
            ))?
            .clone();
        let fat_value_idx = self.record_gc_types.fat_value_type_idx;
        for (i, &field_def_id) in record_def.fields.iter().enumerate() {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                yel_core::definitions::DefKind::Field(f) => f.ty,
                _ => return Err(CodegenError::InvalidIR(
                    "record_lift_to_memory: not a field def".into(),
                )),
            };
            let (_name, field_offset, _ty) = layout
                .field_offsets
                .get(i)
                .cloned()
                .ok_or_else(|| CodegenError::InvalidIR(
                    "record_lift_to_memory: missing field offset".into(),
                ))?;
            let abs_off = base_offset + field_offset;
            let gc_field_idx = gc_field_indices[i];
            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::Adt(field_def) if matches!(
                    self.ctx.defs.kind(*field_def),
                    yel_core::definitions::DefKind::Record(_)
                ) => {
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
                    let arr_idx = self
                        .record_gc_types
                        .list_array_type_idx[&field_ty];
                    let mat_fn = self
                        .gc_list_materializer_fn_indices
                        .get(&arr_idx)
                        .copied()
                        .ok_or_else(|| CodegenError::InvalidIR(format!(
                            "record_lift_to_memory: missing materializer for arr_type_idx={}",
                            arr_idx
                        )))?;
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
                InternedTyKind::String | InternedTyKind::List(_) => {
                    let fv = fat_value_idx.ok_or_else(|| {
                        CodegenError::InvalidIR(
                            "record_lift_to_memory: fat_value type idx missing".into(),
                        )
                    })?;
                    // Store ptr at abs_off.
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
                    func.instruction(&Instruction::RefAsNonNull);
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: fv,
                        field_index: 0,
                    });
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
                    // Store len at abs_off + 4.
                    func.instruction(&Instruction::LocalGet(base_addr_local));
                    func.instruction(&Instruction::I32Const((abs_off + 4) as i32));
                    func.instruction(&Instruction::I32Add);
                    func.instruction(&Instruction::LocalGet(record_ref_local));
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: record_type_idx,
                        field_index: gc_field_idx,
                    });
                    func.instruction(&Instruction::RefAsNonNull);
                    func.instruction(&Instruction::StructGet {
                        struct_type_index: fv,
                        field_index: 1,
                    });
                    func.instruction(&Instruction::I32Store(super::scratch::mem_arg(0, 2)));
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
    fn emit_typed_field_load(
        &self,
        func: &mut Function,
        ty: yel_core::Ty,
    ) {
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
    fn emit_typed_field_store(
        &self,
        func: &mut Function,
        ty: yel_core::Ty,
    ) {
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
        InternedTyKind::S32 | InternedTyKind::U32 | InternedTyKind::F32
        | InternedTyKind::Char => (4, 4),
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
