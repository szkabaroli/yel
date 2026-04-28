//! Core WASM module generation from LIR.
//!
//! This module handles generation of the inner core WASM module including:
//! - Type definitions
//! - Import/export sections
//! - Runtime functions (concat<n>, s32_to_string, bool_to_string)
//! - Function codegen (constructor, mount, unmount, getters, setters) for ALL components + standalone dispatch
//! - Data section for string literals
//! - Name section for debugging

use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemoryType, Module, TypeSection, ValType,
};
use yel_core::{DefId, Ty};

use super::super::CodegenError;
use crate::wasm::AllocatorFuncs;
use yel_core::lir::{LirComponent, LirExpr, LirSlotValType, align_to};
use yel_core::types::InternedTyKind;

use super::super::runtime::{self, RuntimeFunctions};
use super::super::{ImportLayout, MemoryLayout, WasmPackageBuilder, to_kebab_case, to_wit_name};

use super::scratch::{compute_mount_retention_counts, merge_max_slot_counts, push_valtype_locals};

impl<'a> WasmPackageBuilder<'a> {
    pub(crate) fn build_core_module(&mut self) -> Result<Module, CodegenError> {
        let mut module = Module::new();

        // Collect exported component indices to avoid holding borrow during mutable operations
        let exported_indices: Vec<usize> = self
            .components
            .iter()
            .enumerate()
            .filter(|(_, c)| c.is_export)
            .map(|(i, _)| i)
            .collect();

        // Get exported components (used later for the export interface
        // emission). Note the import layout now covers *all* components so
        // non-exported components' callbacks are also registered as imports
        // — their bodies can still invoke them from event handlers etc.
        let exported_components: Vec<&LirComponent> = exported_indices
            .iter()
            .map(|&i| &self.components[i])
            .collect();
        let all_components: Vec<&LirComponent> = self.components.iter().collect();

        // Type section
        let mut types = TypeSection::new();
        types.ty().function([], []); // 0: () -> ()
        types.ty().function([ValType::I32], []); // 1: (i32) -> ()
        types.ty().function([], [ValType::I32]); // 2: () -> i32
        types.ty().function([ValType::I32, ValType::I32], []); // 3: (i32, i32) -> () - setter i32
        types.ty().function([ValType::I32], [ValType::I32]); // 4: (i32) -> i32 - getter i32
        types
            .ty()
            .function([ValType::I32, ValType::I32], [ValType::I32]); // 5: (i32, i32) -> i32
        types
            .ty()
            .function([ValType::I32, ValType::I32, ValType::I32], []); // 6: (i32, i32, i32) -> ()
        types.ty().function(
            [
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
            ],
            [],
        ); // 7
        types
            .ty()
            .function([ValType::I32, ValType::I32, ValType::I32, ValType::I32], []); // 8
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32],
        ); // 9: realloc
        // Additional types for f32, f64, i64 getters/setters
        types.ty().function([ValType::I32], [ValType::F32]); // 10: getter f32
        types.ty().function([ValType::I32, ValType::F32], []); // 11: setter f32
        types.ty().function([ValType::I32], [ValType::F64]); // 12: getter f64
        types.ty().function([ValType::I32, ValType::F64], []); // 13: setter f64
        types.ty().function([ValType::I32], [ValType::I64]); // 14: getter i64
        types.ty().function([ValType::I32, ValType::I64], []); // 15: setter i64
        // Runtime function types
        types
            .ty()
            .function([ValType::I32], [ValType::I32, ValType::I32]); // 16: (i32) -> (i32, i32) - s32_to_string, bool_to_string
        // concat<n> types: concat2..concat8 (dynamic based on arity)
        // Type 17: concat2 - (i32, i32, i32, i32) -> (i32, i32)
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32, ValType::I32],
        );
        // Type 18: concat3 - (i32 x 6) -> (i32, i32)
        types.ty().function(
            [
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
            ],
            [ValType::I32, ValType::I32],
        );
        // Type 19: concat4 - (i32 x 8) -> (i32, i32)
        types
            .ty()
            .function([ValType::I32; 8], [ValType::I32, ValType::I32]);
        // Type 20: concat5 - (i32 x 10) -> (i32, i32)
        types
            .ty()
            .function([ValType::I32; 10], [ValType::I32, ValType::I32]);
        // Type 21: concat6 - (i32 x 12) -> (i32, i32)
        types
            .ty()
            .function([ValType::I32; 12], [ValType::I32, ValType::I32]);
        // Type 22: concat7 - (i32 x 14) -> (i32, i32)
        types
            .ty()
            .function([ValType::I32; 14], [ValType::I32, ValType::I32]);
        // Type 23: concat8 - (i32 x 16) -> (i32, i32)
        types
            .ty()
            .function([ValType::I32; 16], [ValType::I32, ValType::I32]);
        // Type 24: (i32, i32, i32) -> i32 - for record ctor with 3 params
        types
            .ty()
            .function([ValType::I32, ValType::I32, ValType::I32], [ValType::I32]);
        // Type 25: (i32 x 5) -> i32 - for record ctor with 5 params
        types.ty().function([ValType::I32; 5], [ValType::I32]);
        // Type 26: () -> (i32, i32) - for if block results in list_get_opt
        types.ty().function([], [ValType::I32, ValType::I32]);
        // Type 27: (f32) -> (i32, i32) - for f32_to_string
        types
            .ty()
            .function([ValType::F32], [ValType::I32, ValType::I32]);
        // Type 28: (i32, i32, i32) -> (i32, i32) - for list ctor with 3 params
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32, ValType::I32],
        );
        // Type 29: (i32, i32) -> (i32, i32) - for list ctor with 2 params
        types
            .ty()
            .function([ValType::I32, ValType::I32], [ValType::I32, ValType::I32]);
        // Type 30: (i32 x 5) -> (i32, i32) - for list ctor with 5 params
        types
            .ty()
            .function([ValType::I32; 5], [ValType::I32, ValType::I32]);
        // Type 31: set-attribute with attribute-value variant (canonical ABI flattened)
        // (node, name_ptr, name_len, discrim, payload_i64, payload_i32) -> ()
        // The canonical ABI joins variant payloads: i64 slot for (ptr,len)/i64/f64, i32 slot for i32/f32
        // Variant cases: 0=str, 1=bool, 2=s8, 3=s16, 4=s32, 5=s64, 6=u8, 7=u16, 8=u32, 9=u64, 10=f32, 11=f64, 12=char
        types.ty().function(
            [
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I64,
                ValType::I32,
            ],
            [],
        );
        // Type 32: promote_ptr_for_variant - (ptr: i32, len: i32) -> (i64, i32)
        // Promotes fat pointer for canonical ABI variant: returns (ptr as i64, len as i32)
        types
            .ty()
            .function([ValType::I32, ValType::I32], [ValType::I64, ValType::I32]);
        // Type 33: (i64) -> (i32, i32) - s64_to_string
        types
            .ty()
            .function([ValType::I64], [ValType::I32, ValType::I32]);

        // Dynamic type registry: every consumer (setter, accessor,
        // ctor, list-ctor, callback import, dispatch, …) gets its own
        // fresh function type. No dedup by `(params, results)` shape —
        // duplicating types is cheap and gives later optimisation
        // passes the freedom to specialise one consumer's signature
        // without affecting the rest.
        const NEXT_DYN_TYPE_IDX: u32 = 34;
        let mut dyn_types: Vec<(Vec<ValType>, Vec<ValType>)> = Vec::new();
        // Names captured here are flushed into `self.function_type_names`
        // at the end of this fn so the name section can apply them.
        let mut dyn_type_names: Vec<(u32, String)> = Vec::new();
        let mut intern_type = |params: Vec<ValType>, results: Vec<ValType>, name: String| -> u32 {
            let idx = NEXT_DYN_TYPE_IDX + dyn_types.len() as u32;
            dyn_types.push((params, results));
            dyn_type_names.push((idx, name));
            idx
        };

        // Precompute setter type indices for every signal of every component
        // (and ctor_at / ctor type indices for every record type) so the
        // function section can emit the right index and `generate_setter_for`
        // can match the body shape.
        let mut setter_type_by_sig: std::collections::HashMap<(usize, usize), u32> =
            std::collections::HashMap::new();
        for (comp_idx, component) in self.components.iter().enumerate() {
            for (sig_idx, signal) in component.signals.iter().enumerate() {
                if matches!(self.ctx.ty_kind(signal.ty), InternedTyKind::Func { .. }) {
                    continue;
                }
                let mut params = vec![ValType::I32]; // self
                params.extend(self.canonical_flat_valtypes(signal.ty));
                let comp_name = to_kebab_case(&self.ctx.str(component.name));
                let sig_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(signal.def_id)));
                let idx = intern_type(
                    params,
                    vec![],
                    format!("type-{}-set-{}", comp_name, sig_name),
                );
                setter_type_by_sig.insert((comp_idx, sig_idx), idx);
            }
        }

        // Precompute ctor_at / ctor type indices for every record type.
        let mut record_ctor_at_types: std::collections::HashMap<DefId, u32> =
            std::collections::HashMap::new();
        let mut record_ctor_types: std::collections::HashMap<DefId, u32> =
            std::collections::HashMap::new();
        for &record_def in &self.record_types {
            let field_params = self.flatten_record_fields_valtypes(record_def);
            let rec_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(record_def)));
            // ctor_at: (dest: i32, ...field_params) -> ()
            let mut at_params = vec![ValType::I32];
            at_params.extend(field_params.iter().copied());
            let at_idx = intern_type(
                at_params,
                vec![],
                format!("type-record-{}-ctor-at", rec_name),
            );
            record_ctor_at_types.insert(record_def, at_idx);
            // ctor: (...field_params) -> i32
            let ctor_idx = intern_type(
                field_params,
                vec![ValType::I32],
                format!("type-record-{}-ctor", rec_name),
            );
            record_ctor_types.insert(record_def, ctor_idx);
        }

        // Precompute list ctor type indices for every (elem_ty, count) pair.
        // Signature: (i32 x total_params) -> (i32, i32) where total_params
        // depends on the element type (strings/lists = 2 i32s, records = sum
        // of field params, primitives = 1 i32). Dynamic interning means
        // arbitrarily large list literals are supported without a static cap.
        let mut list_ctor_types: std::collections::HashMap<(Ty, usize), u32> =
            std::collections::HashMap::new();
        for &(elem_ty, count) in &self.list_constructs {
            // Use canonical-ABI flattening so each element's params carry the
            // right WASM value type (f32 stays f32, not i32). Previously this
            // hardcoded i32 for every param which caused validation failures
            // when `list<f32>` element values were passed as f32.
            let per_elem = self.flatten_core_valtypes(elem_ty);
            let mut params = Vec::with_capacity(per_elem.len() * count);
            for _ in 0..count {
                params.extend_from_slice(&per_elem);
            }
            let results = vec![ValType::I32, ValType::I32];
            let idx = intern_type(
                params,
                results,
                format!("type-list-ctor-{}-x{}", elem_ty.0, count),
            );
            list_ctor_types.insert((elem_ty, count), idx);
        }

        // Precompute callback import type indices. Every callback takes the
        // component's resource handle as the implicit first `i32` param,
        // followed by its declared params (flattened via canonical ABI), and
        // returns its declared result (flattened). Keyed by
        // (component_idx, name) — each component owns its own callback
        // namespace (one WIT interface per component), so sibling
        // components may freely have same-named callbacks with different
        // signatures.
        let precalc_import_layout = ImportLayout::new(&all_components, self.ctx)?;
        let mut callback_import_types: std::collections::HashMap<(usize, String), u32> =
            std::collections::HashMap::new();
        {
            for (comp_idx, comp_layout) in precalc_import_layout.components.iter().enumerate() {
                for &cb_def_id in &comp_layout.callback_def_ids {
                    let (name, params_flat, results_flat) = {
                        let func_def = match self.ctx.defs.as_function(cb_def_id) {
                            Some(f) => f,
                            None => continue,
                        };
                        let name = to_kebab_case(&self.ctx.str(func_def.name));
                        let mut params = vec![ValType::I32]; // self handle
                        for pid in &func_def.params {
                            if let Some(pty) = self.ctx.defs.type_of(*pid) {
                                params.extend(self.canonical_flat_valtypes(pty));
                            }
                        }
                        // Canonical ABI lowering for imports: flat results
                        // of length 0 or 1 are returned directly; anything
                        // larger is returned via a ret_ptr parameter and an
                        // empty core return type.
                        let mut results = if func_def.ret_ty == yel_core::types::Ty::UNIT {
                            Vec::new()
                        } else {
                            self.canonical_flat_valtypes(func_def.ret_ty)
                        };
                        if results.len() > 1 {
                            params.push(ValType::I32); // ret_ptr
                            results = Vec::new();
                        }
                        (name, params, results)
                    };
                    let comp_name = to_kebab_case(&self.ctx.str(all_components[comp_idx].name));
                    let idx = intern_type(
                        params_flat,
                        results_flat,
                        format!("type-{}-callback-{}", comp_name, name),
                    );
                    callback_import_types.insert((comp_idx, name), idx);
                }
            }
        }

        // Precompute filter function type indices. Signature:
        // (i32 x param_count) -> (i32, i32), where param_count = 2 (src ptr+len)
        // plus each captured signal (strings/lists contribute 2, others 1).
        let mut filter_types: Vec<u32> = Vec::with_capacity(self.filter_calls.len());
        for (filter_idx, (_, _, _, _, predicate)) in self.filter_calls.iter().enumerate() {
            let mut captured_signals: Vec<(DefId, Ty)> = Vec::new();
            self.extract_signal_reads(predicate, &mut captured_signals);
            let mut param_count: usize = 2; // src_ptr, src_len
            for (_, ty) in &captured_signals {
                let is_fat_ptr = matches!(
                    self.ctx.ty_kind(*ty),
                    InternedTyKind::String | InternedTyKind::List(_)
                );
                param_count += if is_fat_ptr { 2 } else { 1 };
            }
            let params = vec![ValType::I32; param_count];
            let results = vec![ValType::I32, ValType::I32];
            let idx = intern_type(params, results, format!("type-filter-{}", filter_idx));
            filter_types.push(idx);
        }

        // Pre-register `() -> (slots…)` function types for every
        // multi-slot ternary in the program. Populated by a one-time
        // walk of every component's LIR expressions, driven by the
        // `repr::collect_ternary_block_shapes` helper. At emit time
        // ternaries look up their block type via
        // `WasmPackageBuilder::block_ty_for`, which consults this
        // registry — one source of truth for "multi-slot ternary block
        // type", no per-emit-site recomputation.
        let mut ternary_shapes: std::collections::HashMap<Vec<ValType>, ()> =
            std::collections::HashMap::new();
        crate::wasm::repr::collect_ternary_block_shapes(self, &mut ternary_shapes);
        for (ternary_idx, (shape, ())) in ternary_shapes.into_iter().enumerate() {
            let idx = intern_type(
                vec![],
                shape.clone(),
                format!("type-ternary-block-{}", ternary_idx),
            );
            self.ternary_block_types.insert(shape, idx);
        }

        // Dispatch: `(handler-id: u32, event: event-value) -> ()`.
        // The WIT `event-value` variant flattens under canonical ABI
        // to `(i32 disc, i64 slot0, i32 slot1)` because its payload
        // arms include both f64 (one case) and (i32, i32) strings
        // (another case) — the joined slot0 is an i64 wide enough
        // for f64/f32-reinterpreted/s32/etc., slot1 is the string
        // length.
        let dispatch_type_idx = intern_type(
            vec![
                ValType::I32, // handler_id
                ValType::I32, // event-value discriminant
                ValType::I64, // joined slot 0 (f64 / reinterpret-f32 / s32-ext / ptr)
                ValType::I32, // joined slot 1 (string len, else 0)
            ],
            vec![],
            "type-dispatch".to_string(),
        );

        // Emit the dynamic types at the end of the Type section.
        for (params, results) in &dyn_types {
            types
                .ty()
                .function(params.iter().copied(), results.iter().copied());
        }
        // Flush the names captured during `intern_type` calls to the
        // builder so the name section can stamp each fresh type.
        self.function_type_names.extend(dyn_type_names);

        // GC types live in the type section after the standard +
        // dynamic function types. Per-component emission below appends
        // the mount-tree boundary structs, the `$Comp_<Name>` struct,
        // and the per-component / per-block function types in order;
        // type-index assignments land in each component's `GcTypeLayout`.
        let gc_type_base_after_dyn = NEXT_DYN_TYPE_IDX + dyn_types.len() as u32;
        let mut gc_layouts: Vec<super::super::gc_types::GcTypeLayout> =
            Vec::with_capacity(self.components.len());
        let mut cursor = gc_type_base_after_dyn;

        // Emit module-shared $handle / $handle-array types up-front so
        // every component's registry alloc/lookup helpers can reference
        // these indices freely. Saves 2(N-1) types vs the per-component
        // handle-type emission that preceded this.
        let (shared_handle_idx, shared_handle_arr_idx) =
            super::super::gc_types::emit_shared_handle_types(&mut types, cursor);
        self.shared_handle_type_idx = Some(shared_handle_idx);
        self.shared_handle_arr_type_idx = Some(shared_handle_arr_idx);
        cursor += 2;

        // Phase 1 of records-to-GC migration: emit one `(struct ...)`
        // GC type per user-defined record into a single program-scope
        // rec group. Types are emitted ONCE here, before any per-
        // component rec group, so each component can reference them
        // via the shared type indices in `self.record_gc_types`. No
        // consumer reads these types yet — signal storage, field
        // access, and constructors continue to use the legacy memory
        // path through Phase 1. The types exist for the WAT-inspection
        // test (`gc_record_type_emitted`) and as a foundation for
        // Phase 2's signal-storage switch.
        let (record_types_count, record_gc_types) =
            super::super::gc_types::emit_program_record_types(self.ctx, &mut types, cursor);
        cursor += record_types_count;
        self.record_gc_types = record_gc_types;

        for component in self.components.iter() {
            let parent_retention_count = compute_mount_retention_counts(component);

            // Emit the per-component concrete-typed mount-tree GC
            // types: one struct per `TreeBoundary` plus a companion
            // array per `ForAnchor`. All emission paths route through
            // these typed walks; no legacy iter-rec / for-arr types
            // exist after Step 6.
            let mut layout = super::super::gc_types::GcTypeLayout::default();
            let tree_types_count = super::super::gc_types::emit_component_tree_types(
                &component.tree_shape,
                &mut types,
                cursor,
                &mut layout,
                self.ctx,
                &self.record_gc_types,
            );
            cursor += tree_types_count;

            // `$Comp_<Name>` struct: one mutable field per WASM stack
            // slot each signal occupies in **internal** representation
            // (records / tuples = single i32 pointer; strings / lists =
            // (ptr, len); option / result / variant = flat canonical
            // shape). Mirroring `emit_signal_store`'s shape lets signal
            // get/set route through `struct.get` / `struct.set` directly.
            let signal_slot_valtypes: Vec<Vec<wasm_encoder::ValType>> = component
                .signals
                .iter()
                .map(|sig| self.signal_storage_valtypes(sig.ty))
                .collect();

            super::super::gc_types::emit_component_struct_type(
                &signal_slot_valtypes,
                parent_retention_count,
                &mut types,
                cursor,
                &mut layout,
            );
            cursor += super::super::gc_types::COMPONENT_TYPE_COUNT;

            // Per-component block function types: every effect /
            // update / handler block takes the component's self ref
            // as its implicit first param. Each block gets its OWN
            // unique function type (see the dynamic-types loop
            // below) — there's no longer a pool of shared
            // `block_1p` / `block_2p_void` / `block_2p_i32` types.
            // Duplicating types is cheap and gives later
            // optimisation passes the freedom to specialise one
            // block's signature without affecting unrelated blocks.
            let comp_struct_idx = layout.component_struct_type_idx.ok_or_else(|| {
                CodegenError::InternalError(
                    "GC layout missing component_struct_type_idx after emit_component_struct_type"
                        .into(),
                )
            })?;
            let comp_ref = wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(comp_struct_idx),
            };

            // Internal-tier function types (Step 5 of per-instance migration).
            // Internal callers receive/return the typed self ref directly,
            // bypassing the host's `[resource-new]` round-trip.
            //   constructor_internal:  () -> (ref null $Comp_<i>)
            //   mount_internal:        (ref null $Comp_<i>, root: i32) -> () | i32
            //   unmount_internal:      (ref null $Comp_<i>) -> ()
            let comp_ref_val = wasm_encoder::ValType::Ref(comp_ref);
            let has_children = self
                .ctx
                .defs
                .as_component(component.def_id)
                .map(|c| c.has_children_slot)
                .unwrap_or(false);
            types.ty().function([], [comp_ref_val]);
            layout.constructor_internal_type_idx = Some(cursor);
            cursor += 1;
            if has_children {
                types.ty().function(
                    [comp_ref_val, wasm_encoder::ValType::I32],
                    [wasm_encoder::ValType::I32],
                );
            } else {
                types
                    .ty()
                    .function([comp_ref_val, wasm_encoder::ValType::I32], []);
            }
            layout.mount_internal_type_idx = Some(cursor);
            cursor += 1;
            types.ty().function([comp_ref_val], []);
            layout.unmount_internal_type_idx = Some(cursor);
            cursor += 1;

            // Per-block function types — one fresh type per emitted
            // block. Signature is
            // `(ref $Comp, <args from `params`...>, <ref null bp_0>, ...) -> <ret>`.
            // The mount-block (whose ops live inline in the mount
            // function) is skipped since it isn't emitted as a
            // standalone function.
            //
            // Blocks with neither `params` nor `boundary_params`
            // follow the legacy single-i32-parent calling convention,
            // honoured here by appending an i32 to the param list.
            for block in &component.blocks {
                if block.id == component.mount_block {
                    continue;
                }
                let mut param_valtypes: Vec<wasm_encoder::ValType> =
                    vec![wasm_encoder::ValType::Ref(comp_ref)];
                for ps in &block.params {
                    let val_ty = component
                        .slots
                        .get(ps.0 as usize)
                        .map(|s| s.val_ty)
                        .unwrap_or(LirSlotValType::I32);
                    let v = match val_ty {
                        LirSlotValType::I32 => wasm_encoder::ValType::I32,
                        LirSlotValType::I64 => wasm_encoder::ValType::I64,
                        LirSlotValType::F32 => wasm_encoder::ValType::F32,
                        LirSlotValType::F64 => wasm_encoder::ValType::F64,
                        LirSlotValType::RefNull(idx) => {
                            wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(idx),
                            })
                        }
                        LirSlotValType::RefNullForBoundary(b_id) => {
                            let ty_idx =
                                *layout.tree_struct_type_idx.get(&b_id).ok_or_else(|| {
                                    CodegenError::InvalidIR(format!(
                                        "dynamic block fn type: missing tree struct type \
                                         for boundary {:?}",
                                        b_id
                                    ))
                                })?;
                            wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                            })
                        }
                        LirSlotValType::RefNullForChildrenArray(anchor_id) => {
                            let ty_idx =
                                *layout
                                    .tree_for_arr_type_idx
                                    .get(&anchor_id)
                                    .ok_or_else(|| {
                                        CodegenError::InvalidIR(format!(
                                            "dynamic block fn type: missing children-array \
                                         type for anchor boundary {:?}",
                                            anchor_id
                                        ))
                                    })?;
                            wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                            })
                        }
                        LirSlotValType::RefNullForListGc(list_ty) => {
                            let ty_idx = *self
                                .record_gc_types
                                .list_array_type_idx
                                .get(&list_ty)
                                .ok_or_else(|| {
                                    CodegenError::InvalidIR(format!(
                                        "dynamic block fn type: missing list_array_type_idx \
                                         for list ty {:?}",
                                        list_ty
                                    ))
                                })?;
                            wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                            })
                        }
                        LirSlotValType::RefNullForRecord(record_ty) => {
                            use yel_core::types::InternedTyKind;
                            let def_id = match self.ctx.ty_kind(record_ty) {
                                InternedTyKind::Adt(d) => *d,
                                _ => {
                                    return Err(CodegenError::InvalidIR(format!(
                                        "dynamic block fn type: RefNullForRecord on non-Adt {:?}",
                                        record_ty
                                    )));
                                }
                            };
                            let ty_idx = *self
                                .record_gc_types
                                .record_type_idx
                                .get(&def_id)
                                .ok_or_else(|| {
                                    CodegenError::InvalidIR(format!(
                                        "dynamic block fn type: missing record_type_idx \
                                         for record {:?}",
                                        def_id
                                    ))
                                })?;
                            wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                                nullable: true,
                                heap_type: wasm_encoder::HeapType::Concrete(ty_idx),
                            })
                        }
                    };
                    param_valtypes.push(v);
                }
                for &b_id in &block.boundary_params {
                    let bty = *layout.tree_struct_type_idx.get(&b_id).ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "dynamic block fn type: missing tree struct type for \
                             boundary {:?}",
                            b_id
                        ))
                    })?;
                    param_valtypes.push(wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(bty),
                    }));
                }
                // Legacy single-implicit-i32-parent fallback: when
                // neither LIR params nor boundary refs are declared,
                // the block's calling convention still requires one
                // i32 (the parent DOM handle).
                if block.params.is_empty() && block.boundary_params.is_empty() {
                    param_valtypes.push(wasm_encoder::ValType::I32);
                }

                let results: Vec<wasm_encoder::ValType> = if block.return_slot.is_some() {
                    vec![wasm_encoder::ValType::I32]
                } else {
                    vec![]
                };
                types.ty().function(param_valtypes, results);
                layout.block_dynamic_type_idx.insert(block.id, cursor);
                cursor += 1;
            }

            gc_layouts.push(layout);
        }

        // Per-top-level-for tracking-array globals were removed in
        // Step 5 of the iter-rec migration: each for's children-array
        // now lives in its `ForAnchor.children` BoundaryField slot,
        // reachable via `$self.tree` (top-level fors) or via the bound
        // outer iter-body (nested fors). Allocator globals occupy
        // indices 0..3 (heap_base, heap_ptr, free_list).
        const ALLOCATOR_GLOBAL_COUNT: u32 = 3;
        let _ = &mut gc_layouts; // keep mutable binding shape consistent

        // Per-component handle-registry globals — the per-instance
        // replacement for the previous singleton `(mut (ref null
        // $Comp_<i>))` self-global. Each component gets a triple of
        // `registry / len / free_head` plus the transient
        // `current_handle` i32 used by AddEventListener encoding.
        let mut next_global: u32 = ALLOCATOR_GLOBAL_COUNT;
        for layout in gc_layouts.iter_mut().take(self.components.len()) {
            layout.registry_global = Some(next_global);
            next_global += 1;
            layout.registry_len_global = Some(next_global);
            next_global += 1;
            layout.registry_free_head_global = Some(next_global);
            next_global += 1;
            // Transient `current_handle` i32 global — set by export
            // wrappers on entry, read by `AddEventListener` op emission.
            // See `GcTypeLayout.current_handle_global` for rationale.
            layout.current_handle_global = Some(next_global);
            next_global += 1;
        }

        self.gc_layouts = gc_layouts;

        // Per-named-`global` block GC types. One `(struct $globals_<name>
        // ...)` per block, with one mutable field per ABI slot of each
        // non-pointer property. Pointer-typed properties (records,
        // tuples) keep memory storage and contribute zero fields. The
        // self-global is emitted later in the global section; type
        // index assignment happens here so the encoder's type-section
        // ordering stays linear with `cursor`.
        let mut globals_layouts: Vec<crate::wasm::gc_types::GlobalsBlockLayout> = Vec::new();
        let mut global_block_def_to_idx: std::collections::HashMap<yel_core::DefId, usize> =
            std::collections::HashMap::new();
        let global_block_ids: Vec<yel_core::DefId> = self.ctx.defs.globals().collect();
        for block_def_id in global_block_ids.iter().copied() {
            let block = self
                .ctx
                .defs
                .as_global(block_def_id)
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "globals() iterator yielded {:?} which is not a GlobalDef",
                        block_def_id
                    ))
                })?
                .clone();
            let prop_slot_valtypes: Vec<Vec<wasm_encoder::ValType>> = block
                .properties
                .iter()
                .map(|&prop_id| {
                    let prop_ty = self
                        .ctx
                        .defs
                        .type_of(prop_id)
                        .unwrap_or(yel_core::types::Ty::ERROR);
                    self.signal_storage_valtypes(prop_ty)
                })
                .collect();
            let layout = crate::wasm::gc_types::emit_globals_struct_type(
                block_def_id,
                &prop_slot_valtypes,
                &mut types,
                cursor,
            );
            cursor += 1;
            global_block_def_to_idx.insert(block_def_id, globals_layouts.len());
            globals_layouts.push(layout);
        }
        self.globals_layouts = globals_layouts;
        self.global_block_def_to_idx = global_block_def_to_idx;

        // Phase 5b-v.3: GC list materializer function types.
        // Only emit materializers for SCALAR lists (is_scalar_list_ty).
        // Non-scalar lists (list<record>, list<string>, etc.) have GC array
        // types emitted too (in gc_types.rs) but are not yet migrated to
        // the GC stack representation — they still use FatPointer internally.
        // Added at the end of the type section (after all GC types) so their
        // type indices don't shift the gc_type_base_after_dyn accounting.
        let gc_list_arr_type_idxs: Vec<(yel_core::Ty, u32)> = self
            .record_gc_types
            .list_array_type_idx
            .iter()
            .filter(|&(&ty, _)| self.is_scalar_list_ty(ty))
            .map(|(&ty, &arr_idx)| (ty, arr_idx))
            .collect();
        let mut materializer_type_by_arr_idx: std::collections::HashMap<u32, u32> =
            std::collections::HashMap::new();
        for (i, (_, arr_type_idx)) in gc_list_arr_type_idxs.iter().enumerate() {
            let type_idx = cursor + i as u32;
            let param = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(*arr_type_idx),
            });
            types.ty().function([param], [ValType::I32, ValType::I32]);
            materializer_type_by_arr_idx.insert(*arr_type_idx, type_idx);
        }
        // Phase 5e.6: per-array un-materializer types — (i32, i32) → (ref null $arr).
        let unmat_type_base = cursor + gc_list_arr_type_idxs.len() as u32;
        let mut unmaterializer_type_by_arr_idx: std::collections::HashMap<u32, u32> =
            std::collections::HashMap::new();
        for (i, (_, arr_type_idx)) in gc_list_arr_type_idxs.iter().enumerate() {
            let type_idx = unmat_type_base + i as u32;
            let result = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(*arr_type_idx),
            });
            types
                .ty()
                .function([ValType::I32, ValType::I32], [result]);
            unmaterializer_type_by_arr_idx.insert(*arr_type_idx, type_idx);
        }

        module.section(&types);

        // Import section - no memory import, define it locally instead
        let mut imports = ImportSection::new();
        const DOM_IMPORT: &str = "yel:ui/dom@0.1.0";
        imports.import(DOM_IMPORT, "create-element", EntityType::Function(5));
        imports.import(DOM_IMPORT, "create-text", EntityType::Function(5));
        imports.import(DOM_IMPORT, "create-comment", EntityType::Function(5));
        imports.import(DOM_IMPORT, "set-attribute", EntityType::Function(31)); // attribute-value variant
        imports.import(DOM_IMPORT, "remove-attribute", EntityType::Function(6));
        imports.import(DOM_IMPORT, "set-text-content", EntityType::Function(6));
        imports.import(DOM_IMPORT, "set-style", EntityType::Function(7));
        imports.import(DOM_IMPORT, "set-class", EntityType::Function(6));
        imports.import(DOM_IMPORT, "append-child", EntityType::Function(3));
        imports.import(DOM_IMPORT, "insert-before", EntityType::Function(6));
        imports.import(DOM_IMPORT, "remove-child", EntityType::Function(3));
        imports.import(DOM_IMPORT, "remove", EntityType::Function(1));
        imports.import(DOM_IMPORT, "get-parent", EntityType::Function(4));
        imports.import(DOM_IMPORT, "get-next-sibling", EntityType::Function(4));
        imports.import(DOM_IMPORT, "add-event-listener", EntityType::Function(8));
        imports.import(DOM_IMPORT, "remove-event-listener", EntityType::Function(8));
        imports.import(DOM_IMPORT, "insert-after", EntityType::Function(6)); // (parent, node, anchor) -> ()
        imports.import(DOM_IMPORT, "create-fragment", EntityType::Function(2)); // () -> i32

        // Calculate import layout for all exported components
        let import_layout = ImportLayout::new(&all_components, self.ctx)?;
        // Store import_layout for use in expression emission
        self.import_layout = Some(import_layout.clone());

        // Per-component callbacks interfaces: one WIT interface per
        // component — `{namespace}:{package}/{component}-callbacks@{version}`.
        // Sibling components no longer share a callback namespace, so two
        // `on-submit` callbacks with different signatures compile cleanly
        // (they land in distinct interfaces). Callbacks from non-exported
        // components are still emitted — their component bodies can invoke
        // them from event handlers, so the Call site needs a valid import
        // target. Whether a callback appears in the component's WIT
        // surface is handled by `wit_ast.rs`.
        for &(comp_idx, cb_def_id) in &import_layout.unique_callbacks {
            let name = if let Some(func_def) = self.ctx.defs.as_function(cb_def_id) {
                to_kebab_case(&self.ctx.str(func_def.name))
            } else {
                continue;
            };
            let owner_comp = all_components[comp_idx];
            let comp_kebab = to_kebab_case(&self.ctx.str(owner_comp.name));
            let callbacks_interface =
                if let Some((namespace, pkg_name, version)) = &self.wit_package {
                    format!(
                        "{}:{}/{}-callbacks@{}",
                        namespace, pkg_name, comp_kebab, version
                    )
                } else {
                    format!("yel:ui/{}-callbacks@0.1.0", comp_kebab)
                };
            let type_idx = callback_import_types
                .get(&(comp_idx, name.clone()))
                .copied()
                .unwrap_or(1);
            imports.import(&callbacks_interface, &name, EntityType::Function(type_idx));
        }

        // Import [resource-new] per exported component — each component is
        // its own WASM resource, so this import genuinely is per-component.
        for exported_comp in exported_components.iter() {
            let comp_name = to_kebab_case(&self.ctx.str(exported_comp.name));
            let export_interface = if let Some((namespace, name, version)) = &self.wit_package {
                format!(
                    "[export]{}:{}/{}-component@{}",
                    namespace, name, comp_name, version
                )
            } else {
                format!("[export]yel:ui/{}-component@0.1.0", comp_name)
            };
            let resource_new_name = format!("[resource-new]{}", comp_name);
            imports.import(
                &export_interface,
                &resource_new_name,
                EntityType::Function(4),
            );
        }

        // Note: Allocator functions (alloc, free, cabi_realloc) are LOCAL, not imported.
        module.section(&imports);

        // Collect required concat arities (deduplicated and sorted)
        let mut concat_arities: Vec<usize> = self.concat_arities.clone();
        concat_arities.sort();
        concat_arities.dedup();
        // Ensure at least concat2 is available
        if concat_arities.is_empty() {
            concat_arities.push(2);
        }

        // Function section - first allocator functions, then runtime functions, then component functions
        let mut functions = FunctionSection::new();

        // Local allocator functions (must be first, right after imports):
        // 1. alloc: type 5 - (i32, i32) -> i32
        functions.function(5);
        // 2. free: type 3 - (i32, i32) -> ()
        functions.function(3);
        // 3. cabi_realloc: type 9 - (i32, i32, i32, i32) -> i32
        functions.function(9);

        // Calculate allocator function indices and store in self
        let alloc_funcs = AllocatorFuncs {
            alloc: import_layout.num_imports,
            free: import_layout.num_imports + 1,
            cabi_realloc: import_layout.num_imports + 2,
        };
        self.alloc_funcs = Some(alloc_funcs);

        // Create RuntimeFunctions starting after imports + allocator functions (3)
        let filter_count = self.filter_calls.len();
        let runtime_funcs = RuntimeFunctions::new(
            import_layout.num_imports + 3,
            &concat_arities,
            &self.record_types,
            &self.list_constructs,
            filter_count,
        );
        self.runtime_funcs = Some(runtime_funcs.clone());

        // Local runtime functions (order must match RuntimeFunctions::new):
        // 1. s32_to_string: type 16 - (i32) -> (i32, i32)
        functions.function(16);
        // 2. s64_to_string: type 33 - (i64) -> (i32, i32)
        functions.function(runtime::types::I64_TO_PTR_LEN);
        // 3. bool_to_string: type 16 - (i32) -> (i32, i32)
        functions.function(16);
        // 4. f32_to_string: type 27 - (f32) -> (i32, i32)
        functions.function(27);
        // 4. concat<n> for each required arity (uses cabi_realloc)
        for &arity in &concat_arities {
            // concat2 = type 17, concat3 = type 18, concat4 = type 19, etc.
            let type_idx = 15 + arity as u32; // type 17 for concat2, 18 for concat3, etc.
            functions.function(type_idx);
        }
        // 4. store_fat_ptr: type 6 - (i32, i32, i32) -> ()
        functions.function(6);
        // 5. load_fat_ptr: type 16 - (i32) -> (i32, i32)
        functions.function(16);
        // 6. store_option: type 6 - (i32, i32, i32) -> ()
        functions.function(6);
        // 7. store_result: type 8 - (i32, i32, i32, i32) -> ()
        functions.function(8);
        // 8. list_get: type 9 - (i32, i32, i32, i32) -> i32
        functions.function(runtime::types::LIST_GET);
        // 7. list_get_opt: type 17 - (i32, i32, i32, i32) -> (i32, i32)
        functions.function(runtime::types::LIST_GET_OPT);
        // 7b. list_get_fat: type 17 - (i32, i32, i32, i32) -> (i32, i32)
        functions.function(runtime::types::LIST_GET_FAT);
        // 8. starts_with: type 9 - (i32, i32, i32, i32) -> i32
        functions.function(runtime::types::I32X4_I32);
        // 9. Record constructor helpers for each record type
        // For each record: ctor_at (stores at address) + ctor (allocates and returns)
        // Type indices for record ctors are computed dynamically based on field count
        for &record_def in &self.record_types {
            let ctor_at_type = *record_ctor_at_types.get(&record_def).ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "missing ctor_at type idx for record {:?}",
                    record_def
                ))
            })?;
            functions.function(ctor_at_type);
            let ctor_type = *record_ctor_types.get(&record_def).ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "missing ctor type idx for record {:?}",
                    record_def
                ))
            })?;
            functions.function(ctor_type);
        }

        // 10. List constructor helpers for each (element_type, count) pair.
        // Types were dynamically interned above — look them up by key.
        for &(elem_ty, count) in &self.list_constructs {
            let type_idx = *list_ctor_types.get(&(elem_ty, count)).ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "missing list ctor type idx for elem_ty {:?} count {}",
                    elem_ty, count
                ))
            })?;
            functions.function(type_idx);
        }
        // 11. pack_fat_ptr_to_i64: type 32 - (i32, i32) -> i64
        functions.function(32);

        // 12. Filter functions: (src_ptr, src_len, [captured_signals...]) -> (result_ptr, result_len)
        // Types were dynamically interned above — look them up by index.
        for &type_idx in &filter_types {
            functions.function(type_idx);
        }

        // 13. Phase 5b-v.3: GC list materializer functions.
        // One per unique arr_type_idx: takes (ref null $arr) → returns (i32, i32).
        // Indices follow immediately after filter functions.
        let materializer_base = import_layout.num_imports + 3 + runtime_funcs.count;
        for (i, (_, arr_type_idx)) in gc_list_arr_type_idxs.iter().enumerate() {
            let type_idx = *materializer_type_by_arr_idx
                .get(arr_type_idx)
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "missing materializer type idx for arr_type_idx {}",
                        arr_type_idx
                    ))
                })?;
            functions.function(type_idx);
            self.gc_list_materializer_fn_indices
                .insert(*arr_type_idx, materializer_base + i as u32);
        }
        // 13b. Phase 5e.6: GC list un-materializer functions.
        // One per unique arr_type_idx: (i32, i32) → (ref null $arr).
        let unmat_base = materializer_base + gc_list_arr_type_idxs.len() as u32;
        for (i, (_, arr_type_idx)) in gc_list_arr_type_idxs.iter().enumerate() {
            let type_idx = *unmaterializer_type_by_arr_idx
                .get(arr_type_idx)
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "missing un-materializer type idx for arr_type_idx {}",
                        arr_type_idx
                    ))
                })?;
            functions.function(type_idx);
            self.gc_list_unmaterializer_fn_indices
                .insert(*arr_type_idx, unmat_base + i as u32);
        }

        // For each component: constructor, mount, unmount, getters, setters
        for (comp_idx, component) in self.components.iter().enumerate() {
            functions.function(2); // constructor: () -> i32
            // Mount signature depends on whether the component is a
            // container: container components return the children-root
            // node id `(i32, i32) -> i32` (type 5); non-containers have
            // no return `(i32, i32) -> ()` (type 3).
            let mount_type = if self
                .ctx
                .defs
                .as_component(component.def_id)
                .map(|c| c.has_children_slot)
                .unwrap_or(false)
            {
                5
            } else {
                3
            };
            functions.function(mount_type); // mount
            functions.function(1); // unmount: (self: i32) -> ()

            for (sig_idx, signal) in component.signals.iter().enumerate() {
                // Skip function-typed signals - they're callbacks, not data properties
                if matches!(self.ctx.ty_kind(signal.ty), InternedTyKind::Func { .. }) {
                    continue;
                }
                // Getter type: primitive/f32/f64/i64 signals return the
                // value directly; composite types return a pointer-to-memory
                // *unless* their canonical-ABI flat shape fits in
                // MAX_FLAT_RESULTS=1 — in which case they must be returned
                // directly as the matching flat valtype (e.g.
                // `record R { a: f32 }` -> `(i32) -> f32`, not `(i32) -> i32`).
                let getter_type: u32 = match self.ctx.ty_kind(signal.ty) {
                    InternedTyKind::F32 => 10,
                    InternedTyKind::F64 => 12,
                    InternedTyKind::S64 | InternedTyKind::U64 => 14,
                    // String/List/Option/Result always flatten to >= 2 slots
                    // (ptr+len for String/List, discriminant+payload for
                    // Option/Result), so the pointer convention is correct.
                    InternedTyKind::String | InternedTyKind::List(_) => 4,
                    InternedTyKind::Option(_) | InternedTyKind::Result { .. } => 4,
                    InternedTyKind::Adt(def_id) => {
                        if self.ctx.defs.as_variant(*def_id).is_some() {
                            // Variants always carry a discriminant slot in
                            // addition to any payload slots, so flat arity
                            // is >= 1 and when it's exactly 1 (enum-shape)
                            // the single slot is i32 — pointer-vs-value
                            // alias on i32 keeps type 4 correct.
                            4
                        } else if self.ctx.defs.as_record(*def_id).is_some() {
                            self.single_slot_getter_type(signal.ty)?.unwrap_or(4)
                        } else {
                            // Enum (no payloads): discriminant stored as i32,
                            // returned directly as i32 — type 4 is correct.
                            4
                        }
                    }
                    InternedTyKind::Tuple(_) => {
                        self.single_slot_getter_type(signal.ty)?.unwrap_or(4)
                    }
                    _ => 4,
                };
                // Setter type: dynamically registered (self: i32, ...flatten(T)) -> ().
                let setter_type =
                    *setter_type_by_sig
                        .get(&(comp_idx, sig_idx))
                        .ok_or_else(|| {
                            CodegenError::InternalError(format!(
                                "missing setter type idx for component {} signal {}",
                                comp_idx, sig_idx
                            ))
                        })?;
                functions.function(getter_type);
                functions.function(setter_type);
            }

            // Step 5: internal-tier functions (constructor_internal,
            // mount_internal, unmount_internal). Indices follow the
            // getter/setter pairs to preserve every existing exported
            // index — host-facing exports continue to map to
            // `comp_func_idx + {0,1,2,3+sig*2,..}`.
            let gc = &self.gc_layouts[comp_idx];
            functions.function(gc.constructor_internal_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "component {}: constructor_internal_type_idx not assigned",
                    comp_idx
                ))
            })?);
            functions.function(gc.mount_internal_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "component {}: mount_internal_type_idx not assigned",
                    comp_idx
                ))
            })?);
            functions.function(gc.unmount_internal_type_idx.ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "component {}: unmount_internal_type_idx not assigned",
                    comp_idx
                ))
            })?);
        }

        // Calculate base function index for block functions
        // = imports + allocator funcs (3) + runtime funcs + materializers + component funcs
        let mut block_func_base = import_layout.num_imports
            + 3
            + runtime_funcs.count
            + (gc_list_arr_type_idxs.len() as u32) * 2; // materializers + un-materializers
        for component in self.components {
            // Count only data signals (non-callback signals)
            let data_signal_count = component
                .signals
                .iter()
                .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
                .count() as u32;
            block_func_base += 6 + (data_signal_count * 2);
        }

        // Add block functions for each component
        // Skip mount block since it's handled by the mount method
        for (comp_idx, component) in self.components.iter().enumerate() {
            for block in &component.blocks {
                // Skip mount block - it's generated inline in the mount method
                if block.id == component.mount_block {
                    continue;
                }
                let func_idx = block_func_base + self.block_func_indices.len() as u32;
                self.block_func_indices
                    .insert((comp_idx, block.id), func_idx);
                // Block function types: derived from `block.params.len()`
                // + `return_slot` flag. Blocks with empty `params` default
                // to the 1-i32-param signature (update blocks, handlers).
                // Blocks with `boundary_params` instead opt into a
                // per-block dynamic function type — see lookup below.
                let param_count = if block.params.is_empty() {
                    1
                } else {
                    block.params.len() as u32
                };
                // Every block (except the inlined mount block) gets
                // its own unique function type registered at type-
                // section emission time. Look it up directly — no
                // shape-based fallback.
                let _ = param_count;
                let type_idx = self.gc_layouts[comp_idx]
                    .block_dynamic_type_idx
                    .get(&block.id)
                    .copied()
                    .ok_or_else(|| {
                        CodegenError::InvalidIR(format!(
                            "block {:?} has no registered dynamic function type",
                            block.id
                        ))
                    })?;
                functions.function(type_idx);
            }
        }

        // Standalone dispatch function: carries event-value payload as
        // 6 extra core params (outer_disc + 5 joined slots). See
        // `dispatch_type_idx` defined earlier.
        let dispatch_func_idx = block_func_base + self.block_func_indices.len() as u32;
        functions.function(dispatch_type_idx);
        self.dispatch_func_idx = Some(dispatch_func_idx);

        // Module start function — seeds global singleton property slots with
        // their declared defaults before any export is invoked. Always present
        // (emits an empty body when no globals have defaults).
        let globals_init_func_idx = dispatch_func_idx + 1;
        functions.function(0); // type 0: () -> ()

        // Per-global-signal fanout helpers. One `() -> ()`
        // function per global property whose mutation triggers effects.
        // Body walks each observing component's registry array and
        // calls every live instance's effect block. Index assigned
        // here so call sites can reference it; body emitted in the
        // code section pass below.
        let mut global_signals_with_observers: Vec<DefId> = Vec::new();
        {
            // The function-section pass runs before `global_property_addrs`
            // is populated (that happens in the memory-layout pass below),
            // so derive the set of global property DefIds straight from the
            // def table. Memory addresses aren't needed here — we only need
            // to know **which** DefIds are globals so we can register one
            // fanout helper per observed-elsewhere global.
            let mut seen: std::collections::HashSet<DefId> = std::collections::HashSet::new();
            for global_def_id in self.ctx.defs.globals().collect::<Vec<_>>() {
                let global = match self.ctx.defs.as_global(global_def_id) {
                    Some(g) => g.clone(),
                    None => continue,
                };
                for &prop_id in &global.properties {
                    let observed_anywhere = self
                        .components
                        .iter()
                        .any(|c| c.effects.iter().any(|e| e.dependencies.contains(&prop_id)));
                    if observed_anywhere && seen.insert(prop_id) {
                        global_signals_with_observers.push(prop_id);
                    }
                }
            }
            // Determinism: sort by raw u32 so the WAT diff is stable.
            global_signals_with_observers.sort_by_key(|d| d.0);
        }
        let mut next_fanout_idx = globals_init_func_idx + 1;
        for sig in &global_signals_with_observers {
            functions.function(0); // () -> ()
            self.global_fanout_func_idx.insert(*sig, next_fanout_idx);
            next_fanout_idx += 1;
        }

        module.section(&functions);

        // Memory section - define memory locally (17 pages minimum)
        // Must come after Function section, before Global section
        let mut memories = wasm_encoder::MemorySection::new();
        memories.memory(MemoryType {
            minimum: 17,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);

        // Compute memory layouts BEFORE globals so we know where heap should start
        // Memory layout: [strings] -> [global properties] -> [component state] -> [heap]
        let string_end = self.strings.base() + self.strings.size();
        let mut mem_base = ((string_end + 7) & !7) as i32; // Align to 8 bytes

        // Reserve scratch bytes for the callback return-area. Imported
        // callbacks whose WIT result has >1 canonical-ABI flat slots (e.g.
        // strings, lists, records, options, results, tuples, variants) are
        // lowered with an extra `i32` ret_ptr parameter and the host writes
        // the result into that address. Using a shared scratch slot is safe
        // because callbacks are synchronous and any caller loads the result
        // immediately after the `Call` instruction.
        //
        // Size is precomputed as the max of `size_of(return_ty)` across all
        // unique callbacks (at least 16 bytes so a string/list always fits).
        let mut scratch_size: i32 = 16;
        {
            let precalc_import_layout = ImportLayout::new(&all_components, self.ctx)?;
            for &(_comp_idx, cb_def_id) in &precalc_import_layout.unique_callbacks {
                if let Some(func_def) = self.ctx.defs.as_function(cb_def_id) {
                    let ret_ty = func_def.ret_ty;
                    if ret_ty == yel_core::types::Ty::UNIT {
                        continue;
                    }
                    let flat = self.canonical_flat_valtypes(ret_ty);
                    if flat.len() <= 1 {
                        continue;
                    }
                    let size = self.layout_ctx.size_of(ret_ty) as i32;
                    if size > scratch_size {
                        scratch_size = size;
                    }
                }
            }
        }
        // Round up to 8-byte alignment so it suits i64/f64 loads.
        scratch_size = (scratch_size + 7) & !7;
        self.cb_return_scratch_addr = Some(mem_base);
        mem_base += scratch_size;
        // Reserve an extra 8-byte slot immediately after the return-area
        // scratch to stash the allocated buffer pointer across a
        // pointer-convention indirect-return callback call (record/tuple).
        // `cb_return_scratch_addr + scratch_size` is the per-builder constant
        // address of this stash; see emit_cb_indirect_return_call.
        self.cb_pointer_stash_addr = Some(mem_base);
        mem_base += 8;
        mem_base = (mem_base + 7) & !7;

        // Allocate memory for global singleton properties that are
        // **not** migrated to per-block GC structs. Migrated properties
        // (everything except pointer-typed records/tuples) live in
        // their block's `$globals_<i>` struct fields and don't reserve
        // bytes here. The remaining pointer-typed properties keep the
        // legacy linear-memory path until a future pass moves them too.
        for global_def_id in self.ctx.defs.globals().collect::<Vec<_>>() {
            let global = self
                .ctx
                .defs
                .as_global(global_def_id)
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "global_def_id {:?} is not a GlobalDef during memory reservation",
                        global_def_id
                    ))
                })?
                .clone();
            for &prop_id in &global.properties {
                let prop_ty = self
                    .ctx
                    .defs
                    .type_of(prop_id)
                    .unwrap_or(yel_core::types::Ty::ERROR);
                if !self.signal_storage_valtypes(prop_ty).is_empty() {
                    // Migrated → backed by GC struct fields, no byte
                    // reservation, no `global_property_addrs` entry.
                    continue;
                }
                let size = self.layout_ctx.size_of(prop_ty) as i32;
                self.global_property_addrs.insert(prop_id, mem_base);
                mem_base += size;
            }
        }
        mem_base = (mem_base + 3) & !3; // Re-align after globals

        let mut layouts: Vec<MemoryLayout> = Vec::new();
        for component in self.components {
            let layout = MemoryLayout::new(component, mem_base, &mut self.layout_ctx);
            mem_base += layout.size + 64; // Add padding between components
            layouts.push(layout);
        }
        self.layouts = layouts.clone();

        // Heap starts AFTER all component state, aligned to 8 bytes
        let heap_start = align_to(mem_base as u32, 8);
        self.heap_base = heap_start;
        self.heap_ptr = heap_start;

        // Globals section - allocator globals for local alloc/free/cabi_realloc
        let mut globals = wasm_encoder::GlobalSection::new();
        let allocator_globals = runtime::AllocatorGlobals::new(0);
        for (global_type, init_expr) in runtime::emit_allocator_globals(heap_start) {
            globals.global(global_type, &init_expr);
        }

        {
            use wasm_encoder::{ConstExpr, GlobalType, HeapType, RefType, ValType};
            // Per-top-level-for tracking-array globals were removed in
            // Step 5: each for's children-array lives on its
            // `ForAnchor.children` BoundaryField slot, reachable via
            // `$self.tree`.

            // No per-component singleton self-ref global is emitted;
            // every signal helper sources self from
            // `current_self_local` or via a registry lookup.

            // Per-component handle-registry globals: one triple of
            // `(registry array, len, free_head)` per component. Order
            // matches the assignment loop above.
            //   - registry: `(mut (ref null $CompHandleArr_<i>))`
            //     init `ref.null $CompHandleArr_<i>` — lazily allocated
            //     by the constructor on the first instantiation.
            //   - len: `(mut i32)` init 0 — number of handle entries
            //     in the array (entries 0..len exist, may be free).
            //   - free_head: `(mut i32)` init -1 — head of the free
            //     chain, sentinel `-1` when the chain is empty.
            let shared_arr_ty_idx = self.shared_handle_arr_type_idx.ok_or_else(|| {
                CodegenError::InternalError(
                    "shared_handle_arr_type_idx not set before registry globals emission".into(),
                )
            })?;
            for i in 0..self.components.len() {
                let _ = i;
                let arr_ty_idx = shared_arr_ty_idx;
                globals.global(
                    GlobalType {
                        val_type: ValType::Ref(RefType {
                            nullable: true,
                            heap_type: HeapType::Concrete(arr_ty_idx),
                        }),
                        mutable: true,
                        shared: false,
                    },
                    &ConstExpr::ref_null(HeapType::Concrete(arr_ty_idx)),
                );
                globals.global(
                    GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &ConstExpr::i32_const(0),
                );
                globals.global(
                    GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &ConstExpr::i32_const(-1),
                );
                // current_handle: transient i32 init 0. Set on entry to
                // mount/constructor export wrappers; read by
                // AddEventListener emission inside mount-internal.
                globals.global(
                    GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &ConstExpr::i32_const(0),
                );
            }

            // Per-named-`global` block self-globals: one
            // `(mut (ref null $globals_<i>))` per block, init `ref.null
            // $globals_<i>`. Lazily allocated by `globals_init` (start
            // function) before any export runs. Order matches the type-
            // section pass that populated `self.globals_layouts`.
            //
            // First WASM global index assigned here is whatever the
            // cursor reached after the per-component handle-registry
            // globals just above. Recompute deterministically: 3
            // allocator globals + 4 per component (registry / len /
            // free_head / current_handle).
            const ALLOCATOR_GLOBAL_COUNT: u32 = 3;
            let mut globals_block_global_cursor =
                ALLOCATOR_GLOBAL_COUNT + (self.components.len() as u32) * 4;
            for layout in self.globals_layouts.iter_mut() {
                let struct_ty_idx = layout.struct_type_idx;
                globals.global(
                    GlobalType {
                        val_type: ValType::Ref(RefType {
                            nullable: true,
                            heap_type: HeapType::Concrete(struct_ty_idx),
                        }),
                        mutable: true,
                        shared: false,
                    },
                    &ConstExpr::ref_null(HeapType::Concrete(struct_ty_idx)),
                );
                layout.self_global_idx = globals_block_global_cursor;
                globals_block_global_cursor += 1;
            }
            let _ = globals_block_global_cursor;
        }

        module.section(&globals);

        // Export section - memory, cabi_realloc, and component functions
        let mut exports = ExportSection::new();

        // Export memory (required by canonical ABI)
        exports.export("memory", ExportKind::Memory, 0);

        // Export cabi_realloc (required by canonical ABI for string/list lifting/lowering)
        exports.export("cabi_realloc", ExportKind::Func, alloc_funcs.cabi_realloc);

        // Component function exports
        let first_component_func = import_layout.num_imports
            + 3
            + runtime_funcs.count
            + (gc_list_arr_type_idxs.len() as u32) * 2; // materializers + un-materializers

        // Pre-compute the function-index base for every component by position.
        // Stored on the builder so MountComponent can look it up directly
        // instead of re-walking the prefix sum.
        self.component_func_bases.clear();
        self.component_func_bases.reserve(self.components.len());
        {
            let mut acc = first_component_func;
            for component in self.components.iter() {
                self.component_func_bases.push(acc);
                acc += 6 + (component.signals.len() as u32 * 2);
            }
        }

        for exported_comp in exported_components.iter() {
            let prefix = to_kebab_case(&self.ctx.str(exported_comp.name));

            // Look up this exported component's function-index base by its
            // position in `self.components`.
            let comp_idx_in_all = self
                .components
                .iter()
                .position(|c| c.def_id == exported_comp.def_id)
                .ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "exported component {:?} not found in components slice",
                        exported_comp.def_id
                    ))
                })?;
            let comp_func_idx = self.component_func_bases[comp_idx_in_all];

            // Build interface path prefix if WIT package info is available
            // Format: namespace:name/interface@version#function
            let interface_prefix = if let Some((namespace, name, version)) = &self.wit_package {
                let interface_name = format!("{}-component", prefix);
                format!("{}:{}/{}@{}#", namespace, name, interface_name, version)
            } else {
                String::new()
            };

            exports.export(
                &format!("{}[constructor]{}", interface_prefix, prefix),
                ExportKind::Func,
                comp_func_idx,
            );
            exports.export(
                &format!("{}[method]{}.mount", interface_prefix, prefix),
                ExportKind::Func,
                comp_func_idx + 1,
            );
            exports.export(
                &format!("{}[method]{}.unmount", interface_prefix, prefix),
                ExportKind::Func,
                comp_func_idx + 2,
            );

            for (sig_idx, signal) in exported_comp.signals.iter().enumerate() {
                let getter_idx = comp_func_idx + 3 + (sig_idx as u32 * 2);
                let setter_idx = getter_idx + 1;
                let sig_name = self.signal_name(signal.def_id);
                exports.export(
                    &format!(
                        "{}[method]{}.get-{}",
                        interface_prefix,
                        prefix,
                        to_wit_name(&sig_name)
                    ),
                    ExportKind::Func,
                    getter_idx,
                );
                exports.export(
                    &format!(
                        "{}[method]{}.set-{}",
                        interface_prefix,
                        prefix,
                        to_wit_name(&sig_name)
                    ),
                    ExportKind::Func,
                    setter_idx,
                );
            }
        }

        // Module-level dispatch export. Emitted exactly once under the
        // shared `yel:ui/dispatch@0.1.0` syscall interface — dispatch has
        // a fixed signature and semantics across every Yel module, so it
        // lives next to `yel:ui/dom`, not in a per-package interface.
        if let Some(dispatch_idx) = self.dispatch_func_idx {
            exports.export(
                "yel:ui/dispatch@0.1.0#dispatch",
                ExportKind::Func,
                dispatch_idx,
            );
        }

        module.section(&exports);

        // Start section - runs `globals_init` once at module instantiation,
        // before any export can be invoked. Must come after Export, before Code.
        module.section(&wasm_encoder::StartSection {
            function_index: globals_init_func_idx,
        });

        // Code section - allocator functions, runtime functions, component functions
        // NOTE: Memory layouts were computed earlier (before globals section)
        let mut code = CodeSection::new();

        // Allocator functions (must be first, matching function declaration order):
        // 1. alloc
        code.function(&runtime::emit_alloc(&allocator_globals));
        // 2. free
        code.function(&runtime::emit_free(&allocator_globals));
        // 3. cabi_realloc (calls alloc and free)
        code.function(&runtime::emit_cabi_realloc(
            alloc_funcs.alloc,
            alloc_funcs.free,
        ));

        // Runtime functions (order must match function declarations):
        // 1. s32_to_string
        code.function(&runtime::emit_s32_to_string());
        // 2. s64_to_string
        code.function(&runtime::emit_s64_to_string());
        // 3. bool_to_string
        let (true_ptr, _) = self.strings.get("true").unwrap_or((0, 0));
        let (false_ptr, _) = self.strings.get("false").unwrap_or((0, 0));
        code.function(&runtime::emit_bool_to_string(true_ptr, false_ptr));
        // 4. f32_to_string
        code.function(&runtime::emit_f32_to_string());
        // 4. concat<n> for each required arity (uses bulk memory.copy internally)
        for &arity in &concat_arities {
            code.function(&runtime::emit_concat_n(arity, alloc_funcs.cabi_realloc));
        }
        // 5. store_fat_ptr
        code.function(&runtime::emit_store_fat_ptr());
        // 6. load_fat_ptr
        code.function(&runtime::emit_load_fat_ptr());
        // 7. store_option (for option signal setters)
        code.function(&runtime::emit_store_option());
        // 8. store_result (for result signal setters)
        code.function(&runtime::emit_store_result());
        // 9. list_get (bounds-checked index, traps on OOB)
        code.function(&runtime::emit_list_get());
        // 8. list_get_opt (safe index, returns option)
        // The if block inside needs type () -> (i32, i32) for multi-value result
        code.function(&runtime::emit_list_get_opt(runtime::types::VOID_I32_I32));
        // 8b. list_get_fat (direct index for 2-slot composite elements)
        code.function(&runtime::emit_list_get_fat());
        // 9. starts_with (string prefix check)
        code.function(&runtime::emit_starts_with());
        // 10. Record constructor helpers
        let record_types_clone = self.record_types.clone();
        for record_def in record_types_clone {
            // Generate ctor_at (stores at given address, no locals)
            code.function(&self.generate_record_ctor_at(record_def, alloc_funcs.alloc)?);
            // Generate ctor (allocates and returns ptr)
            code.function(&self.generate_record_ctor(record_def, alloc_funcs.alloc)?);
        }

        // 10. List constructor helpers
        let list_constructs_clone = self.list_constructs.clone();
        for (elem_ty, count) in list_constructs_clone {
            code.function(&self.generate_list_ctor(elem_ty, count, alloc_funcs.alloc)?);
        }
        // 11. pack_fat_ptr_to_i64 (packs ptr/len into i64 for variant ABI)
        code.function(&runtime::emit_pack_fat_ptr_to_i64());

        // 12. Filter functions (specialized per call site)
        //
        // Filter calls carry `Option<usize>` for their owning component:
        //   Some(i) → inside component i (uses its layout for signal reads)
        //   None    → module scope (e.g. in a global-singleton default);
        //             synthesise an empty carrier so any component-local
        //             SignalRead that leaks in fails loudly, while module
        //             globals resolve via `global_property_addrs`.
        let filter_calls_clone = self.filter_calls.clone();
        let module_carrier_name = self.ctx.intern("<module>");
        let module_carrier = LirComponent::empty_module_carrier(module_carrier_name);
        let module_layout = MemoryLayout::empty_for_module();
        for (filter_id, (comp_idx, elem_ty, elem_size, param, predicate)) in
            filter_calls_clone.iter().enumerate()
        {
            let (component, layout) = match comp_idx {
                Some(idx) => (&self.components[*idx], &layouts[*idx]),
                None => (&module_carrier, &module_layout),
            };
            code.function(&self.generate_filter_function(
                filter_id,
                *elem_ty,
                *elem_size,
                *param,
                predicate.clone(),
                alloc_funcs.alloc,
                component,
                layout,
            )?);
        }

        // 13. Phase 5b-v.3: GC list materializer function bodies.
        // Order matches function section declaration above.
        for (ty, arr_type_idx) in &gc_list_arr_type_idxs {
            let elem_ty = match self.ctx.ty_kind(*ty) {
                yel_core::types::InternedTyKind::List(e) => *e,
                _ => {
                    return Err(CodegenError::InternalError(format!(
                        "gc_list materializer: ty {:?} is not a list",
                        ty
                    )));
                }
            };
            code.function(&self.generate_gc_list_materializer(*arr_type_idx, elem_ty)?);
        }
        // 13b. Phase 5e.6: GC list un-materializer function bodies.
        for (ty, arr_type_idx) in &gc_list_arr_type_idxs {
            let elem_ty = match self.ctx.ty_kind(*ty) {
                yel_core::types::InternedTyKind::List(e) => *e,
                _ => {
                    return Err(CodegenError::InternalError(format!(
                        "gc_list un-materializer: ty {:?} is not a list",
                        ty
                    )));
                }
            };
            code.function(&self.generate_gc_list_unmaterializer(*arr_type_idx, elem_ty)?);
        }

        // Release the borrow on exported_components before mutable operations
        drop(exported_components);

        for (comp_idx, layout_ref) in layouts.iter().enumerate() {
            let layout = layout_ref.clone();
            let component = &self.components[comp_idx];

            // For exported components, get the [resource-new] import index.
            // `import_layout.components` is indexed by position in
            // `self.components` (all components), so this is a direct lookup.
            let resource_new_idx: Option<u32> = if component.is_export {
                import_layout.components[comp_idx].resource_new
            } else {
                None
            };

            let num_signals = component.signals.len();

            code.function(&self.generate_constructor_for(
                &self.components[comp_idx],
                &layout,
                resource_new_idx,
                comp_idx,
            )?);
            // Reset handler counter before mount - dispatch will use same ordering
            self.reset_handler_counter();
            code.function(&self.generate_component_mount(comp_idx, &layout)?);
            code.function(&self.generate_unmount_for(
                &self.components[comp_idx],
                &layout,
                comp_idx,
            )?);

            for sig_idx in 0..num_signals {
                let signal_ty = self.components[comp_idx].signals[sig_idx].ty;
                code.function(&self.generate_getter_for_with_struct(
                    signal_ty,
                    &layout,
                    sig_idx,
                    Some(comp_idx),
                )?);
                code.function(&self.generate_setter_for(
                    comp_idx,
                    &layout,
                    sig_idx,
                    alloc_funcs.cabi_realloc,
                )?);
            }

            // Step 5: internal-tier function bodies, emitted after the
            // getter/setter pairs to match the FunctionSection
            // declaration order.
            code.function(&self.generate_constructor_internal_for(comp_idx)?);
            code.function(&self.generate_component_mount_internal(comp_idx, &layout)?);
            code.function(&self.generate_unmount_internal_for(comp_idx)?);
        }

        // Generate block functions for each component
        for comp_idx in 0..self.components.len() {
            let component = &self.components[comp_idx];
            for block in &component.blocks {
                // Skip mount block - it's generated inline in the mount method
                if block.id == component.mount_block {
                    continue;
                }
                code.function(&self.generate_block_function(comp_idx, block.id)?);
            }
        }

        // Standalone dispatch function — routes all handler IDs to their
        // blocks. Layouts are threaded in because binding-setter handlers
        // emit direct signal writes against per-component memory, which
        // requires each component's base offsets.
        code.function(&self.generate_dispatch(&layouts)?);

        // Module start function — seeds global singleton property slots.
        code.function(&self.generate_globals_init()?);

        // Per-global-signal fanout helpers — one `() -> ()` body per
        // entry in `global_fanout_func_idx`, emitted in the same order
        // they were assigned indices above (sorted by DefId.0).
        let mut fanout_sigs: Vec<(DefId, u32)> = self
            .global_fanout_func_idx
            .iter()
            .map(|(d, i)| (*d, *i))
            .collect();
        fanout_sigs.sort_by_key(|(_, idx)| *idx);
        for (sig, _) in &fanout_sigs {
            code.function(&self.generate_global_fanout_for(*sig)?);
        }

        module.section(&code);

        // Data section - emit interned string literals
        if self.strings.size() > 0 {
            let mut data = wasm_encoder::DataSection::new();
            self.strings.emit(&mut data);
            module.section(&data);
        }

        self.generate_name_section_multi(&mut module, &layouts, &import_layout);

        Ok(module)
    }

    /// Generate the standalone dispatch function.
    /// Signature: `(handler-id: i32) -> ()`.
    /// Routes ALL handler IDs across all components to their block functions.
    /// Generate the module start function that seeds global singleton
    /// property slots with their declared defaults. Runs once at module
    /// instantiation, before any export is invoked.
    ///
    /// Non-literal defaults (e.g. cross-global references, arithmetic) still
    /// go through `emit_signal_store` which reuses the same expression
    /// machinery as component constructors.
    fn generate_globals_init(&mut self) -> Result<Function, CodegenError> {
        // Reserve per-valtype scratch locals sized to the widest global
        // default expression under canonical-ABI flattening. Minimum of 3
        // i32 keeps a stable baseline for simple string/result defaults.
        let mut max_counts: (u32, u32, u32, u32) = (3, 0, 0, 0);
        for global_id in self.ctx.defs.globals() {
            let Some(g) = self.ctx.defs.as_global(global_id) else {
                continue;
            };
            for &prop_id in &g.properties {
                if let Some(default) = self.global_defaults.get(&prop_id) {
                    let slots = self.flatten_core_slots(default.ty);
                    merge_max_slot_counts(&mut max_counts, &slots);
                }
            }
        }
        let (max_i32, max_i64, max_f32, max_f64) = max_counts;
        let mut locals: Vec<(u32, ValType)> = Vec::new();
        push_valtype_locals(&mut locals, max_counts);
        let mut func = Function::new(locals);

        // Allocation pass: for each named `global` block, allocate its
        // singleton struct and store the ref in the per-block self-
        // global. Runs before any default-init expression so block B's
        // default that reads block A's property finds A's struct
        // already allocated (zero-initialised by `struct.new_default`).
        for layout in &self.globals_layouts {
            func.instruction(&Instruction::StructNewDefault(layout.struct_type_idx));
            func.instruction(&Instruction::GlobalSet(layout.self_global_idx));
        }

        let globals_scratch = crate::wasm::FlatScratchBases {
            i32_base: 0,
            i32_count: max_i32,
            i64_base: max_i32,
            i64_count: max_i64,
            f32_base: max_i32 + max_i64,
            f32_count: max_f32,
            f64_base: max_i32 + max_i64 + max_f32,
            f64_count: max_f64,
        };

        // Collect (prop_id, default_expr) pairs in global declaration
        // order so output is deterministic. We dispatch per-property
        // below: migrated properties (in the per-block GC struct) go
        // through `emit_global_struct_store_from_expr`; pointer-typed
        // properties (records/tuples) keep the legacy memory path via
        // `emit_signal_store` against `global_property_addrs`.
        let mut inits: Vec<(yel_core::DefId, LirExpr)> = Vec::new();
        for global_id in self.ctx.defs.globals() {
            let Some(g) = self.ctx.defs.as_global(global_id) else {
                continue;
            };
            for &prop_id in &g.properties {
                if let Some(default) = self.global_defaults.get(&prop_id) {
                    inits.push((prop_id, default.clone()));
                }
            }
        }

        if !inits.is_empty() {
            // Module scope has no owning component. `emit_signal_store` /
            // `emit_expr` still take a `&LirComponent` + `&MemoryLayout`,
            // but global defaults never reference component-local state.
            // Handing in an empty carrier turns any accidental
            // component-local lookup into a loud failure — matching the
            // No-Silent-Fallbacks rule.
            let carrier_name = self.ctx.intern("<module>");
            let carrier = LirComponent::empty_module_carrier(carrier_name);
            let layout = MemoryLayout::empty_for_module();
            self.current_init_scratch_start = Some(0);
            self.current_flat_scratch = Some(globals_scratch);
            for (prop_id, expr) in inits {
                if self.global_in_struct(prop_id) {
                    self.emit_global_struct_store_from_expr(
                        &mut func,
                        prop_id,
                        &expr,
                        &carrier,
                        &layout,
                        globals_scratch,
                    )?;
                } else if let Some(&addr) = self.global_property_addrs.get(&prop_id) {
                    self.emit_signal_store(
                        &mut func,
                        addr,
                        &expr,
                        &carrier,
                        &layout,
                        globals_scratch,
                    )?;
                } else {
                    return Err(CodegenError::InvalidIR(format!(
                        "globals_init: property {:?} has a default expression but is \
                         neither in a per-block GC struct nor allocated in linear memory",
                        prop_id
                    )));
                }
            }
            self.current_init_scratch_start = None;
            self.current_flat_scratch = None;
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }
}
