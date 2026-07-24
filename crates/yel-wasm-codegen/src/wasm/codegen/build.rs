//! Core WASM module generation from LIR.
//!
//! This module handles generation of the inner core WASM module including:
//! - Type definitions
//! - Import/export sections
//! - Runtime functions (concat<n>, s32_to_string, bool_to_string)
//! - Function codegen (constructor, mount, unmount, getters, setters) for ALL components + standalone dispatch
//! - Data section for string literals
//! - Name section for debugging

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use wasm_encoder::{
    CodeSection, DataSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
    HeapType, ImportSection, Instruction, MemorySection, MemoryType, Module, RefType, StartSection,
    TypeSection, ValType,
};

use yel_core::lir::{LirExpr, LirResource, align_to};
use yel_core::types::InternedTyKind;
use yel_core::{DefId, Ty};

use super::super::CodegenError;
use super::super::runtime::{self, RuntimeFunctions};
use super::super::{
    FuncTypes, ImportLayout, MemoryLayout, WasmPackageBuilder, to_kebab_case, to_wit_name,
};
use super::scratch::{compute_mount_retention_counts, merge_max_slot_counts, push_valtype_locals};
use crate::wasm::gc_types::{GlobalsBlockLayout, compute_globals_block_layout};
use crate::wasm::{AllocatorFuncs, FlatScratchBases};

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
        let exported_components: Vec<&LirResource> = exported_indices
            .iter()
            .map(|&i| &self.components[i])
            .collect();
        let all_components: Vec<&LirResource> = self.components.iter().collect();

        // Type section
        let mut types = TypeSection::new();

        // All WASM function types are interned into a single growing
        // registry — there is no fixed 0..33 vocabulary and no computed
        // index bases. Each consumer reads a named index from
        // `self.func_types` (compiler-checked); the indices themselves are
        // pure allocation artifacts. (rustc-shaped: types keyed/allocated,
        // not positionally hardcoded.)
        let mut dyn_types: Vec<(Vec<ValType>, Vec<ValType>)> = Vec::new();
        // Names captured here are flushed into `self.function_type_names`
        // at the end of this fn so the name section can apply them.
        let mut dyn_type_names: Vec<(u32, String)> = Vec::new();
        let mut intern_type = |params: Vec<ValType>, results: Vec<ValType>, name: String| -> u32 {
            let idx = dyn_types.len() as u32;
            dyn_types.push((params, results));
            dyn_type_names.push((idx, name));
            idx
        };

        // The function types for codegen's fixed roles, interned by use.
        // One type per role (no shape-sharing); dead DOM-only types are
        // simply never interned.
        let i = ValType::I32;
        let f = ValType::F32;
        let d = ValType::F64;
        let g = ValType::I64;

        // Intern one `concat` type per distinct arity the program uses.
        // Interpolations lower to a `concat` call whose arity is the number
        // of string pieces, so there is no fixed upper bound. Mirror the
        // normalization in the runtime body-gen loop (dedup + always keep
        // arity 2 so the empty-program default at build time has a type).
        let concat_types = {
            let mut arities: Vec<usize> = self.concat_arities.clone();
            arities.push(2);
            arities.sort();
            arities.dedup();
            arities
                .into_iter()
                .map(|arity| {
                    let idx =
                        intern_type(vec![i; 2 * arity], vec![i, i], format!("concat{}", arity));
                    (arity, idx)
                })
                .collect::<rustc_hash::FxHashMap<usize, u32>>()
        };

        let func_types = FuncTypes {
            alloc: intern_type(vec![i, i], vec![i], "alloc".into()),
            free: intern_type(vec![i, i], vec![], "free".into()),
            cabi_realloc: intern_type(vec![i, i, i, i], vec![i], "cabi-realloc".into()),
            constructor: intern_type(vec![], vec![i], "constructor".into()),
            mount_container: intern_type(vec![i, i], vec![i], "mount-container".into()),
            mount_leaf: intern_type(vec![i, i], vec![], "mount-leaf".into()),
            unmount: intern_type(vec![i], vec![], "unmount".into()),
            resource_new: intern_type(vec![i], vec![i], "resource-new".into()),
            getter_i32: intern_type(vec![i], vec![i], "getter-i32".into()),
            getter_f32: intern_type(vec![i], vec![f], "getter-f32".into()),
            getter_f64: intern_type(vec![i], vec![d], "getter-f64".into()),
            getter_i64: intern_type(vec![i], vec![g], "getter-i64".into()),
            s32_to_string: intern_type(vec![i], vec![i, i], "s32-to-string".into()),
            bool_to_string: intern_type(vec![i], vec![i, i], "bool-to-string".into()),
            f32_to_string: intern_type(vec![f], vec![i, i], "f32-to-string".into()),
            s64_to_string: intern_type(vec![g], vec![i, i], "s64-to-string".into()),
            store_fat_ptr: intern_type(vec![i, i, i], vec![], "store-fat-ptr".into()),
            load_fat_ptr: intern_type(vec![i], vec![i, i], "load-fat-ptr".into()),
            pack_fat_ptr: intern_type(vec![i, i], vec![g, i], "pack-fat-ptr".into()),
            starts_with: intern_type(vec![i, i, i, i], vec![i], "starts-with".into()),
            globals_init: intern_type(vec![], vec![], "globals-init".into()),
            cabi_post: intern_type(vec![i], vec![], "cabi-post".into()),
            setter_spill: intern_type(vec![i], vec![], "setter-spill".into()),
            concat: concat_types,
        };
        self.func_types = func_types;

        // Precompute setter type indices for every signal of every component
        // (and ctor_at / ctor type indices for every record type) so the
        // function section can emit the right index and `generate_setter_for`
        // can match the body shape.
        let mut setter_type_by_sig: rustc_hash::FxHashMap<(usize, usize), u32> =
            rustc_hash::FxHashMap::default();
        for (comp_idx, component) in self.components.iter().enumerate() {
            for (sig_idx, signal) in component.signals.iter().enumerate() {
                if matches!(self.ctx.ty_kind(signal.ty), InternedTyKind::Func { .. }) {
                    continue;
                }
                let mut params = vec![ValType::I32]; // self
                params.extend(self.canonical_flat_valtypes(signal.ty, crate::wasm::repr::WitBoundary::assert()));
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
        let mut record_ctor_at_types: rustc_hash::FxHashMap<DefId, u32> =
            rustc_hash::FxHashMap::default();
        let mut record_ctor_types: rustc_hash::FxHashMap<DefId, u32> =
            rustc_hash::FxHashMap::default();
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
        let mut list_ctor_types: rustc_hash::FxHashMap<(Ty, usize), u32> =
            rustc_hash::FxHashMap::default();
        for &(elem_ty, count) in &self.list_constructs {
            // Use canonical-ABI flattening so each element's params carry the
            // right WASM value type (f32 stays f32, not i32). Previously this
            // hardcoded i32 for every param which caused validation failures
            // when `list<f32>` element values were passed as f32.
            let per_elem = self.flatten_core_valtypes(elem_ty, crate::wasm::repr::WitBoundary::assert());
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

        // List-append helper type indices are interned later — after
        // `emit_program_record_types` populates `list_array_type_idx`.
        // See the late-intern block below.
        let mut list_append_types: rustc_hash::FxHashMap<Ty, u32> =
            rustc_hash::FxHashMap::default();
        // List-get helper type indices, interned in the same late block.
        let mut list_get_types: rustc_hash::FxHashMap<Ty, u32> =
            rustc_hash::FxHashMap::default();

        // Precompute the wasm function type for every host import (component
        // callbacks, global callbacks, DOM), keyed by callee `DefId`. The
        // import registry (`self.imports`) is the single source of the set
        // and its order; `import_wasm_type` applies the canonical-ABI
        // lowering (leading `i32` self for `Borrow` receivers, flattened
        // params, ret_ptr for multi-value results). `intern_type` dedups
        // structurally identical signatures.
        let mut import_types: rustc_hash::FxHashMap<DefId, u32> =
            rustc_hash::FxHashMap::default();
        {
            let imports = self.imports.clone();
            for import in &imports {
                let (params_flat, results_flat) = self.import_wasm_type(import);
                let iface_kebab = to_kebab_case(
                    &self.ctx.str(self.import_interfaces[import.interface.index()].name),
                );
                let fname = to_kebab_case(&self.ctx.str(import.name));
                let idx = intern_type(
                    params_flat,
                    results_flat,
                    format!("type-import-{}-{}", iface_kebab, fname),
                );
                import_types.insert(import.def_id, idx);
            }
        }

        // Stage 6: filter type interning is deferred until after
        // `emit_program_record_types` populates `record_gc_types.
        // list_array_type_idx`. The filter signature is now
        // `(ref null $list_arr, ...captured signal storage slots) ->
        // (ref null $list_arr)`. The Vec is filled below at the
        // deferred site and consulted at function-section emit time.
        let mut filter_types: Vec<u32> = Vec::with_capacity(self.filter_calls.len());

        // Pre-register `() -> (slots…)` function types for every
        // multi-slot ternary in the program. Populated by a one-time
        // walk of every component's LIR expressions, driven by the
        // `repr::collect_ternary_block_shapes` helper. At emit time
        // ternaries look up their block type via
        // `WasmPackageBuilder::block_ty_for`, which consults this
        // registry — one source of truth for "multi-slot ternary block
        // type", no per-emit-site recomputation.
        let mut ternary_shapes: rustc_hash::FxHashMap<Vec<ValType>, ()> =
            rustc_hash::FxHashMap::default();
        crate::wasm::repr::collect_ternary_block_shapes(self, &mut ternary_shapes);
        // Determinism (rule #4): the registry is a `RandomState` HashMap, so
        // its iteration order — and hence the type-section indices assigned
        // here — varies run-to-run. Sort the shapes into a stable order before
        // interning so the emitted module is byte-reproducible.
        let mut ternary_shapes: Vec<Vec<ValType>> = ternary_shapes.into_keys().collect();
        ternary_shapes.sort_by_cached_key(|shape| format!("{:?}", shape));
        for (ternary_idx, shape) in ternary_shapes.into_iter().enumerate() {
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
        let gc_type_base_after_dyn = dyn_types.len() as u32;
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
        // Phase 5e.6: seed list/tuple type collection with LIR
        // expression types that emit typed arrays directly — list
        // literals (`array.new_fixed`) and chained typed-array Field
        // reads. This ensures the GC array type is registered even
        // when no signal/record references the literal type.
        //
        // Phase 6: globals migrate alongside components — global-block
        // property list types route through the same typed GC array
        // path, so we no longer need to exclude them.
        // Phase 7 cleanup: seed every reachable LirExpr's `ty` —
        // including nested sub-expressions inside Box<LirExpr> /
        // Vec<LirExpr> children. Sub-expressions like
        // `[true, false][0]` inside an `if` condition have their
        // ListConstruct stored inline, not in `component.exprs`, so a
        // top-level-only walk misses them, leaving the typed GC array
        // unregistered in `list_array_type_idx` and forcing
        // `ListConstruct` to error on the missing GcArrayRef route.
        // The downstream `gc_types` walker is HashSet-deduped, so
        // over-seeding is harmless.
        let mut extra_seed_tys: Vec<yel_core::Ty> = Vec::new();
        fn walk_expr(
            e: &yel_core::lir::LirExpr,
            exprs: &[yel_core::lir::LirExpr],
            out: &mut Vec<yel_core::Ty>,
        ) {
            use yel_core::lir::LirExprKind as K;
            out.push(e.ty);
            match &e.kind {
                K::Binary { lhs, rhs, .. } => {
                    walk_expr(&exprs[lhs.0 as usize], exprs, out);
                    walk_expr(&exprs[rhs.0 as usize], exprs, out);
                }
                K::Unary { operand, .. } => walk_expr(&exprs[operand.0 as usize], exprs, out),
                K::Field { base, .. } => walk_expr(&exprs[base.0 as usize], exprs, out),
                K::Index { base, index } => {
                    walk_expr(&exprs[base.0 as usize], exprs, out);
                    walk_expr(&exprs[index.0 as usize], exprs, out);
                }
                K::Call { args, .. } => {
                    for a in args {
                        walk_expr(&exprs[a.0 as usize], exprs, out);
                    }
                }
                K::Ternary {
                    condition,
                    then_expr,
                    else_expr,
                } => {
                    walk_expr(&exprs[condition.0 as usize], exprs, out);
                    walk_expr(&exprs[then_expr.0 as usize], exprs, out);
                    walk_expr(&exprs[else_expr.0 as usize], exprs, out);
                }
                K::VariantCtor { payload, .. } => {
                    if let Some(p) = payload {
                        walk_expr(&exprs[p.0 as usize], exprs, out);
                    }
                }
                K::IsCase { base, .. } | K::VariantField { base, .. } => {
                    walk_expr(&exprs[base.0 as usize], exprs, out)
                }
                K::ListConstruct { elements, .. } | K::TupleConstruct { elements, .. } => {
                    for el in elements {
                        walk_expr(&exprs[el.0 as usize], exprs, out);
                    }
                }
                K::RecordConstruct { fields, .. } => {
                    for f in fields {
                        walk_expr(&exprs[f.0 as usize], exprs, out);
                    }
                }
                K::Range { start, end, .. } => {
                    walk_expr(&exprs[start.0 as usize], exprs, out);
                    walk_expr(&exprs[end.0 as usize], exprs, out);
                }
                K::Local(_)
                | K::Def(_)
                | K::Literal(_)
                | K::SignalRead(_)
                | K::EnumCase { .. }
                | K::ListStatic { .. }
                | K::Closure { .. } => {}
            }
        }
        for component in self.components.iter() {
            for expr in &component.exprs {
                walk_expr(expr, &component.exprs, &mut extra_seed_tys);
            }
        }
        // Global-block property defaults (e.g. `global S { v: result<string,
        // string> = ok("x"); }`) live in a separate arena that the component
        // walk above never visits. Their types must be seeded too, or
        // `internal_repr` panics ("option/result Ty not registered as
        // GcVariant") when the global's storage valtype is resolved. Both
        // arenas are flat — every subexpression is its own entry — so a plain
        // push of each entry's type covers the whole tree without recursion.
        for (_, top) in self.global_defaults.iter() {
            extra_seed_tys.push(top.ty);
        }
        for expr in self.global_default_exprs.iter() {
            extra_seed_tys.push(expr.ty);
        }
        // A global property with no default has no expr to seed from, yet its
        // declared type still needs GC-type registration (the globals-layout
        // pass below calls `signal_storage_valtypes` on it). Seed every global
        // property's type directly.
        for block_def_id in self.ctx.defs.globals().collect::<Vec<_>>() {
            if let Some(block) = self.ctx.defs.as_global(block_def_id) {
                for &prop_id in &block.properties {
                    if let Some(prop_ty) = self.ctx.defs.type_of(prop_id) {
                        extra_seed_tys.push(prop_ty);
                    }
                }
            }
        }
        let (record_types_count, record_gc_types) =
            super::super::gc_types::emit_program_record_types(
                self.ctx,
                &mut types,
                cursor,
                &extra_seed_tys,
            );
        cursor += record_types_count;
        self.record_gc_types = record_gc_types;

        // Every valid `list<T>` is a typed GC array — `ListConstruct` emits
        // `array.new_fixed` (the `GcArrayRef` branch), never the legacy memory
        // `list_ctor` helper. Those helpers are therefore dead, and worse than
        // useless: `generate_list_ctor` stores each element as a single i32, so
        // for a tuple / record / float element it emits `i32.store` of an f64 /
        // multi-field value and fails core validation (a dead function still
        // gets validated). Drop every collected list-construct whose list type
        // is a typed GC array so no dead, invalid helper is generated. Only a
        // genuinely non-GC-array list (none exist for valid programs today)
        // would keep a legacy ctor.
        let gc_array_elem_tys: HashSet<Ty> = self
            .record_gc_types
            .list_array_type_idx
            .keys()
            .filter_map(|&list_ty| match self.ctx.ty_kind(list_ty) {
                InternedTyKind::List(elem) => Some(*elem),
                _ => None,
            })
            .collect();
        self.list_constructs
            .retain(|(elem_ty, _)| !gc_array_elem_tys.contains(elem_ty));

        // Stage 6: now that `list_array_type_idx` is populated, intern
        // the per-filter signatures `(ref null $list_arr, ...captured
        // signal storage slots) -> (ref null $list_arr)`. Push directly
        // into the type section because the `intern_type` closure's
        // borrow on `dyn_types` ended when that vec was flushed earlier.
        let filter_call_count = self.filter_calls.len();
        for filter_idx in 0..filter_call_count {
            let (pred_comp_idx, list_ty, _, _, predicate) = self.filter_calls[filter_idx].clone();
            let arr_type_idx = *self
                .record_gc_types
                .list_array_type_idx
                .get(&list_ty)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "filter type registration: missing list_array_type_idx for {:?}",
                        list_ty
                    ))
                })?;
            let arr_ref = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
            });
            let mut params: Vec<ValType> = vec![arr_ref];
            let mut captured_signals: Vec<(DefId, Ty)> = Vec::new();
            // The predicate body's `LirExprId` children index into the arena
            // of the predicate's owning component, or the shared global-default
            // arena for module-scope filters.
            let pred_exprs: &[LirExpr] = match pred_comp_idx {
                Some(ci) => &self.components[ci].exprs,
                None => &self.global_default_exprs,
            };
            self.extract_signal_reads(&predicate, pred_exprs, &mut captured_signals);
            for (_, ty) in &captured_signals {
                params.extend(self.signal_storage_valtypes(*ty));
            }
            let results = [arr_ref];
            types
                .ty()
                .function(params.iter().copied(), results.iter().copied());
            let idx = cursor;
            cursor += 1;
            self.function_type_names
                .push((idx, format!("type-filter-{}", filter_idx)));
            filter_types.push(idx);
        }

        // Intern list-append helper signatures now that the GC type
        // registry has `list_array_type_idx` for every reachable list
        // type. One per unique `list<T>` referenced by an `append` call.
        // Signature: `(ref null $list_arr, <elem-storage>) -> (ref null $list_arr)`.
        for &list_ty in &self.list_appends.clone() {
            let elem_ty = match self.ctx.ty_kind(list_ty) {
                InternedTyKind::List(e) => *e,
                _ => continue,
            };
            let arr_type_idx = *self
                .record_gc_types
                .list_array_type_idx
                .get(&list_ty)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "list_append type registration: missing list_array_type_idx for {:?}",
                        list_ty
                    ))
                })?;
            let arr_ref = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
            });
            let elem_val_ty = super::super::gc_types::list_element_storage_type_pub(
                self.ctx,
                elem_ty,
                &self.record_gc_types,
            );
            types.ty().function([arr_ref, elem_val_ty], [arr_ref]);
            let idx = cursor;
            cursor += 1;
            self.function_type_names
                .push((idx, format!("type-list-append-{}", list_ty.0)));
            list_append_types.insert(list_ty, idx);
        }

        // List-get helper types: `(ref null $list_arr, i32) -> <option repr>`.
        // The result valtype is the single-slot repr of the call's `option<T>`.
        for &(list_ty, option_ty) in &self.list_gets.clone() {
            let arr_type_idx = *self
                .record_gc_types
                .list_array_type_idx
                .get(&list_ty)
                .ok_or_else(|| {
                    CodegenError::InvalidIR(format!(
                        "list_get type registration: missing list_array_type_idx for {:?}",
                        list_ty
                    ))
                })?;
            let arr_ref = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(arr_type_idx),
            });
            let option_vt = match self.signal_storage_valtypes(option_ty).as_slice() {
                [vt] => *vt,
                other => {
                    return Err(CodegenError::InvalidIR(format!(
                        "list_get type registration: option result {:?} is not a \
                         single-slot ref (got {} slots)",
                        option_ty,
                        other.len()
                    )));
                }
            };
            types.ty().function([arr_ref, ValType::I32], [option_vt]);
            let idx = cursor;
            cursor += 1;
            self.function_type_names
                .push((idx, format!("type-list-get-{}", list_ty.0)));
            list_get_types.insert(list_ty, idx);
        }

        for component in self.components.iter() {
            let parent_retention_count = compute_mount_retention_counts(component);
            // Phase 0.3f cross-check: LIR-side comp_struct_layout
            // mirrors codegen's field-allocation order. Catch drift in
            // debug builds.
            debug_assert_eq!(
                parent_retention_count, component.comp_struct_layout.parent_retention_count,
                "comp_struct_layout: parent_retention_count drift for {:?}",
                component.def_id,
            );

            // Emit the per-component concrete-typed mount-tree GC
            // types: one struct per `TreeBoundary` plus a companion
            // array per `ForAnchor`. All emission paths route through
            // these typed walks; no legacy iter-rec / for-arr types
            // exist after Step 6.
            let mut layout = super::super::gc_types::GcTypeLayout::default();
            let tree_types_count = super::super::gc_types::emit_component_tree_types(
                component,
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
                component,
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
            // L3-v2 Phase 2: every per-block function type is now
            // derived from the block's `CallingConv` — built once via
            // `ui_block_calling_conv` and consumed by
            // `register_wasm_function_type`. The inline 150-line
            // construction has been factored into:
            //
            //   * `lir::function::ui_block_calling_conv` (yel-core) —
            //     encodes UI's "(ref $Comp) self-ref + boundary refs
            //     + legacy i32 fallback + i32 return" conventions.
            //   * `WasmPackageBuilder::wasm_function_type_for_conv`
            //     (codegen/function_type.rs) — converts the conv +
            //     user params into wasm `(ValType, ValType)` vectors.
            //
            // Flow functions will reuse `register_wasm_function_type`
            // with a `FreeFunction`-shaped conv (no implicit params,
            // returns derived from the return slot's val_ty).
            // Phase 0.3l: register lifecycle blocks (mount /
            // internal ctor / internal unmount) into
            // `block_dynamic_type_idx` using the already-assigned
            // internal-tier type indices. This lets the per-block
            // emission loops below treat them uniformly with user
            // blocks — no fixed-position carveout, no separate type
            // lookup path. `ui_block_calling_conv` isn't applicable
            // here because lifecycle blocks have role-specific
            // wasm-param shapes (ctor returns the typed ref; mount
            // takes an extra i32 root param; unmount has only the
            // self-ref).
            if let Some(ctor_block_id) = component.internal_constructor_block {
                let ty_idx = layout.constructor_internal_type_idx.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "component {:?}: constructor_internal_type_idx not assigned",
                        component.def_id
                    ))
                })?;
                layout.block_dynamic_type_idx.insert(ctor_block_id, ty_idx);
            }
            {
                let ty_idx = layout.mount_internal_type_idx.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "component {:?}: mount_internal_type_idx not assigned",
                        component.def_id
                    ))
                })?;
                layout
                    .block_dynamic_type_idx
                    .insert(component.mount_block, ty_idx);
            }
            if let Some(unmount_block_id) = component.internal_unmount_block {
                let ty_idx = layout.unmount_internal_type_idx.ok_or_else(|| {
                    CodegenError::InternalError(format!(
                        "component {:?}: unmount_internal_type_idx not assigned",
                        component.def_id
                    ))
                })?;
                layout
                    .block_dynamic_type_idx
                    .insert(unmount_block_id, ty_idx);
            }

            for block in &component.blocks {
                if block.id == component.mount_block {
                    continue;
                }
                if Some(block.id) == component.internal_constructor_block {
                    continue;
                }
                if Some(block.id) == component.internal_unmount_block {
                    continue;
                }
                // Phase 0.3m: skip synthesized export-wrapper blocks —
                // those are declared via the fixed-position 3-per-
                // component slots in the function section below
                // (types 2, 3/5, 1) and don't get a per-block dynamic
                // type entry.
                if Some(block.id) == component.export_constructor_block {
                    continue;
                }
                if Some(block.id) == component.export_mount_block {
                    continue;
                }
                if Some(block.id) == component.export_unmount_block {
                    continue;
                }
                let conv = yel_core::lir::function::ui_block_calling_conv(
                    block,
                    component.def_id,
                    &component.slots,
                );
                let type_idx = self.register_wasm_function_type(
                    &mut types,
                    &mut cursor,
                    &conv,
                    &block.params,
                    &component.slots,
                    Some(&block.slots),
                    &layout,
                )?;
                layout.block_dynamic_type_idx.insert(block.id, type_idx);
                let _ = comp_ref;
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

        // Per-named-`global` block storage layouts. Each non-pointer
        // property slot is backed by a core wasm global (assigned in the
        // global section); pointer-typed properties (records, tuples)
        // keep linear-memory storage and contribute zero slots. No GC
        // type is emitted for a global block — globals are singletons,
        // not instantiable, so they need no struct.
        let mut globals_layouts: Vec<GlobalsBlockLayout> = Vec::new();
        let mut global_block_def_to_idx: HashMap<DefId, usize> = HashMap::default();
        let global_block_ids: Vec<DefId> = self.ctx.defs.globals().collect();

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
            let prop_slot_valtypes: Vec<Vec<ValType>> = block
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

            let layout = compute_globals_block_layout(block_def_id, &prop_slot_valtypes);

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
        let mut gc_list_arr_type_idxs: Vec<(yel_core::Ty, u32)> = self
            .record_gc_types
            .list_array_type_idx
            .iter()
            .filter(|&(&ty, _)| self.is_scalar_list_ty(ty))
            .map(|(&ty, &arr_idx)| (ty, arr_idx))
            .collect();
        // Determinism: `list_array_type_idx` is a HashMap — sort by the GC
        // array type index so the materializer type/function order (and with
        // it the emitted wasm) is byte-stable across runs.
        gc_list_arr_type_idxs.sort_by_key(|&(_, arr_idx)| arr_idx);
        // strings-to-GC (`plans/strings-to-gc.md`): the `$str_bytes` array
        // gets a materializer/un-materializer too, keyed by `Ty::STRING`.
        // Appended last so it never perturbs the list entries' indices.
        // Its body-emission is dispatched specially (String kind, not List).
        {
            let str_bytes_idx = self
                .record_gc_types
                .str_bytes_array_idx
                .expect("gc_list_arr_type_idxs: $str_bytes array type must be registered");
            gc_list_arr_type_idxs.push((yel_core::Ty::STRING, str_bytes_idx));
        }
        let mut materializer_type_by_arr_idx: HashMap<u32, u32> = HashMap::default();
        for (i, (_, arr_type_idx)) in gc_list_arr_type_idxs.iter().enumerate() {
            let type_idx = cursor + i as u32;
            let param = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(*arr_type_idx),
            });
            types.ty().function([param], [ValType::I32, ValType::I32]);
            materializer_type_by_arr_idx.insert(*arr_type_idx, type_idx);
        }
        // Phase 5e.6: per-array un-materializer types — (i32, i32) → (ref null $arr).
        let unmat_type_base = cursor + gc_list_arr_type_idxs.len() as u32;
        let mut unmaterializer_type_by_arr_idx: HashMap<u32, u32> = HashMap::default();
        for (i, (_, arr_type_idx)) in gc_list_arr_type_idxs.iter().enumerate() {
            let type_idx = unmat_type_base + i as u32;
            let result = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(*arr_type_idx),
            });
            types.ty().function([ValType::I32, ValType::I32], [result]);
            unmaterializer_type_by_arr_idx.insert(*arr_type_idx, type_idx);
        }

        // Phase 7: pack_color_to_attr_slots type — only registered if
        // the program references the language `color` type. Signature:
        // (ref null $var_color) → (i64 inner_disc, i32 r, i32 g, i32 b, i32 a).
        // Locate the color Ty (Adt of `known.variants.color`) by
        // scanning the gc-variant registry rather than constructing a
        // fresh interned Ty (which would require mutable ctx).
        let color_def_id = self.ctx.known.variants.color;
        let color_ty_for_helper = color_def_id.and_then(|d| {
            self.record_gc_types
                .gc_variant_super_idx
                .keys()
                .copied()
                .find(|ty| {
                    matches!(
                        self.ctx.ty_kind(*ty),
                    InternedTyKind::Adt(adt_d) if *adt_d == d
                    )
                })
        });
        let color_super_idx = color_ty_for_helper
            .and_then(|ty| self.record_gc_types.gc_variant_super_idx.get(&ty).copied());
        // Use the type section's actual length as the about-to-be-
        // assigned type index — `cursor` only tracks GC types and
        // doesn't account for materializer/un-materializer function
        // types appended just above this point. `types.len()` is the
        // ground truth.
        // Phase 7: pack_color_to_attr_slots type — registered only
        // when the program references the language `color` type.
        // Signature: `(ref null $var_color) → (i64, i32 ×4)` — 1 i64
        // for the inner color disc widened, 4 i32s for the rgba
        // tuple bytes (zero for non-rgba cases).
        //
        // Type-section subtype indices advance by `record_types_count`
        // etc. via `cursor`, while `types.len()` only counts the
        // number of `types.ty()` calls (one per rec group regardless
        // of subtype count). The pack_color helper is appended after
        // the materializer / un-materializer single-sub function
        // types, so its subtype index is `cursor + materializer_count
        // + un_materializer_count` = `cursor + 2 * len`.
        let pack_color_type_idx = if let Some(super_idx) = color_super_idx {
            let param = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(super_idx),
            });
            let type_idx = cursor + (gc_list_arr_type_idxs.len() as u32) * 2;
            types.ty().function(
                [param],
                [
                    ValType::I64,
                    ValType::I32,
                    ValType::I32,
                    ValType::I32,
                    ValType::I32,
                ],
            );
            Some(type_idx)
        } else {
            None
        };

        module.section(&types);

        // Import section - no memory import, define it locally instead
        let mut imports = ImportSection::new();

        // Calculate the import index space from the module's import registry.
        let import_layout = ImportLayout::new(&self.imports, &all_components);
        // Store import_layout for use in expression emission
        self.import_layout = Some(import_layout.clone());

        // Emit every host import from the module's registry, in registry
        // order (which fixes the import indices). Each import's interface name
        // and wasm type come from the same contract the WIT was rendered from,
        // so the core import section and the WIT world cannot disagree. A
        // `Borrow` receiver (component callbacks) carries a leading `i32`
        // self handle; global / DOM callbacks are freestanding.
        {
            let imports_list = self.imports.clone();
            for import in &imports_list {
                let iface_name = self.import_interface_name(import.interface);
                let fname = to_kebab_case(&self.ctx.str(import.name));
                let type_idx = import_types.get(&import.def_id).copied().unwrap_or(1);
                imports.import(&iface_name, &fname, EntityType::Function(type_idx));
            }
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
                EntityType::Function(self.func_types.resource_new),
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
        functions.function(self.func_types.alloc);
        // 2. free: type 3 - (i32, i32) -> ()
        functions.function(self.func_types.free);
        // 3. cabi_realloc: type 9 - (i32, i32, i32, i32) -> i32
        functions.function(self.func_types.cabi_realloc);

        // Calculate allocator function indices and store in self
        let alloc_funcs = AllocatorFuncs {
            alloc: import_layout.num_imports,
            free: import_layout.num_imports + 1,
            cabi_realloc: import_layout.num_imports + 2,
        };
        self.alloc_funcs = Some(alloc_funcs);

        // Create RuntimeFunctions starting after imports + allocator functions (3)
        let filter_count = self.filter_calls.len();
        let runtime_needs = self.runtime_needs;
        let list_appends_clone = self.list_appends.clone();
        let list_gets_clone = self.list_gets.clone();
        let runtime_funcs = RuntimeFunctions::new(
            import_layout.num_imports + 3,
            runtime_needs,
            &concat_arities,
            &self.record_types,
            &self.list_constructs,
            &list_appends_clone,
            &list_gets_clone,
            filter_count,
        );
        self.runtime_funcs = Some(runtime_funcs.clone());

        // Local runtime functions (order MUST match RuntimeFunctions::new
        // so each `functions.function(type)` lines up with the index that
        // `new` assigned). Skipped helpers (None) consume neither an
        // index nor a function-section entry.
        if runtime_needs.s32_to_string {
            // type 16 - (i32) -> (i32, i32)
            functions.function(self.func_types.s32_to_string);
        }
        if runtime_needs.s64_to_string {
            // type 33 - (i64) -> (i32, i32)
            functions.function(self.func_types.s64_to_string);
        }
        if runtime_needs.bool_to_string {
            // type 16 - (i32) -> (i32, i32)
            functions.function(self.func_types.bool_to_string);
        }
        if runtime_needs.f32_to_string {
            // type 27 - (f32) -> (i32, i32)
            functions.function(self.func_types.f32_to_string);
        }
        // concat<n> for each required arity (uses cabi_realloc)
        for &arity in &concat_arities {
            // One interned type per arity (see the `concat_types` map built
            // in the type section). The map is keyed over the same
            // normalized arity set this loop iterates, so the lookup hits.
            let type_idx = *self
                .func_types
                .concat
                .get(&arity)
                .unwrap_or_else(|| panic!("concat type for arity {} not interned", arity));
            functions.function(type_idx);
        }
        if runtime_needs.store_fat_ptr {
            // type 6 - (i32, i32, i32) -> ()
            functions.function(self.func_types.store_fat_ptr);
        }
        if runtime_needs.load_fat_ptr {
            // type 16 - (i32) -> (i32, i32)
            functions.function(self.func_types.load_fat_ptr);
        }
        if runtime_needs.starts_with {
            // type 9 - (i32, i32, i32, i32) -> i32
            functions.function(self.func_types.starts_with);
        }
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
        // 10b. List append helpers (one per unique list type).
        for &list_ty in &self.list_appends {
            let type_idx = *list_append_types.get(&list_ty).ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "missing list append type idx for {:?}",
                    list_ty
                ))
            })?;
            functions.function(type_idx);
        }
        // 10c. List get helpers (one per unique list type).
        for &(list_ty, _option_ty) in &self.list_gets {
            let type_idx = *list_get_types.get(&list_ty).ok_or_else(|| {
                CodegenError::InternalError(format!("missing list get type idx for {:?}", list_ty))
            })?;
            functions.function(type_idx);
        }
        if runtime_needs.pack_fat_ptr_to_i64 {
            // type 32 - (i32, i32) -> i64
            functions.function(self.func_types.pack_fat_ptr);
        }

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
        // 13c. Phase 7: pack_color_to_attr_slots — emitted only if the
        // program references the language `color` type. The function
        // declaration MUST come right after the un-materializer block
        // because the type signature was registered there; any later
        // function-section insertions would shift the function index
        // away from `materializer_base + 2*len`.
        // Use functions.len() + (existing imports + allocator + runtime
        // funcs already declared) to compute the helper's true function
        // index. The function-index space layout: imports first, then
        // declared local functions in order. By the time we get here,
        // every function declared so far in this section gets a
        // function index = num_imports + functions.len() (where
        // functions.len() counts only declarations made via
        // `functions.function(...)`).
        // Function index = imports + (declarations made so far in
        // this section). `functions.len()` is reliable because the
        // function section uses one entry per declared function with
        // no rec-group bundling.
        let pack_color_helper_fn_idx_local = pack_color_type_idx.map(|type_idx| {
            let fn_idx = import_layout.num_imports + functions.len();
            functions.function(type_idx);
            self.pack_color_helper_fn_idx = Some(fn_idx);
            fn_idx
        });

        // For each component: constructor, mount, unmount, getters, setters
        for (comp_idx, component) in self.components.iter().enumerate() {
            // Post-cleanup: export wrappers (ctor / mount / unmount) only
            // emitted for is_export components. Non-exported components
            // have no host-facing surface and nothing calls these
            // wrappers internally — they were dead slots.
            if component.is_export {
                functions.function(self.func_types.constructor); // constructor: () -> i32
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
                    self.func_types.mount_container
                } else {
                    self.func_types.mount_leaf
                };
                functions.function(mount_type); // mount
                functions.function(self.func_types.unmount); // unmount: (self: i32) -> ()
            }

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
                let ft = &self.func_types;
                let getter_type: u32 = match self.ctx.ty_kind(signal.ty) {
                    InternedTyKind::F32 => ft.getter_f32,
                    InternedTyKind::F64 => ft.getter_f64,
                    InternedTyKind::S64 | InternedTyKind::U64 => ft.getter_i64,
                    // String/List/Option/Result always flatten to >= 2 slots
                    // (ptr+len for String/List, discriminant+payload for
                    // Option/Result), so the pointer convention is correct.
                    InternedTyKind::String | InternedTyKind::List(_) => ft.getter_i32,
                    InternedTyKind::Option(_) | InternedTyKind::Result { .. } => ft.getter_i32,
                    InternedTyKind::Adt(def_id) => {
                        if self.ctx.defs.as_variant(*def_id).is_some() {
                            // Variants always carry a discriminant slot in
                            // addition to any payload slots, so flat arity
                            // is >= 1 and when it's exactly 1 (enum-shape)
                            // the single slot is i32 — pointer-vs-value
                            // alias on i32 keeps the i32 getter correct.
                            ft.getter_i32
                        } else if self.ctx.defs.as_record(*def_id).is_some() {
                            self.single_slot_getter_type(signal.ty)?
                                .unwrap_or(ft.getter_i32)
                        } else {
                            // Enum (no payloads): discriminant stored as i32,
                            // returned directly as i32 — i32 getter is correct.
                            ft.getter_i32
                        }
                    }
                    InternedTyKind::Tuple(_) => self
                        .single_slot_getter_type(signal.ty)?
                        .unwrap_or(ft.getter_i32),
                    _ => ft.getter_i32,
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

            // Phase 0.3l: internal-tier functions (constructor_internal,
            // mount_internal, unmount_internal) no longer occupy fixed
            // per-component positions. They are declared by the per-
            // block loop below (alongside user blocks) and their wasm
            // indices are tracked in `block_func_indices`.
        }

        // Calculate base function index for block functions
        // = imports + allocator funcs (3) + runtime funcs + materializers + component funcs
        let first_component_func_local = import_layout.num_imports
            + 3
            + runtime_funcs.count
            + (gc_list_arr_type_idxs.len() as u32) * 2 // materializers + un-materializers
            + if pack_color_helper_fn_idx_local.is_some() { 1 } else { 0 };
        let mut block_func_base = first_component_func_local;
        // Phase 0.3m: per-component prefix base in the function-index
        // space — points at the component's exported constructor.
        // Used by the per-block loop below to record fixed wasm indices
        // for the synthesized export-wrapper blocks.
        let mut comp_func_bases_local: Vec<u32> = Vec::with_capacity(self.components.len());
        for component in self.components {
            comp_func_bases_local.push(block_func_base);
            // Count only data signals (non-callback signals)
            let data_signal_count = component
                .signals
                .iter()
                .filter(|s| !matches!(self.ctx.ty_kind(s.ty), InternedTyKind::Func { .. }))
                .count() as u32;
            // Phase 0.3l: lifecycle blocks (internal ctor / mount /
            // internal unmount) no longer occupy fixed per-component
            // positions — they now flow through the regular per-block
            // loop below. Post-cleanup: export wrappers (ctor / mount /
            // unmount) only exist for is_export components — non-exported
            // components contribute only 2*N getter/setter pairs.
            let export_slots = if component.is_export { 3 } else { 0 };
            block_func_base += export_slots + (data_signal_count * 2);
        }

        // Add block functions for each component.
        // Phase 0.3l: lifecycle blocks (mount / internal ctor / internal
        // unmount) flow through this loop too — they're registered in
        // `block_func_indices` like every other block, and the export
        // wrappers + `MountComponent` op look up their wasm indices
        // here instead of computing `base + 3 + 2N + {0,1,2}`.
        // Phase 0.3m: track the running per-block function count
        // separately from `block_func_indices.len()` because the
        // synthesized export-wrapper blocks get fixed indices inserted
        // into the map but DO NOT contribute to the per-block running
        // counter (they live at the 3 per-component fixed slots
        // declared earlier in the function section).
        let mut per_block_running: u32 = 0;
        for (comp_idx, component) in self.components.iter().enumerate() {
            for block in &component.blocks {
                // Phase 0.3m: synthesized export-wrapper blocks occupy
                // the fixed 3 per-component slots declared earlier in
                // the function section — they don't participate in
                // the per-block loop. Record their wasm indices in
                // `block_func_indices` so name-section labels and
                // intra-component CallBlock dispatch (if any) resolve.
                if Some(block.id) == component.export_constructor_block {
                    self.block_func_indices
                        .insert(block.id, comp_func_bases_local[comp_idx]);
                    continue;
                }
                if Some(block.id) == component.export_mount_block {
                    self.block_func_indices
                        .insert(block.id, comp_func_bases_local[comp_idx] + 1);
                    continue;
                }
                if Some(block.id) == component.export_unmount_block {
                    self.block_func_indices
                        .insert(block.id, comp_func_bases_local[comp_idx] + 2);
                    continue;
                }
                let func_idx = block_func_base + per_block_running;
                per_block_running += 1;
                self.block_func_indices.insert(block.id, func_idx);
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
        let dispatch_func_idx = block_func_base + per_block_running;
        functions.function(dispatch_type_idx);
        self.dispatch_func_idx = Some(dispatch_func_idx);

        // Module start function — seeds global singleton property slots with
        // their declared defaults before any export is invoked. Always present
        // (emits an empty body when no globals have defaults).
        let globals_init_func_idx = dispatch_func_idx + 1;
        functions.function(self.func_types.globals_init); // type 0: () -> ()

        // Global-signal fanout is served by per-component LIR blocks
        // synthesized in yel-core's `resolve_global_triggers` pass —
        // they are ordinary block functions, so no dedicated helper
        // functions are registered here.

        // Gap 1 — post-return (`cabi_post_*`) functions. An exported getter
        // whose result is a multi-slot composite freshly materialised into
        // linear memory (GC-migrated signals, `signal_in_struct`) hands the
        // host a heap buffer that the canonical ABI reclaims via a
        // `cabi_post_<export>` function. Memory-resident composite getters are
        // SKIPPED — they return a pointer into live signal storage, so freeing
        // them would be a use-after-free. These functions are appended at the
        // function-index tail (after the fanout helpers) so no existing index
        // shifts; the encoder auto-wires them by the `cabi_post_` name prefix.
        let cabi_post_base = globals_init_func_idx + 1;
        let mut cabi_post_plan: Vec<(u32, usize, usize, Ty)> = Vec::new();
        {
            let mut next = cabi_post_base;
            for comp_idx in 0..self.components.len() {
                if !self.components[comp_idx].is_export {
                    continue;
                }
                let sig_count = self.components[comp_idx].signals.len();
                for sig_idx in 0..sig_count {
                    let ty = self.components[comp_idx].signals[sig_idx].ty;
                    if matches!(self.ctx.ty_kind(ty), InternedTyKind::Func { .. }) {
                        continue;
                    }
                    // Returned-by-value (≤1 flat slot) getters allocate nothing.
                    if self.canonical_flat_valtypes(ty, crate::wasm::repr::WitBoundary::assert()).len() <= 1 {
                        continue;
                    }
                    // Only GC-migrated getters materialise a fresh buffer;
                    // memory-resident ones alias persistent storage.
                    if !self.signal_in_struct(comp_idx, sig_idx) {
                        continue;
                    }
                    cabi_post_plan.push((next, comp_idx, sig_idx, ty));
                    next += 1;
                }
            }
        }
        for _ in &cabi_post_plan {
            functions.function(self.func_types.cabi_post); // (i32) -> ()
        }

        // Gap 3 — pointer-spill trampolines. An exported setter whose flattened
        // params exceed `MAX_FLAT_PARAMS` (16, counting the `self` handle) is
        // lowered by the canonical ABI as a single pointer to the param tuple
        // in linear memory. The wide-signature setter stays at its slot (no
        // longer exported); a `(i32 ptr) -> ()` trampoline appended here at the
        // tail is exported in its place and forwards to it. Indices follow the
        // cabi_post block so neither shifts existing functions.
        let spill_base = cabi_post_base + cabi_post_plan.len() as u32;
        let mut setter_spill_plan: Vec<(u32, usize, usize, Ty)> = Vec::new();
        {
            let mut next = spill_base;
            for comp_idx in 0..self.components.len() {
                if !self.components[comp_idx].is_export {
                    continue;
                }
                let sig_count = self.components[comp_idx].signals.len();
                for sig_idx in 0..sig_count {
                    let ty = self.components[comp_idx].signals[sig_idx].ty;
                    if matches!(self.ctx.ty_kind(ty), InternedTyKind::Func { .. }) {
                        continue;
                    }
                    // self handle (1) + flattened value > MAX_FLAT_PARAMS (16).
                    if 1 + self.canonical_flat_valtypes(ty, crate::wasm::repr::WitBoundary::assert()).len() <= 16 {
                        continue;
                    }
                    setter_spill_plan.push((next, comp_idx, sig_idx, ty));
                    next += 1;
                }
            }
        }
        for _ in &setter_spill_plan {
            functions.function(self.func_types.setter_spill); // (i32) -> ()
        }

        module.section(&functions);

        // Memory section - define memory locally (17 pages minimum)
        // Must come after Function section, before Global section
        let mut memories = MemorySection::new();
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
            let imports_list = self.imports.clone();
            for import in &imports_list {
                let Some(ret_ty) = import.result else {
                    continue;
                };
                if ret_ty == Ty::UNIT {
                    continue;
                }
                let flat =
                    self.canonical_flat_valtypes(ret_ty, crate::wasm::repr::WitBoundary::assert());
                if flat.len() <= 1 {
                    continue;
                }
                let size = self.layout_ctx.size_of(ret_ty) as i32;
                if size > scratch_size {
                    scratch_size = size;
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

        // §1.5: no linear-memory reservation for global properties —
        // every non-unit property is backed by a core wasm global
        // (declared from `GlobalsBlockLayout.field_valtypes`).

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

            // Per-field core wasm globals: one `(mut <valtype>)` per
            // storage field of each global block — the live state of the
            // module's singletons. Reads (`SignalRead`) and writes
            // (`GlobalFieldSet`) resolve to these. Globals are singletons,
            // not instantiable, so there is no per-block struct or
            // self-global — just module-level globals.
            //
            // First WASM global index assigned here is whatever the
            // cursor reached after the per-component handle-registry
            // globals just above: 3 allocator globals + 4 per component
            // (registry / len / free_head / current_handle).
            const ALLOCATOR_GLOBAL_COUNT: u32 = 3;
            let mut globals_block_global_cursor =
                ALLOCATOR_GLOBAL_COUNT + (self.components.len() as u32) * 4;
            for layout in self.globals_layouts.iter_mut() {
                let mut field_globals = Vec::with_capacity(layout.field_valtypes.len());
                for vt in &layout.field_valtypes {
                    let init = match vt {
                        ValType::I32 => ConstExpr::i32_const(0),
                        ValType::I64 => ConstExpr::i64_const(0),
                        ValType::F32 => ConstExpr::f32_const(wasm_encoder::Ieee32::from(0.0)),
                        ValType::F64 => ConstExpr::f64_const(wasm_encoder::Ieee64::from(0.0)),
                        ValType::Ref(rt) => ConstExpr::ref_null(rt.heap_type),
                        ValType::V128 => ConstExpr::v128_const(0),
                    };
                    globals.global(
                        GlobalType {
                            val_type: *vt,
                            mutable: true,
                            shared: false,
                        },
                        &init,
                    );
                    field_globals.push(globals_block_global_cursor);
                    globals_block_global_cursor += 1;
                }
                layout.field_core_globals = field_globals;
            }
        }

        module.section(&globals);

        // Export section - memory, cabi_realloc, and component functions
        let mut exports = ExportSection::new();

        // Export memory (required by canonical ABI)
        exports.export("memory", ExportKind::Memory, 0);

        // Export cabi_realloc (required by canonical ABI for string/list lifting/lowering)
        exports.export("cabi_realloc", ExportKind::Func, alloc_funcs.cabi_realloc);

        // Per-component registry globals — exported only so external
        // tooling (yel-host's `gc-dump` subcommand) can reach the typed
        // `(array (ref null $handle))` registry from the host's GC API.
        // Each export name is `<comp-name>-registry`. wit-component
        // hides core-module exports from the WIT surface, so these
        // don't leak into the public component world.
        for (comp_idx, component) in self.components.iter().enumerate() {
            let prefix = to_kebab_case(&self.ctx.str(component.name));
            let registry = self.gc_layouts[comp_idx].registry_global;
            if let Some(g) = registry {
                exports.export(&format!("{}-registry", prefix), ExportKind::Global, g);
            }
        }

        // Component function exports
        let first_component_func = import_layout.num_imports
            + 3
            + runtime_funcs.count
            + (gc_list_arr_type_idxs.len() as u32) * 2 // materializers + un-materializers
            + if pack_color_helper_fn_idx_local.is_some() { 1 } else { 0 };

        // Pre-compute the function-index base for every component by position.
        // Stored on the builder so MountComponent can look it up directly
        // instead of re-walking the prefix sum.
        self.component_func_bases.clear();
        self.component_func_bases.reserve(self.components.len());
        {
            let mut acc = first_component_func;
            for component in self.components.iter() {
                self.component_func_bases.push(acc);
                // Phase 0.3l: prefix shrinks from 6+2N to 3+2N — the
                // 3 internal-tier lifecycle entries now emit through
                // the per-block loop and are looked up via
                // `block_func_indices`, not via `func_base + 3 + 2N + k`.
                // Post-cleanup: export wrappers (3) only emitted for
                // is_export components — non-exported contribute 2N only.
                let export_slots = if component.is_export { 3 } else { 0 };
                acc += export_slots + (component.signals.len() as u32 * 2);
            }
        }

        // Gap 3: maps a spill trampoline's index → the wide setter's wasm
        // index it forwards to. Filled here in the export loop (where the wide
        // setter index is known) and read by the code-section body pass.
        let mut spill_wide_idx: rustc_hash::FxHashMap<u32, u32> =
            rustc_hash::FxHashMap::default();
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
                let getter_name = format!(
                    "{}[method]{}.get-{}",
                    interface_prefix,
                    prefix,
                    to_wit_name(&sig_name)
                );
                exports.export(&getter_name, ExportKind::Func, getter_idx);
                // Gap 1: pair the getter with its post-return when it
                // materialises a fresh buffer (see `cabi_post_plan`). The
                // canonical name is `cabi_post_` + the full getter export name.
                if let Some(&(post_idx, _, _, _)) = cabi_post_plan
                    .iter()
                    .find(|(_, c, s, _)| *c == comp_idx_in_all && *s == sig_idx)
                {
                    exports.export(
                        &format!("cabi_post_{}", getter_name),
                        ExportKind::Func,
                        post_idx,
                    );
                }
                let setter_name = format!(
                    "{}[method]{}.set-{}",
                    interface_prefix,
                    prefix,
                    to_wit_name(&sig_name)
                );
                // Gap 3: if this setter's params spill, export the trampoline
                // in place of the wide setter and record the forward target.
                if let Some(&(tramp_idx, _, _, _)) = setter_spill_plan
                    .iter()
                    .find(|(_, c, s, _)| *c == comp_idx_in_all && *s == sig_idx)
                {
                    spill_wide_idx.insert(tramp_idx, setter_idx);
                    exports.export(&setter_name, ExportKind::Func, tramp_idx);
                } else {
                    exports.export(&setter_name, ExportKind::Func, setter_idx);
                }
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
        module.section(&StartSection {
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

        // Runtime functions (order must match function-section declarations
        // in the type section pass — each `code.function(...)` is gated by
        // the same `runtime_needs.X` flag used there).
        if runtime_needs.s32_to_string {
            code.function(&runtime::emit_s32_to_string());
        }
        if runtime_needs.s64_to_string {
            code.function(&runtime::emit_s64_to_string());
        }
        if runtime_needs.bool_to_string {
            let (true_ptr, _) = self.strings.get("true").unwrap_or((0, 0));
            let (false_ptr, _) = self.strings.get("false").unwrap_or((0, 0));
            code.function(&runtime::emit_bool_to_string(true_ptr, false_ptr));
        }
        if runtime_needs.f32_to_string {
            code.function(&runtime::emit_f32_to_string());
        }
        // concat<n> for each required arity (uses bulk memory.copy internally)
        for &arity in &concat_arities {
            code.function(&runtime::emit_concat_n(arity, alloc_funcs.cabi_realloc));
        }
        if runtime_needs.store_fat_ptr {
            code.function(&runtime::emit_store_fat_ptr());
        }
        if runtime_needs.load_fat_ptr {
            code.function(&runtime::emit_load_fat_ptr());
        }
        if runtime_needs.starts_with {
            code.function(&runtime::emit_starts_with());
        }
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
        // 10b. List append helpers
        let list_appends_clone = self.list_appends.clone();
        for list_ty in list_appends_clone {
            code.function(&self.generate_list_append_function(list_ty)?);
        }
        // 10c. List get helpers
        let list_gets_clone = self.list_gets.clone();
        for (list_ty, option_ty) in list_gets_clone {
            code.function(&self.generate_list_get_function(list_ty, option_ty)?);
        }
        if runtime_needs.pack_fat_ptr_to_i64 {
            code.function(&runtime::emit_pack_fat_ptr_to_i64());
        }

        // 12. Filter functions (specialized per call site)
        //
        // Filter calls carry `Option<usize>` for their owning component:
        //   Some(i) → inside component i (uses its layout for signal reads)
        //   None    → module scope (e.g. in a global-singleton default);
        //             emit against a signal-less carrier so any component-local
        //             SignalRead that leaks in fails loudly, while module
        //             globals resolve via their core-global backing. The
        //             carrier holds the global-default arena so a module-scope
        //             predicate's `LirExprId` children resolve.
        let filter_calls_clone = self.filter_calls.clone();
        let module_carrier_name = self.ctx.intern("<module>");
        let module_carrier = LirResource::module_scope_carrier(
            module_carrier_name,
            self.global_default_exprs.clone(),
        );
        for (filter_id, (comp_idx, elem_ty, elem_size, param, predicate)) in
            filter_calls_clone.iter().enumerate()
        {
            let component = match comp_idx {
                Some(idx) => &self.components[*idx],
                None => &module_carrier,
            };
            code.function(&self.generate_filter_function(
                filter_id,
                *elem_ty,
                *elem_size,
                *param,
                predicate.clone(),
                alloc_funcs.alloc,
                component,
            )?);
        }

        // 13. Phase 5b-v.3: GC list materializer function bodies.
        // Order matches function section declaration above.
        for (ty, arr_type_idx) in &gc_list_arr_type_idxs {
            if matches!(self.ctx.ty_kind(*ty), InternedTyKind::String) {
                code.function(&self.generate_str_bytes_materializer(*arr_type_idx)?);
                continue;
            }
            let elem_ty = match self.ctx.ty_kind(*ty) {
                InternedTyKind::List(e) => *e,
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
            if matches!(self.ctx.ty_kind(*ty), InternedTyKind::String) {
                code.function(&self.generate_str_bytes_unmaterializer(*arr_type_idx)?);
                continue;
            }
            let elem_ty = match self.ctx.ty_kind(*ty) {
                InternedTyKind::List(e) => *e,
                _ => {
                    return Err(CodegenError::InternalError(format!(
                        "gc_list un-materializer: ty {:?} is not a list",
                        ty
                    )));
                }
            };
            code.function(&self.generate_gc_list_unmaterializer(*arr_type_idx, elem_ty)?);
        }

        // 13c. Phase 7: pack_color_to_attr_slots body. Order MUST
        // match the function-section declaration above.
        if let Some(color_ty) = color_ty_for_helper {
            code.function(&self.generate_pack_color_to_attr_slots(color_ty)?);
        }

        // Release the borrow on exported_components before mutable operations
        drop(exported_components);

        for (comp_idx, _) in layouts.iter().enumerate() {
            let component = &self.components[comp_idx];

            // For exported components, get the [resource-new] import index.
            let resource_new_idx: Option<u32> = if component.is_export {
                import_layout.resource_new.get(&component.def_id).copied()
            } else {
                None
            };

            let num_signals = component.signals.len();

            // Phase 0.3m / cleanup: export wrappers are emitted ONLY
            // for exported components. Non-exported components have no
            // host-facing WIT surface and nothing internal calls their
            // exported wrappers (cross-component mount routes through
            // internal_constructor_block / mount_block via CallBlock).
            // The 3 inline fallback generators (generate_constructor_for /
            // generate_component_mount / generate_unmount_for) are
            // deleted.
            let _ = resource_new_idx;
            if component.is_export {
                let ctor_block = component
                    .export_constructor_block
                    .expect("exported component must have export_constructor_block synthesized");
                code.function(&self.generate_block_function(comp_idx, ctor_block)?);
                // Reset handler counter before mount - dispatch uses same ordering
                self.reset_handler_counter();
                let mount_block = component
                    .export_mount_block
                    .expect("exported component must have export_mount_block synthesized");
                code.function(&self.generate_block_function(comp_idx, mount_block)?);
                let unmount_block = component
                    .export_unmount_block
                    .expect("exported component must have export_unmount_block synthesized");
                code.function(&self.generate_block_function(comp_idx, unmount_block)?);
            } else {
                self.reset_handler_counter();
            }

            for sig_idx in 0..num_signals {
                let signal_ty = self.components[comp_idx].signals[sig_idx].ty;
                code.function(&self.generate_getter_for_with_struct(
                    signal_ty,
                    sig_idx,
                    Some(comp_idx),
                )?);
                code.function(&self.generate_setter_for(
                    comp_idx,
                    sig_idx,
                    alloc_funcs.cabi_realloc,
                )?);
            }
        }

        // Generate block functions for each component.
        // Phase 0.3l: includes lifecycle blocks (mount / internal ctor
        // / internal unmount). Their wasm indices live in
        // `block_func_indices`, populated during function-section
        // emission, and the export wrappers + MountComponent op look
        // them up by block id.
        for comp_idx in 0..self.components.len() {
            let component = &self.components[comp_idx];
            for block in &component.blocks {
                // Phase 0.3m: export wrappers were emitted at the
                // fixed 3 per-component positions above (constructor /
                // mount / unmount). Skip them here so we don't double-
                // emit their bodies.
                if Some(block.id) == component.export_constructor_block
                    || Some(block.id) == component.export_mount_block
                    || Some(block.id) == component.export_unmount_block
                {
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

        // Gap 1: post-return bodies, in the SAME order their indices/types
        // were assigned above. Each frees the freshly-materialised buffer
        // graph of one aggregate-returning getter (`free` index from the
        // allocator funcs).
        let free_fn = alloc_funcs.free;
        for &(_idx, _comp_idx, _sig_idx, ty) in &cabi_post_plan {
            code.function(&self.generate_cabi_post_getter(ty, free_fn)?);
        }

        // Gap 3: spill-trampoline bodies, in the SAME order their indices/types
        // were assigned. Each forwards to its wide setter.
        for &(tramp_idx, _comp_idx, _sig_idx, value_ty) in &setter_spill_plan {
            let wide = *spill_wide_idx.get(&tramp_idx).ok_or_else(|| {
                CodegenError::InternalError(format!(
                    "spill trampoline {} has no recorded wide setter index",
                    tramp_idx
                ))
            })?;
            code.function(&self.generate_setter_spill_trampoline(value_ty, wide)?);
        }

        module.section(&code);

        // Data section - emit interned string literals
        if self.strings.size() > 0 {
            let mut data = DataSection::new();
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

        // No allocation pass: global state lives in core wasm globals
        // (declared with zero/null inits in the global section), so there
        // is no singleton struct to `struct.new`. The default-init loop
        // below seeds each property's core global(s).

        let globals_scratch = FlatScratchBases {
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
        // order so output is deterministic. §1.5: every non-unit
        // property is core-global-backed, so each init goes through
        // `emit_global_struct_store_from_expr`.
        let mut inits: Vec<(DefId, LirExpr)> = Vec::new();
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
            // `emit_expr` still take a `&LirResource`, but global defaults
            // never reference component-local state — a signal-less carrier
            // makes any accidental component-local lookup a loud failure
            // (No-Silent-Fallbacks). It carries the global-default expression
            // arena so `emit_expr` resolves each default's `LirExprId`
            // children.
            let carrier_name = self.ctx.intern("<module>");
            let carrier = LirResource::module_scope_carrier(
                carrier_name,
                self.global_default_exprs.clone(),
            );
            self.current_init_scratch_start = Some(0);
            self.current_flat_scratch = Some(globals_scratch);
            for (prop_id, expr) in inits {
                if self.global_in_struct(prop_id) {
                    self.emit_global_struct_store_from_expr(
                        &mut func,
                        prop_id,
                        &expr,
                        &carrier,
                        globals_scratch,
                    )?;
                } else {
                    return Err(CodegenError::InvalidIR(format!(
                        "globals_init: property {:?} has a default expression but no \
                         core-global backing (§1.5: no memory-resident globals)",
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
