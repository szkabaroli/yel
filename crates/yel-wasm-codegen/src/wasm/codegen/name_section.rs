//! Name-section emission: function names, type names, local-variable
//! names, label names, and data-segment names. All of these are debug
//! hints — the WASM module remains valid even if the section is omitted —
//! but `wasm-tools print` and similar tooling rely on them for readable
//! output, so we go to some lengths to keep them in sync with the actual
//! emission order.

use wasm_encoder::{IndirectNameMap, Module, NameMap, NameSection};
use yel_core::BlockDebugName;
use yel_core::Ty;
use yel_core::lir::{LirBlock, LirSlotKind};

/// Build the WASM name-section function name for a block, in the form
/// `<comp>-<kind>[-b<bid>]*[-s<sid>]#<block_id>`.
///
/// All inputs come from structured data captured at lowering time —
/// `BlockDebugName.kind` / `.signal` plus the block's `boundary_params`.
/// No string parsing.
fn build_block_func_name(
    comp_prefix: &str,
    name: &BlockDebugName,
    block: &LirBlock,
    slots: &[yel_core::lir::LirSlotInfo],
    block_id_raw: u32,
) -> String {
    let mut s = format!("{}-{}", comp_prefix, name.kind);
    // Stage 5c: derive boundary-id list for the debug suffix from
    // `boundary_param_slots` (slot val_ty carries the id) instead of
    // reading the parallel `boundary_params` field. Same labels.
    for bp in block.boundary_param_ids_from_slots(slots) {
        s.push_str(&format!("-b{}", bp.0));
    }
    if let Some(sig) = name.signal {
        s.push_str(&format!("-s{}", sig.index()));
    }
    s.push_str(&format!("#{}", block_id_raw));
    s
}

use super::super::{ImportLayout, MemoryLayout, WasmPackageBuilder, to_kebab_case, to_wit_name};

impl<'a> WasmPackageBuilder<'a> {
    pub(super) fn generate_name_section_multi(
        &mut self,
        module: &mut Module,
        layouts: &[MemoryLayout],
        import_layout: &ImportLayout,
    ) {
        let exported_components = self.get_exported_components();
        let mut names = NameSection::new();

        // Debug name for the core module. Prefer the package header
        // (`{namespace}-{name}`) — the module is the compilation unit, not
        // any single component. Fall back to the first exported component's
        // name for legacy / anonymous-module callers.
        let module_debug_name: String = if let Some((namespace, name, _version)) = &self.wit_package
        {
            format!("{}-{}", namespace, name)
        } else if let Some(first_export) = exported_components.first() {
            self.ctx.str(first_export.name).to_string()
        } else {
            "yel-module".to_string()
        };
        names.module(&module_debug_name);

        // =================================================================
        // Type names - describe each function type signature
        // =================================================================
        let mut type_names = NameMap::new();
        // Function-type names come from `intern_type` (applied via
        // `self.function_type_names` below) — no fixed-index block.

        // Module-shared registry-handle types (one pair per module
        // instead of per-component pre-unification).
        if let Some(idx) = self.shared_handle_type_idx {
            type_names.append(idx, "handle");
        }
        if let Some(idx) = self.shared_handle_arr_type_idx {
            type_names.append(idx, "handle-array");
        }

        // Phase 1 records-to-GC: per-record program-scope GC struct
        // types. Names follow the convention `$<lowercased_name>_record`
        // (e.g. record `Point` → `point_record`). Type indices were
        // assigned in `emit_program_record_types`; the registry holds
        // pre-formatted name strings for direct emission here.
        // Sort by type index so the name subsection's entries appear
        // in ascending index order — `wasm-encoder::NameMap::append`
        // requires monotonic indices.
        let mut record_type_names: Vec<(u32, String)> = self.record_gc_types.type_names.clone();
        record_type_names.sort_by_key(|(idx, _)| *idx);
        for (idx, name) in record_type_names {
            type_names.append(idx, &name);
        }

        // GC types + per-for mount function types, keyed by their
        // registered type-section indices (computed in Phase-1 emission).
        for (comp_idx, gc_layout) in self.gc_layouts.iter().enumerate() {
            let comp = &self.components[comp_idx];
            let comp_name = to_kebab_case(&self.ctx.str(comp.name));

            // Mount-tree boundary structs + per-ForAnchor companion
            // arrays. Names follow the boundary's kind so the WAT is
            // self-documenting (`<comp>_tree_root`, `<comp>_if_<id>`,
            // `<comp>_if_<id>_then`, `<comp>_for_<id>`,
            // `<comp>_for_<id>_arr`, `<comp>_for_<id>_iter`).
            // Stage 5d: walk the resource's struct-type registry and
            // use its pre-baked `name` field directly. struct_types[i]
            // corresponds 1:1 to TreeBoundaryId(i) so the type-section
            // index lookup goes through the same `tree_struct_type_idx`
            // map but indexed by id without the kind switch.
            for (i, struct_decl) in comp.struct_types.iter().enumerate() {
                let bid = yel_core::ids::TreeBoundaryId(i as u32);
                let Some(&ty_idx) = gc_layout.tree_struct_type_idx.get(&bid) else {
                    continue;
                };
                type_names.append(ty_idx, &format!("{}-{}", comp_name, struct_decl.name));
            }
            // Determinism: HashMap — sort by the emitted type index.
            let mut sorted_tree_for: Vec<(_, u32)> = gc_layout
                .tree_for_arr_type_idx
                .iter()
                .map(|(&anchor_id, &arr_idx)| (anchor_id, arr_idx))
                .collect();
            sorted_tree_for.sort_by_key(|&(_, arr_idx)| arr_idx);
            for (anchor_id, arr_idx) in sorted_tree_for {
                let anchor_id = &anchor_id;
                // Stage 5d: read kind from registry.
                let Some(struct_decl) = comp.struct_types.get(anchor_id.0 as usize) else {
                    continue;
                };
                if let yel_core::lir::block::TreeBoundaryKind::ForAnchor { for_id, .. } =
                    &struct_decl.kind
                {
                    type_names.append(arr_idx, &format!("{}_for_{}_arr", comp_name, for_id.0));
                }
            }

            // Per-instance migration types — the component's own
            // GC struct, registry-handle struct, registry array, and
            // the internal/exported function types.
            if let Some(ty_idx) = gc_layout.component_struct_type_idx {
                type_names.append(ty_idx, &format!("{}-component", comp_name));
            }
            if let Some(ty_idx) = gc_layout.constructor_internal_type_idx {
                type_names.append(ty_idx, &format!("type-{}-constructor-internal", comp_name));
            }
            if let Some(ty_idx) = gc_layout.mount_internal_type_idx {
                type_names.append(ty_idx, &format!("type-{}-mount-internal", comp_name));
            }
            if let Some(ty_idx) = gc_layout.unmount_internal_type_idx {
                type_names.append(ty_idx, &format!("type-{}-unmount-internal", comp_name));
            }
            // Per-block dynamic function types — one per block whose
            // signature carries boundary-ref params (typed mount-tree
            // refs as additional WASM params). These are 1:1 with
            // their owning block, so name them after the block's
            // structured debug name (`type-<func_name>`) — gives WAT
            // dumps a direct visual link between the type decl and
            // the function that uses it.
            //
            // Note we deliberately don't dedupe these types across
            // structurally identical signatures: a fresh type per
            // block keeps later optimisation passes free to mutate
            // one block's signature without touching another's.
            // Determinism: HashMap — sort by the emitted type index.
            let mut sorted_block_dynamic: Vec<(_, u32)> = gc_layout
                .block_dynamic_type_idx
                .iter()
                .map(|(&block_id, &ty_idx)| (block_id, ty_idx))
                .collect();
            sorted_block_dynamic.sort_by_key(|&(_, ty_idx)| ty_idx);
            for (block_id, ty_idx) in sorted_block_dynamic {
                let block_id = &block_id;
                let info = self
                    .ctx
                    .get_block_name(comp.def_id, *block_id)
                    .unwrap_or_else(|| BlockDebugName::kind("block"));
                let block = comp.get_block(*block_id);
                let fn_name =
                    build_block_func_name(&comp_name, &info, block, &comp.slots, block_id.0);
                type_names.append(ty_idx, &format!("type-{}", fn_name));
            }
        }
        // Names accumulated for dynamically-emitted function types
        // (setters, accessors, ctors, list ctors, callback imports,
        // filter fns, ternary blocks, dispatch). Each `intern_type`
        // call in `build_core_module` pushes one entry here so the
        // type subsection covers every fresh function type with a
        // human-readable name in WAT.
        for (idx, name) in &self.function_type_names {
            type_names.append(*idx, name);
        }

        // (Global singletons have no GC struct type — their state lives
        // in core wasm globals, named in the global-name subsection.)

        // Subsection write deferred to end of fn — must be emitted in
        // ascending subsection-id order (1=function, 2=local, 3=label,
        // 4=type, 6=memory, 7=global, 9=data, 10=field). wasm-opt
        // refuses out-of-order subsections with `out-of-order name
        // subsection: <id>` warnings + a parse failure.

        // =================================================================
        // Global names - allocator globals, per-for tracking arrays,
        // and per-component handle-registry quadruples.
        // =================================================================
        let mut global_names = NameMap::new();
        global_names.append(0, "heap_base");
        global_names.append(1, "heap_ptr");
        global_names.append(2, "free_list");
        for (comp_idx, gc_layout) in self.gc_layouts.iter().enumerate() {
            let comp = &self.components[comp_idx];
            let comp_name = to_kebab_case(&self.ctx.str(comp.name));

            if let Some(g) = gc_layout.registry_global {
                global_names.append(g, &format!("{}-registry", comp_name));
            }
            if let Some(g) = gc_layout.registry_len_global {
                global_names.append(g, &format!("{}-registry-len", comp_name));
            }
            if let Some(g) = gc_layout.registry_free_head_global {
                global_names.append(g, &format!("{}-registry-free-head", comp_name));
            }
            if let Some(g) = gc_layout.current_handle_global {
                global_names.append(g, &format!("{}-current-handle", comp_name));
            }
        }
        // Per-field core wasm globals backing each global block's state.
        for layout in self.globals_layouts.iter() {
            let block_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(layout.block_def_id)))
                .replace('-', "_");
            for (field, &g) in layout.field_core_globals.iter().enumerate() {
                global_names.append(g, &format!("{}-global-{}", block_name, field));
            }
        }
        // Globals subsection: deferred to end-of-fn ordered emission.

        // =================================================================
        // Struct field names — surface signal names and the registry
        // handle struct's `inst` / `next` fields, plus iter-record
        // fields, in WAT dumps. wasm-encoder exposes this via
        // `NameSection::fields(IndirectNameMap)` keyed by type index.
        // =================================================================
        let mut field_names = IndirectNameMap::new();
        // Shared $handle: { inst, next } — one pair for the whole module.
        if let Some(handle_ty_idx) = self.shared_handle_type_idx {
            let mut handle_fields = NameMap::new();
            handle_fields.append(0, "inst");
            handle_fields.append(1, "next");
            field_names.append(handle_ty_idx, &handle_fields);
        }
        for (comp_idx, gc_layout) in self.gc_layouts.iter().enumerate() {
            let comp = &self.components[comp_idx];

            // $Comp_<name>: one or more fields per signal (1 for
            // primitives, 2 for FatPointer, N for Flat composites),
            // followed by mount-component retention fields.
            if let Some(struct_ty_idx) = gc_layout.component_struct_type_idx {
                let mut comp_fields = NameMap::new();
                for (sig_idx, signal) in comp.signals.iter().enumerate() {
                    let fp = comp.signal_layout.signal_field_path(sig_idx);
                    if fp.is_empty() {
                        continue;
                    }
                    let field_path = &fp;
                    let sig_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(signal.def_id)))
                        .replace('-', "_");
                    if field_path.len() == 1 {
                        comp_fields.append(field_path[0], &sig_name);
                    } else {
                        for (slot_i, &f) in field_path.iter().enumerate() {
                            comp_fields.append(f, &format!("{}_slot_{}", sig_name, slot_i));
                        }
                    }
                }
                // Trailing retention fields, if any. They follow the
                // signal fields contiguously per the layout in
                // `gc_types.rs::emit_component_struct_type`.
                let total_signal_fields: u32 = (0..comp.signals.len())
                    .map(|i| comp.signal_layout.signal_field_path(i).len() as u32)
                    .sum();
                for r in 0..gc_layout.parent_retention_count {
                    comp_fields.append(total_signal_fields + r, &format!("retain_{}", r));
                }
                if let Some(self_handle_idx) = gc_layout.self_handle_field_idx {
                    comp_fields.append(self_handle_idx, "self_handle");
                }
                if let Some(tree_field_idx) = gc_layout.tree_root_field_idx {
                    comp_fields.append(tree_field_idx, "tree");
                }
                field_names.append(struct_ty_idx, &comp_fields);
            }

            // Stage 5d: boundary struct field names from the
            // resource registry. struct_types[i].fields[j].name is
            // already projected from the synthesizer's
            // `TreeFieldDecl.name` — same labels, no kind switch.
            for (i, struct_decl) in comp.struct_types.iter().enumerate() {
                let bid = yel_core::ids::TreeBoundaryId(i as u32);
                let Some(&ty_idx) = gc_layout.tree_struct_type_idx.get(&bid) else {
                    continue;
                };
                let mut bnd_fields = NameMap::new();
                for (fi, field) in struct_decl.fields.iter().enumerate() {
                    bnd_fields.append(fi as u32, &field.name);
                }
                field_names.append(ty_idx, &bnd_fields);
            }
        }
        // (Global singletons have no GC struct, so no struct field names
        // — their state is core wasm globals, named above.)
        // Fields subsection: deferred to end-of-fn ordered emission.

        // =================================================================
        // Memory names
        // =================================================================
        let mut memory_names = NameMap::new();
        memory_names.append(0, "memory");
        // Memories subsection: deferred to end-of-fn ordered emission.

        // =================================================================
        // Function names
        // =================================================================
        let mut func_names = NameMap::new();

        // Name-section labels must follow the *actual* import emission
        // order: every component's callbacks (one per component, in
        // component order), then every global's callbacks (the built-in
        // `Dom` global's callbacks are named here too, as
        // `[global-callback]dom.*`), then one `[resource-new]` per
        // exported component.
        // Name each host import by its interface + function, in registry
        // order (which matches the import section). The interface name (from
        // the contract) labels whether it is a component callback, a global
        // callback, or DOM — no per-kind branching here.
        let mut import_idx = 0u32;
        for import in &self.imports {
            let iface = &self.import_interfaces[import.interface.index()];
            let iface_kebab = to_kebab_case(&self.ctx.str(iface.name));
            let fname = to_kebab_case(&self.ctx.str(import.name));
            func_names.append(import_idx, &format!("[import]{}.{}", iface_kebab, fname));
            import_idx += 1;
        }
        for exported_comp in exported_components.iter() {
            let prefix = to_kebab_case(&self.ctx.str(exported_comp.name));
            func_names.append(import_idx, &format!("[resource-new]{}", prefix));
            import_idx += 1;
        }

        // Local allocator function names
        func_names.append(import_layout.num_imports, "alloc");
        func_names.append(import_layout.num_imports + 1, "free");
        func_names.append(import_layout.num_imports + 2, "cabi_realloc");

        // Runtime function names (local functions, after allocator functions)
        if let Some(ref runtime_funcs) = self.runtime_funcs {
            // Name-section entries are skipped for absent helpers —
            // those weren't allocated an index by demand-driven gating.
            if let Some(idx) = runtime_funcs.s32_to_string {
                func_names.append(idx, "s32_to_string");
            }
            if let Some(idx) = runtime_funcs.s64_to_string {
                func_names.append(idx, "s64_to_string");
            }
            if let Some(idx) = runtime_funcs.bool_to_string {
                func_names.append(idx, "bool_to_string");
            }
            if let Some(idx) = runtime_funcs.f32_to_string {
                func_names.append(idx, "f32_to_string");
            }
            if let Some(idx) = runtime_funcs.store_fat_ptr {
                func_names.append(idx, "store_fat_ptr");
            }
            if let Some(idx) = runtime_funcs.load_fat_ptr {
                func_names.append(idx, "load_fat_ptr");
            }
            if let Some(idx) = runtime_funcs.pack_fat_ptr_to_i64 {
                func_names.append(idx, "pack_fat_ptr_to_i64");
            }
            if let Some(idx) = runtime_funcs.starts_with {
                func_names.append(idx, "starts_with");
            }
            // Determinism: these are HashMaps — sort each by func index so
            // the name-map entry order (and the emitted bytes) is stable.
            let mut sorted_concats: Vec<(usize, u32)> = runtime_funcs
                .concat_indices
                .iter()
                .map(|(&arity, &fi)| (arity, fi))
                .collect();
            sorted_concats.sort_by_key(|&(_, fi)| fi);
            for (arity, func_idx) in sorted_concats {
                func_names.append(func_idx, &format!("concat{}", arity));
            }
            let mut sorted_record_ctors: Vec<(_, u32)> = runtime_funcs
                .record_ctors
                .iter()
                .map(|(&def_id, &fi)| (def_id, fi))
                .collect();
            sorted_record_ctors.sort_by_key(|&(_, fi)| fi);
            for (def_id, func_idx) in sorted_record_ctors {
                let name = self.ctx.str(self.ctx.defs.name(def_id)).to_string();
                func_names.append(func_idx, &format!("record_ctor_{}", name));
            }
            let mut sorted_record_ctors_at: Vec<(_, u32)> = runtime_funcs
                .record_ctors_at
                .iter()
                .map(|(&def_id, &fi)| (def_id, fi))
                .collect();
            sorted_record_ctors_at.sort_by_key(|&(_, fi)| fi);
            for (def_id, func_idx) in sorted_record_ctors_at {
                let name = self.ctx.str(self.ctx.defs.name(def_id)).to_string();
                func_names.append(func_idx, &format!("record_ctor_at_{}", name));
            }
            let mut sorted_list_ctors: Vec<(usize, u32)> = runtime_funcs
                .list_ctors
                .iter()
                .map(|(&(_elem_ty, count), &fi)| (count, fi))
                .collect();
            sorted_list_ctors.sort_by_key(|&(_, fi)| fi);
            for (count, func_idx) in sorted_list_ctors {
                func_names.append(func_idx, &format!("list_ctor_{}", count));
            }
            // Sort list_appends by func index so NameMap.append sees
            // monotonically-increasing keys (NameMap requires it).
            let mut sorted_appends: Vec<(Ty, u32)> = runtime_funcs
                .list_appends
                .iter()
                .map(|(&ty, &fi)| (ty, fi))
                .collect();
            sorted_appends.sort_by_key(|(_, fi)| *fi);
            for (list_ty, func_idx) in sorted_appends {
                func_names.append(func_idx, &format!("list_append_{}", list_ty.0));
            }
            // Same monotonic-key requirement for list_gets.
            let mut sorted_gets: Vec<(Ty, u32)> = runtime_funcs
                .list_gets
                .iter()
                .map(|(&ty, &fi)| (ty, fi))
                .collect();
            sorted_gets.sort_by_key(|(_, fi)| *fi);
            for (list_ty, func_idx) in sorted_gets {
                func_names.append(func_idx, &format!("list_get_{}", list_ty.0));
            }
            let mut sorted_filters: Vec<(usize, u32)> = runtime_funcs
                .filter_indices
                .iter()
                .map(|(&call_id, &fi)| (call_id, fi))
                .collect();
            sorted_filters.sort_by_key(|&(_, fi)| fi);
            for (call_id, func_idx) in sorted_filters {
                func_names.append(func_idx, &format!("filter_{}", call_id));
            }
        }

        // Component function names - start after allocator + runtime
        // functions + per-array materializers/un-materializers + the
        // optional `pack_color_to_attr_slots` helper.
        let gc_list_helper_count = self
            .record_gc_types
            .list_array_type_idx
            .iter()
            .filter(|&(&ty, _)| self.is_scalar_list_ty(ty))
            .count() as u32
            * 2;
        let pack_color_count = if self.pack_color_helper_fn_idx.is_some() {
            1
        } else {
            0
        };
        let first_component_func = if let Some(ref runtime_funcs) = self.runtime_funcs {
            import_layout.num_imports
                + 3
                + runtime_funcs.count
                + gc_list_helper_count
                + pack_color_count
        } else {
            import_layout.num_imports + 3
        };

        let mut func_idx = first_component_func;
        for component in self.components {
            let prefix = to_kebab_case(&self.ctx.str(component.name));
            func_names.append(func_idx, &format!("[constructor]{}", prefix));
            func_names.append(func_idx + 1, &format!("[method]{}.mount", prefix));
            func_names.append(func_idx + 2, &format!("[method]{}.unmount", prefix));

            for (sig_idx, signal) in component.signals.iter().enumerate() {
                let getter_idx = func_idx + 3 + (sig_idx as u32 * 2);
                let setter_idx = getter_idx + 1;
                let sig_name = self.signal_name(signal.def_id);
                func_names.append(
                    getter_idx,
                    &format!("[method]{}.get-{}", prefix, to_wit_name(&sig_name)),
                );
                func_names.append(
                    setter_idx,
                    &format!("[method]{}.set-{}", prefix, to_wit_name(&sig_name)),
                );
            }
            // Internal-tier function names. The `[<role>]<comp>`
            // bracket style is reserved for the canonical-ABI
            // component-model exports (`[constructor]`, `[method]`,
            // `[resource-new]`) so external tools recognise them.
            // Internal-tier helpers are pure WASM module functions —
            // name them in the same `<comp>-<kind>` style as block
            // funcs so WAT dumps read consistently.
            let internal_base = func_idx + 3 + (component.signals.len() as u32 * 2);
            func_names.append(internal_base, &format!("{}-constructor-internal", prefix));
            func_names.append(internal_base + 1, &format!("{}-mount-internal", prefix));
            func_names.append(internal_base + 2, &format!("{}-unmount-internal", prefix));

            func_idx += 6 + (component.signals.len() as u32 * 2);
        }

        // Standalone dispatch function name
        if let Some(dispatch_idx) = self.dispatch_func_idx {
            func_names.append(dispatch_idx, "dispatch");
            // Module start function is emitted immediately after
            // dispatch (see the type-section pass). Naming it makes
            // `(start $globals_init)` readable.
            func_names.append(dispatch_idx + 1, "globals_init");
        }

        // Block function names — each block carries a debug label
        // registered during lowering (via `finish_block_named`). Emit
        // them into the name section so WAT dumps show
        // `$for-item-mount-row` instead of `(func (;36;))`. Prefix
        // with the owning component so the same block kind in two
        // components doesn't collide.
        // Determinism: HashMap — sort by the wasm function index.
        let mut sorted_block_funcs: Vec<(_, u32)> = self
            .block_func_indices
            .iter()
            .map(|(&block_id, &fi)| (block_id, fi))
            .collect();
        sorted_block_funcs.sort_by_key(|&(_, fi)| fi);
        for (block_id, wasm_func_idx) in sorted_block_funcs {
            let block_id = &block_id;
            // Phase 0.3q: locate the owning component (BlockIds are
            // module-wide unique so a linear scan suffices).
            let Some(component) = self
                .components
                .iter()
                .find(|c| c.blocks.iter().any(|b| b.id == *block_id))
            else {
                continue;
            };
            let comp_prefix = to_kebab_case(&self.ctx.str(component.name));
            let info = self
                .ctx
                .get_block_name(component.def_id, *block_id)
                .unwrap_or_else(|| BlockDebugName::kind("block"));

            let block = component.get_block(*block_id);
            let name =
                build_block_func_name(&comp_prefix, &info, block, &component.slots, block_id.0);
            func_names.append(wasm_func_idx, &name);
        }

        // Functions subsection: deferred to end-of-fn ordered emission.

        // =================================================================
        // Local variable names for each function
        // =================================================================
        let mut local_names = IndirectNameMap::new();

        // Runtime function locals
        // Note: memcpy, alloc, free, cabi_realloc are now imports - their locals are in allocator module
        if let Some(ref runtime_funcs) = self.runtime_funcs {
            if let Some(idx) = runtime_funcs.s32_to_string {
                let mut s32_to_string_locals = NameMap::new();
                s32_to_string_locals.append(0, "value");
                s32_to_string_locals.append(1, "is_negative");
                s32_to_string_locals.append(2, "abs_value");
                s32_to_string_locals.append(3, "digit_count");
                s32_to_string_locals.append(4, "write_ptr");
                local_names.append(idx, &s32_to_string_locals);
            }
            if let Some(idx) = runtime_funcs.bool_to_string {
                let mut bool_to_string_locals = NameMap::new();
                bool_to_string_locals.append(0, "b");
                local_names.append(idx, &bool_to_string_locals);
            }
        }

        // Generate locals for each component's functions
        let mut func_idx = first_component_func;
        for (comp_idx, component) in self.components.iter().enumerate() {
            let _layout = &layouts[comp_idx];
            let _prefix = to_kebab_case(&self.ctx.str(component.name));

            // Constructor - no locals
            // (func_idx is constructor)
            func_idx += 1;

            // Mount - has (self, root) params + slot locals
            let mut mount_locals = NameMap::new();
            mount_locals.append(0, "self");
            mount_locals.append(1, "root");
            // Mount declares one local per Temp slot in compacted
            // `local_idx` order. Memory slots don't take locals and
            // don't appear here — no placeholder gaps to label.
            let mut temp_slots_named: Vec<&yel_core::lir::LirSlotInfo> = component
                .slots
                .iter()
                .filter(|s| matches!(s.kind, LirSlotKind::Temp { .. }))
                .collect();
            temp_slots_named.sort_by_key(|s| match s.kind {
                LirSlotKind::Temp { local_idx } => local_idx,
                _ => unreachable!(),
            });
            let base_i32_locals = temp_slots_named.len() as u32;
            for s in &temp_slots_named {
                let local_idx = match s.kind {
                    LirSlotKind::Temp { local_idx } => local_idx,
                    _ => unreachable!(),
                };
                let name = s.name.clone().unwrap_or_else(|| format!("slot_{}", s.id));
                mount_locals.append(local_idx + 2, &name);
            }
            // Scratch locals (flat-slot signal-store helpers) come
            // after the slot locals, grouped i32 → i64 → f32 → f64.
            let (mount_i32, mount_i64, mount_f32, mount_f64) = component
                .get_block(component.mount_block)
                .max_flat_scratch_counts;
            let scratch_base = 2 + base_i32_locals;
            for k in 0..mount_i32 {
                mount_locals.append(scratch_base + k, &format!("scratch_i32_{}", k));
            }
            for k in 0..mount_i64 {
                mount_locals.append(scratch_base + mount_i32 + k, &format!("scratch_i64_{}", k));
            }
            for k in 0..mount_f32 {
                mount_locals.append(
                    scratch_base + mount_i32 + mount_i64 + k,
                    &format!("scratch_f32_{}", k),
                );
            }
            for k in 0..mount_f64 {
                mount_locals.append(
                    scratch_base + mount_i32 + mount_i64 + mount_f32 + k,
                    &format!("scratch_f64_{}", k),
                );
            }
            local_names.append(func_idx, &mount_locals);
            func_idx += 1;

            // Unmount - has (self) param
            let mut unmount_locals = NameMap::new();
            unmount_locals.append(0, "self");
            local_names.append(func_idx, &unmount_locals);
            func_idx += 1;

            // Getters and setters for each signal
            for signal in component.signals.iter() {
                let sig_name = self.signal_name(signal.def_id);

                // Getter - has (self) param
                let mut getter_locals = NameMap::new();
                getter_locals.append(0, "self");
                local_names.append(func_idx, &getter_locals);
                func_idx += 1;

                // Setter - has (self, value) params
                let mut setter_locals = NameMap::new();
                setter_locals.append(0, "self");
                setter_locals.append(1, &format!("{}_value", sig_name));
                local_names.append(func_idx, &setter_locals);
                func_idx += 1;
            }
        }

        // Block function locals — param names + slot names. Slots
        // tagged with `SlotInfo.name` are surfaced so WAT dumps read
        // `local.get $iter_record_ptr` rather than `local.get 97`.
        //
        // Local indices in the emitted function: params come first
        // (0..param_count), then one local per slot. The slot's
        // local index inside the function is `param_count + slot.id`
        // — see `generate_block_function` for the canonical layout.
        // Determinism: HashMap — sort by the wasm function index.
        let mut sorted_block_funcs: Vec<(_, u32)> = self
            .block_func_indices
            .iter()
            .map(|(&block_id, &fi)| (block_id, fi))
            .collect();
        sorted_block_funcs.sort_by_key(|&(_, fi)| fi);
        for (block_id, wasm_func_idx) in sorted_block_funcs {
            let block_id = &block_id;
            let mut block_locals = NameMap::new();
            // Phase 0.3q: locate the owning component.
            let Some(component) = self
                .components
                .iter()
                .find(|c| c.blocks.iter().any(|b| b.id == *block_id))
            else {
                continue;
            };
            let block = component.get_block(*block_id);
            // Step 4: every block carries an implicit `(ref null
            // $Comp)` self ref at WASM param 0; LIR-declared params
            // start at WASM param 1.
            // Stage 5c: read counts from `boundary_param_slots`.
            let boundary_param_count: u32 = block.boundary_param_slots.len() as u32;
            let lir_param_count: u32 = if !block.params.is_empty() {
                block.params.len() as u32
            } else if boundary_param_count == 0 {
                1
            } else {
                0
            };
            let param_count: u32 = lir_param_count + boundary_param_count + 1;
            block_locals.append(0, "self");
            if block.params.is_empty() {
                block_locals.append(1, "parent");
            } else {
                for (i, slot) in block.params.iter().enumerate() {
                    // Task #105 B2: Block-variant param slots live on the
                    // block's own slots vec; Resource-variant on the
                    // component's.
                    let info = match slot {
                        yel_core::lir::LirSlotId::Block { idx, .. } => {
                            block.slots.get(*idx as usize)
                        }
                        yel_core::lir::LirSlotId::Resource { idx } => {
                            component.slots.get(*idx as usize)
                        }
                    };
                    let name = info
                        .and_then(|s| s.name.clone())
                        .unwrap_or_else(|| format!("param{}", i));
                    block_locals.append((i as u32) + 1, &name);
                }
            }
            for slot_info in &component.slots {
                if let LirSlotKind::Temp { local_idx } = slot_info.kind
                    && let Some(name) = &slot_info.name
                {
                    block_locals.append(param_count + local_idx, name);
                }
            }
            local_names.append(wasm_func_idx, &block_locals);
        }

        // Locals subsection: deferred to end-of-fn ordered emission.

        // =================================================================
        // Label names — debug labels for `block` / `loop` / `if`
        // structural ops emitted by block/mount functions. Ordered by
        // WASM function index; each function gets a NameMap keyed by
        // the structural op's preorder label index (see `emit_op`).
        // Omitted when a function emitted no labelled structural ops
        // — the name section is a debug hint, so no entry is valid.
        // =================================================================
        let mut label_names = IndirectNameMap::new();
        let mut sorted_label_fns: Vec<(&u32, &Vec<(u32, String)>)> =
            self.function_label_names.iter().collect();
        sorted_label_fns.sort_by_key(|(k, _)| **k);
        for (wasm_func_idx, entries) in sorted_label_fns {
            if entries.is_empty() {
                continue;
            }
            let mut map = NameMap::new();
            // Entries are already recorded in ascending label-idx order
            // (preorder counter), but sort defensively so append() sees
            // monotonically-increasing keys (required by NameMap).
            let mut sorted: Vec<&(u32, String)> = entries.iter().collect();
            sorted.sort_by_key(|(idx, _)| *idx);
            for (idx, name) in sorted {
                map.append(*idx, name);
            }
            label_names.append(*wasm_func_idx, &map);
        }
        // Labels subsection: deferred to end-of-fn ordered emission.

        // =================================================================
        // Data segment names
        // =================================================================
        let data_names: Option<NameMap> = if self.strings.size() > 0 {
            let mut m = NameMap::new();
            m.append(0, "string_data");
            Some(m)
        } else {
            None
        };

        // Final ordered emission. The wasm name-section spec requires
        // subsection IDs to appear in strictly ascending order
        // (binary spec: each subsection has a `u8` id, ids must be
        // monotonic). Engine validators (wasmparser, Binaryen) enforce
        // this; out-of-order subsections fail to parse.
        //   id 0 = module       — already written at top of fn
        //   id 1 = function
        //   id 2 = local
        //   id 3 = label
        //   id 4 = type
        //   id 5 = table        — unused
        //   id 6 = memory
        //   id 7 = global
        //   id 8 = element      — unused
        //   id 9 = data
        //   id 10 = field
        //   id 11 = tag         — unused
        names.functions(&func_names);
        names.locals(&local_names);
        names.labels(&label_names);
        names.types(&type_names);
        names.memories(&memory_names);
        names.globals(&global_names);
        if let Some(d) = &data_names {
            names.data(d);
        }
        names.fields(&field_names);

        module.section(&names);
    }
}
