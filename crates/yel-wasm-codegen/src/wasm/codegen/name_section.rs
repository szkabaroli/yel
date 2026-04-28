//! Name-section emission: function names, type names, local-variable
//! names, label names, and data-segment names. All of these are debug
//! hints — the WASM module remains valid even if the section is omitted —
//! but `wasm-tools print` and similar tooling rely on them for readable
//! output, so we go to some lengths to keep them in sync with the actual
//! emission order.

use wasm_encoder::{IndirectNameMap, Module, NameMap, NameSection};
use yel_core::BlockDebugName;
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
    block_id_raw: u32,
) -> String {
    let mut s = format!("{}-{}", comp_prefix, name.kind);
    for bp in &block.boundary_params {
        s.push_str(&format!("-b{}", bp.0));
    }
    if let Some(sig) = name.signal {
        s.push_str(&format!("-s{}", sig));
    }
    s.push_str(&format!("#{}", block_id_raw));
    s
}

use super::super::{
    ImportLayout, MemoryLayout, NUM_DOM_IMPORTS, WasmPackageBuilder, to_kebab_case, to_wit_name,
};

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
        type_names.append(0, "type_void_void"); // () -> ()
        type_names.append(1, "type_i32_void"); // (i32) -> ()
        type_names.append(2, "type_void_i32"); // () -> i32
        type_names.append(3, "type_i32_i32_void"); // (i32, i32) -> ()
        type_names.append(4, "type_i32_i32"); // (i32) -> i32
        type_names.append(5, "type_i32_i32_i32"); // (i32, i32) -> i32
        type_names.append(6, "type_i32_i32_i32_void"); // (i32, i32, i32) -> ()
        type_names.append(7, "type_i32x5_void"); // (i32, i32, i32, i32, i32) -> ()
        type_names.append(8, "type_i32x4_void"); // (i32, i32, i32, i32) -> ()
        type_names.append(9, "type_i32x4_i32"); // (i32, i32, i32, i32) -> i32 (realloc)
        type_names.append(10, "type_i32_f32"); // (i32) -> f32 (getter f32)
        type_names.append(11, "type_i32_f32_void"); // (i32, f32) -> () (setter f32)
        type_names.append(12, "type_i32_f64"); // (i32) -> f64 (getter f64)
        type_names.append(13, "type_i32_f64_void"); // (i32, f64) -> () (setter f64)
        type_names.append(14, "type_i32_i64"); // (i32) -> i64 (getter i64)
        type_names.append(15, "type_i32_i64_void"); // (i32, i64) -> ()
        // Runtime / ad-hoc types declared at module construction time
        // (build.rs:90-180). Each is the function type of exactly one
        // runtime fn — naming them here means WAT shows the role at a
        // glance instead of `(type 17)`.
        type_names.append(16, "type-i32-to-ptr-len"); // s32_to_string, bool_to_string, load_fat_ptr
        type_names.append(17, "type-runtime-concat2");
        type_names.append(18, "type-runtime-concat3");
        type_names.append(19, "type-runtime-concat4");
        type_names.append(20, "type-runtime-concat5");
        type_names.append(21, "type-runtime-concat6");
        type_names.append(22, "type-runtime-concat7");
        type_names.append(23, "type-runtime-concat8");
        type_names.append(24, "type-record-ctor-3"); // 3-field record ctor
        type_names.append(25, "type-record-ctor-5"); // 5-field record ctor
        type_names.append(26, "type-runtime-void-i32-i32"); // list_get_opt's if-block result
        type_names.append(27, "type-f32-to-ptr-len"); // f32_to_string
        type_names.append(28, "type-list-ctor-3");
        type_names.append(29, "type-list-ctor-2");
        type_names.append(30, "type-list-ctor-5");
        type_names.append(31, "type-set-attribute-variant"); // DOM set-attribute import
        type_names.append(32, "type-promote-ptr-for-variant");
        type_names.append(33, "type-i64-to-ptr-len"); // s64_to_string (setter i64)

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
            for boundary in &comp.tree_shape.boundaries {
                let Some(&ty_idx) = gc_layout.tree_struct_type_idx.get(&boundary.id) else {
                    continue;
                };
                let suffix = match &boundary.kind {
                    yel_core::lir::block::TreeBoundaryKind::Root => "tree_root".to_string(),
                    yel_core::lir::block::TreeBoundaryKind::IfAnchor { if_id, .. } => {
                        format!("if_{}", if_id.0)
                    }
                    yel_core::lir::block::TreeBoundaryKind::IfBranch { if_id, branch_idx } => {
                        match branch_idx {
                            0 => format!("if_{}_then", if_id.0),
                            n => format!("if_{}_branch_{}", if_id.0, n),
                        }
                    }
                    yel_core::lir::block::TreeBoundaryKind::ForAnchor { for_id, .. } => {
                        format!("for_{}", for_id.0)
                    }
                    yel_core::lir::block::TreeBoundaryKind::ForIterBody { for_id } => {
                        format!("for_{}_iter", for_id.0)
                    }
                };
                type_names.append(ty_idx, &format!("{}-{}", comp_name, suffix));
            }
            for (anchor_id, &arr_idx) in &gc_layout.tree_for_arr_type_idx {
                let Some(boundary) = comp
                    .tree_shape
                    .boundaries
                    .iter()
                    .find(|b| b.id == *anchor_id)
                else {
                    continue;
                };
                if let yel_core::lir::block::TreeBoundaryKind::ForAnchor { for_id, .. } =
                    &boundary.kind
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
            for (block_id, &ty_idx) in &gc_layout.block_dynamic_type_idx {
                let info = self
                    .ctx
                    .get_block_name(comp.def_id, *block_id)
                    .unwrap_or_else(|| BlockDebugName::kind("block"));
                let block = comp.get_block(*block_id);
                let fn_name = build_block_func_name(&comp_name, &info, block, block_id.0);
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

        // Per-named-`global`-block GC struct types. Indices come after
        // every per-component type so monotonic ascending order is
        // preserved by walking `globals_layouts` in declaration order.
        for layout in self.globals_layouts.iter() {
            let block_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(layout.block_def_id)))
                .replace('-', "_");
            type_names.append(layout.struct_type_idx, &format!("{}-global", block_name));
        }
        names.types(&type_names);

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
        // Per-named-`global`-block self-globals. Indices come after
        // the per-component handle-registry quadruple, in
        // `globals_layouts` order — monotonic ascending preserved.
        for layout in self.globals_layouts.iter() {
            let block_name = to_kebab_case(&self.ctx.str(self.ctx.defs.name(layout.block_def_id)))
                .replace('-', "_");
            global_names.append(
                layout.self_global_idx,
                &format!("{}-global-self", block_name),
            );
        }
        names.globals(&global_names);

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
                    let Some(field_path) = gc_layout.signal_field_paths.get(sig_idx) else {
                        continue;
                    };
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
                let total_signal_fields: u32 = gc_layout
                    .signal_field_paths
                    .iter()
                    .map(|p| p.len() as u32)
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

            // Phase B.3 boundary struct field names. Mirror the
            // synthesizer's TreeFieldDecl `name` so WAT is readable.
            for boundary in &comp.tree_shape.boundaries {
                let Some(&ty_idx) = gc_layout.tree_struct_type_idx.get(&boundary.id) else {
                    continue;
                };
                let mut bnd_fields = NameMap::new();
                for (i, decl) in boundary.fields.iter().enumerate() {
                    let name = match decl {
                        yel_core::lir::block::TreeFieldDecl::DomHandle { name } => name.clone(),
                        yel_core::lir::block::TreeFieldDecl::LoopVar { name, .. } => name.clone(),
                        yel_core::lir::block::TreeFieldDecl::SubBoundary { name, .. } => {
                            name.clone()
                        }
                        yel_core::lir::block::TreeFieldDecl::ChildrenArray { name, .. } => {
                            name.clone()
                        }
                        yel_core::lir::block::TreeFieldDecl::ActiveTag { name } => name.clone(),
                    };
                    bnd_fields.append(i as u32, &name);
                }
                field_names.append(ty_idx, &bnd_fields);
            }
        }
        // Per-named-`global`-block struct field names. One field name
        // per migrated property's ABI slot; multi-slot properties get
        // `<prop>_slot_0`, `<prop>_slot_1` suffixes (mirroring component
        // signal field naming). Pointer-typed properties have empty
        // field paths and contribute nothing.
        for layout in self.globals_layouts.iter() {
            let block = match self.ctx.defs.as_global(layout.block_def_id) {
                Some(b) => b,
                None => continue,
            };
            let mut block_fields = NameMap::new();
            let mut any = false;
            for (prop_pos, &prop_def_id) in block.properties.iter().enumerate() {
                let Some(field_path) = layout.property_field_paths.get(prop_pos) else {
                    continue;
                };
                if field_path.is_empty() {
                    continue;
                }
                let prop_name =
                    to_kebab_case(&self.ctx.str(self.ctx.defs.name(prop_def_id))).replace('-', "_");
                if field_path.len() == 1 {
                    block_fields.append(field_path[0], &prop_name);
                } else {
                    for (slot_i, &f) in field_path.iter().enumerate() {
                        block_fields.append(f, &format!("{}_slot_{}", prop_name, slot_i));
                    }
                }
                any = true;
            }
            if any {
                field_names.append(layout.struct_type_idx, &block_fields);
            }
        }
        names.fields(&field_names);

        // =================================================================
        // Memory names
        // =================================================================
        let mut memory_names = NameMap::new();
        memory_names.append(0, "memory");
        names.memories(&memory_names);

        // =================================================================
        // Function names
        // =================================================================
        let mut func_names = NameMap::new();
        let dom_func_names = [
            "create-element",
            "create-text",
            "create-comment",
            "set-attribute",
            "remove-attribute",
            "set-text-content",
            "set-style",
            "set-class",
            "append-child",
            "insert-before",
            "remove-child",
            "remove",
            "get-parent",
            "get-next-sibling",
            "add-event-listener",
            "remove-event-listener",
            "insert-after",
            "create-fragment",
        ];
        for (i, name) in dom_func_names.iter().enumerate() {
            func_names.append(i as u32, name);
        }

        // Name-section labels must follow the *actual* import emission
        // order: first every component's callbacks (one per component, in
        // component order), then one `[resource-new]` per exported
        // component.
        let mut import_idx = NUM_DOM_IMPORTS;
        for &(comp_idx, cb_def_id) in &import_layout.unique_callbacks {
            if let Some(func_def) = self.ctx.defs.as_function(cb_def_id) {
                let name = to_kebab_case(&self.ctx.str(func_def.name));
                let owner_comp = &self.components[comp_idx];
                let comp_kebab = to_kebab_case(&self.ctx.str(owner_comp.name));
                func_names.append(import_idx, &format!("[callback]{}.{}", comp_kebab, name));
                import_idx += 1;
            }
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
            func_names.append(runtime_funcs.s32_to_string, "s32_to_string");
            func_names.append(runtime_funcs.s64_to_string, "s64_to_string");
            func_names.append(runtime_funcs.bool_to_string, "bool_to_string");
            func_names.append(runtime_funcs.f32_to_string, "f32_to_string");
            func_names.append(runtime_funcs.store_fat_ptr, "store_fat_ptr");
            func_names.append(runtime_funcs.load_fat_ptr, "load_fat_ptr");
            func_names.append(runtime_funcs.pack_fat_ptr_to_i64, "pack_fat_ptr_to_i64");
            func_names.append(runtime_funcs.store_option, "store_option");
            func_names.append(runtime_funcs.store_result, "store_result");
            func_names.append(runtime_funcs.list_get, "list_get");
            func_names.append(runtime_funcs.list_get_opt, "list_get_opt");
            func_names.append(runtime_funcs.list_get_fat, "list_get_fat");
            func_names.append(runtime_funcs.starts_with, "starts_with");
            for (&arity, &func_idx) in &runtime_funcs.concat_indices {
                func_names.append(func_idx, &format!("concat{}", arity));
            }
            for (&def_id, &func_idx) in &runtime_funcs.record_ctors {
                let name = self.ctx.str(self.ctx.defs.name(def_id)).to_string();
                func_names.append(func_idx, &format!("record_ctor_{}", name));
            }
            for (&def_id, &func_idx) in &runtime_funcs.record_ctors_at {
                let name = self.ctx.str(self.ctx.defs.name(def_id)).to_string();
                func_names.append(func_idx, &format!("record_ctor_at_{}", name));
            }
            for (&(_elem_ty, count), &func_idx) in &runtime_funcs.list_ctors {
                func_names.append(func_idx, &format!("list_ctor_{}", count));
            }
            for (&call_id, &func_idx) in &runtime_funcs.filter_indices {
                func_names.append(func_idx, &format!("filter_{}", call_id));
            }
        }

        // Component function names - start after allocator + runtime functions
        let first_component_func = if let Some(ref runtime_funcs) = self.runtime_funcs {
            import_layout.num_imports + 3 + runtime_funcs.count
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
        for ((comp_idx, block_id), &wasm_func_idx) in &self.block_func_indices {
            let component = &self.components[*comp_idx];
            let comp_prefix = to_kebab_case(&self.ctx.str(component.name));
            let info = self
                .ctx
                .get_block_name(component.def_id, *block_id)
                .unwrap_or_else(|| BlockDebugName::kind("block"));

            let block = component.get_block(*block_id);
            let name = build_block_func_name(&comp_prefix, &info, block, block_id.0);
            func_names.append(wasm_func_idx, &name);
        }

        names.functions(&func_names);

        // =================================================================
        // Local variable names for each function
        // =================================================================
        let mut local_names = IndirectNameMap::new();

        // Runtime function locals
        // Note: memcpy, alloc, free, cabi_realloc are now imports - their locals are in allocator module
        if let Some(ref runtime_funcs) = self.runtime_funcs {
            // s32_to_string locals
            let mut s32_to_string_locals = NameMap::new();
            s32_to_string_locals.append(0, "value");
            s32_to_string_locals.append(1, "is_negative");
            s32_to_string_locals.append(2, "abs_value");
            s32_to_string_locals.append(3, "digit_count");
            s32_to_string_locals.append(4, "write_ptr");
            local_names.append(runtime_funcs.s32_to_string, &s32_to_string_locals);

            // bool_to_string locals
            let mut bool_to_string_locals = NameMap::new();
            bool_to_string_locals.append(0, "b");
            local_names.append(runtime_funcs.bool_to_string, &bool_to_string_locals);
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
                let name = s.name.clone().unwrap_or_else(|| format!("slot_{}", s.id.0));
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
        for ((comp_idx, block_id), &wasm_func_idx) in &self.block_func_indices {
            let mut block_locals = NameMap::new();
            let component = &self.components[*comp_idx];
            let block = component.get_block(*block_id);
            // Step 4: every block carries an implicit `(ref null
            // $Comp)` self ref at WASM param 0; LIR-declared params
            // start at WASM param 1.
            let lir_param_count: u32 = if !block.params.is_empty() {
                block.params.len() as u32
            } else if block.boundary_params.is_empty() {
                1
            } else {
                0
            };
            let boundary_param_count: u32 = block.boundary_params.len() as u32;
            let param_count: u32 = lir_param_count + boundary_param_count + 1;
            block_locals.append(0, "self");
            if block.params.is_empty() {
                block_locals.append(1, "parent");
            } else {
                for (i, slot) in block.params.iter().enumerate() {
                    let name = component
                        .slots
                        .get(slot.0 as usize)
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

        names.locals(&local_names);

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
        names.labels(&label_names);

        // =================================================================
        // Data segment names
        // =================================================================
        if self.strings.size() > 0 {
            let mut data_names = NameMap::new();
            data_names.append(0, "string_data");
            names.data(&data_names);
        }

        module.section(&names);
    }
}
