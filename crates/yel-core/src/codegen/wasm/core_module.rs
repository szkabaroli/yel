//! Core WASM module generation from LIR.
//!
//! This module handles generation of the inner core WASM module including:
//! - Type definitions
//! - Import/export sections
//! - Runtime functions (concat<n>, s32_to_string, bool_to_string)
//! - Function codegen (constructor, mount, unmount, dispatch, getters, setters) for ALL components
//! - Data section for string literals
//! - Name section for debugging

use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
    ImportSection, IndirectNameMap, Instruction, MemoryType, Module, NameMap, NameSection,
    TypeSection, ValType,
};

use crate::codegen::CodegenError;
use crate::codegen::wasm::AllocatorFuncs;
use crate::lir::{LirComponent, LirExpr, SlotValType};
use crate::types::InternedTyKind;

use super::{
    to_kebab_case, to_wit_name, ImportLayout, MemoryLayout, WasmPackageBuilder,
    IMPORT_ADD_EVENT_LISTENER, IMPORT_APPEND_CHILD, IMPORT_CREATE_COMMENT, IMPORT_CREATE_ELEMENT, IMPORT_CREATE_TEXT,
    IMPORT_INSERT_AFTER, IMPORT_REMOVE, IMPORT_SET_ATTRIBUTE, IMPORT_SET_TEXT_CONTENT, NUM_DOM_IMPORTS,
};
use super::runtime::{self, RuntimeFunctions};

/// Helper to create a MemArg for load/store instructions.
pub(super) fn mem_arg(offset: u64, align: u32) -> wasm_encoder::MemArg {
    wasm_encoder::MemArg {
        offset,
        align,
        memory_index: 0,
    }
}

impl<'a> WasmPackageBuilder<'a> {
    pub(crate) fn build_core_module(&mut self) -> Result<Module, CodegenError> {
        let mut module = Module::new();

        // Collect exported component indices to avoid holding borrow during mutable operations
        let exported_indices: Vec<usize> = self.components
            .iter()
            .enumerate()
            .filter(|(_, c)| c.is_export)
            .map(|(i, _)| i)
            .collect();

        // Get exported components for import layout calculation
        let exported_components: Vec<&LirComponent> = exported_indices
            .iter()
            .map(|&i| &self.components[i])
            .collect();

        // Type section
        let mut types = TypeSection::new();
        types.ty().function([], []);                                           // 0: () -> ()
        types.ty().function([ValType::I32], []);                               // 1: (i32) -> ()
        types.ty().function([], [ValType::I32]);                               // 2: () -> i32
        types.ty().function([ValType::I32, ValType::I32], []);                 // 3: (i32, i32) -> () - setter i32
        types.ty().function([ValType::I32], [ValType::I32]);                   // 4: (i32) -> i32 - getter i32
        types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);     // 5: (i32, i32) -> i32
        types.ty().function([ValType::I32, ValType::I32, ValType::I32], []);   // 6: (i32, i32, i32) -> ()
        types.ty().function([ValType::I32, ValType::I32, ValType::I32, ValType::I32, ValType::I32], []); // 7
        types.ty().function([ValType::I32, ValType::I32, ValType::I32, ValType::I32], []);   // 8
        types.ty().function([ValType::I32, ValType::I32, ValType::I32, ValType::I32], [ValType::I32]); // 9: realloc
        // Additional types for f32, f64, i64 getters/setters
        types.ty().function([ValType::I32], [ValType::F32]);                   // 10: getter f32
        types.ty().function([ValType::I32, ValType::F32], []);                 // 11: setter f32
        types.ty().function([ValType::I32], [ValType::F64]);                   // 12: getter f64
        types.ty().function([ValType::I32, ValType::F64], []);                 // 13: setter f64
        types.ty().function([ValType::I32], [ValType::I64]);                   // 14: getter i64
        types.ty().function([ValType::I32, ValType::I64], []);                 // 15: setter i64
        // Runtime function types
        types.ty().function([ValType::I32], [ValType::I32, ValType::I32]);     // 16: (i32) -> (i32, i32) - s32_to_string, bool_to_string
        // concat<n> types: concat2..concat8 (dynamic based on arity)
        // Type 17: concat2 - (i32, i32, i32, i32) -> (i32, i32)
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32, ValType::I32]
        );
        // Type 18: concat3 - (i32 x 6) -> (i32, i32)
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32, ValType::I32]
        );
        // Type 19: concat4 - (i32 x 8) -> (i32, i32)
        types.ty().function(
            [ValType::I32; 8],
            [ValType::I32, ValType::I32]
        );
        // Type 20: concat5 - (i32 x 10) -> (i32, i32)
        types.ty().function(
            [ValType::I32; 10],
            [ValType::I32, ValType::I32]
        );
        // Type 21: concat6 - (i32 x 12) -> (i32, i32)
        types.ty().function(
            [ValType::I32; 12],
            [ValType::I32, ValType::I32]
        );
        // Type 22: concat7 - (i32 x 14) -> (i32, i32)
        types.ty().function(
            [ValType::I32; 14],
            [ValType::I32, ValType::I32]
        );
        // Type 23: concat8 - (i32 x 16) -> (i32, i32)
        types.ty().function(
            [ValType::I32; 16],
            [ValType::I32, ValType::I32]
        );
        // Type 24: (i32, i32, i32) -> i32 - for record ctor with 3 params
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32]
        );
        // Type 25: (i32 x 5) -> i32 - for record ctor with 5 params
        types.ty().function(
            [ValType::I32; 5],
            [ValType::I32]
        );
        // Type 26: () -> (i32, i32) - for if block results in list_get_opt
        types.ty().function(
            [],
            [ValType::I32, ValType::I32]
        );
        // Type 27: (f32) -> (i32, i32) - for f32_to_string
        types.ty().function(
            [ValType::F32],
            [ValType::I32, ValType::I32]
        );
        // Type 28: (i32, i32, i32) -> (i32, i32) - for list ctor with 3 params
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32, ValType::I32]
        );
        // Type 29: (i32, i32) -> (i32, i32) - for list ctor with 2 params
        types.ty().function(
            [ValType::I32, ValType::I32],
            [ValType::I32, ValType::I32]
        );
        // Type 30: (i32 x 5) -> (i32, i32) - for list ctor with 5 params
        types.ty().function(
            [ValType::I32; 5],
            [ValType::I32, ValType::I32]
        );
        // Type 31: set-attribute with attribute-value variant (canonical ABI flattened)
        // (node, name_ptr, name_len, discrim, payload_i64, payload_i32) -> ()
        // The canonical ABI joins variant payloads: i64 slot for (ptr,len)/i64/f64, i32 slot for i32/f32
        // Variant cases: 0=str, 1=bool, 2=s8, 3=s16, 4=s32, 5=s64, 6=u8, 7=u16, 8=u32, 9=u64, 10=f32, 11=f64, 12=char
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32, ValType::I64, ValType::I32],
            []
        );
        // Type 32: promote_ptr_for_variant - (ptr: i32, len: i32) -> (i64, i32)
        // Promotes fat pointer for canonical ABI variant: returns (ptr as i64, len as i32)
        types.ty().function(
            [ValType::I32, ValType::I32],
            [ValType::I64, ValType::I32]
        );
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

        // Calculate import layout for all exported components
        let import_layout = ImportLayout::new(&exported_components, self.ctx);
        // Store import_layout for use in expression emission
        self.import_layout = Some(import_layout.clone());

        // Import callbacks for each exported component
        for (comp_idx, exported_comp) in exported_components.iter().enumerate() {
            let comp_name = to_kebab_case(&self.ctx.str(exported_comp.name));

            // Use WIT package info if available for interface-qualified callback imports
            let callbacks_interface = if let Some((namespace, name, version)) = &self.wit_package {
                // Match WIT callback interface: {namespace}:{name}/{component}-callbacks@{version}
                format!("{}:{}/{}-callbacks@{}", namespace, name, comp_name, version)
            } else {
                format!("yel:ui/{}-callbacks@0.1.0", comp_name)
            };

            // Import each callback function
            for &cb_def_id in &import_layout.components[comp_idx].callback_def_ids {
                if let Some(func_def) = self.ctx.defs.as_function(cb_def_id) {
                    let cb_name = to_kebab_case(&self.ctx.str(func_def.name));
                    imports.import(&callbacks_interface, &cb_name, EntityType::Function(0));
                }
            }

            // Import [resource-new] from the EXPORT interface with [export] prefix
            // This is how wit-component expects resource intrinsics for exported resources
            let export_interface = if let Some((namespace, name, version)) = &self.wit_package {
                format!("[export]{}:{}/{}-component@{}", namespace, name, comp_name, version)
            } else {
                format!("[export]yel:ui/{}-component@0.1.0", comp_name)
            };
            let resource_new_name = format!("[resource-new]{}", comp_name);
            // Type 4: (i32) -> i32 - converts rep to handle
            imports.import(&export_interface, &resource_new_name, EntityType::Function(4));
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
        let runtime_funcs = RuntimeFunctions::new(import_layout.num_imports + 3, &concat_arities, &self.record_types, &self.list_constructs);
        self.runtime_funcs = Some(runtime_funcs.clone());

        // Local runtime functions (order must match RuntimeFunctions::new):
        // 1. s32_to_string: type 16 - (i32) -> (i32, i32)
        functions.function(16);
        // 2. bool_to_string: type 16 - (i32) -> (i32, i32)
        functions.function(16);
        // 3. f32_to_string: type 27 - (f32) -> (i32, i32)
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
        // 6. list_get: type 9 - (i32, i32, i32, i32) -> i32
        functions.function(runtime::types::LIST_GET);
        // 7. list_get_opt: type 17 - (i32, i32, i32, i32) -> (i32, i32)
        functions.function(runtime::types::LIST_GET_OPT);
        // 8. Record constructor helpers for each record type
        // For each record: ctor_at (stores at address) + ctor (allocates and returns)
        // Type indices for record ctors are computed dynamically based on field count
        for &record_def in &self.record_types {
            let field_count = self.count_record_wasm_params(record_def);
            // ctor_at: (dest: i32, ...fields) -> ()
            // Total params = field_count + 1 (for dest)
            let ctor_at_type = match field_count + 1 {
                1 => 1,  // (i32) -> ()
                2 => 3,  // (i32, i32) -> ()
                3 => 6,  // (i32, i32, i32) -> ()
                4 => 8,  // (i32, i32, i32, i32) -> ()
                5 => 7,  // (i32 x 5) -> ()
                _ => 8,  // Default to 4-param for now
            };
            functions.function(ctor_at_type);
            // ctor: (...fields) -> ptr
            // Type indices:
            //   0 params: type 2 - () -> i32
            //   1 param:  type 4 - (i32) -> i32
            //   2 params: type 5 - (i32, i32) -> i32
            //   3 params: type 24 - (i32, i32, i32) -> i32
            //   4 params: type 9 - (i32 x 4) -> i32
            //   5 params: type 25 - (i32 x 5) -> i32
            let ctor_type = match field_count {
                0 => 2,  // () -> i32
                1 => 4,  // (i32) -> i32
                2 => 5,  // (i32, i32) -> i32
                3 => 24, // (i32, i32, i32) -> i32
                4 => 9,  // (i32 x 4) -> i32
                5 => 25, // (i32 x 5) -> i32
                _ => 9,  // Default for now
            };
            functions.function(ctor_type);
        }

        // 10. List constructor helpers for each (element_type, count) pair
        // Type: (element_values...) -> (ptr, len)
        // Each element needs count_type_wasm_params params (strings=2, records=sum of fields, etc.)
        for &(elem_ty, count) in &self.list_constructs {
            let params_per_elem = self.count_type_wasm_params(elem_ty);
            let total_params = params_per_elem * count;
            // Type indices for (i32 x N) -> (i32, i32):
            // 16=1, 29=2, 28=3, 17=4, 30=5, 18=6, 19=8, 20=10, 21=12, 22=14, 23=16
            let type_idx = match total_params {
                1 => 16,  // (i32) -> (i32, i32)
                2 => 29,  // (i32, i32) -> (i32, i32)
                3 => 28,  // (i32, i32, i32) -> (i32, i32)
                4 => 17,  // (i32 x 4) -> (i32, i32)
                5 => 30,  // (i32 x 5) -> (i32, i32)
                6 => 18,  // (i32 x 6) -> (i32, i32)
                8 => 19,  // (i32 x 8) -> (i32, i32)
                10 => 20, // (i32 x 10) -> (i32, i32)
                12 => 21, // (i32 x 12) -> (i32, i32)
                14 => 22, // (i32 x 14) -> (i32, i32)
                16 => 23, // (i32 x 16) -> (i32, i32)
                _ => {
                    eprintln!("WARN: No type for list ctor with {} params, using type 17", total_params);
                    17
                }
            };
            functions.function(type_idx);
        }
        // 11. pack_fat_ptr_to_i64: type 32 - (i32, i32) -> i64
        functions.function(32);

        // For each component: constructor, mount, unmount, dispatch, getters, setters
        for component in self.components {
            functions.function(2); // constructor: () -> i32
            functions.function(3); // mount: (self: i32, root: i32) -> ()
            functions.function(1); // unmount: (self: i32) -> ()
            functions.function(3); // dispatch: (self: i32, handler-id: i32) -> ()

            for signal in &component.signals {
                let (getter_type, setter_type) = match self.ctx.ty_kind(signal.ty) {
                    InternedTyKind::F32 => (10, 11),
                    InternedTyKind::F64 => (12, 13),
                    InternedTyKind::S64 | InternedTyKind::U64 => (14, 15),
                    // String/List:
                    // - Getter returns i32 pointer to (ptr, len) tuple in memory
                    //   (MAX_FLAT_RESULTS=1 means multi-value returns use pointer-to-tuple)
                    // - Setter takes (self, ptr, len)
                    // Type 4 is (i32) -> i32
                    InternedTyKind::String | InternedTyKind::List(_) => (4, 6),
                    _ => (4, 3),
                };
                functions.function(getter_type);
                functions.function(setter_type);
            }
        }

        // Calculate base function index for block functions
        // = imports + allocator funcs (3) + runtime funcs + component funcs
        let mut block_func_base = import_layout.num_imports + 3 + runtime_funcs.count;
        for component in self.components {
            block_func_base += 4 + (component.signals.len() as u32 * 2);
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
                self.block_func_indices.insert((comp_idx, block.id), func_idx);
                // Use type 3 (i32, i32) -> () for blocks with captured locals (e.g., for-item-mount)
                // Use type 1 (i32) -> () for regular blocks
                let type_idx = if !block.captured_locals.is_empty() { 3 } else { 1 };
                functions.function(type_idx);
            }
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
        // Memory layout: [strings] -> [component state] -> [heap]
        let string_end = self.strings.base() + self.strings.size();
        let mut mem_base = ((string_end + 3) & !3) as i32; // Align to 4 bytes
        let mut layouts: Vec<MemoryLayout> = Vec::new();
        for component in self.components {
            let layout = MemoryLayout::new(component, mem_base, &mut self.layout_ctx);
            mem_base += layout.size + 64; // Add padding between components
            layouts.push(layout);
        }
        self.layouts = layouts.clone();

        // Heap starts AFTER all component state, aligned to 8 bytes
        let heap_start = crate::lir::align_to(mem_base as u32, 8);
        self.heap_base = heap_start;
        self.heap_ptr = heap_start;

        // Globals section - allocator globals for local alloc/free/cabi_realloc
        let mut globals = wasm_encoder::GlobalSection::new();
        let allocator_globals = runtime::AllocatorGlobals::new(0);
        for (global_type, init_expr) in runtime::emit_allocator_globals(heap_start) {
            globals.global(global_type, &init_expr);
        }
        module.section(&globals);

        // Export section - memory, cabi_realloc, and component functions
        let mut exports = ExportSection::new();

        // Export memory (required by canonical ABI)
        exports.export("memory", ExportKind::Memory, 0);

        // Export cabi_realloc (required by canonical ABI for string/list lifting/lowering)
        exports.export("cabi_realloc", ExportKind::Func, alloc_funcs.cabi_realloc);

        // Component function exports
        let first_component_func = import_layout.num_imports + 3 + runtime_funcs.count;

        for (_comp_idx, exported_comp) in exported_components.iter().enumerate() {
            let prefix = to_kebab_case(&self.ctx.str(exported_comp.name));

            // Find the function index for this component
            // Need to skip past all earlier components' functions
            let mut comp_func_idx = first_component_func;
            for component in self.components.iter() {
                if std::ptr::eq(component, *exported_comp) {
                    break;
                }
                // Each component has: constructor, mount, unmount, dispatch + 2*signals
                comp_func_idx += 4 + (component.signals.len() as u32 * 2);
            }

            // Build interface path prefix if WIT package info is available
            // Format: namespace:name/interface@version#function
            let interface_prefix = if let Some((namespace, name, version)) = &self.wit_package {
                let interface_name = format!("{}-component", prefix);
                format!("{}:{}/{}@{}#", namespace, name, interface_name, version)
            } else {
                String::new()
            };

            exports.export(&format!("{}[constructor]{}", interface_prefix, prefix), ExportKind::Func, comp_func_idx);
            exports.export(&format!("{}[method]{}.mount", interface_prefix, prefix), ExportKind::Func, comp_func_idx + 1);
            exports.export(&format!("{}[method]{}.unmount", interface_prefix, prefix), ExportKind::Func, comp_func_idx + 2);
            exports.export(&format!("{}[method]{}.dispatch", interface_prefix, prefix), ExportKind::Func, comp_func_idx + 3);

            for (sig_idx, signal) in exported_comp.signals.iter().enumerate() {
                let getter_idx = comp_func_idx + 4 + (sig_idx as u32 * 2);
                let setter_idx = getter_idx + 1;
                let sig_name = self.signal_name(signal.def_id);
                exports.export(
                    &format!("{}[method]{}.get-{}", interface_prefix, prefix, to_wit_name(&sig_name)),
                    ExportKind::Func,
                    getter_idx,
                );
                exports.export(
                    &format!("{}[method]{}.set-{}", interface_prefix, prefix, to_wit_name(&sig_name)),
                    ExportKind::Func,
                    setter_idx,
                );
            }
        }
        module.section(&exports);

        // Code section - allocator functions, runtime functions, component functions
        // NOTE: Memory layouts were computed earlier (before globals section)
        let mut code = CodeSection::new();

        // Allocator functions (must be first, matching function declaration order):
        // 1. alloc
        code.function(&runtime::emit_alloc(&allocator_globals));
        // 2. free
        code.function(&runtime::emit_free(&allocator_globals));
        // 3. cabi_realloc (calls alloc and free)
        code.function(&runtime::emit_cabi_realloc(alloc_funcs.alloc, alloc_funcs.free));

        // Runtime functions (order must match function declarations):
        // 1. s32_to_string
        code.function(&runtime::emit_s32_to_string());
        // 2. bool_to_string
        let (true_ptr, _) = self.strings.get("true").unwrap_or((0, 0));
        let (false_ptr, _) = self.strings.get("false").unwrap_or((0, 0));
        code.function(&runtime::emit_bool_to_string(true_ptr, false_ptr));
        // 3. f32_to_string
        code.function(&runtime::emit_f32_to_string());
        // 4. concat<n> for each required arity (uses bulk memory.copy internally)
        for &arity in &concat_arities {
            code.function(&runtime::emit_concat_n(arity, alloc_funcs.cabi_realloc));
        }
        // 5. store_fat_ptr
        code.function(&runtime::emit_store_fat_ptr());
        // 6. load_fat_ptr
        code.function(&runtime::emit_load_fat_ptr());
        // 7. list_get (bounds-checked index, traps on OOB)
        code.function(&runtime::emit_list_get());
        // 8. list_get_opt (safe index, returns option)
        // The if block inside needs type () -> (i32, i32) for multi-value result
        code.function(&runtime::emit_list_get_opt(runtime::types::VOID_I32_I32));
        // 9. Record constructor helpers
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

        // Release the borrow on exported_components before mutable operations
        drop(exported_components);

        for comp_idx in 0..self.components.len() {
            let layout = layouts[comp_idx].clone();
            let component = &self.components[comp_idx];

            // For exported components, get the [resource-new] import index
            // This is imported from [export]namespace:name/interface and converts rep → handle
            let resource_new_idx: Option<u32> = if component.is_export {
                let export_idx = exported_indices.iter().position(|&i| i == comp_idx);
                export_idx.map(|idx| import_layout.components[idx].resource_new)
            } else {
                None
            };

            let num_signals = component.signals.len();

            code.function(&self.generate_constructor_for(&self.components[comp_idx], &layout, resource_new_idx, comp_idx)?);
            // Reset handler counter before mount - dispatch will use same ordering
            self.reset_handler_counter();
            code.function(&self.generate_mount_for(comp_idx, &layout)?);
            code.function(&self.generate_unmount_for(&self.components[comp_idx], &layout)?);
            // Reset again for dispatch to match mount's ordering
            self.reset_handler_counter();
            code.function(&self.generate_dispatch_for(comp_idx, &layout)?);

            for sig_idx in 0..num_signals {
                code.function(&self.generate_getter_for(&self.components[comp_idx], &layout, sig_idx)?);
                code.function(&self.generate_setter_for(comp_idx, &layout, sig_idx, alloc_funcs.cabi_realloc)?);
            }
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

    fn generate_name_section_multi(&self, module: &mut Module, layouts: &[MemoryLayout], import_layout: &ImportLayout) {
        let exported_components = self.get_exported_components();
        let mut names = NameSection::new();

        // Use the first exported component's name as the module name
        if let Some(first_export) = exported_components.first() {
            names.module(&self.ctx.str(first_export.name));
        }

        // =================================================================
        // Type names - describe each function type signature
        // =================================================================
        let mut type_names = NameMap::new();
        type_names.append(0, "type_void_void");           // () -> ()
        type_names.append(1, "type_i32_void");            // (i32) -> ()
        type_names.append(2, "type_void_i32");            // () -> i32
        type_names.append(3, "type_i32_i32_void");        // (i32, i32) -> ()
        type_names.append(4, "type_i32_i32");             // (i32) -> i32
        type_names.append(5, "type_i32_i32_i32");         // (i32, i32) -> i32
        type_names.append(6, "type_i32_i32_i32_void");    // (i32, i32, i32) -> ()
        type_names.append(7, "type_i32x5_void");          // (i32, i32, i32, i32, i32) -> ()
        type_names.append(8, "type_i32x4_void");          // (i32, i32, i32, i32) -> ()
        type_names.append(9, "type_i32x4_i32");           // (i32, i32, i32, i32) -> i32 (realloc)
        type_names.append(10, "type_i32_f32");            // (i32) -> f32 (getter f32)
        type_names.append(11, "type_i32_f32_void");       // (i32, f32) -> () (setter f32)
        type_names.append(12, "type_i32_f64");            // (i32) -> f64 (getter f64)
        type_names.append(13, "type_i32_f64_void");       // (i32, f64) -> () (setter f64)
        type_names.append(14, "type_i32_i64");            // (i32) -> i64 (getter i64)
        type_names.append(15, "type_i32_i64_void");       // (i32, i64) -> () (setter i64)
        names.types(&type_names);

        // =================================================================
        // Memory names
        // =================================================================
        let mut memory_names = NameMap::new();
        memory_names.append(0, "memory");
        names.memories(&memory_names);

        // Note: No globals in main module - allocator globals are in allocator module

        // =================================================================
        // Function names
        // =================================================================
        let mut func_names = NameMap::new();
        let dom_func_names = [
            "create-element", "create-text", "create-comment", "set-attribute",
            "remove-attribute", "set-text-content", "set-style", "set-class",
            "append-child", "insert-before", "remove-child", "remove",
            "get-parent", "get-next-sibling", "add-event-listener", "remove-event-listener",
            "insert-after",
        ];
        for (i, name) in dom_func_names.iter().enumerate() {
            func_names.append(i as u32, name);
        }

        // Callback imports and resource intrinsics for each exported component
        let mut import_idx = NUM_DOM_IMPORTS;
        for (comp_idx, exported_comp) in exported_components.iter().enumerate() {
            let prefix = to_kebab_case(&self.ctx.str(exported_comp.name));
            for &cb_def_id in &import_layout.components[comp_idx].callback_def_ids {
                if let Some(func_def) = self.ctx.defs.as_function(cb_def_id) {
                    let cb_name = to_kebab_case(&self.ctx.str(func_def.name));
                    func_names.append(import_idx, &format!("[callback]{}.{}", prefix, cb_name));
                    import_idx += 1;
                }
            }
            // [resource-new]component import
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
            func_names.append(runtime_funcs.bool_to_string, "bool_to_string");
            for (&arity, &func_idx) in &runtime_funcs.concat_indices {
                func_names.append(func_idx, &format!("concat{}", arity));
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
            func_names.append(func_idx + 3, &format!("[method]{}.dispatch", prefix));

            for (sig_idx, signal) in component.signals.iter().enumerate() {
                let getter_idx = func_idx + 4 + (sig_idx as u32 * 2);
                let setter_idx = getter_idx + 1;
                let sig_name = self.signal_name(signal.def_id);
                func_names.append(getter_idx, &format!("[method]{}.get-{}", prefix, to_wit_name(&sig_name)));
                func_names.append(setter_idx, &format!("[method]{}.set-{}", prefix, to_wit_name(&sig_name)));
            }

            func_idx += 4 + (component.signals.len() as u32 * 2);
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
            // Slot locals (temps) start at 2
            for slot in &component.slots {
                if let crate::lir::SlotKind::Temp { local_idx } = slot.kind {
                    mount_locals.append(local_idx + 2, &format!("slot_{}", slot.id.0));
                }
            }
            local_names.append(func_idx, &mount_locals);
            func_idx += 1;

            // Unmount - has (self) param
            let mut unmount_locals = NameMap::new();
            unmount_locals.append(0, "self");
            local_names.append(func_idx, &unmount_locals);
            func_idx += 1;

            // Dispatch - has (handler_id) param
            let mut dispatch_locals = NameMap::new();
            dispatch_locals.append(0, "handler_id");
            local_names.append(func_idx, &dispatch_locals);
            func_idx += 1;

            // Getters and setters for each signal
            for (_sig_idx, signal) in component.signals.iter().enumerate() {
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

        // Block function locals - each has (parent) param + temp locals
        for ((_comp_idx, _block_id), &wasm_func_idx) in &self.block_func_indices {
            let mut block_locals = NameMap::new();
            block_locals.append(0, "parent");
            // TODO: add temp local names if we track them per-block
            local_names.append(wasm_func_idx, &block_locals);
        }

        names.locals(&local_names);

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
 
  

    fn generate_constructor_for(
        &mut self,
        component: &LirComponent,
        layout: &MemoryLayout,
        import_resource_new: Option<u32>,
        comp_idx: usize,
    ) -> Result<Function, CodegenError> {
        // Check if we need temp locals for signal initialization
        let has_string_signals = component.signals.iter().any(|s| {
            matches!(self.ctx.ty_kind(s.ty), InternedTyKind::String)
        });
        let has_list_signals = component.signals.iter().any(|s| {
            matches!(self.ctx.ty_kind(s.ty), InternedTyKind::List(_))
        });

        // Allocate temp locals if needed for string or list initialization
        let locals: Vec<(u32, ValType)> = if has_string_signals || has_list_signals {
            vec![(2, ValType::I32)] // temp_ptr (local 0), temp_len (local 1)
        } else {
            vec![]
        };
        let mut func = Function::new(locals);

        // Execute constructor block operations (signal initialization)
        let constructor_block = component.get_block(component.constructor_block);
        for op in &constructor_block.ops.clone() {
            self.emit_op(&mut func, op, comp_idx, 0)?; // Constructor has no params, offset 0
        }

        // Initialize memory slots to 0 (these are allocated during mount block lowering)
        for slot in &component.slots {
            if let crate::lir::SlotKind::Memory { offset, size } = slot.kind {
                func.instruction(&Instruction::I32Const(layout.base + offset as i32));
                func.instruction(&Instruction::I32Const(0));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                let _ = size; // May need to handle larger sizes
            }
        }

        // For exported resources, we need to convert rep → handle using [resource-new]
        // Push rep (memory address of component state) onto stack
        func.instruction(&Instruction::I32Const(layout.base));
        // Call [resource-new]component to register the rep and get a handle
        if let Some(resource_new_idx) = import_resource_new {
            func.instruction(&Instruction::Call(resource_new_idx));
        }
        // Return handle (or rep if not using resource intrinsics)
        func.instruction(&Instruction::End);
        Ok(func)
    }

    fn generate_mount_for(&mut self, comp_idx: usize, _layout: &MemoryLayout) -> Result<Function, CodegenError> {
        let component = &self.components[comp_idx];
        // Find the max slot ID among Temp slots (not count, since IDs may have gaps due to Memory slots)
        let max_temp_slot = component.slots.iter()
            .enumerate()
            .filter(|(_, s)| matches!(s.kind, crate::lir::SlotKind::Temp { .. }))
            .map(|(i, _)| i as u32)
            .max()
            .unwrap_or(0);
        let locals = vec![(max_temp_slot + 1, ValType::I32)];
        let mut func = Function::new(locals);

        // Copy root parameter (param 1) to slot 0 (local 2)
        // Slot 0 is the implicit "root" slot used by AppendChild operations
        func.instruction(&Instruction::LocalGet(1)); // root param
        func.instruction(&Instruction::LocalSet(2)); // slot_0

        // Block-based: iterate mount block operations
        let mount_block = component.get_block(component.mount_block);
        for op in &mount_block.ops.clone() {
            self.emit_op(&mut func, op, comp_idx, 2)?; // Mount has self, root params, offset 2
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Emit a single block operation as WASM instructions.
    /// `local_offset` is added to slot indices for local variable access:
    /// - Mount function: 2 (for self, root params)
    /// - Block functions: 1 (for parent param) or 2 (for parent, item_ptr params)
    fn emit_op(
        &mut self,
        func: &mut Function,
        op: &crate::lir::LirOp,
        comp_idx: usize,
        local_offset: u32,
    ) -> Result<(), CodegenError> {
        use crate::lir::LirOp;
        let component = &self.components[comp_idx];
        let layout = self.layouts.get(comp_idx).cloned().unwrap_or_else(|| {
            MemoryLayout {
                base: 324,
                signal_offsets: component.signals.iter().enumerate()
                    .map(|(i, _)| (i as i32) * 4)
                    .collect(),
                size: 0,
            }
        });

        match op {
            LirOp::CreateElement { tag, result } => {
                let tag_str = component.get_string(*tag);
                if let Some((ptr, len)) = self.get_string_info(tag_str) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    func.instruction(&Instruction::Call(IMPORT_CREATE_ELEMENT));
                    func.instruction(&Instruction::LocalSet(result.0 + local_offset));
                }
            }
            LirOp::CreateText { content, result } => {
                let text = component.get_string(*content);
                if let Some((ptr, len)) = self.get_string_info(text) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    func.instruction(&Instruction::Call(IMPORT_CREATE_TEXT));
                    func.instruction(&Instruction::LocalSet(result.0 + local_offset));
                }
            }
            LirOp::AppendChild { parent, child } => {
                func.instruction(&Instruction::LocalGet(parent.0 + local_offset));
                func.instruction(&Instruction::LocalGet(child.0 + local_offset));
                func.instruction(&Instruction::Call(IMPORT_APPEND_CHILD));
            }
            LirOp::SetAttribute { node, name, expr } => {
                let attr_name = component.get_string(*name);
                let attr_expr = component.get_expr(*expr);
                func.instruction(&Instruction::LocalGet(node.0 + local_offset));
                if let Some((name_ptr, name_len)) = self.get_string_info(attr_name) {
                    func.instruction(&Instruction::I32Const(name_ptr as i32));
                    func.instruction(&Instruction::I32Const(name_len as i32));
                }
                // Emit attribute-value variant: (discrim, p0: i32, p1: i32, p2: i64, p3: f32, p4: f64)
                self.emit_expr_as_attr_value(func, attr_expr, component, &layout)?;
                func.instruction(&Instruction::Call(IMPORT_SET_ATTRIBUTE));
            }
            LirOp::CreateTextDynamic { expr, result } => {
                let text_expr = component.get_expr(*expr);
                self.emit_expr_as_string(func, text_expr, component, &layout)?;
                func.instruction(&Instruction::Call(IMPORT_CREATE_TEXT));
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }
            LirOp::StoreHandle { slot, from } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    if let crate::lir::SlotKind::Memory { offset, .. } = slot_info.kind {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::LocalGet(from.0 + local_offset));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                }
            }
            LirOp::LoadHandle { slot, to } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    if let crate::lir::SlotKind::Memory { offset, .. } = slot_info.kind {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        func.instruction(&Instruction::LocalSet(to.0 + local_offset));
                    }
                }
            }
            LirOp::SetTextContent { node, expr } => {
                let text_expr = component.get_expr(*expr);
                func.instruction(&Instruction::LocalGet(node.0 + local_offset));
                self.emit_expr_as_string(func, text_expr, component, &layout)?;
                func.instruction(&Instruction::Call(IMPORT_SET_TEXT_CONTENT));
            }
            LirOp::CreateComment { content, result } => {
                let text = component.get_string(*content);
                if let Some((ptr, len)) = self.get_string_info(text) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                    func.instruction(&Instruction::Call(IMPORT_CREATE_COMMENT));
                    func.instruction(&Instruction::LocalSet(result.0 + local_offset));
                }
            }
            LirOp::Remove { node } => {
                func.instruction(&Instruction::LocalGet(node.0 + local_offset));
                func.instruction(&Instruction::Call(IMPORT_REMOVE));
            }
            LirOp::InsertAfter { parent, node, anchor } => {
                func.instruction(&Instruction::LocalGet(parent.0 + local_offset));
                func.instruction(&Instruction::LocalGet(node.0 + local_offset));
                func.instruction(&Instruction::LocalGet(anchor.0 + local_offset));
                func.instruction(&Instruction::Call(IMPORT_INSERT_AFTER));
            }
            LirOp::StoreI32 { slot, value } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    if let crate::lir::SlotKind::Memory { offset, .. } = slot_info.kind {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Const(*value));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                }
            }
            LirOp::LoadI32 { slot, to } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    if let crate::lir::SlotKind::Memory { offset, .. } = slot_info.kind {
                        func.instruction(&Instruction::I32Const(offset as i32));
                        func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                        func.instruction(&Instruction::LocalSet(to.0 + local_offset));
                    }
                }
            }
            LirOp::EvalExpr { expr, result } => {
                let lir_expr = component.get_expr(*expr);
                self.emit_expr(func, lir_expr, component, &layout)?;
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }
            LirOp::If { cond, then_ops, else_ops } => {
                func.instruction(&Instruction::LocalGet(cond.0 + local_offset));
                func.instruction(&Instruction::If(BlockType::Empty));

                for nested_op in then_ops {
                    self.emit_op(func, nested_op, comp_idx, local_offset)?;
                }

                if !else_ops.is_empty() {
                    func.instruction(&Instruction::Else);
                    for nested_op in else_ops {
                        self.emit_op(func, nested_op, comp_idx, local_offset)?;
                    }
                }

                func.instruction(&Instruction::End);
            }
            LirOp::CallBlock { block, parent } => {
                if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, *block)) {
                    func.instruction(&Instruction::LocalGet(parent.0 + local_offset));
                    func.instruction(&Instruction::Call(func_idx));
                }
            }
            LirOp::AddEventListener { node, event, handler } => {
                let event_str = component.get_string(*event);
                func.instruction(&Instruction::LocalGet(node.0 + local_offset));
                if let Some((ptr, len)) = self.get_string_info(event_str) {
                    func.instruction(&Instruction::I32Const(ptr as i32));
                    func.instruction(&Instruction::I32Const(len as i32));
                }
                func.instruction(&Instruction::I32Const(handler.0 as i32));
                func.instruction(&Instruction::Call(IMPORT_ADD_EVENT_LISTENER));
            }

            // === Constructor Operations ===
            LirOp::InitSignal { signal_idx, expr } => {
                let addr = layout.signal_addr(*signal_idx as usize);
                let default_expr = component.get_expr(*expr);
                self.emit_signal_store(func, addr, default_expr, component, &layout, 0)?;
            }

            LirOp::InitSignalDefault { signal_idx } => {
                let signal = &component.signals[*signal_idx as usize];
                let addr = layout.signal_addr(*signal_idx as usize);
                match self.ctx.ty_kind(signal.ty) {
                    InternedTyKind::String => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        func.instruction(&Instruction::I32Const(addr + 4));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                    InternedTyKind::F32 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::F32Const(0.0));
                        func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                    }
                    InternedTyKind::F64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::F64Const(0.0));
                        func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                    }
                    InternedTyKind::S64 | InternedTyKind::U64 => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I64Const(0));
                        func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                    }
                    _ => {
                        func.instruction(&Instruction::I32Const(addr));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                }
            }

            LirOp::InitMemorySlot { slot } => {
                if let Some(slot_info) = component.slots.get(slot.0 as usize) {
                    if let crate::lir::SlotKind::Memory { offset, .. } = slot_info.kind {
                        func.instruction(&Instruction::I32Const(layout.base + offset as i32));
                        func.instruction(&Instruction::I32Const(0));
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                    }
                }
            }

            LirOp::ResourceNew { base_addr } => {
                func.instruction(&Instruction::I32Const(*base_addr));
            }

            LirOp::SignalRead { .. } => {
                // TODO: implement signal read
            }

            LirOp::SignalWrite { signal, value } => {
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    let addr = layout.signal_addr(sig_idx);
                    let signal_ty = component.signals[sig_idx].ty;
                    func.instruction(&Instruction::I32Const(addr));
                    func.instruction(&Instruction::LocalGet(value.0 + local_offset));
                    // Use appropriate store instruction based on signal type
                    match self.ctx.ty_kind(signal_ty) {
                        InternedTyKind::F32 => {
                            func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                        }
                        InternedTyKind::F64 => {
                            func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                        }
                        InternedTyKind::S64 | InternedTyKind::U64 => {
                            func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                        }
                        _ => {
                            func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        }
                    }
                }
            }

            LirOp::TriggerEffects { signal } => {
                for effect in &component.effects {
                    if effect.dependencies.contains(signal) {
                        if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, effect.update_block)) {
                            func.instruction(&Instruction::I32Const(0));
                            func.instruction(&Instruction::Call(func_idx));
                        }
                    }
                }
            }

            LirOp::Return => {
                func.instruction(&Instruction::Return);
            }

            // === Loop Operations ===
            LirOp::Loop { break_cond, body_ops } => {
                func.instruction(&Instruction::Block(BlockType::Empty));
                func.instruction(&Instruction::Loop(BlockType::Empty));

                func.instruction(&Instruction::LocalGet(break_cond.0 + local_offset));
                func.instruction(&Instruction::BrIf(1));

                for nested_op in body_ops {
                    self.emit_op(func, nested_op, comp_idx, local_offset)?;
                }

                func.instruction(&Instruction::Br(0));
                func.instruction(&Instruction::End);
                func.instruction(&Instruction::End);
            }

            LirOp::CallBlock2 { block, param0, param1 } => {
                if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, *block)) {
                    func.instruction(&Instruction::LocalGet(param0.0 + local_offset));
                    func.instruction(&Instruction::LocalGet(param1.0 + local_offset));
                    func.instruction(&Instruction::Call(func_idx));
                }
            }

            LirOp::GeU { index, len, result } => {
                func.instruction(&Instruction::LocalGet(index.0 + local_offset));
                func.instruction(&Instruction::LocalGet(len.0 + local_offset));
                func.instruction(&Instruction::I32GeU);
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }

            LirOp::ComputeItemPtr { base, index, element_size, result } => {
                func.instruction(&Instruction::LocalGet(base.0 + local_offset));
                func.instruction(&Instruction::LocalGet(index.0 + local_offset));
                func.instruction(&Instruction::I32Const(*element_size as i32));
                func.instruction(&Instruction::I32Mul);
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }

            LirOp::IncrSlot { slot } => {
                func.instruction(&Instruction::LocalGet(slot.0 + local_offset));
                func.instruction(&Instruction::I32Const(1));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(slot.0 + local_offset));
            }

            LirOp::Alloc { size, align, result } => {
                if let Some(alloc_funcs) = &self.alloc_funcs {
                    func.instruction(&Instruction::LocalGet(size.0 + local_offset));
                    func.instruction(&Instruction::I32Const(*align as i32));
                    func.instruction(&Instruction::Call(alloc_funcs.alloc));
                    func.instruction(&Instruction::LocalSet(result.0 + local_offset));
                }
            }

            LirOp::Free { ptr, size } => {
                if let Some(alloc_funcs) = &self.alloc_funcs {
                    func.instruction(&Instruction::LocalGet(ptr.0 + local_offset));
                    func.instruction(&Instruction::LocalGet(size.0 + local_offset));
                    func.instruction(&Instruction::Call(alloc_funcs.free));
                }
            }

            LirOp::MulConst { slot, constant, result } => {
                func.instruction(&Instruction::LocalGet(slot.0 + local_offset));
                func.instruction(&Instruction::I32Const(*constant as i32));
                func.instruction(&Instruction::I32Mul);
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }

            LirOp::AddSlots { a, b, result } => {
                func.instruction(&Instruction::LocalGet(a.0 + local_offset));
                func.instruction(&Instruction::LocalGet(b.0 + local_offset));
                func.instruction(&Instruction::I32Add);
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }

            LirOp::LoadI32Addr { addr, result } => {
                func.instruction(&Instruction::LocalGet(addr.0 + local_offset));
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                func.instruction(&Instruction::LocalSet(result.0 + local_offset));
            }

            LirOp::StoreI32Addr { addr, value } => {
                func.instruction(&Instruction::LocalGet(addr.0 + local_offset));
                func.instruction(&Instruction::LocalGet(value.0 + local_offset));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }

            LirOp::LoadList { signal, ptr_result, len_result } => {
                if let Some(sig_idx) = self.signal_index_in(component, *signal) {
                    let addr = layout.signal_addr(sig_idx);
                    // Load ptr from signal_addr
                    func.instruction(&Instruction::I32Const(addr));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalSet(ptr_result.0 + local_offset));

                    // Load len from signal_addr + 4
                    func.instruction(&Instruction::I32Const(addr + 4));
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalSet(len_result.0 + local_offset));
                }
            }

            LirOp::LoadListFromLocal { base_local, field_idx, record_def, ptr_result, len_result } => {
                // Get the base local's slot (it holds a pointer to the record)
                let base_slot = if let Some(captured_map) = &self.current_block_captured_locals {
                    captured_map.get(base_local).map(|&slot| slot + local_offset)
                } else {
                    None
                }.or_else(|| {
                    self.current_block_local_to_slot.as_ref()
                        .and_then(|map| map.get(base_local))
                        .map(|slot_id| slot_id.0 + local_offset)
                });

                if let Some(base_local_idx) = base_slot {
                    // Compute field offset within the record
                    let field_offset = self.compute_field_offset(*record_def, *field_idx);

                    // Load ptr from base + field_offset
                    func.instruction(&Instruction::LocalGet(base_local_idx));
                    if field_offset > 0 {
                        func.instruction(&Instruction::I32Const(field_offset as i32));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalSet(ptr_result.0 + local_offset));

                    // Load len from base + field_offset + 4
                    func.instruction(&Instruction::LocalGet(base_local_idx));
                    if field_offset > 0 {
                        func.instruction(&Instruction::I32Const(field_offset as i32 + 4));
                        func.instruction(&Instruction::I32Add);
                    } else {
                        func.instruction(&Instruction::I32Const(4));
                        func.instruction(&Instruction::I32Add);
                    }
                    func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                    func.instruction(&Instruction::LocalSet(len_result.0 + local_offset));
                }
            }

            LirOp::SetSlot { slot, value } => {
                func.instruction(&Instruction::I32Const(*value));
                func.instruction(&Instruction::LocalSet(slot.0 + local_offset));
            }
        }
        Ok(())
    }

    // Expression emission functions moved to expr.rs

    fn generate_unmount_for(&self, component: &LirComponent, _layout: &MemoryLayout) -> Result<Function, CodegenError> {
        // TODO: Block-based unmount - iterate memory slots and remove nodes
        let mut func = Function::new([]);

        // Remove nodes stored in memory slots
        for slot in &component.slots {
            if let crate::lir::SlotKind::Memory { offset, .. } = slot.kind {
                func.instruction(&Instruction::I32Const(offset as i32));
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
                func.instruction(&Instruction::Call(IMPORT_REMOVE));
            }
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    fn generate_dispatch_for(&mut self, comp_idx: usize, _layout: &MemoryLayout) -> Result<Function, CodegenError> {
        // Dispatch function: (self: i32, handler_id: i32) -> ()
        // WIT method signature: dispatch(handler-id: u32)
        // Lowered: (self: i32, handler_id: i32) -> ()
        // Looks up handler block by ID and calls it
        let mut func = Function::new([]);

        let component = &self.components[comp_idx];

        // Collect all handler blocks from AddEventListener ops in mount block
        let mount_block = component.get_block(component.mount_block);
        let mut handlers: Vec<(u32, crate::lir::BlockId)> = Vec::new();
        for op in &mount_block.ops {
            if let crate::lir::LirOp::AddEventListener { handler, .. } = op {
                handlers.push((handler.0, *handler));
            }
        }

        if handlers.is_empty() {
            func.instruction(&Instruction::End);
            return Ok(func);
        }

        // Generate dispatch table using if-else chain
        // param 0 = self (resource rep, unused)
        // param 1 = handler_id
        for (i, (handler_id, block_id)) in handlers.iter().enumerate() {
            // Check if handler_id matches
            func.instruction(&Instruction::LocalGet(1)); // handler_id param (was 0, now 1 due to self)
            func.instruction(&Instruction::I32Const(*handler_id as i32));
            func.instruction(&Instruction::I32Eq);

            func.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            // Call the handler block function
            if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, *block_id)) {
                func.instruction(&Instruction::I32Const(0)); // parent = 0 (unused in handlers)
                func.instruction(&Instruction::Call(func_idx));
            }
            func.instruction(&Instruction::Return);

            func.instruction(&Instruction::End); // end if

            // If last handler, no else needed
            if i < handlers.len() - 1 {
                // Continue checking next handler
            }
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Generate a WASM function for a LIR block.
    /// Regular blocks have signature (i32) -> () where the parameter is the parent slot.
    /// For-item-mount blocks have signature (i32, i32) -> () where params are (parent, item_ptr).
    fn generate_block_function(
        &mut self,
        comp_idx: usize,
        block_id: crate::lir::BlockId,
    ) -> Result<Function, CodegenError> {
        let component = &self.components[comp_idx];
        let block = component.get_block(block_id);
        let has_captures = !block.captured_locals.is_empty();

        // Count slots needed for this block (minimum 1 for parent slot, +1 if has item_ptr)
        let num_slots = self.count_slots_in_ops(&block.ops).max(if has_captures { 2 } else { 1 });

        // Declare locals with their actual types (in slot order)
        let mut locals = Vec::new();
        for slot_idx in 0..num_slots {
            let val_ty = component
                .slots
                .get(slot_idx as usize)
                .map(|s| match s.val_ty {
                    SlotValType::I32 => ValType::I32,
                    SlotValType::I64 => ValType::I64,
                    SlotValType::F32 => ValType::F32,
                    SlotValType::F64 => ValType::F64,
                })
                .unwrap_or(ValType::I32);
            locals.push((1, val_ty));
        }

        let mut func = Function::new(locals);

        // Copy parent parameter (param 0) to slot 0 (local starts after params)
        let local_offset = if has_captures { 2 } else { 1 }; // Adjust for number of params
        func.instruction(&Instruction::LocalGet(0)); // parent param
        func.instruction(&Instruction::LocalSet(local_offset)); // slot_0

        // If this is a for-item-mount block, copy item_ptr param to slot 1
        if has_captures {
            func.instruction(&Instruction::LocalGet(1)); // item_ptr param
            func.instruction(&Instruction::LocalSet(local_offset + 1)); // slot_1
        }

        // Set captured locals, local_to_slot, and local_offset for expression emission
        self.current_block_local_offset = Some(local_offset);
        if has_captures {
            self.current_block_captured_locals = Some(block.captured_locals.clone());
        }
        if !block.local_to_slot.is_empty() {
            self.current_block_local_to_slot = Some(block.local_to_slot.clone());
        }

        // Emit block operations
        for op in &block.ops.clone() {
            self.emit_op(&mut func, op, comp_idx, local_offset)?;
        }

        // Clear captured locals, local_to_slot, and local_offset
        self.current_block_captured_locals = None;
        self.current_block_local_to_slot = None;
        self.current_block_local_offset = None;

        func.instruction(&Instruction::End);
        Ok(func)
    }




    fn generate_getter_for(&self, component: &LirComponent, layout: &MemoryLayout, sig_idx: usize) -> Result<Function, CodegenError> {
        let mut func = Function::new([]);
        let addr = layout.signal_addr(sig_idx);
        let signal = &component.signals[sig_idx];

        match self.ctx.ty_kind(signal.ty) {
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
            _ => {
                func.instruction(&Instruction::I32Const(addr));
                func.instruction(&Instruction::I32Load(mem_arg(0, 2)));
            }
        }

        func.instruction(&Instruction::End);
        Ok(func)
    }

    fn generate_setter_for(
        &mut self,
        comp_idx: usize,
        layout: &MemoryLayout,
        sig_idx: usize,
        _import_realloc: u32,
    ) -> Result<Function, CodegenError> {
        let component = &self.components[comp_idx];
        let signal = &component.signals[sig_idx];
        let signal_def_id = signal.def_id;

        // Setter just stores value and calls effect block functions - no locals needed
        let mut func = Function::new([]);
        let addr = layout.signal_addr(sig_idx);

        func.instruction(&Instruction::I32Const(addr));
        func.instruction(&Instruction::LocalGet(1));

        match self.ctx.ty_kind(signal.ty) {
            InternedTyKind::F32 => {
                func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
            }
            InternedTyKind::F64 => {
                func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
            }
            InternedTyKind::S64 | InternedTyKind::U64 => {
                func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
            }
            _ => {
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }
        }

        self.generate_effects_for_signal_in(&mut func, signal_def_id, comp_idx, layout)?;

        func.instruction(&Instruction::End);
        Ok(func)
    }

    /// Emits code to store an expression value to a signal address.
    /// For multi-value types like strings, uses temp_local_start and temp_local_start+1 as scratch locals.
    fn emit_signal_store(
        &mut self,
        func: &mut Function,
        addr: i32,
        expr: &LirExpr,
        component: &LirComponent,
        layout: &MemoryLayout,
        temp_local_start: u32,
    ) -> Result<(), CodegenError> {
        match self.ctx.ty_kind(expr.ty) {
            InternedTyKind::String | InternedTyKind::List(_) => {
                // String and List signals store (ptr, len) at addr and addr+4
                // Emit expression - pushes (ptr, len) onto stack
                self.emit_expr(func, expr, component, layout)?;
                // Stack: [ptr, len] with len on top
                func.instruction(&Instruction::LocalSet(temp_local_start + 1)); // save len
                func.instruction(&Instruction::LocalSet(temp_local_start)); // save ptr
                // Store ptr at addr
                func.instruction(&Instruction::I32Const(addr));
                func.instruction(&Instruction::LocalGet(temp_local_start));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                // Store len at addr+4
                func.instruction(&Instruction::I32Const(addr + 4));
                func.instruction(&Instruction::LocalGet(temp_local_start + 1));
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }
            InternedTyKind::F32 => {
                func.instruction(&Instruction::I32Const(addr));
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
            }
            InternedTyKind::F64 => {
                func.instruction(&Instruction::I32Const(addr));
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
            }
            InternedTyKind::S64 | InternedTyKind::U64 => {
                func.instruction(&Instruction::I32Const(addr));
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
            }
            _ => {
                func.instruction(&Instruction::I32Const(addr));
                self.emit_expr(func, expr, component, layout)?;
                func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
            }
        }
        Ok(())
    }

    /// Recursively count max slot used in ops (including nested If ops).
    fn count_slots_in_ops(&self, ops: &[crate::lir::LirOp]) -> u32 {
        use crate::lir::LirOp;

        let mut max_slot = 0u32;
        for op in ops {
            match op {
                LirOp::LoadHandle { to, .. } | LirOp::LoadI32 { to, .. } => {
                    max_slot = max_slot.max(to.0 + 1);
                }
                LirOp::SetTextContent { node, .. } | LirOp::Remove { node } => {
                    max_slot = max_slot.max(node.0 + 1);
                }
                LirOp::EvalExpr { result, .. } => {
                    // EvalExpr stores result in the specified slot
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::CreateElement { result, .. }
                | LirOp::CreateText { result, .. }
                | LirOp::CreateTextDynamic { result, .. }
                | LirOp::CreateComment { result, .. } => {
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::AppendChild { parent, child } => {
                    max_slot = max_slot.max(parent.0 + 1);
                    max_slot = max_slot.max(child.0 + 1);
                }
                LirOp::InsertAfter { parent, node, anchor } => {
                    max_slot = max_slot.max(parent.0 + 1);
                    max_slot = max_slot.max(node.0 + 1);
                    max_slot = max_slot.max(anchor.0 + 1);
                }
                LirOp::If { cond, then_ops, else_ops } => {
                    max_slot = max_slot.max(cond.0 + 1);
                    max_slot = max_slot.max(self.count_slots_in_ops(then_ops));
                    max_slot = max_slot.max(self.count_slots_in_ops(else_ops));
                }
                LirOp::Loop { break_cond, body_ops } => {
                    max_slot = max_slot.max(break_cond.0 + 1);
                    max_slot = max_slot.max(self.count_slots_in_ops(body_ops));
                }
                LirOp::GeU { index, len, result } => {
                    max_slot = max_slot.max(index.0 + 1);
                    max_slot = max_slot.max(len.0 + 1);
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::ComputeItemPtr { base, index, result, .. } => {
                    max_slot = max_slot.max(base.0 + 1);
                    max_slot = max_slot.max(index.0 + 1);
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::IncrSlot { slot } => {
                    max_slot = max_slot.max(slot.0 + 1);
                }
                LirOp::CallBlock { parent, .. } => {
                    max_slot = max_slot.max(parent.0 + 1);
                }
                LirOp::CallBlock2 { param0, param1, .. } => {
                    max_slot = max_slot.max(param0.0 + 1);
                    max_slot = max_slot.max(param1.0 + 1);
                }
                LirOp::Alloc { size, result, .. } => {
                    max_slot = max_slot.max(size.0 + 1);
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::Free { ptr, size } => {
                    max_slot = max_slot.max(ptr.0 + 1);
                    max_slot = max_slot.max(size.0 + 1);
                }
                LirOp::MulConst { slot, result, .. } => {
                    max_slot = max_slot.max(slot.0 + 1);
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::AddSlots { a, b, result } => {
                    max_slot = max_slot.max(a.0 + 1);
                    max_slot = max_slot.max(b.0 + 1);
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::LoadI32Addr { addr, result } => {
                    max_slot = max_slot.max(addr.0 + 1);
                    max_slot = max_slot.max(result.0 + 1);
                }
                LirOp::StoreI32Addr { addr, value } => {
                    max_slot = max_slot.max(addr.0 + 1);
                    max_slot = max_slot.max(value.0 + 1);
                }
                LirOp::LoadList { ptr_result, len_result, .. } => {
                    max_slot = max_slot.max(ptr_result.0 + 1);
                    max_slot = max_slot.max(len_result.0 + 1);
                }
                LirOp::LoadListFromLocal { ptr_result, len_result, .. } => {
                    max_slot = max_slot.max(ptr_result.0 + 1);
                    max_slot = max_slot.max(len_result.0 + 1);
                }
                LirOp::SetSlot { slot, .. } => {
                    max_slot = max_slot.max(slot.0 + 1);
                }
                LirOp::StoreHandle { slot, from } => {
                    max_slot = max_slot.max(slot.0 + 1);
                    max_slot = max_slot.max(from.0 + 1);
                }
                LirOp::StoreI32 { slot, .. } => {
                    max_slot = max_slot.max(slot.0 + 1);
                }
                LirOp::SetAttribute { node, .. } => {
                    max_slot = max_slot.max(node.0 + 1);
                }
                LirOp::AddEventListener { node, .. } => {
                    max_slot = max_slot.max(node.0 + 1);
                }
                _ => {}
            }
        }
        max_slot
    }

    /// Compute the byte offset of a field within a record type.
    ///
    /// Fields are laid out following Canonical ABI:
    /// - Strings and Lists are fat pointers (8 bytes = ptr + len)
    /// - F32, i32, u32, bool, etc. are 4 bytes
    /// - F64, i64, u64 are 8 bytes
    fn compute_field_offset(&self, record_def: crate::ids::DefId, target_field_idx: crate::ids::FieldIdx) -> u32 {
        let rec_def = match self.ctx.defs.as_record(record_def) {
            Some(r) => r,
            None => return 0,
        };

        let mut offset = 0u32;
        for (idx, &field_def_id) in rec_def.fields.iter().enumerate() {
            if idx == target_field_idx.0 as usize {
                return offset;
            }

            let field_ty = match self.ctx.defs.kind(field_def_id) {
                crate::definitions::DefKind::Field(f) => f.ty,
                _ => continue,
            };

            let is_fat_ptr = matches!(
                self.ctx.ty_kind(field_ty),
                InternedTyKind::String | InternedTyKind::List(_)
            );

            if is_fat_ptr {
                offset += 8;
            } else {
                match self.ctx.ty_kind(field_ty) {
                    InternedTyKind::F64 | InternedTyKind::S64 | InternedTyKind::U64 => {
                        offset += 8;
                    }
                    _ => {
                        offset += 4;
                    }
                }
            }
        }

        offset
    }

    fn generate_effects_for_signal_in(
        &mut self,
        func: &mut Function,
        signal_def_id: crate::ids::DefId,
        comp_idx: usize,
        _layout: &MemoryLayout,
    ) -> Result<(), CodegenError> {
        let component = &self.components[comp_idx];
        // Block-based effects: call update_block for each effect triggered by this signal
        // The setter function uses local offset 2 (params: self, value)
        for effect in &component.effects.clone() {
            if effect.dependencies.contains(&signal_def_id) {
                // Call the effect's update block function
                if let Some(&func_idx) = self.block_func_indices.get(&(comp_idx, effect.update_block)) {
                    // Pass a dummy parent (0) - effect blocks typically don't use parent
                    func.instruction(&Instruction::I32Const(0));
                    func.instruction(&Instruction::Call(func_idx));
                }
            }
        }
        Ok(())
    }
    /// Generate record constructor "at" variant that stores at a given address.
    ///
    /// Signature: (dest: i32, ...fields) -> ()
    ///
    /// This is the PRIMARY helper - no locals needed, just uses parameters.
    /// Fields are stored inline at dest, following Canonical ABI layout.
    fn generate_record_ctor_at(&self, record_def: crate::ids::DefId, _alloc_idx: u32) -> Result<Function, CodegenError> {
        let rec_def = self.ctx.defs.as_record(record_def)
            .ok_or_else(|| CodegenError::InvalidIR(format!("Expected record definition for {:?}", record_def)))?;

        // No locals needed - this is the key benefit!
        let mut func = Function::new([]);

        // Parameter indices:
        // 0: dest address
        // 1+: field values (strings/lists take 2 params each)
        let mut param_idx = 1u32;
        let mut offset = 0u32;

        for &field_def_id in &rec_def.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                crate::definitions::DefKind::Field(f) => f.ty,
                _ => continue,
            };

            let is_fat_ptr = matches!(
                self.ctx.ty_kind(field_ty),
                InternedTyKind::String | InternedTyKind::List(_)
            );

            if is_fat_ptr {
                // String/List: use store_fat_ptr helper
                // Stack: [] -> call store_fat_ptr(dest+offset, ptr, len)
                func.instruction(&Instruction::LocalGet(0)); // dest
                if offset > 0 {
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(param_idx)); // ptr
                func.instruction(&Instruction::LocalGet(param_idx + 1)); // len
                let store_fat_ptr_idx = self.runtime_funcs.as_ref()
                    .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?
                    .store_fat_ptr;
                func.instruction(&Instruction::Call(store_fat_ptr_idx));
                param_idx += 2;
                offset += 8;
            } else {
                // Single value field
                func.instruction(&Instruction::LocalGet(0)); // dest
                if offset > 0 {
                    func.instruction(&Instruction::I32Const(offset as i32));
                    func.instruction(&Instruction::I32Add);
                }
                func.instruction(&Instruction::LocalGet(param_idx));

                // Store based on type
                match self.ctx.ty_kind(field_ty) {
                    InternedTyKind::F32 => {
                        func.instruction(&Instruction::F32Store(mem_arg(0, 2)));
                        offset += 4;
                    }
                    InternedTyKind::F64 => {
                        func.instruction(&Instruction::F64Store(mem_arg(0, 3)));
                        offset += 8;
                    }
                    InternedTyKind::S64 | InternedTyKind::U64 => {
                        func.instruction(&Instruction::I64Store(mem_arg(0, 3)));
                        offset += 8;
                    }
                    _ => {
                        func.instruction(&Instruction::I32Store(mem_arg(0, 2)));
                        offset += 4;
                    }
                }
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
    fn generate_record_ctor(&mut self, record_def: crate::ids::DefId, alloc_idx: u32) -> Result<Function, CodegenError> {
        let rec_def = self.ctx.defs.as_record(record_def)
            .ok_or_else(|| CodegenError::InvalidIR(format!("Expected record definition for {:?}", record_def)))?;

        // Need one local for the allocated pointer
        let mut func = Function::new([(1, ValType::I32)]);

        // Calculate total size
        let mut total_size = 0u32;
        for &field_def_id in &rec_def.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                crate::definitions::DefKind::Field(f) => f.ty,
                _ => continue,
            };
            total_size += self.layout_ctx.size_of(field_ty) as u32;
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
        let ctor_at_idx = self.runtime_funcs.as_ref()
            .ok_or_else(|| CodegenError::InvalidIR("Runtime functions not initialized".to_string()))?
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
    fn generate_list_ctor(&mut self, elem_ty: crate::types::Ty, count: usize, alloc_idx: u32) -> Result<Function, CodegenError> {
        use crate::types::InternedTyKind;

        // Get element size and param count
        let elem_size = self.layout_ctx.size_of(elem_ty) as u32;
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
                        for &field_def_id in &rec_def.fields.clone() {
                            let field_ty = match self.ctx.defs.kind(field_def_id) {
                                crate::definitions::DefKind::Field(f) => f.ty,
                                _ => continue,
                            };
                            let field_size = self.layout_ctx.size_of(field_ty) as u32;

                            match self.ctx.ty_kind(field_ty) {
                                InternedTyKind::String | InternedTyKind::List(_) => {
                                    // Fat pointer field
                                    func.instruction(&Instruction::LocalGet(ptr_local));
                                    func.instruction(&Instruction::LocalGet(param_idx));
                                    func.instruction(&Instruction::I32Store(mem_arg((elem_offset + field_offset) as u64, 2)));
                                    func.instruction(&Instruction::LocalGet(ptr_local));
                                    func.instruction(&Instruction::LocalGet(param_idx + 1));
                                    func.instruction(&Instruction::I32Store(mem_arg((elem_offset + field_offset + 4) as u64, 2)));
                                    param_idx += 2;
                                }
                                _ => {
                                    // Simple field
                                    func.instruction(&Instruction::LocalGet(ptr_local));
                                    func.instruction(&Instruction::LocalGet(param_idx));
                                    func.instruction(&Instruction::I32Store(mem_arg((elem_offset + field_offset) as u64, 2)));
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
                    // Primitive: single i32 store
                    func.instruction(&Instruction::LocalGet(ptr_local));
                    func.instruction(&Instruction::LocalGet(param_idx));
                    func.instruction(&Instruction::I32Store(mem_arg(elem_offset as u64, 2)));
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
}
