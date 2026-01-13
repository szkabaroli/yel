//! Direct WASM component generation from LIR.
//!
//! Generates WASM component model components from LIR representation.
//! Uses wasm-encoder to build the binary format.
//!
//! This module is organized into:
//! - `mod.rs` - Public API, types, constants, and main builder
//! - `core_module.rs` - Core WASM module generation (functions, memory, etc.)
//! - `component.rs` - Component model wrapper generation
//! - `runtime/` - Runtime function generation (string ops, memory ops)

mod core_module;
mod expr;
pub mod runtime;

use std::collections::HashMap;

use crate::codegen::CodegenError;
use crate::codegen::wit_ast::WitAstBuilder;
use crate::context::CompilerContext;
use crate::lir::LirLiteral;
use crate::ids::{DefId, LocalId};
use crate::lir::{LirComponent, LirExpr, LirExprKind, SlotId};
use crate::types::Ty;

use self::runtime::{RuntimeFunctions, StringData};

/// Generate a WASM component from all LIR components.
/// Only the exported component(s) will be exposed in the component wrapper,
/// but all components' code is generated in the core module.
///
/// This uses wit-component to generate proper component model wrappers.
/// Uses default options (namespace: "yel", name: "ui", version: "0.1.0").
pub fn generate_wasm(components: &[LirComponent], ctx: &CompilerContext) -> Result<Vec<u8>, CodegenError> {
    generate_wasm_with_wit(components, ctx, &WasmWithWitOptions::default())
}

/// Options for WASM generation with embedded WIT.
pub struct WasmWithWitOptions {
    pub namespace: String,
    pub name: String,
    pub version: String,
}

impl Default for WasmWithWitOptions {
    fn default() -> Self {
        Self {
            namespace: "yel".to_string(),
            name: "ui".to_string(),
            version: "0.1.0".to_string(),
        }
    }
}

/// Generate a WASM component with embedded WIT metadata using wit-component.
///
/// This function:
/// 1. Builds the core WASM module with all component code
/// 2. Builds WIT AST using WitAstBuilder
/// 3. Embeds WIT metadata into the core module
/// 4. Uses ComponentEncoder to produce the final component
///
/// This approach properly handles complex types like `list<record>` by letting
/// wit-component generate the correct component type encoding.
pub fn generate_wasm_with_wit(
    components: &[LirComponent],
    ctx: &CompilerContext,
    options: &WasmWithWitOptions,
) -> Result<Vec<u8>, CodegenError> {
    use wit_component::{ComponentEncoder, StringEncoding};

    // Find the primary exported component
    let exported_component = components
        .iter()
        .find(|c| c.is_export)
        .ok_or_else(|| CodegenError::InvalidIR(
            "No exported component found. Mark at least one component with 'export'.".to_string(),
        ))?;

    // Build WIT AST
    let mut wit_builder = WitAstBuilder::new(ctx, &options.namespace, &options.name, &options.version);
    wit_builder.build_component_wit(exported_component)?;
    let (resolve, world_id) = wit_builder.into_resolve_and_world();

    // Build the core module
    let mut builder = WasmPackageBuilder::new(components, ctx);

    // Set WIT package info for interface-qualified export names
    builder.set_wit_package(&options.namespace, &options.name, &options.version);

    // Pre-intern common strings
    builder.strings.intern("true");
    builder.strings.intern("false");
    builder.strings.intern("");
    builder.strings.intern("[number]");
    builder.strings.intern("[object]");

    // Collect strings and initialize heap
    builder.collect_strings();
    builder.init_heap();

    // Build the core module
    let core_module = builder.build_core_module()?;
    let mut core_module_bytes = core_module.finish();

    // Debug: save core module for inspection (ignore errors in WASM/sandbox environments)
    if let Err(e) = std::fs::write("/tmp/debug_core_module.wasm", &core_module_bytes) {
        eprintln!("Note: Could not write debug core module: {}", e);
    }

    // Embed WIT metadata into the core module (modifies in place)
    wit_component::embed_component_metadata(
        &mut core_module_bytes,
        &resolve,
        world_id,
        StringEncoding::UTF8,
    ).map_err(|e| CodegenError::EncodingError(format!("Failed to embed WIT metadata: {}", e)))?;

    // Debug: save module with embedded metadata for inspection
    if let Err(e) = std::fs::write("/tmp/debug_module_with_metadata.wasm", &core_module_bytes) {
        eprintln!("Note: Could not write debug module with metadata: {}", e);
    }

    // Use ComponentEncoder to produce the final component
    let encoder = ComponentEncoder::default()
        .module(&core_module_bytes)
        .map_err(|e| {
            CodegenError::EncodingError(format!("Failed to set module: {}", e))
        })?;

    let component_bytes = encoder
        .validate(true)
        .encode()
        .map_err(|e| {
            CodegenError::EncodingError(format!("Failed to encode component: {}", e))
        })?;

    Ok(component_bytes)
}

// ============================================================================
// Types
// ============================================================================

/// Memory layout for a component (computed from block-based slots).
#[derive(Clone)]
pub(crate) struct MemoryLayout {
    /// Base offset in memory for this component
    pub base: i32,
    /// Offset for each signal (relative to base)
    pub signal_offsets: Vec<i32>,
    /// Total size used
    #[allow(dead_code)]
    pub size: i32,
}

impl MemoryLayout {
    pub fn new(component: &LirComponent, base: i32, layout_ctx: &mut crate::lir::LayoutContext) -> Self {
        use crate::lir::SlotKind;

        let mut offset = 0i32;

        // Signals - use proper type sizes from layout
        let signal_offsets: Vec<i32> = component
            .signals
            .iter()
            .map(|signal| {
                let o = offset;
                let size = layout_ctx.size_of(signal.ty) as i32;
                offset += size;
                o
            })
            .collect();

        // Memory slots are pre-computed in component.slots
        // Find max offset to get total size
        for slot in &component.slots {
            if let SlotKind::Memory { offset: slot_offset, size } = &slot.kind {
                let end = (*slot_offset + *size) as i32;
                if end > offset {
                    offset = end;
                }
            }
        }

        MemoryLayout {
            base,
            signal_offsets,
            size: offset,
        }
    }

    pub fn signal_addr(&self, idx: usize) -> i32 {
        self.base + self.signal_offsets[idx]
    }
}

// ============================================================================
// Constants - Import function indices (must match order in build_core_module)
// ============================================================================

pub(crate) const IMPORT_CREATE_ELEMENT: u32 = 0;
pub(crate) const IMPORT_CREATE_TEXT: u32 = 1;
#[allow(dead_code)]
pub(crate) const IMPORT_CREATE_COMMENT: u32 = 2;
pub(crate) const IMPORT_SET_ATTRIBUTE: u32 = 3;
#[allow(dead_code)]
pub(crate) const IMPORT_REMOVE_ATTRIBUTE: u32 = 4;
pub(crate) const IMPORT_SET_TEXT_CONTENT: u32 = 5;
#[allow(dead_code)]
pub(crate) const IMPORT_SET_STYLE: u32 = 6;
#[allow(dead_code)]
pub(crate) const IMPORT_SET_CLASS: u32 = 7;
pub(crate) const IMPORT_APPEND_CHILD: u32 = 8;
#[allow(dead_code)]
pub(crate) const IMPORT_INSERT_BEFORE: u32 = 9;
#[allow(dead_code)]
pub(crate) const IMPORT_REMOVE_CHILD: u32 = 10;
pub(crate) const IMPORT_REMOVE: u32 = 11;
#[allow(dead_code)]
pub(crate) const IMPORT_GET_PARENT: u32 = 12;
#[allow(dead_code)]
pub(crate) const IMPORT_GET_NEXT_SIBLING: u32 = 13;
pub(crate) const IMPORT_ADD_EVENT_LISTENER: u32 = 14;
#[allow(dead_code)]
pub(crate) const IMPORT_REMOVE_EVENT_LISTENER: u32 = 15;
/// Insert node after anchor (for conditional rendering).
/// Signature: insert_after(parent: i32, node: i32, anchor: i32) -> ()
/// Semantically: parent.insertBefore(node, anchor.nextSibling)
pub(crate) const IMPORT_INSERT_AFTER: u32 = 16;
// After DOM imports (17 total), callbacks are imported dynamically
// Then: resource-new, resource-drop, realloc
pub(crate) const NUM_DOM_IMPORTS: u32 = 17;

/// Import indices for a single component's callbacks and resource intrinsics
#[derive(Debug, Clone)]
pub(crate) struct ComponentCallbackLayout {
    /// Index of the first callback import for this component
    pub first_callback: u32,
    /// DefIds of the callbacks (in order)
    pub callback_def_ids: Vec<crate::ids::DefId>,
    /// Index of [resource-new]component import (for constructor return)
    pub resource_new: u32,
}

/// Import layout - tracks imports for all exported components
#[derive(Debug, Clone)]
pub(crate) struct ImportLayout {
    /// Callback layouts for each exported component (in order)
    pub components: Vec<ComponentCallbackLayout>,
    /// Total number of imports
    pub num_imports: u32,
}

/// Local function indices for allocator functions (defined in core module, not imported)
#[derive(Debug, Clone, Copy)]
pub(crate) struct AllocatorFuncs {
    /// Index of the alloc function
    pub alloc: u32,
    /// Index of the free function
    pub free: u32,
    /// Index of the cabi_realloc function
    pub cabi_realloc: u32,
}

impl ImportLayout {
    /// Calculate import layout for exported components
    pub fn new(exported_components: &[&LirComponent], ctx: &crate::context::CompilerContext) -> Self {
        let mut current_idx = NUM_DOM_IMPORTS;
        let mut components = Vec::new();

        for component in exported_components {
            // Get callbacks from component definition
            let comp_def = ctx.defs.as_component(component.def_id);
            let callbacks: Vec<crate::ids::DefId> = comp_def
                .map(|c| {
                    c.callbacks
                        .iter()
                        .filter(|&def_id| {
                            ctx.defs.as_function(*def_id).map(|f| f.is_export).unwrap_or(false)
                        })
                        .copied()
                        .collect()
                })
                .unwrap_or_default();

            let num_callbacks = callbacks.len() as u32;
            let first_callback = current_idx;
            current_idx += num_callbacks;

            // [resource-new]component import comes after callbacks
            let resource_new = current_idx;
            current_idx += 1;

            components.push(ComponentCallbackLayout {
                first_callback,
                callback_def_ids: callbacks,
                resource_new,
            });
        }

        // Note: allocator functions are LOCAL (not imported)
        let num_imports = current_idx;

        Self {
            components,
            num_imports,
        }
    }

    /// Find the callback index for a given DefId
    pub fn find_callback_index(&self, def_id: crate::ids::DefId) -> Option<u32> {
        for comp_layout in &self.components {
            for (i, &cb_def_id) in comp_layout.callback_def_ids.iter().enumerate() {
                if cb_def_id == def_id {
                    return Some(comp_layout.first_callback + i as u32);
                }
            }
        }
        None
    }
}

// ============================================================================
// Builder
// ============================================================================

/// Builder for WASM package (component) generation.
pub(crate) struct WasmPackageBuilder<'a> {
    /// All components (code is generated for all of them)
    pub components: &'a [LirComponent],
    pub ctx: &'a CompilerContext,
    /// String data manager for literal interning
    pub strings: StringData,
    /// First free address after string data (compile-time heap base)
    heap_base: u32,
    /// Current bump pointer for compile-time allocations
    heap_ptr: u32,
    /// Layout context for type size/alignment queries.
    pub layout_ctx: crate::lir::LayoutContext<'a>,
    /// Import layout (set during build_core_module, used for callback lookups)
    pub import_layout: Option<ImportLayout>,
    /// Allocator function indices (set during build_core_module)
    pub alloc_funcs: Option<AllocatorFuncs>,
    /// Runtime function indices (set during build_core_module)
    pub runtime_funcs: Option<RuntimeFunctions>,
    /// Required concat arities (collected during string collection pass)
    pub concat_arities: Vec<usize>,
    /// Record types that need constructor helpers (collected during expression pass)
    pub record_types: Vec<DefId>,
    /// Global handler counter for event handler registration/dispatch
    pub handler_counter: usize,
    /// Block function index mapping: (component_idx, block_id) -> wasm_func_idx
    pub block_func_indices: std::collections::HashMap<(usize, crate::lir::BlockId), u32>,
    /// Memory layouts by component index
    pub layouts: Vec<MemoryLayout>,
    /// WIT package info for interface-qualified export names (namespace, name, version)
    pub wit_package: Option<(String, String, String)>,
    /// Current block's local variable offset (for block functions)
    pub current_block_local_offset: Option<u32>,
    /// Mapping from LocalId to slot index for captured locals in current block
    pub current_block_captured_locals: Option<HashMap<LocalId, u32>>,
    /// Mapping from LocalId to SlotId for inline-computed locals (e.g., for-loop items)
    pub current_block_local_to_slot: Option<HashMap<LocalId, SlotId>>,
    /// List construct info: (element_type, element_count) for runtime function generation
    pub list_constructs: Vec<(Ty, usize)>,
}

impl<'a> WasmPackageBuilder<'a> {
    /// String data base offset (after reserved memory for conversion buffers).
    /// Memory layout:
    /// - 0x0000-0x001F: s32_to_string buffer (32 bytes)
    /// - 0x0020-0x00FF: Reserved for future conversion buffers
    /// - 0x0100+: String data section
    const STRING_DATA_BASE: u32 = 256;

    pub fn new(components: &'a [LirComponent], ctx: &'a CompilerContext) -> Self {
        Self {
            components,
            ctx,
            strings: StringData::new(Self::STRING_DATA_BASE),
            heap_base: 0,
            heap_ptr: 0,
            layout_ctx: crate::lir::LayoutContext::new(ctx),
            import_layout: None,
            alloc_funcs: None,
            runtime_funcs: None,
            concat_arities: Vec::new(),
            record_types: Vec::new(),
            handler_counter: 0,
            block_func_indices: std::collections::HashMap::new(),
            layouts: Vec::new(),
            wit_package: None,
            current_block_local_offset: None,
            current_block_captured_locals: None,
            current_block_local_to_slot: None,
            list_constructs: Vec::new(),
        }
    }

    /// Set the WIT package info for interface-qualified export names
    pub fn set_wit_package(&mut self, namespace: &str, name: &str, version: &str) {
        self.wit_package = Some((namespace.to_string(), name.to_string(), version.to_string()));
    }

    /// Reset handler counter (call before generating each component's functions)
    pub fn reset_handler_counter(&mut self) {
        self.handler_counter = 0;
    }

    /// Get exported components
    pub fn get_exported_components(&self) -> Vec<&LirComponent> {
        self.components.iter().filter(|c| c.is_export).collect()
    }

    /// Initialize heap after collecting all strings.
    /// Heap starts after string data, aligned to 8 bytes.
    pub(crate) fn init_heap(&mut self) {
        let string_end = self.strings.base() + self.strings.size();
        self.heap_base = crate::lir::align_to(string_end, 8);
        self.heap_ptr = self.heap_base;
    }

    // ========================================================================
    // String handling
    // ========================================================================

    pub(crate) fn collect_strings(&mut self) {
        // Collect strings from all components
        // LirComponent now has pre-computed strings, so we just copy them
        for component in self.components {
            // Copy pre-interned strings from component
            for s in &component.strings {
                self.add_string(s);
            }

            // Collect strings, concat arities, record types, and list constructs from signal defaults
            for signal in &component.signals {
                if let Some(default_expr) = &signal.default {
                    self.collect_strings_from_expr(default_expr);
                    self.collect_concat_arities(default_expr);
                    self.collect_record_types(default_expr);
                    self.collect_list_constructs(default_expr);
                }
            }

            // Collect strings, concat arities, record types, and list constructs from pre-lowered expressions
            // IMPORTANT: Must collect strings here so layout calculation includes them
            for expr in &component.exprs {
                self.collect_strings_from_expr(expr);
                self.collect_concat_arities(expr);
                self.collect_record_types(expr);
                self.collect_list_constructs(expr);
            }
        }
    }

    /// Collect literal strings from an expression for the data section.
    /// Also tracks concat arities for runtime function generation.
    fn collect_strings_from_expr(&mut self, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::Literal(LirLiteral::String(s)) => {
                self.add_string(s);
            }
            LirExprKind::Call { func, args } => {
                // Check if this is a concat call and track its arity
                let func_name = self.ctx.str(self.ctx.defs.name(*func));
                if func_name == "concat" && args.len() >= 2 {
                    self.concat_arities.push(args.len());
                }
                for arg in args {
                    self.collect_strings_from_expr(arg);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_strings_from_expr(lhs);
                self.collect_strings_from_expr(rhs);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_strings_from_expr(operand);
            }
            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.collect_strings_from_expr(condition);
                self.collect_strings_from_expr(then_expr);
                self.collect_strings_from_expr(else_expr);
            }
            LirExprKind::Field { base, .. } => {
                self.collect_strings_from_expr(base);
            }
            LirExprKind::Index { base, index } => {
                self.collect_strings_from_expr(base);
                self.collect_strings_from_expr(index);
            }
            LirExprKind::ListConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_strings_from_expr(elem);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for field in fields {
                    self.collect_strings_from_expr(field);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_strings_from_expr(elem);
                }
            }
            _ => {}
        }
    }

    /// Collect concat arities from an expression (for runtime function generation).
    fn collect_concat_arities(&mut self, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::Call { func, args } => {
                let func_name = self.ctx.str(self.ctx.defs.name(*func));
                if func_name == "concat" && args.len() >= 2 {
                    self.concat_arities.push(args.len());
                }
                for arg in args {
                    self.collect_concat_arities(arg);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_concat_arities(lhs);
                self.collect_concat_arities(rhs);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_concat_arities(operand);
            }
            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.collect_concat_arities(condition);
                self.collect_concat_arities(then_expr);
                self.collect_concat_arities(else_expr);
            }
            LirExprKind::Field { base, .. } => {
                self.collect_concat_arities(base);
            }
            LirExprKind::Index { base, index } => {
                self.collect_concat_arities(base);
                self.collect_concat_arities(index);
            }
            LirExprKind::ListConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_concat_arities(elem);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for field in fields {
                    self.collect_concat_arities(field);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_concat_arities(elem);
                }
            }
            // Leaf expressions with no sub-expressions
            LirExprKind::SignalRead(_) |
            LirExprKind::Local(_) |
            LirExprKind::Def(_) |
            LirExprKind::Literal(_) |
            LirExprKind::EnumCase { .. } |
            LirExprKind::VariantCtor { .. } |
            LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Collect record types that need constructor helpers.
    /// These are record types used in RecordConstruct expressions.
    fn collect_record_types(&mut self, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::RecordConstruct { record_def, fields, .. } => {
                // Add this record type if not already present
                if !self.record_types.contains(record_def) {
                    self.record_types.push(*record_def);
                }
                // Recurse into field expressions (may contain nested records)
                for field in fields {
                    self.collect_record_types(field);
                }
            }
            LirExprKind::ListConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_record_types(elem);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_record_types(elem);
                }
            }
            LirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_record_types(arg);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_record_types(lhs);
                self.collect_record_types(rhs);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_record_types(operand);
            }
            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.collect_record_types(condition);
                self.collect_record_types(then_expr);
                self.collect_record_types(else_expr);
            }
            LirExprKind::Field { base, .. } => {
                self.collect_record_types(base);
            }
            LirExprKind::Index { base, index } => {
                self.collect_record_types(base);
                self.collect_record_types(index);
            }
            // Leaf expressions
            LirExprKind::SignalRead(_) |
            LirExprKind::Local(_) |
            LirExprKind::Def(_) |
            LirExprKind::Literal(_) |
            LirExprKind::EnumCase { .. } |
            LirExprKind::VariantCtor { .. } |
            LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Collect list constructs from an expression (for runtime function generation).
    /// These are list literals that need runtime constructor helpers.
    fn collect_list_constructs(&mut self, expr: &LirExpr) {
        use crate::types::InternedTyKind;

        match &expr.kind {
            LirExprKind::ListConstruct { elements, .. } => {
                // Get element type from the list type
                let element_ty = match self.ctx.ty_kind(expr.ty) {
                    InternedTyKind::List(elem_ty) => *elem_ty,
                    _ => return, // Not a list type
                };

                let count = elements.len();
                let key = (element_ty, count);

                // Add if not already present
                if !self.list_constructs.contains(&key) {
                    self.list_constructs.push(key);
                }

                // Recurse into elements
                for elem in elements {
                    self.collect_list_constructs(elem);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for field in fields {
                    self.collect_list_constructs(field);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_list_constructs(elem);
                }
            }
            LirExprKind::Call { args, .. } => {
                for arg in args {
                    self.collect_list_constructs(arg);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_list_constructs(lhs);
                self.collect_list_constructs(rhs);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_list_constructs(operand);
            }
            LirExprKind::Ternary { condition, then_expr, else_expr } => {
                self.collect_list_constructs(condition);
                self.collect_list_constructs(then_expr);
                self.collect_list_constructs(else_expr);
            }
            LirExprKind::Field { base, .. } => {
                self.collect_list_constructs(base);
            }
            LirExprKind::Index { base, index } => {
                self.collect_list_constructs(base);
                self.collect_list_constructs(index);
            }
            // Leaf expressions
            LirExprKind::SignalRead(_) |
            LirExprKind::Local(_) |
            LirExprKind::Def(_) |
            LirExprKind::Literal(_) |
            LirExprKind::EnumCase { .. } |
            LirExprKind::VariantCtor { .. } |
            LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Count the total number of WASM parameters needed to pass all fields of a record.
    /// Strings and lists need 2 params (ptr + len), other types need 1.
    pub fn count_record_wasm_params(&self, record_def: DefId) -> usize {
        use crate::types::InternedTyKind;

        let rec_def = match self.ctx.defs.as_record(record_def) {
            Some(r) => r,
            None => return 0,
        };

        let mut count = 0;
        for &field_def_id in &rec_def.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                crate::definitions::DefKind::Field(f) => f.ty,
                _ => continue,
            };

            match self.ctx.ty_kind(field_ty) {
                InternedTyKind::String | InternedTyKind::List(_) => {
                    count += 2; // ptr + len
                }
                _ => {
                    count += 1;
                }
            }
        }
        count
    }

    /// Count the number of WASM parameters needed to pass a value of the given type.
    /// - Primitives (s32, bool, etc.): 1 param
    /// - Strings and lists: 2 params (ptr + len)
    /// - Records: sum of field params (flattened)
    pub fn count_type_wasm_params(&self, ty: Ty) -> usize {
        use crate::types::InternedTyKind;

        match self.ctx.ty_kind(ty) {
            InternedTyKind::String | InternedTyKind::List(_) => 2, // ptr + len
            InternedTyKind::Adt(def_id) => {
                // Check if it's a record
                if self.ctx.defs.as_record(*def_id).is_some() {
                    self.count_record_wasm_params(*def_id)
                } else {
                    1 // Enums, variants - single i32
                }
            }
            _ => 1, // Primitives
        }
    }

    /// Add/intern a string literal, returning (ptr, len).
    pub fn add_string(&mut self, s: &str) -> (u32, u32) {
        self.strings.intern(s)
    }

    /// Get string info if already interned.
    pub fn get_string_info(&self, s: &str) -> Option<(u32, u32)> {
        self.strings.get(s)
    }

    /// Get signal name by DefId
    pub fn signal_name(&self, def_id: DefId) -> String {
        let name = self.ctx.defs.name(def_id);
        self.ctx.str(name).to_string()
    }

    /// Get signal index by DefId within a specific component
    pub fn signal_index_in(&self, component: &LirComponent, def_id: DefId) -> Option<usize> {
        component.signals.iter().position(|s| s.def_id == def_id)
    }
}

// ============================================================================
// Utility functions
// ============================================================================

pub(crate) fn to_kebab_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('-');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result.replace('_', "-")
}

/// Convert a name to a valid WIT identifier.
///
/// WIT identifiers must be kebab-case where each segment starts with a letter.
/// This function handles cases like `item-8` by converting to `item-n8`.
pub(crate) fn to_wit_name(s: &str) -> String {
    let kebab = to_kebab_case(s);

    // Split by hyphens and ensure each segment starts with a letter
    let segments: Vec<String> = kebab
        .split('-')
        .map(|seg| {
            if seg.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                // Segment starts with digit, prefix with 'n'
                format!("n{}", seg)
            } else {
                seg.to_string()
            }
        })
        .collect();

    segments.join("-")
}

/// Get the WASM type for a Ty.
pub(crate) fn ty_to_wasm_valtype(ty: Ty, ctx: &CompilerContext) -> wasm_encoder::ValType {
    use crate::types::InternedTyKind;
    use wasm_encoder::ValType;

    match ctx.ty_kind(ty) {
        InternedTyKind::Bool => ValType::I32,
        InternedTyKind::S8 | InternedTyKind::S16 | InternedTyKind::S32 => ValType::I32,
        InternedTyKind::U8 | InternedTyKind::U16 | InternedTyKind::U32 => ValType::I32,
        InternedTyKind::S64 | InternedTyKind::U64 => ValType::I64,
        InternedTyKind::F32 => ValType::F32,
        InternedTyKind::F64 => ValType::F64,
        InternedTyKind::Char => ValType::I32,
        InternedTyKind::String => ValType::I32, // pointer
        InternedTyKind::List(_) => ValType::I32, // pointer
        InternedTyKind::Option(_) => ValType::I32, // discriminant + value
        InternedTyKind::Result { .. } => ValType::I32,
        InternedTyKind::Tuple(_) => ValType::I32, // pointer
        InternedTyKind::Adt(_) => ValType::I32, // pointer
        InternedTyKind::Length | InternedTyKind::PhysicalLength => ValType::F32,
        InternedTyKind::Angle | InternedTyKind::Duration | InternedTyKind::Percent => ValType::F32,
        InternedTyKind::RelativeFontSize => ValType::F32,
        InternedTyKind::Color => ValType::I32, // packed RGBA
        InternedTyKind::Brush | InternedTyKind::Image | InternedTyKind::Easing => ValType::I32,
        InternedTyKind::Func { .. } => ValType::I32,
        InternedTyKind::Error | InternedTyKind::Unknown | InternedTyKind::Unit => ValType::I32,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_kebab_case() {
        assert_eq!(to_kebab_case("Counter"), "counter");
        assert_eq!(to_kebab_case("MyComponent"), "my-component");
        assert_eq!(to_kebab_case("some_name"), "some-name");
    }

    #[test]
    fn test_to_wit_name() {
        assert_eq!(to_wit_name("counter"), "counter");
        assert_eq!(to_wit_name("item8"), "item8");
        assert_eq!(to_wit_name("8item"), "n8item");
    }
}
