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

mod codegen;
mod expr;
pub mod functions;
pub(crate) mod gc_types;
pub(super) mod repr;
pub mod runtime;

use std::collections::HashMap;
use std::error::Error as _;
use std::fmt::Write;

use super::CodegenError;
use super::wit_ast::WitAstBuilder;
use wasmparser::{CompositeInnerType, FuncType, Parser, Payload, TypeRef};
use wit_component::ComponentEncoder;
use wit_component::{StringEncoding, dummy_module};
use wit_parser::{ManglingAndAbi, Resolve, WorldId};
use yel_core::context::CompilerContext;
use yel_core::ids::{BlockId, DefId, LocalId};
use yel_core::lir::{LirBindingMode, LirLayoutContext, LirLiteral, LirSlotKind, align_to};
use yel_core::lir::{LirExpr, LirExprKind, LirModule, LirResource, LirSlotId};
use yel_core::types::Ty;
use yel_core::{definitions::DefKind, types::InternedTyKind};

use self::runtime::{RuntimeFunctions, StringData};

/// Info about which core-module function contains a given byte offset.
struct FuncLoc {
    /// Function index within the code section (local function index, does NOT
    /// include imported functions). Add the import count to cross-reference
    /// the `(func $name (;N;) …)` numbers in the printed WAT.
    func_index: u32,
    /// Type index (points into the type section).
    type_index: Option<u32>,
    /// Function signature rendered as text (e.g. `(i32) -> i32`).
    signature: Option<String>,
    body_start: usize,
    body_end: usize,
    /// Hex dump of a handful of bytes around the failing offset.
    hex_context: String,
}

/// Parse `msg` and return the last byte offset mentioned — the encoder error
/// chain typically ends with `... (at offset 0xd55)`.
fn extract_last_offset(msg: &str) -> Option<usize> {
    let marker = "offset 0x";
    let idx = msg.rfind(marker)?;
    let tail = &msg[idx + marker.len()..];
    let end = tail
        .find(|c: char| !c.is_ascii_hexdigit())
        .unwrap_or(tail.len());
    usize::from_str_radix(&tail[..end], 16).ok()
}

/// Build a diagnostic suffix: function types, function imports, and (if the
/// error message contains a byte offset) the function body at that offset.
fn augment_with_context(bytes: &[u8], msg: &str) -> String {
    let mut out = String::new();
    if let Some(types) = list_function_types(bytes) {
        out.push_str("\n  function types:");
        for (i, sig) in types.iter().enumerate() {
            out.push_str(&format!("\n    type {} = {}", i, sig));
        }
    }
    if let Some(imports) = list_function_imports(bytes) {
        out.push_str("\n  function imports:");
        for (i, (module, name, type_idx)) in imports.iter().enumerate() {
            out.push_str(&format!(
                "\n    #{} {}/{} (type {})",
                i, module, name, type_idx,
            ));
        }
    }
    if let Some(offset) = extract_last_offset(msg)
        && let Some(info) = locate_function_at_offset(bytes, offset)
    {
        out.push_str(&format!(
            "\n  in core func #{} (body bytes 0x{:x}..0x{:x})",
            info.func_index, info.body_start, info.body_end,
        ));
        if let Some(sig) = &info.signature {
            out.push_str(&format!(
                "\n  signature: {}{}",
                sig,
                info.type_index
                    .map(|t| format!(" (type {})", t))
                    .unwrap_or_default(),
            ));
        }
        out.push_str(&format!("\n  full body hex:\n    {}", info.hex_context));
        if let Some(wat) = print_single_function_wat(bytes, info.func_index) {
            out.push_str("\n  --- function WAT ---\n");
            out.push_str(&wat);
            out.push_str("\n  --- end function WAT ---");
        }
    }
    out
}

/// List every declared function type as a human-readable signature string.
fn list_function_types(bytes: &[u8]) -> Option<Vec<String>> {
    let mut out: Vec<String> = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        if let Ok(Payload::TypeSection(reader)) = payload {
            for rec_group in reader {
                let rec_group = rec_group.ok()?;
                for sub in rec_group.into_types() {
                    if let CompositeInnerType::Func(ft) = sub.composite_type.inner {
                        let mut s = String::from("(");
                        for (i, t) in ft.params().iter().enumerate() {
                            if i > 0 {
                                s.push_str(", ");
                            }
                            let _ = write!(s, "{}", t);
                        }
                        s.push_str(") -> (");
                        for (i, t) in ft.results().iter().enumerate() {
                            if i > 0 {
                                s.push_str(", ");
                            }
                            let _ = write!(s, "{}", t);
                        }
                        s.push(')');
                        out.push(s);
                    }
                }
            }
            return Some(out);
        }
    }
    None
}

/// List every function import in order. Returns (module, name, type_index).
fn list_function_imports(bytes: &[u8]) -> Option<Vec<(String, String, u32)>> {
    let mut out = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        if let Ok(Payload::ImportSection(reader)) = payload {
            for import in reader.into_imports() {
                if let Ok(imp) = import
                    && let TypeRef::Func(type_idx) = imp.ty
                {
                    out.push((imp.module.to_string(), imp.name.to_string(), type_idx));
                }
            }
            return Some(out);
        }
    }
    None
}

/// Pretty-print just one function body from `bytes`. Uses wasmprinter's full
/// module printer and then extracts the matching `(func (;N;) …)` block via
/// text search, which is simpler and lazier than building a custom printer.
fn print_single_function_wat(bytes: &[u8], func_index: u32) -> Option<String> {
    let wat = wasmprinter::print_bytes(bytes).ok()?;
    // wasmprinter prints local functions after imports; compute absolute
    // index in the module's function index space.
    let mut num_imports: u32 = 0;
    for payload in Parser::new(0).parse_all(bytes) {
        if let Ok(Payload::ImportSection(reader)) = payload {
            for import in reader.into_imports() {
                if let Ok(imp) = import
                    && matches!(imp.ty, TypeRef::Func(_))
                {
                    num_imports += 1;
                }
            }
            break;
        }
    }
    let abs_index = func_index + num_imports;
    let needle = format!("(;{};)", abs_index);
    let start = wat.find(&needle)?;
    // Walk back to the enclosing `(func …` opening paren.
    let func_start = wat[..start].rfind("(func ")?;
    // Then find the matching close paren by counting depth.
    let mut depth = 0usize;
    let rest = &wat[func_start..];
    let mut in_string = false;
    let mut last = 0usize;
    for (i, c) in rest.char_indices() {
        match c {
            '"' => in_string = !in_string,
            '(' if !in_string => depth += 1,
            ')' if !in_string => {
                depth -= 1;
                if depth == 0 {
                    last = i + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    if last == 0 {
        return None;
    }
    Some(rest[..last].to_string())
}

/// Walk the code section and find the function whose body contains `offset`.
/// Returns `None` if the module is malformed or no function covers the offset.
fn locate_function_at_offset(bytes: &[u8], offset: usize) -> Option<FuncLoc> {
    // Collect type section (FuncTypes) and function section (type indices)
    // as we scan so we can annotate the result.
    let mut func_types: Vec<FuncType> = Vec::new();
    let mut func_type_indices: Vec<u32> = Vec::new();
    let mut func_index: u32 = 0;

    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload.ok()?;
        match payload {
            Payload::TypeSection(reader) => {
                for rec_group in reader {
                    let rec_group = rec_group.ok()?;
                    for sub in rec_group.into_types() {
                        if let CompositeInnerType::Func(ft) = sub.composite_type.inner {
                            func_types.push(ft);
                        }
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                for i in reader.into_iter().flatten() {
                    func_type_indices.push(i);
                }
            }
            Payload::CodeSectionEntry(body) => {
                let range = body.range();
                if range.start <= offset && offset < range.end {
                    let type_index = func_type_indices.get(func_index as usize).copied();
                    let signature =
                        type_index
                            .and_then(|ti| func_types.get(ti as usize))
                            .map(|ft| {
                                let mut s = String::from("(");
                                for (i, t) in ft.params().iter().enumerate() {
                                    if i > 0 {
                                        s.push_str(", ");
                                    }
                                    let _ = write!(s, "{}", t);
                                }
                                s.push_str(") -> (");
                                for (i, t) in ft.results().iter().enumerate() {
                                    if i > 0 {
                                        s.push_str(", ");
                                    }
                                    let _ = write!(s, "{}", t);
                                }
                                s.push(')');
                                s
                            });
                    // Dump the entire function body as hex with the failing
                    // offset bracketed. Clearer than a narrow window when the
                    // real bug is a mis-encoded instruction earlier on.
                    let mut hex = String::new();
                    for (i, b) in bytes[range.start..range.end].iter().enumerate() {
                        let abs = range.start + i;
                        if i > 0 && i % 32 == 0 {
                            hex.push('\n');
                            hex.push_str("    ");
                        }
                        if abs == offset {
                            hex.push('[');
                        }
                        let _ = write!(hex, "{:02x}", b);
                        if abs == offset {
                            hex.push(']');
                        }
                        hex.push(' ');
                    }
                    return Some(FuncLoc {
                        func_index,
                        type_index,
                        signature,
                        body_start: range.start,
                        body_end: range.end,
                        hex_context: hex,
                    });
                }
                func_index += 1;
            }
            _ => {}
        }
    }
    None
}

/// Generate a WASM component from a list of LIR components.
///
/// Legacy entry — prefer [`generate_wasm_module`] for new code. Wraps the
/// slice into an anonymous `LirModule` and delegates.
pub fn generate_wasm(
    components: &[LirResource],
    ctx: &CompilerContext,
) -> Result<Vec<u8>, CodegenError> {
    generate_wasm_with_wit(components, ctx, &WasmWithWitOptions::default())
}

/// Legacy entry that accepts a component slice plus `options.global_defaults`.
/// Prefer [`generate_wasm_module`]. Kept as a shim while callers migrate.
pub fn generate_wasm_with_wit(
    components: &[LirResource],
    ctx: &CompilerContext,
    options: &WasmWithWitOptions,
) -> Result<Vec<u8>, CodegenError> {
    let module = LirModule {
        components: components.to_vec(),
        global_defaults: options.global_defaults.clone(),
        package: None,
    };
    generate_wasm_module(&module, ctx, options)
}

/// Options for WASM generation with embedded WIT.
pub struct WasmWithWitOptions {
    pub namespace: String,
    pub name: String,
    pub version: String,
    /// LIR-lowered default expressions for global singleton properties. The
    /// module start function stores them to each property's backing slot
    /// before any export runs.
    pub global_defaults: HashMap<DefId, LirExpr>,
    /// Optional Binaryen `wasm-opt` invocation. When `Some`, the core
    /// module bytes are piped through the `wasm-opt` binary on `PATH`
    /// after build but before WIT-metadata embedding. The contained
    /// args are forwarded verbatim — typical examples: `-O3`,
    /// `--enable-gc`, `--enable-reference-types`, `--type-merging`.
    pub wasm_opt_args: Option<Vec<String>>,
}

impl Default for WasmWithWitOptions {
    fn default() -> Self {
        Self {
            namespace: "yel".to_string(),
            name: "ui".to_string(),
            version: "0.1.0".to_string(),
            global_defaults: HashMap::new(),
            wasm_opt_args: None,
        }
    }
}

/// Generate a WASM component with embedded WIT metadata using wit-component.
///
/// This is the primary codegen entry point — it consumes a whole
/// `LirModule` (components, global defaults, package header) rather than a
/// loose slice of components plus side tables.
///
/// 1. Builds the core WASM module with all component code
/// 2. Builds WIT AST using WitAstBuilder
/// 3. Embeds WIT metadata into the core module
/// 4. Uses ComponentEncoder to produce the final component
pub fn generate_wasm_module(
    module: &LirModule,
    ctx: &CompilerContext,
    options: &WasmWithWitOptions,
) -> Result<Vec<u8>, CodegenError> {
    let mut wit_builder =
        WitAstBuilder::new(ctx, &options.namespace, &options.name, &options.version);
    let exported: Vec<&LirResource> = module.exported_components().collect();
    let all: Vec<&LirResource> = module.components.iter().collect();
    wit_builder.build_wit_with_all(&exported, &all)?;
    let (resolve, world_id) = wit_builder.into_resolve_and_world();
    generate_wasm_module_with_wit(module, ctx, options, resolve, world_id)
}

/// Like [`generate_wasm_module`] but the caller supplies a pre-built
/// `(Resolve, WorldId)` pair instead of letting yel-wasm-codegen derive
/// one from the LIR. Used by `yel-flow-core` to emit components whose
/// WIT is authored directly from a `WireModule` tree (interfaces,
/// resources, fields synthesised to get/set methods) rather than
/// projected from UI-flavoured LIR resources.
pub fn generate_wasm_module_with_wit(
    module: &LirModule,
    ctx: &CompilerContext,
    options: &WasmWithWitOptions,
    resolve: Resolve,
    world_id: WorldId,
) -> Result<Vec<u8>, CodegenError> {
    // Zero-component modules (e.g. globals-only libraries) emit a real core
    // module — allocator, memory, start function that seeds global defaults —
    // not a dummy stub. The only case we still stub is truly empty modules
    // with no state worth initializing.
    let has_module_state = !module.components.is_empty() || !module.global_defaults.is_empty();
    if !has_module_state {
        let dummy = dummy_module(&resolve, world_id, ManglingAndAbi::Standard32);
        let mut dummy_bytes = dummy;
        wit_component::embed_component_metadata(
            &mut dummy_bytes,
            &resolve,
            world_id,
            StringEncoding::UTF8,
        )
        .map_err(|e| CodegenError::EncodingError(format!("Failed to embed WIT metadata: {}", e)))?;
        let encoder = ComponentEncoder::default()
            .module(&dummy_bytes)
            .map_err(|e| CodegenError::EncodingError(format!("Failed to set module: {}", e)))?;
        return encoder.validate(true).encode().map_err(|e| {
            CodegenError::EncodingError(format!("Failed to encode component: {}", e))
        });
    }

    // Build the core module
    let mut builder = WasmPackageBuilder::new(&module.components, ctx);

    // Set WIT package info for interface-qualified export names
    builder.set_wit_package(&options.namespace, &options.name, &options.version);

    // Seed global singleton defaults — the start function emits these.
    builder.set_global_defaults(module.global_defaults.clone());

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

    // Opt-in debug dumps + pre-validation. Off by default — the test harness
    // compiles dozens of fixtures per run and the /tmp writes race, and the
    // pre-validator duplicates work that ComponentEncoder::validate already
    // does below. Set YEL_DEBUG_WASM=1 to enable when chasing a validator error.
    #[cfg(not(target_family = "wasm"))]
    let debug_wasm = std::env::var_os("YEL_DEBUG_WASM").is_some();
    #[cfg(target_family = "wasm")]
    let debug_wasm = false;

    if debug_wasm {
        #[cfg(not(target_family = "wasm"))]
        if let Err(e) = std::fs::write("/tmp/debug_core_module.wasm", &core_module_bytes) {
            eprintln!("Note: Could not write debug core module: {}", e);
        }

        let mut validator = wasmparser::Validator::new();
        if let Err(e) = validator.validate_all(&core_module_bytes) {
            let mut msg = format!("Core module failed validation: {}", e);
            msg.push_str(&augment_with_context(&core_module_bytes, &msg));
            return Err(CodegenError::EncodingError(msg));
        }
    }

    // Optionally pipe through `wasm-opt`. Must run before metadata
    // embedding — wasm-opt operates on raw core modules and would
    // strip the custom sections wit_component is about to add.
    #[cfg(not(target_family = "wasm"))]
    if let Some(args) = options.wasm_opt_args.as_ref() {
        core_module_bytes = run_wasm_opt(&core_module_bytes, args)?;
    }

    // Embed WIT metadata into the core module (modifies in place)
    wit_component::embed_component_metadata(
        &mut core_module_bytes,
        &resolve,
        world_id,
        StringEncoding::UTF8,
    )
    .map_err(|e| CodegenError::EncodingError(format!("Failed to embed WIT metadata: {}", e)))?;

    if debug_wasm {
        #[cfg(not(target_family = "wasm"))]
        if let Err(e) = std::fs::write("/tmp/debug_module_with_metadata.wasm", &core_module_bytes) {
            eprintln!("Note: Could not write debug module with metadata: {}", e);
        }
    }

    // Use ComponentEncoder to produce the final component
    let encoder = ComponentEncoder::default()
        .module(&core_module_bytes)
        .map_err(|e| CodegenError::EncodingError(format!("Failed to set module: {}", e)))?;

    let component_bytes = encoder.validate(true).encode().map_err(|e| {
        // Unwind the full anyhow chain — the top-level message is usually
        // just "failed to validate component output"; the real reason is
        // nested underneath, often with a byte offset into the core module.
        let mut msg = format!("Failed to encode component: {}", e);
        let mut src = e.source();
        while let Some(cause) = src {
            msg.push_str(&format!("\n  caused by: {}", cause));
            src = cause.source();
        }
        // Try to map the deepest offset in the error text to a function
        // index in the core module so we can jump straight to the
        // misbehaving emitter.
        msg.push_str(&augment_with_context(&core_module_bytes, &msg));
        CodegenError::EncodingError(msg)
    })?;

    Ok(component_bytes)
}

// ============================================================================
// Types
// ============================================================================

/// How to store a flattened canonical-ABI slot into memory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StoreWidth {
    I32,
    I32_8,
    I32_16,
    I64,
    F32,
    F64,
}

impl StoreWidth {
    /// Emit the store instruction for this width. Caller is responsible for
    /// having pushed `(addr, value)` on the stack.
    pub fn emit_store(self, func: &mut wasm_encoder::Function) {
        use wasm_encoder::{Instruction, MemArg};
        let ma = |offset: u64, align: u32| MemArg {
            offset,
            align,
            memory_index: 0,
        };
        match self {
            StoreWidth::I32 => func.instruction(&Instruction::I32Store(ma(0, 2))),
            StoreWidth::I32_8 => func.instruction(&Instruction::I32Store8(ma(0, 0))),
            StoreWidth::I32_16 => func.instruction(&Instruction::I32Store16(ma(0, 1))),
            StoreWidth::I64 => func.instruction(&Instruction::I64Store(ma(0, 3))),
            StoreWidth::F32 => func.instruction(&Instruction::F32Store(ma(0, 2))),
            StoreWidth::F64 => func.instruction(&Instruction::F64Store(ma(0, 3))),
        };
    }
}

/// One entry of a value's canonical-ABI flat representation, annotated with
/// the byte offset it should be stored at relative to the value's base address.
#[derive(Clone, Copy, Debug)]
pub struct FlatSlot {
    pub valtype: wasm_encoder::ValType,
    pub offset: u32,
    pub store: StoreWidth,
}

/// Per-valtype base local indices for canonical-ABI flat-slot stores.
/// The scratch region is laid out as:
///   [i32_base .. i32_base+i32_count)
///   [i64_base .. i64_base+i64_count)
///   [f32_base .. f32_base+f32_count)
///   [f64_base .. f64_base+f64_count)
/// Each block/function pre-computes counts by walking its ops so only
/// valtypes actually used reserve locals.
#[derive(Clone, Copy, Debug, Default)]
pub struct FlatScratchBases {
    pub i32_base: u32,
    pub i32_count: u32,
    pub i64_base: u32,
    pub i64_count: u32,
    pub f32_base: u32,
    pub f32_count: u32,
    pub f64_base: u32,
    pub f64_count: u32,
}

/// Slot-wise join of two flattened param lists under the canonical ABI rules
/// used by variants/results: at each position take the "wider" slot that can
/// hold either case's value. Returns the longer list with each common position
/// promoted to the shared representation.
fn join_flat_valtypes(
    a: &[wasm_encoder::ValType],
    b: &[wasm_encoder::ValType],
) -> Vec<wasm_encoder::ValType> {
    use wasm_encoder::ValType;
    let n = a.len().max(b.len());
    let mut out = Vec::with_capacity(n);
    for i in 0..n {
        let av = a.get(i).copied();
        let bv = b.get(i).copied();
        let merged = match (av, bv) {
            (Some(x), None) | (None, Some(x)) => x,
            (Some(x), Some(y)) if x == y => x,
            // Promote mismatched slots. Any 64-bit type wins over 32-bit; within
            // a width, integer wins over float (canonical ABI uses integer
            // transport for mixed cases). Refs are kept as-is when they appear
            // alone; ref-vs-non-ref width mismatches fall back to i64 to give
            // 8 bytes of underlying transport (the GC ref will be reinterpret-
            // cast at the consumer; this is a stop-gap for variant-of-list
            // joins that callers should structurally avoid by promoting both
            // arms to ref via boxing — Phase 5d).
            (Some(x), Some(y)) => {
                let is_64 = |v: ValType| matches!(v, ValType::I64 | ValType::F64);
                let is_ref = |v: ValType| matches!(v, ValType::Ref(_));
                if is_ref(x) && is_ref(y) {
                    // Two refs at the same slot but different types — emit
                    // the more-permissive option. For Phase 5b-v.3 we don't
                    // expect this (only one ref kind per slot).
                    x
                } else if is_ref(x) || is_ref(y) {
                    ValType::I64
                } else if is_64(x) || is_64(y) {
                    ValType::I64
                } else {
                    ValType::I32
                }
            }
            (None, None) => ValType::I32,
        };
        out.push(merged);
    }
    out
}

/// Memory layout for a component (computed from block-based slots).
#[derive(Clone)]
pub(crate) struct MemoryLayout {
    /// Base offset in memory for this component
    pub base: i32,
    /// Offset for each signal (relative to base)
    pub signal_offsets: Vec<i32>,
    /// Total size used
    pub size: i32,
}

impl MemoryLayout {
    pub fn new(component: &LirResource, base: i32, _layout_ctx: &mut LirLayoutContext) -> Self {
        // Phase 1.1a: per-signal offsets sourced from
        // `component.signal_layout` (computed at LIR-lowering time).
        // GC-struct-migrated signals keep their `signal_offsets[i] == -1`
        // sentinel so the legacy `signal_addr` API stays valid; callers
        // gate on `signal_in_struct` before dereferencing.
        let signal_offsets: Vec<i32> = component
            .signal_layout
            .signals
            .iter()
            .map(|storage| match storage.mem {
                Some(m) => m.offset as i32,
                None => -1,
            })
            .collect();
        let mut offset = component.signal_layout.memory_size as i32;

        // Memory slots are pre-computed in component.slots
        // Find max offset to get total size
        for slot in &component.slots {
            if let LirSlotKind::Memory {
                offset: slot_offset,
                size,
            } = &slot.kind
            {
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

    /// An empty layout for module-scope emission (no component signals).
    ///
    /// Paired with [`LirResource`] containing `signals: []`, it routes every
    /// signal lookup through the module-level `global_property_addrs` path
    /// rather than the component-local one. Any expression that does try to
    /// resolve a component-local signal will hit an out-of-bounds
    /// `signal_addr` and fail loudly — which is the desired behaviour for
    /// module scope (global defaults must not reference component state).
    pub fn empty_for_module() -> Self {
        MemoryLayout {
            base: 0,
            signal_offsets: Vec::new(),
            size: 0,
        }
    }
}

// ============================================================================
// Constants - Import function indices (must match order in build_core_module)
// ============================================================================

pub(crate) const IMPORT_CREATE_ELEMENT: u32 = 0;
pub(crate) const IMPORT_CREATE_TEXT: u32 = 1;
pub(crate) const IMPORT_CREATE_COMMENT: u32 = 2;
pub(crate) const IMPORT_SET_ATTRIBUTE: u32 = 3;
pub(crate) const IMPORT_REMOVE_ATTRIBUTE: u32 = 4;
pub(crate) const IMPORT_SET_TEXT_CONTENT: u32 = 5;
pub(crate) const IMPORT_SET_STYLE: u32 = 6;
pub(crate) const IMPORT_SET_CLASS: u32 = 7;
pub(crate) const IMPORT_APPEND_CHILD: u32 = 8;
pub(crate) const IMPORT_INSERT_BEFORE: u32 = 9;
pub(crate) const IMPORT_REMOVE_CHILD: u32 = 10;
pub(crate) const IMPORT_REMOVE: u32 = 11;
pub(crate) const IMPORT_GET_PARENT: u32 = 12;
pub(crate) const IMPORT_GET_NEXT_SIBLING: u32 = 13;
pub(crate) const IMPORT_ADD_EVENT_LISTENER: u32 = 14;
pub(crate) const IMPORT_REMOVE_EVENT_LISTENER: u32 = 15;
/// Insert node after anchor (for conditional rendering).
/// Signature: insert_after(parent: i32, node: i32, anchor: i32) -> ()
/// Semantically: parent.insertBefore(node, anchor.nextSibling)
pub(crate) const IMPORT_INSERT_AFTER: u32 = 16;
/// Create a layout-neutral wrapper element used to group `for`
/// iteration content and `if` branch content under a single DOM root.
/// `host.remove(wrapper)` cascades to detach the entire subtree.
/// Signature: create_fragment() -> i32
pub(crate) const IMPORT_CREATE_FRAGMENT: u32 = 17;
// After DOM imports (18 total), callbacks are imported dynamically
// Then: resource-new, resource-drop, realloc
pub(crate) const NUM_DOM_IMPORTS: u32 = 18;

/// Round-trip a `DomImports.*` DefId back to its wasm import index.
///
/// Phase 2.1 lands this helper; Phase 2.2 invokes it from
/// `LirOp::CallFunction` emission so the lowering can switch DOM-op
/// sites to the generic call op against the pre-allocated DOM-import
/// DefIds. Returns `None` if `def_id` is not one of the 18
/// pre-allocated DOM-import DefIds.
pub(crate) fn wasm_import_index_for_dom_def(ctx: &CompilerContext, def_id: DefId) -> Option<u32> {
    let d = ctx.dom_imports();
    if def_id == d.create_element {
        Some(IMPORT_CREATE_ELEMENT)
    } else if def_id == d.create_text {
        Some(IMPORT_CREATE_TEXT)
    } else if def_id == d.create_comment {
        Some(IMPORT_CREATE_COMMENT)
    } else if def_id == d.create_fragment {
        Some(IMPORT_CREATE_FRAGMENT)
    } else if def_id == d.set_attribute {
        Some(IMPORT_SET_ATTRIBUTE)
    } else if def_id == d.remove_attribute {
        Some(IMPORT_REMOVE_ATTRIBUTE)
    } else if def_id == d.set_text_content {
        Some(IMPORT_SET_TEXT_CONTENT)
    } else if def_id == d.set_style {
        Some(IMPORT_SET_STYLE)
    } else if def_id == d.set_class {
        Some(IMPORT_SET_CLASS)
    } else if def_id == d.append_child {
        Some(IMPORT_APPEND_CHILD)
    } else if def_id == d.insert_before {
        Some(IMPORT_INSERT_BEFORE)
    } else if def_id == d.insert_after {
        Some(IMPORT_INSERT_AFTER)
    } else if def_id == d.remove_child {
        Some(IMPORT_REMOVE_CHILD)
    } else if def_id == d.remove {
        Some(IMPORT_REMOVE)
    } else if def_id == d.get_parent {
        Some(IMPORT_GET_PARENT)
    } else if def_id == d.get_next_sibling {
        Some(IMPORT_GET_NEXT_SIBLING)
    } else if def_id == d.add_event_listener {
        Some(IMPORT_ADD_EVENT_LISTENER)
    } else if def_id == d.remove_event_listener {
        Some(IMPORT_REMOVE_EVENT_LISTENER)
    } else {
        None
    }
}

/// Import indices for a single component's callbacks and resource intrinsics.
///
/// Callback imports are registered for every component whose body declares
/// `func`-typed properties — regardless of whether the component itself is
/// `export`ed — because the component's code can invoke them from its body
/// (e.g. from an event handler) and those `Call` sites need a concrete
/// import index to target. Only `export`ed components get a
/// `[resource-new]` import, since non-exported components do not surface a
/// WIT resource constructor.
#[derive(Debug, Clone)]
pub(crate) struct ComponentCallbackLayout {
    /// DefIds of the callbacks (in order) - used for iteration/labeling.
    /// Import index for any individual DefId must be looked up via
    /// `ImportLayout::callback_indices` because callbacks are deduped by
    /// kebab-case name across the whole module at emission time.
    pub callback_def_ids: Vec<DefId>,
    /// Index of [resource-new]component import (for constructor return).
    /// `None` for non-exported components (they have no resource surface).
    pub resource_new: Option<u32>,
}

/// Import layout - tracks imports for all components (exported or not).
/// Callbacks are registered for every component; `resource_new` slots are
/// only allocated for exported components.
#[derive(Debug, Clone)]
pub(crate) struct ImportLayout {
    /// Callback layouts for each component (in LirModule order)
    pub components: Vec<ComponentCallbackLayout>,
    /// Authoritative map from callback DefId to its actual WASM import
    /// index. Each component has its own callback namespace (no
    /// cross-component dedup); this map is the direct DefId → import
    /// index lookup used by expr.rs call emission.
    pub callback_indices: HashMap<DefId, u32>,
    /// Ordered list of unique callback entries as `(component_idx,
    /// cb_def_id)` pairs, in emission order. Each component owns its
    /// own callback namespace (one WIT interface per component), so two
    /// sibling components can both declare `on-submit` with different
    /// signatures without colliding. Used by the emission loops so
    /// `find_callback_index` and actual import order agree.
    pub unique_callbacks: Vec<(usize, DefId)>,
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
    /// Calculate import layout covering every component in `all_components`.
    ///
    /// Every component (exported or not) contributes its callback imports —
    /// a `func`-typed property is always host-implemented, and the
    /// component body can invoke those callbacks directly, so each needs a
    /// concrete import index. Only exported components additionally get a
    /// `[resource-new]` import slot.
    ///
    /// The `export` modifier on a `func` property controls whether the
    /// callback is re-surfaced in the component's WIT export interface —
    /// it does NOT gate whether it is imported. See `wit_ast.rs` for the
    /// export-surface side of this distinction.
    pub fn new(
        all_components: &[&LirResource],
        ctx: &CompilerContext,
    ) -> Result<Self, CodegenError> {
        // Step 1: collect each component's callback DefIds.
        let mut per_component: Vec<Vec<DefId>> = Vec::with_capacity(all_components.len());
        for component in all_components.iter() {
            let comp_def = ctx.defs.as_component(component.def_id);
            let callbacks: Vec<DefId> = comp_def
                .map(|c| {
                    c.callbacks
                        .iter()
                        .filter(|&def_id| ctx.defs.as_function(*def_id).is_some())
                        .copied()
                        .collect()
                })
                .unwrap_or_default();
            per_component.push(callbacks);
        }

        // Step 2: each component owns its own callback namespace — one WIT
        // interface per component (`{component}-callbacks`). Two sibling
        // components can both declare e.g. `on-submit` with different
        // signatures; they land in separate interfaces and get separate
        // import slots, so no collision. We still refuse duplicate names
        // WITHIN a single component (defence-in-depth; the parser shouldn't
        // produce this, but if it does we won't silently collapse).
        let mut callback_indices: HashMap<DefId, u32> = HashMap::new();
        let mut unique_callbacks: Vec<(usize, DefId)> = Vec::new();
        let mut current_idx = NUM_DOM_IMPORTS;
        for (comp_idx, callbacks) in per_component.iter().enumerate() {
            let mut seen_in_component: HashMap<String, DefId> = HashMap::new();
            for &cb_def_id in callbacks {
                let name = if let Some(func_def) = ctx.defs.as_function(cb_def_id) {
                    ctx.str(func_def.name).to_string()
                } else {
                    continue;
                };
                if let Some(&prior) = seen_in_component.get(&name) {
                    return Err(CodegenError::InvalidIR(format!(
                        "component declares callback `{}` twice (DefIds {:?} and {:?}); \
                         a single component cannot host two callbacks of the same name",
                        name, prior, cb_def_id
                    )));
                }
                seen_in_component.insert(name, cb_def_id);
                let idx = current_idx;
                current_idx += 1;
                unique_callbacks.push((comp_idx, cb_def_id));
                callback_indices.insert(cb_def_id, idx);
            }
        }

        // Step 3: after all callback imports, each exported component gets a
        // [resource-new] import. Non-exported components never surface a
        // resource and therefore do not consume a slot here.
        let mut components = Vec::with_capacity(all_components.len());
        for (i, component) in all_components.iter().enumerate() {
            let callbacks = per_component[i].clone();
            let _ = i;
            let resource_new = if component.is_export {
                let idx = current_idx;
                current_idx += 1;
                Some(idx)
            } else {
                None
            };
            components.push(ComponentCallbackLayout {
                callback_def_ids: callbacks,
                resource_new,
            });
        }

        // Note: allocator functions are LOCAL (not imported)
        let num_imports = current_idx;

        Ok(Self {
            components,
            callback_indices,
            unique_callbacks,
            num_imports,
        })
    }

    /// Find the callback index for a given DefId. Each component's
    /// callbacks get their own slots (one WIT interface per component),
    /// so this is a direct DefId → index lookup.
    pub fn find_callback_index(&self, def_id: DefId) -> Option<u32> {
        self.callback_indices.get(&def_id).copied()
    }
}

// ============================================================================
// Builder
// ============================================================================

/// One entry in `WasmPackageBuilder::filter_calls`.
///
/// Tuple components: (owning component index or `None` for module-scope,
/// element type, element size in bytes, predicate parameter binding,
/// predicate body expression).
pub type FilterCallEntry = (Option<usize>, Ty, u32, (LocalId, Ty), LirExpr);

/// Builder for WASM package (component) generation.
pub(crate) struct WasmPackageBuilder<'a> {
    /// All components (code is generated for all of them)
    pub components: &'a [LirResource],
    pub ctx: &'a CompilerContext,
    /// String data manager for literal interning
    pub strings: StringData,
    /// First free address after string data (compile-time heap base)
    heap_base: u32,
    /// Current bump pointer for compile-time allocations
    heap_ptr: u32,
    /// Layout context for type size/alignment queries.
    pub layout_ctx: LirLayoutContext<'a>,
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
    /// Block function index mapping: block_id -> wasm_func_idx.
    /// Phase 0.3q: BlockIds are module-wide unique, so `(comp_idx, BlockId)`
    /// collapsed to just `BlockId`. Cross-component calls (lifecycle)
    /// resolve through this single map identically to intra-component calls.
    pub block_func_indices: std::collections::HashMap<BlockId, u32>,
    /// `DefId → wasm function index` map used by `LirOp::CallFunction`.
    /// Populated externally before `op_emit` runs — yel-lang's UI
    /// compiler never emits `CallFunction` so this stays empty for
    /// pure-UI builds. Flow-frontend codegen pre-populates it with one
    /// entry per registered flow function so cross-function calls
    /// resolve to the right wasm idx.
    pub def_id_to_func_idx: std::collections::HashMap<DefId, u32>,
    /// Memory layouts by component index
    pub layouts: Vec<MemoryLayout>,
    /// Names accumulated for dynamically-emitted function types in the
    /// type section — `(type_idx, name)` pairs. The name section reads
    /// this to emit `$name` in WAT. Populated in `build_core_module`
    /// alongside `intern_type` / direct `types.ty().function(...)`
    /// emissions for the runtime, accessor, ctor, list-ctor, callback,
    /// dispatch, and block-fn types so every function type has a
    /// human-readable identity in the dump.
    pub function_type_names: Vec<(u32, String)>,
    /// WIT package info for interface-qualified export names (namespace, name, version)
    pub wit_package: Option<(String, String, String)>,
    /// Current block's local variable offset (for block functions)
    pub current_block_local_offset: Option<u32>,
    /// Mapping from LocalId to slot index for captured locals in current block
    /// Map: for-loop / filter-closure captured `LocalId` → the absolute WASM
    /// local index holding its backing value. For regular blocks this is the
    /// item-ptr slot's resolved local; for inline filter closures it's a
    /// reserved scratch local. The value is an absolute WASM local idx
    /// (already includes any param / local_offset adjustment), so emit_expr
    /// can use it directly without further offsetting.
    pub current_block_captured_locals: Option<HashMap<LocalId, u32>>,
    /// Mapping from LocalId to SlotId for inline-computed locals (e.g., for-loop items)
    pub current_block_local_to_slot: Option<HashMap<LocalId, LirSlotId>>,
    /// Per-LocalId binding-mode override for the current block. Mirrors
    /// `LirBlock.local_modes`; consulted in the `Local` expr arm to gate
    /// the typed load after `local.get`. Missing entries (or
    /// `BindingMode::Ptr`) preserve today's behavior — the slot holds an
    /// address and a typed load follows. `BindingMode::Value` (introduced
    /// in 5b-v.3 for migrated-list iter bindings) skips the load.
    pub current_block_local_modes: Option<HashMap<LocalId, LirBindingMode>>,
    /// List construct info: (element_type, element_count) for runtime function generation
    pub list_constructs: Vec<(Ty, usize)>,

    /// List types that need an `append` runtime helper. One per unique
    /// `list<T>` referenced by `Call { func: append, args: [list, elem] }`.
    /// Each entry triggers `generate_list_append_function(list_ty)` and
    /// gets a `RuntimeFunctions::list_append` index.
    pub list_appends: Vec<Ty>,
    /// Memory addresses for global singleton properties, keyed by property DefId.
    pub global_property_addrs: HashMap<DefId, i32>,
    /// Per-block layouts for migrated `global Foo { ... }` blocks. One
    /// entry per `defs.globals()` in declaration order. Holds the GC
    /// struct type index, self-global index, and per-property field
    /// paths (empty path = pointer-typed, stays on memory).
    pub globals_layouts: Vec<crate::wasm::gc_types::GlobalsBlockLayout>,
    /// Block DefId → index into `globals_layouts`. Reverse lookup for
    /// `Definitions::owning_global_block`.
    pub global_block_def_to_idx: HashMap<DefId, usize>,
    /// Typed default expressions for global singleton properties, keyed by
    /// property DefId. Lowered at module start to seed each backing slot.
    pub global_defaults: HashMap<DefId, LirExpr>,
    /// Recorded `AddEventListener` sites: `(local_id, comp_idx, handler_block)`.
    /// `local_id` is the per-component 16-bit ordinal assigned when the
    /// op was emitted; combined with the host handle at runtime
    /// (`(handle << 16) | local_id`) it uniquely identifies a handler
    /// invocation across every live instance of every component.
    pub global_handler_map: Vec<(u32, usize, BlockId)>,
    /// Function index of the standalone dispatch function in the core module.
    pub dispatch_func_idx: Option<u32>,
    /// Per-global-signal fanout helper functions. For each global
    /// property whose mutation must trigger effects in 1+ components,
    /// we emit a `() -> ()` helper that walks each observing
    /// component's registry array and calls each live instance's
    /// effect block. Setters/handlers that mutate a global signal then
    /// just `call $global_fanout_<sig>` — no inline scratch locals
    /// needed at the call site, and the registry walk runs against the
    /// **current** state of every component's registry, hitting all
    /// live instances no matter how many.
    pub global_fanout_func_idx: HashMap<DefId, u32>,
    /// Filter calls: (component_idx, elem_ty, elem_size, param, predicate) for function generation
    /// Index into Vec is the filter ID, maps to $filter_0, $filter_1, etc.
    /// Captured signals are extracted from predicate LIR on-demand (SignalRead nodes)
    /// Filter call sites. `Option<usize>` is the owning component index:
    /// `Some(i)` for a call inside component `i`, `None` for module-scope
    /// (e.g. a `.filter(...)` in a global-singleton default).
    pub filter_calls: Vec<FilterCallEntry>,

    /// Demand-driven runtime helper flags. Populated by
    /// [`collect_runtime_needs`] before the type/function/code section
    /// build pass. Drives both index allocation in
    /// [`runtime::RuntimeFunctions::new`] and emission gating in
    /// `build_core_module` — anything `false` here is neither indexed
    /// nor written into the code section.
    pub runtime_needs: runtime::RuntimeNeeds,

    /// Block-type indices for `if … else …` expressions whose result is
    /// multi-slot (e.g. `option<s32>` flattens to `(i32 discr, i32 val)`).
    /// WASM's `BlockType::Result(valtype)` only declares ONE result —
    /// multi-slot branches would fail validation there. We intern a
    /// function type `() -> (slots)` per unique shape during the Type
    /// section build and store the index here, keyed by the flattened
    /// shape; emit sites look up the index and use
    /// `BlockType::FunctionType(idx)`. Single-slot ternaries don't enter
    /// this map — they keep the original `BlockType::Result` path.
    pub ternary_block_types: HashMap<Vec<wasm_encoder::ValType>, u32>,
    /// Per-component GC type index tables. Populated during type-section
    /// emission in `build_core_module`; one entry per component in
    /// `self.components` order. Phase 1 populates this; phases 2-6 read
    /// it from emit sites when producing `struct.new`/`array.get`/etc.
    pub gc_layouts: Vec<gc_types::GcTypeLayout>,
    /// Module-shared type index of `$handle` (the registry handle
    /// struct: anyref + i32-next). Populated by `emit_shared_handle_types`
    /// once before per-component types are emitted.
    pub shared_handle_type_idx: Option<u32>,
    /// Module-shared type index of `$handle-array` (array of nullable
    /// `$handle` refs). Same population path as `shared_handle_type_idx`.
    pub shared_handle_arr_type_idx: Option<u32>,
    /// Phase 1 of records-to-GC migration: per-program record GC type
    /// registry. Populated by `emit_program_record_types` during
    /// type-section emission. Phase 1 only emits the types; no consumer
    /// reads from this map yet (signal storage / field access /
    /// constructors all stay on the legacy memory path through Phase 1).
    /// Phase 2+ migration reads `record_type_idx[def_id]` at every
    /// `struct.new` / `struct.get` / `struct.set` site. See
    /// `gc_types::RecordGcTypes` for the full layout.
    pub record_gc_types: gc_types::RecordGcTypes,
    /// Current filter call index (incremented during emit_expr to match collection order)
    pub current_filter_call_idx: usize,
    /// Mapping from DefId to local index for captured signals in current filter function
    /// Used by emit_predicate_expr to handle SignalRead
    pub current_filter_captured_signals: Option<HashMap<DefId, (u32, bool)>>, // (local_idx, is_fat_ptr)
    /// Absolute address of a 16-byte scratch region used as the return-area
    /// pointer for imported callbacks whose canonical-ABI result requires
    /// indirect return (e.g. string, list, multi-flat records). Set during
    /// `build_core_module` before any callback call site is emitted.
    pub cb_return_scratch_addr: Option<i32>,
    /// Absolute address of an 8-byte slot used to stash the allocated buffer
    /// pointer across a pointer-convention indirect-return callback call
    /// (record/tuple). The callsite writes the ptr here before Call, then
    /// reads it back after Call to produce the expression result.
    pub cb_pointer_stash_addr: Option<i32>,
    /// Starting local index of i32 scratch locals reserved for canonical-ABI
    /// flat-slot stores within the current function body. Block functions,
    /// constructors, and the globals-init all reserve these past their
    /// declared params/slots and set this field while emitting their ops.
    pub current_init_scratch_start: Option<u32>,
    /// Per-valtype scratch base local indices for composite flat-slot stores
    /// (SignalWriteExpr + InitSignal of composite types). Each contains the
    /// first local index for scratches of that valtype; consecutive locals of
    /// the same type follow. `None` when the current function has none.
    pub current_flat_scratch: Option<FlatScratchBases>,
    /// WASM local index holding the current function's `(ref $Comp_<i>)`
    /// self ref, when the function operates on a struct-typed self
    /// (constructor body, internal-ref entry points). Signal struct
    /// helpers source self from this local. `None` outside such
    /// bodies — `emit_self_ref` rejects emit attempts and demands
    /// callers route through a registry lookup first.
    pub current_self_local: Option<u32>,
    /// Index of the component that owns `current_self_local`'s ref
    /// type — `(ref null $Comp_<i>)` for component `i`. `emit_self_ref`
    /// uses this to refuse pushing the local for a foreign component
    /// (cross-component trigger fan-out, dispatch indirect-call) — the
    /// caller must perform a registry lookup into the foreign
    /// component's typed self instead.
    pub current_self_comp_idx: Option<usize>,
    /// In-scope boundary struct refs. Keyed by `TreeBoundaryId`,
    /// value is the WASM local index holding `(ref null
    /// <boundary_struct>)` for the duration of the current function
    /// body. The component-root boundary is **never** kept here — it
    /// is always materialized on demand via `$self.tree` so a stale
    /// local can't drift from a re-allocated root.
    ///
    /// Populated when emitting an inner mount/update/handler scope
    /// that takes a boundary as a parameter (for-iter mount, if-branch
    /// mount, fan-out callbacks). Cleared on function exit so a
    /// subsequent function never sees a foreign function's locals.
    pub current_boundary_locals: HashMap<yel_core::ids::TreeBoundaryId, u32>,
    /// Per-component counter for the local-id portion of the encoded
    /// handler-id. Each `AddEventListener` site within a component mints
    /// the next local-id from this map (entry created on first use,
    /// starting at 0). 16 bits, capping each component at 65536 listener
    /// sites — far above any realistic UI tree.
    pub next_handler_local_id: HashMap<usize, u32>,
    /// Per-component running cursor over the parent-retention region
    /// in `$Comp_<i>`. Incremented each time a `MountComponent` op
    /// outside any for-iter body is emitted; resets when the cursor
    /// hits `gc_layouts[i].parent_retention_count`.
    pub parent_retention_cursor: HashMap<usize, u32>,
    /// For each distinct child component index reachable from a
    /// `MountComponent` op in the **current** function body, the WASM
    /// local index of a typed `(ref null $Comp_<child>)` scratch local
    /// reserved up front. The local holds the typed ref returned by
    /// the child's internal constructor across the matching internal
    /// mount call and the parent-retention struct.set. `None` in
    /// functions that contain no `MountComponent` ops.
    pub current_mount_child_locals: Option<HashMap<usize, u32>>,
    /// Per-child-component scratch i32 local index reserved for
    /// `emit_registry_alloc`'s `idx` scratch when the surrounding
    /// function emits a `MountComponent` for that child. `None` when
    /// the surrounding function emits no MountComponent ops.
    pub current_mount_child_alloc_idx_locals: Option<HashMap<usize, u32>>,
    /// Per-child-component scratch typed `(ref null $CompHandleArr_<child>)`
    /// local index reserved for `emit_registry_alloc`'s `arr` scratch.
    pub current_mount_child_alloc_arr_locals: Option<HashMap<usize, u32>>,
    /// Function-index base for every component by position; `[i]` is the
    /// constructor index for `components[i]`. Populated inside
    /// `build_core_module` once the final `first_component_func` is known.
    pub component_func_bases: Vec<u32>,

    /// Name-section label entries accumulated during emission of the
    /// *current* function. Each entry is `(label_idx, name)` where
    /// `label_idx` is the depth-first preorder index of a structural
    /// WASM op (block / loop / if) within the function. Reset to
    /// empty before each function body emission. Drained into
    /// `function_label_names` keyed by wasm function index when the
    /// body is finished.
    pub current_function_labels: Vec<(u32, String)>,
    /// Running counter of structural ops emitted in the current
    /// function (used to mint label indices as ops are visited).
    pub current_label_counter: u32,
    /// Per-WASM-function label-name entries, accumulated across the
    /// entire module. Consumed by `generate_name_section_multi` to
    /// build the `labels` indirect name map. `None` / missing entries
    /// get no `label` subsection entries (debug-only hint).
    pub function_label_names: HashMap<u32, Vec<(u32, String)>>,
    /// Phase 5b-v.3: per-GC-array-type materializer function indices.
    /// Maps `arr_type_idx` → wasm function index of `$gc_list_unbox_<i>`,
    /// which takes `(ref null $arr)` and returns `(i32, i32)` (data_ptr, len).
    /// Used by `SignalRead` when a GC-list signal is read in a non-for-loop
    /// expression context (filter source, method call, etc.).
    pub gc_list_materializer_fn_indices: HashMap<u32, u32>,
    /// Phase 5e.6: per-GC-array-type un-materializer function indices.
    /// Maps `arr_type_idx` → wasm function index of the helper that takes
    /// canonical `(ptr, len)` and returns `(ref null $arr)`. Used by
    /// `record_pack_from_memory` when a record field is a typed-array
    /// list (DTR-eligible nested list).
    pub gc_list_unmaterializer_fn_indices: HashMap<u32, u32>,
    /// Phase 7: function index of `$pack_color_to_attr_slots`, the
    /// per-program helper that lifts a `(ref null $var_color)` to the
    /// canonical-ABI flattening of the `attribute-value::color(color)`
    /// case — `(i64 inner_disc, i32 r, i32 g, i32 b, i32 a)`. Emitted
    /// only when the program references the language `color` type.
    pub pack_color_helper_fn_idx: Option<u32>,
}

impl<'a> WasmPackageBuilder<'a> {
    /// String data base offset (after reserved memory for conversion buffers).
    /// Memory layout:
    /// - 0x0000-0x001F: s32_to_string buffer (32 bytes)
    /// - 0x0020-0x00FF: Reserved for future conversion buffers
    /// - 0x0100+: String data section
    const STRING_DATA_BASE: u32 = 256;

    pub fn new(components: &'a [LirResource], ctx: &'a CompilerContext) -> Self {
        Self {
            components,
            ctx,
            strings: StringData::new(Self::STRING_DATA_BASE),
            heap_base: 0,
            heap_ptr: 0,
            layout_ctx: LirLayoutContext::new(ctx),
            import_layout: None,
            alloc_funcs: None,
            runtime_funcs: None,
            concat_arities: Vec::new(),
            record_types: Vec::new(),
            handler_counter: 0,
            block_func_indices: std::collections::HashMap::new(),
            def_id_to_func_idx: std::collections::HashMap::new(),
            function_type_names: Vec::new(),
            layouts: Vec::new(),
            global_property_addrs: HashMap::new(),
            globals_layouts: Vec::new(),
            global_block_def_to_idx: HashMap::new(),
            global_defaults: HashMap::new(),
            global_handler_map: Vec::new(),
            dispatch_func_idx: None,
            global_fanout_func_idx: HashMap::new(),
            wit_package: None,
            current_block_local_offset: None,
            current_block_captured_locals: None,
            current_block_local_to_slot: None,
            current_block_local_modes: None,
            list_constructs: Vec::new(),
            list_appends: Vec::new(),
            filter_calls: Vec::new(),
            runtime_needs: runtime::RuntimeNeeds::default(),
            ternary_block_types: HashMap::new(),
            gc_layouts: Vec::new(),
            shared_handle_type_idx: None,
            shared_handle_arr_type_idx: None,
            record_gc_types: gc_types::RecordGcTypes::default(),
            current_filter_call_idx: 0,
            current_filter_captured_signals: None,
            cb_return_scratch_addr: None,
            cb_pointer_stash_addr: None,
            current_init_scratch_start: None,
            current_flat_scratch: None,
            current_self_local: None,
            current_self_comp_idx: None,
            current_boundary_locals: HashMap::new(),
            next_handler_local_id: HashMap::new(),
            parent_retention_cursor: HashMap::new(),
            current_mount_child_locals: None,
            current_mount_child_alloc_idx_locals: None,
            current_mount_child_alloc_arr_locals: None,
            component_func_bases: Vec::new(),
            current_function_labels: Vec::new(),
            current_label_counter: 0,
            function_label_names: HashMap::new(),
            gc_list_materializer_fn_indices: HashMap::new(),
            gc_list_unmaterializer_fn_indices: HashMap::new(),
            pack_color_helper_fn_idx: None,
        }
    }

    /// Set the WIT package info for interface-qualified export names.
    ///
    /// Assumes `namespace` and `name` are already valid WIT kebab-case
    /// identifiers — `Compiler::validate_package` rejects non-compliant
    /// package declarations at parse time.
    pub fn set_wit_package(&mut self, namespace: &str, name: &str, version: &str) {
        self.wit_package = Some((namespace.to_string(), name.to_string(), version.to_string()));
    }

    /// Provide the LIR-lowered default expressions for global singleton
    /// properties. The module start function stores them to each property's
    /// backing slot before any export runs.
    pub fn set_global_defaults(&mut self, defaults: HashMap<DefId, LirExpr>) {
        self.global_defaults = defaults;
    }

    /// Reset handler counter (call before generating each component's functions)
    pub fn reset_handler_counter(&mut self) {
        self.handler_counter = 0;
    }

    /// Get exported components
    pub fn get_exported_components(&self) -> Vec<&LirResource> {
        self.components.iter().filter(|c| c.is_export).collect()
    }

    /// Initialize heap after collecting all strings.
    /// Heap starts after string data, aligned to 8 bytes.
    pub(crate) fn init_heap(&mut self) {
        let string_end = self.strings.base() + self.strings.size();
        self.heap_base = align_to(string_end, 8);
        self.heap_ptr = self.heap_base;
    }

    // ========================================================================
    // String handling
    // ========================================================================

    pub(crate) fn collect_strings(&mut self) {
        // Collect strings from all components
        // LirResource now has pre-computed strings, so we just copy them
        for (comp_idx, component) in self.components.iter().enumerate() {
            // Copy pre-interned strings from component
            for s in &component.strings {
                self.add_string(s);
            }

            // Collect strings, concat arities, record types, list constructs, and filter calls from signal defaults
            for signal in &component.signals {
                if let Some(default_expr) = &signal.default {
                    self.collect_strings_from_expr(default_expr);
                    self.collect_concat_arities(default_expr);
                    self.collect_record_types(default_expr);
                    self.collect_list_constructs(default_expr);
                    self.collect_filter_calls(Some(comp_idx), default_expr);
                    self.collect_runtime_needs(default_expr);
                }
                // FatPointer signals trigger load_fat_ptr/store_fat_ptr.
                self.note_signal_runtime_needs(signal.ty);
            }

            // Collect strings, concat arities, record types, list constructs, and filter calls from pre-lowered expressions
            // IMPORTANT: Must collect strings here so layout calculation includes them
            for expr in &component.exprs {
                self.collect_strings_from_expr(expr);
                self.collect_concat_arities(expr);
                self.collect_record_types(expr);
                self.collect_list_constructs(expr);
                self.collect_filter_calls(Some(comp_idx), expr);
                self.collect_runtime_needs(expr);
            }

            // Walk every block's ops to detect helpers triggered by ops
            // (CreateTextDynamic / SignalWriteExpr → emit_expr_as_string
            // / emit_expr_as_attr_value).
            for block in &component.blocks {
                for op in &block.ops {
                    self.collect_runtime_needs_for_op(component, op);
                }
            }
        }

        // Global singleton defaults are module-scoped — collect their strings
        // (and any concat/record/list/filter machinery) so the module start
        // function can emit them without allocating on the heap. Filter calls
        // nested in a global default register under `None` (no owning comp).
        let global_default_exprs: Vec<LirExpr> = self.global_defaults.values().cloned().collect();
        for expr in &global_default_exprs {
            self.collect_strings_from_expr(expr);
            self.collect_concat_arities(expr);
            self.collect_record_types(expr);
            self.collect_list_constructs(expr);
            self.collect_runtime_needs(expr);
            self.collect_filter_calls(None, expr);
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
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
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
            LirExprKind::Range { start, end, .. } => {
                self.collect_strings_from_expr(start);
                self.collect_strings_from_expr(end);
            }
            LirExprKind::VariantCtor {
                payload: Some(p), ..
            } => {
                self.collect_strings_from_expr(p);
            }
            LirExprKind::VariantCtor { payload: None, .. } => {}
            LirExprKind::Closure { body, .. } => {
                for stmt in body {
                    if let yel_core::lir::expr::LirStatement::Expr(e) = stmt {
                        self.collect_strings_from_expr(e);
                    }
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
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
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
            LirExprKind::Range { start, end, .. } => {
                self.collect_concat_arities(start);
                self.collect_concat_arities(end);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_concat_arities(p);
                }
            }
            LirExprKind::IsCase { base, .. } => {
                self.collect_concat_arities(base);
            }
            LirExprKind::VariantField { base, .. } => {
                self.collect_concat_arities(base);
            }
            LirExprKind::Closure { body, .. } => {
                for stmt in body {
                    if let yel_core::lir::expr::LirStatement::Expr(e) = stmt {
                        self.collect_concat_arities(e);
                    }
                }
            }
            LirExprKind::GlobalCall { args, .. } => {
                for arg in args {
                    self.collect_concat_arities(arg);
                }
            }
            // Leaf expressions with no sub-expressions
            LirExprKind::SignalRead(_)
            | LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Collect record types that need constructor helpers.
    /// These are record types used in RecordConstruct expressions.
    fn collect_record_types(&mut self, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::RecordConstruct {
                record_def, fields, ..
            } => {
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
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
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
            LirExprKind::Range { start, end, .. } => {
                self.collect_record_types(start);
                self.collect_record_types(end);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_record_types(p);
                }
            }
            LirExprKind::IsCase { base, .. } => {
                self.collect_record_types(base);
            }
            LirExprKind::VariantField { base, .. } => {
                self.collect_record_types(base);
            }
            LirExprKind::Closure { body, .. } => {
                for stmt in body {
                    if let yel_core::lir::expr::LirStatement::Expr(e) = stmt {
                        self.collect_record_types(e);
                    }
                }
            }
            LirExprKind::GlobalCall { args, .. } => {
                for arg in args {
                    self.collect_record_types(arg);
                }
            }
            // Leaf expressions
            LirExprKind::SignalRead(_)
            | LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Collect list constructs from an expression (for runtime function generation).
    /// These are list literals that need runtime constructor helpers.
    fn collect_list_constructs(&mut self, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::ListConstruct { elements, .. } => {
                // Get element type from the list type
                let element_ty = match self.ctx.ty_kind(expr.ty) {
                    InternedTyKind::List(elem_ty) => *elem_ty,
                    _ => return, // Not a list type
                };

                let count = elements.len();
                let key = (element_ty, count);
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
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
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
            LirExprKind::Range { start, end, .. } => {
                self.collect_list_constructs(start);
                self.collect_list_constructs(end);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                // VariantCtor can have a payload expression (e.g., some(list_expr))
                if let Some(p) = payload {
                    self.collect_list_constructs(p);
                }
            }
            LirExprKind::IsCase { base, .. } => {
                self.collect_list_constructs(base);
            }
            LirExprKind::VariantField { base, .. } => {
                self.collect_list_constructs(base);
            }
            LirExprKind::Closure { body, .. } => {
                // Closures contain statements with expressions
                for stmt in body {
                    if let yel_core::lir::expr::LirStatement::Expr(e) = stmt {
                        self.collect_list_constructs(e);
                    }
                }
            }
            LirExprKind::GlobalCall { args, .. } => {
                for arg in args {
                    self.collect_list_constructs(arg);
                }
            }
            // Leaf expressions
            LirExprKind::SignalRead(_)
            | LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Walk an expression tree and turn on `runtime_needs.X` flags for
    /// every helper any reachable emit-site will eventually call. The
    /// scan must over-approximate the live set rather than under-: a
    /// missed flag turns a real `Call(idx)` into a `Call(None.unwrap())`
    /// at codegen time. Conservative heuristic: any `Call { func: "X" }`
    /// with a known builtin name flips the matching helper on, and any
    /// expression typed as a primitive scalar that could route through
    /// `emit_expr_as_string` / `emit_expr_as_attr_value` flips its
    /// to_string helper on too (handled per-op in
    /// `collect_runtime_needs_for_op`).
    fn collect_runtime_needs(&mut self, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::Call { func, args } => {
                let func_name = self.ctx.str(self.ctx.defs.name(*func));
                match func_name.as_str() {
                    "s32-to-string" | "u32-to-string" | "char-to-string" => {
                        self.runtime_needs.s32_to_string = true;
                    }
                    "s64-to-string" | "u64-to-string" => {
                        self.runtime_needs.s64_to_string = true;
                    }
                    "bool-to-string" => self.runtime_needs.bool_to_string = true,
                    "f32-to-string" => self.runtime_needs.f32_to_string = true,
                    "f64-to-string" => {
                        // f64-to-string demotes to f32 then calls f32_to_string.
                        self.runtime_needs.f32_to_string = true;
                    }
                    "starts-with" | "starts_with" => self.runtime_needs.starts_with = true,
                    "append" => {
                        // list.append(elem) — register a per-list-Ty
                        // runtime helper. The receiver (args[0]) carries
                        // the list type.
                        if let Some(receiver) = args.first() {
                            let list_ty = receiver.ty;
                            if !self.list_appends.contains(&list_ty) {
                                self.list_appends.push(list_ty);
                            }
                        }
                    }
                    _ => {}
                }
                for arg in args {
                    self.collect_runtime_needs(arg);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_runtime_needs(lhs);
                self.collect_runtime_needs(rhs);
            }
            LirExprKind::Unary { operand, .. } => self.collect_runtime_needs(operand),
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_runtime_needs(condition);
                self.collect_runtime_needs(then_expr);
                self.collect_runtime_needs(else_expr);
            }
            LirExprKind::Field { base, .. } => self.collect_runtime_needs(base),
            LirExprKind::Index { base, index } => {
                self.collect_runtime_needs(base);
                self.collect_runtime_needs(index);
            }
            LirExprKind::ListConstruct { elements, .. }
            | LirExprKind::TupleConstruct { elements, .. } => {
                for e in elements {
                    self.collect_runtime_needs(e);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for f in fields {
                    self.collect_runtime_needs(f);
                }
            }
            LirExprKind::Range { start, end, .. } => {
                self.collect_runtime_needs(start);
                self.collect_runtime_needs(end);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_runtime_needs(p);
                }
            }
            LirExprKind::IsCase { base, .. } | LirExprKind::VariantField { base, .. } => {
                self.collect_runtime_needs(base);
            }
            LirExprKind::Closure { body, .. } => {
                for stmt in body {
                    if let yel_core::lir::expr::LirStatement::Expr(e) = stmt {
                        self.collect_runtime_needs(e);
                    }
                }
            }
            LirExprKind::GlobalCall { args, .. } => {
                for a in args {
                    self.collect_runtime_needs(a);
                }
            }
            LirExprKind::SignalRead(_)
            | LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {}
        }
    }

    /// Inspect a single LIR op to detect helpers triggered by ops
    /// (rather than expressions). The Push* stack-prefix ops route
    /// expr payloads through `emit_expr_as_string` /
    /// `emit_expr_as_attr_value`, which dispatch by type and call one
    /// of the to_string runtime fns. `SignalWriteExpr` to a
    /// FatPointer signal needs `store_fat_ptr`.
    fn collect_runtime_needs_for_op(
        &mut self,
        component: &LirResource,
        op: &yel_core::lir::block::LirOp,
    ) {
        use yel_core::lir::block::LirOp;
        match op {
            // Phase 2.2b: the Push* stack-prefix ops carry the expr
            // payload that the legacy DOM ops embedded. Mirror the
            // runtime-needs side-effects so stringify / fat-ptr helpers
            // remain materialized.
            LirOp::PushExprAsString { expr } => {
                let e = component.get_expr(*expr);
                self.note_to_string_for_ty(e.ty);
                self.collect_runtime_needs(e);
            }
            LirOp::PushExprAsAttrValue { expr } => {
                let e = component.get_expr(*expr);
                self.runtime_needs.pack_fat_ptr_to_i64 = true;
                self.note_to_string_for_ty(e.ty);
                self.collect_runtime_needs(e);
            }
            _ => {}
        }
    }

    /// Set the to_string flag matching a type's `emit_expr_as_string`
    /// dispatch (mirrors the type-table at `expr.rs::emit_expr_as_string`).
    fn note_to_string_for_ty(&mut self, ty: Ty) {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::S32
            | InternedTyKind::U32
            | InternedTyKind::S8
            | InternedTyKind::S16
            | InternedTyKind::U8
            | InternedTyKind::U16
            | InternedTyKind::Char => {
                self.runtime_needs.s32_to_string = true;
            }
            InternedTyKind::S64 | InternedTyKind::U64 => {
                self.runtime_needs.s64_to_string = true;
            }
            InternedTyKind::Bool => self.runtime_needs.bool_to_string = true,
            InternedTyKind::F32 | InternedTyKind::F64 => {
                self.runtime_needs.f32_to_string = true;
            }
            _ => {}
        }
    }

    /// Note runtime needs implied by a signal's storage shape.
    ///
    /// FatPointer signals (today only the rare unmigrated string /
    /// raw-pointer-list shapes) trigger load_fat_ptr / store_fat_ptr.
    /// We can't consult `internal_repr` here because the GC type
    /// registry isn't populated yet at collection time, so the gating
    /// stays structural: any String or List type counts. Over-flag is
    /// safe (helpers stay alive); under-flag would crash codegen.
    fn note_signal_runtime_needs(&mut self, ty: Ty) {
        match self.ctx.ty_kind(ty) {
            InternedTyKind::String | InternedTyKind::List(_) => {
                self.runtime_needs.load_fat_ptr = true;
                self.runtime_needs.store_fat_ptr = true;
            }
            _ => {}
        }
    }

    /// Collect filter calls from an expression (for function generation).
    /// Each filter call gets a unique ID (index into filter_calls Vec).
    ///
    /// `comp_idx` is `Some(i)` when walking an expression that lives inside
    /// component `i`, `None` when walking a module-scope expression (e.g. a
    /// global-singleton default).
    fn collect_filter_calls(&mut self, comp_idx: Option<usize>, expr: &LirExpr) {
        match &expr.kind {
            LirExprKind::Call { func, args } => {
                let func_name = self.ctx.str(self.ctx.defs.name(*func));
                if func_name == "filter" && args.len() == 2 {
                    // Extract closure from second arg
                    if let LirExprKind::Closure { params, body } = &args[1].kind {
                        // Get element type and size from source list
                        if let InternedTyKind::List(elem_ty) = self.ctx.ty_kind(args[0].ty) {
                            let elem_size = self.layout_ctx.size_of(*elem_ty);

                            // Get predicate expression (last statement in body)
                            if let Some(yel_core::lir::expr::LirStatement::Expr(predicate)) =
                                body.last()
                                && let Some(param) = params.first()
                            {
                                self.filter_calls.push((
                                    comp_idx,
                                    args[0].ty,
                                    elem_size,
                                    *param,
                                    predicate.clone(),
                                ));
                            }
                        }
                    }
                }
                // Recurse into args
                for arg in args {
                    self.collect_filter_calls(comp_idx, arg);
                }
            }
            LirExprKind::Binary { lhs, rhs, .. } => {
                self.collect_filter_calls(comp_idx, lhs);
                self.collect_filter_calls(comp_idx, rhs);
            }
            LirExprKind::Unary { operand, .. } => {
                self.collect_filter_calls(comp_idx, operand);
            }
            LirExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                self.collect_filter_calls(comp_idx, condition);
                self.collect_filter_calls(comp_idx, then_expr);
                self.collect_filter_calls(comp_idx, else_expr);
            }
            LirExprKind::Field { base, .. } => {
                self.collect_filter_calls(comp_idx, base);
            }
            LirExprKind::Index { base, index } => {
                self.collect_filter_calls(comp_idx, base);
                self.collect_filter_calls(comp_idx, index);
            }
            LirExprKind::ListConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_filter_calls(comp_idx, elem);
                }
            }
            LirExprKind::RecordConstruct { fields, .. } => {
                for field in fields {
                    self.collect_filter_calls(comp_idx, field);
                }
            }
            LirExprKind::TupleConstruct { elements, .. } => {
                for elem in elements {
                    self.collect_filter_calls(comp_idx, elem);
                }
            }
            LirExprKind::Range { start, end, .. } => {
                self.collect_filter_calls(comp_idx, start);
                self.collect_filter_calls(comp_idx, end);
            }
            LirExprKind::VariantCtor { payload, .. } => {
                if let Some(p) = payload {
                    self.collect_filter_calls(comp_idx, p);
                }
            }
            LirExprKind::IsCase { base, .. } => {
                self.collect_filter_calls(comp_idx, base);
            }
            LirExprKind::VariantField { base, .. } => {
                self.collect_filter_calls(comp_idx, base);
            }
            LirExprKind::Closure { body, .. } => {
                for stmt in body {
                    if let yel_core::lir::expr::LirStatement::Expr(e) = stmt {
                        self.collect_filter_calls(comp_idx, e);
                    }
                }
            }
            LirExprKind::GlobalCall { args, .. } => {
                for arg in args {
                    self.collect_filter_calls(comp_idx, arg);
                }
            }
            // Leaf expressions
            LirExprKind::SignalRead(_)
            | LirExprKind::Local(_)
            | LirExprKind::Def(_)
            | LirExprKind::Literal(_)
            | LirExprKind::EnumCase { .. }
            | LirExprKind::ListStatic { .. } => {
                // No sub-expressions to traverse
            }
        }
    }

    /// Count the total number of WASM parameters needed to pass all fields of
    /// a record under the canonical-ABI flat representation. Delegates to
    /// [`flatten_record_fields_valtypes`] so it stays in lockstep with the
    /// Type-section entry registered for the record ctor.
    pub fn count_record_wasm_params(&self, record_def: DefId) -> usize {
        self.flatten_record_fields_valtypes(record_def).len()
    }

    /// Count the number of WASM parameters needed to pass a value of the given type
    /// under the canonical ABI "flat" representation.
    /// - Primitives (s32, bool, etc.): 1 param
    /// - Strings and lists: 2 params (ptr + len)
    /// - Records: recursive sum of flatten(field)
    /// - Option<T>: 1 (discriminant) + flatten(T)
    /// - Result<O, E>: 1 + slot-wise join(flatten(O), flatten(E))
    /// - Variant { case1(T1), case2(T2), ... }: 1 + slot-wise join over cases
    /// - Enum (no payloads): 1
    pub fn count_type_wasm_params(&self, ty: Ty) -> usize {
        self.flatten_core_valtypes(ty).len()
    }

    /// Compute the canonical ABI "flat" core-module param types for a value of `ty`.
    /// Used for both core function signatures and (via [`flatten_core_slots`])
    /// setter body emission.
    /// Canonical-ABI flattening — never returns GC refs. Used at the
    /// WIT boundary (callback imports, exported signal setters,
    /// `collect_flat_slots` linear-memory layout). Internal call sites
    /// should use `flatten_core_valtypes`, which collapses scalar
    /// lists (and, eventually, more composites) to single GC ref
    /// slots.
    pub fn canonical_flat_valtypes(&self, ty: Ty) -> Vec<wasm_encoder::ValType> {
        use wasm_encoder::ValType;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => vec![ValType::F32],
            InternedTyKind::F64 => vec![ValType::F64],
            InternedTyKind::S64 | InternedTyKind::U64 => vec![ValType::I64],
            InternedTyKind::String | InternedTyKind::List(_) => {
                vec![ValType::I32, ValType::I32]
            }
            InternedTyKind::Option(inner) => {
                let mut v = vec![ValType::I32];
                v.extend(self.canonical_flat_valtypes(*inner));
                v
            }
            InternedTyKind::Result { ok, err } => {
                let ok_flat = ok
                    .map(|t| self.canonical_flat_valtypes(t))
                    .unwrap_or_default();
                let err_flat = err
                    .map(|t| self.canonical_flat_valtypes(t))
                    .unwrap_or_default();
                let mut v = vec![ValType::I32];
                v.extend(join_flat_valtypes(&ok_flat, &err_flat));
                v
            }
            InternedTyKind::Tuple(elements) => {
                let mut v = Vec::new();
                for t in elements {
                    v.extend(self.canonical_flat_valtypes(*t));
                }
                v
            }
            InternedTyKind::Adt(def_id) => {
                if let Some(rec_def) = self.ctx.defs.as_record(*def_id) {
                    let mut v = Vec::new();
                    for &field_def_id in &rec_def.fields {
                        let field_ty = match self.ctx.defs.kind(field_def_id) {
                            DefKind::Field(f) => f.ty,
                            _ => continue,
                        };
                        v.extend(self.canonical_flat_valtypes(field_ty));
                    }
                    v
                } else if let Some(var_def) = self.ctx.defs.as_variant(*def_id) {
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
                        joined = join_flat_valtypes(&joined, f);
                    }
                    let mut v = vec![ValType::I32];
                    v.extend(joined);
                    v
                } else {
                    vec![ValType::I32]
                }
            }
            _ => vec![ValType::I32],
        }
    }

    pub fn flatten_core_valtypes(&self, ty: Ty) -> Vec<wasm_encoder::ValType> {
        use wasm_encoder::{HeapType, RefType, ValType};
        // Phase 5b-v.3: scalar lists collapse to a single typed
        // GC array ref slot internally. Canonical-ABI boundary code
        // (collect_flat_slots, callback imports, exported signal
        // setters) must use `canonical_flat_valtypes` instead — that
        // path keeps the multi-slot (ptr, len) shape required by the
        // WIT canonical ABI.
        if self.is_scalar_list_ty(ty) {
            if let Some(&arr_idx) = self.record_gc_types.list_array_type_idx.get(&ty) {
                return vec![ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(arr_idx),
                })];
            }
        }
        // Option-of-ref collapse: option<T> where T's internal repr
        // is itself a GC ref becomes a single nullable ref slot.
        if let Some(arr_idx) = self.option_collapses_to_ref(ty) {
            return vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(arr_idx),
            })];
        }
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => vec![ValType::F32],
            InternedTyKind::F64 => vec![ValType::F64],
            InternedTyKind::S64 | InternedTyKind::U64 => vec![ValType::I64],
            InternedTyKind::String | InternedTyKind::List(_) => {
                vec![ValType::I32, ValType::I32] // ptr, len
            }
            InternedTyKind::Option(inner) => {
                let mut v = vec![ValType::I32]; // discriminant
                v.extend(self.flatten_core_valtypes(*inner));
                v
            }
            InternedTyKind::Result { ok, err } => {
                let ok_flat = ok
                    .map(|t| self.flatten_core_valtypes(t))
                    .unwrap_or_default();
                let err_flat = err
                    .map(|t| self.flatten_core_valtypes(t))
                    .unwrap_or_default();
                let mut v = vec![ValType::I32]; // discriminant
                v.extend(join_flat_valtypes(&ok_flat, &err_flat));
                v
            }
            InternedTyKind::Tuple(elements) => {
                // Canonical ABI: tuple<T1, T2, ...> flattens to the
                // concatenation of each element's flattening (no
                // discriminant, unlike variants/options).
                let mut v = Vec::new();
                for t in elements {
                    v.extend(self.flatten_core_valtypes(*t));
                }
                v
            }
            InternedTyKind::Adt(def_id) => {
                // Record: recursive flattening of fields.
                if let Some(rec_def) = self.ctx.defs.as_record(*def_id) {
                    let mut v = Vec::new();
                    for &field_def_id in &rec_def.fields {
                        let field_ty = match self.ctx.defs.kind(field_def_id) {
                            DefKind::Field(f) => f.ty,
                            _ => continue,
                        };
                        v.extend(self.flatten_core_valtypes(field_ty));
                    }
                    v
                } else if let Some(var_def) = self.ctx.defs.as_variant(*def_id) {
                    // Variant: 1 discriminant + join over all case payload flattenings.
                    let mut case_flats: Vec<Vec<ValType>> = Vec::new();
                    for &case_def_id in &var_def.cases {
                        let payload = match self.ctx.defs.kind(case_def_id) {
                            yel_core::definitions::DefKind::VariantCase(c) => c.payload,
                            _ => None,
                        };
                        case_flats.push(
                            payload
                                .map(|t| self.flatten_core_valtypes(t))
                                .unwrap_or_default(),
                        );
                    }
                    let mut joined: Vec<ValType> = Vec::new();
                    for f in &case_flats {
                        joined = join_flat_valtypes(&joined, f);
                    }
                    let mut v = vec![ValType::I32];
                    v.extend(joined);
                    v
                } else {
                    // Enum: single discriminant.
                    vec![ValType::I32]
                }
            }
            // Primitives (including small ints / bool / char) — 1 slot, widened to i32.
            _ => vec![ValType::I32],
        }
    }

    /// Flatten the fields of a record definition in declaration order (as if
    /// the record value itself were being flattened). Useful for record
    /// constructor signatures where the whole record shape is implied by its
    /// DefId rather than a `Ty`.
    pub fn flatten_record_fields_valtypes(&self, record_def: DefId) -> Vec<wasm_encoder::ValType> {
        let rec_def = match self.ctx.defs.as_record(record_def) {
            Some(r) => r,
            None => return Vec::new(),
        };
        let mut v = Vec::new();
        for &field_def_id in &rec_def.fields {
            let field_ty = match self.ctx.defs.kind(field_def_id) {
                DefKind::Field(f) => f.ty,
                _ => continue,
            };
            // Phase 5e.4: record ctor params follow canonical ABI
            // (each list/string field takes 2 i32 = ptr+len). Records
            // whose fields are FlatGcStruct-migrated reach this path
            // ONLY for non-DTR records (memory-backed); DTR records
            // use the SLR struct.new path in expr.rs which never
            // calls record_ctor.
            v.extend(self.canonical_flat_valtypes(field_ty));
        }
        v
    }

    /// Compute per-slot store descriptors for a value of `ty` laid out in memory
    /// at an implicit base. Each entry is (param_valtype, in_memory_offset,
    /// store_width). Used by setter body emission to copy each flattened param
    /// to the right byte offset under its target type's alignment rules.
    pub fn flatten_core_slots(&mut self, ty: Ty) -> Vec<FlatSlot> {
        let mut out = Vec::new();
        self.collect_flat_slots(ty, 0, &mut out);
        out
    }

    fn collect_flat_slots(&mut self, ty: Ty, base_offset: u32, out: &mut Vec<FlatSlot>) {
        use wasm_encoder::ValType;
        match self.ctx.ty_kind(ty) {
            InternedTyKind::F32 => out.push(FlatSlot {
                valtype: ValType::F32,
                offset: base_offset,
                store: StoreWidth::F32,
            }),
            InternedTyKind::F64 => out.push(FlatSlot {
                valtype: ValType::F64,
                offset: base_offset,
                store: StoreWidth::F64,
            }),
            InternedTyKind::S64 | InternedTyKind::U64 => out.push(FlatSlot {
                valtype: ValType::I64,
                offset: base_offset,
                store: StoreWidth::I64,
            }),
            InternedTyKind::Bool
            | InternedTyKind::U8
            | InternedTyKind::S8
            | InternedTyKind::Char => {
                // Char is 4-byte in canonical ABI memory layout; bool/u8/s8 are 1-byte.
                let store = match self.ctx.ty_kind(ty) {
                    InternedTyKind::Char => StoreWidth::I32,
                    _ => StoreWidth::I32_8,
                };
                out.push(FlatSlot {
                    valtype: ValType::I32,
                    offset: base_offset,
                    store,
                });
            }
            InternedTyKind::U16 | InternedTyKind::S16 => out.push(FlatSlot {
                valtype: ValType::I32,
                offset: base_offset,
                store: StoreWidth::I32_16,
            }),
            InternedTyKind::String | InternedTyKind::List(_) => {
                // ptr at +0, len at +4
                out.push(FlatSlot {
                    valtype: ValType::I32,
                    offset: base_offset,
                    store: StoreWidth::I32,
                });
                out.push(FlatSlot {
                    valtype: ValType::I32,
                    offset: base_offset + 4,
                    store: StoreWidth::I32,
                });
            }
            InternedTyKind::Option(inner) => {
                // Discriminant at +0 (1 byte), payload at aligned offset.
                let inner_layout = self.layout_ctx.layout_of(*inner);
                let payload_offset = align_to(1, inner_layout.align);
                out.push(FlatSlot {
                    valtype: ValType::I32,
                    offset: base_offset,
                    store: StoreWidth::I32_8,
                });
                self.collect_flat_slots(*inner, base_offset + payload_offset, out);
            }
            InternedTyKind::Result { ok, err } => {
                // Canonical-ABI Result: 1-byte discriminant, then joined
                // payload slots laid out back-to-back at their natural
                // sizes starting at offset 4 (aligned for i32).
                out.push(FlatSlot {
                    valtype: ValType::I32,
                    offset: base_offset,
                    store: StoreWidth::I32_8,
                });
                let ok_flat = ok
                    .map(|t| self.canonical_flat_valtypes(t))
                    .unwrap_or_default();
                let err_flat = err
                    .map(|t| self.canonical_flat_valtypes(t))
                    .unwrap_or_default();
                let joined = join_flat_valtypes(&ok_flat, &err_flat);
                let payload_base = base_offset + 4;
                let mut slot_off = 0u32;
                for vt in &joined {
                    let (store, size) = match vt {
                        ValType::I32 => (StoreWidth::I32, 4u32),
                        ValType::I64 => (StoreWidth::I64, 8u32),
                        ValType::F32 => (StoreWidth::F32, 4u32),
                        ValType::F64 => (StoreWidth::F64, 8u32),
                        _ => (StoreWidth::I32, 4u32),
                    };
                    out.push(FlatSlot {
                        valtype: *vt,
                        offset: payload_base + slot_off,
                        store,
                    });
                    slot_off += size;
                }
            }
            InternedTyKind::Tuple(elements) => {
                // Canonical-ABI tuple memory layout: elements placed in
                // declaration order, each aligned to its own alignment. No
                // discriminant. Mirrors `LayoutContext::compute_tuple_layout`
                // so the offsets line up with whatever the layout context
                // would tell us.
                let elems: Vec<Ty> = elements.to_vec();
                let mut offset: u32 = 0;
                for elem_ty in elems {
                    let elem_layout = self.layout_ctx.layout_of(elem_ty);
                    offset = align_to(offset, elem_layout.align);
                    self.collect_flat_slots(elem_ty, base_offset + offset, out);
                    offset += elem_layout.size;
                }
            }
            InternedTyKind::Adt(def_id) => {
                if let Some(rec_def) = self.ctx.defs.as_record(*def_id) {
                    // Use record layout for correct field offsets.
                    let layout = self
                        .layout_ctx
                        .record_layout_by_id(*def_id)
                        .expect("record layout must exist for declared record");
                    let fields = rec_def.fields.clone();
                    for (i, &_field_def_id) in fields.iter().enumerate() {
                        let (_, field_off, field_ty) = layout.field_offsets[i].clone();
                        self.collect_flat_slots(field_ty, base_offset + field_off, out);
                    }
                } else if let Some(var_def) = self.ctx.defs.as_variant(*def_id) {
                    // User variant: discriminant at base, joined payload
                    // slots laid out back-to-back starting at the variant
                    // layout's payload_offset.
                    out.push(FlatSlot {
                        valtype: ValType::I32,
                        offset: base_offset,
                        store: StoreWidth::I32_8,
                    });
                    let vd = var_def.clone();
                    let var_layout = self.layout_ctx.compute_variant_layout_from_def_public(&vd);
                    let payload_offset = var_layout.payload_offset;
                    let mut case_flats: Vec<Vec<ValType>> = Vec::new();
                    for &case_def_id in &vd.cases {
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
                        joined = join_flat_valtypes(&joined, f);
                    }
                    let mut slot_off = 0u32;
                    for vt in &joined {
                        let (store, size) = match vt {
                            ValType::I32 => (StoreWidth::I32, 4u32),
                            ValType::I64 => (StoreWidth::I64, 8u32),
                            ValType::F32 => (StoreWidth::F32, 4u32),
                            ValType::F64 => (StoreWidth::F64, 8u32),
                            _ => (StoreWidth::I32, 4u32),
                        };
                        out.push(FlatSlot {
                            valtype: *vt,
                            offset: base_offset + payload_offset + slot_off,
                            store,
                        });
                        slot_off += size;
                    }
                } else {
                    // Enum: single i32 discriminant at offset 0.
                    out.push(FlatSlot {
                        valtype: ValType::I32,
                        offset: base_offset,
                        store: StoreWidth::I32,
                    });
                }
            }
            _ => out.push(FlatSlot {
                valtype: ValType::I32,
                offset: base_offset,
                store: StoreWidth::I32,
            }),
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

    /// Get signal name by DefId. Returns `ArcStr` (cheap to clone;
    /// derefs to `&str`) so hot-path emitters can avoid the per-call
    /// `String` allocation.
    pub fn signal_name(&self, def_id: DefId) -> yel_core::ArcStr {
        let name = self.ctx.defs.name(def_id);
        self.ctx.str(name)
    }

    /// Get signal index by DefId within a specific component
    pub fn signal_index_in(&self, component: &LirResource, def_id: DefId) -> Option<usize> {
        component.signals.iter().position(|s| s.def_id == def_id)
    }

    /// Position of `component` in `self.components` — the index used
    /// to look up `self.gc_layouts[i]` and other per-component data.
    /// Returns `None` only for the empty `MemoryLayout::empty_for_module`
    /// carrier used during global-defaults emission, where no component
    /// owns the expressions being lowered.
    pub fn comp_idx_of(&self, component: &LirResource) -> Option<usize> {
        self.components
            .iter()
            .position(|c| c.def_id == component.def_id)
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
            if seg
                .chars()
                .next()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
            {
                // Segment starts with digit, prefix with 'n'
                format!("n{}", seg)
            } else {
                seg.to_string()
            }
        })
        .collect();

    segments.join("-")
}

/// Spawn Binaryen's `wasm-opt` and pipe `input_bytes` through it.
/// `extra_args` are forwarded verbatim after the `-i`/`-o` paths,
/// e.g. `["-O3", "--enable-gc"]`.
#[cfg(not(target_family = "wasm"))]
fn run_wasm_opt(input_bytes: &[u8], extra_args: &[String]) -> Result<Vec<u8>, CodegenError> {
    use std::io::Write;
    use std::process::Command;

    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let tmp_dir = std::env::temp_dir();
    let in_path = tmp_dir.join(format!("yel-wasm-opt-in-{}-{}.wasm", pid, nanos));
    let out_path = tmp_dir.join(format!("yel-wasm-opt-out-{}-{}.wasm", pid, nanos));

    {
        let mut f = std::fs::File::create(&in_path).map_err(|e| {
            CodegenError::EncodingError(format!("wasm-opt: cannot create input temp file: {}", e))
        })?;
        f.write_all(input_bytes).map_err(|e| {
            CodegenError::EncodingError(format!("wasm-opt: cannot write input temp file: {}", e))
        })?;
    }

    let output = Command::new("wasm-opt")
        .arg(&in_path)
        .arg("-o")
        .arg(&out_path)
        .args(extra_args)
        .output();

    let result = match output {
        Ok(out) if out.status.success() => std::fs::read(&out_path).map_err(|e| {
            CodegenError::EncodingError(format!("wasm-opt: cannot read output: {}", e))
        }),
        Ok(out) => {
            let stderr = String::from_utf8_lossy(&out.stderr);
            Err(CodegenError::EncodingError(format!(
                "wasm-opt failed (status {}): {}",
                out.status, stderr
            )))
        }
        Err(e) => Err(CodegenError::EncodingError(format!(
            "wasm-opt: failed to spawn (is it on PATH?): {}",
            e
        ))),
    };

    let _ = std::fs::remove_file(&in_path);
    let _ = std::fs::remove_file(&out_path);
    result
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

    /// KNOWN BUG: `to_kebab_case` currently inserts `-` between every
    /// consecutive pair of uppercase letters, so an acronym like
    /// `HTTPServer` becomes `h-t-t-p-server` instead of `http-server`.
    /// Correct kebab-case treats acronyms as contiguous runs and only
    /// inserts a separator where an uppercase is *preceded* by a
    /// lowercase (or where an acronym ends because the next char starts
    /// a new word — `HTTPServer` → `HTTP` + `Server`).
    ///
    /// This test asserts the **correct** expected behaviour and is
    /// `#[ignore]`d so the crate builds green — `cargo test -- --ignored`
    /// surfaces the bug. Remove the `#[ignore]` when fixed.
    #[test]
    #[ignore = "known bug: to_kebab_case splits acronyms incorrectly — \
                 `HTTPServer` → `h-t-t-p-server` instead of `http-server`"]
    fn to_kebab_case_handles_acronyms_and_boundaries() {
        // Consecutive capitals (acronyms) should stay contiguous and
        // only split where a lowercase follows.
        assert_eq!(to_kebab_case("HTTPServer"), "http-server");
        assert_eq!(to_kebab_case("parseURL"), "parse-url");
        // Already-kebab input should round-trip unchanged.
        assert_eq!(to_kebab_case("my-widget"), "my-widget");
        // Empty input is accepted (no panic) and returns empty.
        assert_eq!(to_kebab_case(""), "");
        // A single character stays lowercase.
        assert_eq!(to_kebab_case("A"), "a");
    }

    #[test]
    fn test_to_wit_name() {
        assert_eq!(to_wit_name("counter"), "counter");
        assert_eq!(to_wit_name("item8"), "item8");
        assert_eq!(to_wit_name("8item"), "n8item");
    }

    #[test]
    fn to_wit_name_handles_all_digit_names() {
        // All-digit names are unusual but valid Yel identifiers in some
        // contexts. WIT requires the first character to be a letter —
        // the `n` prefix keeps the name unique while making it legal.
        assert_eq!(to_wit_name("1"), "n1");
        assert_eq!(to_wit_name("123"), "n123");
    }

    /// `StringData::intern` is idempotent: the same string is only stored
    /// once regardless of how many times it's requested. Fatal regression
    /// if data-segment size grows linearly with duplicate interns.
    #[test]
    fn string_data_interning_is_idempotent() {
        let mut strings = runtime::StringData::new(256);
        let (p1, l1) = strings.intern("hello");
        let (p2, l2) = strings.intern("hello");
        let (p3, _) = strings.intern("world");
        assert_eq!(
            (p1, l1),
            (p2, l2),
            "same string must intern to the same (ptr, len) pair"
        );
        assert_ne!(p1, p3, "different strings must get different pointers");
    }

    /// Pre-intern common strings (`true`, `false`, etc.) once — repeat
    /// calls shouldn't waste space. Checks the raw interning contract so
    /// the builder's `collect_strings` pass can't accidentally grow the
    /// data section by duplicating the same string across components.
    #[test]
    fn repeated_string_interns_share_storage() {
        let mut strings = runtime::StringData::new(256);
        let before_size = strings.size();
        let (ptr, _) = strings.intern("foo");
        let mid_size = strings.size();
        // Second intern of the same string must not grow the data segment.
        let (ptr2, _) = strings.intern("foo");
        let after_size = strings.size();
        assert_eq!(ptr, ptr2);
        assert!(
            mid_size > before_size,
            "first intern should grow the segment"
        );
        assert_eq!(
            after_size, mid_size,
            "repeat intern must not grow the segment"
        );
    }

    /// `MemoryLayout::empty_for_module` is used when emitting module-scope
    /// expressions that don't belong to any component (e.g. global
    /// defaults in the start function). It must produce a valid layout
    /// with no signal slots — any `signal_addr(_)` call on it would
    /// panic, which is the desired behaviour (module scope has no
    /// component-local signals). The self-handle slot moved off the
    /// `MemoryLayout` to a `(mut i32)` field on `$Comp_<Name>`; the
    /// invariant tested by the older self-handle test is now structural
    /// (nothing in linear memory to overlap with).
    #[test]
    fn empty_module_layout_has_zero_state() {
        let layout = MemoryLayout::empty_for_module();
        assert_eq!(layout.base, 0);
        assert_eq!(layout.size, 0);
        assert!(layout.signal_offsets.is_empty());
    }
}
