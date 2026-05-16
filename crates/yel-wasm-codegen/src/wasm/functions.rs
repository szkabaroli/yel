//! Top-level wasm-function module assembly.
//!
//! Sibling to [`generate_wasm_module`](super::generate_wasm_module),
//! which builds a UI-component wasm component (mount / unmount /
//! signals / effects / WIT resources). This module builds a **core
//! wasm module from a flat list of top-level functions** — no UI
//! scaffolding, no component-model wrapping.
//!
//! Callers: anything producing LIR functions that aren't UI
//! components. Today that's the flow-graph frontend; future yel-lang
//! free functions and any direct-emit frontend would consume the
//! same path.
//!
//! ## What this module does
//!
//! 1. **Drive a slimmed-down module assembly.** Build the wasm type
//!    section, function section, export section, and code section
//!    directly — bypassing every UI-flavoured pass in
//!    [`WasmPackageBuilder::build_core_module`].
//!
//! 2. **Reuse the body emitter.** Per-function body emission goes
//!    through [`WasmPackageBuilder::emit_op`], which already handles
//!    every LIR op (~1700 lines) and every expression kind (~3000
//!    lines). No duplication.
//!
//! 3. **Reuse the type-section helper.** L3-v2 Phase 2's
//!    `register_wasm_function_type` converts each function's
//!    `CallingConv` into a wasm function type for both UI blocks and
//!    free functions.
//!
//! ## What's caller-specific
//!
//! * The list of [`FunctionInput`] entries — caller decides which
//!   `LirResource`s + `LirFunction` metadata go in.
//! * Module-level orchestration (which sections, in what order).
//! * `def_id_to_func_idx` pre-population so `LirOp::CallFunction`
//!   resolves during body emit.
//! * Minimal `GcTypeLayout` placeholders so `emit_op`'s
//!   `gc_layouts[i]` lookups don't panic — non-UI callers don't emit
//!   the ops that actually read those layouts, but the index must
//!   exist.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Module, TypeSection,
};
use wit_parser::{Resolve, WorldId};
use yel_core::context::CompilerContext;
use yel_core::ids::{BlockId, DefId};
use yel_core::lir::block::LirSlotValType;
use yel_core::lir::function::{ExportShape, FunctionRole, LirFunction};
use yel_core::lir::node::LirResource;

use crate::CodegenError;
use crate::wasm::WasmPackageBuilder;

/// One top-level wasm function's inputs to the builder. Callers
/// construct these in module order: `[(resource, lir_fn), ...]`.
///
/// The body lives at `resource.blocks[0]` (a one-block resource by
/// caller convention); slots / exprs are on the resource itself. The
/// `LirFunction` carries the wasm calling convention and identity
/// (`FreeFunction { def_id, is_export }`).
pub struct FunctionInput<'a> {
    pub resource: &'a LirResource,
    pub lir_fn: &'a LirFunction,
}

/// Build a standalone core wasm module from a flat list of top-level
/// functions. The output is raw core wasm — not a wasm component —
/// so it lacks WIT bindings; callers that want a component wrap
/// externally (e.g. `wit_component::ComponentEncoder`).
///
/// This is the non-UI sibling of [`crate::generate_wasm_module`]:
/// no mount/unmount/signals/effects pipeline, no WIT resource
/// encoding. Functions whose `LirFunction::export_shape()` is
/// `FreeFunction` get a world-level export by their `DefId`-resolved
/// name; other shapes are emitted as internal-only functions.
pub fn generate_function_module(
    ctx: &CompilerContext,
    inputs: &[FunctionInput<'_>],
) -> Result<Vec<u8>, CodegenError> {
    generate_function_module_with_names(ctx, inputs, &HashMap::new())
}

/// Variant of [`generate_function_module`] that lets the caller
/// override the wasm export name for selected functions.
///
/// Used by wit-component-aware callers that need their interface
/// methods exported under `<pkg>:<name>/<iface>@<ver>#<fn>` so the
/// component encoder can bind them to the corresponding WIT entry.
/// Functions absent from `export_names` fall back to `ctx.str(def.name)`.
pub fn generate_function_module_with_names(
    ctx: &CompilerContext,
    inputs: &[FunctionInput<'_>],
    export_names: &HashMap<DefId, String>,
) -> Result<Vec<u8>, CodegenError> {
    // Reify the inputs as a `Vec<LirResource>` for the builder.
    let resources: Vec<LirResource> = inputs.iter().map(|i| i.resource.clone()).collect();
    let mut builder = WasmPackageBuilder::new(&resources, ctx);

    // Empty placeholder layouts so `emit_op`'s `gc_layouts[comp_idx]`
    // index lookups don't panic. Non-UI callers don't emit any op that
    // dereferences these (no MountComponent, no SignalRead, no
    // boundary-typed slots).
    builder.gc_layouts = inputs
        .iter()
        .map(|_| crate::wasm::gc_types::GcTypeLayout::default())
        .collect();

    let mut module = Module::new();
    let mut types = TypeSection::new();
    let mut funcs = FunctionSection::new();
    let mut exports = ExportSection::new();
    let mut codes = CodeSection::new();

    // Pass 1: pre-populate the DefId → wasm-func-idx map so
    // `LirOp::CallFunction` resolves during body emit (handled by
    // emit_op via `self.def_id_to_func_idx`).
    let mut def_to_idx: HashMap<DefId, u32> = HashMap::new();
    for (i, input) in inputs.iter().enumerate() {
        if let FunctionRole::FreeFunction { def_id, .. } = &input.lir_fn.role {
            def_to_idx.insert(*def_id, i as u32);
            // Mirror into the builder so emit_op's CallFunction arm
            // can resolve cross-function calls.
            builder.def_id_to_func_idx.insert(*def_id, i as u32);
        }
        // Even non-FreeFunction roles get a block_func_indices entry
        // — emit_op uses these for CallBlock dispatch. Non-UI callers don't
        // emit CallBlock today, but populating them keeps the
        // bookkeeping consistent. Phase 0.3q: keyed by BlockId only
        // (module-wide unique).
        let block_id = input
            .resource
            .blocks
            .first()
            .map(|b| b.id)
            .unwrap_or(BlockId(0));
        builder.block_func_indices.insert(block_id, i as u32);
    }

    // Pass 2: register one wasm function type per function. Type
    // idx == function idx (no imports up here). Reuses the
    // L3-v2 Phase 2 helper.
    let mut cursor: u32 = 0;
    for (i, input) in inputs.iter().enumerate() {
        let block = input.resource.blocks.first().ok_or_else(|| {
            CodegenError::InternalError(format!(
                "function input #{i}: carrier resource has no blocks"
            ))
        })?;
        let layout = &builder.gc_layouts[i];
        let _type_idx = builder.register_wasm_function_type(
            &mut types,
            &mut cursor,
            &input.lir_fn.conv,
            &block.params,
            &input.resource.slots,
            layout,
        )?;
        funcs.function(i as u32);

        // Exports: per L3-v2 `export_shape`. World-level free
        // functions get one entry; resource methods need a paired
        // resource declaration (Phase 4); internal blocks are
        // skipped entirely.
        match input.lir_fn.export_shape() {
            ExportShape::FreeFunction => {
                if let FunctionRole::FreeFunction { def_id, .. } = &input.lir_fn.role {
                    // Caller-supplied rename wins (used by component
                    // callers that need `pkg:name/iface@ver#fn` form).
                    // Otherwise fall back to the function's interned
                    // name.
                    let name = if let Some(custom) = export_names.get(def_id) {
                        custom.clone()
                    } else {
                        let name_id = ctx
                            .defs
                            .as_function(*def_id)
                            .ok_or_else(|| {
                                CodegenError::InternalError(format!(
                                    "function input #{i}: DefId not registered as Function"
                                ))
                            })?
                            .name;
                        ctx.str(name_id).to_string()
                    };
                    exports.export(&name, ExportKind::Func, i as u32);
                }
            }
            ExportShape::ResourceMethod { .. } | ExportShape::Internal => {}
        }
    }

    // Pass 3: emit each function's body via the shared emitter.
    for (i, input) in inputs.iter().enumerate() {
        let func = build_function(&mut builder, i, input)?;
        codes.function(&func);
    }

    module.section(&types);
    module.section(&funcs);
    module.section(&exports);
    module.section(&codes);
    let _ = def_to_idx; // populated into builder above; kept local for clarity
    Ok(module.finish())
}

/// Build the wasm `Function` body for one top-level function: declare a
/// local per Temp slot beyond the wasm-level params, walk the body
/// ops through `WasmPackageBuilder::emit_op`, then push the return
/// slot onto the stack at function exit (LIR's `Return` op carries
/// no value — the return slot is implicit; the emit_op `Return` arm
/// emits a structural `return` which suffices for typed returns
/// IFF the stack already has the value; we satisfy that here).
fn build_function(
    builder: &mut WasmPackageBuilder<'_>,
    comp_idx: usize,
    input: &FunctionInput<'_>,
) -> Result<Function, CodegenError> {
    let block = &input.resource.blocks[0];
    let slots = &input.resource.slots;

    // Local declarations: every Temp slot becomes a wasm local. The
    // wasm-level params already occupy locals 0..params.len(); the
    // remaining Temp slots are declared explicitly via the shared
    // L3-v2 Phase 2 helper. UI's `block_fn::generate_block_function`
    // routes through the same helper so both paths agree on slot
    // ordering and GC val-type resolution.
    let param_count = block.params.len();
    let layout = builder.gc_layouts[comp_idx].clone();
    let locals = builder.declare_function_locals(slots, param_count, &layout)?;
    let mut func = Function::new(locals);

    // The shared `emit_expr` looks up `Local(LocalId)` references
    // through `current_block_local_to_slot` (slot lookup) +
    // `current_block_local_modes` (Ptr-load vs scalar `local.get`).
    // UI's convention is "every Local maps to a slot that holds a
    // pointer; deref on read" (Ptr mode). The non-UI convention is
    // "every LocalId(i) IS the wasm local at index i, scalar"
    // (Value mode).
    //
    // Set up the per-function maps before any emit_op runs:
    //   * local_to_slot: LocalId(i) → SlotId(i)  (1:1 by convention)
    //   * local_modes:   LocalId(i) → Value      (no pointer deref)
    //
    // This wires LocalId-keyed scalar reads through the shared emitter without
    // emitter changes.
    let local_to_slot: HashMap<yel_core::ids::LocalId, yel_core::lir::block::LirSlotId> = slots
        .iter()
        .enumerate()
        .map(|(i, _)| {
            (
                yel_core::ids::LocalId(i as u32),
                yel_core::lir::block::LirSlotId::resource(i as u32),
            )
        })
        .collect();
    let local_modes: HashMap<yel_core::ids::LocalId, yel_core::lir::block::LirBindingMode> =
        local_to_slot
            .keys()
            .map(|k| (*k, yel_core::lir::block::LirBindingMode::Value))
            .collect();
    builder.current_block_local_to_slot = Some(local_to_slot);
    builder.current_block_local_modes = Some(local_modes);
    builder.current_block_local_offset = Some(0);

    // Body emit: pure delegation to the shared emitter. Valued
    // early returns are handled via `LirOp::ReturnValue` (added in
    // yel-core's LIR for exactly this use case) — the shared
    // emitter's `ReturnValue` arm does `local.get <value>; return`,
    // satisfying the function's typed wasm return on the stack at
    // the early-exit point. No per-caller walker needed.
    for op in &block.ops {
        builder.emit_op(&mut func, op, comp_idx, block, 0)?;
    }

    // Tear down per-function state so a later function gets clean
    // slate (the maps are HashMap<LocalId, _>; LocalId(0) collides
    // across functions, so reuse would silently mis-bind).
    builder.current_block_local_to_slot = None;
    builder.current_block_local_modes = None;
    builder.current_block_local_offset = None;

    // Trailing terminator. Every valued return goes through
    // `LirOp::ReturnValue` (which already pushes the value and
    // emits a wasm `return`), so control never reaches the
    // function tail on a well-formed graph. But wasm validation
    // checks the stack at function `end` against the declared
    // return type — if the body terminates earlier via structured
    // control flow, the validator still wants something on the
    // stack at the structural end of the function.
    //
    // For typed-return functions, emit `unreachable` before `end`.
    // Its stack effect is polymorphic (unifies with any expected
    // type), so validation passes; at runtime, reaching it
    // signals a malformed graph (missing `Return` on a path) with
    // a clean trap rather than a silent default-zero return.
    if block.return_slot.is_some() {
        func.instruction(&wasm_encoder::Instruction::Unreachable);
    }
    func.instruction(&wasm_encoder::Instruction::End);
    Ok(func)
}

// ─────────────────────────────────────────────────────────────────────
// WIT generation + component wrapping
// ─────────────────────────────────────────────────────────────────────

/// Map a scalar `LirSlotValType` back to a yel `Ty`. Lossy: every
/// I32-family / I64 / F32 / F64 val_ty currently widens to
/// `Ty::S32` (the only pre-interned scalar constant; S64 / F32 / F64
/// require `&mut` for interning, which `generate_component` doesn't
/// have).
///
/// The WIT output therefore lists every primitive param as `s32`
/// until L4 plumbs the original `Ty` (from `FlowFunc::slot_tys` /
/// equivalent) onto `FunctionInput`. Compound types are unaffected
/// — they route through `WitAstBuilder::ty_to_wit_type` directly
/// once the real `Ty` is threaded in.
fn scalar_yel_ty_from_val_ty(_val_ty: LirSlotValType) -> yel_core::Ty {
    yel_core::Ty::S32
}

/// Package coordinates for the synthesised WIT document. Caller picks
/// a namespace + name; we default the version to `0.1.0` for now.
///
/// The world we produce is flat: every exported flow function lives
/// directly on `world`, no interfaces (Phase 4 may promote to
/// per-source-unit interfaces).
#[derive(Debug, Clone)]
pub struct ComponentPackage {
    pub namespace: String,
    pub name: String,
    pub version: String,
    /// World name. Conventionally `flow` or the source-unit name.
    pub world: String,
}

impl Default for ComponentPackage {
    fn default() -> Self {
        Self {
            namespace: "floc".into(),
            name: "program".into(),
            version: "0.1.0".into(),
            world: "flow".into(),
        }
    }
}

/// One-shot: build the core wasm module from `inputs`, the matching
/// WIT world (via [`crate::wit_ast::WitAstBuilder::build_function_world`]),
/// embed metadata, and wrap as a wasm component.
///
/// Output: component-model wasm bytes — the format jco transpiles
/// and wasmtime / wit-component-aware hosts execute. This is the
/// canonical shipping format for non-UI yel programs.
///
/// WIT generation goes directly through `WitAstBuilder`'s
/// `wit-parser` AST — no textual-WIT roundtrip — so compound types
/// (records, variants, lists, options, results, tuples) thread
/// through `ty_to_wit_type` and get canonical type ids the moment
/// they're registered in `ctx.defs`.
/// Component variant that accepts a pre-built `(Resolve, WorldId)` and
/// an explicit core-export-name map. Use this when the WIT structure
/// is authored *outside* the LIR — for example, by `yel_flow_core`'s
/// `module_to_resolve` walking a `WireModule` tree of interfaces and
/// resources. Wires that have functions inside interfaces need each
/// such function exported under the canonical `pkg:name/iface@ver#fn`
/// form so `wit-component`'s encoder can bind core exports to the
/// corresponding WIT items.
///
/// Functions whose `DefId` isn't in `export_names` keep their default
/// (`ctx.str(def.name)`) name, which is the right behaviour for
/// world-level free functions.
pub fn generate_component_with_wit(
    ctx: &CompilerContext,
    inputs: &[FunctionInput<'_>],
    resolve: Resolve,
    world_id: WorldId,
    export_names: &HashMap<DefId, String>,
) -> Result<Vec<u8>, CodegenError> {
    use wit_component::{ComponentEncoder, StringEncoding};

    // 1. Core module with caller-controlled export names.
    let mut core_bytes = generate_function_module_with_names(ctx, inputs, export_names)?;

    // 2. Embed metadata so the encoder knows which world this module
    //    satisfies. The resolve was authored by the caller — typically
    //    via `yel_flow_core::wit_emit::module_to_resolve`.
    wit_component::embed_component_metadata(
        &mut core_bytes,
        &resolve,
        world_id,
        StringEncoding::UTF8,
    )
    .map_err(|e| CodegenError::EncodingError(format!("embed_component_metadata: {e}")))?;

    // 3. Wrap as a component. `validate(true)` runs the component-model
    //    validator over the result; any export-name mismatch between the
    //    core module and the WIT surface trips a `validate` error here.
    let bytes = ComponentEncoder::default()
        .module(&core_bytes)
        .map_err(|e| CodegenError::EncodingError(format!("encoder.module: {e}")))?
        .validate(true)
        .encode()
        .map_err(|e| {
            // Walk the anyhow chain so the inner cause (usually
            // something like "function X is not implemented") makes it
            // into the diagnostic the caller surfaces in the UI.
            let mut msg = format!("encoder.encode: {e}");
            let mut src = std::error::Error::source(&*e);
            while let Some(cause) = src {
                msg.push_str(&format!("\n  caused by: {cause}"));
                src = cause.source();
            }
            CodegenError::EncodingError(msg)
        })?;

    Ok(bytes)
}

pub fn generate_component(
    ctx: &CompilerContext,
    inputs: &[FunctionInput<'_>],
    pkg: &ComponentPackage,
) -> Result<Vec<u8>, CodegenError> {
    use wit_component::{ComponentEncoder, StringEncoding};

    // 1. Core module.
    let mut core_bytes = generate_function_module(ctx, inputs)?;

    // 2. Collect the WIT-export rows up-front, owning their strings
    //    so the `FreeFunctionExport` borrows have stable backing.
    //    Skip non-FreeFunction shapes (ResourceMethod / Internal).
    struct ExportRow {
        name: String,
        param_names: Vec<String>,
        param_tys: Vec<yel_core::Ty>,
        result_ty: Option<yel_core::Ty>,
    }
    let mut rows: Vec<ExportRow> = Vec::new();
    for (i, input) in inputs.iter().enumerate() {
        if !matches!(input.lir_fn.export_shape(), ExportShape::FreeFunction) {
            continue;
        }
        let def_id = match &input.lir_fn.role {
            FunctionRole::FreeFunction { def_id, .. } => *def_id,
            _ => continue,
        };
        let func_def = ctx.defs.as_function(def_id).ok_or_else(|| {
            CodegenError::InternalError(format!(
                "wit ast: function input #{i} has unregistered DefId"
            ))
        })?;
        let block = input.resource.blocks.first().ok_or_else(|| {
            CodegenError::InternalError(format!("wit ast: function input #{i} has no body"))
        })?;
        let param_names: Vec<String> = block
            .params
            .iter()
            .enumerate()
            .map(|(j, ps)| {
                input
                    .resource
                    .slots
                    .get(ps.legacy_u32() as usize)
                    .and_then(|s| s.name.clone())
                    .unwrap_or_else(|| format!("arg{j}"))
            })
            .collect();
        // Yel-level param / return types — derived from each slot's
        // `val_ty`. L4 will thread the real `Ty` through
        // `FunctionInput` so this lookup becomes exact for compounds;
        // for now we hit the scalar fast-path and fall back to S32 for
        // anything GC-typed (which would have errored at body emit
        // already).
        let param_tys: Vec<yel_core::Ty> = block
            .params
            .iter()
            .map(|ps| {
                input
                    .resource
                    .slots
                    .get(ps.legacy_u32() as usize)
                    .map(|s| scalar_yel_ty_from_val_ty(s.val_ty))
                    .unwrap_or(yel_core::Ty::S32)
            })
            .collect();
        let result_ty = input
            .lir_fn
            .conv
            .returns
            .first()
            .copied()
            .map(scalar_yel_ty_from_val_ty);
        rows.push(ExportRow {
            name: ctx.str(func_def.name).to_string(),
            param_names,
            param_tys,
            result_ty,
        });
    }

    // 3. WIT world — built directly as a `wit-parser` AST.
    //    Reuses `WitAstBuilder`'s `ty_to_wit_type` so compound types
    //    (records / variants / lists / option / result) thread through
    //    the same path UI components use.
    let mut wit_builder =
        crate::wit_ast::WitAstBuilder::new(ctx, &pkg.namespace, &pkg.name, &pkg.version);
    // Materialise the borrowed-slice views over the row storage.
    let param_name_refs: Vec<Vec<&str>> = rows
        .iter()
        .map(|r| r.param_names.iter().map(String::as_str).collect())
        .collect();
    let exports: Vec<crate::wit_ast::FreeFunctionExport<'_>> = rows
        .iter()
        .enumerate()
        .map(|(i, r)| crate::wit_ast::FreeFunctionExport {
            name: r.name.as_str(),
            param_names: &param_name_refs[i],
            param_tys: &r.param_tys,
            result_ty: r.result_ty,
        })
        .collect();
    let world_id = wit_builder
        .build_function_world(&pkg.world, &exports)
        .map_err(|e| CodegenError::EncodingError(format!("WIT AST build: {e}")))?;
    let (resolve, _) = wit_builder.into_resolve_and_world();
    // `into_resolve_and_world` returns the first world it finds; for a
    // freshly-built builder that's our world by construction. Re-bind
    // the id to be explicit.
    let _ = world_id;
    let world_id = resolve
        .worlds
        .iter()
        .next()
        .map(|(id, _)| id)
        .ok_or_else(|| CodegenError::InternalError("WIT AST: no world in resolve".into()))?;

    // 4. Embed component metadata into the core bytes so the encoder
    //    knows which world the module satisfies.
    wit_component::embed_component_metadata(
        &mut core_bytes,
        &resolve,
        world_id,
        StringEncoding::UTF8,
    )
    .map_err(|e| CodegenError::EncodingError(format!("embed_component_metadata: {e}")))?;

    // 5. Encode as a component. `validate(true)` runs the
    //    component-model validator over the result.
    let bytes = ComponentEncoder::default()
        .module(&core_bytes)
        .map_err(|e| CodegenError::EncodingError(format!("encoder.module: {e}")))?
        .validate(true)
        .encode()
        .map_err(|e| CodegenError::EncodingError(format!("encoder.encode: {e}")))?;

    Ok(bytes)
}
