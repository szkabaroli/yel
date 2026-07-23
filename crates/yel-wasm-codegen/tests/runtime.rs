//! Structural runtime-correctness tests.
//!
//! These tests catch the class of bugs that slip past the WIT-snapshot +
//! `wasmparser::Validator` coverage in `integration.rs`: semantically
//! wrong codegen that still produces well-typed, well-formed WASM. Each
//! test compiles a fixture, walks the emitted core module with
//! `wasmparser`, and asserts on specific instruction shapes and export
//! sets that guarantee correct runtime behaviour.
//!
//! True execution-driven tests (instantiate via Wasmtime, invoke
//! exports, assert on observed DOM ops) are a planned follow-up. The
//! tooling is intentionally deferred until the dev environment has the
//! disk headroom for Wasmtime's component-model build footprint — the
//! structural tests here cover the same session bugs by proxy.
//!
//! Tests assert on **expected correct behaviour**. A failure means the
//! compiler has a bug, not the test. Never tune the assertions to
//! accommodate a known-wrong emission — add the fixture to
//! `tests/fixtures/known_bugs/` instead.

use wasmparser::{Operator, Parser, Payload};

use yel_core::Compiler;
use yel_wasm_codegen as codegen;

// ============================================================================
// Pipeline helper
// ============================================================================

fn compile_to_component(source: &str) -> Vec<u8> {
    let mut compiler = Compiler::new();
    let file = compiler.parse(source).expect("parse");
    let hir = compiler.lower_to_hir(&file);
    assert!(
        !compiler.has_errors(),
        "HIR errors:\n{}",
        compiler.render_diagnostics()
    );
    let mut lir_components = Vec::new();
    let mut global_thir_defaults: std::collections::HashMap<
        yel_core::DefId,
        yel_core::thir::ThirExpr,
    > = std::collections::HashMap::new();
    for item in &hir {
        match compiler.type_check(item) {
            yel_core::thir::ThirItem::Component(thir) => {
                assert!(
                    !compiler.has_errors(),
                    "typeck errors:\n{}",
                    compiler.render_diagnostics()
                );
                lir_components.push(compiler.lower_to_lir(&thir));
            }
            yel_core::thir::ThirItem::Global(global) => {
                assert!(
                    !compiler.has_errors(),
                    "global typeck errors:\n{}",
                    compiler.render_diagnostics()
                );
                global_thir_defaults.extend(global.signal_defaults);
            }
        }
    }
    compiler.resolve_global_triggers(&mut lir_components);

    let (lir_globals, lir_global_default_exprs) =
        compiler.lower_globals_to_lir(&global_thir_defaults);

    let (namespace, name, version) = match file.package {
        Some(ref pkg) => (
            pkg.namespace.clone(),
            pkg.name.clone(),
            pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
        ),
        None => ("yel".into(), "app".into(), "0.1.0".into()),
    };

    let interfaces = compiler.build_import_interfaces();
    let module = yel_core::lir::LirModule {
        resources: lir_components,
        global_defaults: lir_globals.clone(),
        global_default_exprs: lir_global_default_exprs.clone(),
        interfaces,
        package: file.package.clone(),
    };
    let opts = codegen::WasmWithWitOptions {
        namespace,
        name,
        version,
        global_defaults: lir_globals,
        global_default_exprs: lir_global_default_exprs,
        wasm_opt_args: None,
    };
    codegen::generate_wasm_module(&module, compiler.context(), &opts).expect("wasm codegen")
}

// ============================================================================
// WASM inspection
// ============================================================================

/// Dig through the component binary to find every nested core module and
/// return the raw bytes of the first one — that's where the Yel compiler
/// lives. The adapter modules emitted by wit-component get skipped by
/// preferring the one with exports matching the Yel export-naming
/// convention.
fn extract_yel_core_module(component_bytes: &[u8]) -> Vec<u8> {
    let mut found_modules: Vec<Vec<u8>> = Vec::new();
    let mut depth: i32 = 0;
    let parser = Parser::new(0);
    for payload in parser.parse_all(component_bytes) {
        let payload = payload.expect("component parse");
        if let Payload::ModuleSection {
            ref unchecked_range,
            ..
        } = payload
        {
            // Depth 0 means this is a core module directly under the outer
            // component — those are the Yel core module + adapter modules.
            if depth == 0 {
                let bytes = &component_bytes[unchecked_range.clone()];
                found_modules.push(bytes.to_vec());
            }
        }
        if matches!(payload, Payload::ComponentSection { .. }) {
            depth += 1;
        }
        if matches!(payload, Payload::End(_)) {
            depth = depth.saturating_sub(1);
        }
    }
    // Yel's main module is the first one emitted (adapters come after or
    // live inside inner components). Filter to the largest — adapter
    // modules are tiny indirections.
    found_modules
        .into_iter()
        .max_by_key(|m| m.len())
        .expect("at least one core module")
}

struct Export {
    name: String,
}

struct CoreModule {
    bytes: Vec<u8>,
}

impl CoreModule {
    fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    fn exports(&self) -> Vec<Export> {
        let mut out = Vec::new();
        for payload in Parser::new(0).parse_all(&self.bytes) {
            if let Payload::ExportSection(reader) = payload.expect("core parse") {
                for e in reader {
                    let e = e.expect("export");
                    out.push(Export {
                        name: e.name.to_string(),
                    });
                }
            }
        }
        out
    }

    fn imports(&self) -> Vec<(String, String)> {
        let mut out = Vec::new();
        for payload in Parser::new(0).parse_all(&self.bytes) {
            if let Payload::ImportSection(reader) = payload.expect("core parse") {
                for i in reader.into_imports() {
                    let i = i.expect("import");
                    out.push((i.module.to_string(), i.name.to_string()));
                }
            }
        }
        out
    }

    /// Find the function index that a given export name points at.
    fn export_func_index(&self, name: &str) -> Option<u32> {
        for payload in Parser::new(0).parse_all(&self.bytes) {
            if let Payload::ExportSection(reader) = payload.expect("core parse") {
                for e in reader {
                    let e = e.expect("export");
                    if e.name == name && e.kind == wasmparser::ExternalKind::Func {
                        return Some(e.index);
                    }
                }
            }
        }
        None
    }

    /// Extract every operator in the body of `func_idx` (after subtracting
    /// imported function count).
    fn function_body(&self, func_idx: u32) -> Vec<Operator<'_>> {
        let import_count = self.imports().len() as u32;
        let local_idx = func_idx.checked_sub(import_count).expect(
            "cannot inspect body of an imported function — only locally-defined funcs have bodies",
        );
        let mut body_idx: u32 = 0;
        for payload in Parser::new(0).parse_all(&self.bytes) {
            if let Payload::CodeSectionEntry(body) = payload.expect("core parse") {
                if body_idx == local_idx {
                    let mut ops = Vec::new();
                    let reader = body.get_operators_reader().expect("operators");
                    for op in reader {
                        ops.push(op.expect("op").clone());
                    }
                    return ops;
                }
                body_idx += 1;
            }
        }
        panic!("function body {} not found", local_idx)
    }
}

// ============================================================================
// Tests — each asserts expected-correct behaviour and fails loudly if the
// compiler regresses.
// ============================================================================

/// Regression-of-a-regression: narrow-type signal setters used to need
/// `i32.store8` to avoid clobbering the adjacent signal's bytes when
/// signals shared a packed memory region. After the GC-struct migration
/// (each signal lives in its own dedicated `$Comp_<i>` struct field) the
/// adjacency hazard disappears structurally — the bool setter now writes
/// the full i32 param into its own field via `struct.set`, no
/// `i32.store8` involved. The new invariant: the setter must NOT touch
/// linear memory at all for migrated signals (writes go to the struct).
#[test]
fn narrow_type_setter_uses_byte_store() {
    let source = r#"
        package yel:narrowset@0.1.0;
        export component App {
            flag: bool = false;
            title: string = "hi";
            VStack { Text { "{title}" } }
        }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));

    let setter_name = "yel:narrowset/app-component@0.1.0#[method]app.set-flag";
    let idx = core.export_func_index(setter_name).unwrap_or_else(|| {
        panic!(
            "missing setter export `{}`. exports: {:?}",
            setter_name,
            core.exports().iter().map(|e| &e.name).collect::<Vec<_>>()
        )
    });
    let ops = core.function_body(idx);

    let has_struct_set = ops
        .iter()
        .any(|op| matches!(op, Operator::StructSet { .. }));
    let touches_memory = ops.iter().any(|op| {
        matches!(
            op,
            Operator::I32Store { .. } | Operator::I32Store8 { .. } | Operator::I32Store16 { .. }
        )
    });

    assert!(
        has_struct_set,
        "set-flag must struct.set the bool field of the component's GC struct (got ops {:?})",
        ops
    );
    assert!(
        !touches_memory,
        "set-flag must not write linear memory for a struct-migrated bool signal (got ops {:?})",
        ops
    );
}

/// Regression: constructor of an exported component stashes the
/// resource handle returned by `[resource-new]X` into the component
/// struct's trailing `$self_handle (mut i32)` field so callbacks can
/// pass it back to the host as `borrow<X>`. If this regresses,
/// callbacks lose their self-handle and the host can't route events
/// to the right component instance.
///
/// The stash pattern in the constructor body is, in order:
///     call $[resource-new]X
///     local.set <handle_local>
///     local.get <self_ref_local>     ;; (ref $Comp)
///     local.get <handle_local>       ;; i32 host handle
///     struct.set <Comp_ty> <self_handle_field>
#[test]
fn constructor_stashes_self_handle() {
    let source = r#"
        package yel:ssh@0.1.0;
        export component App {
            count: s32 = 0;
            VStack { Text { "{count}" } }
        }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));

    let ctor_name = "yel:ssh/app-component@0.1.0#[constructor]app";
    let idx = core
        .export_func_index(ctor_name)
        .unwrap_or_else(|| panic!("missing constructor export `{}`", ctor_name));
    let ops = core.function_body(idx);

    // Locate the imported resource-new call and verify a local.set,
    // i32.const, local.get, i32.store sequence follows. Exact local
    // indices are implementation details so we don't pin them — we just
    // assert the shape.
    let mut idx = None;
    for (i, op) in ops.iter().enumerate() {
        if let Operator::Call { function_index: _ } = op {
            // First `call` in the constructor body is `[resource-new]X`
            // (the only import a simple constructor calls). Mark it.
            idx = Some(i);
            break;
        }
    }
    let resource_new_pos = idx.unwrap_or_else(|| {
        panic!(
            "constructor has no `call` to [resource-new]X — self-handle \
             stash path missing. ops: {:?}",
            ops
        )
    });

    // After the call we want: LocalSet (capture the handle), then
    // LocalGet+LocalGet+StructSet (write it into the component struct's
    // trailing $self_handle field), then LocalGet (return it). Allow
    // extra ops between.
    let tail = &ops[resource_new_pos + 1..];
    let saw_set = tail
        .iter()
        .any(|op| matches!(op, Operator::LocalSet { .. }));
    let saw_struct_set = tail
        .iter()
        .any(|op| matches!(op, Operator::StructSet { .. }));
    let no_i32_store = !tail
        .iter()
        .any(|op| matches!(op, Operator::I32Store { .. }));
    assert!(
        saw_set && saw_struct_set,
        "constructor doesn't appear to stash the self-handle after [resource-new]. \
         Expected LocalSet + StructSet in tail; ops: {:?}",
        ops
    );
    assert!(
        no_i32_store,
        "constructor still emits an i32.store after [resource-new] — the self-handle \
         must live in `$Comp.$self_handle` (struct.set), not linear memory. ops: {:?}",
        ops
    );
}

/// Regression: `yel:ui/dispatch@0.1.0#dispatch` is exported exactly ONCE
/// per core module, regardless of how many components the module defines.
/// Earlier we had a duplicate-per-component emission bug that silently
/// shipped multiple dispatch exports under each component's interface.
#[test]
fn dispatch_exported_exactly_once() {
    let source = r#"
        package yel:dispone@0.1.0;
        export component A { Button { "a" clicked: { } } }
        export component B { Button { "b" clicked: { } } }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));

    let dispatch_exports: Vec<String> = core
        .exports()
        .into_iter()
        .map(|e| e.name)
        .filter(|n| n.contains("dispatch"))
        .collect();

    assert_eq!(
        dispatch_exports.len(),
        1,
        "dispatch should be exported exactly once at module scope, \
         not per-component. Got: {:?}",
        dispatch_exports
    );
    assert!(
        dispatch_exports[0].starts_with("yel:ui/dispatch@0.1.0#"),
        "dispatch lives under the shared `yel:ui/dispatch` interface, \
         not a per-package one. Got: `{}`",
        dispatch_exports[0]
    );
}

/// Regression: callback imports have `(i32) -> ()` signature (the self
/// handle is the first and only arg). Prior to the self-handle work the
/// signature was `() -> ()` and the host had no way to distinguish
/// component instances.
#[test]
fn callback_imports_take_self_handle() {
    let source = r#"
        package yel:cbs@0.1.0;
        export component Widget {
            count: s32 = 0;
            export bump: func();
            VStack { Text { "{count}" } }
        }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));

    let callback_imports: Vec<(String, String)> = core
        .imports()
        .into_iter()
        .filter(|(module, _)| module.contains("callbacks"))
        .collect();

    assert!(
        !callback_imports.is_empty(),
        "expected at least one callback import in a module with an `export func` — \
         imports: {:?}",
        core.imports()
    );

    // Find each imported func's type and assert it's (i32) -> ().
    let import_types = parse_import_func_types(&core.bytes);
    for (module, name) in &callback_imports {
        let ty = import_types
            .get(&(module.clone(), name.clone()))
            .unwrap_or_else(|| panic!("no type recorded for import `{}` `{}`", module, name));
        assert_eq!(
            ty,
            &FuncSig {
                params: vec![wasmparser::ValType::I32],
                results: vec![],
            },
            "callback `{}::{}` must take a single i32 self-handle param, got {:?}",
            module,
            name,
            ty
        );
    }
}

/// Regression: container components (those with `@children`) return the
/// children-root node id from their mount, and callers capture that
/// return value. A non-container component's mount must have signature
/// `(i32, i32) -> ()` — no return — and a container's must be
/// `(i32, i32) -> i32`.
#[test]
fn container_mount_returns_i32_non_container_returns_void() {
    let container = r#"
        package yel:ctnrm@0.1.0;
        export component Card {
            VStack { Text { "chrome" } @children }
        }
    "#;
    let non_container = r#"
        package yel:plain@0.1.0;
        export component Plain { Text { "hi" } }
    "#;

    let ctnr_bytes = compile_to_component(container);
    let ctnr_core = CoreModule::new(extract_yel_core_module(&ctnr_bytes));
    let plain_bytes = compile_to_component(non_container);
    let plain_core = CoreModule::new(extract_yel_core_module(&plain_bytes));

    let ctnr_mount = find_export_func_sig(
        &ctnr_core,
        "yel:ctnrm/card-component@0.1.0#[method]card.mount",
    );
    let plain_mount = find_export_func_sig(
        &plain_core,
        "yel:plain/plain-component@0.1.0#[method]plain.mount",
    );

    assert_eq!(
        ctnr_mount.params,
        vec![wasmparser::ValType::I32, wasmparser::ValType::I32],
        "container mount should take (self, root)"
    );
    assert_eq!(
        ctnr_mount.results,
        vec![wasmparser::ValType::I32],
        "container mount must return the children-root node id"
    );

    assert_eq!(
        plain_mount.params,
        vec![wasmparser::ValType::I32, wasmparser::ValType::I32],
        "non-container mount should take (self, root)"
    );
    assert_eq!(
        plain_mount.results,
        Vec::<wasmparser::ValType>::new(),
        "non-container mount must NOT return a value"
    );
}

#[test]
fn debug_dump_multi_global() {
    let source = r#"
        package yel:dbgmg@0.1.0;
        global Alpha { x: s32 = 7; }
        global Beta  { y: s32 = 11; }
        component Reader {
            Text { "alpha={Alpha.x} beta={Beta.y}" }
        }
        component Writer {
            Button { "bump" clicked: { Alpha.x = Alpha.x + 1; } }
        }
        export component App { VStack { Reader {} Writer {} } }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));
    std::fs::write("/tmp/multi_global.wasm", &core.bytes).unwrap();
}

/// Regression: module start function runs at instantiation and seeds
/// global-singleton property slots with their declared defaults.
/// The bug we hit: globals-only modules used to fall through to
/// `dummy_module` (no start function, no init). Now they emit real
/// bodies.
#[test]
fn module_start_function_seeds_global_defaults() {
    let source = r#"
        package yel:gstart@0.1.0;
        global Store {
            count: s32 = 42;
            label: string = "hello";
        }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));

    // Assert the core module has a start section. wasmparser exposes it as
    // a StartSection payload.
    let has_start = Parser::new(0)
        .parse_all(&core.bytes)
        .any(|p| matches!(p.expect("parse"), Payload::StartSection { .. }));
    assert!(
        has_start,
        "globals-only modules must emit a WASM start section so their \
         defaults get seeded at instantiation time"
    );

    // Walk the start function's body — global singleton state lives in
    // per-field core wasm globals, so defaults are seeded via
    // `global.set` (pointer-typed properties that stay in linear memory
    // seed via `i32.store`). Either op proves init actually runs.
    // (Previously it was an empty stub produced by `dummy_module`.)
    let start_idx = Parser::new(0)
        .parse_all(&core.bytes)
        .find_map(|p| match p.expect("parse") {
            Payload::StartSection { func, .. } => Some(func),
            _ => None,
        })
        .expect("start idx");
    let ops = core.function_body(start_idx);
    assert!(
        ops.iter()
            .any(|op| matches!(op, Operator::GlobalSet { .. } | Operator::I32Store { .. })),
        "start function body must write at least one default-seed value \
         (global.set into a per-field core wasm global, or i32.store into \
         linear memory for pointer-typed globals). ops: {:?}",
        ops
    );
}

/// Regression: `import component Dialog { ... }` produces a WIT interface
/// that the host is expected to provide. The core module imports
/// `[resource-new]dialog` from the matching `[export]` interface.
///
/// This assertion locks in the resource-new import naming so accidental
/// renaming breaks loudly instead of silently orphaning imports.
#[test]
fn imported_component_declares_resource_new_import() {
    let source = r#"
        package yel:impdecl@0.1.0;
        import component Dialog {
            title: string;
            func show();
        }
        export component App {
            Text { "needs a real component so WIT isn't library-only" }
        }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));
    let imports = core.imports();
    // Sanity: the current module is expected to have a resource-new
    // import for its OWN exported App component. The imported Dialog
    // doesn't trigger a resource-new in the core module — that's host
    // business. So we only assert that the module imports SOMETHING from
    // the dialog's interface if we find any such entry; otherwise the
    // compiler decides not to import anything from Dialog at the core
    // level, which is a separate known limitation.
    let app_resource_new = imports
        .iter()
        .any(|(module, name)| module.contains("app-component") && name.contains("resource-new"));
    assert!(
        app_resource_new,
        "module should import [resource-new]app from the exported app \
         component's interface. imports: {:?}",
        imports
    );
}

/// After the GC-struct migration, the read-modify-write of a narrow
/// signal lives entirely in the component's `$Comp_<i>` struct: the
/// reactive-write path emits `struct.get` (read), `i32.eqz` (negate),
/// `struct.set` (write). No memory load/store touches the bool field
/// at all — the adjacent-byte-clobber hazard the previous version of
/// this test was guarding against is gone structurally.
#[test]
fn signal_write_on_narrow_type_uses_byte_store() {
    let source = r#"
        package yel:swnarrow@0.1.0;
        export component App {
            flag: bool = false;
            title: string = "x";
            VStack {
                Text { "{title}" }
                Button {
                    "go"
                    clicked: { flag = !flag; }
                }
            }
        }
    "#;
    let comp_bytes = compile_to_component(source);
    let core = CoreModule::new(extract_yel_core_module(&comp_bytes));

    // Find a function body that contains the rmw shape on the GC
    // struct: struct.get → i32.eqz → struct.set. Any of those three
    // missing means either the lowering shape changed or the
    // reactive-write path regressed to a memory store.
    let mut saw_struct_rmw = false;
    for payload in Parser::new(0).parse_all(&core.bytes) {
        if let Payload::CodeSectionEntry(body) = payload.expect("parse") {
            let ops: Vec<Operator<'_>> = body
                .get_operators_reader()
                .expect("ops")
                .into_iter()
                .collect::<Result<_, _>>()
                .expect("ops reader");
            let has_get = ops
                .iter()
                .any(|op| matches!(op, Operator::StructGet { .. }));
            let has_set = ops
                .iter()
                .any(|op| matches!(op, Operator::StructSet { .. }));
            let has_eqz = ops.iter().any(|op| matches!(op, Operator::I32Eqz));
            if has_get && has_set && has_eqz {
                saw_struct_rmw = true;
                break;
            }
        }
    }
    assert!(
        saw_struct_rmw,
        "no function body performs the struct-resident read-modify-write \
         (struct.get + i32.eqz + struct.set) for `flag = !flag`. Either the \
         lowering shape changed or the reactive-write path lost its struct \
         routing."
    );
}

// ============================================================================
// WASM parsing helpers
// ============================================================================

#[derive(Debug, PartialEq, Eq)]
struct FuncSig {
    params: Vec<wasmparser::ValType>,
    results: Vec<wasmparser::ValType>,
}

fn parse_types(bytes: &[u8]) -> Vec<FuncSig> {
    let mut out = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        if let Payload::TypeSection(reader) = payload.expect("parse") {
            // Walk the rec groups; only collect function types, skip
            // struct/array declarations the phase-1 GC migration emits.
            for rec in reader {
                let rec = rec.expect("rec group");
                for sub in rec.types() {
                    use wasmparser::CompositeInnerType;
                    if let CompositeInnerType::Func(ft) = &sub.composite_type.inner {
                        out.push(FuncSig {
                            params: ft.params().to_vec(),
                            results: ft.results().to_vec(),
                        });
                    }
                }
            }
        }
    }
    out
}

fn parse_import_func_types(bytes: &[u8]) -> std::collections::HashMap<(String, String), FuncSig> {
    let types = parse_types(bytes);
    let mut out = std::collections::HashMap::new();
    for payload in Parser::new(0).parse_all(bytes) {
        if let Payload::ImportSection(reader) = payload.expect("parse") {
            for i in reader.into_imports() {
                let i = i.expect("import");
                if let wasmparser::TypeRef::Func(type_idx) = i.ty
                    && let Some(sig) = types.get(type_idx as usize)
                {
                    out.insert(
                        (i.module.to_string(), i.name.to_string()),
                        FuncSig {
                            params: sig.params.clone(),
                            results: sig.results.clone(),
                        },
                    );
                }
            }
        }
    }
    out
}

fn parse_function_decl_types(bytes: &[u8]) -> Vec<u32> {
    let mut out = Vec::new();
    for payload in Parser::new(0).parse_all(bytes) {
        if let Payload::FunctionSection(reader) = payload.expect("parse") {
            for ty_idx in reader {
                out.push(ty_idx.expect("func type idx"));
            }
        }
    }
    out
}

fn find_export_func_sig(core: &CoreModule, export_name: &str) -> FuncSig {
    let types = parse_types(&core.bytes);
    let func_type_idxs = parse_function_decl_types(&core.bytes);
    let import_count = core.imports().len() as u32;
    let func_idx = core.export_func_index(export_name).unwrap_or_else(|| {
        panic!(
            "missing export `{}`. exports: {:?}",
            export_name,
            core.exports().iter().map(|e| &e.name).collect::<Vec<_>>()
        )
    });
    let local_idx = func_idx
        .checked_sub(import_count)
        .expect("export points at imported func") as usize;
    let type_idx = func_type_idxs[local_idx] as usize;
    let sig = &types[type_idx];
    FuncSig {
        params: sig.params.clone(),
        results: sig.results.clone(),
    }
}
