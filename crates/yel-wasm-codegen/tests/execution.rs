//! Wasmtime-backed execution tests.
//!
//! Compiles a Yel source, instantiates the resulting WASM component
//! under Wasmtime, wires `yel:ui/dom@0.1.0` to recording closures, and
//! drives the component's exports (`[constructor]X`, `[method]X.mount`,
//! `yel:ui/dispatch@0.1.0#dispatch`, setters) to assert on observed
//! behaviour.
//!
//! This complements the structural tests in `runtime.rs`: where the
//! structural tests inspect static bytecode shapes, these execute the
//! real code and watch for correct DOM-op sequences. They catch the
//! class of bugs that emit valid WASM but behave wrong at runtime —
//! e.g. a reactive effect that computes the wrong string, a dispatch
//! that routes to the wrong handler, a setter that silently no-ops.
//!
//! **Correctness rule**: every assertion describes *expected correct
//! behaviour*. Tests stay failing when the compiler has a bug (marked
//! `#[ignore]` with a reference). Never soften an assertion to match
//! known-wrong output.

use std::sync::{Arc, Mutex};

use wasmtime::component::{Component, Linker, ResourceAny, Val};
use wasmtime::{Config, Engine, Store};

use yel_core::Compiler;
use yel_wasm_codegen as codegen;

// ============================================================================
// DOM recorder
// ============================================================================

/// Observable DOM-op trace. Each `yel:ui/dom@0.1.0` call the component
/// makes appends one entry here. Tests assert on ordered subsequences so
/// incidental reordering doesn't break them.
#[derive(Debug, Clone, PartialEq, Eq)]
enum DomOp {
    CreateElement {
        tag: String,
        id: u32,
    },
    CreateText {
        content: String,
        id: u32,
    },
    CreateComment {
        content: String,
        id: u32,
    },
    AppendChild {
        parent: u32,
        child: u32,
    },
    SetTextContent {
        node: u32,
        content: String,
    },
    SetAttribute {
        node: u32,
        name: String,
        value: String,
    },
    AddEventListener {
        node: u32,
        event: String,
        handler_id: u32,
    },
    Remove {
        node: u32,
    },
    InsertAfter {
        parent: u32,
        node: u32,
        anchor: u32,
    },
}

#[derive(Default)]
struct DomState {
    next_id: u32,
    ops: Vec<DomOp>,
    /// Registry of click/input/etc. listeners so tests can fire
    /// dispatch with the right handler_id.
    listeners: Vec<(u32, String, u32)>,
    /// Callbacks (package-level `{package}-callbacks` interface) that
    /// fired during the last op. Stored as (iface, func, self_rep_hint).
    callbacks: Vec<(String, String, u32)>,
}

impl DomState {
    fn next_id(&mut self) -> u32 {
        self.next_id += 1;
        self.next_id
    }

    fn push(&mut self, op: DomOp) {
        self.ops.push(op);
    }
}

type SharedDom = Arc<Mutex<DomState>>;

// ============================================================================
// Pipeline
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
// Wasmtime wiring
// ============================================================================

fn engine() -> Engine {
    let mut cfg = Config::new();
    cfg.wasm_component_model(true);
    // WASM GC + function references are prerequisites for our phase-1
    // type declarations (rec groups, struct/array types). Even though
    // no emission path uses them yet, the type section carries them
    // and wasmtime's validator rejects them without these features.
    cfg.wasm_gc(true);
    cfg.wasm_function_references(true);
    Engine::new(&cfg).expect("engine")
}

/// Register every `yel:ui/dom@0.1.0` import with a recording closure.
/// The closures mutate the shared `DomState` via `Store`'s state slot.
fn register_dom(linker: &mut Linker<SharedDom>) {
    let mut dom = linker
        .instance("yel:ui/dom@0.1.0")
        .expect("dom instance slot");

    dom.func_wrap(
        "create-element",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (tag,): (String,)| {
            let state = store.data();
            let mut s = state.lock().unwrap();
            let id = s.next_id();
            s.push(DomOp::CreateElement { tag, id });
            Ok((id,))
        },
    )
    .unwrap();

    dom.func_wrap(
        "create-text",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (content,): (String,)| {
            let state = store.data();
            let mut s = state.lock().unwrap();
            let id = s.next_id();
            s.push(DomOp::CreateText { content, id });
            Ok((id,))
        },
    )
    .unwrap();

    dom.func_wrap(
        "create-comment",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (content,): (String,)| {
            let state = store.data();
            let mut s = state.lock().unwrap();
            let id = s.next_id();
            s.push(DomOp::CreateComment { content, id });
            Ok((id,))
        },
    )
    .unwrap();

    dom.func_wrap(
        "create-fragment",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (): ()| {
            let state = store.data();
            let mut s = state.lock().unwrap();
            let id = s.next_id();
            // Reuse the CreateElement variant with a synthetic tag so
            // existing tests can still pattern-match wrapper ids via
            // `tag == "yel-frag"`.
            s.push(DomOp::CreateElement {
                tag: "yel-frag".to_string(),
                id,
            });
            Ok((id,))
        },
    )
    .unwrap();

    dom.func_wrap(
        "append-child",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (parent, child): (u32, u32)| {
            store
                .data()
                .lock()
                .unwrap()
                .push(DomOp::AppendChild { parent, child });
            Ok(())
        },
    )
    .unwrap();

    dom.func_wrap(
        "set-text-content",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (node, content): (u32, String)| {
            store
                .data()
                .lock()
                .unwrap()
                .push(DomOp::SetTextContent { node, content });
            Ok(())
        },
    )
    .unwrap();

    // `set-attribute` takes a variant second parameter — use the dynamic
    // `func_new` so we don't have to mirror the variant's Rust shape.
    dom.func_new("set-attribute", |store, _ty, args, _results| {
        let name = args
            .get(1)
            .and_then(|v| match v {
                Val::String(s) => Some(s.clone()),
                _ => None,
            })
            .unwrap_or_default();
        let node = args
            .first()
            .and_then(|v| match v {
                Val::U32(n) => Some(*n),
                _ => None,
            })
            .unwrap_or(0);
        // 3rd arg is the attribute-value variant; dump it as debug-repr.
        let value = args.get(2).map(|v| format!("{:?}", v)).unwrap_or_default();
        store
            .data()
            .lock()
            .unwrap()
            .push(DomOp::SetAttribute { node, name, value });
        Ok(())
    })
    .unwrap();

    dom.func_wrap(
        "remove-attribute",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>, (_node, _name): (u32, String)| Ok(()),
    )
    .unwrap();

    dom.func_wrap(
        "set-style",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>,
         (_node, _prop, _val): (u32, String, String)| Ok(()),
    )
    .unwrap();

    dom.func_wrap(
        "set-class",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>, (_node, _cls): (u32, String)| Ok(()),
    )
    .unwrap();

    dom.func_wrap(
        "add-event-listener",
        |store: wasmtime::StoreContextMut<'_, SharedDom>,
         (node, event, handler_id): (u32, String, u32)| {
            let state = store.data();
            let mut s = state.lock().unwrap();
            s.listeners.push((node, event.clone(), handler_id));
            s.push(DomOp::AddEventListener {
                node,
                event,
                handler_id,
            });
            Ok(())
        },
    )
    .unwrap();

    dom.func_wrap(
        "remove-event-listener",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>,
         (_node, _event, _handler): (u32, String, u32)| Ok(()),
    )
    .unwrap();

    dom.func_wrap(
        "remove",
        |store: wasmtime::StoreContextMut<'_, SharedDom>, (node,): (u32,)| {
            store.data().lock().unwrap().push(DomOp::Remove { node });
            Ok(())
        },
    )
    .unwrap();

    dom.func_wrap(
        "insert-before",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>, (_p, _n, _r): (u32, u32, u32)| Ok(()),
    )
    .unwrap();

    dom.func_wrap(
        "insert-after",
        |store: wasmtime::StoreContextMut<'_, SharedDom>,
         (parent, node, anchor): (u32, u32, u32)| {
            store.data().lock().unwrap().push(DomOp::InsertAfter {
                parent,
                node,
                anchor,
            });
            Ok(())
        },
    )
    .unwrap();

    dom.func_wrap(
        "remove-child",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>, (_p, _c): (u32, u32)| Ok(()),
    )
    .unwrap();

    dom.func_wrap(
        "get-parent",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>, (_n,): (u32,)| Ok((0u32,)),
    )
    .unwrap();

    dom.func_wrap(
        "get-next-sibling",
        |_store: wasmtime::StoreContextMut<'_, SharedDom>, (_n,): (u32,)| Ok((0u32,)),
    )
    .unwrap();
}

/// Register a module-level callbacks interface dynamically. Each
/// callback receives `(borrow<resource>)` as its first arg — we record
/// the invocation and (if available) the resource rep. Rep extraction
/// requires the resource-new machinery; for now we just log "callback
/// fired with some resource" — enough to prove dispatch routes to the
/// host.
fn register_callbacks(linker: &mut Linker<SharedDom>, iface_name: &str, names: &[&str]) {
    let mut iface = match linker.instance(iface_name) {
        Ok(i) => i,
        Err(_) => return,
    };
    for &name in names {
        let iface_owned = iface_name.to_string();
        let name_owned = name.to_string();
        iface
            .func_new(name, move |store, _ty, _args, _results| {
                // Dynamic API gives us `ResourceAny` without a way to
                // extract the raw rep without a typed binding. For
                // behaviour tests we only need to know the callback
                // fired and with what signature — log it, leave the
                // self-rep as a placeholder 0.
                store.data().lock().unwrap().callbacks.push((
                    iface_owned.clone(),
                    name_owned.clone(),
                    0,
                ));
                Ok(())
            })
            .unwrap();
    }
}

// ============================================================================
// Harness driver
// ============================================================================

struct Harness {
    store: Store<SharedDom>,
    instance: wasmtime::component::Instance,
}

fn instantiate(bytes: &[u8], callback_interfaces: &[(&str, &[&str])]) -> (Harness, SharedDom) {
    let engine = engine();
    let component = Component::from_binary(&engine, bytes).expect("component decode");
    let dom: SharedDom = Arc::new(Mutex::new(DomState::default()));
    let mut linker = Linker::new(&engine);
    register_dom(&mut linker);
    for (iface, names) in callback_interfaces {
        register_callbacks(&mut linker, iface, names);
    }
    let mut store = Store::new(&engine, dom.clone());
    let instance = linker
        .instantiate(&mut store, &component)
        .expect("instantiate");
    (Harness { store, instance }, dom)
}

/// Look up `{interface}#{func}` on the instance.
fn get_func(h: &mut Harness, iface: &str, func: &str) -> wasmtime::component::Func {
    let iface_idx = h
        .instance
        .get_export_index(&mut h.store, None, iface)
        .unwrap_or_else(|| panic!("instance has no interface export `{}`", iface));
    let func_idx = h
        .instance
        .get_export_index(&mut h.store, Some(&iface_idx), func)
        .unwrap_or_else(|| panic!("interface `{}` has no function `{}`", iface, func));
    h.instance
        .get_func(&mut h.store, func_idx)
        .unwrap_or_else(|| panic!("`{}#{}` is not a function", iface, func))
}

/// Construct the component's resource (`[constructor]X`) and call
/// `[method]X.mount(root_node_id)`. Returns the constructed resource as
/// a `ResourceAny` for subsequent method calls.
fn ctor_and_mount(h: &mut Harness, iface: &str, resource: &str) -> ResourceAny {
    // Pre-seed a stable root id so caller-visible append-child ops show
    // a predictable `parent`.
    const ROOT: u32 = 1_000_000;
    {
        let dom: &SharedDom = h.store.data();
        let mut s = dom.lock().unwrap();
        if s.next_id < ROOT {
            s.next_id = ROOT;
        }
    }
    let ctor = get_func(h, iface, &format!("[constructor]{}", resource));
    let mut out = [Val::Bool(false)];
    ctor.call(&mut h.store, &[], &mut out).expect("ctor call");
    let resource_val = std::mem::replace(&mut out[0], Val::Bool(false));
    let self_res = match resource_val {
        Val::Resource(r) => r,
        other => panic!(
            "[constructor]{} returned non-resource Val {:?}",
            resource, other
        ),
    };

    let mount = get_func(h, iface, &format!("[method]{}.mount", resource));
    let mut mount_out: Vec<Val> = Vec::new();
    mount
        .call(
            &mut h.store,
            &[Val::Resource(self_res), Val::U32(ROOT)],
            &mut mount_out,
        )
        .expect("mount call");
    self_res
}

/// Invoke `yel:ui/dispatch@0.1.0#dispatch(handler_id, event-value)`.
/// `event` carries the DOM event payload; pass `Val::Variant("none", None)`
/// for click / hover / pressed / changed handlers that don't consume data.
fn call_dispatch_with_event(h: &mut Harness, handler_id: u32, event: Val) {
    let f = get_func(h, "yel:ui/dispatch@0.1.0", "dispatch");
    let mut out: Vec<Val> = Vec::new();
    f.call(&mut h.store, &[Val::U32(handler_id), event], &mut out)
        .expect("dispatch");
}

/// Convenience wrapper for payload-less events (click / hover / pressed /
/// changed). Passes `event-value::none`.
fn call_dispatch(h: &mut Harness, handler_id: u32) {
    call_dispatch_with_event(h, handler_id, Val::Variant("none".into(), None));
}

/// Dispatch a DOM `input` event whose underlying `<input>` had
/// `type="number"` — wraps `event-value::input-f64(value)`.
fn dispatch_input_number(h: &mut Harness, handler_id: u32, value: f64) {
    call_dispatch_with_event(
        h,
        handler_id,
        Val::Variant("input-f64".into(), Some(Box::new(Val::Float64(value)))),
    );
}

/// Call a setter method `[method]X.set-Y(value)` on a resource.
fn call_setter(
    h: &mut Harness,
    iface: &str,
    resource: &str,
    prop_kebab: &str,
    self_res: &ResourceAny,
    value: Val,
) {
    let f = get_func(
        h,
        iface,
        &format!("[method]{}.set-{}", resource, prop_kebab),
    );
    let mut out: Vec<Val> = Vec::new();
    f.call(&mut h.store, &[Val::Resource(*self_res), value], &mut out)
        .expect("setter");
}

fn find_listener_handler(dom: &SharedDom, event: &str) -> Option<u32> {
    dom.lock()
        .unwrap()
        .listeners
        .iter()
        .find(|(_, e, _)| e == event)
        .map(|(_, _, h)| *h)
}

// ============================================================================
// Tests
// ============================================================================

/// Smoke: a trivial component mounts and emits the expected DOM
/// operations. Establishes that the full Wasmtime harness (component
/// decode, import wiring, instance lookup, resource construction,
/// method invocation) works end-to-end before richer tests layer on.
#[test]
fn smoke_mount_emits_expected_dom_ops() {
    let source = r#"
        package yel:smoke@0.1.0;
        export component App {
            VStack { Text { "hello world" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:smoke/app-component@0.1.0", "app");

    let ops = dom.lock().unwrap().ops.clone();
    // Compiled DOM structure:
    //   root
    //    └ VStack (element)
    //       └ Text (element)
    //          └ "hello world" (CreateText content node)
    let vstack_id = ops
        .iter()
        .find_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "VStack" => Some(*id),
            _ => None,
        })
        .expect("no VStack created");
    let text_elem_id = ops
        .iter()
        .find_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Text" => Some(*id),
            _ => None,
        })
        .expect("no Text element created");
    let text_content_id = ops
        .iter()
        .find_map(|op| match op {
            DomOp::CreateText { content, id } if content == "hello world" => Some(*id),
            _ => None,
        })
        .expect("no \"hello world\" text node created");

    assert!(
        ops.iter()
            .any(|op| matches!(op, DomOp::AppendChild { parent, child }
                if *parent == 1_000_000 && *child == vstack_id)),
        "VStack was not appended to the root. Trace: {:?}",
        ops
    );
    assert!(
        ops.iter()
            .any(|op| matches!(op, DomOp::AppendChild { parent, child }
                if *parent == vstack_id && *child == text_elem_id)),
        "Text element was not appended under VStack. Trace: {:?}",
        ops
    );
    assert!(
        ops.iter()
            .any(|op| matches!(op, DomOp::AppendChild { parent, child }
                if *parent == text_elem_id && *child == text_content_id)),
        "\"hello world\" text node was not appended under the Text \
         element. Trace: {:?}",
        ops
    );
}

/// Reactive propagation: changing a signal must cause the downstream
/// interpolation effect to re-fire with the new value via
/// `set-text-content`. This is THE reactivity contract — the whole
/// framework assumes it works. No structural-inspection test can catch
/// a bug here; only execution can.
#[test]
fn setter_triggers_reactive_text_update() {
    let source = r#"
        package yel:react@0.1.0;
        export component App {
            count: s32 = 0;
            VStack {
                Text { "count: {count}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:react/app-component@0.1.0", "app");

    // Drain mount-time ops so we only look at setter-induced ones.
    let mount_op_count = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:react/app-component@0.1.0",
        "app",
        "count",
        &self_res,
        Val::S32(7),
    );

    let all_ops = dom.lock().unwrap().ops.clone();
    let after_setter = &all_ops[mount_op_count..];
    // The setter should fire at least one `set-text-content` with a
    // string containing the new count. If reactivity is broken, the DOM
    // stays at the initial "count: 0" and no update fires.
    let saw_updated_text = after_setter.iter().any(
        |op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("count: 7")),
    );
    assert!(
        saw_updated_text,
        "changing `count` to 7 should trigger `set-text-content` with \"count: 7\". \
         Setter produced ops: {:?}",
        after_setter
    );
}

/// Two instances of the same component must not share signal state:
/// writing to A's `count` and reading B's getter must return the
/// initial default, not A's value. Validates the per-instance
/// `$Comp_<i>` GC struct + handle registry — without it (singleton
/// global model), B's getter would read whatever ref the global last
/// pointed to and report A's value.
#[test]
fn two_instances_have_independent_signals() {
    let source = r#"
        package yel:multins@0.1.0;
        export component App {
            count: s32 = 0;
            VStack { Text { "v={count}" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:multins/app-component@0.1.0";

    // Two independent ctor calls — no mount, no host wiring beyond
    // resource creation. Each should land in a fresh registry slot.
    let ctor = get_func(&mut h, iface, "[constructor]app");
    let mut out_a = [Val::Bool(false)];
    ctor.call(&mut h.store, &[], &mut out_a).expect("ctor A");
    let a = match std::mem::replace(&mut out_a[0], Val::Bool(false)) {
        Val::Resource(r) => r,
        other => panic!("ctor A non-resource {:?}", other),
    };
    let mut out_b = [Val::Bool(false)];
    ctor.call(&mut h.store, &[], &mut out_b).expect("ctor B");
    let b = match std::mem::replace(&mut out_b[0], Val::Bool(false)) {
        Val::Resource(r) => r,
        other => panic!("ctor B non-resource {:?}", other),
    };

    call_setter(&mut h, iface, "app", "count", &a, Val::S32(42));
    call_setter(&mut h, iface, "app", "count", &b, Val::S32(7));

    let get_count = get_func(&mut h, iface, "[method]app.get-count");
    let read = |h: &mut Harness, r: &ResourceAny| -> i32 {
        let mut out = [Val::Bool(false)];
        get_count
            .call(&mut h.store, &[Val::Resource(*r)], &mut out)
            .expect("get-count");
        match &out[0] {
            Val::S32(v) => *v,
            other => panic!("get-count returned non-s32 {:?}", other),
        }
    };
    let a_val = read(&mut h, &a);
    let b_val = read(&mut h, &b);
    assert_eq!(a_val, 42, "instance A's count must be 42, got {}", a_val);
    assert_eq!(b_val, 7, "instance B's count must be 7, got {}", b_val);
}

/// Step 4/5 verification: when an exported parent component mounts a
/// non-exported child component, two parent instances must each end up
/// with their own child instance. The child's exported constructor
/// allocates a fresh registry handle per call, so two ctor-calls
/// produce two distinct registry slots; subsequent mounts look up
/// each rep through the registry, so each child observes its own
/// per-instance state.
///
/// Today the child is non-exported, so this exercises the
/// MountComponent fan-out path (ctor + mount called inline from
/// parent's mount). Pre-Step-4 the child's mount-time effect would
/// read self via the singleton global and clobber whichever instance
/// constructed last. With self-ref threaded into block calls, each
/// parent's mount runs with its own self ref and pushes that ref into
/// every internal block call.
#[test]
fn two_parents_each_mount_independent_children() {
    let source = r#"
        package yel:p2c@0.1.0;
        export component App {
            count: s32 = 0;
            VStack { Text { "v={count}" } }
        }
    "#;
    // Same shape as `two_instances_have_independent_signals` but routed
    // through ctor_and_mount so the mount path runs (verifying the
    // mount-time registry lookup added in step 4).
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:p2c/app-component@0.1.0";
    let a = ctor_and_mount(&mut h, iface, "app");
    let b = ctor_and_mount(&mut h, iface, "app");
    call_setter(&mut h, iface, "app", "count", &b, Val::S32(123));
    let get_count = get_func(&mut h, iface, "[method]app.get-count");
    let read = |h: &mut Harness, r: &ResourceAny| -> i32 {
        let mut out = [Val::Bool(false)];
        get_count
            .call(&mut h.store, &[Val::Resource(*r)], &mut out)
            .expect("get-count");
        match &out[0] {
            Val::S32(v) => *v,
            other => panic!("get-count non-s32 {:?}", other),
        }
    };
    assert_eq!(read(&mut h, &a), 0, "A's count must remain 0");
    assert_eq!(read(&mut h, &b), 123, "B's count must be 123");
}

/// Step 4 verification: a setter on instance B's signal must trigger
/// the effect block bound to B's `Text { "v={count}" }`, observing B's
/// own count value — not A's. Pre-step-4, effect blocks read self via
/// the singleton global, so a setter on B would either (a) trigger the
/// last-constructed instance's effect (whichever overwrote the global)
/// or (b) read A's signal value despite being triggered by B's setter.
/// With the (ref Comp, parent) signature, every block call sources self
/// from the caller's typed ref, so each setter's trigger fan-out hits
/// its own instance's effect with its own signal.
#[test]
fn setter_trigger_routes_to_owning_instance_effect() {
    let source = r#"
        package yel:multinseff@0.1.0;
        export component App {
            count: s32 = 0;
            VStack { Text { "v={count}" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let iface = "yel:multinseff/app-component@0.1.0";

    let a = ctor_and_mount(&mut h, iface, "app");
    let b = ctor_and_mount(&mut h, iface, "app");

    // Snapshot the DOM op count before any setter — both mounts have
    // already issued their initial-render set_text_content ops.
    let baseline_ops = dom.lock().unwrap().ops.len();

    call_setter(&mut h, iface, "app", "count", &b, Val::S32(99));

    // The setter on B's count must have triggered B's text-update
    // effect, which writes "v=99" to B's text node. Verify a fresh
    // set_text_content op landed and it carries "v=99".
    let new_ops = dom.lock().unwrap().ops.clone();
    assert!(
        new_ops.len() > baseline_ops,
        "setter should have produced new DOM ops; before={}, after={}",
        baseline_ops,
        new_ops.len()
    );
    let last_text = new_ops
        .iter()
        .rev()
        .find_map(|op| match op {
            DomOp::SetTextContent { content, .. } => Some(content.clone()),
            _ => None,
        })
        .expect("expected a set_text_content after setter");
    assert_eq!(
        last_text, "v=99",
        "B's effect must observe B's own count (99), not A's (0)"
    );

    // Now flip A's count and confirm A's text node updates without
    // disturbing B's last value. The trace's tail must reflect A's
    // value, since A's setter is the last write.
    call_setter(&mut h, iface, "app", "count", &a, Val::S32(7));
    let final_ops = dom.lock().unwrap().ops.clone();
    let final_text = final_ops
        .iter()
        .rev()
        .find_map(|op| match op {
            DomOp::SetTextContent { content, .. } => Some(content.clone()),
            _ => None,
        })
        .expect("expected a set_text_content after A's setter");
    assert_eq!(
        final_text, "v=7",
        "A's effect must observe A's own count (7) after B's was 99"
    );
}

/// Dispatching a handler id must run the exact handler block that was
/// registered for that id. Three buttons each register a different
/// handler; dispatching button B's id must change the count by -1, not
/// the other amounts.
#[test]
fn dispatch_routes_to_correct_handler() {
    let source = r#"
        package yel:disproute@0.1.0;
        export component App {
            count: s32 = 0;
            VStack {
                Text { "v={count}" }
                Button { "inc10"   clicked: { count = count + 10; } }
                Button { "dec1"    clicked: { count = count - 1; } }
                Button { "reset"   clicked: { count = 0; } }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:disproute/app-component@0.1.0", "app");

    // Collect the three click handler ids in mount order.
    let click_handlers: Vec<u32> = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .filter(|(_, e, _)| e == "clicked")
        .map(|(_, _, h)| *h)
        .collect();
    assert_eq!(
        click_handlers.len(),
        3,
        "expected 3 click handlers, got {:?}",
        click_handlers
    );

    // Fire the MIDDLE handler (dec1). After dispatch, a getter on
    // `count` should return -1, NOT +10 or 0.
    call_dispatch(&mut h, click_handlers[1]);

    // Read the signal back via its exported getter.
    let get_count = get_func(
        &mut h,
        "yel:disproute/app-component@0.1.0",
        "[method]app.get-count",
    );
    let mut out = [Val::Bool(false)];
    get_count
        .call(&mut h.store, &[Val::Resource(self_res)], &mut out)
        .expect("get-count");
    let count = match &out[0] {
        Val::S32(v) => *v,
        other => panic!("get-count returned non-s32 {:?}", other),
    };
    assert_eq!(
        count, -1,
        "dispatching the middle handler (`dec1`, sets count-=1) should \
         make count==-1. Actual: {}. Check that dispatch routes to the \
         right handler, not the first one or a random one.",
        count
    );
}

/// Step 7 verification: with two live instances of the same component,
/// clicking a button that belongs to instance B must mutate B's signal,
/// not A's. Pre-Step-7 dispatch read self via the singleton, so the
/// last-mounted instance always won the click. The new encoding packs
/// the host handle into the upper 16 bits of the handler-id at
/// `AddEventListener` time, so dispatch can look up the right typed
/// self via the registry.
#[test]
fn two_instances_dispatch_to_correct_handler() {
    let source = r#"
        package yel:multinsdisp@0.1.0;
        export component App {
            count: s32 = 0;
            VStack {
                Text { "v={count}" }
                Button { "inc" clicked: { count = count + 1; } }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let iface = "yel:multinsdisp/app-component@0.1.0";

    // Mount two independent instances. After both mounts, the DOM has
    // two `clicked` listener registrations — one per instance. Their
    // handler-ids encode the per-instance handle in their upper 16 bits.
    let a = ctor_and_mount(&mut h, iface, "app");
    let listeners_after_a: Vec<u32> = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .filter(|(_, e, _)| e == "clicked")
        .map(|(_, _, h)| *h)
        .collect();
    assert_eq!(
        listeners_after_a.len(),
        1,
        "after mounting A there should be 1 clicked listener; saw {:?}",
        listeners_after_a
    );
    let click_a = listeners_after_a[0];
    let b = ctor_and_mount(&mut h, iface, "app");
    let all_listeners: Vec<u32> = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .filter(|(_, e, _)| e == "clicked")
        .map(|(_, _, h)| *h)
        .collect();
    let click_b = *all_listeners
        .iter()
        .find(|id| **id != click_a)
        .expect("instance B's clicked listener should have a distinct id");
    assert_ne!(
        click_a, click_b,
        "two instances must produce two distinct handler-ids; the \
         encoding `(handle << 16) | local_id` makes them differ in \
         the upper 16 bits"
    );

    // Click B's button three times. A's count must remain 0; B's must be 3.
    for _ in 0..3 {
        call_dispatch(&mut h, click_b);
    }

    let get_count = get_func(&mut h, iface, "[method]app.get-count");
    let read = |h: &mut Harness, r: &ResourceAny| -> i32 {
        let mut out = [Val::Bool(false)];
        get_count
            .call(&mut h.store, &[Val::Resource(*r)], &mut out)
            .expect("get-count");
        match &out[0] {
            Val::S32(v) => *v,
            other => panic!("get-count returned non-s32 {:?}", other),
        }
    };
    assert_eq!(
        read(&mut h, &a),
        0,
        "instance A must remain at 0 — clicks were routed to B's handler"
    );
    assert_eq!(read(&mut h, &b), 3, "instance B must reflect 3 increments");
}

/// Step 7 verification: when a global property mutates, every live
/// instance of every observing component must see the effect — not
/// just the last-mounted one. The fanout helper walks each observing
/// component's registry array, calling each live instance's effect
/// block. Pre-Step-7 the foreign-component branch fell back to the
/// singleton ref global, so only the most recently mounted instance
/// of each observing component re-rendered.
#[test]
fn global_signal_fans_out_to_all_instances() {
    let source = r#"
        package yel:globalfan@0.1.0;

        global Store { count: s32 = 0; }

        component Writer {
            Button {
                "inc"
                clicked: { Store.count = Store.count + 1; }
            }
        }

        component Reader {
            Text { "read: {Store.count}" }
        }

        export component App {
            VStack { Reader {} Reader {} Writer {} }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:globalfan/app-component@0.1.0", "app");

    let click = find_listener_handler(&dom, "clicked").expect("no clicked listener");
    let mount_op_count = dom.lock().unwrap().ops.len();

    call_dispatch(&mut h, click);

    // Both Reader instances must have re-rendered with the new value.
    let post_dispatch = dom.lock().unwrap().ops[mount_op_count..].to_vec();
    let reader_updates = post_dispatch
        .iter()
        .filter(
            |op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("read: 1")),
        )
        .count();
    assert_eq!(
        reader_updates, 2,
        "global mutation must fan out to BOTH Reader instances. \
         Pre-Step-7 only the last-mounted one re-rendered (singleton \
         ref global). Saw {} updates: {:?}",
        reader_updates, post_dispatch
    );
}

/// Regression guard: the strings-to-GC migration FIXED the
/// `s32_to_string_aliasing` bug (see
/// `tests/fixtures/known_bugs/runtime/s32_to_string_aliasing.yel`).
///
/// The fixture interleaves two integer reads of distinct globals
/// (Alpha=7, Beta=11) into one Text. Under the old fat-pointer repr two
/// consecutive `s32_to_string` calls shared a static linear-memory
/// buffer, so the second clobbered the first and `concat` produced the
/// truncated `"alpha=1 beta=11"`. Now each `s32_to_string` result is
/// interned into its own `$str_bytes` GC array, so the correct
/// `"alpha=7 beta=11"` is produced.
#[test]
fn s32_to_string_no_aliasing() {
    let source = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/fixtures/known_bugs/runtime/s32_to_string_aliasing.yel",
    ))
    .expect("read s32_to_string_aliasing fixture");
    let bytes = compile_to_component(&source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:s32-alias-bug/app-component@0.1.0", "app");
    let ops = dom.lock().unwrap().ops.clone();
    let buggy = ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "alpha=1 beta=11"));
    let correct = ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "alpha=7 beta=11"));
    // strings-to-GC (`plans/strings-to-gc.md`): each `s32_to_string` result
    // is interned into its own `$str_bytes` GC array, so `alpha`'s
    // conversion is no longer clobbered by `beta`'s.
    assert!(
        correct && !buggy,
        "strings-to-GC fixes the s32_to_string aliasing bug: expected \
         `alpha=7 beta=11`, got ops: {:?}",
        ops
    );
}

/// Two named `global` blocks each compile to their own `$globals_<name>`
/// GC struct + per-block self-global. The post-migration invariant we
/// check here: writing to one block's property does not change the
/// other's, and the two structs both surface their declared defaults
/// independently when read from a single component.
///
/// Pre-migration regression: a single shared linear-memory region
/// could accidentally alias if `mem_base` accounting drifted; per-
/// block GC structs make the isolation structural rather than offset-
/// dependent.
#[test]
fn multi_global_blocks_each_have_own_struct() {
    // Two independent Text elements per global so each `s32_to_string`
    // emits to a separate `CreateText` op — sidestepping the shared
    // string-scratch reuse that would alias adjacent reads in a single
    // concat. This test is about per-block isolation of GC structs,
    // not about the runtime stringification path.
    let source = r#"
        package yel:multiglob@0.1.0;

        global Alpha { x: s32 = 7; }
        global Beta  { y: s32 = 11; }

        component Reader {
            VStack {
                Text { "alpha={Alpha.x}" }
                Text { "beta={Beta.y}" }
            }
        }

        component Writer {
            Button {
                "bump-alpha"
                clicked: { Alpha.x = Alpha.x + 1; }
            }
        }

        export component App {
            VStack { Reader {} Writer {} }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:multiglob/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let saw_alpha = mount_ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "alpha=7"));
    let saw_beta = mount_ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "beta=11"));
    assert!(
        saw_alpha && saw_beta,
        "Reader must render both globals at their declared defaults \
         (alpha=7, beta=11). Per-block GC structs make the two reads \
         independent. Saw alpha={}, beta={}, ops={:?}",
        saw_alpha,
        saw_beta,
        mount_ops
    );

    // Click `bump-alpha`: only Alpha.x must change. Beta.y stays at 11.
    let click = find_listener_handler(&dom, "clicked").expect("no clicked listener");
    let mount_op_count = dom.lock().unwrap().ops.len();
    call_dispatch(&mut h, click);
    let post = dom.lock().unwrap().ops[mount_op_count..].to_vec();
    let saw_alpha_bump = post
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content == "alpha=8"));
    let beta_changed = post.iter().any(|op| {
        matches!(op, DomOp::SetTextContent { content, .. } if content.starts_with("beta=") && content != "beta=11")
    });
    assert!(
        saw_alpha_bump,
        "After bump-alpha, Reader must re-render alpha=8. Aliased \
         storage would leave alpha stale. Post-dispatch ops: {:?}",
        post
    );
    assert!(
        !beta_changed,
        "Bumping Alpha must NOT touch Beta. Aliased storage would update \
         beta's text too. Post-dispatch ops: {:?}",
        post
    );
}

/// Multi-step reactive chain: flipping a bool and reading the getter
/// back must reflect the new value AND the reactive Text effect must
/// have fired. Catches bugs where a signal write succeeds internally
/// but doesn't propagate to downstream effects.
#[test]
fn button_click_propagates_through_full_reactive_chain() {
    let source = r#"
        package yel:chain@0.1.0;
        export component App {
            on: bool = false;
            VStack {
                Text { "on: {on}" }
                Button {
                    "toggle"
                    clicked: { on = !on; }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:chain/app-component@0.1.0", "app");

    let click = find_listener_handler(&dom, "clicked").expect("no clicked listener after mount");
    let mount_op_count = dom.lock().unwrap().ops.len();

    call_dispatch(&mut h, click);

    // 1. The reactive effect must have re-run, producing a new
    //    `set-text-content` whose value reflects `on=true`.
    let all_ops = dom.lock().unwrap().ops.clone();
    let post_dispatch = &all_ops[mount_op_count..];
    let saw_true_update = post_dispatch.iter().any(|op| {
        matches!(op, DomOp::SetTextContent { content, .. }
            if content.contains("true") || content.contains("on: 1"))
    });
    assert!(
        saw_true_update,
        "click should flip `on` to true and trigger a set-text-content \
         whose value contains \"true\". Post-dispatch ops: {:?}",
        post_dispatch
    );

    // 2. The getter must now return true.
    let get_on = get_func(
        &mut h,
        "yel:chain/app-component@0.1.0",
        "[method]app.get-on",
    );
    let mut out = [Val::Bool(false)];
    get_on
        .call(&mut h.store, &[Val::Resource(self_res)], &mut out)
        .expect("get-on");
    let value = match &out[0] {
        Val::Bool(v) => *v,
        other => panic!("get-on returned non-bool {:?}", other),
    };
    assert!(
        value,
        "after click, get-on should return true. Got {}. \
         The signal write didn't land (or it landed but the getter \
         reads a stale location — i.e. the narrow-type packing bug).",
        value
    );
}

/// Regression for the narrow-type packing bug **as runtime behaviour**,
/// not just a bytecode shape. The structural test in `runtime.rs`
/// verifies the setter uses `i32.store8`. This test verifies the
/// OBSERVABLE contract: writing to the bool signal doesn't corrupt the
/// adjacent string signal's value — a read-back of the string must
/// return the original content after any number of bool flips.
///
/// KNOWN BUG (uncovered by this test): `MemoryLayout::new` packs
/// signal offsets without alignment. A `bool` (size 1) at offset 0
/// places the following `string` fat-pointer at offset 1, which is
/// NOT 4-byte aligned. When Wasmtime's component-ABI lift reads the
/// returned pointer, it rejects the access with "return pointer not
/// aligned" before any value corruption is even visible.
///
/// Fix: align signal offsets by the max-of(1, 4, type_size) boundary
/// matching what the canonical ABI expects (4 for i32, 8 for i64/f64,
/// 4 for pointers). The width-correct store/load work we did in
/// Phase-B is still necessary — this test verifies that AFTER the
/// alignment fix, narrow-type writes also don't corrupt neighbours.
#[test]
#[ignore = "known bug: MemoryLayout::new packs narrow + wide signals \
             without alignment — bool at offset 0 puts the adjacent string \
             fat-pointer at offset 1, misaligned for component-ABI lifting"]
fn narrow_type_signal_write_preserves_adjacent_string() {
    let source = r#"
        package yel:narrowrt@0.1.0;
        export component App {
            flag: bool = false;
            label: string = "keep-me";
            VStack {
                Text { "{label}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:narrowrt/app-component@0.1.0", "app");

    // Toggle flag five times via the setter.
    for _ in 0..5 {
        // Flip by reading current, inverting, writing back.
        let get_flag = get_func(
            &mut h,
            "yel:narrowrt/app-component@0.1.0",
            "[method]app.get-flag",
        );
        let mut out = [Val::Bool(false)];
        get_flag
            .call(&mut h.store, &[Val::Resource(self_res)], &mut out)
            .unwrap();
        let flipped = match out[0] {
            Val::Bool(b) => !b,
            _ => panic!(),
        };
        call_setter(
            &mut h,
            "yel:narrowrt/app-component@0.1.0",
            "app",
            "flag",
            &self_res,
            Val::Bool(flipped),
        );
    }

    // After five flips, read back `label`. If the narrow-type write
    // regresses to `i32.store`, the adjacent string's fat-pointer gets
    // clobbered and `get-label` returns garbage (or the empty string).
    let get_label = get_func(
        &mut h,
        "yel:narrowrt/app-component@0.1.0",
        "[method]app.get-label",
    );
    let mut out = [Val::Bool(false)];
    get_label
        .call(&mut h.store, &[Val::Resource(self_res)], &mut out)
        .expect("get-label");
    let label = match &out[0] {
        Val::String(s) => s.clone(),
        other => panic!("get-label returned non-string {:?}", other),
    };
    assert_eq!(
        label, "keep-me",
        "bool signal writes corrupted the adjacent string signal's \
         memory. This is the narrow-type packing bug returning at \
         runtime — `i32.store` instead of `i32.store8` on the bool \
         slot blew out the string's fat-pointer bytes."
    );

    // And the Text in the DOM should still reflect the original string.
    let ops = dom.lock().unwrap().ops.clone();
    assert!(
        ops.iter()
            .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "keep-me")),
        "initial Text creation lost the string content; DOM trace: {:?}",
        ops
    );
}

/// Global-singleton reactive propagation. Changing a global property
/// via a component's event handler must re-fire effects in OTHER
/// components that depend on that global. This is the global-store
/// pattern (CounterStore in the viewer sample) — if it breaks, every
/// multi-component app loses cross-component reactivity.
#[test]
fn global_mutation_fires_cross_component_effect() {
    let source = r#"
        package yel:globalrt@0.1.0;

        global Store { count: s32 = 0; }

        component Writer {
            Button {
                "inc"
                clicked: { Store.count = Store.count + 1; }
            }
        }

        component Reader {
            Text { "read: {Store.count}" }
        }

        export component App {
            VStack {
                Reader {}
                Writer {}
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:globalrt/app-component@0.1.0", "app");

    let click = find_listener_handler(&dom, "clicked").expect("no clicked listener");
    let mount_op_count = dom.lock().unwrap().ops.len();

    call_dispatch(&mut h, click);

    let post = &dom.lock().unwrap().ops[mount_op_count..].to_vec().clone();
    let saw_reader_update = post.iter().any(
        |op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("read: 1")),
    );
    assert!(
        saw_reader_update,
        "mutating the global from Writer's handler must re-render the \
         Reader's Text (it interpolates the same global). \
         Observed post-dispatch ops: {:?}",
        post
    );
}

/// Container component `@children` slot at runtime: the caller's Text
/// node must become a DOM child of the returned children-root. The
/// structural test verifies the WIT `mount -> u32` signature; this
/// test verifies that the returned id is actually used as the parent
/// for caller-supplied children.
#[test]
fn container_component_children_mount_under_returned_root() {
    let source = r#"
        package yel:ctnrrt@0.1.0;

        component Card {
            VStack {
                Text { "chrome" }
                @children
            }
        }

        export component App {
            Card { Text { "payload" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let _self = ctor_and_mount(&mut h, "yel:ctnrrt/app-component@0.1.0", "app");

    // At this point mount has run; inspect the DOM trace.
    let dom: &SharedDom = h.store.data();
    let ops = dom.lock().unwrap().ops.clone();

    // `Text { "payload" }` compiles to a `<Text>` element that wraps a
    // CreateText content node. To verify the container's `@children`
    // slot, we need to find the wrapping Text ELEMENT the caller's
    // subtree produced, and check that *it* was appended under the
    // children-root returned from Card's mount.
    //
    // Walk the DOM graph:
    //   root
    //    └ Card's VStack                       ← children-root
    //       ├ <Text> chrome-wrapper            ← Card's own chrome
    //       │   └ CreateText("chrome")
    //       └ <Text> payload-wrapper           ← caller's Text element
    //           └ CreateText("payload")

    // Find payload's CreateText leaf and walk up to its wrapping element.
    let payload_leaf = ops
        .iter()
        .find_map(|op| match op {
            DomOp::CreateText { content, id } if content == "payload" => Some(*id),
            _ => None,
        })
        .expect("no `payload` text node created");
    let payload_wrapper = ops
        .iter()
        .find_map(|op| match op {
            DomOp::AppendChild { parent, child } if *child == payload_leaf => Some(*parent),
            _ => None,
        })
        .expect("payload leaf never appended");
    // The wrapper's parent is the container target — must NOT be the
    // root and must be the same node Card's own chrome sits under.
    let payload_wrapper_parent = ops
        .iter()
        .find_map(|op| match op {
            DomOp::AppendChild { parent, child } if *child == payload_wrapper => Some(*parent),
            _ => None,
        })
        .expect("payload wrapper never appended");
    assert_ne!(
        payload_wrapper_parent, 1_000_000,
        "caller's `<Text>` wrapper was appended to the root, bypassing \
         Card's children-root. Either `mount` didn't return the slot id, \
         or the caller didn't capture the return value."
    );

    // Same walk for Card's own `chrome` Text — its wrapper must sit
    // under the SAME parent as payload's wrapper (i.e. Card's VStack).
    let chrome_leaf = ops
        .iter()
        .find_map(|op| match op {
            DomOp::CreateText { content, id } if content == "chrome" => Some(*id),
            _ => None,
        })
        .expect("no `chrome` text node created");
    let chrome_wrapper = ops
        .iter()
        .find_map(|op| match op {
            DomOp::AppendChild { parent, child } if *child == chrome_leaf => Some(*parent),
            _ => None,
        })
        .expect("chrome leaf never appended");
    let chrome_wrapper_parent = ops
        .iter()
        .find_map(|op| match op {
            DomOp::AppendChild { parent, child } if *child == chrome_wrapper => Some(*parent),
            _ => None,
        })
        .expect("chrome wrapper never appended");
    assert_eq!(
        chrome_wrapper_parent, payload_wrapper_parent,
        "`payload` wrapper should be a sibling of `chrome` wrapper \
         under Card's VStack. chrome wrapper parent={}, payload \
         wrapper parent={}.",
        chrome_wrapper_parent, payload_wrapper_parent
    );
}

/// `for x in Store.items` must re-evaluate the (global) list iterable
/// whenever `Store.items` is written, growing or shrinking the rendered
/// per-item subtree by the length delta. Before this test landed, the
/// for-loop only registered a reactive effect when the iterable was a
/// bare component-local signal read — any other iterable shape (global
/// field, list literal with signals, stdlib call, signal-bound range)
/// silently went stale after the first mount.
///
/// Drives two handlers:
///   - button 1 sets `Store.items = [1, 2, 3, 4, 5]` (grow 1 → 5).
///   - button 2 sets `Store.items = [10, 20]`        (shrink 5 → 2).
///
/// Asserts that each dispatch produces the expected mount (CreateText
/// "item") / unmount (Remove) DOM-op deltas.
#[test]
fn for_loop_over_global_list_reacts_to_writes() {
    let source = include_str!("fixtures/positive/for_global_list.yel");
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:forgloblist/app-component@0.1.0", "app");

    // Mount-time: Store.items = [1] → one "item" Text node.
    let mount_ops = dom.lock().unwrap().ops.clone();
    let mount_item_count = mount_ops
        .iter()
        .filter(|op| matches!(op, DomOp::CreateText { content, .. } if content == "item"))
        .count();
    assert_eq!(
        mount_item_count, 1,
        "initial Store.items = [1] should mount one \"item\" Text node. \
         Mount ops: {:?}",
        mount_ops
    );

    // Collect the two click handler ids in listener-registration order.
    let click_handlers: Vec<u32> = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .filter(|(_, e, _)| e == "clicked")
        .map(|(_, _, h)| *h)
        .collect();
    assert_eq!(
        click_handlers.len(),
        2,
        "expected 2 click listeners (set-five, set-two); got {:?}",
        click_handlers
    );

    // === Grow: [1] → [1, 2, 3, 4, 5] ===
    let pre_grow_op_count = dom.lock().unwrap().ops.len();
    call_dispatch(&mut h, click_handlers[0]);
    let post_grow = dom.lock().unwrap().ops.clone();
    let grow_delta = &post_grow[pre_grow_op_count..];

    // After 1 → 5 the for-update reuses the survivor (index 0) and
    // mounts only the 4 new tail iterations (indices 1..5) — a proper
    // diff, not a wholesale re-mount. Expect 4 new "item" Text nodes
    // and zero Removes.
    let new_items_after_grow = grow_delta
        .iter()
        .filter(|op| matches!(op, DomOp::CreateText { content, .. } if content == "item"))
        .count();
    assert_eq!(
        new_items_after_grow, 4,
        "growing Store.items from 1 to 5 should create 4 new \"item\" \
         Text nodes (the survivor at index 0 is reused). \
         Post-dispatch ops: {:?}",
        grow_delta
    );
    let removes_after_grow = grow_delta
        .iter()
        .filter(|op| matches!(op, DomOp::Remove { .. }))
        .count();
    assert_eq!(
        removes_after_grow, 0,
        "growing must not unmount any existing iteration — the diff \
         reuses survivors. Post-dispatch ops: {:?}",
        grow_delta
    );

    // === Shrink: [1, 2, 3, 4, 5] → [10, 20] ===
    let pre_shrink_op_count = dom.lock().unwrap().ops.len();
    call_dispatch(&mut h, click_handlers[1]);
    let post_shrink = dom.lock().unwrap().ops.clone();
    let shrink_delta = &post_shrink[pre_shrink_op_count..];

    // After 5 → 2, for-update tears down all 5 and mounts 2 new ones.
    // We require 5 Removes (one per old item) and 2 new "item" creates.
    let removes_after_shrink = shrink_delta
        .iter()
        .filter(|op| matches!(op, DomOp::Remove { .. }))
        .count();
    let new_items_after_shrink = shrink_delta
        .iter()
        .filter(|op| matches!(op, DomOp::CreateText { content, .. } if content == "item"))
        .count();
    assert_eq!(
        removes_after_shrink, 3,
        "shrinking Store.items from 5 to 2 should unmount only the \
         tail (3 iterations) — survivors at indices 0..2 are reused. \
         Post-dispatch ops: {:?}",
        shrink_delta
    );
    assert_eq!(
        new_items_after_shrink, 0,
        "shrinking must not mount new iterations — survivors are \
         reused in place. Post-dispatch ops: {:?}",
        shrink_delta
    );
}

// ============================================================================
// Derived-signal reactivity tests
// ============================================================================
//
// A "derived signal" is a property whose default expression reads one or
// more other signals:
//
//     count: s32 = 0;
//     doubled: s32 = count * 2;
//
// The intended semantics: `doubled` recomputes whenever any of its sources
// changes, and any effect reading `doubled` re-runs. Today the compiler
// evaluates the default once at construction time and never re-runs it —
// so Text rendering `doubled` goes stale the moment `count` is written.
//
// These tests assert the correct semantics; they fail until the derived-
// signal effect is wired through LIR lowering.

/// Baseline: initial value of a derived signal must be correct at mount
/// time, before any setter call. (The constructor already evaluates
/// defaults, so this should pass even before the reactivity fix lands.)
#[test]
fn derived_signal_initial_value_is_correct_at_mount() {
    let source = r#"
        package yel:deriv-init@0.1.0;
        export component App {
            count: s32 = 5;
            doubled: s32 = count * 2;
            VStack {
                Text { "doubled: {doubled}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let _self_res = ctor_and_mount(&mut h, "yel:deriv-init/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let saw_initial_text = mount_ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "doubled: 10"));
    assert!(
        saw_initial_text,
        "mount should render `doubled: 10` from `doubled = count * 2` with \
         initial count=5. Mount ops: {:?}",
        mount_ops
    );
}

/// Core reactivity: writing the source signal must re-evaluate the
/// derived signal AND re-run any effect reading it. Fails today because
/// no effect is registered for the `doubled = count * 2` dependency.
#[test]
fn derived_signal_updates_when_source_changes() {
    let source = r#"
        package yel:deriv-src@0.1.0;
        export component App {
            count: s32 = 0;
            doubled: s32 = count * 2;
            VStack {
                Text { "doubled: {doubled}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:deriv-src/app-component@0.1.0", "app");

    let mount_op_count = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:deriv-src/app-component@0.1.0",
        "app",
        "count",
        &self_res,
        Val::S32(7),
    );

    let all_ops = dom.lock().unwrap().ops.clone();
    let after_setter = &all_ops[mount_op_count..];
    let saw_updated_text = after_setter
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content == "doubled: 14"));
    assert!(
        saw_updated_text,
        "writing count=7 should recompute doubled=14 and trigger a \
         `set-text-content` with \"doubled: 14\". Setter-induced ops: {:?}",
        after_setter
    );
}

/// Chain: a → b → c. Writing `a` must propagate through both derived
/// levels. Fails today; propagation stops at `a` because `b` and `c`
/// have no effects registered for their source deps.
#[test]
fn derived_signal_chain_propagates_through_multiple_levels() {
    let source = r#"
        package yel:deriv-chain@0.1.0;
        export component App {
            a: s32 = 1;
            b: s32 = a + 10;
            c: s32 = b * 2;
            VStack {
                Text { "c: {c}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:deriv-chain/app-component@0.1.0", "app");

    let mount_op_count = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:deriv-chain/app-component@0.1.0",
        "app",
        "a",
        &self_res,
        Val::S32(5),
    );

    let all_ops = dom.lock().unwrap().ops.clone();
    let after_setter = &all_ops[mount_op_count..];
    // a=5 → b = 5 + 10 = 15 → c = 15 * 2 = 30
    let saw_updated_text = after_setter
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content == "c: 30"));
    assert!(
        saw_updated_text,
        "writing a=5 should propagate a→b→c and render \"c: 30\". \
         Setter-induced ops: {:?}",
        after_setter
    );
}

/// A derived signal with multiple source dependencies must recompute
/// when any of them changes. Here `total = price * quantity`: writing
/// either input should update the rendered total.
#[test]
fn derived_signal_with_multiple_deps_updates_when_any_changes() {
    let source = r#"
        package yel:deriv-multi@0.1.0;
        export component App {
            price: s32 = 10;
            quantity: s32 = 3;
            total: s32 = price * quantity;
            VStack {
                Text { "total: {total}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:deriv-multi/app-component@0.1.0", "app");

    // Write #1: change `price`.
    let pre_price_op_count = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:deriv-multi/app-component@0.1.0",
        "app",
        "price",
        &self_res,
        Val::S32(20),
    );
    let after_price = dom.lock().unwrap().ops[pre_price_op_count..].to_vec();
    // 20 * 3 = 60
    let saw_after_price = after_price
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content == "total: 60"));
    assert!(
        saw_after_price,
        "writing price=20 should recompute total=60 (20 * 3). \
         Setter-induced ops: {:?}",
        after_price
    );

    // Write #2: change `quantity`.
    let pre_qty_op_count = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:deriv-multi/app-component@0.1.0",
        "app",
        "quantity",
        &self_res,
        Val::S32(4),
    );
    let after_qty = dom.lock().unwrap().ops[pre_qty_op_count..].to_vec();
    // 20 * 4 = 80
    let saw_after_qty = after_qty
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content == "total: 80"));
    assert!(
        saw_after_qty,
        "writing quantity=4 should recompute total=80 (20 * 4). \
         Setter-induced ops: {:?}",
        after_qty
    );
}

/// Derived signal version of the canonical temperature converter:
/// `fahrenheit` derives from `celsius`. Setting celsius propagates.
/// This is the one-way case enabled by the derived-signal effect
/// lowering — the two-way Input binding is a separate open gap
/// (see `temp_converter_two_way_input_bindings_propagate`).
#[test]
fn temp_converter_derived_celsius_to_fahrenheit() {
    let source = r#"
        package yel:temp-oneway@0.1.0;
        export component TempConverter {
            celsius: f32 = 0.0;
            fahrenheit: f32 = 32.0 + (9.0 / 5.0) * celsius;
            VStack {
                Text { "F={fahrenheit}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(
        &mut h,
        "yel:temp-oneway/temp-converter-component@0.1.0",
        "temp-converter",
    );

    // Initial render: celsius=0 → fahrenheit=32.
    let mount_ops = dom.lock().unwrap().ops.clone();
    let saw_initial = mount_ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content.starts_with("F=32")));
    assert!(
        saw_initial,
        "mount should render fahrenheit=32 from celsius=0. Mount ops: {:?}",
        mount_ops
    );

    // Write celsius=100 → expect fahrenheit=212.
    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:temp-oneway/temp-converter-component@0.1.0",
        "temp-converter",
        "celsius",
        &self_res,
        Val::Float32(100.0),
    );
    let after = dom.lock().unwrap().ops[pre..].to_vec();
    let saw_updated = after.iter().any(
        |op| matches!(op, DomOp::SetTextContent { content, .. } if content.starts_with("F=212")),
    );
    assert!(
        saw_updated,
        "writing celsius=100 should recompute fahrenheit=212 (32+9/5*100) and \
         update the text. Setter-induced ops: {:?}",
        after
    );
}

/// Two-way Input binding from 7GUIs temperature converter. Mirrors
/// the Svelte reference:
///
/// ```html
/// <input value={c} oninput={e => { c = +e.target.value;
///     f = +(32 + (9/5) * c).toFixed(1); }}>
/// <input value={f} oninput={e => { f = +e.target.value;
///     c = +((5/9) * (f - 32)).toFixed(1); }}>
/// ```
///
/// Critical Svelte semantic: the setter closure fires ONLY on user
/// input events. Programmatic writes to `c` do NOT trigger the
/// closure — only DOM `input` events do. That distinguishes this from
/// derived-signal reactivity (where any write propagates).
///
/// Expected lowering: `value: celsius` + `set value: { body }` becomes
///   1. A reactive attribute effect (getter) — already works.
///   2. An event handler on the Input's `input` event that
///      (a) writes the typed DOM value back into `celsius`,
///      (b) runs `body` in the post-write state.
///
/// Currently the setter closure is parsed + type-checked but dropped at
/// LIR lowering, so step (2) is missing entirely. Marked `#[ignore]`
/// until binding-setter lowering lands. Remove the ignore then.
#[test]
fn temp_converter_two_way_input_bindings_svelte_semantics() {
    let source = r#"
        package yel:temp-twoway@0.1.0;
        export component TempConverter {
            celsius: f32 = 0.0;
            fahrenheit: f32 = 32.0;
            VStack {
                FloatInput {
                    value: celsius
                    set value: {
                        fahrenheit = 32.0 + (9.0 / 5.0) * celsius;
                    }
                }
                FloatInput {
                    value: fahrenheit
                    set value: {
                        celsius = (5.0 / 9.0) * (fahrenheit - 32.0);
                    }
                }
                Text { "c={celsius} f={fahrenheit}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(
        &mut h,
        "yel:temp-twoway/temp-converter-component@0.1.0",
        "temp-converter",
    );

    // --- PART 1: Programmatic writes must NOT fire the setter closure. ---
    //
    // Writing celsius directly is equivalent to `c = 5` in JS — Svelte's
    // oninput handler doesn't run because no DOM input event occurred.
    // fahrenheit should remain at 32 (its default), not become 32+9/5*5=41.
    let pre_prog = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:temp-twoway/temp-converter-component@0.1.0",
        "temp-converter",
        "celsius",
        &self_res,
        Val::Float32(5.0),
    );
    let after_prog = dom.lock().unwrap().ops[pre_prog..].to_vec();
    let saw_f_update = after_prog
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("f=41")));
    assert!(
        !saw_f_update,
        "programmatic set-celsius(5) must NOT run the input-event closure \
         (Svelte `oninput` semantics); fahrenheit should remain at 32. \
         Observed setter-induced ops: {:?}",
        after_prog
    );

    // --- PART 2: Simulate `<input type="number">` firing `input`
    // with value=100 via `dispatch(handler_id, event-value::input-f64(100.0))`.
    // The generated dispatch preamble must:
    //   a. parse the input-f64 arm,
    //   b. demote f64→f32 and write celsius,
    //   c. trigger effects watching celsius,
    //   d. then run the user-authored `set value: { ... }` body, which
    //      sees celsius=100 and computes fahrenheit = 32 + 9/5 * 100 = 212.
    let c_input_handler = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .find(|(_, e, _)| e == "input" || e == "change")
        .map(|(_, _, h)| *h)
        .expect(
            "Input element should register an `input` (or `change`) event handler \
             for its `set value:` closure",
        );
    let pre_evt = dom.lock().unwrap().ops.len();
    dispatch_input_number(&mut h, c_input_handler, 100.0);
    let after_evt = dom.lock().unwrap().ops[pre_evt..].to_vec();
    let saw_f_update_after_event = after_evt
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("f=212")));
    assert!(
        saw_f_update_after_event,
        "dispatching `input-f64(100.0)` to the C-input handler should: \
         (1) auto-sync celsius=100, (2) trigger effects, (3) run the setter \
         body writing fahrenheit=212. Post-dispatch ops: {:?}",
        after_evt
    );
}

/// Input binding on a `u32` signal: typing a number in the Input
/// should auto-sync into the signal, trigger effects, and update the
/// text interpolation. Mirrors the checkerboard sample's customisation
/// controls.
#[test]
fn input_binding_u32_autosync() {
    let source = r#"
        package yel:u32bind@0.1.0;
        export component App {
            n: u32 = 5;
            VStack {
                IntegerInput { bind value: n }
                Text { "n={n}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let _self_res = ctor_and_mount(&mut h, "yel:u32bind/app-component@0.1.0", "app");

    // Find the Input's registered event handler.
    let input_handler = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .find(|(_, e, _)| e == "input")
        .map(|(_, _, h)| *h)
        .expect("Input should register an `input` event handler");

    let pre = dom.lock().unwrap().ops.len();
    dispatch_input_number(&mut h, input_handler, 12.0);
    let after = dom.lock().unwrap().ops[pre..].to_vec();

    let saw_text_update = after
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content == "n=12"));
    assert!(
        saw_text_update,
        "dispatch_input_number(12.0) on u32 signal should update text to \"n=12\". \
         Post-dispatch ops: {:?}",
        after
    );
}

/// Regression: initial binding of a `u32` signal to an input's `value`
/// attribute must be reflected in the dispatched `set-attribute` variant
/// payload. Previously the canonical-ABI flattening for narrow-int
/// attribute arms placed the value in the i32-slot with the i64-slot
/// padded to 0; jco/wasmtime read slot-0 as the payload, giving 0.
#[test]
fn narrow_int_attribute_variant_roundtrips_payload() {
    let source = r#"
        package yel:nv@0.1.0;
        export component App {
            n: u32 = 8;
            VStack {
                IntegerInput { bind value: n }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let _self_res = ctor_and_mount(&mut h, "yel:nv/app-component@0.1.0", "app");
    let ops = dom.lock().unwrap().ops.clone();
    let set_value = ops
        .iter()
        .find_map(|op| match op {
            DomOp::SetAttribute { name, value, .. } if name == "value" => Some(value.clone()),
            _ => None,
        })
        .expect("set-attribute(value) must fire at mount");
    assert!(
        set_value.contains("U32(8)"),
        "initial value attribute payload must be 8, got: {}",
        set_value
    );
}

/// Dispatch-driven diff regression: changing a signal that drives a
/// `for` bound should diff the rendered grid — add/remove iterations
/// without duplicating existing children.
///
/// Currently broken: shrinking rows 3→2 emits three bogus
/// `Remove { node: 0 }` ops (the per-iteration handle slots are loading
/// zeros — the for-loop unmount stores handles to a slot that gets
/// clobbered) and then *re-creates* both surviving rows from scratch.
/// Growing 2→4 similarly re-creates the first two rows instead of just
/// appending the new ones. The reactive update_block for a for-loop
/// driven by a signal bound should diff iterations, not wholesale
/// re-mount the body.
///
/// Modelled on the checkerboard sample: an `IntegerInput { bind value: rows }`
/// feeds a `for row in 0..rows` that holds a nested `if/else` Box body.
/// Dispatch drives `rows`, exercising the full reactive path
/// (binding-setter → signal write → effect re-run → DOM diff).
#[test]
fn input_driven_grid_diffs_when_for_bound_shrinks_and_grows() {
    let source = r#"
        package yel:grid-diff@0.1.0;
        export component App {
            rows: u32 = 3;
            VStack {
                IntegerInput { bind value: rows }
                VStack {
                    for row in 0..rows {
                        HStack {
                            if row % 2 == 0 {
                                Box { style: "r:{row}" }
                            } else {
                                Box { style: "r:{row}" }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let _ = ctor_and_mount(&mut h, "yel:grid-diff/app-component@0.1.0", "app");

    // Initial mount: 3 rows → 3 HStack rows and 3 Box children.
    let initial = dom.lock().unwrap().ops.clone();
    let count_tag = |ops: &[DomOp], tag: &str| {
        ops.iter()
            .filter(|op| matches!(op, DomOp::CreateElement { tag: t, .. } if t == tag))
            .count()
    };
    assert_eq!(
        count_tag(&initial, "HStack"),
        3,
        "initial mount: 3 rows → 3 HStack creations"
    );
    assert_eq!(
        count_tag(&initial, "Box"),
        3,
        "initial mount: 3 rows → 3 Box creations"
    );

    // Find the IntegerInput's input handler (registered for `bind value:`).
    let input_handler = dom
        .lock()
        .unwrap()
        .listeners
        .iter()
        .find(|(_, e, _)| e == "input")
        .map(|(_, _, h)| *h)
        .expect("IntegerInput must register an `input` handler");

    // Shrink rows 3 → 2: diff should remove, not re-create.
    let pre_shrink = dom.lock().unwrap().ops.len();
    dispatch_input_number(&mut h, input_handler, 2.0);
    let post_shrink = dom.lock().unwrap().ops[pre_shrink..].to_vec();
    let creates_after_shrink = count_tag(&post_shrink, "HStack") + count_tag(&post_shrink, "Box");
    let removes_after_shrink = post_shrink
        .iter()
        .filter(|op| matches!(op, DomOp::Remove { .. }))
        .count();
    assert!(
        removes_after_shrink >= 1,
        "shrinking rows 3→2 must emit at least one `remove` op (one row removed). \
         Ops after dispatch: {:?}",
        post_shrink
    );
    assert!(
        creates_after_shrink == 0,
        "shrinking rows must NOT re-create the surviving rows. \
         Observed {} HStack/Box CreateElement ops after shrink — the diff is \
         re-mounting the whole for-body instead of removing one entry. \
         Post-dispatch ops: {:?}",
        creates_after_shrink,
        post_shrink
    );

    // Grow rows 2 → 4: diff should add two new HStack+Box pairs (not recreate the first two).
    let pre_grow = dom.lock().unwrap().ops.len();
    dispatch_input_number(&mut h, input_handler, 4.0);
    let post_grow = dom.lock().unwrap().ops[pre_grow..].to_vec();
    let hstack_creates = count_tag(&post_grow, "HStack");
    let box_creates = count_tag(&post_grow, "Box");
    assert_eq!(
        hstack_creates, 2,
        "growing rows 2→4 must create exactly 2 new HStack rows (the delta), got {}. \
         A figure larger than 2 means the existing rows are also being re-created — \
         the diff is not reusing existing nodes. Ops: {:?}",
        hstack_creates, post_grow
    );
    assert_eq!(
        box_creates, 2,
        "growing rows 2→4 must create exactly 2 new Box cells (one per new row), got {}. \
         Ops: {:?}",
        box_creates, post_grow
    );
}

/// Compound regression: grow the outer loop first, then grow the
/// inner loop. The newly-added outer iter's record must stash its
/// item value so the subsequent inner fan-out sees the correct
/// outer variable for that row. Pre-fix, the rows-grow path (which
/// runs through the for-update block's mount tail, not the initial
/// mount) skipped the item-value stash, leaving value=0 in the new
/// row's record; then when cols changed, the new row's cells all
/// computed as if row=0.
#[test]
fn checker_pattern_survives_grow_rows_then_grow_cols() {
    let source = r#"
        package yel:checker2@0.1.0;
        export component App {
            rows: u32 = 2;
            cols: u32 = 2;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            if (row + col) % 2 == 0 {
                                Box { style: "white" }
                            } else {
                                Box { style: "dim" }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:checker2/app-component@0.1.0", "app");

    // Grow rows 2 → 3. The new outer iter (row=2) should mount its
    // initial 2 cells as white/dim (col=0: white, col=1: dim).
    let pre_rows = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:checker2/app-component@0.1.0",
        "app",
        "rows",
        &self_res,
        Val::U32(3),
    );
    let post_rows = dom.lock().unwrap().ops[pre_rows..].to_vec();
    let rows_styles: Vec<String> = post_rows
        .iter()
        .filter_map(|op| match op {
            DomOp::SetAttribute { name, value, .. } if name == "style" => Some(value.clone()),
            _ => None,
        })
        .collect();
    eprintln!("after rows grow, styles: {:?}", rows_styles);

    // Now grow cols 2 → 3. The new column (col=2) must produce the
    // correct alternation across ALL rows, including the just-added
    // row=2: white / dim / white.
    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:checker2/app-component@0.1.0",
        "app",
        "cols",
        &self_res,
        Val::U32(3),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    let new_box_styles: Vec<String> = post
        .iter()
        .filter_map(|op| match op {
            DomOp::SetAttribute { name, value, .. } if name == "style" => Some(value.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        new_box_styles.len(),
        3,
        "expected 3 new cells (one per row), got {}. Ops: {:?}",
        new_box_styles.len(),
        post
    );
    let whites = new_box_styles
        .iter()
        .filter(|s| s.contains("white"))
        .count();
    let dims = new_box_styles.iter().filter(|s| s.contains("dim")).count();
    assert_eq!(
        (whites, dims),
        (2, 1),
        "after growing rows then growing cols, the new column should \
         alternate white/dim/white — got {:?}. If all three are the \
         same, the rows-grow path didn't stash the new row's item \
         value into its tracking record.",
        new_box_styles
    );
}

/// Checker pattern regression: growing `cols` must produce
/// alternating-color Boxes using the correct outer loop variable
/// (`row`) per row. A broken fan-out rebinds only the inner state
/// and leaves the outer `row` stuck at the last-mounted value, so
/// every newly-added column has the same color across all rows.
#[test]
fn checker_pattern_preserved_when_inner_for_bound_grows() {
    let source = r#"
        package yel:checker@0.1.0;
        export component App {
            rows: u32 = 3;
            cols: u32 = 2;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            if (row + col) % 2 == 0 {
                                Box { style: "white" }
                            } else {
                                Box { style: "dim" }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:checker/app-component@0.1.0", "app");

    let pre = dom.lock().unwrap().ops.len();
    // Grow cols 2 → 3. The new column is index 2, so:
    //   row=0 → (0+2)%2=0 → white
    //   row=1 → (1+2)%2=1 → dim
    //   row=2 → (2+2)%2=0 → white
    call_setter(
        &mut h,
        "yel:checker/app-component@0.1.0",
        "app",
        "cols",
        &self_res,
        Val::U32(3),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    // Count alternation among the newly-created Boxes.
    let new_box_styles: Vec<String> = post
        .iter()
        .filter_map(|op| match op {
            DomOp::SetAttribute { name, value, .. } if name == "style" => Some(value.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        new_box_styles.len(),
        3,
        "expected 3 new Box styles (one per row), got {}. Ops: {:?}",
        new_box_styles.len(),
        post
    );
    let whites = new_box_styles
        .iter()
        .filter(|s| s.contains("white"))
        .count();
    let dims = new_box_styles.iter().filter(|s| s.contains("dim")).count();
    assert_eq!(
        (whites, dims),
        (2, 1),
        "new column should alternate white/dim/white based on each row's \
         own `row` value — got styles: {:?}. If all 3 are the same, the \
         fan-out isn't rebinding the outer loop variable.",
        new_box_styles
    );
}

/// Three-level nested for: a signal bound on the innermost for
/// must fan out across every path through the two enclosing fors.
///
/// Structure:
/// ```
/// for a in 0..A {
///     for b in 0..B {
///         for c in 0..C { ... }
///     }
/// }
/// ```
/// With A=2, B=2, C=3 → 12 innermost leaves. Shrinking `C` from 3 to
/// 2 must emit one `Remove` per (a, b) pair — so A*B = 4 total, not
/// just one. Catches the silent single-level-only wrap bug: an
/// implementation that only walks the immediate enclosing-for runs
/// the inner diff against whichever (a,b) happens to have its state
/// in the static slots, missing the others.
#[test]
fn innermost_for_reacts_across_all_outer_nesting_levels() {
    let source = r#"
        package yel:triple@0.1.0;
        export component App {
            a-count: u32 = 2;
            b-count: u32 = 2;
            c-count: u32 = 3;
            VStack {
                for a in 0..a-count {
                    HStack {
                        for b in 0..b-count {
                            VStack {
                                for c in 0..c-count {
                                    Box { style: "x" }
                                }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:triple/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let initial_box_count = mount_ops
        .iter()
        .filter(|op| matches!(op, DomOp::CreateElement { tag, .. } if tag == "Box"))
        .count();
    assert_eq!(
        initial_box_count, 12,
        "A=2 * B=2 * C=3 = 12 Boxes on initial mount; got {}",
        initial_box_count
    );

    // Shrink c-count 3 → 2. Every (a, b) pair has its own innermost
    // tracking array — shrinking once must fire A*B = 4 Removes, not
    // one. An implementation that only wraps at ONE ancestor level
    // runs the diff against whichever (a,b) was most recently active
    // and drops the other three pairs entirely.
    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:triple/app-component@0.1.0",
        "app",
        "c-count",
        &self_res,
        Val::U32(2),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();
    let removes = post
        .iter()
        .filter(|op| matches!(op, DomOp::Remove { .. }))
        .count();
    assert_eq!(
        removes, 4,
        "shrinking c-count 3→2 in a 2×2 outer grid must drop 1 Box \
         per (a,b) pair (4 total), got {}. Ops: {:?}",
        removes, post
    );
}

/// Inner for-loop reacts per outer iteration (the `cols` bug):
///
/// The sample has `for row in 0..rows { HStack { for col in 0..cols { Box } } }`.
/// When `cols` changes, every row's inner for-loop must diff
/// independently — if the inner-for's update runs only once per
/// signal change, only one row ends up with the new column count.
/// The fan-out model needs to walk the outer for's tracking array
/// and run the inner diff against each outer iteration's per-record
/// tracking state.
#[test]
fn inner_for_reacts_per_outer_iteration() {
    let source = r#"
        package yel:innerfor@0.1.0;
        export component App {
            rows: u32 = 3;
            cols: u32 = 3;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            Box { style: "x" }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:innerfor/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let box_count = mount_ops
        .iter()
        .filter(|op| matches!(op, DomOp::CreateElement { tag, .. } if tag == "Box"))
        .count();
    assert_eq!(box_count, 9, "mount 3x3 grid -> 9 Boxes; got {}", box_count);

    // Shrink cols 3 → 2. Every outer row (3 of them) should drop
    // exactly one Box — 3 Removes total. Pre-fan-out the inner update
    // runs only once against whichever row's static slot was last
    // written, giving a single Remove.
    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:innerfor/app-component@0.1.0",
        "app",
        "cols",
        &self_res,
        Val::U32(2),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();
    let removes = post
        .iter()
        .filter(|op| matches!(op, DomOp::Remove { .. }))
        .count();
    assert_eq!(
        removes, 3,
        "shrinking cols 3→2 must drop 1 Box per row (3 total), got {}. \
         Ops: {:?}",
        removes, post
    );
}

/// Per-iteration attribute reactivity (fan-out):
///
/// When a signal read inside a `for` body's attribute binding
/// changes, the update must fire SetAttribute on every live iteration
/// — not just the one whose handle happens to sit in the static
/// memory slot. Without fan-out, the per-template-position effect
/// updates only the last-mounted iteration and every surviving Box
/// retains its stale style.
///
/// Setup: 3 Box elements whose `style` reads `cell-size`. Change
/// `cell-size` 24 → 48. We expect THREE SetAttribute ops, one per
/// Box node id.
#[test]
fn per_iteration_attribute_effect_fans_out_across_grid() {
    let source = r#"
        package yel:fanattr@0.1.0;
        export component App {
            rows: u32 = 3;
            cell-size: u32 = 24;
            VStack {
                for row in 0..rows {
                    Box { style: "w:{cell-size}" }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:fanattr/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let box_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(box_ids.len(), 3, "expected 3 Boxes, got {:?}", box_ids);

    // Dispatch the cell-size setter. Update-block must fan out and
    // call set-attribute on each Box's handle with the new value.
    let pre_count = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:fanattr/app-component@0.1.0",
        "app",
        "cell-size",
        &self_res,
        Val::U32(48),
    );
    let post = dom.lock().unwrap().ops[pre_count..].to_vec();

    for id in &box_ids {
        let got = post.iter().any(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, name, value }
                    if node == id && name == "style" && value.contains("w:48")
            )
        });
        assert!(
            got,
            "Box node {} didn't get its style re-applied after \
             cell-size change; fan-out update_block is dropping this \
             iteration. Ops: {:?}",
            id, post
        );
    }
}

/// Nested for-loop regression: every Box created inside a
/// `for row in 0..rows { for col in 0..cols { Box { style: "…{sig}…" } } }`
/// grid must get its own `set-attribute("style", …)` call, with the
/// signal-interpolated value baked in. The checkerboard sample depends
/// on this — a missing or merged setAttribute makes the cells invisible.
#[test]
fn nested_for_loop_applies_attributes_per_iteration() {
    let source = r#"
        package yel:chk@0.1.0;
        export component App {
            rows: u32 = 2;
            cols: u32 = 2;
            cell-size: u32 = 24;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            if (row + col) % 2 == 0 {
                                Box {
                                    style: "w:{cell-size}px"
                                }
                            } else {
                                Box {
                                    style: "w:{cell-size}px"
                                }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let _ = ctor_and_mount(&mut h, "yel:chk/app-component@0.1.0", "app");
    let ops = dom.lock().unwrap().ops.clone();

    let box_ids: Vec<u32> = ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        box_ids.len(),
        4,
        "expected 2×2 Box grid (one CreateElement per iteration), got {} — ops: {:?}",
        box_ids.len(),
        ops
    );

    for id in &box_ids {
        let applied = ops.iter().any(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, name, value }
                    if node == id && name == "style" && value.contains("w:24px")
            )
        });
        assert!(
            applied,
            "Box node {} is missing its `style: \"w:{{cell-size}}px\"` set-attribute \
             with the interpolated cell-size baked in. Full op trace: {:?}",
            id, ops
        );
    }
}

/// Update-path conditional rendering: when an `if`-`else if` chain's
/// active branch must change in response to a signal write, the new
/// branch's content must be MOUNTED (CreateElement / CreateText for
/// the Text wrapping its content). The mount path can start with no
/// active branch — what we test here is the *update* dispatch that
/// fires from the signal-write trigger.
///
/// This is the counter example's `else if count < 0 { Text { "Negative!" } }`
/// case: count starts at 0 (neither branch active), then flips to -1
/// (the `count < 0` branch should mount).
///
/// Regression symptom: after `set count(-1)`, no `CreateElement Text`
/// runs and no `CreateText "Negative!"` runs — the else-if branch
/// silently fails to render.
#[test]
fn if_update_path_mounts_else_if_branch_when_active() {
    let source = r#"
        package yel:ifupdate@0.1.0;
        export component App {
            count: s32 = 0;
            VStack {
                if count > 10 {
                    Text { "High!" }
                } else if count < 0 {
                    Text { "Negative!" }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:ifupdate/app-component@0.1.0", "app");

    let mount_op_count = dom.lock().unwrap().ops.len();

    // Flip count to a value that activates the else-if branch.
    call_setter(
        &mut h,
        "yel:ifupdate/app-component@0.1.0",
        "app",
        "count",
        &self_res,
        Val::S32(-1),
    );

    let all_ops = dom.lock().unwrap().ops.clone();
    let after_setter = &all_ops[mount_op_count..];

    let mounted_negative = after_setter
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "Negative!"));
    assert!(
        mounted_negative,
        "Setting `count=-1` should mount the else-if branch — \
         `CreateText \"Negative!\"` must fire — but the update path \
         emitted no such op. After-setter ops: {:?}\nFull trace: {:?}",
        after_setter, all_ops
    );
}

/// Inner-for shrink: when `cols` decrements from 3 to 2 in a checker
/// pattern, every row's rightmost cell must be unmounted. Each iter
/// wraps its content in a host fragment element (`yel-frag`); a single
/// `Remove(wrapper)` per dropped iter cascades to detach the cell's
/// Box and any anchor comments via the DOM tree. With R rows and 1
/// dropped column, R `Remove` ops are required — one per row's
/// children-array shrink, each targeting that row's column-2 wrapper.
///
/// A buggy implementation that walks only the innermost for state (or
/// only the first row) emits 0 or 1 Remove and leaves R-1 stale DOM
/// subtrees alive.
///
/// Counterpart to `checker_pattern_preserved_when_inner_for_bound_grows`
/// — same shape, opposite direction.
#[test]
fn checker_pattern_unmounts_inner_for_when_bound_shrinks() {
    let source = r#"
        package yel:checkershrink@0.1.0;
        export component App {
            rows: u32 = 3;
            cols: u32 = 3;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            if (row + col) % 2 == 0 {
                                Box { style: "white" }
                            } else {
                                Box { style: "dim" }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:checkershrink/app-component@0.1.0", "app");

    // After mount: 3 rows × 3 cols = 9 cells. Each iter-mount AND
    // each if-branch-mount emits a `yel-frag` wrapper before its body
    // content. Per row that's: 1 outer-row wrapper + 3 cells × (1
    // iter wrapper + 1 active-branch wrapper) = 7 wrappers/row × 3
    // rows = 21 wrappers.
    //
    // Within a row's 7 wrappers (group of 7):
    //   offset 0   — outer-row wrapper
    //   offsets 1, 3, 5 — cell iter wrappers (col 0, 1, 2)
    //   offsets 2, 4, 6 — cell active-branch wrappers (col 0, 1, 2)
    let mount_ops = dom.lock().unwrap().ops.clone();
    let frag_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "yel-frag" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        frag_ids.len(),
        21,
        "mount should have created 21 yel-frag wrappers (3 rows × 7 \
         wrappers/row); got {}. Ops: {:?}",
        frag_ids.len(),
        mount_ops
    );
    let cell_iter_wrapper = |row: usize, col: usize| frag_ids[row * 7 + 1 + col * 2];
    // Rightmost-column iter wrappers per row.
    let rightmost_wrappers: [u32; 3] = [
        cell_iter_wrapper(0, 2),
        cell_iter_wrapper(1, 2),
        cell_iter_wrapper(2, 2),
    ];
    // Surviving cell iter wrappers (cols 0 and 1 of each row).
    let surviving_wrappers: Vec<u32> = (0..3)
        .flat_map(|row| [cell_iter_wrapper(row, 0), cell_iter_wrapper(row, 1)])
        .collect();

    let pre_shrink = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:checkershrink/app-component@0.1.0",
        "app",
        "cols",
        &self_res,
        Val::U32(2),
    );
    let post_shrink = dom.lock().unwrap().ops[pre_shrink..].to_vec();

    // Every rightmost-column cell's wrapper must be Remove'd; the
    // browser then detaches the cell's Box + anchors via DOM cascade.
    for id in &rightmost_wrappers {
        let removed = post_shrink
            .iter()
            .any(|op| matches!(op, DomOp::Remove { node } if node == id));
        assert!(
            removed,
            "yel-frag wrapper id {} (a rightmost-column cell from before \
             the shrink) was not Remove'd after `cols` decremented from 3 \
             to 2. The inner for's diff path is failing to walk every \
             row's children-array. After-setter ops: {:?}",
            id, post_shrink
        );
    }

    // No surviving left-column cell wrapper should have been Remove'd.
    for id in &surviving_wrappers {
        let bogus_remove = post_shrink
            .iter()
            .any(|op| matches!(op, DomOp::Remove { node } if node == id));
        assert!(
            !bogus_remove,
            "yel-frag wrapper id {} (a surviving left-column cell) was \
             incorrectly Remove'd during cols shrink. After-setter ops: {:?}",
            id, post_shrink
        );
    }
}

/// If-branch body containing another `if`: when the OUTER if toggles
/// false (and its then-branch unmounts), the inner if's content (a
/// Box) must be detached too. Today's branch-unmount does
/// `Remove(branch.wrapper)` where `branch.wrapper` is the branch's
/// first DOM op — for a branch body that starts with another `if`,
/// that's the inner if-anchor comment. Removing the comment leaves
/// the inner if's active Box orphaned in the DOM as a sibling.
///
/// The fix: each if-branch wraps its body in a `yel-frag` element so
/// `Remove(wrapper)` cascades. Symmetric to the for-iter wrapper fix
/// landed for `checker_pattern_unmounts_inner_for_when_bound_shrinks`.
#[test]
fn nested_if_unmounts_inner_when_outer_branch_unmounts() {
    let source = r#"
        package yel:nestedif@0.1.0;
        export component App {
            outer: bool = true;
            inner-val: s32 = 1;
            VStack {
                if outer {
                    if inner-val > 0 {
                        Box { style: "case-A" }
                    } else {
                        Box { style: "case-B" }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:nestedif/app-component@0.1.0", "app");

    // Mount with outer=true, inner-val=1 → case-A's Box is mounted.
    let mount_ops = dom.lock().unwrap().ops.clone();
    let case_a_box_id = mount_ops
        .iter()
        .find_map(|op| match op {
            DomOp::SetAttribute { node, name, value }
                if name == "style" && value.contains("case-A") =>
            {
                Some(*node)
            }
            _ => None,
        })
        .expect("mount should set style=case-A on the active branch's Box");

    let pre = dom.lock().unwrap().ops.len();
    // Toggle outer false: outer-then's branch should unmount.
    call_setter(
        &mut h,
        "yel:nestedif/app-component@0.1.0",
        "app",
        "outer",
        &self_res,
        Val::Bool(false),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    // Either the case-A Box itself is Remove'd, OR a yel-frag
    // wrapper containing it is Remove'd. With the wrapper fix, the
    // wrapper case is the expected path. Without the fix, neither
    // fires (only the inner if-anchor comment gets removed).
    let cleared = post.iter().any(|op| match op {
        DomOp::Remove { node } if *node == case_a_box_id => true,
        DomOp::Remove { node } => {
            // Check if this Remove targets a yel-frag wrapper that
            // contains the case-A Box. Since DomState doesn't model
            // parent-child, we accept any yel-frag Remove as long as
            // SOME teardown happens for the case-A Box. The host's
            // browser implementation will cascade.
            mount_ops.iter().any(|m| {
                matches!(
                    m,
                    DomOp::CreateElement { tag, id } if tag == "yel-frag" && id == node
                )
            })
        }
        _ => false,
    });
    assert!(
        cleared,
        "Setting `outer=false` should detach the case-A Box (id {}) — \
         either via direct Remove or via Remove of a yel-frag wrapper \
         that contains it. After-setter ops emitted no such Remove.\n\
         After-setter ops: {:?}",
        case_a_box_id, post
    );
}

/// A signal interpolated into an attribute on an element inside a
/// **nested** `for` must, on update, fan out across every iteration
/// of every enclosing for. Update of `cell-size` should re-apply
/// `set-attribute` on EVERY cell (rows × cols), not just one.
///
/// Regression observed at runtime: only the very first cell of the
/// nested for got its style re-applied; the remaining cells kept
/// their stale `w:24px`.
#[test]
fn nested_for_attribute_effect_fans_out_to_every_cell() {
    let source = r#"
        package yel:nestedfan@0.1.0;
        export component App {
            rows: u32 = 2;
            cols: u32 = 2;
            cell-size: u32 = 24;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            Box { style: "w:{cell-size}px" }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:nestedfan/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let box_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        box_ids.len(),
        4,
        "expected 2×2 grid of Boxes, got {:?}",
        box_ids
    );

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:nestedfan/app-component@0.1.0",
        "app",
        "cell-size",
        &self_res,
        Val::U32(48),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    for id in &box_ids {
        let got = post.iter().any(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, name, value }
                    if node == id && name == "style" && value.contains("w:48")
            )
        });
        assert!(
            got,
            "Box id {} (cell in a 2×2 nested-for grid) did not receive \
             a `style: w:48px` update after `cell-size` was set to 48. \
             The nested-for fan-out is missing this iteration.\n\
             After-setter ops: {:?}",
            id, post
        );
    }
}

/// Checker-style case: nested for with `if` inside each cell. A signal
/// interpolated into the active branch's Box style must, on update,
/// fan out across every cell. Mirrors the actual checkerboard
/// observed at runtime where only the first cell got the new value.
#[test]
fn nested_for_with_if_branches_fans_out_attribute_update() {
    let source = r#"
        package yel:nestedifattr@0.1.0;
        export component App {
            rows: u32 = 2;
            cols: u32 = 2;
            cell-size: u32 = 24;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            if (row + col) % 2 == 0 {
                                Box { style: "w:{cell-size}px;c:white" }
                            } else {
                                Box { style: "w:{cell-size}px;c:dim" }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:nestedifattr/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let box_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        box_ids.len(),
        4,
        "expected 2×2 grid (one Box per cell, the active branch), got {:?}",
        box_ids
    );

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:nestedifattr/app-component@0.1.0",
        "app",
        "cell-size",
        &self_res,
        Val::U32(48),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    for id in &box_ids {
        let got = post.iter().any(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, name, value }
                    if node == id && name == "style" && value.contains("w:48")
            )
        });
        assert!(
            got,
            "Box id {} (a cell in a 2×2 if-inside-nested-for grid) did \
             not receive a `style: w:48px` update after `cell-size` \
             was set to 48. The fan-out is missing iterations whose \
             active branch contains the effect target.\n\
             After-setter ops: {:?}",
            id, post
        );
    }
}

// ----------------------------------------------------------------------------
// Phase 0 red tests for the per-boundary update-fn refactor.
//
// These tests describe the post-refactor invariants: per-(boundary, signal)
// update fns + per-signal subtree mask. They go green incrementally as the
// phases land. See plan at
// /Users/rolandsz.kovacs/.claude/plans/i-want-you-to-immutable-wirth.md
// ----------------------------------------------------------------------------

/// Phase 1c canary. Variant of `nested_for_with_if_branches_fans_out_attribute_update`
/// where only the then-branch's Box has the dynamic binding; the else-branch's
/// Box is fully static. Today's per-binding effect machinery pins the
/// attr-update target to `then_branch.box_handle`. When a cell's active
/// branch is `else`, the then-branch struct ref is null and the fan-out
/// walker traps at ref.as_non_null. After Phase 1c the per-(boundary, signal)
/// update fn dispatches via ActiveTag and only touches the live branch —
/// no trap, and only the active-then cells receive the SetAttribute.
#[test]
fn update_fn_per_boundary_attr_update_in_branch_active_then() {
    let source = r#"
        package yel:perboundaryattr@0.1.0;
        export component App {
            rows: u32 = 2;
            cols: u32 = 2;
            cell-size: u32 = 24;
            VStack {
                for row in 0..rows {
                    HStack {
                        for col in 0..cols {
                            if (row + col) % 2 == 0 {
                                Box { style: "w:{cell-size}px;c:white" }
                            } else {
                                Box { style: "c:dim" }
                            }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:perboundaryattr/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let box_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        box_ids.len(),
        4,
        "expected 2×2 grid of Boxes, got {:?}",
        box_ids
    );

    // Identify which boxes belong to the then-branch (`(row+col) % 2 == 0`)
    // by walking the mount op trace and tracking iter contexts. Simpler:
    // record which Box ids carried the `c:white` style at mount, since only
    // the then-branch sets that attribute via the dynamic binding.
    let then_box_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::SetAttribute { node, name, value }
                if name == "style" && value.contains("w:24") && value.contains("c:white") =>
            {
                Some(*node)
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        then_box_ids.len(),
        2,
        "expected 2 active-then cells (positions where (row+col)%2==0) in 2×2; got {:?}",
        then_box_ids,
    );
    let else_box_ids: Vec<u32> = box_ids
        .iter()
        .copied()
        .filter(|id| !then_box_ids.contains(id))
        .collect();

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:perboundaryattr/app-component@0.1.0",
        "app",
        "cell-size",
        &self_res,
        Val::U32(48),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    // Every active-then box receives a w:48 update.
    for id in &then_box_ids {
        let got = post.iter().any(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, name, value }
                    if node == id && name == "style" && value.contains("w:48")
            )
        });
        assert!(
            got,
            "active-then Box id {} did not receive style update after cell-size=48. \
             Per-(boundary, signal) update fn must dispatch via ActiveTag and update \
             every live then-branch.\nPost-setter ops: {:?}",
            id, post,
        );
    }
    // No active-else box receives any SetAttribute — the else-branch has no
    // dynamic bindings, so its IfBranch boundary should not appear in
    // signal_to_path[cell-size].
    for id in &else_box_ids {
        let stray = post.iter().find(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, .. } if node == id
            )
        });
        assert!(
            stray.is_none(),
            "active-else Box id {} (no dynamic bindings) received a stray \
             SetAttribute after cell-size=48: {:?}. Per-signal pruning must \
             skip branches whose subtree has no dependency on the signal.",
            id,
            stray,
        );
    }
}

/// Phase 1c. Two top-level for loops over the same range; each uses a
/// distinct signal in its bindings. Mutating one signal must not produce
/// any DOM ops in the other for's iters. Verifies that the per-signal
/// subtree mask actually prunes — today's effect dispatch fires every
/// effect whose dependencies include the signal regardless of subtree.
#[test]
fn signal_trigger_skips_unrelated_subtree() {
    let source = r#"
        package yel:prunedsubtree@0.1.0;
        export component App {
            n: u32 = 3;
            sig-a: u32 = 1;
            sig-b: u32 = 1;
            VStack {
                for i in 0..n {
                    Box { style: "a:{sig-a}" }
                }
                for j in 0..n {
                    Box { style: "b:{sig-b}" }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:prunedsubtree/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    // The first for emits 3 boxes (a-boxes), the second emits 3 (b-boxes),
    // in source order. Positional split is more robust than reading style
    // values, since attribute application timing is implementation detail.
    let all_boxes: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        all_boxes.len(),
        6,
        "expected 6 boxes (2×3), got {:?}",
        all_boxes
    );
    let a_boxes: Vec<u32> = all_boxes[0..3].to_vec();
    let b_boxes: Vec<u32> = all_boxes[3..6].to_vec();

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:prunedsubtree/app-component@0.1.0",
        "app",
        "sig-a",
        &self_res,
        Val::U32(99),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    // Every a-box receives an update.
    for id in &a_boxes {
        assert!(
            post.iter().any(|op| matches!(
                op,
                DomOp::SetAttribute { node, name, .. } if node == id && name == "style"
            )),
            "a-box {} missed a-signal update; ops={:?}",
            id,
            post,
        );
    }
    // No b-box should be touched — sig-a is not in its subtree's deps.
    for id in &b_boxes {
        let stray = post.iter().find(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, .. } if node == id
            )
        });
        assert!(
            stray.is_none(),
            "b-box {} received a stray SetAttribute after sig-a setter: {:?}. \
             Per-signal subtree mask must prune the unrelated for subtree.",
            id,
            stray,
        );
    }
}

/// Phase 1c. A nested for whose body has no dynamic bindings at all
/// (purely static content). Mutating any signal must not enter the for's
/// iter-body update path — its `subtree_deps` is empty so no update fn
/// is emitted for that boundary. Today's machinery wraps every per-binding
/// effect's update_block in a fan-out walker that re-walks every for
/// regardless of whether the for's subtree has a dependency.
#[test]
fn signal_trigger_skips_empty_for_subtree() {
    let source = r#"
        package yel:emptyforsubtree@0.1.0;
        export component App {
            n: u32 = 4;
            tick: u32 = 0;
            label: string = "hi";
            VStack {
                Text { value: label }
                for i in 0..n {
                    Box { style: "static" }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:emptyforsubtree/app-component@0.1.0", "app");

    let mount_ops = dom.lock().unwrap().ops.clone();
    let static_box_ids: Vec<u32> = mount_ops
        .iter()
        .filter_map(|op| match op {
            DomOp::CreateElement { tag, id } if tag == "Box" => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        static_box_ids.len(),
        4,
        "expected 4 static boxes, got {:?}",
        static_box_ids
    );

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:emptyforsubtree/app-component@0.1.0",
        "app",
        "tick",
        &self_res,
        Val::U32(1),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();

    // The `tick` signal has no dependents at all. Post-setter ops should
    // be empty (or contain nothing touching the static boxes).
    for id in &static_box_ids {
        let stray = post.iter().find(|op| {
            matches!(
                op,
                DomOp::SetAttribute { node, .. } | DomOp::SetTextContent { node, .. }
                    if node == id
            )
        });
        assert!(
            stray.is_none(),
            "static box {} touched after unrelated `tick` mutation: {:?}. \
             Empty-subtree boundary should produce zero update fns and the \
             signal trigger should be a no-op.",
            id,
            stray,
        );
    }
    // Even the label Text node should not be touched — `tick` is not a
    // dependency of `label`.
    let label_text_touched = post
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { .. }));
    assert!(
        !label_text_touched,
        "label Text was updated after unrelated `tick` setter; ops={:?}",
        post,
    );
}

/// Phase 3a. Repeated nested-for templates should produce one
/// `update_b<inner>_s<signal>` fn shared across iter instances, not one
/// per instance. Verified via WAT inspection of the emitted core module's
/// name section.
///
/// Currently ignored — depends on the WAT-inspection helper landing
/// alongside Phase 3a.
#[test]
fn update_fn_dedupes_repeated_iter_shape() {
    let source = r#"
        package yel:dedupetempl@0.1.0;
        export component App {
            n: u32 = 3;
            v: u32 = 1;
            VStack {
                for i in 0..n {
                    HStack {
                        for j in 0..n {
                            Box { style: "v:{v}" }
                        }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let count = count_named_fns_with_prefix(&bytes, "update_b");
    // Boundaries on path-to-dependent for signal `v`: Root, ForI_anchor,
    // IterI_body, ForJ_anchor, IterJ_body. After dedupe each unique
    // boundary shape collapses to one fn — expect ≤ 5 update fns total
    // for the single signal `v`.
    assert!(
        count <= 5,
        "expected ≤ 5 update fns after Phase 3a dedupe, got {}. Bitwise \
         structural dedupe should collapse repeated iter-body shapes.",
        count,
    );
}

/// Phase 3b. With funcref-parameterized walkers, the structural traversal
/// (children-array loop, ActiveTag dispatch) lives in shared library fns
/// — `walk_for_children` and `walk_if_active`. Multiple distinct fors and
/// ifs in a fixture must not produce multiple copies of these walkers.
///
/// Currently ignored — depends on the WAT-inspection helper and Phase 3b
/// landing.
#[test]
#[ignore = "needs Phase 3b funcref shape walkers + WAT-inspection helper"]
fn walker_library_shared_across_for_anchors() {
    let source = r#"
        package yel:walkerlib@0.1.0;
        export component App {
            n: u32 = 2;
            v: u32 = 1;
            VStack {
                for i in 0..n {
                    Box { style: "i:{v}" }
                }
                for j in 0..n {
                    Box { style: "j:{v}" }
                }
                for k in 0..n {
                    if k % 2 == 0 {
                        Box { style: "k:{v}" }
                    }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let walk_for = count_named_fns_with_prefix(&bytes, "walk_for_children");
    let walk_if = count_named_fns_with_prefix(&bytes, "walk_if_active");
    assert!(
        walk_for <= 1,
        "expected at most one walk_for_children library fn (Route A); got {}",
        walk_for,
    );
    assert!(
        walk_if <= 1,
        "expected at most one walk_if_active library fn (Route A); got {}",
        walk_if,
    );
}

/// Helper: count internal/exported fns in the emitted component whose
/// name section entry starts with `prefix`. Walks every core module
/// embedded in the component and inspects its name section.
fn count_named_fns_with_prefix(bytes: &[u8], prefix: &str) -> usize {
    use wasmparser::{KnownCustom, Name, Parser, Payload};

    let mut count = 0usize;
    for payload in Parser::new(0).parse_all(bytes) {
        let payload = match payload {
            Ok(p) => p,
            Err(_) => continue,
        };
        let Payload::CustomSection(reader) = payload else {
            continue;
        };
        let KnownCustom::Name(name_reader) = reader.as_known() else {
            continue;
        };
        for name in name_reader {
            let Ok(Name::Function(fn_names)) = name else {
                continue;
            };
            for naming in fn_names {
                let Ok(naming) = naming else { continue };
                if naming.name.starts_with(prefix) {
                    count += 1;
                }
            }
        }
    }
    count
}

// ----------------------------------------------------------------------------
// GC migration Phase 0 — behavioral pinning tests for records / tuples.
//
// These tests pin current (memory-backed) behavior so that the migration to
// GC structs / arrays in Phases 1-7 can be verified non-regressively. They
// must pass against today's implementation AND continue to pass after each
// migration phase. The lone WAT-inspection test stays `#[ignore]` until
// Phase 1 emits per-record GC types.
//
// Plan: /Users/rolandsz.kovacs/.claude/plans/migrate-records-tuples-to-gc.md
// ----------------------------------------------------------------------------

/// Phase 0 regression-guard: a record signal whose field is read from
/// interpolated text. Setter mutates a primitive that drives a derived
/// `RecordConstruct`, and the displayed text reflects the new field. Pins
/// the behavior of SignalRead → field-access → text under the migration.
///
/// Uses internal-mutation (a primitive-typed setter triggering a derived
/// record signal) rather than passing a `Val::Record` directly — avoids
/// any harness coupling to wasmtime's record-Val ABI shape, which is
/// orthogonal to what we want to pin (in-component field reads).
#[test]
fn record_signal_field_read_after_setter() {
    let source = r#"
        package yel:gcrec0a@0.1.0;
        record Point { x: s32, y: s32, }
        export component App {
            xv: s32 = 1;
            origin: Point = { x: xv, y: 20 };
            VStack {
                Text { "x={origin.x}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:gcrec0a/app-component@0.1.0", "app");

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:gcrec0a/app-component@0.1.0",
        "app",
        "xv",
        &self_res,
        Val::S32(77),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();
    let saw = post
        .iter()
        .any(|op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("x=77")));
    assert!(
        saw,
        "after setting `xv=77`, derived record `origin` field `x` should \
         flow into Text. Setter ops: {:?}",
        post
    );
}

/// Phase 3 SLR migration gate: a single-level record signal whose
/// fields include a `string` AND a primitive. Pins the
/// SignalRead → Field → Text path under the new `(ref null
/// $user_record)` storage with `(ref null $fat_value)` boxing for
/// the string field. Setter mutates a primitive that drives a
/// derived SLR-record signal; assert the post-mutation Text
/// includes both the (unchanged) string field and the new primitive
/// field — proving (a) the string-field box read survives, (b) the
/// primitive-field read reflects the new value, (c) `struct.new
/// $fat_value` writes are reachable from RecordConstruct.
#[test]
fn slr_record_signal_string_field_read_after_setter() {
    let source = r#"
        package yel:gcrec3a@0.1.0;
        record User { name: string, age: u32, }
        export component App {
            agev: u32 = 30;
            user: User = { name: "alice", age: agev };
            VStack {
                Text { "{user.name}={user.age}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    let self_res = ctor_and_mount(&mut h, "yel:gcrec3a/app-component@0.1.0", "app");

    let pre = dom.lock().unwrap().ops.len();
    call_setter(
        &mut h,
        "yel:gcrec3a/app-component@0.1.0",
        "app",
        "agev",
        &self_res,
        Val::U32(77),
    );
    let post = dom.lock().unwrap().ops[pre..].to_vec();
    let saw = post.iter().any(
        |op| matches!(op, DomOp::SetTextContent { content, .. } if content.contains("alice=77")),
    );
    assert!(
        saw,
        "after setting `agev=77`, derived SLR record `user` field `name` \
         (string, fat_value-boxed) should still read `alice` and field \
         `age` (u32) should reflect 77 — the Text node should display \
         `alice=77`. Setter ops: {:?}",
        post
    );
}

/// Phase 0 regression-guard: a record signal whose two fields are
/// independently observable on mount. Pins the in-component round-trip
/// of record storage (today: cabi_realloc + memory; post-GC: struct.new
/// + struct.get) independent of any host-side `Val::Record` ABI quirk.
///
/// Simplified vs the plan's original "WIT setter call" framing: the
/// record-typed setter has a known canonical-ABI bug today (see
/// `tests/fixtures/known_bugs/README.md` and the `record_literal_signal`
/// comment). Driving the record through a derived signal is also flaky
/// today (the derived doesn't pin both fields cleanly under setter
/// invalidation). So the pinned behavior here is the literal-default
/// path: a record built once at init, with both fields read at mount —
/// the cleanest cross-phase invariant.
#[test]
fn record_signal_passes_through_canonical_abi() {
    let source = r#"
        package yel:gcrec0b@0.1.0;
        record Point { x: s32, y: s32, }
        export component App {
            origin: Point = { x: 7, y: 99 };
            VStack {
                Text { "{origin.x}" }
                Text { "{origin.y}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:gcrec0b/app-component@0.1.0", "app");

    let ops = dom.lock().unwrap().ops.clone();
    // Two separate Text nodes — one per field — sidesteps any quirks
    // of multi-field interpolation in a single string. Each must
    // render its respective field value, proving field storage
    // round-trips independently.
    let saw_x = ops.iter().any(|op| {
        matches!(op,
        DomOp::CreateText { content, .. } | DomOp::SetTextContent { content, .. }
            if content == "7")
    });
    let saw_y = ops.iter().any(|op| {
        matches!(op,
        DomOp::CreateText { content, .. } | DomOp::SetTextContent { content, .. }
            if content == "99")
    });
    assert!(
        saw_x && saw_y,
        "literal-default record `origin = {{ x: 7, y: 99 }}` must render \
         `7` and `99` in separate Text nodes — both fields round-trip \
         independently through record storage. Mount ops: {:?}",
        ops
    );
}

/// Phase 0 regression-guard: iterating a record list and reading a
/// per-iter record field. Pins inline-byte list + offset-load today,
/// `array<ref>` + `struct.get` post-Phase 5.
#[test]
fn list_of_records_iter_field_access() {
    let source = r#"
        package yel:gcrec0c@0.1.0;
        record Person { name: string, age: s32, }
        export component App {
            people: list<Person> = [
                { name: "alice", age: 30 },
                { name: "bob",   age: 31 },
                { name: "carol", age: 32 },
            ];
            VStack {
                for p in people {
                    Text { "{p.name}" }
                }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:gcrec0c/app-component@0.1.0", "app");

    let ops = dom.lock().unwrap().ops.clone();
    for expected in &["alice", "bob", "carol"] {
        let saw = ops.iter().any(|op| match op {
            DomOp::CreateText { content, .. } => content.contains(*expected),
            DomOp::SetTextContent { content, .. } => content.contains(*expected),
            _ => false,
        });
        assert!(
            saw,
            "expected the for-iter over `people` to render a text node \
             containing `{}`. Mount ops: {:?}",
            expected, ops
        );
    }
}

/// Phase 0 regression-guard: tuple signal with a literal default. Pins
/// tuple ctor + tuple init under the migration.
///
/// Simplified vs the plan's original ask: the parser today doesn't accept
/// numeric tuple-field access in string interpolation (`{cursor.0}`),
/// so this test asserts the component compiles, instantiates, and
/// mounts a child VStack/Text — i.e. the tuple default initialization
/// doesn't trap or fail validation. Once `{cursor.0}` parsing is added,
/// upgrade the assertion to read both elements.
#[test]
fn tuple_signal_default_init() {
    let source = r#"
        package yel:gcrec0d@0.1.0;
        export component App {
            cursor: tuple<s32, s32> = (10, 20);
            VStack {
                Text { "ok" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:gcrec0d/app-component@0.1.0", "app");

    let ops = dom.lock().unwrap().ops.clone();
    let saw_vstack = ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateElement { tag, .. } if tag == "VStack"));
    let saw_text = ops
        .iter()
        .any(|op| matches!(op, DomOp::CreateText { content, .. } if content == "ok"));
    assert!(
        saw_vstack && saw_text,
        "tuple-default-init `cursor = (10, 20)` should mount the child \
         VStack + Text without trapping. Mount ops: {:?}",
        ops
    );
}

/// Boundary round-trip: a `tuple<s32, string>` signal set through the
/// exported setter and read back through the exported getter must return
/// the same value. The tuple is stored internally as a GC struct whose
/// string element is a `$str_bytes` ref, so the setter has to un-materialize
/// the canonical `(ptr, len)` into that ref and the getter has to materialize
/// it back — exercising both directions of the tuple-element canonical-ABI
/// flattening (regression for the "type mismatch: expected i32, found (ref …)"
/// encode failure that made every tuple-with-a-string signal unbuildable).
#[test]
fn tuple_with_string_element_setter_getter_roundtrip() {
    let source = r#"
        package yel:tupstr@0.1.0;
        export component App {
            pair: tuple<s32, string> = (1, "init");
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:tupstr/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");

    call_setter(
        &mut h,
        iface,
        "app",
        "pair",
        &r,
        Val::Tuple(vec![Val::S32(42), Val::String("hello".into())]),
    );

    let get_pair = get_func(&mut h, iface, "[method]app.get-pair");
    let mut out = [Val::Bool(false)];
    get_pair
        .call(&mut h.store, &[Val::Resource(r)], &mut out)
        .expect("get-pair");
    match &out[0] {
        Val::Tuple(elems) => {
            assert_eq!(elems.len(), 2, "tuple arity, got {:?}", elems);
            assert!(
                matches!(&elems[0], Val::S32(42)),
                "tuple.0 must be 42, got {:?}",
                elems[0]
            );
            match &elems[1] {
                Val::String(s) => assert_eq!(&**s, "hello", "tuple.1 must round-trip"),
                other => panic!("tuple.1 must be a string, got {:?}", other),
            }
        }
        other => panic!("get-pair returned non-tuple {:?}", other),
    }
}

/// Boundary round-trip for a tuple whose element is a *record* (with a
/// string field). The tuple stores the record as a nested GC struct ref, so
/// the setter recursively packs the record from its canonical params and the
/// getter recursively lowers it back — reusing the same recursive record
/// machinery that record signals use. Verifies both directions round-trip.
#[test]
fn tuple_with_record_element_setter_getter_roundtrip() {
    let source = r#"
        package yel:tuprec@0.1.0;
        record Item { n: s32, label: string, }
        export component App {
            entry: tuple<s32, Item> = (0, { n: 1, label: "init" });
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:tuprec/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");

    call_setter(
        &mut h,
        iface,
        "app",
        "entry",
        &r,
        Val::Tuple(vec![
            Val::S32(9),
            Val::Record(vec![
                ("n".into(), Val::S32(77)),
                ("label".into(), Val::String("deep".into())),
            ]),
        ]),
    );

    let get_entry = get_func(&mut h, iface, "[method]app.get-entry");
    let mut out = [Val::Bool(false)];
    get_entry
        .call(&mut h.store, &[Val::Resource(r)], &mut out)
        .expect("get-entry");
    match &out[0] {
        Val::Tuple(elems) => {
            assert!(matches!(&elems[0], Val::S32(9)), "tuple.0, got {:?}", elems[0]);
            match &elems[1] {
                Val::Record(fields) => {
                    let get = |k: &str| fields.iter().find(|(n, _)| n == k).map(|(_, v)| v);
                    assert!(
                        matches!(get("n"), Some(Val::S32(77))),
                        "record.n must be 77, got {:?}",
                        get("n")
                    );
                    match get("label") {
                        Some(Val::String(s)) => assert_eq!(&**s, "deep"),
                        other => panic!("record.label must round-trip, got {:?}", other),
                    }
                }
                other => panic!("tuple.1 must be a record, got {:?}", other),
            }
        }
        other => panic!("get-entry returned non-tuple {:?}", other),
    }
}

/// Boundary round-trip for a *nested* tuple (`tuple<s32, tuple<s32,
/// string>>`). Both the setter (`emit_setter_pack_tuple`) and getter
/// (`emit_getter_lift_tuple`) recurse into the inner tuple, so this pins the
/// recursive tuple pack/lift end-to-end.
#[test]
fn nested_tuple_setter_getter_roundtrip() {
    let source = r#"
        package yel:nesttup@0.1.0;
        export component App {
            nested: tuple<s32, tuple<s32, string>> = (0, (1, "init"));
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:nesttup/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");

    call_setter(
        &mut h,
        iface,
        "app",
        "nested",
        &r,
        Val::Tuple(vec![
            Val::S32(5),
            Val::Tuple(vec![Val::S32(6), Val::String("inner".into())]),
        ]),
    );

    let get_nested = get_func(&mut h, iface, "[method]app.get-nested");
    let mut out = [Val::Bool(false)];
    get_nested
        .call(&mut h.store, &[Val::Resource(r)], &mut out)
        .expect("get-nested");
    match &out[0] {
        Val::Tuple(outer) => {
            assert!(matches!(&outer[0], Val::S32(5)), "outer.0, got {:?}", outer[0]);
            match &outer[1] {
                Val::Tuple(inner) => {
                    assert!(
                        matches!(&inner[0], Val::S32(6)),
                        "inner.0 must be 6, got {:?}",
                        inner[0]
                    );
                    match &inner[1] {
                        Val::String(s) => assert_eq!(&**s, "inner"),
                        other => panic!("inner.1 must round-trip, got {:?}", other),
                    }
                }
                other => panic!("outer.1 must be a tuple, got {:?}", other),
            }
        }
        other => panic!("get-nested returned non-tuple {:?}", other),
    }
}

/// Boundary round-trip for a mixed-width `result<s32, s64>`. Its cases have
/// different payload widths, so the canonical ABI `join`s the payload slot up
/// to i64; the `Ok(s32)` case must be narrowed (i32.wrap_i64) on the setter
/// and widened on the getter. Verifies both the low-32 case and the full-64
/// case round-trip losslessly.
#[test]
fn result_mixed_width_join_roundtrip() {
    let source = r#"
        package yel:reswide@0.1.0;
        export component App {
            r: result<s32, s64> = ok(0);
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:reswide/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");

    let get_r = get_func(&mut h, iface, "[method]app.get-r");
    let read = |h: &mut Harness, res: &ResourceAny| -> Val {
        let mut out = [Val::Bool(false)];
        get_r
            .call(&mut h.store, &[Val::Resource(*res)], &mut out)
            .expect("get-r");
        std::mem::replace(&mut out[0], Val::Bool(false))
    };

    // Ok(42): narrow payload stored in the joined i64 slot.
    call_setter(
        &mut h,
        iface,
        "app",
        "r",
        &r,
        Val::Result(Ok(Some(Box::new(Val::S32(42))))),
    );
    match read(&mut h, &r) {
        Val::Result(Ok(Some(v))) => assert!(
            matches!(*v, Val::S32(42)),
            "Ok payload must be 42, got {:?}",
            v
        ),
        other => panic!("expected Ok(42), got {:?}", other),
    }

    // Err(big): full-width i64 payload that must not be truncated.
    let big: i64 = 9_000_000_000;
    call_setter(
        &mut h,
        iface,
        "app",
        "r",
        &r,
        Val::Result(Err(Some(Box::new(Val::S64(big))))),
    );
    match read(&mut h, &r) {
        Val::Result(Err(Some(v))) => assert!(
            matches!(*v, Val::S64(x) if x == big),
            "Err payload must be {}, got {:?}",
            big,
            v
        ),
        other => panic!("expected Err({}), got {:?}", big, other),
    }
}

/// Boundary round-trip for a collapsed `option<tuple<s32, string>>`. The
/// option collapses to a single nullable tuple ref (none = null); the setter
/// builds the tuple from canonical params on Some and the getter lowers it
/// (via the recursive tuple lift). Verifies Some and None both round-trip.
#[test]
fn record_with_mixed_width_result_field_roundtrip() {
    // A record field that is a mixed-width result (`result<s32, s64>`): the
    // record pack must narrow the joined i64 param to the Ok case's i32 field
    // (i32.wrap_i64), and the record lift must widen it back. Verifies both
    // the Ok (narrow) and Err (full 64) cases round-trip through the field.
    let source = r#"
        package yel:recres@0.1.0;
        record R { v: result<s32, s64>, }
        export component App {
            r: R = { v: ok(0) };
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:recres/app-component@0.1.0";
    let res = ctor_and_mount(&mut h, iface, "app");
    let get_r = get_func(&mut h, iface, "[method]app.get-r");
    let read_v = |h: &mut Harness, r: &ResourceAny| -> Val {
        let mut out = [Val::Bool(false)];
        get_r
            .call(&mut h.store, &[Val::Resource(*r)], &mut out)
            .expect("get-r");
        match std::mem::replace(&mut out[0], Val::Bool(false)) {
            Val::Record(mut f) => f
                .drain(..)
                .find(|(n, _)| n == "v")
                .map(|(_, v)| v)
                .expect("field v"),
            other => panic!("get-r non-record {:?}", other),
        }
    };
    // Ok(42) — narrow payload in the joined i64 slot.
    call_setter(
        &mut h,
        iface,
        "app",
        "r",
        &res,
        Val::Record(vec![(
            "v".into(),
            Val::Result(Ok(Some(Box::new(Val::S32(42))))),
        )]),
    );
    match read_v(&mut h, &res) {
        Val::Result(Ok(Some(v))) => {
            assert!(matches!(*v, Val::S32(42)), "Ok must be 42, got {:?}", v)
        }
        other => panic!("expected Ok(42), got {:?}", other),
    }
    // Err(big) — full-width i64.
    let big: i64 = 9_000_000_000;
    call_setter(
        &mut h,
        iface,
        "app",
        "r",
        &res,
        Val::Record(vec![(
            "v".into(),
            Val::Result(Err(Some(Box::new(Val::S64(big))))),
        )]),
    );
    match read_v(&mut h, &res) {
        Val::Result(Err(Some(v))) => {
            assert!(matches!(*v, Val::S64(x) if x == big), "Err must be {}, got {:?}", big, v)
        }
        other => panic!("expected Err({}), got {:?}", big, other),
    }
}

#[test]
fn record_with_tuple_field_roundtrip() {
    // A record field that is a tuple: the record lift/pack must delegate the
    // field to the recursive tuple lift/pack (it's a nested GC struct), not
    // treat it as a scalar slot. Verifies a record { s32, tuple<s32, string> }
    // round-trips through set→get, string included.
    let source = r#"
        package yel:rectup@0.1.0;
        record R { n: s32, pair: tuple<s32, string>, }
        export component App {
            r: R = { n: 0, pair: (1, "init") };
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:rectup/app-component@0.1.0";
    let res = ctor_and_mount(&mut h, iface, "app");
    call_setter(
        &mut h,
        iface,
        "app",
        "r",
        &res,
        Val::Record(vec![
            ("n".into(), Val::S32(9)),
            (
                "pair".into(),
                Val::Tuple(vec![Val::S32(8), Val::String("hi".into())]),
            ),
        ]),
    );
    let get_r = get_func(&mut h, iface, "[method]app.get-r");
    let mut out = [Val::Bool(false)];
    get_r
        .call(&mut h.store, &[Val::Resource(res)], &mut out)
        .expect("get-r");
    match &out[0] {
        Val::Record(f) => {
            let g = |k: &str| f.iter().find(|(n, _)| n == k).map(|(_, v)| v);
            assert!(matches!(g("n"), Some(Val::S32(9))), "n, got {:?}", g("n"));
            match g("pair") {
                Some(Val::Tuple(e)) => {
                    assert!(matches!(&e[0], Val::S32(8)), "pair.0, got {:?}", e[0]);
                    match &e[1] {
                        Val::String(s) => assert_eq!(&**s, "hi"),
                        other => panic!("pair.1: {:?}", other),
                    }
                }
                other => panic!("pair must be a tuple, got {:?}", other),
            }
        }
        other => panic!("get-r returned non-record {:?}", other),
    }
}

#[test]
fn single_slot_nested_record_roundtrip() {
    // A record that flattens to a single 64-bit slot (a single-field record
    // wrapping a single-s64-field record) is returned by-value as i64, not by
    // pointer. The getter must read down the chain `o.i.a` to the leaf scalar;
    // previously it stopped at `o.i` and returned a `(ref $I)` where i64 was
    // expected, failing validation.
    let source = r#"
        package yel:sslot@0.1.0;
        record I { a: s64, }
        record O { i: I, }
        export component App {
            o: O = { i: { a: 1 } };
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:sslot/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");
    let big: i64 = 7_000_000_000;
    call_setter(
        &mut h,
        iface,
        "app",
        "o",
        &r,
        Val::Record(vec![(
            "i".into(),
            Val::Record(vec![("a".into(), Val::S64(big))]),
        )]),
    );
    let get_o = get_func(&mut h, iface, "[method]app.get-o");
    let mut out = [Val::Bool(false)];
    get_o
        .call(&mut h.store, &[Val::Resource(r)], &mut out)
        .expect("get-o");
    match &out[0] {
        Val::Record(f) => match f.iter().find(|(n, _)| n == "i").map(|(_, v)| v) {
            Some(Val::Record(inner)) => {
                match inner.iter().find(|(n, _)| n == "a").map(|(_, v)| v) {
                    Some(Val::S64(x)) => assert_eq!(*x, big, "o.i.a must round-trip"),
                    other => panic!("o.i.a: {:?}", other),
                }
            }
            other => panic!("o.i must be a record, got {:?}", other),
        },
        other => panic!("get-o returned non-record {:?}", other),
    }
}

#[test]
fn option_record_collapse_roundtrip() {
    // Guards the pre-existing collapse-path discriminant bug: the collapsed
    // option getter/setter used `ref.is_null` directly (some=0), inverting the
    // canonical-ABI convention (some=1), so every host `some(record)` was
    // stored as none and read back as none/zeros. Now fixed.
    let source = r#"
        package yel:optrec@0.1.0;
        record Item { n: s32, label: string, }
        export component App {
            maybe: option<Item> = none;
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:optrec/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");
    let get_m = get_func(&mut h, iface, "[method]app.get-maybe");
    let read = |h: &mut Harness, res: &ResourceAny| -> Val {
        let mut out = [Val::Bool(false)];
        get_m
            .call(&mut h.store, &[Val::Resource(*res)], &mut out)
            .expect("get-maybe");
        std::mem::replace(&mut out[0], Val::Bool(false))
    };
    call_setter(
        &mut h,
        iface,
        "app",
        "maybe",
        &r,
        Val::Option(Some(Box::new(Val::Record(vec![
            ("n".into(), Val::S32(88)),
            ("label".into(), Val::String("hi".into())),
        ])))),
    );
    match read(&mut h, &r) {
        Val::Option(Some(v)) => match *v {
            Val::Record(ref f) => {
                let g = |k: &str| f.iter().find(|(n, _)| n == k).map(|(_, v)| v);
                assert!(matches!(g("n"), Some(Val::S32(88))), "n, got {:?}", g("n"));
                match g("label") {
                    Some(Val::String(s)) => assert_eq!(&**s, "hi"),
                    other => panic!("label: {:?}", other),
                }
            }
            other => panic!("Some payload must be record, got {:?}", other),
        },
        other => panic!("expected Some(record), got {:?}", other),
    }
    call_setter(&mut h, iface, "app", "maybe", &r, Val::Option(None));
    match read(&mut h, &r) {
        Val::Option(None) => {}
        other => panic!("expected None, got {:?}", other),
    }
}

#[test]
fn option_tuple_collapse_roundtrip() {
    let source = r#"
        package yel:opttup@0.1.0;
        export component App {
            maybe: option<tuple<s32, string>> = none;
            VStack { Text { "ok" } }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let iface = "yel:opttup/app-component@0.1.0";
    let r = ctor_and_mount(&mut h, iface, "app");

    let get_m = get_func(&mut h, iface, "[method]app.get-maybe");
    let read = |h: &mut Harness, res: &ResourceAny| -> Val {
        let mut out = [Val::Bool(false)];
        get_m
            .call(&mut h.store, &[Val::Resource(*res)], &mut out)
            .expect("get-maybe");
        std::mem::replace(&mut out[0], Val::Bool(false))
    };

    // Some((7, "hi"))
    call_setter(
        &mut h,
        iface,
        "app",
        "maybe",
        &r,
        Val::Option(Some(Box::new(Val::Tuple(vec![
            Val::S32(7),
            Val::String("hi".into()),
        ])))),
    );
    match read(&mut h, &r) {
        Val::Option(Some(v)) => match *v {
            Val::Tuple(ref e) => {
                assert!(matches!(&e[0], Val::S32(7)), "tuple.0, got {:?}", e[0]);
                match &e[1] {
                    Val::String(s) => assert_eq!(&**s, "hi"),
                    other => panic!("tuple.1 must round-trip, got {:?}", other),
                }
            }
            other => panic!("Some payload must be a tuple, got {:?}", other),
        },
        other => panic!("expected Some, got {:?}", other),
    }

    // None
    call_setter(
        &mut h,
        iface,
        "app",
        "maybe",
        &r,
        Val::Option(None),
    );
    match read(&mut h, &r) {
        Val::Option(None) => {}
        other => panic!("expected None, got {:?}", other),
    }
}

/// Phase 0 regression-guard: nested record field access through two
/// levels: `state.user.addr.city`. Pins multi-level offset loads today
/// and chained `struct.get` post-Phase 4.
///
/// Currently `#[ignore]`d: a record-with-record-field signal fails
/// component validation today (see `tests/fixtures/known_bugs/README.md`
/// → `nested_records.yel`: "Setter for a signal of a record-containing-
/// record type fails the same wit-component classification"). When
/// Phase 4 lands and nested-record codegen is fixed, un-ignore.
#[test]
fn record_in_record_field_access() {
    let source = r#"
        package yel:gcrec0e@0.1.0;
        record Address { city: string, zip: string, }
        record User    { name: string, addr: Address, }
        export component App {
            user: User = { name: "alice", addr: { city: "paris", zip: "75001" } };
            VStack {
                Text { "{user.addr.city}" }
            }
        }
    "#;
    let bytes = compile_to_component(source);
    let (mut h, dom) = instantiate(&bytes, &[]);
    ctor_and_mount(&mut h, "yel:gcrec0e/app-component@0.1.0", "app");

    let ops = dom.lock().unwrap().ops.clone();
    let saw = ops.iter().any(|op| match op {
        DomOp::CreateText { content, .. } => content.contains("paris"),
        DomOp::SetTextContent { content, .. } => content.contains("paris"),
        _ => false,
    });
    assert!(
        saw,
        "nested record access `user.addr.city` should render `paris`. \
         Mount ops: {:?}",
        ops
    );
}

/// Phase 1 red test (currently `#[ignore]`): assert a per-record GC
/// `(struct ...)` type is emitted with the expected name. Goes green
/// when Phase 1 of the migration plan lands. See
/// `count_struct_types_with_name_prefix` for the type-section walker.
#[test]
fn gc_record_type_emitted() {
    let source = r#"
        package yel:gcrec0f@0.1.0;
        record Point { x: s32, y: s32, }
        export component App {
            origin: Point = { x: 1, y: 2 };
            Text { "x={origin.x}" }
        }
    "#;
    let bytes = compile_to_component(source);
    // Expected naming convention from the migration plan: lowercased
    // record name + `_record` suffix → `$point_record`.
    let n = count_struct_types_with_name_prefix(&bytes, "point_record");
    assert!(
        n >= 1,
        "expected at least one named GC struct type whose name starts \
         with `point_record` (i.e. `$point_record`); found {}.",
        n,
    );
}

/// Helper: count named GC struct types in the emitted component whose
/// name-section type entry starts with `prefix`. Walks every core module
/// in the component, collects the indices of every `Struct` composite
/// type, then walks the custom `name` section's *type names* subsection
/// and matches names against the prefix — only counting entries whose
/// referenced type is a Struct (so array / function names that share
/// a prefix don't get counted).
///
/// Mirrors `count_named_fns_with_prefix` but for GC type-section
/// entries. Used by the Phase 1 migration test to assert per-record
/// `(struct ...)` types are emitted with the convention
/// `$<lowercased_record_name>_record`.
fn count_struct_types_with_name_prefix(bytes: &[u8], prefix: &str) -> usize {
    use wasmparser::{CompositeInnerType, KnownCustom, Name, Parser, Payload};

    // Pass 1 — collect the indices of every Struct type in every core
    // module. Indices are module-local; we key (module_idx, type_idx).
    let mut struct_indices: std::collections::HashSet<(usize, u32)> =
        std::collections::HashSet::new();
    let mut module_idx: usize = 0;
    let mut type_idx_in_module: u32 = 0;
    for payload in Parser::new(0).parse_all(bytes) {
        let Ok(payload) = payload else { continue };
        match payload {
            Payload::ModuleSection { .. } => {
                type_idx_in_module = 0;
            }
            Payload::TypeSection(reader) => {
                for rec in reader {
                    let Ok(rec) = rec else { continue };
                    for sub in rec.types() {
                        if matches!(sub.composite_type.inner, CompositeInnerType::Struct(_)) {
                            struct_indices.insert((module_idx, type_idx_in_module));
                        }
                        type_idx_in_module += 1;
                    }
                }
            }
            Payload::End(_) => {
                module_idx += 1;
            }
            _ => {}
        }
    }

    // Pass 2 — walk the name section's Type-names subsection and count
    // entries whose name starts with `prefix` AND whose type index is
    // a Struct.
    let mut count = 0usize;
    let mut module_idx: usize = 0;
    for payload in Parser::new(0).parse_all(bytes) {
        let Ok(payload) = payload else { continue };
        match payload {
            Payload::CustomSection(reader) => {
                if let KnownCustom::Name(name_reader) = reader.as_known() {
                    for name in name_reader {
                        let Ok(name) = name else { continue };
                        if let Name::Type(type_names) = name {
                            for naming in type_names {
                                let Ok(naming) = naming else { continue };
                                if naming.name.starts_with(prefix)
                                    && struct_indices.contains(&(module_idx, naming.index))
                                {
                                    count += 1;
                                }
                            }
                        }
                    }
                }
            }
            Payload::End(_) => {
                module_idx += 1;
            }
            _ => {}
        }
    }
    count
}

// ============================================================================
// Gap 1 — post-return (`cabi_post_*`) free-walk for aggregate getters
// ============================================================================

/// A GC-migrated `list` signal's getter materialises a fresh linear-memory
/// buffer per call and returns a pointer to it; the canonical ABI reclaims it
/// through an exported `cabi_post_*`. This is both a correctness AND a leak
/// test:
///
/// * **Wiring/validation** — the component is encoded with `.validate(true)`
///   and decoded+instantiated by wasmtime; a malformed post-return fails here.
/// * **Reclamation** — the bump allocator never calls `memory.grow`, so the
///   heap is capped at the initial 17 pages (~1.1 MiB). Each call materialises
///   a 256-element (~1 KiB) buffer; 4000 calls would allocate ~4 MiB. Without
///   the free this exhausts the heap and the next store traps; with the
///   `cabi_post` free-walk reclaiming each buffer the heap stays bounded and
///   every call returns the correct list. wasmtime invokes the guest's
///   post-return automatically after lifting each result.
#[test]
fn aggregate_list_getter_post_return_frees_and_stays_correct() {
    const N: u32 = 256;
    let elems = (0..N).map(|i| i.to_string()).collect::<Vec<_>>().join(", ");
    let source = format!(
        r#"
        package yel:agg@0.1.0;
        export component App {{
            nums: list<u32> = [{elems}];
            VStack {{ Text {{ "x" }} }}
        }}
    "#
    );
    let bytes = compile_to_component(&source);
    let iface = "yel:agg/app-component@0.1.0";
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let res = ctor_and_mount(&mut h, iface, "app");
    let getter = get_func(&mut h, iface, "[method]app.get-nums");
    let expected: Vec<u32> = (0..N).collect();
    // 4000 * ~1 KiB = ~4 MiB > 1.1 MiB heap: only reclamation keeps this alive.
    for i in 0..4000u32 {
        let mut out = [Val::Bool(false)];
        getter
            .call(&mut h.store, &[Val::Resource(res)], &mut out)
            .unwrap_or_else(|e| panic!("get-nums call #{i} failed (heap exhausted = leak?): {e}"));
        match &out[0] {
            Val::List(items) => {
                let got: Vec<u32> = items
                    .iter()
                    .map(|v| match v {
                        Val::U32(n) => *n,
                        other => panic!("non-u32 list elem: {other:?}"),
                    })
                    .collect();
                assert_eq!(got, expected, "wrong list on iteration #{i}");
            }
            other => panic!("get-nums returned non-list: {other:?}"),
        }
    }
}

/// Nested `list<list<u32>>` getter — exercises the RECURSIVE arm of the
/// free-walk: each call materialises one outer element buffer plus an inner
/// element buffer per outer element, and the post-return must free every inner
/// buffer (via a runtime loop) before the outer buffer and the scratch. Sized
/// so that leaking even one level would exhaust the ~1.1 MiB heap.
#[test]
fn nested_list_getter_post_return_frees_recursively() {
    const OUTER: u32 = 32;
    const INNER: u32 = 32;
    let inner_lit = (0..INNER).map(|i| i.to_string()).collect::<Vec<_>>().join(", ");
    let outer_lit = (0..OUTER)
        .map(|_| format!("[{inner_lit}]"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
        package yel:agg2@0.1.0;
        export component App {{
            grid: list<list<u32>> = [{outer_lit}];
            VStack {{ Text {{ "x" }} }}
        }}
    "#
    );
    let bytes = compile_to_component(&source);
    let iface = "yel:agg2/app-component@0.1.0";
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let res = ctor_and_mount(&mut h, iface, "app");
    let getter = get_func(&mut h, iface, "[method]app.get-grid");
    let expected_inner: Vec<u32> = (0..INNER).collect();
    // 4000 * (32*32*4) ≈ 16 MiB if leaking; heap is ~1.1 MiB.
    for i in 0..4000u32 {
        let mut out = [Val::Bool(false)];
        getter
            .call(&mut h.store, &[Val::Resource(res)], &mut out)
            .unwrap_or_else(|e| panic!("get-grid #{i} failed (heap exhausted = leak?): {e}"));
        match &out[0] {
            Val::List(rows) => {
                assert_eq!(rows.len(), OUTER as usize, "row count #{i}");
                if let Val::List(first) = &rows[0] {
                    let got: Vec<u32> = first
                        .iter()
                        .map(|v| match v {
                            Val::U32(n) => *n,
                            o => panic!("non-u32: {o:?}"),
                        })
                        .collect();
                    assert_eq!(got, expected_inner, "inner row #{i}");
                } else {
                    panic!("row 0 not a list: {:?}", rows[0]);
                }
            }
            other => panic!("get-grid returned non-list: {other:?}"),
        }
    }
}

/// `option<list<u32>>` getter — exercises the discriminant-branch arm: the
/// free-walk must read the option tag and free the inner list buffer only when
/// the value is `some`. Sized to OOM-without-free.
#[test]
fn option_list_getter_post_return_frees_payload() {
    const N: u32 = 256;
    let elems = (0..N).map(|i| i.to_string()).collect::<Vec<_>>().join(", ");
    let source = format!(
        r#"
        package yel:agg3@0.1.0;
        export component App {{
            maybe: option<list<u32>> = some([{elems}]);
            VStack {{ Text {{ "x" }} }}
        }}
    "#
    );
    let bytes = compile_to_component(&source);
    let iface = "yel:agg3/app-component@0.1.0";
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let res = ctor_and_mount(&mut h, iface, "app");
    let getter = get_func(&mut h, iface, "[method]app.get-maybe");
    let expected: Vec<u32> = (0..N).collect();
    for i in 0..4000u32 {
        let mut out = [Val::Bool(false)];
        getter
            .call(&mut h.store, &[Val::Resource(res)], &mut out)
            .unwrap_or_else(|e| panic!("get-maybe #{i} failed (heap exhausted = leak?): {e}"));
        match &out[0] {
            Val::Option(Some(inner)) => match inner.as_ref() {
                Val::List(items) => {
                    let got: Vec<u32> = items
                        .iter()
                        .map(|v| match v {
                            Val::U32(n) => *n,
                            o => panic!("non-u32: {o:?}"),
                        })
                        .collect();
                    assert_eq!(got, expected, "inner list #{i}");
                }
                o => panic!("some payload not a list: {o:?}"),
            },
            other => panic!("get-maybe returned non-some-option: {other:?}"),
        }
    }
}

/// `list<record>` getter where the record has a `string` field — exercises the
/// Record arm of the free-walk AND the critical safety boundary that aliased
/// strings are NOT freed. Each record's `label` points into the static string
/// section / persistent storage; if the free-walk wrongly freed it, that
/// address would be pushed onto the allocator free-list and corrupt a later
/// call. Stable correct values across 4000 calls prove strings are treated as
/// leaves while the element buffer + scratch are reclaimed.
#[test]
fn list_of_record_getter_frees_buffer_but_not_aliased_strings() {
    const N: u32 = 256;
    let items = (0..N)
        .map(|i| format!(r#"{{ label: "item", n: {i} }}"#))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
        package yel:agg4@0.1.0;
        record Item {{ label: string, n: u32 }}
        export component App {{
            items: list<Item> = [{items}];
            VStack {{ Text {{ "x" }} }}
        }}
    "#
    );
    let bytes = compile_to_component(&source);
    let iface = "yel:agg4/app-component@0.1.0";
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let res = ctor_and_mount(&mut h, iface, "app");
    let getter = get_func(&mut h, iface, "[method]app.get-items");
    for i in 0..4000u32 {
        let mut out = [Val::Bool(false)];
        getter
            .call(&mut h.store, &[Val::Resource(res)], &mut out)
            .unwrap_or_else(|e| panic!("get-items #{i} failed (heap exhausted = leak?): {e}"));
        match &out[0] {
            Val::List(items) => {
                assert_eq!(items.len(), N as usize, "item count #{i}");
                // Spot-check first/last records: label intact (not freed) + n correct.
                for (idx, slot) in [(0usize, 0u32), (N as usize - 1, N - 1)] {
                    if let Val::Record(fields) = &items[idx] {
                        let label = fields.iter().find(|(k, _)| k == "label").map(|(_, v)| v);
                        let n = fields.iter().find(|(k, _)| k == "n").map(|(_, v)| v);
                        assert_eq!(label, Some(&Val::String("item".into())), "label #{i}[{idx}]");
                        assert_eq!(n, Some(&Val::U32(slot)), "n #{i}[{idx}]");
                    } else {
                        panic!("element {idx} not a record: {:?}", items[idx]);
                    }
                }
            }
            other => panic!("get-items returned non-list: {other:?}"),
        }
    }
}

/// `variant` signal with a `list` payload case — exercises the multi-case
/// discriminant-branch arm of the free-walk: it must read the variant tag and
/// free the payload's list buffer only for the matching case. Sized to
/// OOM-without-free.
#[test]
fn variant_with_list_payload_getter_frees_active_case() {
    const N: u32 = 256;
    let elems = (0..N).map(|i| i.to_string()).collect::<Vec<_>>().join(", ");
    let source = format!(
        r#"
        package yel:agg5@0.1.0;
        variant Choice {{ empty, picked(list<u32>) }}
        export component App {{
            choice: Choice = picked([{elems}]);
            VStack {{ Text {{ "x" }} }}
        }}
    "#
    );
    let bytes = compile_to_component(&source);
    let iface = "yel:agg5/app-component@0.1.0";
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let res = ctor_and_mount(&mut h, iface, "app");
    let getter = get_func(&mut h, iface, "[method]app.get-choice");
    let expected: Vec<u32> = (0..N).collect();
    for i in 0..4000u32 {
        let mut out = [Val::Bool(false)];
        getter
            .call(&mut h.store, &[Val::Resource(res)], &mut out)
            .unwrap_or_else(|e| panic!("get-choice #{i} failed (heap exhausted = leak?): {e}"));
        match &out[0] {
            Val::Variant(case, Some(payload)) if case == "picked" => match payload.as_ref() {
                Val::List(items) => {
                    let got: Vec<u32> = items
                        .iter()
                        .map(|v| match v {
                            Val::U32(n) => *n,
                            o => panic!("non-u32: {o:?}"),
                        })
                        .collect();
                    assert_eq!(got, expected, "payload list #{i}");
                }
                o => panic!("picked payload not a list: {o:?}"),
            },
            other => panic!("get-choice returned unexpected: {other:?}"),
        }
    }
}

// ============================================================================
// Gap 3 — >16 flat-param pointer-spill trampoline for exported setters
// ============================================================================

/// A 16-field record signal makes the setter take `self` + 16 = 17 flat
/// params, exceeding the canonical-ABI `MAX_FLAT_PARAMS` (16). Before the fix,
/// the encoder rejected the wide `(i32 x17) -> ()` core signature (expected
/// `(i32) -> ()`). The spill trampoline now presents `(i32 ptr) -> ()`,
/// decodes the param tuple `(self, value)` from linear memory, and forwards to
/// the wide setter. This test proves (a) the component now compiles + encodes,
/// and (b) a 16-field record round-trips correctly through set→get.
#[test]
fn wide_record_setter_spills_params_and_round_trips() {
    let source = r#"
        package yel:wide@0.1.0;
        record Big { a: u32, b: u32, c: u32, d: u32, e: u32, f: u32, g: u32, h: u32,
                     i: u32, j: u32, k: u32, l: u32, m: u32, n: u32, o: u32, p: u32 }
        export component App {
            big: Big = { a:1,b:2,c:3,d:4,e:5,f:6,g:7,h:8,i:9,j:10,k:11,l:12,m:13,n:14,o:15,p:16 };
            VStack { Text { "x" } }
        }
    "#;
    let bytes = compile_to_component(source); // would panic pre-fix (encode error)
    let iface = "yel:wide/app-component@0.1.0";
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let res = ctor_and_mount(&mut h, iface, "app");

    let names = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"];
    // New values 100..115; set via the spilled setter (wasmtime lowers the
    // 17 flat params to a single pointer → our trampoline decodes it).
    let new_fields: Vec<(String, Val)> = names
        .iter()
        .enumerate()
        .map(|(idx, n)| (n.to_string(), Val::U32(100 + idx as u32)))
        .collect();
    call_setter(&mut h, iface, "app", "big", &res, Val::Record(new_fields));

    // Read it back and assert every field survived the spill round-trip.
    let getter = get_func(&mut h, iface, "[method]app.get-big");
    let mut out = [Val::Bool(false)];
    getter
        .call(&mut h.store, &[Val::Resource(res)], &mut out)
        .expect("get-big");
    match &out[0] {
        Val::Record(fields) => {
            for (idx, n) in names.iter().enumerate() {
                let got = fields.iter().find(|(k, _)| k == n).map(|(_, v)| v);
                assert_eq!(
                    got,
                    Some(&Val::U32(100 + idx as u32)),
                    "field `{n}` did not round-trip through the spilled setter"
                );
            }
        }
        other => panic!("get-big returned non-record: {other:?}"),
    }
}
