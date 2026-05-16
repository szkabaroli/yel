//! Generic Yel component host.
//!
//! Loads any compiled Yel component, wires the static `yel:ui/dom@0.1.0`
//! host surface, dynamically stubs every other import (per-component
//! callbacks etc.), discovers the exported `*-component@*` interface
//! and its resource at runtime, and exposes mount / unmount / property
//! access via subcommands.

use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU32, Ordering};
use wasmparser::HeapType as PHeapType;
use wasmparser::{
    CompositeInnerType, KnownCustom, Name, Parser, Payload, RefType, StorageType, SubType, ValType,
};
use wasmtime::component::{
    types::ComponentItem, Component, ComponentExportIndex, Func, HasSelf, Instance, Linker,
    LinkerInstance, ResourceAny, ResourceTable, ResourceType, Val,
};
use wasmtime::{AnyRef, ArrayRef, RootScope, Rooted, StructRef, Val as CoreVal};
use wasmtime::{Config, Engine, Store};
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

wasmtime::component::bindgen!({
    path: "wit",
    world: "generic-host",
});

// ============================================================================
// In-memory DOM
// ============================================================================

#[derive(Debug, Clone)]
struct DomNode {
    id: u32,
    tag: String,
    attributes: HashMap<String, String>,
    children: Vec<u32>,
    parent: Option<u32>,
}

struct HostState {
    wasi: WasiCtx,
    table: ResourceTable,
    nodes: HashMap<u32, DomNode>,
    next_node_id: AtomicU32,
    event_handlers: HashMap<(u32, String), u32>,
    /// One-line summary per host-import call when `--trace` is set.
    trace: bool,
}

impl HostState {
    fn new(trace: bool) -> Result<Self> {
        Ok(Self {
            wasi: WasiCtxBuilder::new().inherit_stdio().build(),
            table: ResourceTable::new(),
            nodes: HashMap::new(),
            next_node_id: AtomicU32::new(1),
            event_handlers: HashMap::new(),
            trace,
        })
    }

    fn alloc_node(&mut self, tag: &str) -> u32 {
        let id = self.next_node_id.fetch_add(1, Ordering::SeqCst);
        self.nodes.insert(
            id,
            DomNode {
                id,
                tag: tag.to_string(),
                attributes: HashMap::new(),
                children: Vec::new(),
                parent: None,
            },
        );
        id
    }

    fn print_tree(&self, root: u32) {
        println!("\n┌─ DOM Tree ─────────────────────────────────────");
        self.print_node(root, "", true);
        println!("└────────────────────────────────────────────────\n");
    }

    fn print_node(&self, id: u32, prefix: &str, is_last: bool) {
        let connector = if is_last { "└── " } else { "├── " };
        let child_prefix = if is_last { "    " } else { "│   " };
        if let Some(node) = self.nodes.get(&id) {
            println!("│{}{}[{}] <{}>", prefix, connector, node.id, node.tag);
            let attr_prefix = format!("{}{}    ", prefix, child_prefix);
            for (key, value) in &node.attributes {
                if key != "textContent" {
                    println!("│{}@{}: {}", attr_prefix, key, value);
                }
            }
            if node.tag == "#text" || node.tag == "#comment" {
                if let Some(content) = node.attributes.get("textContent") {
                    println!("│{}\"{}\"", attr_prefix, content);
                }
            }
            for ((node_id, event), handler_id) in &self.event_handlers {
                if *node_id == id {
                    println!("│{}on {} => handler_{}", attr_prefix, event, handler_id);
                }
            }
            for (i, &child_id) in node.children.iter().enumerate() {
                let is_last_child = i == node.children.len() - 1;
                self.print_node(
                    child_id,
                    &format!("{}{}", prefix, child_prefix),
                    is_last_child,
                );
            }
        }
    }

    fn find_roots(&self) -> Vec<u32> {
        self.nodes
            .values()
            .filter(|n| n.parent.is_none() || !self.nodes.contains_key(&n.parent.unwrap()))
            .map(|n| n.id)
            .collect()
    }
}

impl WasiView for HostState {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

fn attribute_value_to_string(value: &yel::ui::dom::AttributeValue) -> String {
    use yel::ui::dom::AttributeValue;
    match value {
        AttributeValue::Str(s) => s.clone(),
        AttributeValue::Bool(b) => {
            if *b {
                "true".into()
            } else {
                "false".into()
            }
        }
        AttributeValue::S8(v) => v.to_string(),
        AttributeValue::S16(v) => v.to_string(),
        AttributeValue::S32(v) => v.to_string(),
        AttributeValue::S64(v) => v.to_string(),
        AttributeValue::U8(v) => v.to_string(),
        AttributeValue::U16(v) => v.to_string(),
        AttributeValue::U32(v) => v.to_string(),
        AttributeValue::U64(v) => v.to_string(),
        AttributeValue::F32(v) => v.to_string(),
        AttributeValue::F64(v) => v.to_string(),
        AttributeValue::Char(c) => c.to_string(),
        AttributeValue::Color(c) => color_to_css(c),
    }
}

fn color_to_css(c: &yel::ui::dom::Color) -> String {
    use yel::ui::dom::Color;
    match c {
        Color::Red => "red".into(),
        Color::Green => "green".into(),
        Color::Blue => "blue".into(),
        Color::White => "white".into(),
        Color::Black => "black".into(),
        Color::Transparent => "transparent".into(),
        Color::Rgba((r, g, b, a)) => {
            format!("rgba({}, {}, {}, {:.3})", r, g, b, (*a as f32) / 255.0)
        }
    }
}

impl yel::ui::dom::Host for HostState {
    fn create_element(&mut self, tag: String) -> u32 {
        let id = self.alloc_node(&tag);
        if self.trace {
            println!("[DOM] create_element({:?}) -> {}", tag, id);
        }
        id
    }
    fn create_text(&mut self, content: String) -> u32 {
        let id = self.alloc_node("#text");
        if let Some(node) = self.nodes.get_mut(&id) {
            node.attributes
                .insert("textContent".into(), content.clone());
        }
        if self.trace {
            println!("[DOM] create_text({:?}) -> {}", content, id);
        }
        id
    }
    fn create_comment(&mut self, content: String) -> u32 {
        let id = self.alloc_node("#comment");
        if let Some(node) = self.nodes.get_mut(&id) {
            node.attributes
                .insert("textContent".into(), content.clone());
        }
        if self.trace {
            println!("[DOM] create_comment({:?}) -> {}", content, id);
        }
        id
    }
    fn create_fragment(&mut self) -> u32 {
        let id = self.alloc_node("yel-frag");
        if self.trace {
            println!("[DOM] create_fragment() -> {}", id);
        }
        id
    }
    fn set_attribute(&mut self, node: u32, name: String, value: yel::ui::dom::AttributeValue) {
        let s = attribute_value_to_string(&value);
        if self.trace {
            println!("[DOM] set_attribute({}, {:?}, {:?})", node, name, s);
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert(name, s);
        }
    }
    fn remove_attribute(&mut self, node: u32, name: String) {
        if self.trace {
            println!("[DOM] remove_attribute({}, {:?})", node, name);
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.remove(&name);
        }
    }
    fn set_text_content(&mut self, node: u32, content: String) {
        if self.trace {
            println!("[DOM] set_text_content({}, {:?})", node, content);
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert("textContent".into(), content);
        }
    }
    fn set_style(&mut self, node: u32, property: String, value: String) {
        if self.trace {
            println!("[DOM] set_style({}, {:?}, {:?})", node, property, value);
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert(format!("style.{}", property), value);
        }
    }
    fn set_class(&mut self, node: u32, class_name: String) {
        if self.trace {
            println!("[DOM] set_class({}, {:?})", node, class_name);
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert("class".into(), class_name);
        }
    }
    fn append_child(&mut self, parent: u32, child: u32) {
        if self.trace {
            println!("[DOM] append_child({}, {})", parent, child);
        }
        if let Some(p) = self.nodes.get_mut(&parent) {
            p.children.push(child);
        }
        if let Some(c) = self.nodes.get_mut(&child) {
            c.parent = Some(parent);
        }
    }
    fn insert_before(&mut self, parent: u32, node: u32, reference: u32) {
        if self.trace {
            println!("[DOM] insert_before({}, {}, {})", parent, node, reference);
        }
        if let Some(p) = self.nodes.get_mut(&parent) {
            if reference != 0 {
                if let Some(pos) = p.children.iter().position(|&x| x == reference) {
                    p.children.insert(pos, node);
                } else {
                    p.children.push(node);
                }
            } else {
                p.children.push(node);
            }
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.parent = Some(parent);
        }
    }
    fn insert_after(&mut self, parent: u32, node: u32, anchor: u32) {
        if self.trace {
            println!("[DOM] insert_after({}, {}, {})", parent, node, anchor);
        }
        if let Some(p) = self.nodes.get_mut(&parent) {
            if anchor != 0 {
                if let Some(pos) = p.children.iter().position(|&x| x == anchor) {
                    p.children.insert(pos + 1, node);
                } else {
                    p.children.push(node);
                }
            } else {
                p.children.insert(0, node);
            }
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.parent = Some(parent);
        }
    }
    fn remove_child(&mut self, parent: u32, child: u32) {
        if self.trace {
            println!("[DOM] remove_child({}, {})", parent, child);
        }
        if let Some(p) = self.nodes.get_mut(&parent) {
            p.children.retain(|&x| x != child);
        }
        if let Some(c) = self.nodes.get_mut(&child) {
            c.parent = None;
        }
    }
    fn remove(&mut self, node: u32) {
        if self.trace {
            println!("[DOM] remove({})", node);
        }
        if let Some(n) = self.nodes.remove(&node) {
            if let Some(parent_id) = n.parent {
                if let Some(p) = self.nodes.get_mut(&parent_id) {
                    p.children.retain(|&x| x != node);
                }
            }
        }
    }
    fn get_parent(&mut self, node: u32) -> u32 {
        let r = self.nodes.get(&node).and_then(|n| n.parent).unwrap_or(0);
        if self.trace {
            println!("[DOM] get_parent({}) -> {}", node, r);
        }
        r
    }
    fn get_next_sibling(&mut self, node: u32) -> u32 {
        let r = self
            .nodes
            .get(&node)
            .and_then(|n| {
                n.parent.and_then(|pid| {
                    self.nodes.get(&pid).and_then(|p| {
                        p.children
                            .iter()
                            .position(|&x| x == node)
                            .and_then(|pos| p.children.get(pos + 1).copied())
                    })
                })
            })
            .unwrap_or(0);
        if self.trace {
            println!("[DOM] get_next_sibling({}) -> {}", node, r);
        }
        r
    }
    fn add_event_listener(&mut self, node: u32, event: String, handler_id: u32) {
        if self.trace {
            println!(
                "[DOM] add_event_listener({}, {:?}, {})",
                node, event, handler_id
            );
        }
        self.event_handlers.insert((node, event), handler_id);
    }
    fn remove_event_listener(&mut self, node: u32, event: String, _handler_id: u32) {
        if self.trace {
            println!(
                "[DOM] remove_event_listener({}, {:?}, {})",
                node, event, _handler_id
            );
        }
        self.event_handlers.remove(&(node, event));
    }
}

// ============================================================================
// Runtime introspection — discover component shape from Component::component_type()
// ============================================================================

#[derive(Debug, Clone)]
struct ComponentDescriptor {
    /// The full export interface name, e.g. `yel:counter/counter-component@1.0.0`.
    iface_name: String,
    /// The exported resource name within that interface, e.g. `counter`.
    resource_name: String,
    /// Exported resource methods discovered on the interface — each entry is
    /// the bare method name as exported, e.g. `[constructor]counter`,
    /// `[method]counter.mount`, `[method]counter.get-count`, …
    methods: Vec<String>,
}

/// Walk the component's type and find the unique `*-component@*` export
/// interface plus its resource type. Yel codegen always emits exactly one
/// per component file.
fn describe_component(engine: &Engine, component: &Component) -> Result<ComponentDescriptor> {
    let ty = component.component_type();
    let mut found: Option<ComponentDescriptor> = None;
    for (export_name, item) in ty.exports(engine) {
        // Only component interfaces are candidates. The Yel naming
        // convention is `<ns>:<pkg>/<component-kebab>-component@<ver>`.
        let inst_ty = match item {
            ComponentItem::ComponentInstance(i) => i,
            _ => continue,
        };
        if !is_component_iface_name(&export_name) {
            continue;
        }
        // Locate the single exported resource on this interface.
        let mut resource_name: Option<String> = None;
        let mut methods: Vec<String> = Vec::new();
        for (sub_name, sub_item) in inst_ty.exports(engine) {
            match sub_item {
                ComponentItem::Resource(_) => {
                    if resource_name.is_some() {
                        return Err(anyhow!(
                            "interface {} exports multiple resources; not supported",
                            export_name
                        ));
                    }
                    resource_name = Some(sub_name.to_string());
                }
                ComponentItem::ComponentFunc(_) => {
                    methods.push(sub_name.to_string());
                }
                _ => {}
            }
        }
        let resource_name = resource_name
            .ok_or_else(|| anyhow!("interface {} has no exported resource", export_name))?;
        if found.is_some() {
            return Err(anyhow!(
                "component exports multiple `*-component@*` interfaces; \
                 not supported"
            ));
        }
        found = Some(ComponentDescriptor {
            iface_name: export_name.to_string(),
            resource_name,
            methods,
        });
    }
    found.ok_or_else(|| anyhow!("component does not export a `*-component@*` interface"))
}

fn is_component_iface_name(name: &str) -> bool {
    // `<ns>:<pkg>/<...>-component@<ver>` — allow either with-version or
    // bare. We don't anchor on `-component` because that's standard.
    if let Some(slash) = name.find('/') {
        let after = &name[slash + 1..];
        let bare = after.split('@').next().unwrap_or(after);
        bare.ends_with("-component")
    } else {
        false
    }
}

/// Walk every import in the component type and register a stub for each
/// non-DOM import. Yel components import `yel:ui/dom@0.1.0` (provided by
/// the static DOM bindings) plus zero or more `*-callbacks@*` interfaces
/// that fire when the user code calls a `func()` callback. The stubs are
/// untyped wasmtime funcs — they print on call when `--trace` is set and
/// otherwise drop the args.
fn register_dynamic_imports(
    engine: &Engine,
    component: &Component,
    linker: &mut Linker<HostState>,
) -> Result<()> {
    let ty = component.component_type();
    for (name, item) in ty.imports(engine) {
        if name == "yel:ui/dom@0.1.0" {
            continue; // handled by Host::add_to_linker
        }
        if name.starts_with("wasi:") {
            continue; // handled by wasmtime_wasi
        }
        let inst_ty = match item {
            ComponentItem::ComponentInstance(i) => i,
            _ => continue,
        };
        let iface_name = name.to_string();
        let mut iface = match linker.instance(&iface_name) {
            Ok(i) => i,
            Err(e) => {
                eprintln!(
                    "[host] note: could not register linker instance {}: {}",
                    iface_name, e
                );
                continue;
            }
        };
        register_iface_stubs(engine, &iface_name, inst_ty, &mut iface)?;
    }
    Ok(())
}

fn register_iface_stubs(
    engine: &Engine,
    iface_name: &str,
    inst_ty: wasmtime::component::types::ComponentInstance,
    iface: &mut LinkerInstance<'_, HostState>,
) -> Result<()> {
    for (fn_name, item) in inst_ty.exports(engine) {
        match item {
            ComponentItem::ComponentFunc(_) => {
                let label = format!("{}::{}", iface_name, fn_name);
                let label_clone = label.clone();
                iface.func_new(fn_name, move |store, _func_ty, params, _results| {
                    if store.data().trace {
                        println!("[stub] {} args={:?}", label_clone, params);
                    }
                    // All Yel callback funcs are unit-returning today;
                    // even if a future callback returns a value the
                    // stub yields zero results which trips at call
                    // time, which is what we want until a user wires a
                    // real handler via the host.
                    Ok(())
                })?;
                if std::env::var("YEL_HOST_DEBUG").is_ok() {
                    eprintln!("[host] stubbed import {}", label);
                }
            }
            ComponentItem::Resource(_) => {
                // Yel components import their own `*-component` interface
                // (containing the resource type) so callback interfaces
                // can refer to the resource via `borrow<self>`. The
                // import never has its methods called — it's a forward-
                // declaration. We register the resource type with a
                // no-op destructor so wasmtime's instance check passes.
                iface.resource(fn_name, ResourceType::host::<()>(), |_store, _rep| Ok(()))?;
                if std::env::var("YEL_HOST_DEBUG").is_ok() {
                    eprintln!("[host] stubbed resource import {}::{}", iface_name, fn_name);
                }
            }
            _ => {}
        }
    }
    Ok(())
}

// ============================================================================
// Subcommand: inspect
// ============================================================================

fn cmd_inspect(component: &Component, engine: &Engine) -> Result<()> {
    let ty = component.component_type();
    println!("=== Imports ===");
    for (name, item) in ty.imports(engine) {
        print_item(engine, name, &item, 0);
    }
    println!();
    println!("=== Exports ===");
    for (name, item) in ty.exports(engine) {
        print_item(engine, name, &item, 0);
    }
    Ok(())
}

fn print_item(engine: &Engine, name: &str, item: &ComponentItem, depth: usize) {
    let pad = "  ".repeat(depth);
    match item {
        ComponentItem::ComponentFunc(_) => println!("{}- fn   {}", pad, name),
        ComponentItem::CoreFunc(_) => println!("{}- core {}", pad, name),
        ComponentItem::Module(_) => println!("{}- mod  {}", pad, name),
        ComponentItem::Component(_) => println!("{}- comp {}", pad, name),
        ComponentItem::ComponentInstance(inst) => {
            println!("{}- iface {}", pad, name);
            for (sub_name, sub_item) in inst.exports(engine) {
                print_item(engine, sub_name, &sub_item, depth + 1);
            }
        }
        ComponentItem::Type(_) => println!("{}- type {}", pad, name),
        ComponentItem::Resource(_) => println!("{}- res  {}", pad, name),
    }
}

// ============================================================================
// Subcommand: run
// ============================================================================

fn cmd_run(component: &Component, engine: &Engine, root: u32, trace: bool) -> Result<()> {
    let descriptor = describe_component(engine, component)?;
    println!("[host] discovered:");
    println!("  interface:  {}", descriptor.iface_name);
    println!("  resource:   {}", descriptor.resource_name);
    println!("  methods:    {}", descriptor.methods.len());
    for m in &descriptor.methods {
        println!("    - {}", m);
    }
    println!();

    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    GenericHost::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |s| s)?;
    register_dynamic_imports(engine, component, &mut linker)?;

    let mut store = Store::new(engine, HostState::new(trace)?);
    let instance = linker
        .instantiate(&mut store, component)
        .map_err(|e| anyhow!("failed to instantiate component: {}", e))?;

    let resource = call_constructor(&mut store, &instance, &descriptor)?;
    println!("[host] constructed resource");

    call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &format!("[method]{}.mount", descriptor.resource_name),
        resource,
        &[Val::U32(root)],
        0,
    )?;
    println!("[host] mounted at root={}", root);

    let roots = store.data().find_roots();
    for r in roots {
        store.data().print_tree(r);
    }

    // Probe every getter and print results — reuse the same resource.
    let getter_prefix = format!("[method]{}.get-", descriptor.resource_name);
    println!("\n=== Component state ===");
    for m in &descriptor.methods {
        if !m.starts_with(&getter_prefix) {
            continue;
        }
        let prop = m.trim_start_matches(&getter_prefix);
        match call_typed_method(&mut store, &instance, &descriptor, m, resource, &[], 1) {
            Ok(results) => println!("  {} = {:?}", prop, results.first()),
            Err(e) => println!("  {} = <error: {}>", prop, e),
        }
    }

    let unmount_name = format!("[method]{}.unmount", descriptor.resource_name);
    match call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &unmount_name,
        resource,
        &[],
        0,
    ) {
        Ok(_) => println!("\n[host] unmounted"),
        Err(e) => println!("\n[host] unmount failed (non-fatal): {}", e),
    }
    Ok(())
}

fn find_constructor(descriptor: &ComponentDescriptor) -> Option<String> {
    let key = format!("[constructor]{}", descriptor.resource_name);
    descriptor.methods.iter().find(|m| **m == key).cloned()
}

fn iface_export_index(
    instance: &Instance,
    store: &mut Store<HostState>,
    iface: &str,
) -> Result<ComponentExportIndex> {
    instance
        .get_export_index(&mut *store, None, iface)
        .ok_or_else(|| anyhow!("interface {} not found in instance", iface))
}

fn fn_export_index(
    instance: &Instance,
    store: &mut Store<HostState>,
    iface_idx: &ComponentExportIndex,
    fn_name: &str,
) -> Result<ComponentExportIndex> {
    instance
        .get_export_index(&mut *store, Some(iface_idx), fn_name)
        .ok_or_else(|| anyhow!("export {} not found in interface", fn_name))
}

fn call_constructor(
    store: &mut Store<HostState>,
    instance: &Instance,
    descriptor: &ComponentDescriptor,
) -> Result<ResourceAny> {
    let ctor = find_constructor(descriptor)
        .ok_or_else(|| anyhow!("no constructor for resource {}", descriptor.resource_name))?;
    let iface_idx = iface_export_index(instance, store, &descriptor.iface_name)?;
    let fn_idx = fn_export_index(instance, store, &iface_idx, &ctor)?;
    let func: Func = instance
        .get_func(&mut *store, &fn_idx)
        .ok_or_else(|| anyhow!("constructor not callable"))?;
    let mut results = vec![Val::Bool(false)];
    func.call(&mut *store, &[], &mut results)?;
    // post_return is a no-op in wasmtime 44+.
    match results.into_iter().next() {
        Some(Val::Resource(r)) => Ok(r),
        other => Err(anyhow!("constructor returned non-resource: {:?}", other)),
    }
}

fn call_typed_method(
    store: &mut Store<HostState>,
    instance: &Instance,
    descriptor: &ComponentDescriptor,
    full_name: &str,
    resource: ResourceAny,
    extra_args: &[Val],
    expected_results: usize,
) -> Result<Vec<Val>> {
    let iface_idx = iface_export_index(instance, store, &descriptor.iface_name)?;
    let fn_idx = fn_export_index(instance, store, &iface_idx, full_name)?;
    let func: Func = instance
        .get_func(&mut *store, &fn_idx)
        .ok_or_else(|| anyhow!("method {} not callable", full_name))?;
    let mut args: Vec<Val> = Vec::with_capacity(extra_args.len() + 1);
    args.push(Val::Resource(resource));
    args.extend(extra_args.iter().cloned());
    let mut results = vec![Val::Bool(false); expected_results];
    func.call(&mut *store, &args, &mut results)?;
    // post_return is a no-op in wasmtime 44+.
    Ok(results)
}

// ============================================================================
// CLI
// ============================================================================

#[derive(Parser)]
#[command(
    name = "yel-host",
    about = "Generic runner for compiled Yel components"
)]
struct Args {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Print the component's imports / exports tree.
    Inspect { component: PathBuf },
    /// Mount the component into the in-memory DOM and print the tree.
    Run {
        component: PathBuf,
        #[arg(short, long, default_value = "0")]
        root: u32,
        /// Trace every host import call.
        #[arg(long)]
        trace: bool,
    },
    /// Mount the component and dump every signal value as a typed tree
    /// — records expanded to fields, lists to elements, options/results
    /// to active case + payload — then print the DOM tree.
    Dump {
        component: PathBuf,
        #[arg(short, long, default_value = "0")]
        root: u32,
        /// Trace every host import call.
        #[arg(long)]
        trace: bool,
        /// Optional setter to apply before dumping. Format: `name=value`.
        /// `value` is one of: integer, `true`/`false`, `"string"`, list
        /// `[a,b,c]`, record `{f1:v1,f2:v2}`. Repeatable.
        #[arg(long = "set", value_name = "PROP=VAL")]
        sets: Vec<String>,
    },
    /// Mount the component and walk its internal Wasm-GC heap directly
    /// via the patched `wasmtime::component::Instance::core_instance`
    /// bridge. Reads the per-component handle registry, follows the
    /// active handle's `$inst` (anyref) to the typed `$Comp_<name>`
    /// struct, and recursively expands every field — including the
    /// mount tree (boundary structs, iter bodies, typed list arrays)
    /// that `dump` cannot reach.
    GcDump {
        component: PathBuf,
        #[arg(short, long, default_value = "0")]
        root: u32,
        /// Maximum recursion depth (cycles in the heap won't be visited
        /// twice but very deep trees can spam output).
        #[arg(long, default_value = "8")]
        max_depth: usize,
    },
    /// Mount the component, then drop into an interactive REPL for
    /// driving its lifecycle: setters via WAVE syntax, getters,
    /// firing recorded `clicked` / `input` event handlers, and
    /// inspecting the current state / DOM tree / GC heap. Useful for
    /// exercising reactive flows (CRUD-style demos) against the
    /// in-memory DOM stub. Type `help` at the prompt for the full
    /// command list.
    Repl {
        component: PathBuf,
        #[arg(short, long, default_value = "0")]
        root: u32,
        /// Trace every host import call.
        #[arg(long)]
        trace: bool,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    let mut config = Config::new();
    config.wasm_component_model(true);
    config.wasm_function_references(true);
    config.wasm_gc(true);
    config.wasm_reference_types(true);
    let engine = Engine::new(&config)?;

    match args.cmd {
        Cmd::Inspect { component } => {
            let component = Component::from_file(&engine, &component)
                .map_err(|e| anyhow!("failed to load {:?}: {}", component, e))?;
            cmd_inspect(&component, &engine)
        }
        Cmd::Run {
            component,
            root,
            trace,
        } => {
            let comp = Component::from_file(&engine, &component)
                .map_err(|e| anyhow!("failed to load {:?}: {}", component, e))?;
            cmd_run(&comp, &engine, root, trace)
        }
        Cmd::Dump {
            component,
            root,
            trace,
            sets,
        } => {
            let comp = Component::from_file(&engine, &component)
                .map_err(|e| anyhow!("failed to load {:?}: {}", component, e))?;
            cmd_dump(&comp, &engine, root, trace, &sets)
        }
        Cmd::GcDump {
            component,
            root,
            max_depth,
        } => {
            let comp = Component::from_file(&engine, &component)
                .map_err(|e| anyhow!("failed to load {:?}: {}", component, e))?;
            let names = build_type_name_map(&component);
            cmd_gc_dump(&comp, &engine, root, max_depth, names)
        }
        Cmd::Repl {
            component,
            root,
            trace,
        } => {
            let comp = Component::from_file(&engine, &component)
                .map_err(|e| anyhow!("failed to load {:?}: {}", component, e))?;
            cmd_repl(&comp, &engine, root, trace)
        }
    }
}

// ============================================================================
// Subcommand: gc-dump — walk component-internal Wasm-GC heap via the patched
// wasmtime `Instance::core_instance` bridge.
// ============================================================================

/// Per-type debug info recovered from the wasm name section: the
/// type's own name (`$counter-component`) plus an optional name per
/// field (e.g. `count`, `label`, `tree`). Field names are sparse —
/// indices without an entry fall back to `.N`.
#[derive(Default, Clone)]
struct TypeDebug {
    name: String,
    field_names: Vec<Option<String>>,
}

/// Parse the component's name section + type section to build a
/// structural-fingerprint → debug-info map. Used at walk time to
/// label runtime structs/arrays with their WAT names
/// (`$counter-component`, `count`, `label`, …) instead of `<#N>` /
/// `.0` / `.1`.
fn build_type_name_map(wasm_path: &std::path::Path) -> HashMap<String, TypeDebug> {
    let mut out: HashMap<String, TypeDebug> = HashMap::new();
    let bytes = match std::fs::read(wasm_path) {
        Ok(b) => b,
        Err(_) => return out,
    };

    let mut all_types: Vec<Option<SubType>> = Vec::new();
    let mut local_to_name: HashMap<u32, String> = HashMap::new();
    // (type_idx, field_idx) → field name
    let mut field_names: HashMap<(u32, u32), String> = HashMap::new();

    fn fingerprint_storage(
        s: &StorageType,
        all_types: &[Option<SubType>],
        visiting: &mut HashSet<u32>,
    ) -> String {
        match s {
            StorageType::I8 => "i8".into(),
            StorageType::I16 => "i16".into(),
            StorageType::Val(v) => fingerprint_val(v, all_types, visiting),
        }
    }
    fn fingerprint_val(
        v: &ValType,
        all_types: &[Option<SubType>],
        visiting: &mut HashSet<u32>,
    ) -> String {
        match v {
            ValType::I32 => "i32".into(),
            ValType::I64 => "i64".into(),
            ValType::F32 => "f32".into(),
            ValType::F64 => "f64".into(),
            ValType::V128 => "v128".into(),
            ValType::Ref(r) => fingerprint_ref(r, all_types, visiting),
        }
    }
    fn fingerprint_ref(
        r: &RefType,
        all_types: &[Option<SubType>],
        visiting: &mut HashSet<u32>,
    ) -> String {
        let nullable = if r.is_nullable() { " null" } else { "" };
        let concrete_fingerprint = |idx: wasmparser::UnpackedIndex,
                                    all_types: &[Option<SubType>],
                                    visiting: &mut HashSet<u32>|
         -> String {
            let n = idx.as_module_index().unwrap_or(0);
            if visiting.contains(&n) {
                "(rec)".into()
            } else if let Some(Some(sub)) = all_types.get(n as usize) {
                visiting.insert(n);
                let s = fingerprint_inner(&sub.composite_type.inner, all_types, visiting);
                visiting.remove(&n);
                s
            } else {
                "?".into()
            }
        };
        let inner = match r.heap_type() {
            PHeapType::Abstract { ty, .. } => format!("{:?}", ty),
            PHeapType::Concrete(idx) => concrete_fingerprint(idx, all_types, visiting),
            PHeapType::Exact(idx) => concrete_fingerprint(idx, all_types, visiting),
        };
        format!("(ref{} {})", nullable, inner)
    }
    fn fingerprint_inner(
        inner: &CompositeInnerType,
        all_types: &[Option<SubType>],
        visiting: &mut HashSet<u32>,
    ) -> String {
        match inner {
            CompositeInnerType::Func(_) => "func".into(),
            CompositeInnerType::Cont(_) => "cont".into(),
            CompositeInnerType::Struct(s) => {
                let mut buf = String::from("struct[");
                for (i, f) in s.fields.iter().enumerate() {
                    if i > 0 {
                        buf.push(',');
                    }
                    buf.push_str(&fingerprint_storage(&f.element_type, all_types, visiting));
                    if f.mutable {
                        buf.push('!');
                    }
                }
                buf.push(']');
                buf
            }
            CompositeInnerType::Array(a) => {
                let mut buf = String::from("array[");
                buf.push_str(&fingerprint_storage(&a.0.element_type, all_types, visiting));
                if a.0.mutable {
                    buf.push('!');
                }
                buf.push(']');
                buf
            }
        }
    }

    for payload in Parser::new(0).parse_all(&bytes).flatten() {
        match payload {
            Payload::TypeSection(reader) => {
                for rec in reader.into_iter().flatten() {
                    for sub in rec.into_types() {
                        all_types.push(Some(sub));
                    }
                }
            }
            Payload::CustomSection(cs) => {
                if let KnownCustom::Name(reader) = cs.as_known() {
                    for sub in reader.into_iter().flatten() {
                        match sub {
                            Name::Type(map) => {
                                for entry in map.into_iter().flatten() {
                                    local_to_name.insert(entry.index, entry.name.to_string());
                                }
                            }
                            Name::Field(indirect) => {
                                for indirect_naming in indirect.into_iter().flatten() {
                                    let type_idx = indirect_naming.index;
                                    for entry in indirect_naming.names.into_iter().flatten() {
                                        field_names.insert(
                                            (type_idx, entry.index),
                                            entry.name.to_string(),
                                        );
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }

    for (idx, sub) in all_types.iter().enumerate() {
        let sub = match sub {
            Some(s) => s,
            None => continue,
        };
        let name = match local_to_name.get(&(idx as u32)) {
            Some(n) => n.clone(),
            None => continue,
        };
        let mut visiting = HashSet::new();
        visiting.insert(idx as u32);
        let key = fingerprint_inner(&sub.composite_type.inner, &all_types, &mut visiting);
        // Collect per-field names (sparse — only the indices that
        // appear in the name section get an entry).
        let n_fields = match &sub.composite_type.inner {
            CompositeInnerType::Struct(s) => s.fields.len(),
            _ => 0,
        };
        let mut fnames: Vec<Option<String>> = vec![None; n_fields];
        for fi in 0..n_fields as u32 {
            if let Some(n) = field_names.get(&(idx as u32, fi)) {
                fnames[fi as usize] = Some(n.clone());
            }
        }
        out.entry(key).or_insert(TypeDebug {
            name,
            field_names: fnames,
        });
    }
    out
}

/// Compute a runtime fingerprint that matches what
/// `build_type_name_map` produces for the wasm binary. Walks concrete
/// heap-typed refs recursively (with cycle detection) so two arrays
/// like `(array (ref null $handle))` and
/// `(array (ref null $counter-for_0_iter))` map to distinct keys.
fn fingerprint_runtime_struct(ty: &wasmtime::StructType) -> String {
    let mut visiting: Vec<String> = Vec::new();
    rt_fp_struct(ty, &mut visiting)
}

fn fingerprint_runtime_array(ty: &wasmtime::ArrayType) -> String {
    let mut visiting: Vec<String> = Vec::new();
    rt_fp_array(ty, &mut visiting)
}

fn rt_fp_struct(ty: &wasmtime::StructType, visiting: &mut Vec<String>) -> String {
    let mut buf = String::from("struct[");
    for (i, field) in ty.fields().enumerate() {
        if i > 0 {
            buf.push(',');
        }
        buf.push_str(&rt_fp_storage(&field.element_type(), visiting));
        if field.mutability().is_var() {
            buf.push('!');
        }
    }
    buf.push(']');
    buf
}

fn rt_fp_array(ty: &wasmtime::ArrayType, visiting: &mut Vec<String>) -> String {
    let f = ty.field_type();
    let mut buf = String::from("array[");
    buf.push_str(&rt_fp_storage(&f.element_type(), visiting));
    if f.mutability().is_var() {
        buf.push('!');
    }
    buf.push(']');
    buf
}

fn rt_fp_storage(s: &wasmtime::StorageType, visiting: &mut Vec<String>) -> String {
    use wasmtime::{StorageType as RtStorage, ValType as RtVal};
    match s {
        RtStorage::I8 => "i8".into(),
        RtStorage::I16 => "i16".into(),
        RtStorage::ValType(RtVal::I32) => "i32".into(),
        RtStorage::ValType(RtVal::I64) => "i64".into(),
        RtStorage::ValType(RtVal::F32) => "f32".into(),
        RtStorage::ValType(RtVal::F64) => "f64".into(),
        RtStorage::ValType(RtVal::V128) => "v128".into(),
        RtStorage::ValType(RtVal::Ref(r)) => {
            let nullable = if r.is_nullable() { " null" } else { "" };
            let ht = r.heap_type();
            // Use a stable identity-string for each concrete type to
            // detect cycles. wasmtime's StructType / ArrayType implement
            // Display, which approximates a structural string — fine
            // as a cycle key.
            let inner = if let Some(st) = ht.as_concrete_struct() {
                let id = format!("{}", st);
                if visiting.iter().any(|v| v == &id) {
                    "(rec)".into()
                } else {
                    visiting.push(id);
                    let s = rt_fp_struct(st, visiting);
                    visiting.pop();
                    s
                }
            } else if let Some(at) = ht.as_concrete_array() {
                let id = format!("{}", at);
                if visiting.iter().any(|v| v == &id) {
                    "(rec)".into()
                } else {
                    visiting.push(id);
                    let s = rt_fp_array(at, visiting);
                    visiting.pop();
                    s
                }
            } else {
                // Abstract heap types (any, eq, struct, array, …):
                // their wasmtime Display matches the parser's
                // `format!("{:?}", abstract_ty)` for our purposes.
                format!("{:?}", ht)
            };
            format!("(ref{} {})", nullable, inner)
        }
    }
}

fn cmd_gc_dump(
    component: &Component,
    engine: &Engine,
    root: u32,
    max_depth: usize,
    names: HashMap<String, TypeDebug>,
) -> Result<()> {
    let descriptor = describe_component(engine, component)?;

    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    GenericHost::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |s| s)?;
    register_dynamic_imports(engine, component, &mut linker)?;

    let mut store = Store::new(engine, HostState::new(false)?);
    let instance = linker
        .instantiate(&mut store, component)
        .map_err(|e| anyhow!("failed to instantiate component: {}", e))?;

    // Constructor → mount, so the registry is populated and the
    // component-struct fields hold their initial values.
    let resource = call_constructor(&mut store, &instance, &descriptor)?;
    call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &format!("[method]{}.mount", descriptor.resource_name),
        resource,
        &[Val::U32(root)],
        0,
    )?;

    // Drill into the underlying core instance via the patch. A
    // component instantiates several core modules (jco-style adapter
    // shims wrap the user core), so scan all of them and pick the one
    // that exports the registry global we expect.
    let registry_name_for_search = format!("{}-registry", descriptor.resource_name);
    let mut core_opt: Option<wasmtime::Instance> = None;
    for idx in 0..32u32 {
        let inst = match instance.core_instance(&mut store, idx) {
            Some(i) => i,
            None => break,
        };
        if inst
            .get_global(&mut store, &registry_name_for_search)
            .is_some()
        {
            core_opt = Some(inst);
            break;
        }
    }
    let core = core_opt.ok_or_else(|| {
        anyhow!(
            "no core instance exports a global named {:?} — did yel-codegen \
             emit it? did you re-build the .wasm after upgrading codegen?",
            registry_name_for_search
        )
    })?;

    // Yel emits one global per exported component called
    // `$<comp-name>-registry` — a `(array (ref null $handle))`.
    // Derive `<comp-name>` from the resource name (kebab-case already).
    let registry_name = format!("{}-registry", descriptor.resource_name);

    let global = core.get_global(&mut store, &registry_name).or_else(|| {
        // Diagnostic: enumerate all exports the core instance does
        // expose so the user can see what's reachable.
        eprintln!(
            "[host] note: core instance has no global {:?}; visible core exports:",
            registry_name
        );
        let exports: Vec<(String, &'static str)> = core
            .exports(&mut store)
            .map(|e| {
                let name = e.name().to_string();
                let kind = match e.into_extern() {
                    wasmtime::Extern::Func(_) => "func",
                    wasmtime::Extern::Global(_) => "global",
                    wasmtime::Extern::Table(_) => "table",
                    wasmtime::Extern::Memory(_) => "memory",
                    wasmtime::Extern::SharedMemory(_) => "shared_memory",
                    wasmtime::Extern::Tag(_) => "tag",
                };
                (name, kind)
            })
            .collect();
        for (name, kind) in &exports {
            eprintln!("    {:6} {}", kind, name);
        }
        None
    });
    let global =
        global.ok_or_else(|| anyhow!("core instance has no global named {:?}", registry_name))?;

    println!();
    println!("┌─ GC Heap ──────────────────────────────────────");
    println!("│ entry: ${}", registry_name);

    // Use a single root scope for the whole walk so every Rooted<> we
    // create gets unrooted on drop — keeps the GC heap reclaimable.
    let mut scope = RootScope::new(&mut store);

    let registry_val = global.get(&mut scope);
    let registry_anyref = match registry_val {
        CoreVal::AnyRef(Some(r)) => r,
        CoreVal::AnyRef(None) => {
            println!("│  (registry is null)");
            println!("└────────────────────────────────────────────────");
            return Ok(());
        }
        other => {
            println!("│  (unexpected registry type: {:?})", other);
            println!("└────────────────────────────────────────────────");
            return Ok(());
        }
    };

    let mut visited: HashMap<u64, u32> = HashMap::new();
    let mut next_id: u32 = 0;
    walk_anyref(
        &mut scope,
        &registry_anyref,
        "",
        true,
        None,
        max_depth,
        &mut visited,
        &mut next_id,
        &names,
    )?;
    println!("└────────────────────────────────────────────────");
    Ok(())
}

/// Recursively walk an `AnyRef` and pretty-print the typed Wasm-GC tree.
/// `visited` deduplicates cycles (struct/array refs we've already shown
/// once get printed as `<seen #N>`).
/// Tree-line printing helpers. `prefix` is everything that's already
/// drawn on the current line up to (but not including) the connector
/// for this node; `is_last` decides whether to draw `└──` or `├──`
/// and which prefix segment (`    ` vs `│   `) child nodes inherit.
fn connector(is_last: bool) -> &'static str {
    if is_last {
        "└── "
    } else {
        "├── "
    }
}
fn child_prefix(is_last: bool) -> &'static str {
    if is_last {
        "    "
    } else {
        "│   "
    }
}

fn walk_anyref(
    store: &mut RootScope<&mut Store<HostState>>,
    anyref: &Rooted<AnyRef>,
    prefix: &str,
    is_last: bool,
    field_label: Option<&str>,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    names: &HashMap<String, TypeDebug>,
) -> Result<()> {
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();

    if max_depth == 0 {
        println!("│{}{}{}<…max-depth>", prefix, connector(is_last), lab);
        return Ok(());
    }

    let key = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut h = DefaultHasher::new();
        format!("{:?}", anyref).hash(&mut h);
        h.finish()
    };
    if let Some(&id) = visited.get(&key) {
        println!("│{}{}{}<seen #{}>", prefix, connector(is_last), lab, id);
        return Ok(());
    }
    let my_id = *next_id;
    *next_id += 1;
    visited.insert(key, my_id);

    if let Ok(Some(sr)) = anyref.as_struct(&store) {
        return walk_struct(
            store,
            &sr,
            prefix,
            is_last,
            field_label,
            max_depth - 1,
            visited,
            next_id,
            my_id,
            names,
        );
    }
    if let Ok(Some(ar)) = anyref.as_array(&store) {
        return walk_array(
            store,
            &ar,
            prefix,
            is_last,
            field_label,
            max_depth - 1,
            visited,
            next_id,
            my_id,
            names,
        );
    }
    println!(
        "│{}{}{}#{} opaque anyref ({:?})",
        prefix,
        connector(is_last),
        lab,
        my_id,
        anyref
    );
    Ok(())
}

fn walk_struct(
    store: &mut RootScope<&mut Store<HostState>>,
    sr: &Rooted<StructRef>,
    prefix: &str,
    is_last: bool,
    field_label: Option<&str>,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    my_id: u32,
    names: &HashMap<String, TypeDebug>,
) -> Result<()> {
    let ty = sr.ty(&store).map_err(|e| anyhow!("struct.ty: {}", e))?;
    let n = ty.fields().len();
    let fp = fingerprint_runtime_struct(&ty);
    let dbg = names.get(&fp);
    let type_name = dbg
        .map(|d| format!("${}", d.name))
        .unwrap_or_else(|| "<unnamed-struct>".into());
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    println!(
        "│{}{}{}{} #{} ({} fields)",
        prefix,
        connector(is_last),
        lab,
        type_name,
        my_id,
        n
    );
    let new_prefix = format!("{}{}", prefix, child_prefix(is_last));
    for i in 0..n {
        let val = sr
            .field(&mut *store, i)
            .map_err(|e| anyhow!("struct.field({}): {}", i, e))?;
        let last = i + 1 == n;
        // Prefer the field's debug name from the wasm name section
        // (prefixed with `$` to match WAT). Fall back to `$fieldN`
        // when a name isn't present.
        let field_name = dbg
            .and_then(|d| d.field_names.get(i))
            .and_then(|opt| opt.as_deref())
            .map(|n| format!("${}", n))
            .unwrap_or_else(|| format!("$field{}", i));
        walk_core_val(
            store,
            &val,
            &new_prefix,
            last,
            Some(&field_name),
            max_depth,
            visited,
            next_id,
            names,
        )?;
    }
    Ok(())
}

fn walk_array(
    store: &mut RootScope<&mut Store<HostState>>,
    ar: &Rooted<ArrayRef>,
    prefix: &str,
    is_last: bool,
    field_label: Option<&str>,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    my_id: u32,
    names: &HashMap<String, TypeDebug>,
) -> Result<()> {
    let len = ar.len(&store).map_err(|e| anyhow!("array.len: {}", e))?;
    let ty = ar.ty(&store).map_err(|e| anyhow!("array.ty: {}", e))?;
    let fp = fingerprint_runtime_array(&ty);
    let name = match names.get(&fp) {
        Some(d) => format!("${}", d.name),
        None => "<unnamed-array>".into(),
    };
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    println!(
        "│{}{}{}{} #{} (len={})",
        prefix,
        connector(is_last),
        lab,
        name,
        my_id,
        len
    );
    let new_prefix = format!("{}{}", prefix, child_prefix(is_last));
    let cap = std::cmp::min(len, 64);
    let extra = len > cap;
    for i in 0..cap {
        let val = ar
            .get(&mut *store, i)
            .map_err(|e| anyhow!("array.get({}): {}", i, e))?;
        let last_visible = i + 1 == cap && !extra;
        let label = format!("[{}]", i);
        walk_core_val(
            store,
            &val,
            &new_prefix,
            last_visible,
            Some(&label),
            max_depth,
            visited,
            next_id,
            names,
        )?;
    }
    if extra {
        println!(
            "│{}{}…{} more elements",
            new_prefix,
            connector(true),
            len - cap
        );
    }
    Ok(())
}

fn walk_core_val(
    store: &mut RootScope<&mut Store<HostState>>,
    val: &CoreVal,
    prefix: &str,
    is_last: bool,
    field_label: Option<&str>,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    names: &HashMap<String, TypeDebug>,
) -> Result<()> {
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    let leaf = |s: String| {
        println!("│{}{}{}{}", prefix, connector(is_last), lab, s);
    };
    match val {
        CoreVal::I32(v) => leaf(format!("{} : i32", v)),
        CoreVal::I64(v) => leaf(format!("{} : i64", v)),
        CoreVal::F32(v) => leaf(format!("{} : f32", f32::from_bits(*v))),
        CoreVal::F64(v) => leaf(format!("{} : f64", f64::from_bits(*v))),
        CoreVal::V128(v) => leaf(format!("{:?} : v128", v)),
        CoreVal::FuncRef(_) => leaf("<funcref>".into()),
        CoreVal::ExternRef(None) => leaf("null externref".into()),
        CoreVal::ExternRef(Some(_)) => leaf("<externref>".into()),
        CoreVal::AnyRef(None) => leaf("null".into()),
        CoreVal::AnyRef(Some(a)) => walk_anyref(
            store,
            a,
            prefix,
            is_last,
            field_label,
            max_depth,
            visited,
            next_id,
            names,
        )?,
        CoreVal::ExnRef(_) => leaf("<exnref>".into()),
        CoreVal::ContRef(_) => leaf("<contref>".into()),
    }
    Ok(())
}

// ============================================================================
// Subcommand: dump — runtime state dump with deeply-expanded values
// ============================================================================

fn cmd_dump(
    component: &Component,
    engine: &Engine,
    root: u32,
    trace: bool,
    sets: &[String],
) -> Result<()> {
    let descriptor = describe_component(engine, component)?;

    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    GenericHost::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |s| s)?;
    register_dynamic_imports(engine, component, &mut linker)?;

    let mut store = Store::new(engine, HostState::new(trace)?);
    let instance = linker
        .instantiate(&mut store, component)
        .map_err(|e| anyhow!("failed to instantiate component: {}", e))?;

    let resource = call_constructor(&mut store, &instance, &descriptor)?;

    // Mount FIRST so reactive setters that re-render have a tree.
    call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &format!("[method]{}.mount", descriptor.resource_name),
        resource,
        &[Val::U32(root)],
        0,
    )?;

    // Apply --set property writes (so the dump shows post-mutation state).
    // Each setter trap is recoverable — wasmtime resets the instance's
    // entry-guard once `post_return` runs, but if the setter itself
    // trapped mid-call the instance may be poisoned for the rest of
    // this session. Stop applying further --sets after the first trap
    // to surface the original error cleanly.
    for spec in sets {
        match apply_set(&mut store, &instance, &descriptor, resource, spec) {
            Ok(()) => println!("[host] applied --set {}", spec),
            Err(e) => {
                eprintln!("[host] --set {} failed: {}", spec, e);
                eprintln!("[host] aborting further --set; component instance may be poisoned");
                break;
            }
        }
    }

    // Pretty-print signal values via every getter.
    println!("═══════════════════════════════════════════════════════════════");
    println!(" {} — runtime state", descriptor.iface_name);
    println!("═══════════════════════════════════════════════════════════════");
    let getter_prefix = format!("[method]{}.get-", descriptor.resource_name);
    let mut signal_count = 0usize;
    for m in &descriptor.methods {
        if !m.starts_with(&getter_prefix) {
            continue;
        }
        let prop = m.trim_start_matches(&getter_prefix);
        match call_typed_method(&mut store, &instance, &descriptor, m, resource, &[], 1) {
            Ok(results) => {
                let val = results.into_iter().next().unwrap_or(Val::Bool(false));
                print_named_val(prop, &val, 0);
                signal_count += 1;
            }
            Err(e) => println!("{} — <error: {}>", prop, e),
        }
    }
    println!();
    println!("({} signals)", signal_count);

    // DOM tree.
    println!();
    println!("═══════════════════════════════════════════════════════════════");
    println!(" DOM tree (synthetic, root={})", root);
    println!("═══════════════════════════════════════════════════════════════");
    let roots = store.data().find_roots();
    for r in roots {
        store.data().print_tree(r);
    }

    // Quiet unmount.
    let _ = call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &format!("[method]{}.unmount", descriptor.resource_name),
        resource,
        &[],
        0,
    );
    Ok(())
}

/// Recursive `Val` pretty printer with WIT-friendly formatting.
/// Width budget after which a Val is expanded across multiple lines
/// instead of formatted inline. Tuned for typical terminal width.
const PRETTY_INLINE_WIDTH: usize = 72;

/// Render a property as `name: <value>`. The value goes inline when
/// `name: <inline>` fits in [`PRETTY_INLINE_WIDTH`] at the given depth;
/// otherwise the value drops to the next line indented one level
/// deeper, matching the multi-line expansion in [`pretty_print_val`].
fn print_named_val(name: &str, val: &Val, depth: usize) {
    let pad = "  ".repeat(depth);
    let inline = fmt_inline(val);
    if pad.len() + name.len() + 2 + inline.len() <= PRETTY_INLINE_WIDTH {
        println!("{}{}: {}", pad, name, inline);
    } else {
        println!("{}{}:", pad, name);
        pretty_print_val(val, depth + 1);
    }
}

/// Compact one-line WAVE-style formatter. Prefer [`pretty_print_val`]
/// at call sites — it falls back here when the result fits in
/// [`PRETTY_INLINE_WIDTH`] and expands aggregate cases (list / record /
/// tuple / variant-with-payload / option-some / result-ok-some) onto
/// multiple lines otherwise. Bare scalars / strings / `none` / enum
/// variants always render inline.
fn fmt_inline(val: &Val) -> String {
    match val {
        Val::Bool(b) => b.to_string(),
        Val::S8(v) => v.to_string(),
        Val::S16(v) => v.to_string(),
        Val::S32(v) => v.to_string(),
        Val::S64(v) => v.to_string(),
        Val::U8(v) => v.to_string(),
        Val::U16(v) => v.to_string(),
        Val::U32(v) => v.to_string(),
        Val::U64(v) => v.to_string(),
        Val::Float32(v) => format!("{}", v),
        Val::Float64(v) => format!("{}", v),
        Val::Char(c) => format!("'{}'", c),
        Val::String(s) => format!("{:?}", s),
        Val::List(items) => {
            let parts: Vec<String> = items.iter().map(fmt_inline).collect();
            format!("[{}]", parts.join(", "))
        }
        Val::Record(fields) => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(k, v)| format!("{}: {}", k, fmt_inline(v)))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        Val::Tuple(elems) => {
            let parts: Vec<String> = elems.iter().map(fmt_inline).collect();
            format!("({})", parts.join(", "))
        }
        Val::Variant(name, payload) => match payload {
            None => name.clone(),
            Some(p) => format!("{}({})", name, fmt_inline(p)),
        },
        Val::Enum(name) => name.clone(),
        Val::Option(None) => "none".to_string(),
        Val::Option(Some(inner)) => format!("some({})", fmt_inline(inner)),
        Val::Result(Ok(None)) => "ok".to_string(),
        Val::Result(Ok(Some(inner))) => format!("ok({})", fmt_inline(inner)),
        Val::Result(Err(None)) => "err".to_string(),
        Val::Result(Err(Some(inner))) => format!("err({})", fmt_inline(inner)),
        Val::Flags(names) => format!("{{{}}}", names.join(", ")),
        Val::Resource(_) => "<resource>".to_string(),
        Val::Future(_) => "<future>".to_string(),
        Val::Stream(_) => "<stream>".to_string(),
        Val::ErrorContext(_) => "<error-context>".to_string(),
        Val::Map(entries) => {
            let parts: Vec<String> = entries
                .iter()
                .map(|(k, v)| format!("{} -> {}", fmt_inline(k), fmt_inline(v)))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
    }
}

fn pretty_print_val(val: &Val, depth: usize) {
    let pad = "  ".repeat(depth);

    // Try inline first. Fits in budget → one line, done.
    let inline = fmt_inline(val);
    if pad.len() + inline.len() <= PRETTY_INLINE_WIDTH {
        println!("{}{}", pad, inline);
        return;
    }

    // Expanded form: aggregate values get multi-line layout, scalars
    // never reach here (they always fit inline).
    match val {
        Val::List(items) => {
            println!("{}[", pad);
            for (i, it) in items.iter().enumerate() {
                let item_inline = fmt_inline(it);
                let comma = if i + 1 < items.len() { "," } else { "" };
                if pad.len() + 2 + item_inline.len() + comma.len() <= PRETTY_INLINE_WIDTH {
                    println!("{}  {}{}", pad, item_inline, comma);
                } else {
                    pretty_print_val(it, depth + 1);
                    if !comma.is_empty() {
                        // trailing comma sits on its own line for clarity
                        println!("{}  ,", pad);
                    }
                }
            }
            println!("{}]", pad);
        }
        Val::Record(fields) => {
            println!("{}{{", pad);
            for (i, (name, fv)) in fields.iter().enumerate() {
                let v_inline = fmt_inline(fv);
                let comma = if i + 1 < fields.len() { "," } else { "" };
                let head = format!("  {}: ", name);
                if pad.len() + head.len() + v_inline.len() + comma.len() <= PRETTY_INLINE_WIDTH {
                    println!("{}{}{}{}", pad, head, v_inline, comma);
                } else {
                    println!("{}{}", pad, head.trim_end());
                    pretty_print_val(fv, depth + 2);
                    if !comma.is_empty() {
                        println!("{}  ,", pad);
                    }
                }
            }
            println!("{}}}", pad);
        }
        Val::Tuple(elems) => {
            println!("{}(", pad);
            for (i, e) in elems.iter().enumerate() {
                let comma = if i + 1 < elems.len() { "," } else { "" };
                let inline_e = fmt_inline(e);
                if pad.len() + 2 + inline_e.len() + comma.len() <= PRETTY_INLINE_WIDTH {
                    println!("{}  {}{}", pad, inline_e, comma);
                } else {
                    pretty_print_val(e, depth + 1);
                }
            }
            println!("{})", pad);
        }
        Val::Variant(name, Some(p)) => {
            println!("{}{}(", pad, name);
            pretty_print_val(p, depth + 1);
            println!("{})", pad);
        }
        Val::Option(Some(inner)) => {
            println!("{}some(", pad);
            pretty_print_val(inner, depth + 1);
            println!("{})", pad);
        }
        Val::Result(Ok(Some(inner))) => {
            println!("{}ok(", pad);
            pretty_print_val(inner, depth + 1);
            println!("{})", pad);
        }
        Val::Result(Err(Some(inner))) => {
            println!("{}err(", pad);
            pretty_print_val(inner, depth + 1);
            println!("{})", pad);
        }
        Val::Map(entries) => {
            println!("{}{{", pad);
            for (i, (k, v)) in entries.iter().enumerate() {
                let comma = if i + 1 < entries.len() { "," } else { "" };
                println!("{}  {} ->", pad, fmt_inline(k));
                pretty_print_val(v, depth + 2);
                if !comma.is_empty() {
                    println!("{}  ,", pad);
                }
            }
            println!("{}}}", pad);
        }
        // Scalars / atoms never reach here — they always fit inline.
        _ => println!("{}{}", pad, inline),
    }
}

/// Apply a `--set NAME=VAL` spec. `VAL` is parsed against the setter's
/// declared parameter [`Type`] using WAVE — Bytecode Alliance's textual
/// component-value format. So `42` → s32 / s64 / u32 / etc. depending on
/// what the setter expects, `[1,2,3]` → typed list, `{f1: v1}` → record,
/// `some(42)` / `none` → option, etc., with no ad-hoc heuristics.
///
/// See https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasm-wave
/// for the syntax.
fn apply_set(
    store: &mut Store<HostState>,
    instance: &Instance,
    descriptor: &ComponentDescriptor,
    resource: ResourceAny,
    spec: &str,
) -> Result<()> {
    let (name, raw) = spec
        .split_once('=')
        .ok_or_else(|| anyhow!("--set spec missing `=` separator: {}", spec))?;
    let setter = format!("[method]{}.set-{}", descriptor.resource_name, name);
    if !descriptor.methods.iter().any(|m| m == &setter) {
        return Err(anyhow!("no setter for property {:?}", name));
    }

    // Discover the setter's value-parameter type so WAVE parsing knows
    // the target shape. Setter signatures are
    // `(self: borrow<resource>, value: T)` — params[1] is the value.
    let iface_idx = iface_export_index(instance, store, &descriptor.iface_name)?;
    let fn_idx = fn_export_index(instance, store, &iface_idx, &setter)?;
    let func: Func = instance
        .get_func(&mut *store, &fn_idx)
        .ok_or_else(|| anyhow!("setter {} not callable", setter))?;
    let params: Vec<(String, wasmtime::component::Type)> = func
        .ty(&*store)
        .params()
        .map(|(n, t)| (n.to_string(), t))
        .collect();
    let value_ty = params
        .get(1)
        .ok_or_else(|| anyhow!("setter {} has no value param (expected at index 1)", setter))?
        .1
        .clone();

    let val = Val::from_wave(&value_ty, raw.trim())
        .map_err(|e| anyhow!("WAVE parse failure for `{}`: {}", raw.trim(), e))?;

    call_typed_method(store, instance, descriptor, &setter, resource, &[val], 0)?;
    Ok(())
}

// ============================================================================
// Subcommand: repl — interactive lifecycle driver
// ============================================================================

/// Look up the component's exported `yel:ui/dispatch@0.1.0#dispatch`
/// function and invoke it with the given handler id. The component
/// runs its registered closure (the body of e.g. `clicked: { ... }`),
/// which mutates signals and re-renders.
fn fire_handler(store: &mut Store<HostState>, instance: &Instance, handler_id: u32) -> Result<()> {
    const DISPATCH_IFACE: &str = "yel:ui/dispatch@0.1.0";
    let iface_idx = instance
        .get_export_index(&mut *store, None, DISPATCH_IFACE)
        .ok_or_else(|| anyhow!("component does not export {}", DISPATCH_IFACE))?;
    let fn_idx = instance
        .get_export_index(&mut *store, Some(&iface_idx), "dispatch")
        .ok_or_else(|| anyhow!("{} missing `dispatch` function", DISPATCH_IFACE))?;
    let func: Func = instance
        .get_func(&mut *store, &fn_idx)
        .ok_or_else(|| anyhow!("dispatch not callable"))?;
    // Signature: `dispatch(handler-id: u32, event: event-value)`. The
    // `event-value` variant has a `none` arm used for fire-and-forget
    // signals like `clicked`; carrier-bearing events (e.g. text-input
    // change) take `input-text(string)` etc. The REPL only supports
    // `none` today — adding payload-bearing fires is a small extension.
    let event = Val::Variant("none".to_string(), None);
    let mut results: Vec<Val> = Vec::new();
    func.call(&mut *store, &[Val::U32(handler_id), event], &mut results)?;
    Ok(())
}

fn print_repl_help() {
    println!(
        "
yel-host repl commands:

  set <prop>=<wave>            apply a setter; <wave> is parsed against the
                                 setter's WIT type — e.g.
                                   set count=42
                                   set label=\"hello\"
                                   set items=[1,2,3]
                                   set items=[{{name:\"A\",subitems:[\"x\"]}}]
                                   set maybe=some(7)
  get <prop>                   call the getter and pretty-print the value
  fire <node>:<event>          dispatch the handler registered for that
                                 (node,event) pair via the in-memory DOM
                                 (e.g. `fire 12:clicked`)
  fire #<handler_id>           dispatch a handler by raw id (e.g. `fire #0`)
  handlers                     list every recorded (node, event) → handler_id
  state                        dump every signal as a typed tree
  tree                         print the current DOM tree
  gc                           walk the GC heap (typed structs, arrays, refs)
  trace [on|off]               toggle host-import tracing
  help                         this message
  quit | exit                  unmount and leave
"
    );
}

fn cmd_repl(component: &Component, engine: &Engine, root: u32, trace: bool) -> Result<()> {
    use std::io::{BufRead, Write};

    let descriptor = describe_component(engine, component)?;

    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    GenericHost::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |s| s)?;
    register_dynamic_imports(engine, component, &mut linker)?;

    let mut store = Store::new(engine, HostState::new(trace)?);
    let instance = linker
        .instantiate(&mut store, component)
        .map_err(|e| anyhow!("failed to instantiate component: {}", e))?;

    let resource = call_constructor(&mut store, &instance, &descriptor)?;
    call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &format!("[method]{}.mount", descriptor.resource_name),
        resource,
        &[Val::U32(root)],
        0,
    )?;
    println!("[host] mounted {} at root={}", descriptor.iface_name, root);
    println!("[host] type `help` for commands, `quit` to exit");

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut input = String::new();
    let getter_prefix = format!("[method]{}.get-", descriptor.resource_name);

    loop {
        print!("yel> ");
        stdout.flush().ok();
        input.clear();
        if stdin.lock().read_line(&mut input)? == 0 {
            // EOF (Ctrl-D)
            println!();
            break;
        }
        let line = input.trim();
        if line.is_empty() {
            continue;
        }

        let (cmd, rest) = match line.split_once(char::is_whitespace) {
            Some((c, r)) => (c, r.trim()),
            None => (line, ""),
        };

        match cmd {
            "quit" | "exit" => break,
            "help" | "?" => print_repl_help(),
            "set" => match apply_set(&mut store, &instance, &descriptor, resource, rest) {
                Ok(()) => println!("[ok]"),
                Err(e) => println!("[err] {}", e),
            },
            "get" => {
                let prop = rest;
                let getter = format!("{}{}", getter_prefix, prop);
                if !descriptor.methods.iter().any(|m| m == &getter) {
                    println!("[err] no getter for {:?}", prop);
                    continue;
                }
                match call_typed_method(
                    &mut store,
                    &instance,
                    &descriptor,
                    &getter,
                    resource,
                    &[],
                    1,
                ) {
                    Ok(results) => {
                        let val = results.into_iter().next().unwrap_or(Val::Bool(false));
                        print_named_val(prop, &val, 0);
                    }
                    Err(e) => println!("[err] {}", e),
                }
            }
            "fire" => {
                // Two forms: `fire <node>:<event>` or `fire #<handler_id>`.
                let handler_id_opt: Option<u32> = if let Some(idx) = rest.strip_prefix('#') {
                    idx.trim().parse::<u32>().ok()
                } else if let Some((node_s, event)) = rest.split_once(':') {
                    let node: u32 = match node_s.trim().parse() {
                        Ok(n) => n,
                        Err(_) => {
                            println!("[err] invalid node id `{}`", node_s);
                            continue;
                        }
                    };
                    let event = event.trim().to_string();
                    store
                        .data()
                        .event_handlers
                        .get(&(node, event.clone()))
                        .copied()
                        .or_else(|| {
                            println!("[err] no handler registered for ({}, {:?})", node, event);
                            None
                        })
                } else {
                    println!("[err] usage: `fire <node>:<event>` or `fire #<handler_id>`");
                    None
                };
                if let Some(hid) = handler_id_opt {
                    match fire_handler(&mut store, &instance, hid) {
                        Ok(()) => println!("[ok] handler {} fired", hid),
                        Err(e) => println!("[err] {}", e),
                    }
                }
            }
            "handlers" => {
                let entries: Vec<_> = store
                    .data()
                    .event_handlers
                    .iter()
                    .map(|((n, ev), h)| (*n, ev.clone(), *h))
                    .collect();
                if entries.is_empty() {
                    println!("(no handlers)");
                } else {
                    for (n, ev, h) in entries {
                        println!("  {}:{} -> handler_{}", n, ev, h);
                    }
                }
            }
            "state" => {
                for m in &descriptor.methods {
                    if !m.starts_with(&getter_prefix) {
                        continue;
                    }
                    let prop = m.trim_start_matches(&getter_prefix);
                    match call_typed_method(&mut store, &instance, &descriptor, m, resource, &[], 1)
                    {
                        Ok(results) => {
                            let val = results.into_iter().next().unwrap_or(Val::Bool(false));
                            print_named_val(prop, &val, 0);
                        }
                        Err(e) => println!("{} <error: {}>", prop, e),
                    }
                }
            }
            "tree" => {
                let roots = store.data().find_roots();
                for r in roots {
                    store.data().print_tree(r);
                }
            }
            "gc" => {
                println!(
                    "[host] gc-dump in the REPL is not wired (the GC walker \
                     keeps its own type-name map keyed off the original \
                     component bytes). Run `yel-host gc-dump <file>` \
                     separately to see the typed heap snapshot."
                );
            }
            "trace" => match rest {
                "on" => {
                    store.data_mut().trace = true;
                    println!("[host] trace=on");
                }
                "off" => {
                    store.data_mut().trace = false;
                    println!("[host] trace=off");
                }
                "" => println!("[host] trace={}", store.data().trace),
                _ => println!("[err] usage: `trace on|off`"),
            },
            _ => println!("[err] unknown command `{}`; type `help`", cmd),
        }
    }

    let _ = call_typed_method(
        &mut store,
        &instance,
        &descriptor,
        &format!("[method]{}.unmount", descriptor.resource_name),
        resource,
        &[],
        0,
    );
    println!("[host] unmounted, bye");
    Ok(())
}
