//! Generic Yel component host.
//!
//! Loads any compiled Yel component, wires the static `yel:ui/dom@0.1.0`
//! host surface, dynamically stubs every other import (per-component
//! callbacks etc.), discovers the exported `*-component@*` interface
//! and its resource at runtime, and exposes mount / unmount / property
//! access via subcommands.

use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU32, Ordering};
use wasmparser::HeapType as PHeapType;
use wasmparser::{
    CompositeInnerType, KnownCustom, Name, Parser as WasmParser, Payload, RefType, StorageType,
    SubType, ValType,
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
        for l in self.render_tree(root) {
            println!("{}", l);
        }
    }

    /// Line-producing form of [`print_tree`] (for the TUI / capture).
    fn render_tree(&self, root: u32) -> Vec<String> {
        let mut out = Vec::new();
        out.push("┌─ DOM Tree ─────────────────────────────────────".to_string());
        self.render_node(root, "", true, &mut out);
        out.push("└────────────────────────────────────────────────".to_string());
        out
    }

    fn render_node(&self, id: u32, prefix: &str, is_last: bool, out: &mut Vec<String>) {
        let connector = if is_last { "└── " } else { "├── " };
        let child_prefix = if is_last { "    " } else { "│   " };
        if let Some(node) = self.nodes.get(&id) {
            out.push(format!("│{}{}[{}] <{}>", prefix, connector, node.id, node.tag));
            let attr_prefix = format!("{}{}    ", prefix, child_prefix);
            for (key, value) in &node.attributes {
                if key != "textContent" {
                    out.push(format!("│{}@{}: {}", attr_prefix, key, value));
                }
            }
            if node.tag == "#text" || node.tag == "#comment" {
                if let Some(content) = node.attributes.get("textContent") {
                    out.push(format!("│{}\"{}\"", attr_prefix, content));
                }
            }
            for ((node_id, event), handler_id) in &self.event_handlers {
                if *node_id == id {
                    out.push(format!(
                        "│{}on {} => handler_{}",
                        attr_prefix, event, handler_id
                    ));
                }
            }
            for (i, &child_id) in node.children.iter().enumerate() {
                let is_last_child = i == node.children.len() - 1;
                self.render_node(
                    child_id,
                    &format!("{}{}", prefix, child_prefix),
                    is_last_child,
                    out,
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
    for l in inspect_lines(component, engine) {
        println!("{}", l);
    }
    Ok(())
}

/// Line-producing form of [`cmd_inspect`] — the component's import/export
/// tree. Shared by the `inspect` subcommand and the TUI Inspect panel.
fn inspect_lines(component: &Component, engine: &Engine) -> Vec<String> {
    let ty = component.component_type();
    let mut out = Vec::new();
    out.push("=== Imports ===".to_string());
    for (name, item) in ty.imports(engine) {
        push_item(engine, name, &item, 0, &mut out);
    }
    out.push(String::new());
    out.push("=== Exports ===".to_string());
    for (name, item) in ty.exports(engine) {
        push_item(engine, name, &item, 0, &mut out);
    }
    out
}

fn push_item(engine: &Engine, name: &str, item: &ComponentItem, depth: usize, out: &mut Vec<String>) {
    let pad = "  ".repeat(depth);
    match item {
        ComponentItem::ComponentFunc(_) => out.push(format!("{}- fn   {}", pad, name)),
        ComponentItem::CoreFunc(_) => out.push(format!("{}- core {}", pad, name)),
        ComponentItem::Module(_) => out.push(format!("{}- mod  {}", pad, name)),
        ComponentItem::Component(_) => out.push(format!("{}- comp {}", pad, name)),
        ComponentItem::ComponentInstance(inst) => {
            out.push(format!("{}- iface {}", pad, name));
            for (sub_name, sub_item) in inst.exports(engine) {
                push_item(engine, sub_name, &sub_item, depth + 1, out);
            }
        }
        ComponentItem::Type(_) => out.push(format!("{}- type {}", pad, name)),
        ComponentItem::Resource(_) => out.push(format!("{}- res  {}", pad, name)),
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
    /// Print the component's imports / exports tree to stdout. For the
    /// interactive explorer (expand/collapse, signatures, search), use the
    /// `tui` subcommand's Inspect tab.
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
    /// Launch the full-screen ratatui shell: load / unload / reload
    /// components into a live session and inspect them across tabbed
    /// panels — signals (State), the DOM tree, the typed GC heap,
    /// registered handlers (Enter = fire/click), and the import/export
    /// tree (Inspect) — plus a `:` command line for set / get / fire /
    /// gc. The component argument is optional; load one with `:load`.
    Tui {
        component: Option<PathBuf>,
        #[arg(short, long, default_value = "0")]
        root: u32,
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
            let comp = Component::from_file(&engine, &component)
                .map_err(|e| anyhow!("failed to load {:?}: {}", component, e))?;
            cmd_inspect(&comp, &engine)
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
        Cmd::Tui { component, root } => cmd_tui(&engine, component, root),
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

    for payload in WasmParser::new(0).parse_all(&bytes).flatten() {
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

    println!();
    for l in gc_walk_lines(&mut store, &instance, &descriptor, max_depth, &names)? {
        println!("{}", l);
    }
    Ok(())
}

/// Drill into the component's underlying core instance, locate the
/// `<resource>-registry` Wasm-GC global, and walk the typed heap it roots,
/// returning the rendered tree as lines.
///
/// Operates on a **live** `store`/`instance` (already constructed + mounted),
/// so the TUI session can call it directly without re-instantiating — this
/// is what wires `gc` into the interactive session that the plain REPL
/// could not reach.
fn gc_walk_lines(
    store: &mut Store<HostState>,
    instance: &Instance,
    descriptor: &ComponentDescriptor,
    max_depth: usize,
    names: &HashMap<String, TypeDebug>,
) -> Result<Vec<String>> {
    let mut out = Vec::new();

    // Drill into the underlying core instance via the patch. A
    // component instantiates several core modules (jco-style adapter
    // shims wrap the user core), so scan all of them and pick the one
    // that exports the registry global we expect.
    let registry_name = format!("{}-registry", descriptor.resource_name);
    let mut core_opt: Option<wasmtime::Instance> = None;
    for idx in 0..32u32 {
        let inst = match instance.core_instance(&mut *store, idx) {
            Some(i) => i,
            None => break,
        };
        if inst.get_global(&mut *store, &registry_name).is_some() {
            core_opt = Some(inst);
            break;
        }
    }
    let core = core_opt.ok_or_else(|| {
        anyhow!(
            "no core instance exports a global named {:?} — did yel-codegen \
             emit it? did you re-build the .wasm after upgrading codegen?",
            registry_name
        )
    })?;

    let global = match core.get_global(&mut *store, &registry_name) {
        Some(g) => g,
        None => {
            // Diagnostic: enumerate all exports the core instance does
            // expose so the user can see what's reachable.
            out.push(format!(
                "note: core instance has no global {:?}; visible core exports:",
                registry_name
            ));
            let exports: Vec<(String, &'static str)> = core
                .exports(&mut *store)
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
                out.push(format!("    {:6} {}", kind, name));
            }
            return Err(anyhow!(
                "core instance has no global named {:?}",
                registry_name
            ));
        }
    };

    out.push("┌─ GC Heap ──────────────────────────────────────".to_string());
    out.push(format!("│ entry: ${}", registry_name));

    // Use a single root scope for the whole walk so every Rooted<> we
    // create gets unrooted on drop — keeps the GC heap reclaimable.
    let mut scope = RootScope::new(&mut *store);

    let registry_val = global.get(&mut scope);
    let registry_anyref = match registry_val {
        CoreVal::AnyRef(Some(r)) => r,
        CoreVal::AnyRef(None) => {
            out.push("│  (registry is null)".to_string());
            out.push("└────────────────────────────────────────────────".to_string());
            return Ok(out);
        }
        other => {
            out.push(format!("│  (unexpected registry type: {:?})", other));
            out.push("└────────────────────────────────────────────────".to_string());
            return Ok(out);
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
        names,
        &mut out,
    )?;
    out.push("└────────────────────────────────────────────────".to_string());
    Ok(out)
}

// ----------------------------------------------------------------------------
// GC heap → interactive tree (INode arena) for the TUI's GC Heap tab.
//
// Mirrors the line-based walk above, but materialises the typed heap as
// expandable `INode`s so the TUI can navigate / collapse it like the Inspect
// tab. Operates on a live (constructed + mounted) store/instance.
// ----------------------------------------------------------------------------

/// Push a non-expandable leaf node and return its arena index.
fn gc_push_leaf(arena: &mut Vec<INode>, depth: usize, label: String) -> usize {
    let index = arena.len();
    arena.push(INode {
        depth,
        search: label.to_lowercase(),
        detail: vec![label.clone()],
        label,
        children: Vec::new(),
        parent: None,
        spans: Vec::new(),
        type_col: String::new(),
        expandable: false,
        expanded: false,
    });
    index
}

/// Stable hash of an anyref handle, for cycle detection (matches the
/// line-based walker's scheme).
fn gc_anyref_key(anyref: &Rooted<AnyRef>) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    format!("{:?}", anyref).hash(&mut hasher);
    hasher.finish()
}

/// Walk the live typed Wasm-GC heap into an `INode` arena. Returns a single
/// root node (the component's `<resource>-registry` global) whose subtree is
/// the reachable heap, depth-bounded by `max_depth`.
fn gc_build_tree(
    store: &mut Store<HostState>,
    instance: &Instance,
    descriptor: &ComponentDescriptor,
    max_depth: usize,
    names: &HashMap<String, TypeDebug>,
) -> Result<Vec<INode>> {
    let registry_name = format!("{}-registry", descriptor.resource_name);

    // Find the core instance exporting the registry global.
    let mut core_option: Option<wasmtime::Instance> = None;
    for runtime_index in 0..32u32 {
        let candidate = match instance.core_instance(&mut *store, runtime_index) {
            Some(instance) => instance,
            None => break,
        };
        if candidate.get_global(&mut *store, &registry_name).is_some() {
            core_option = Some(candidate);
            break;
        }
    }
    let core = core_option.ok_or_else(|| {
        anyhow!(
            "no core instance exports a global named {:?} — did yel-codegen \
             emit it? did you re-build the .wasm after upgrading codegen?",
            registry_name
        )
    })?;
    let global = core.get_global(&mut *store, &registry_name).ok_or_else(|| {
        anyhow!("core instance has no global named {:?}", registry_name)
    })?;

    let mut arena: Vec<INode> = Vec::new();
    let root = arena.len();
    arena.push(INode {
        depth: 0,
        label: format!("${} (registry)", registry_name),
        detail: vec![format!("GC heap root — global ${}", registry_name)],
        search: registry_name.to_lowercase(),
        children: Vec::new(),
        parent: None,
        spans: Vec::new(),
        type_col: String::new(),
        expandable: true,
        expanded: true,
    });

    let mut scope = RootScope::new(&mut *store);
    let registry_value = global.get(&mut scope);
    let registry_anyref = match registry_value {
        CoreVal::AnyRef(Some(reference)) => reference,
        CoreVal::AnyRef(None) => {
            arena[root].label = format!("${} (registry is null)", registry_name);
            arena[root].expandable = false;
            arena[root].expanded = false;
            return Ok(arena);
        }
        other => {
            arena[root].label = format!("${} (unexpected type: {:?})", registry_name, other);
            arena[root].expandable = false;
            arena[root].expanded = false;
            return Ok(arena);
        }
    };

    let mut visited: HashMap<u64, u32> = HashMap::new();
    let mut next_id: u32 = 0;
    let child = gc_node_from_anyref(
        &mut scope,
        &registry_anyref,
        1,
        None,
        max_depth,
        &mut visited,
        &mut next_id,
        names,
        &mut arena,
    )?;
    arena[child].parent = Some(root);
    arena[root].children = vec![child];
    Ok(arena)
}

#[allow(clippy::too_many_arguments)]
fn gc_node_from_anyref(
    scope: &mut RootScope<&mut Store<HostState>>,
    anyref: &Rooted<AnyRef>,
    depth: usize,
    field_label: Option<&str>,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    names: &HashMap<String, TypeDebug>,
    arena: &mut Vec<INode>,
) -> Result<usize> {
    let prefix = field_label.map(|s| format!("{} ", s)).unwrap_or_default();

    if max_depth == 0 {
        return Ok(gc_push_leaf(arena, depth, format!("{}<…max-depth>", prefix)));
    }
    let key = gc_anyref_key(anyref);
    if let Some(&id) = visited.get(&key) {
        return Ok(gc_push_leaf(arena, depth, format!("{}<seen #{}>", prefix, id)));
    }
    let my_id = *next_id;
    *next_id += 1;
    visited.insert(key, my_id);

    if let Ok(Some(struct_ref)) = anyref.as_struct(&scope) {
        return gc_node_from_struct(
            scope,
            &struct_ref,
            depth,
            field_label,
            my_id,
            max_depth - 1,
            visited,
            next_id,
            names,
            arena,
        );
    }
    if let Ok(Some(array_ref)) = anyref.as_array(&scope) {
        return gc_node_from_array(
            scope,
            &array_ref,
            depth,
            field_label,
            my_id,
            max_depth - 1,
            visited,
            next_id,
            names,
            arena,
        );
    }
    Ok(gc_push_leaf(arena, depth, format!("{}#{} opaque anyref", prefix, my_id)))
}

#[allow(clippy::too_many_arguments)]
fn gc_node_from_struct(
    scope: &mut RootScope<&mut Store<HostState>>,
    struct_ref: &Rooted<StructRef>,
    depth: usize,
    field_label: Option<&str>,
    my_id: u32,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    names: &HashMap<String, TypeDebug>,
    arena: &mut Vec<INode>,
) -> Result<usize> {
    let ty = struct_ref.ty(&scope).map_err(|e| anyhow!("struct.ty: {}", e))?;
    let field_count = ty.fields().len();
    let fingerprint = fingerprint_runtime_struct(&ty);
    let debug = names.get(&fingerprint);
    let type_name = debug
        .map(|d| format!("${}", d.name))
        .unwrap_or_else(|| "<unnamed-struct>".into());
    // The type name (e.g. `$item_record`) goes in the Type column; the tree
    // cell keeps the field label + id + field count.
    let prefix = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    let meta = format!("#{} ({} fields)", my_id, field_count);
    let label = format!("{}{}", prefix, meta);
    let search = format!("{}{} {}", prefix, type_name, meta).to_lowercase();
    let detail = vec![
        format!("struct {}", type_name),
        format!("{} field(s)", field_count),
    ];
    let spans = vec![
        Span::raw(prefix.clone()),
        Span::styled(meta, Style::default().fg(COLOR_TYPE)),
    ];

    let index = arena.len();
    arena.push(INode {
        depth,
        search,
        detail,
        label,
        children: Vec::new(),
        parent: None,
        spans,
        type_col: type_name,
        expandable: field_count > 0,
        expanded: true,
    });

    let mut children = Vec::new();
    for field_index in 0..field_count {
        let value = struct_ref
            .field(&mut *scope, field_index)
            .map_err(|e| anyhow!("struct.field({}): {}", field_index, e))?;
        let field_name = debug
            .and_then(|d| d.field_names.get(field_index))
            .and_then(|opt| opt.as_deref())
            .map(|n| format!("${}", n))
            .unwrap_or_else(|| format!("$field{}", field_index));
        let child = gc_node_from_coreval(
            scope,
            &value,
            depth + 1,
            Some(&field_name),
            max_depth,
            visited,
            next_id,
            names,
            arena,
        )?;
        arena[child].parent = Some(index);
        children.push(child);
    }
    arena[index].children = children;
    Ok(index)
}

#[allow(clippy::too_many_arguments)]
fn gc_node_from_array(
    scope: &mut RootScope<&mut Store<HostState>>,
    array_ref: &Rooted<ArrayRef>,
    depth: usize,
    field_label: Option<&str>,
    my_id: u32,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    names: &HashMap<String, TypeDebug>,
    arena: &mut Vec<INode>,
) -> Result<usize> {
    let length = array_ref.len(&scope).map_err(|e| anyhow!("array.len: {}", e))?;
    let ty = array_ref.ty(&scope).map_err(|e| anyhow!("array.ty: {}", e))?;
    let fingerprint = fingerprint_runtime_array(&ty);
    let name = names
        .get(&fingerprint)
        .map(|d| format!("${}", d.name))
        .unwrap_or_else(|| "<unnamed-array>".into());
    // The array type name (e.g. `$item_list`) goes in the Type column.
    let prefix = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    let meta = format!("#{} (len={})", my_id, length);
    let label = format!("{}{}", prefix, meta);
    let search = format!("{}{} {}", prefix, name, meta).to_lowercase();
    let detail = vec![format!("array {}", name), format!("len = {}", length)];
    let spans = vec![
        Span::raw(prefix.clone()),
        Span::styled(meta, Style::default().fg(COLOR_TYPE)),
    ];

    let index = arena.len();
    arena.push(INode {
        depth,
        search,
        detail,
        label,
        children: Vec::new(),
        parent: None,
        spans,
        type_col: name,
        expandable: length > 0,
        expanded: true,
    });

    let cap = std::cmp::min(length, 64);
    let mut children = Vec::new();
    for element_index in 0..cap {
        let value = array_ref
            .get(&mut *scope, element_index)
            .map_err(|e| anyhow!("array.get({}): {}", element_index, e))?;
        let element_label = format!("[{}]", element_index);
        let child = gc_node_from_coreval(
            scope,
            &value,
            depth + 1,
            Some(&element_label),
            max_depth,
            visited,
            next_id,
            names,
            arena,
        )?;
        arena[child].parent = Some(index);
        children.push(child);
    }
    if length > cap {
        let more = gc_push_leaf(arena, depth + 1, format!("…{} more elements", length - cap));
        arena[more].parent = Some(index);
        children.push(more);
    }
    arena[index].children = children;
    Ok(index)
}

#[allow(clippy::too_many_arguments)]
fn gc_node_from_coreval(
    scope: &mut RootScope<&mut Store<HostState>>,
    value: &CoreVal,
    depth: usize,
    field_label: Option<&str>,
    max_depth: usize,
    visited: &mut HashMap<u64, u32>,
    next_id: &mut u32,
    names: &HashMap<String, TypeDebug>,
    arena: &mut Vec<INode>,
) -> Result<usize> {
    let prefix = field_label.map(|s| format!("{} ", s)).unwrap_or_default();

    // A typed scalar leaf: `<prefix><value>`, value coloured, type in its column.
    let typed = |arena: &mut Vec<INode>, value_str: String, ty: &str, color: Color| -> usize {
        let index = gc_push_leaf(arena, depth, format!("{}{} : {}", prefix, value_str, ty));
        arena[index].spans = vec![
            Span::raw(prefix.clone()),
            Span::styled(value_str, Style::default().fg(color)),
        ];
        arena[index].type_col = ty.to_string();
        index
    };
    // A dim/atom leaf (`<funcref>`, `null`, …).
    let atom = |arena: &mut Vec<INode>, text: &str| -> usize {
        let index = gc_push_leaf(arena, depth, format!("{}{}", prefix, text));
        arena[index].spans = vec![
            Span::raw(prefix.clone()),
            Span::styled(text.to_string(), Style::default().fg(COLOR_NULLISH)),
        ];
        index
    };

    let index = match value {
        CoreVal::I32(v) => typed(arena, v.to_string(), "i32", COLOR_NUMBER),
        CoreVal::I64(v) => typed(arena, v.to_string(), "i64", COLOR_NUMBER),
        CoreVal::F32(v) => typed(arena, f32::from_bits(*v).to_string(), "f32", COLOR_NUMBER),
        CoreVal::F64(v) => typed(arena, f64::from_bits(*v).to_string(), "f64", COLOR_NUMBER),
        CoreVal::V128(v) => typed(arena, format!("{:?}", v), "v128", COLOR_NUMBER),
        CoreVal::FuncRef(_) => atom(arena, "<funcref>"),
        CoreVal::ExternRef(None) => atom(arena, "null externref"),
        CoreVal::ExternRef(Some(_)) => atom(arena, "<externref>"),
        CoreVal::AnyRef(None) => atom(arena, "null"),
        CoreVal::AnyRef(Some(reference)) => gc_node_from_anyref(
            scope,
            reference,
            depth,
            field_label,
            max_depth,
            visited,
            next_id,
            names,
            arena,
        )?,
        CoreVal::ExnRef(_) => atom(arena, "<exnref>"),
        CoreVal::ContRef(_) => atom(arena, "<contref>"),
    };
    Ok(index)
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

#[allow(clippy::too_many_arguments)]
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
    out: &mut Vec<String>,
) -> Result<()> {
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();

    if max_depth == 0 {
        out.push(format!("│{}{}{}<…max-depth>", prefix, connector(is_last), lab));
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
        out.push(format!(
            "│{}{}{}<seen #{}>",
            prefix,
            connector(is_last),
            lab,
            id
        ));
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
            out,
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
            out,
        );
    }
    out.push(format!(
        "│{}{}{}#{} opaque anyref ({:?})",
        prefix,
        connector(is_last),
        lab,
        my_id,
        anyref
    ));
    Ok(())
}

#[allow(clippy::too_many_arguments)]
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
    out: &mut Vec<String>,
) -> Result<()> {
    let ty = sr.ty(&store).map_err(|e| anyhow!("struct.ty: {}", e))?;
    let n = ty.fields().len();
    let fp = fingerprint_runtime_struct(&ty);
    let dbg = names.get(&fp);
    let type_name = dbg
        .map(|d| format!("${}", d.name))
        .unwrap_or_else(|| "<unnamed-struct>".into());
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    out.push(format!(
        "│{}{}{}{} #{} ({} fields)",
        prefix,
        connector(is_last),
        lab,
        type_name,
        my_id,
        n
    ));
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
            out,
        )?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
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
    out: &mut Vec<String>,
) -> Result<()> {
    let len = ar.len(&store).map_err(|e| anyhow!("array.len: {}", e))?;
    let ty = ar.ty(&store).map_err(|e| anyhow!("array.ty: {}", e))?;
    let fp = fingerprint_runtime_array(&ty);
    let name = match names.get(&fp) {
        Some(d) => format!("${}", d.name),
        None => "<unnamed-array>".into(),
    };
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    out.push(format!(
        "│{}{}{}{} #{} (len={})",
        prefix,
        connector(is_last),
        lab,
        name,
        my_id,
        len
    ));
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
            out,
        )?;
    }
    if extra {
        out.push(format!(
            "│{}{}…{} more elements",
            new_prefix,
            connector(true),
            len - cap
        ));
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
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
    out: &mut Vec<String>,
) -> Result<()> {
    let lab = field_label.map(|s| format!("{} ", s)).unwrap_or_default();
    let mut leaf = |s: String| {
        out.push(format!("│{}{}{}{}", prefix, connector(is_last), lab, s));
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
            out,
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
    for l in named_val_lines(name, val, depth) {
        println!("{}", l);
    }
}

/// Line-producing form of [`print_named_val`] for the TUI / capture.
fn named_val_lines(name: &str, val: &Val, depth: usize) -> Vec<String> {
    let mut out = Vec::new();
    push_named_val(name, val, depth, &mut out);
    out
}

fn push_named_val(name: &str, val: &Val, depth: usize, out: &mut Vec<String>) {
    let pad = "  ".repeat(depth);
    let inline = fmt_inline(val);
    if pad.len() + name.len() + 2 + inline.len() <= PRETTY_INLINE_WIDTH {
        out.push(format!("{}{}: {}", pad, name, inline));
    } else {
        out.push(format!("{}{}:", pad, name));
        push_val(val, depth + 1, out);
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

fn push_val(val: &Val, depth: usize, out: &mut Vec<String>) {
    let pad = "  ".repeat(depth);

    // Try inline first. Fits in budget → one line, done.
    let inline = fmt_inline(val);
    if pad.len() + inline.len() <= PRETTY_INLINE_WIDTH {
        out.push(format!("{}{}", pad, inline));
        return;
    }

    // Expanded form: aggregate values get multi-line layout, scalars
    // never reach here (they always fit inline).
    match val {
        Val::List(items) => {
            out.push(format!("{}[", pad));
            for (i, it) in items.iter().enumerate() {
                let item_inline = fmt_inline(it);
                let comma = if i + 1 < items.len() { "," } else { "" };
                if pad.len() + 2 + item_inline.len() + comma.len() <= PRETTY_INLINE_WIDTH {
                    out.push(format!("{}  {}{}", pad, item_inline, comma));
                } else {
                    push_val(it, depth + 1, out);
                    if !comma.is_empty() {
                        // trailing comma sits on its own line for clarity
                        out.push(format!("{}  ,", pad));
                    }
                }
            }
            out.push(format!("{}]", pad));
        }
        Val::Record(fields) => {
            out.push(format!("{}{{", pad));
            for (i, (name, fv)) in fields.iter().enumerate() {
                let v_inline = fmt_inline(fv);
                let comma = if i + 1 < fields.len() { "," } else { "" };
                let head = format!("  {}: ", name);
                if pad.len() + head.len() + v_inline.len() + comma.len() <= PRETTY_INLINE_WIDTH {
                    out.push(format!("{}{}{}{}", pad, head, v_inline, comma));
                } else {
                    out.push(format!("{}{}", pad, head.trim_end()));
                    push_val(fv, depth + 2, out);
                    if !comma.is_empty() {
                        out.push(format!("{}  ,", pad));
                    }
                }
            }
            out.push(format!("{}}}", pad));
        }
        Val::Tuple(elems) => {
            out.push(format!("{}(", pad));
            for (i, e) in elems.iter().enumerate() {
                let comma = if i + 1 < elems.len() { "," } else { "" };
                let inline_e = fmt_inline(e);
                if pad.len() + 2 + inline_e.len() + comma.len() <= PRETTY_INLINE_WIDTH {
                    out.push(format!("{}  {}{}", pad, inline_e, comma));
                } else {
                    push_val(e, depth + 1, out);
                }
            }
            out.push(format!("{})", pad));
        }
        Val::Variant(name, Some(p)) => {
            out.push(format!("{}{}(", pad, name));
            push_val(p, depth + 1, out);
            out.push(format!("{})", pad));
        }
        Val::Option(Some(inner)) => {
            out.push(format!("{}some(", pad));
            push_val(inner, depth + 1, out);
            out.push(format!("{})", pad));
        }
        Val::Result(Ok(Some(inner))) => {
            out.push(format!("{}ok(", pad));
            push_val(inner, depth + 1, out);
            out.push(format!("{})", pad));
        }
        Val::Result(Err(Some(inner))) => {
            out.push(format!("{}err(", pad));
            push_val(inner, depth + 1, out);
            out.push(format!("{})", pad));
        }
        Val::Map(entries) => {
            out.push(format!("{}{{", pad));
            for (i, (k, v)) in entries.iter().enumerate() {
                let comma = if i + 1 < entries.len() { "," } else { "" };
                out.push(format!("{}  {} ->", pad, fmt_inline(k)));
                push_val(v, depth + 2, out);
                if !comma.is_empty() {
                    out.push(format!("{}  ,", pad));
                }
            }
            out.push(format!("{}}}", pad));
        }
        // Scalars / atoms never reach here — they always fit inline.
        _ => out.push(format!("{}{}", pad, inline)),
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

// ============================================================================
// Subcommand: tui — interactive ratatui shell over a live component session
// ============================================================================

use crossterm::event::{self, Event, KeyCode, KeyEventKind};
use ratatui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{
    Block, BorderType, Borders, Cell, Clear, List, ListItem, ListState, Paragraph, Row, Table,
    TableState, Tabs, Wrap,
};
use ratatui::Frame;
use std::path::Path;
use tracing::{error, info, warn};
use tui_logger::{TuiLoggerLevelOutput, TuiLoggerWidget, TuiWidgetEvent, TuiWidgetState};

// ----------------------------------------------------------------------------
// Shared TUI theme — one accent colour, one border/title style, one highlight,
// so every panel looks the same.
// ----------------------------------------------------------------------------

/// Accent colour for titles, the selected tab, and list selection.
/// Monochrome: white titles, and a white-on-black inverted selection bar.
const ACCENT: Color = Color::White;

/// A bordered panel with the shared rounded border + accent title.
fn panel(title: &str) -> Block<'static> {
    Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(Color::DarkGray))
        .title(Span::styled(
            format!(" {title} "),
            Style::default().fg(ACCENT).add_modifier(Modifier::BOLD),
        ))
}

/// The highlight style for the selected row in any list/tree.
fn selected_style() -> Style {
    Style::default()
        .bg(ACCENT)
        .fg(Color::Black)
        .add_modifier(Modifier::BOLD)
}

// Literal-value syntax colours (named terminal tokens).
const COLOR_NUMBER: Color = Color::Cyan; // numbers, bools (constants)
const COLOR_STRING: Color = Color::LightBlue; // strings / chars
const COLOR_BOOL: Color = Color::Cyan; // constants
const COLOR_VARIANT: Color = Color::LightMagenta; // enum / variant / option / result cases
const COLOR_TAG: Color = Color::LightGreen; // DOM tags
const COLOR_NULLISH: Color = Color::DarkGray; // none / empty / null
const COLOR_TYPE: Color = Color::DarkGray; // type/size summaries (list[2], : i32)

/// Colour for a component-model literal value, by type (GitHub palette).
fn value_color(val: &Val) -> Color {
    match val {
        Val::Bool(_) => COLOR_BOOL,
        Val::S8(_) | Val::S16(_) | Val::S32(_) | Val::S64(_) => COLOR_NUMBER,
        Val::U8(_) | Val::U16(_) | Val::U32(_) | Val::U64(_) => COLOR_NUMBER,
        Val::Float32(_) | Val::Float64(_) => COLOR_NUMBER,
        Val::Char(_) | Val::String(_) => COLOR_STRING,
        Val::Enum(_) | Val::Variant(_, None) | Val::Flags(_) => COLOR_VARIANT,
        Val::Option(None) | Val::Result(Ok(None)) | Val::Result(Err(None)) => COLOR_NULLISH,
        _ => Color::Gray,
    }
}

/// A live, mounted component session: owns the wasmtime store/instance and
/// drives the resource lifecycle. Every TUI panel reads and acts through it,
/// so load → mount → set/fire/gc all run against one persistent instance
/// (unlike the per-subcommand CLI paths that re-instantiate each time).
struct Session {
    path: PathBuf,
    root: u32,
    component: Component,
    descriptor: ComponentDescriptor,
    store: Store<HostState>,
    instance: Instance,
    resource: ResourceAny,
    type_names: HashMap<String, TypeDebug>,
    mounted: bool,
}

impl Session {
    /// Load a component from disk, instantiate it, run its constructor and
    /// mount it at `root`. The DOM host stays silent (trace off) so guest
    /// `[DOM] …` prints don't corrupt the alternate-screen TUI.
    fn load(engine: &Engine, path: &Path, root: u32) -> Result<Session> {
        let component = Component::from_file(engine, path)
            .map_err(|e| anyhow!("failed to load {:?}: {}", path, e))?;
        let descriptor = describe_component(engine, &component)?;

        let mut linker = Linker::new(engine);
        wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
        GenericHost::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |s| s)?;
        register_dynamic_imports(engine, &component, &mut linker)?;

        let mut store = Store::new(engine, HostState::new(false)?);
        let instance = linker
            .instantiate(&mut store, &component)
            .map_err(|e| anyhow!("instantiate: {}", e))?;

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

        let type_names = build_type_name_map(path);
        Ok(Session {
            path: path.to_path_buf(),
            root,
            component,
            descriptor,
            store,
            instance,
            resource,
            type_names,
            mounted: true,
        })
    }

    /// Best-effort unmount — ignored if the instance is already poisoned.
    fn unmount(&mut self) {
        if self.mounted {
            let _ = call_typed_method(
                &mut self.store,
                &self.instance,
                &self.descriptor,
                &format!("[method]{}.unmount", self.descriptor.resource_name),
                self.resource,
                &[],
                0,
            );
            self.mounted = false;
        }
    }

    /// A single getter, expanded (for the `get` command).
    fn get_lines(&mut self, prop: &str) -> Result<Vec<String>> {
        let getter = format!("[method]{}.get-{}", self.descriptor.resource_name, prop);
        if !self.descriptor.methods.iter().any(|m| m == &getter) {
            return Err(anyhow!("no getter for {:?}", prop));
        }
        let results = call_typed_method(
            &mut self.store,
            &self.instance,
            &self.descriptor,
            &getter,
            self.resource,
            &[],
            1,
        )?;
        let val = results.into_iter().next().unwrap_or(Val::Bool(false));
        Ok(named_val_lines(prop, &val, 0))
    }

    fn set(&mut self, spec: &str) -> Result<()> {
        apply_set(
            &mut self.store,
            &self.instance,
            &self.descriptor,
            self.resource,
            spec,
        )
    }

    /// Recorded `(node, event) → handler_id`, sorted for stable display.
    fn handlers(&self) -> Vec<(u32, String, u32)> {
        let mut v: Vec<(u32, String, u32)> = self
            .store
            .data()
            .event_handlers
            .iter()
            .map(|((n, ev), h)| (*n, ev.clone(), *h))
            .collect();
        v.sort_by(|a, b| (a.0, a.1.as_str()).cmp(&(b.0, b.1.as_str())));
        v
    }

    fn fire(&mut self, handler_id: u32) -> Result<()> {
        fire_handler(&mut self.store, &self.instance, handler_id)
    }

    /// Resolve a `node:event` spec to its registered handler id.
    fn handler_for(&self, node: u32, event: &str) -> Option<u32> {
        self.store
            .data()
            .event_handlers
            .get(&(node, event.to_string()))
            .copied()
    }

    fn header(&self) -> String {
        format!(
            "{} · root={} · {} · {}",
            self.path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_else(|| self.path.display().to_string()),
            self.root,
            if self.mounted { "mounted" } else { "unmounted" },
            self.descriptor.iface_name,
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tab {
    State,
    Dom,
    Gc,
    Handlers,
    Inspect,
    Log,
}

impl Tab {
    const ALL: [Tab; 6] = [
        Tab::State,
        Tab::Dom,
        Tab::Gc,
        Tab::Handlers,
        Tab::Inspect,
        Tab::Log,
    ];
    fn title(self) -> &'static str {
        match self {
            Tab::State => "State",
            Tab::Dom => "DOM",
            Tab::Gc => "GC Heap",
            Tab::Handlers => "Handlers",
            Tab::Inspect => "Inspect",
            Tab::Log => "Log",
        }
    }
    fn index(self) -> usize {
        Tab::ALL.iter().position(|t| *t == self).unwrap()
    }
}

enum Mode {
    Normal,
    Command,
    /// Editing the Inspect tab's search filter.
    TreeFilter,
}

struct App {
    engine: Engine,
    session: Option<Session>,
    tab: Tab,
    /// Interactive signal inspector for the State tab (snapshot at last build).
    state: Option<TreeState>,
    /// Interactive DOM "Elements" tree for the DOM tab (snapshot at last build).
    dom: Option<TreeState>,
    /// Interactive imports/exports explorer for the Inspect tab.
    inspect: Option<TreeState>,
    /// Interactive typed heap tree for the GC Heap tab (snapshot at last build).
    gc: Option<TreeState>,
    /// Last GC-walk error (e.g. patch not applied), shown when `gc` is empty.
    gc_error: Option<String>,
    handler_state: ListState,
    mode: Mode,
    cmd: String,
    /// Scroll/filter state for the `tui-logger` Log panel.
    log_state: TuiWidgetState,
    /// Whether the help overlay is shown (toggled with `?`).
    show_help: bool,
    default_root: u32,
    gc_depth: usize,
    quit: bool,
}

impl App {
    fn new(engine: Engine, default_root: u32) -> Self {
        let mut handler_state = ListState::default();
        handler_state.select(Some(0));
        App {
            engine,
            session: None,
            tab: Tab::State,
            state: None,
            dom: None,
            inspect: None,
            gc: None,
            gc_error: None,
            handler_state,
            mode: Mode::Normal,
            cmd: String::new(),
            log_state: TuiWidgetState::new()
                .set_default_display_level(tui_logger::LevelFilter::Info),
            show_help: false,
            default_root,
            gc_depth: 8,
            quit: false,
        }
    }

    /// (Re)build the Inspect explorer from the loaded component, or clear it
    /// when nothing is loaded. Called on load/reload/unload.
    fn rebuild_inspect(&mut self) {
        self.inspect = self
            .session
            .as_ref()
            .map(|s| TreeState::inspect(&s.component, &self.engine));
    }

    /// The tree currently being navigated, if the active tab is a tree tab.
    fn active_tree_mut(&mut self) -> Option<&mut TreeState> {
        match self.tab {
            Tab::State => self.state.as_mut(),
            Tab::Dom => self.dom.as_mut(),
            Tab::Inspect => self.inspect.as_mut(),
            Tab::Gc => self.gc.as_mut(),
            _ => None,
        }
    }

    /// (Re)build the State inspector by reading every signal from the live
    /// session. Rebuilt on entry / `r` / after `set`/`fire`.
    fn rebuild_state(&mut self) {
        self.state = self.session.as_mut().map(build_state_tree).map(TreeState::from_arena);
    }

    /// (Re)build the DOM "Elements" tree from the host's in-memory DOM.
    fn rebuild_dom(&mut self) {
        self.dom = self
            .session
            .as_ref()
            .map(|s| TreeState::from_arena(build_dom_tree(s.store.data())));
    }

    /// (Re)build the GC heap tree by walking the live session's typed heap.
    fn rebuild_gc(&mut self) {
        let depth = self.gc_depth;
        match self.session.as_mut() {
            Some(s) => match gc_build_tree(&mut s.store, &s.instance, &s.descriptor, depth, &s.type_names) {
                Ok(arena) => {
                    self.gc = Some(TreeState::from_arena(arena));
                    self.gc_error = None;
                }
                Err(e) => {
                    self.gc = None;
                    self.gc_error = Some(e.to_string());
                }
            },
            None => {
                self.gc = None;
                self.gc_error = None;
            }
        }
    }

    /// Recompute the cached `view` for the active text panel. Panels that
    /// call into wasmtime (State/GC) only run here — never per-keystroke —
    /// so scrolling stays free.
    fn refresh(&mut self) {
        match self.tab {
            // Each tab owns an interactive widget; rebuild the live ones from
            // the session. State/DOM/GC re-read the heap; Inspect is built once.
            Tab::State => self.rebuild_state(),
            Tab::Dom => self.rebuild_dom(),
            Tab::Gc => self.rebuild_gc(),
            Tab::Inspect => {
                if self.inspect.is_none() && self.session.is_some() {
                    self.rebuild_inspect();
                }
            }
            // Handlers (interactive list) and Log (tui-logger) need no rebuild.
            Tab::Handlers | Tab::Log => {}
        }
    }

    fn switch(&mut self, tab: Tab) {
        self.tab = tab;
        self.refresh();
    }

    fn handler_count(&self) -> usize {
        self.session.as_ref().map(|s| s.handlers().len()).unwrap_or(0)
    }

    /// Execute a `:` command line.
    fn exec(&mut self, line: &str) {
        let line = line.trim();
        if line.is_empty() {
            return;
        }
        let (cmd, rest) = match line.split_once(char::is_whitespace) {
            Some((c, r)) => (c, r.trim()),
            None => (line, ""),
        };
        match cmd {
            "load" => {
                let mut parts = rest.split_whitespace();
                let path = match parts.next() {
                    Some(p) => p.to_string(),
                    None => {
                        warn!("usage: load <path> [root]");
                        return;
                    }
                };
                let root = parts
                    .next()
                    .and_then(|r| r.parse::<u32>().ok())
                    .unwrap_or(self.default_root);
                if let Some(s) = self.session.as_mut() {
                    s.unmount();
                }
                match Session::load(&self.engine, Path::new(&path), root) {
                    Ok(s) => {
                        info!("loaded {} (root={})", path, root);
                        self.session = Some(s);
                        self.rebuild_inspect();
                        self.refresh();
                    }
                    Err(e) => error!("load failed: {}", e),
                }
            }
            "unload" => {
                if let Some(mut s) = self.session.take() {
                    s.unmount();
                    info!("unloaded {}", s.path.display());
                } else {
                    warn!("nothing loaded");
                }
                self.state = None;
                self.dom = None;
                self.inspect = None;
                self.gc = None;
                self.gc_error = None;
                self.refresh();
            }
            "reload" => match self.session.as_ref().map(|s| (s.path.clone(), s.root)) {
                Some((path, root)) => {
                    if let Some(s) = self.session.as_mut() {
                        s.unmount();
                    }
                    match Session::load(&self.engine, &path, root) {
                        Ok(s) => {
                            info!("reloaded {}", path.display());
                            self.session = Some(s);
                            self.rebuild_inspect();
                            self.refresh();
                        }
                        Err(e) => error!("reload failed: {}", e),
                    }
                }
                None => warn!("nothing loaded to reload"),
            },
            "set" => match self.session.as_mut() {
                Some(s) => match s.set(rest) {
                    Ok(()) => {
                        info!("set {}", rest);
                        self.refresh();
                    }
                    Err(e) => error!("set failed: {}", e),
                },
                None => warn!("no component loaded"),
            },
            "get" => match self.session.as_mut() {
                Some(s) => match s.get_lines(rest) {
                    Ok(lines) => info!("get {} = {}", rest, lines.join(" ")),
                    Err(e) => error!("{}", e),
                },
                None => warn!("no component loaded"),
            },
            "fire" => self.exec_fire(rest),
            "gc" => {
                if let Ok(d) = rest.parse::<usize>() {
                    self.gc_depth = d;
                }
                self.switch(Tab::Gc);
            }
            "depth" => match rest.parse::<usize>() {
                Ok(d) => {
                    self.gc_depth = d;
                    info!("gc depth = {}", d);
                    if self.tab == Tab::Gc {
                        self.refresh();
                    }
                }
                Err(_) => warn!("usage: depth <n>"),
            },
            "root" => match rest.parse::<u32>() {
                Ok(r) => {
                    self.default_root = r;
                    info!("default root = {} (applies to next load)", r);
                }
                Err(_) => warn!("usage: root <n>"),
            },
            "inspect" => self.switch(Tab::Inspect),
            "state" => self.switch(Tab::State),
            "tree" | "dom" => self.switch(Tab::Dom),
            "handlers" => self.switch(Tab::Handlers),
            "help" | "?" => self.show_help = true,
            "quit" | "q" | "exit" => self.quit = true,
            other => warn!("unknown command `{}` (try `help`)", other),
        }
    }

    /// `fire <node>:<event>` or `fire #<handler_id>` — dispatches a recorded
    /// handler (this is the "click").
    fn exec_fire(&mut self, rest: &str) {
        if self.session.is_none() {
            warn!("no component loaded");
            return;
        }
        // Resolve the handler id under a short immutable borrow, returning
        // owned data so the mutable `fire` call doesn't overlap.
        let resolved: std::result::Result<u32, String> = if let Some(idx) = rest.strip_prefix('#') {
            idx.trim()
                .parse::<u32>()
                .map_err(|_| "usage: fire #<handler_id>".to_string())
        } else if let Some((node_s, event)) = rest.split_once(':') {
            match node_s.trim().parse::<u32>() {
                Ok(node) => {
                    let event = event.trim();
                    self.session
                        .as_ref()
                        .unwrap()
                        .handler_for(node, event)
                        .ok_or_else(|| format!("no handler for ({}, {:?})", node, event))
                }
                Err(_) => Err(format!("invalid node id `{}`", node_s)),
            }
        } else {
            Err("usage: fire <node>:<event> or fire #<id>".to_string())
        };

        match resolved {
            Ok(hid) => {
                let res = self.session.as_mut().unwrap().fire(hid);
                match res {
                    Ok(()) => {
                        info!("fired handler #{}", hid);
                        self.refresh();
                    }
                    Err(e) => error!("fire failed: {}", e),
                }
            }
            Err(msg) => warn!("{}", msg),
        }
    }

    /// Fire the handler currently selected in the Handlers panel.
    fn fire_selected(&mut self) {
        let sel = self.handler_state.selected().unwrap_or(0);
        let entry = self
            .session
            .as_ref()
            .and_then(|s| s.handlers().into_iter().nth(sel));
        match entry {
            Some((node, event, hid)) => {
                if let Some(s) = self.session.as_mut() {
                    match s.fire(hid) {
                        Ok(()) => info!("fired {}:{} (#{})", node, event, hid),
                        Err(e) => error!("fire failed: {}", e),
                    }
                }
            }
            None => warn!("no handler selected"),
        }
    }
}

fn tui_help() -> String {
    "\
keys:
  1-6 / Tab     switch panel (State, DOM, GC, Handlers, Inspect, Log)
  j/k ↑/↓       scroll / move handler or tree selection
  g / G         jump to top / bottom
  PgUp/PgDn     scroll a page
  Enter         (Handlers) fire selected handler = click;
                (Inspect) expand/collapse the selected interface
  r             refresh the active panel
  :             enter a command
  ? q           help / quit
Inspect panel:
  l/h           expand / collapse the selected interface
  e / E         expand all / collapse all
  /             search (whole tree, auto-expands matches)
  n / N         jump to next / previous match
commands:
  load <path> [root]   instantiate + mount a component
  unload               unmount and drop the session
  reload               re-instantiate the current component
  set <prop>=<wave>    apply a setter (WAVE syntax, e.g. set count=5)
  get <prop>           read one signal into the log
  fire <node>:<event>  dispatch a handler (also: fire #<id>)
  gc [depth]           walk the typed GC heap
  depth <n>            set GC walk depth
  root <n>             default mount root for the next load
  inspect              show the import/export tree
  quit                 leave"
        .to_string()
}

fn ui(f: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(1),
            Constraint::Length(3),
        ])
        .split(f.area());

    // --- Top: tab bar; session status is the block title. ---
    let header = match app.session.as_ref() {
        Some(s) => format!("yel-host · {}", s.header()),
        None => "yel-host · no component — :load <path>".to_string(),
    };
    let titles: Vec<Line> = Tab::ALL
        .iter()
        .enumerate()
        .map(|(i, t)| Line::from(format!(" {} {} ", i + 1, t.title())))
        .collect();
    let tabs = Tabs::new(titles)
        .select(app.tab.index())
        .block(panel(&header))
        .style(Style::default().fg(Color::Gray))
        .highlight_style(selected_style())
        .divider(Span::styled("·", Style::default().fg(Color::DarkGray)));
    f.render_widget(tabs, chunks[0]);

    // --- Middle: the active panel. ---
    match app.tab {
        Tab::Handlers => render_handlers(f, app, chunks[1]),
        Tab::State => match app.state.as_mut() {
            Some(tree) => render_tree(f, tree, chunks[1], "State · signals"),
            None => render_placeholder(f, chunks[1], "State", "no component loaded — :load <path>"),
        },
        Tab::Dom => match app.dom.as_mut() {
            Some(tree) => render_tree(f, tree, chunks[1], "DOM · elements"),
            None => render_placeholder(f, chunks[1], "DOM", "no component loaded — :load <path>"),
        },
        Tab::Inspect => match app.inspect.as_mut() {
            Some(tree) => render_tree(f, tree, chunks[1], "Inspect"),
            None => render_placeholder(f, chunks[1], "Inspect", "no component loaded — :load <path>"),
        },
        Tab::Gc => match app.gc.as_mut() {
            Some(tree) => render_tree(f, tree, chunks[1], "GC Heap"),
            None => {
                let msg = match &app.gc_error {
                    Some(e) => format!(
                        "GC walk failed: {e}\n\nThe GC heap needs `patches/apply.sh` applied."
                    ),
                    None => "no component loaded — :load <path>".to_string(),
                };
                render_placeholder(f, chunks[1], "GC Heap", &msg);
            }
        },
        Tab::Log => render_log(f, app, chunks[1]),
    }

    // --- Bottom: mode-aware command / key-hint line. ---
    let bottom = match app.mode {
        Mode::Command => Paragraph::new(Line::from(format!(":{}", app.cmd)))
            .block(panel("command · Enter run · Esc cancel")),
        Mode::TreeFilter => {
            let filter = match app.tab {
                Tab::State => app.state.as_ref(),
                Tab::Dom => app.dom.as_ref(),
                Tab::Gc => app.gc.as_ref(),
                _ => app.inspect.as_ref(),
            }
            .map(|s| s.filter.as_str())
            .unwrap_or("");
            Paragraph::new(Line::from(format!("/{}", filter)))
                .block(panel("search · Enter next · Esc cancel"))
        }
        Mode::Normal => {
            let hint = match app.tab {
                Tab::State | Tab::Dom | Tab::Inspect | Tab::Gc => {
                    "l/h expand · e/E all · / search · n/N next · Tab panel · : cmd · q quit"
                }
                Tab::Handlers => "j/k select · Enter fire · Tab panel · : cmd · ? help · q quit",
                Tab::Log => "↑/↓ select · PgUp/PgDn page · Esc tail · Tab panel · ? help · q quit",
            };
            Paragraph::new(Line::from(hint))
                .style(Style::default().fg(Color::Gray))
                .block(panel("keys"))
        }
    };
    f.render_widget(bottom, chunks[2]);

    // --- Help overlay (toggled with `?`). ---
    if app.show_help {
        render_help_overlay(f);
    }
}

/// Render the Log tab using the `tui-logger` widget (levels, colours, scrollback).
fn render_log(f: &mut Frame, app: &App, area: Rect) {
    let widget = TuiLoggerWidget::default()
        .block(panel("Log"))
        .output_separator(' ')
        .output_timestamp(Some("%H:%M:%S".to_string()))
        .output_level(Some(TuiLoggerLevelOutput::Abbreviated))
        .output_target(false)
        .output_file(false)
        .output_line(false)
        .style_error(Style::default().fg(Color::Red))
        .style_warn(Style::default().fg(Color::Yellow))
        .style_info(Style::default().fg(Color::Green))
        .style_debug(Style::default().fg(Color::Gray))
        .style_trace(Style::default().fg(Color::DarkGray))
        .state(&app.log_state);
    f.render_widget(widget, area);
}

/// Centred modal help overlay listing every key and command.
fn render_help_overlay(f: &mut Frame) {
    let area = f.area();
    let w = area.width.saturating_sub(8).min(76);
    let h = area.height.saturating_sub(4).min(26);
    let x = area.x + (area.width.saturating_sub(w)) / 2;
    let y = area.y + (area.height.saturating_sub(h)) / 2;
    let popup = Rect::new(x, y, w, h);
    f.render_widget(Clear, popup);
    let para = Paragraph::new(tui_help())
        .block(panel("help · any key to close"))
        .wrap(Wrap { trim: false });
    f.render_widget(para, popup);
}

/// A bordered panel showing a dimmed message (empty/error states).
fn render_placeholder(f: &mut Frame, area: ratatui::layout::Rect, title: &str, message: &str) {
    let para = Paragraph::new(message.to_string())
        .block(panel(title))
        .style(Style::default().fg(Color::DarkGray))
        .wrap(Wrap { trim: false });
    f.render_widget(para, area);
}

fn render_handlers(f: &mut Frame, app: &mut App, area: ratatui::layout::Rect) {
    let items: Vec<ListItem> = match app.session.as_ref() {
        Some(s) => {
            let hs = s.handlers();
            if hs.is_empty() {
                vec![ListItem::new("(no handlers registered)")]
            } else {
                hs.iter()
                    .map(|(n, ev, h)| {
                        ListItem::new(format!("node {:>4}  on {:<10}  → handler #{}", n, ev, h))
                    })
                    .collect()
            }
        }
        None => vec![ListItem::new("(no component loaded)")],
    };
    let list = List::new(items)
        .block(panel("Handlers"))
        .highlight_style(selected_style());
    f.render_stateful_widget(list, area, &mut app.handler_state);
}

fn handle_normal(app: &mut App, code: KeyCode) {
    // The help overlay swallows the next keypress to dismiss itself.
    if app.show_help {
        app.show_help = false;
        return;
    }

    // Tab-agnostic global keys.
    match code {
        KeyCode::Char('q') => {
            app.quit = true;
            return;
        }
        KeyCode::Char(':') => {
            app.mode = Mode::Command;
            app.cmd.clear();
            return;
        }
        KeyCode::Char('?') => {
            app.show_help = true;
            return;
        }
        KeyCode::Char('r') => {
            app.refresh();
            return;
        }
        KeyCode::Tab => {
            let next = (app.tab.index() + 1) % Tab::ALL.len();
            app.switch(Tab::ALL[next]);
            return;
        }
        KeyCode::BackTab => {
            let n = Tab::ALL.len();
            let prev = (app.tab.index() + n - 1) % n;
            app.switch(Tab::ALL[prev]);
            return;
        }
        KeyCode::Char(c @ '1'..='6') => {
            let idx = (c as u8 - b'1') as usize;
            app.switch(Tab::ALL[idx]);
            return;
        }
        _ => {}
    }

    // State, DOM, Inspect and GC Heap are interactive trees; route nav there.
    if matches!(app.tab, Tab::State | Tab::Dom | Tab::Inspect | Tab::Gc) {
        handle_tree_key(app, code);
        return;
    }

    // Log: drive the tui-logger scrollback state.
    if app.tab == Tab::Log {
        let event = match code {
            KeyCode::Char('j') | KeyCode::Down => Some(TuiWidgetEvent::DownKey),
            KeyCode::Char('k') | KeyCode::Up => Some(TuiWidgetEvent::UpKey),
            KeyCode::PageDown => Some(TuiWidgetEvent::NextPageKey),
            KeyCode::PageUp => Some(TuiWidgetEvent::PrevPageKey),
            KeyCode::Esc => Some(TuiWidgetEvent::EscapeKey),
            _ => None,
        };
        if let Some(event) = event {
            app.log_state.transition(event);
        }
        return;
    }

    // Remaining tab: Handlers — a selectable list; Enter fires (click).
    let count = app.handler_count();
    match code {
        KeyCode::Char('j') | KeyCode::Down if count > 0 => {
            let cur = app.handler_state.selected().unwrap_or(0);
            app.handler_state.select(Some((cur + 1).min(count - 1)));
        }
        KeyCode::Char('k') | KeyCode::Up => {
            let cur = app.handler_state.selected().unwrap_or(0);
            app.handler_state.select(Some(cur.saturating_sub(1)));
        }
        KeyCode::Char('g') => app.handler_state.select(Some(0)),
        KeyCode::Char('G') => app.handler_state.select(Some(count.saturating_sub(1))),
        KeyCode::Enter => {
            app.fire_selected();
            app.refresh();
        }
        _ => {}
    }
}

/// Navigation keys for a tree tab (Inspect / GC Heap).
fn handle_tree_key(app: &mut App, code: KeyCode) {
    // `/` starts a filter — handled before borrowing the tree so we can flip
    // the App mode without an aliasing conflict.
    if let KeyCode::Char('/') = code {
        if let Some(tree) = app.active_tree_mut() {
            tree.filter.clear();
        }
        app.mode = Mode::TreeFilter;
        return;
    }
    let Some(tree) = app.active_tree_mut() else {
        return;
    };
    match code {
        KeyCode::Char('j') | KeyCode::Down => tree.move_by(1),
        KeyCode::Char('k') | KeyCode::Up => tree.move_by(-1),
        KeyCode::PageDown => tree.move_by(10),
        KeyCode::PageUp => tree.move_by(-10),
        KeyCode::Char('g') => {
            tree.sel = 0;
            tree.table_state.select(Some(0));
        }
        KeyCode::Char('G') => {
            tree.sel = tree.visible.len().saturating_sub(1);
            tree.table_state.select(Some(tree.sel));
        }
        KeyCode::Enter | KeyCode::Char(' ') => tree.toggle(),
        KeyCode::Char('l') | KeyCode::Right => tree.set_expanded(true),
        KeyCode::Char('h') | KeyCode::Left => tree.set_expanded(false),
        KeyCode::Char('e') => tree.expand_all(true),
        KeyCode::Char('E') => tree.expand_all(false),
        KeyCode::Char('n') => tree.jump_match(true),
        KeyCode::Char('N') => tree.jump_match(false),
        _ => {}
    }
}

/// Filter-input keys for a tree tab (`Mode::TreeFilter`).
fn handle_tree_filter(app: &mut App, code: KeyCode) {
    match code {
        KeyCode::Enter => {
            if let Some(tree) = app.active_tree_mut() {
                tree.jump_match(true);
            }
            app.mode = Mode::Normal;
        }
        KeyCode::Esc => {
            if let Some(tree) = app.active_tree_mut() {
                tree.filter.clear();
            }
            app.mode = Mode::Normal;
        }
        KeyCode::Backspace => {
            if let Some(tree) = app.active_tree_mut() {
                tree.filter.pop();
            }
        }
        KeyCode::Char(c) => {
            if let Some(tree) = app.active_tree_mut() {
                tree.filter.push(c);
            }
        }
        _ => {}
    }
}

fn handle_command(app: &mut App, code: KeyCode) {
    match code {
        KeyCode::Enter => {
            let line = std::mem::take(&mut app.cmd);
            app.mode = Mode::Normal;
            app.exec(&line);
        }
        KeyCode::Esc => {
            app.mode = Mode::Normal;
            app.cmd.clear();
        }
        KeyCode::Backspace => {
            app.cmd.pop();
        }
        KeyCode::Char(c) => app.cmd.push(c),
        _ => {}
    }
}

/// Route `tracing` events into the `tui-logger` buffer rendered by the Log tab.
/// Capped at Info so wasmtime's debug/trace spam stays out; the Log panel can
/// still raise the display level interactively.
fn init_tui_logging() {
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::util::SubscriberInitExt;

    let _ = tui_logger::init_logger(tui_logger::LevelFilter::Info);
    tui_logger::set_default_level(tui_logger::LevelFilter::Info);
    let _ = tracing_subscriber::registry()
        .with(tui_logger::tracing_subscriber_layer())
        .try_init();
}

fn cmd_tui(engine: &Engine, initial: Option<PathBuf>, root: u32) -> Result<()> {
    init_tui_logging();

    let mut app = App::new(engine.clone(), root);
    info!("yel-host tui — press ? for help, : for command, q to quit");
    if let Some(path) = initial {
        match Session::load(&app.engine, &path, root) {
            Ok(s) => {
                info!("loaded {} (root={})", path.display(), root);
                app.session = Some(s);
                app.rebuild_inspect();
            }
            Err(e) => error!("load failed: {}", e),
        }
    }
    app.refresh();

    let mut terminal = ratatui::init();
    let result = run_tui_loop(&mut terminal, &mut app);
    ratatui::restore();

    // Clean up the live session outside raw mode.
    if let Some(s) = app.session.as_mut() {
        s.unmount();
    }
    result
}

fn run_tui_loop(terminal: &mut ratatui::DefaultTerminal, app: &mut App) -> Result<()> {
    loop {
        terminal.draw(|f| ui(f, app))?;
        if event::poll(std::time::Duration::from_millis(200))? {
            if let Event::Key(key) = event::read()? {
                if key.kind != KeyEventKind::Press {
                    continue;
                }
                match app.mode {
                    Mode::Normal => handle_normal(app, key.code),
                    Mode::Command => handle_command(app, key.code),
                    Mode::TreeFilter => handle_tree_filter(app, key.code),
                }
            }
        }
        if app.quit {
            break;
        }
    }
    Ok(())
}

// ============================================================================
// `inspect` explorer — interactive imports/exports tree (ratatui)
// ============================================================================

/// Concise, depth-bounded label for a component-model [`wasmtime::component::Type`].
/// Used to render function signatures in the inspect explorer's detail pane.
fn fmt_component_ty(t: &wasmtime::component::Type, depth: usize) -> String {
    use wasmtime::component::Type as T;
    if depth == 0 {
        // Shallow guard against deeply nested / recursive types.
        return match t {
            T::List(_) => "list<…>".into(),
            T::Record(_) => "record {…}".into(),
            T::Tuple(_) => "tuple<…>".into(),
            T::Variant(_) => "variant {…}".into(),
            T::Option(_) => "option<…>".into(),
            T::Result(_) => "result<…>".into(),
            T::Map(_) => "map<…>".into(),
            other => fmt_component_ty(other, 1),
        };
    }
    match t {
        T::Bool => "bool".into(),
        T::S8 => "s8".into(),
        T::U8 => "u8".into(),
        T::S16 => "s16".into(),
        T::U16 => "u16".into(),
        T::S32 => "s32".into(),
        T::U32 => "u32".into(),
        T::S64 => "s64".into(),
        T::U64 => "u64".into(),
        T::Float32 => "float32".into(),
        T::Float64 => "float64".into(),
        T::Char => "char".into(),
        T::String => "string".into(),
        T::List(l) => format!("list<{}>", fmt_component_ty(&l.ty(), depth - 1)),
        T::Map(m) => format!(
            "map<{}, {}>",
            fmt_component_ty(&m.key(), depth - 1),
            fmt_component_ty(&m.value(), depth - 1)
        ),
        T::Record(r) => {
            let fs: Vec<String> = r
                .fields()
                .map(|f| format!("{}: {}", f.name, fmt_component_ty(&f.ty, depth - 1)))
                .collect();
            format!("record {{ {} }}", fs.join(", "))
        }
        T::Tuple(tp) => {
            let ts: Vec<String> = tp
                .types()
                .map(|x| fmt_component_ty(&x, depth - 1))
                .collect();
            format!("tuple<{}>", ts.join(", "))
        }
        T::Variant(v) => {
            let cs: Vec<String> = v
                .cases()
                .map(|c| match c.ty {
                    Some(ty) => format!("{}({})", c.name, fmt_component_ty(&ty, depth - 1)),
                    None => c.name.to_string(),
                })
                .collect();
            format!("variant {{ {} }}", cs.join(", "))
        }
        T::Enum(e) => format!("enum {{ {} }}", e.names().collect::<Vec<_>>().join(", ")),
        T::Option(o) => format!("option<{}>", fmt_component_ty(&o.ty(), depth - 1)),
        T::Result(r) => {
            let ok = r.ok().map(|t| fmt_component_ty(&t, depth - 1));
            let er = r.err().map(|t| fmt_component_ty(&t, depth - 1));
            match (ok, er) {
                (Some(o), Some(e)) => format!("result<{}, {}>", o, e),
                (Some(o), None) => format!("result<{}>", o),
                (None, Some(e)) => format!("result<_, {}>", e),
                (None, None) => "result".into(),
            }
        }
        T::Flags(f) => format!("flags {{ {} }}", f.names().collect::<Vec<_>>().join(", ")),
        T::Own(_) => "own<resource>".into(),
        T::Borrow(_) => "borrow<resource>".into(),
        T::Future(_) => "future".into(),
        T::Stream(_) => "stream".into(),
        T::ErrorContext => "error-context".into(),
    }
}

/// One row in the inspect tree (flattened into an arena; `children` are arena
/// indices so a node can be expanded/collapsed without rebuilding).
struct INode {
    depth: usize,
    label: String,
    /// Styled segments for the tree row (after indent + marker). Empty → render
    /// `label` plain; populated by builders that syntax-colour literal values.
    spans: Vec<Span<'static>>,
    /// Second-column text — the value's type/kind (e.g. `s32`, `list`, `fn`).
    type_col: String,
    detail: Vec<String>,
    search: String,
    children: Vec<usize>,
    parent: Option<usize>,
    expandable: bool,
    expanded: bool,
}

/// Recursively add a `ComponentItem` to the arena, returning its index.
fn add_inspect_item(
    arena: &mut Vec<INode>,
    engine: &Engine,
    name: &str,
    item: &ComponentItem,
    depth: usize,
    parent_path: &str,
) -> usize {
    let idx = arena.len();
    let full = if parent_path.is_empty() {
        name.to_string()
    } else {
        format!("{}/{}", parent_path, name)
    };
    // Reserve the slot; fields are filled in after any children are built.
    arena.push(INode {
        depth,
        label: String::new(),
        detail: Vec::new(),
        search: full.to_lowercase(),
        children: Vec::new(),
        parent: None,
        spans: Vec::new(),
        type_col: String::new(),
        expandable: false,
        expanded: false,
    });

    let (label, detail, children, expandable) = match item {
        ComponentItem::ComponentFunc(f) => {
            let params: Vec<String> = f
                .params()
                .map(|(n, t)| format!("{}: {}", n, fmt_component_ty(&t, 4)))
                .collect();
            let results: Vec<String> = f.results().map(|t| fmt_component_ty(&t, 4)).collect();
            let mut d = vec![format!("ƒ {}", name), String::new(), "params:".to_string()];
            if params.is_empty() {
                d.push("  (none)".to_string());
            } else {
                d.extend(params.iter().map(|p| format!("  {}", p)));
            }
            d.push(String::new());
            d.push("results:".to_string());
            if results.is_empty() {
                d.push("  (none)".to_string());
            } else {
                d.extend(results.iter().map(|r| format!("  {}", r)));
            }
            d.push(String::new());
            d.push(format!("path: {}", full));
            let sig = if results.is_empty() {
                format!("ƒ {}({})", name, params.join(", "))
            } else {
                format!("ƒ {}({}) -> {}", name, params.join(", "), results.join(", "))
            };
            (sig, d, Vec::new(), false)
        }
        ComponentItem::ComponentInstance(inst) => {
            let kids: Vec<(String, ComponentItem)> = inst
                .exports(engine)
                .map(|(n, i)| (n.to_string(), i))
                .collect();
            let mut child_idx = Vec::with_capacity(kids.len());
            for (sub, subitem) in &kids {
                child_idx.push(add_inspect_item(arena, engine, sub, subitem, depth + 1, &full));
            }
            let d = vec![
                format!("interface {}", name),
                String::new(),
                format!("{} member(s)", kids.len()),
                String::new(),
                format!("path: {}", full),
            ];
            (format!("interface {}", name), d, child_idx, true)
        }
        ComponentItem::Resource(_) => (
            format!("resource {}", name),
            vec![format!("resource {}", name), String::new(), format!("path: {}", full)],
            Vec::new(),
            false,
        ),
        ComponentItem::Type(_) => (
            format!("type {}", name),
            vec![format!("type {}", name), String::new(), format!("path: {}", full)],
            Vec::new(),
            false,
        ),
        ComponentItem::CoreFunc(_) => (format!("core-fn {}", name), vec![format!("core fn {}", name)], Vec::new(), false),
        ComponentItem::Module(_) => (format!("module {}", name), vec![format!("module {}", name)], Vec::new(), false),
        ComponentItem::Component(_) => (format!("component {}", name), vec![format!("component {}", name)], Vec::new(), false),
    };

    arena[idx].label = label;
    arena[idx].detail = detail;
    arena[idx].children = children;
    arena[idx].expandable = expandable;
    arena[idx].type_col = match item {
        ComponentItem::ComponentFunc(_) => "fn",
        ComponentItem::ComponentInstance(_) => "iface",
        ComponentItem::Resource(_) => "resource",
        ComponentItem::Type(_) => "type",
        ComponentItem::CoreFunc(_) => "core-fn",
        ComponentItem::Module(_) => "module",
        ComponentItem::Component(_) => "component",
    }
    .to_string();
    let kids = arena[idx].children.clone();
    for ci in kids {
        arena[ci].parent = Some(idx);
    }
    idx
}

/// Build the two-section (Imports / Exports) tree for a component.
fn build_inspect_tree(component: &Component, engine: &Engine) -> Vec<INode> {
    let ty = component.component_type();
    let imports: Vec<(String, ComponentItem)> = ty
        .imports(engine)
        .map(|(n, i)| (n.to_string(), i))
        .collect();
    let exports: Vec<(String, ComponentItem)> = ty
        .exports(engine)
        .map(|(n, i)| (n.to_string(), i))
        .collect();

    let mut arena: Vec<INode> = Vec::new();

    let imports_index = arena.len();
    arena.push(INode {
        depth: 0,
        label: String::new(),
        detail: vec!["Imports".to_string()],
        search: "imports".to_string(),
        children: Vec::new(),
        parent: None,
        spans: Vec::new(),
        type_col: String::new(),
        expandable: true,
        expanded: true,
    });
    let mut import_children = Vec::new();
    for (name, item) in &imports {
        import_children.push(add_inspect_item(&mut arena, engine, name, item, 1, "imports"));
    }
    arena[imports_index].label = format!("Imports ({})", import_children.len());
    for &child_index in &import_children {
        arena[child_index].parent = Some(imports_index);
    }
    arena[imports_index].children = import_children;

    let exports_index = arena.len();
    arena.push(INode {
        depth: 0,
        label: String::new(),
        detail: vec!["Exports".to_string()],
        search: "exports".to_string(),
        children: Vec::new(),
        parent: None,
        spans: Vec::new(),
        type_col: String::new(),
        expandable: true,
        expanded: true,
    });
    let mut export_children = Vec::new();
    for (name, item) in &exports {
        export_children.push(add_inspect_item(&mut arena, engine, name, item, 1, "exports"));
    }
    arena[exports_index].label = format!("Exports ({})", export_children.len());
    for &child_index in &export_children {
        arena[child_index].parent = Some(exports_index);
    }
    arena[exports_index].children = export_children;

    arena
}

// ----------------------------------------------------------------------------
// State inspector — signals as a React-DevTools-style expandable value tree.
// ----------------------------------------------------------------------------

/// Short type word for a component-model value (shown in the detail pane).
fn val_type_name(val: &Val) -> &'static str {
    match val {
        Val::Bool(_) => "bool",
        Val::S8(_) => "s8",
        Val::S16(_) => "s16",
        Val::S32(_) => "s32",
        Val::S64(_) => "s64",
        Val::U8(_) => "u8",
        Val::U16(_) => "u16",
        Val::U32(_) => "u32",
        Val::U64(_) => "u64",
        Val::Float32(_) => "float32",
        Val::Float64(_) => "float64",
        Val::Char(_) => "char",
        Val::String(_) => "string",
        Val::List(_) => "list",
        Val::Record(_) => "record",
        Val::Tuple(_) => "tuple",
        Val::Variant(..) => "variant",
        Val::Enum(_) => "enum",
        Val::Option(_) => "option",
        Val::Result(_) => "result",
        Val::Flags(_) => "flags",
        Val::Map(_) => "map",
        Val::Resource(_) => "resource",
        Val::Future(_) => "future",
        Val::Stream(_) => "stream",
        Val::ErrorContext(_) => "error-context",
    }
}

fn state_detail(name: &str, val: &Val) -> Vec<String> {
    vec![
        format!("name:  {}", name),
        format!("type:  {}", val_type_name(val)),
        String::new(),
        "value:".to_string(),
        fmt_inline(val),
    ]
}

fn push_tree_node(
    arena: &mut Vec<INode>,
    depth: usize,
    label: String,
    detail: Vec<String>,
    expandable: bool,
    expanded: bool,
) -> usize {
    let index = arena.len();
    arena.push(INode {
        depth,
        search: label.to_lowercase(),
        label,
        detail,
        children: Vec::new(),
        parent: None,
        spans: Vec::new(),
        type_col: String::new(),
        expandable,
        expanded,
    });
    index
}

/// Build a node (and subtree) for a named value. Scalars become leaves;
/// aggregates (list / record / tuple / variant / option / result / map) become
/// expandable branches. Top-level signals expand one level by default.
fn build_val_node(arena: &mut Vec<INode>, depth: usize, name: &str, val: &Val) -> usize {
    let detail = state_detail(name, val);

    // `tree_value`: the value shown after `name:` in the tree (None → just the
    // name; the aggregate's `type[size]` summary goes in the Type column).
    // `type_col`: the Type column. `children`: the subtree.
    let (tree_value, type_col, children): (Option<(String, Color)>, String, Vec<(String, Val)>) =
        match val {
            Val::List(items) => (
                None,
                format!("list[{}]", items.len()),
                items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| (format!("[{}]", i), item.clone()))
                    .collect(),
            ),
            Val::Record(fields) => (
                None,
                format!("record {{{}}}", fields.len()),
                fields.iter().map(|(n, v)| (n.clone(), v.clone())).collect(),
            ),
            Val::Tuple(elems) => (
                None,
                format!("tuple({})", elems.len()),
                elems
                    .iter()
                    .enumerate()
                    .map(|(i, e)| (i.to_string(), e.clone()))
                    .collect(),
            ),
            Val::Map(entries) => (
                None,
                format!("map[{}]", entries.len()),
                entries
                    .iter()
                    .map(|(k, v)| (fmt_inline(k), v.clone()))
                    .collect(),
            ),
            // Variant/option/result carry a discriminant (a value) in the tree,
            // with the kind in the Type column.
            Val::Variant(case, Some(payload)) => (
                Some((case.clone(), COLOR_VARIANT)),
                "variant".to_string(),
                vec![(case.clone(), (**payload).clone())],
            ),
            Val::Option(Some(inner)) => (
                Some(("some".to_string(), COLOR_VARIANT)),
                "option".to_string(),
                vec![("value".to_string(), (**inner).clone())],
            ),
            Val::Result(Ok(Some(inner))) => (
                Some(("ok".to_string(), COLOR_VARIANT)),
                "result".to_string(),
                vec![("value".to_string(), (**inner).clone())],
            ),
            Val::Result(Err(Some(inner))) => (
                Some(("err".to_string(), COLOR_VARIANT)),
                "result".to_string(),
                vec![("value".to_string(), (**inner).clone())],
            ),
            // Scalars / atoms (incl. none/empty variants) → coloured literal.
            _ => (
                Some((fmt_inline(val), value_color(val))),
                val_type_name(val).to_string(),
                Vec::new(),
            ),
        };

    let expandable = !children.is_empty();
    let (label, spans) = match &tree_value {
        Some((text, color)) => (
            format!("{}: {}", name, text),
            vec![
                Span::raw(format!("{}: ", name)),
                Span::styled(text.clone(), Style::default().fg(*color)),
            ],
        ),
        None => (name.to_string(), vec![Span::raw(name.to_string())]),
    };
    let index = push_tree_node(arena, depth, label, detail, expandable, expandable && depth == 0);
    arena[index].search = format!(
        "{} {} {}",
        name,
        tree_value.as_ref().map(|(t, _)| t.as_str()).unwrap_or(""),
        type_col
    )
    .to_lowercase();
    arena[index].spans = spans;
    arena[index].type_col = type_col;

    let mut links = Vec::with_capacity(children.len());
    for (child_name, child_val) in &children {
        let child = build_val_node(arena, depth + 1, child_name, child_val);
        arena[child].parent = Some(index);
        links.push(child);
    }
    arena[index].children = links;
    index
}

/// Build the State inspector tree: one root per signal, each calling its getter
/// and expanding the returned value.
fn build_state_tree(session: &mut Session) -> Vec<INode> {
    let prefix = format!("[method]{}.get-", session.descriptor.resource_name);
    let getters: Vec<String> = session
        .descriptor
        .methods
        .iter()
        .filter(|m| m.starts_with(&prefix))
        .cloned()
        .collect();

    let mut arena: Vec<INode> = Vec::new();
    for getter in &getters {
        let prop = getter.trim_start_matches(&prefix).to_string();
        match call_typed_method(
            &mut session.store,
            &session.instance,
            &session.descriptor,
            getter,
            session.resource,
            &[],
            1,
        ) {
            Ok(mut results) => {
                let value = results.drain(..).next().unwrap_or(Val::Bool(false));
                build_val_node(&mut arena, 0, &prop, &value);
            }
            Err(e) => {
                let label = format!("{}: <error: {}>", prop, e);
                push_tree_node(&mut arena, 0, label, vec![format!("getter failed: {}", e)], false, false);
            }
        }
    }
    if arena.is_empty() {
        push_tree_node(&mut arena, 0, "(no signals)".to_string(), Vec::new(), false, false);
    }
    arena
}

// ----------------------------------------------------------------------------
// DOM inspector — the in-memory DOM as an "Elements"-style interactive tree.
// ----------------------------------------------------------------------------

/// Build a node (and subtree) for the DOM node `id`; the detail pane gets its
/// attributes, text, and registered event handlers.
fn build_dom_node(arena: &mut Vec<INode>, host: &HostState, id: u32, depth: usize) -> usize {
    let node = match host.nodes.get(&id) {
        Some(n) => n,
        None => return push_tree_node(arena, depth, format!("<missing #{}>", id), Vec::new(), false, false),
    };

    let text = node.attributes.get("textContent").cloned();
    let label = match &text {
        Some(t) if node.tag == "#text" || node.tag == "#comment" => {
            format!("<{}> #{}  {:?}", node.tag, node.id, t)
        }
        _ => format!("<{}> #{}", node.tag, node.id),
    };

    // Detail pane: tag, id, attributes, text, events.
    let mut detail = vec![format!("<{}>", node.tag), format!("id:  {}", node.id)];
    let mut attrs: Vec<(&String, &String)> = node
        .attributes
        .iter()
        .filter(|(k, _)| k.as_str() != "textContent")
        .collect();
    attrs.sort_by(|a, b| a.0.cmp(b.0));
    if !attrs.is_empty() {
        detail.push(String::new());
        detail.push("attributes:".to_string());
        for (k, v) in attrs {
            detail.push(format!("  {} = {}", k, v));
        }
    }
    if let Some(t) = &text {
        detail.push(String::new());
        detail.push(format!("text: {:?}", t));
    }
    let mut handlers: Vec<(String, u32)> = host
        .event_handlers
        .iter()
        .filter(|((n, _), _)| *n == id)
        .map(|((_, event), handler)| (event.clone(), *handler))
        .collect();
    handlers.sort();
    if !handlers.is_empty() {
        detail.push(String::new());
        detail.push("events:".to_string());
        for (event, handler) in handlers {
            detail.push(format!("  on {} → handler #{}", event, handler));
        }
    }

    let index = push_tree_node(arena, depth, label, detail, !node.children.is_empty(), true);
    // Colour: tag like a GitHub HTML tag, id dim, text content as a string.
    let mut spans = vec![
        Span::styled(format!("<{}>", node.tag), Style::default().fg(COLOR_TAG)),
        Span::styled(format!(" #{}", node.id), Style::default().fg(COLOR_TYPE)),
    ];
    if let Some(t) = &text {
        if node.tag == "#text" || node.tag == "#comment" {
            spans.push(Span::raw("  "));
            spans.push(Span::styled(format!("{:?}", t), Style::default().fg(COLOR_STRING)));
        }
    }
    arena[index].spans = spans;
    arena[index].type_col = match node.tag.as_str() {
        "#text" => "text",
        "#comment" => "comment",
        "yel-frag" => "fragment",
        _ => "element",
    }
    .to_string();

    let mut children = Vec::with_capacity(node.children.len());
    for &child_id in &node.children {
        let child = build_dom_node(arena, host, child_id, depth + 1);
        arena[child].parent = Some(index);
        children.push(child);
    }
    arena[index].children = children;
    index
}

/// Build the DOM "Elements" tree from the host's in-memory DOM.
fn build_dom_tree(host: &HostState) -> Vec<INode> {
    let mut arena: Vec<INode> = Vec::new();
    let mut roots = host.find_roots();
    roots.sort_unstable();
    for root in &roots {
        build_dom_node(&mut arena, host, *root, 0);
    }
    if arena.is_empty() {
        push_tree_node(&mut arena, 0, "(empty DOM)".to_string(), Vec::new(), false, false);
    }
    arena
}

/// A generic interactive expandable tree. Owned by the main TUI `App` and
/// reused for the **State**, **DOM**, **Inspect**, and **GC Heap** tabs;
/// filter-input is driven by `Mode::TreeFilter`.
struct TreeState {
    arena: Vec<INode>,
    roots: Vec<usize>,
    visible: Vec<usize>, // arena indices, in display order
    sel: usize,          // position within `visible`
    table_state: TableState,
    filter: String,
}

impl TreeState {
    /// Build the imports/exports tree for the Inspect tab.
    fn inspect(component: &Component, engine: &Engine) -> Self {
        Self::from_arena(build_inspect_tree(component, engine))
    }

    /// Wrap a pre-built node arena (depth-0 nodes become roots).
    fn from_arena(arena: Vec<INode>) -> Self {
        let roots: Vec<usize> = arena
            .iter()
            .enumerate()
            .filter(|(_, n)| n.depth == 0)
            .map(|(i, _)| i)
            .collect();
        let mut s = TreeState {
            arena,
            roots,
            visible: Vec::new(),
            sel: 0,
            table_state: TableState::default(),
            filter: String::new(),
        };
        s.recompute();
        s.table_state.select(Some(0));
        s
    }

    fn recompute(&mut self) {
        fn walk(arena: &[INode], idx: usize, out: &mut Vec<usize>) {
            out.push(idx);
            if arena[idx].expanded {
                for &c in &arena[idx].children {
                    walk(arena, c, out);
                }
            }
        }
        let mut v = Vec::new();
        for &r in &self.roots {
            walk(&self.arena, r, &mut v);
        }
        self.visible = v;
        if self.sel >= self.visible.len() {
            self.sel = self.visible.len().saturating_sub(1);
        }
        self.table_state.select(Some(self.sel));
    }

    fn sel_node(&self) -> Option<&INode> {
        self.visible.get(self.sel).map(|&i| &self.arena[i])
    }

    fn move_by(&mut self, delta: isize) {
        let n = self.visible.len() as isize;
        if n == 0 {
            return;
        }
        let mut s = self.sel as isize + delta;
        if s < 0 {
            s = 0;
        }
        if s >= n {
            s = n - 1;
        }
        self.sel = s as usize;
        self.table_state.select(Some(self.sel));
    }

    fn toggle(&mut self) {
        if let Some(&idx) = self.visible.get(self.sel) {
            if self.arena[idx].expandable {
                self.arena[idx].expanded = !self.arena[idx].expanded;
                self.recompute();
            }
        }
    }

    fn set_expanded(&mut self, expanded: bool) {
        if let Some(&idx) = self.visible.get(self.sel) {
            if self.arena[idx].expandable && self.arena[idx].expanded != expanded {
                self.arena[idx].expanded = expanded;
                self.recompute();
            }
        }
    }

    fn expand_all(&mut self, expanded: bool) {
        for n in &mut self.arena {
            if n.expandable {
                n.expanded = expanded;
            }
        }
        self.recompute();
    }

    /// Jump to the next node matching the current filter, searching the whole
    /// tree (not just visible rows) and auto-expanding the match's ancestors.
    /// Wraps around; `forward` chooses direction in arena (pre-order) order.
    fn jump_match(&mut self, forward: bool) {
        if self.filter.is_empty() {
            return;
        }
        let needle = self.filter.to_lowercase();
        let n = self.arena.len();
        if n == 0 {
            return;
        }
        let cur = self.visible.get(self.sel).copied().unwrap_or(0);
        for step in 1..=n {
            let idx = if forward {
                (cur + step) % n
            } else {
                (cur + n - step) % n
            };
            if self.arena[idx].search.contains(&needle) {
                // Expand every ancestor so the match becomes visible.
                let mut p = self.arena[idx].parent;
                while let Some(pi) = p {
                    self.arena[pi].expanded = true;
                    p = self.arena[pi].parent;
                }
                self.recompute();
                if let Some(pos) = self.visible.iter().position(|&v| v == idx) {
                    self.sel = pos;
                    self.table_state.select(Some(pos));
                }
                return;
            }
        }
    }
}

/// Render a two-pane explorer (tree + detail) into `area`. Shared by the
/// Inspect and GC Heap tabs; `tree_title` labels the tree pane.
fn render_tree(
    f: &mut Frame,
    tree_state: &mut TreeState,
    area: ratatui::layout::Rect,
    tree_title: &str,
) {
    let panes = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(55), Constraint::Percentage(45)])
        .split(area);

    // Tree pane — a two-column table: the indented tree, plus a Type column.
    let dim = Style::default().fg(Color::DarkGray);
    let rows: Vec<Row> = tree_state
        .visible
        .iter()
        .map(|&index| {
            let node = &tree_state.arena[index];
            let indent = "  ".repeat(node.depth);
            let marker = if node.expandable {
                if node.expanded {
                    "▾ "
                } else {
                    "▸ "
                }
            } else {
                "  "
            };
            let mut spans = vec![Span::raw(indent), Span::styled(marker, dim)];
            if node.spans.is_empty() {
                spans.push(Span::raw(node.label.clone()));
            } else {
                spans.extend(node.spans.iter().cloned());
            }
            let type_cell =
                Cell::from(Line::from(Span::styled(node.type_col.clone(), dim)).alignment(Alignment::Right));
            Row::new(vec![Cell::from(Line::from(spans)), type_cell])
        })
        .collect();
    // Size the Type column to its widest value (clamped), so long GC type
    // names like `$counter-component` aren't truncated.
    let type_width = tree_state
        .visible
        .iter()
        .map(|&i| tree_state.arena[i].type_col.chars().count())
        .max()
        .unwrap_or(4)
        .clamp(4, 28) as u16;
    let widths = [Constraint::Min(0), Constraint::Length(type_width)];
    let header = Row::new(vec![
        Cell::from(Span::styled("tree", dim)),
        Cell::from(Line::from(Span::styled("type", dim)).alignment(Alignment::Right)),
    ]);
    let tree_widget = Table::new(rows, widths)
        .header(header)
        .column_spacing(1)
        .block(panel(tree_title))
        .row_highlight_style(selected_style());
    f.render_stateful_widget(tree_widget, panes[0], &mut tree_state.table_state);

    // Detail pane.
    let detail: Vec<Line> = tree_state
        .sel_node()
        .map(|n| n.detail.iter().map(|l| Line::from(l.clone())).collect())
        .unwrap_or_default();
    let para = Paragraph::new(detail)
        .block(panel("detail"))
        .wrap(Wrap { trim: false });
    f.render_widget(para, panes[1]);
}
