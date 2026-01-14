//! Host runtime for Yel components.
//!
//! Provides dummy/polyfill implementations of the DOM APIs for testing.

use anyhow::{Context, Result};
use clap::Parser;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU32, Ordering};
use wasmtime::component::{Component, HasSelf, Linker, ResourceTable};
use wasmtime::{Config, Engine, Store};
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

wasmtime::component::bindgen!({
    path: "wit",
    world: "counter-ui",
});

// Separate module for nested-ui bindings - reuse DOM types from counter-ui
mod nested_bindings {
    wasmtime::component::bindgen!({
        path: "wit",
        world: "nested-ui",
        with: {
            "yel:ui/dom": crate::yel::ui::dom,
        },
    });
}

// Separate module for temp-converter-ui bindings - reuse DOM types from counter-ui
mod temp_converter_bindings {
    wasmtime::component::bindgen!({
        path: "wit",
        world: "temp-converter-ui",
        with: {
            "yel:ui/dom": crate::yel::ui::dom,
        },
    });
}

/// Simple DOM node for testing
#[derive(Debug, Clone)]
struct DomNode {
    id: u32,
    tag: String,
    attributes: HashMap<String, String>,
    children: Vec<u32>,
    parent: Option<u32>,
}

/// Host state containing WASI context and DOM state
struct HostState {
    wasi: WasiCtx,
    table: ResourceTable,
    nodes: HashMap<u32, DomNode>,
    next_node_id: AtomicU32,
    event_handlers: HashMap<(u32, String), u32>,
}

impl HostState {
    fn new() -> Result<Self> {
        let wasi = WasiCtxBuilder::new()
            .inherit_stdio()
            .build();

        Ok(Self {
            wasi,
            table: ResourceTable::new(),
            nodes: HashMap::new(),
            next_node_id: AtomicU32::new(1),
            event_handlers: HashMap::new(),
        })
    }

    fn alloc_node(&mut self, tag: &str) -> u32 {
        let id = self.next_node_id.fetch_add(1, Ordering::SeqCst);
        self.nodes.insert(id, DomNode {
            id,
            tag: tag.to_string(),
            attributes: HashMap::new(),
            children: Vec::new(),
            parent: None,
        });
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
            // Print node tag and id
            println!("│{}{}[{}] <{}>", prefix, connector, node.id, node.tag);

            // Print attributes
            let attr_prefix = format!("{}{}    ", prefix, child_prefix);
            for (key, value) in &node.attributes {
                if key != "textContent" {
                    println!("│{}@{}: {}", attr_prefix, key, value);
                }
            }

            // Print text content for text nodes
            if node.tag == "#text" || node.tag == "#comment" {
                if let Some(content) = node.attributes.get("textContent") {
                    println!("│{}\"{}\"", attr_prefix, content);
                }
            }

            // Print event handlers
            for ((node_id, event), handler_id) in &self.event_handlers {
                if *node_id == id {
                    println!("│{}on {} => handler_{}", attr_prefix, event, handler_id);
                }
            }

            // Print children
            let children = &node.children;
            for (i, &child_id) in children.iter().enumerate() {
                let is_last_child = i == children.len() - 1;
                self.print_node(child_id, &format!("{}{}", prefix, child_prefix), is_last_child);
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

// Note: counter_callbacks interface was removed from the world

// Helper to convert AttributeValue variant to String
fn attribute_value_to_string(value: &yel::ui::dom::AttributeValue) -> String {
    use yel::ui::dom::AttributeValue;
    match value {
        AttributeValue::Str(s) => s.clone(),
        AttributeValue::Bool(b) => if *b { "true".to_string() } else { "false".to_string() },
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
    }
}

// Implement the DOM interface
impl yel::ui::dom::Host for HostState {
    fn create_element(&mut self, tag: String) -> u32 {
        let id = self.alloc_node(&tag);
        println!("[DOM] create_element({:?}) -> {}", tag, id);
        id
    }

    fn create_text(&mut self, content: String) -> u32 {
        let id = self.alloc_node("#text");
        if let Some(node) = self.nodes.get_mut(&id) {
            node.attributes.insert("textContent".to_string(), content.clone());
        }
        println!("[DOM] create_text({:?}) -> {}", content, id);
        id
    }

    fn create_comment(&mut self, content: String) -> u32 {
        let id = self.alloc_node("#comment");
        if let Some(node) = self.nodes.get_mut(&id) {
            node.attributes.insert("textContent".to_string(), content.clone());
        }
        println!("[DOM] create_comment({:?}) -> {}", content, id);
        id
    }

    fn set_attribute(&mut self, node: u32, name: String, value: yel::ui::dom::AttributeValue) {
        let str_value = attribute_value_to_string(&value);
        println!("[DOM] set_attribute({}, {:?}, {:?})", node, name, str_value);
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert(name, str_value);
        }
    }

    fn remove_attribute(&mut self, node: u32, name: String) {
        println!("[DOM] remove_attribute({}, {:?})", node, name);
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.remove(&name);
        }
    }

    fn set_text_content(&mut self, node: u32, content: String) {
        println!("[DOM] set_text_content({}, {:?})", node, content);
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert("textContent".to_string(), content);
        }
    }

    fn set_style(&mut self, node: u32, property: String, value: String) {
        println!("[DOM] set_style({}, {:?}, {:?})", node, property, value);
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert(format!("style.{}", property), value);
        }
    }

    fn set_class(&mut self, node: u32, class_name: String) {
        println!("[DOM] set_class({}, {:?})", node, class_name);
        if let Some(n) = self.nodes.get_mut(&node) {
            n.attributes.insert("class".to_string(), class_name);
        }
    }

    fn append_child(&mut self, parent: u32, child: u32) {
        println!("[DOM] append_child({}, {})", parent, child);
        if let Some(p) = self.nodes.get_mut(&parent) {
            p.children.push(child);
        }
        if let Some(c) = self.nodes.get_mut(&child) {
            c.parent = Some(parent);
        }
    }

    fn insert_before(&mut self, parent: u32, node: u32, reference: u32) {
        println!("[DOM] insert_before({}, {}, {})", parent, node, reference);
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
        println!("[DOM] insert_after({}, {}, {})", parent, node, anchor);
        if let Some(p) = self.nodes.get_mut(&parent) {
            if anchor != 0 {
                if let Some(pos) = p.children.iter().position(|&x| x == anchor) {
                    p.children.insert(pos + 1, node);
                } else {
                    p.children.push(node);
                }
            } else {
                // anchor == 0 means insert at beginning
                p.children.insert(0, node);
            }
        }
        if let Some(n) = self.nodes.get_mut(&node) {
            n.parent = Some(parent);
        }
    }

    fn remove_child(&mut self, parent: u32, child: u32) {
        println!("[DOM] remove_child({}, {})", parent, child);
        if let Some(p) = self.nodes.get_mut(&parent) {
            p.children.retain(|&x| x != child);
        }
        if let Some(c) = self.nodes.get_mut(&child) {
            c.parent = None;
        }
    }

    fn remove(&mut self, node: u32) {
        println!("[DOM] remove({})", node);
        if let Some(n) = self.nodes.remove(&node) {
            if let Some(parent_id) = n.parent {
                if let Some(p) = self.nodes.get_mut(&parent_id) {
                    p.children.retain(|&x| x != node);
                }
            }
        }
    }

    fn get_parent(&mut self, node: u32) -> u32 {
        let result = self.nodes.get(&node).and_then(|n| n.parent).unwrap_or(0);
        println!("[DOM] get_parent({}) -> {}", node, result);
        result
    }

    fn get_next_sibling(&mut self, node: u32) -> u32 {
        let result = self.nodes.get(&node).and_then(|n| {
            n.parent.and_then(|parent_id| {
                self.nodes.get(&parent_id).and_then(|p| {
                    p.children.iter().position(|&x| x == node).and_then(|pos| {
                        p.children.get(pos + 1).copied()
                    })
                })
            })
        }).unwrap_or(0);
        println!("[DOM] get_next_sibling({}) -> {}", node, result);
        result
    }

    fn add_event_listener(&mut self, node: u32, event: String, handler_id: u32) {
        println!("[DOM] add_event_listener({}, {:?}, {})", node, event, handler_id);
        self.event_handlers.insert((node, event), handler_id);
    }

    fn remove_event_listener(&mut self, node: u32, event: String, _handler_id: u32) {
        println!("[DOM] remove_event_listener({}, {:?}, {})", node, event, _handler_id);
        self.event_handlers.remove(&(node, event));
    }
}

// DOM interface implementations for nested_bindings and temp_converter_bindings
// are not needed since they reuse yel::ui::dom via the `with` option in bindgen!

#[derive(Parser)]
#[command(name = "yel-run")]
#[command(about = "Run Yel components with a dummy DOM implementation")]
struct Args {
    /// Path to the WASM component file
    #[arg(required = true)]
    component: PathBuf,

    /// Root node ID to mount the component to
    #[arg(short, long, default_value = "0")]
    root: u32,
}

fn main() -> Result<()> {
    let args = Args::parse();

    println!("Loading component: {:?}", args.component);

    // Configure wasmtime engine
    let mut config = Config::new();
    config.wasm_component_model(true);

    let engine = Engine::new(&config)?;

    // Load the component
    let component = Component::from_file(&engine, &args.component)
        .with_context(|| format!("Failed to load component from {:?}", args.component))?;

    // Try counter-ui world first
    if let Ok(()) = run_counter_ui(&engine, &component, args.root) {
        return Ok(());
    }

    // Try nested-ui world
    if let Ok(()) = run_nested_ui(&engine, &component, args.root) {
        return Ok(());
    }

    // Try temp-converter-ui world
    if let Ok(()) = run_temp_converter_ui(&engine, &component, args.root) {
        return Ok(());
    }

    anyhow::bail!("Component doesn't match any known world (counter-ui, nested-ui, or temp-converter-ui)")
}

fn run_counter_ui(engine: &Engine, component: &Component, root: u32) -> Result<()> {
    // Create linker and add WASI + our implementations
    let mut linker = Linker::new(engine);

    // Add WASI
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;

    // Add our DOM implementation
    CounterUi::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |state| state)?;

    // Create store with our state
    let mut store = Store::new(engine, HostState::new()?);

    // Instantiate the component
    let instance = CounterUi::instantiate(&mut store, component, &linker)
        .context("Failed to instantiate counter-ui component")?;

    println!("\n=== Counter-UI Component instantiated ===\n");

    // Get the counter-component interface
    let counter_component = instance.yel_ui_counter_component();

    // Create a new Counter resource using the constructor
    let counter = counter_component.counter().call_constructor(&mut store)?;
    println!("Created Counter resource");

    // Mount the component
    println!("\n=== Mounting component to root {} ===\n", root);
    counter_component.counter().call_mount(&mut store, counter, root)?;

    // Print the DOM tree
    let roots = store.data().find_roots();
    for r in roots {
        store.data().print_tree(r);
    }

    // Get initial values (checkerboard has rows/cols)
    let rows = counter_component.counter().call_get_rows(&mut store, counter)?;
    let cols = counter_component.counter().call_get_cols(&mut store, counter)?;
    println!("\n=== Component state ===");
    println!("  rows: {:?}", rows);
    println!("  cols: {:?}", cols);

    // Unmount
    println!("\n=== Unmounting component ===\n");
    counter_component.counter().call_unmount(&mut store, counter)?;

    println!("\n=== Done ===");

    Ok(())
}

fn run_nested_ui(engine: &Engine, component: &Component, root: u32) -> Result<()> {
    use nested_bindings::NestedUi;

    // Create linker and add WASI + our implementations
    let mut linker = Linker::new(engine);

    // Add WASI
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;

    // Add our DOM implementation for nested bindings
    NestedUi::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |state| state)?;

    // Create store with our state
    let mut store = Store::new(engine, HostState::new()?);

    // Instantiate the component
    let instance = NestedUi::instantiate(&mut store, component, &linker)
        .context("Failed to instantiate nested-ui component")?;

    println!("\n=== Nested-UI Component instantiated ===\n");

    // Get the nested-parent-component interface
    let nested_component = instance.yel_ui_nested_parent_component();

    // Create a new NestedParent resource using the constructor
    let nested_parent = nested_component.nested_parent().call_constructor(&mut store)?;
    println!("Created NestedParent resource");

    // Mount the component
    println!("\n=== Mounting component to root {} ===\n", root);
    nested_component.nested_parent().call_mount(&mut store, nested_parent, root)?;

    // Print the DOM tree
    let roots = store.data().find_roots();
    for r in roots {
        store.data().print_tree(r);
    }

    // Get initial items
    let items = nested_component.nested_parent().call_get_items(&mut store, nested_parent)?;
    println!("\n=== Component state ===");
    println!("  items: {:?}", items);

    // Unmount
    println!("\n=== Unmounting component ===\n");
    nested_component.nested_parent().call_unmount(&mut store, nested_parent)?;

    println!("\n=== Done ===");

    Ok(())
}

fn run_temp_converter_ui(engine: &Engine, component: &Component, root: u32) -> Result<()> {
    use temp_converter_bindings::TempConverterUi;

    // Create linker and add WASI + our implementations
    let mut linker = Linker::new(engine);

    // Add WASI
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;

    // Add our DOM implementation for temp converter bindings
    TempConverterUi::add_to_linker::<HostState, HasSelf<HostState>>(&mut linker, |state| state)?;

    // Create store with our state
    let mut store = Store::new(engine, HostState::new()?);

    // Instantiate the component
    let instance = TempConverterUi::instantiate(&mut store, component, &linker)
        .context("Failed to instantiate temp-converter-ui component")?;

    println!("\n=== Temp-Converter-UI Component instantiated ===\n");

    // Get the temp-converter-component interface
    let temp_converter_component = instance.yel_ui_temp_converter_component();

    // Create a new TempConverter resource using the constructor
    let temp_converter = temp_converter_component.temp_converter().call_constructor(&mut store)?;
    println!("Created TempConverter resource");

    // Mount the component
    println!("\n=== Mounting component to root {} ===\n", root);
    temp_converter_component.temp_converter().call_mount(&mut store, temp_converter, root)?;

    // Print the DOM tree
    let roots = store.data().find_roots();
    for r in roots {
        store.data().print_tree(r);
    }

    // Get initial values
    let celsius = temp_converter_component.temp_converter().call_get_celsius(&mut store, temp_converter)?;
    let fahrenheit = temp_converter_component.temp_converter().call_get_fahrenheit(&mut store, temp_converter)?;
    println!("\n=== Component state ===");
    println!("  celsius: {}", celsius);
    println!("  fahrenheit: {}", fahrenheit);

    // Unmount
    println!("\n=== Unmounting component ===\n");
    temp_converter_component.temp_converter().call_unmount(&mut store, temp_converter)?;

    println!("\n=== Done ===");

    Ok(())
}
