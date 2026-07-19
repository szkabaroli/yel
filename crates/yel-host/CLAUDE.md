# CLAUDE.md — yel-host

## What this crate is

**yel-host** is a **developer host/runtime** for exercising compiled Yel **WebAssembly components** locally. It is **not** a production browser or full DOM implementation.

- **Engine**: Wasmtime with the **component model** enabled.
- **Imports**: Implements **`yel:ui/dom@0.1.0`** (see `wit/component.wit` and generated bindings) as an **in-memory stub**: nodes are numeric IDs in a `HashMap`, tree edges are tracked in-memory, and most operations **`println!`** for visibility.
- **WASI**: Links **Wasmtime WASI preview2** (`wasmtime_wasi::p2::add_to_linker_sync`) so components can use standard WASI like stdio.

The binary **`yel-host`** takes a **`.wasm` component path** and optional **`--root <u32>`** (default `0`) as the mount root node id.

## WIT worlds and instantiation

Host code is driven by **`wasmtime::component::bindgen!`** against `crates/yel-host/wit/`. **`component.wit`** defines multiple **worlds** under `package yel:host`:

| World | Export interface (guest) | Typical demo |
|-------|---------------------------|--------------|
| `counter-ui` | `yel:ui/counter-component@0.1.0` | Checkerboard-style counter |
| `nested-ui` | `yel:ui/nested-parent-component@0.1.0` | Nested lists / parent-child |
| `temp-converter-ui` | `yel:ui/temp-converter-component@0.1.0` | °C / °F bindings |

**`main`** tries **`run_counter_ui` → `run_nested_ui` → `run_temp_converter_ui`** in order. The first world whose **`instantiate`** succeeds wins; if none match, it errors with a message listing the three worlds.

Nested/temp worlds **reuse the same DOM `Host` impl** via bindgen `with: { "yel:ui/dom": crate::yel::ui::dom }` so only one Rust implementation of `yel::ui::dom::Host` is maintained.

## DOM stub semantics (important for debugging)

- **Node IDs**: Monotonic `AtomicU32`; **`0`** is used as **none** where the WIT says navigation returns “no parent/sibling”.
- **Not a real DOM**: No layout, no real events bubbling, no JS. **`add_event_listener`** only records **`(node_id, event) → handler_id`** for the debug tree printer.
- **`create_fragment`**: Allocates a node tagged **`yel-frag`** (placeholder for grouped updates).

When changing guest codegen or host behavior, prefer **matching the WIT** in `wit/` and regenerating bindings conceptually aligned with **`wasmtime component bindgen`** expectations.

## Adding another world / sample component

1. Add a **`world my-world { ... }`** block to **`wit/component.wit`** (import `yel:ui/dom`, export your component interface).
2. Add a **`mod my_bindings { wasmtime::component::bindgen!(...) }`** block mirroring **`nested_bindings`** / **`temp_converter_bindings`**, with **`with: { "yel:ui/dom": crate::yel::ui::dom }`** if you reuse DOM.
3. Implement **`run_my_world`** (linker + instantiate + ctor/mount/unmount + any getters your fixture needs).
4. Call **`run_my_world`** from **`main`** after the existing tries (or reorder if you need a different probe order).

Keep **`HostState`** shared unless you need separate tables per scenario.

## Subcommands (`Cmd` in `main.rs`)

`inspect` (import/export tree) · `run` (mount + print DOM) · `dump` (signals +
DOM, with `--set`) · `gc-dump` (walk the internal Wasm-GC heap; needs
`patches/apply.sh`) · `repl` (line-based lifecycle driver) · **`tui`**
(full-screen ratatui shell).

The **`tui`** subcommand wraps the same machinery in a `Session` (one live,
mounted `Store`/`Instance`/`ResourceAny` + the GC type-name map) with tabbed
panels — **State** (signals), **DOM**, **GC Heap**, **Handlers** (Enter = fire =
"click"), **Inspect**, **Log** — and a `:` command line (`load`/`unload`/
`reload`/`set`/`get`/`fire`/`gc`). It keeps the session **non-tracing** so the
DOM stub's `if self.trace` prints don't corrupt the alternate screen.

**State, DOM, Inspect and GC Heap are interactive trees** — a React-DevTools-style
inspector. All four reuse one generic `TreeState` (an `INode` arena with
parent/children indices: expand/collapse, `/` whole-tree search with ancestor
auto-expand, and a detail pane). Builders all produce the same `INode` shape:
`build_state_tree`/`build_val_node` (signals → expandable values),
`build_dom_tree`/`build_dom_node` (the in-memory DOM as an Elements tree;
detail = attributes/text/events), `build_inspect_tree` (imports/exports with
`fmt_component_ty` signatures), and `gc_build_tree` (the live typed Wasm-GC
heap; needs the `core_instance` patch). Tree nav routes through
`App::active_tree_mut` keyed on the active tab; `Mode::TreeFilter` drives search.
State/DOM/GC rebuild on tab entry / `r` / after `set`/`fire`; Inspect is built
once per load. The shared theme (`panel`, `selected_style`, `ACCENT`) keeps every
panel visually identical, and literal values are syntax-coloured using named
terminal tokens: each `INode` carries optional styled `spans` (built by
`value_color` / the DOM + GC builders) — numbers/bools cyan, strings light-blue,
enum/variant cases magenta, DOM tags green, none/type/size dim (DarkGray).

The **Log** tab is a `tracing` + `tui-logger` panel: `init_tui_logging` installs
a `TuiTracingSubscriberLayer` (capped at Info so wasmtime trace/debug stays out),
and all host actions log via `tracing::{info,warn,error}!` instead of an ad-hoc
buffer — so the panel gets levels, colours, timestamps, and scrollback (driven by
a `TuiWidgetState`). `?` opens a modal help overlay (`render_help_overlay`)
rather than logging into the panel.

The `println!` formatters were split into line-producing variants
(`named_val_lines`/`push_val`, `inspect_lines`, `gc_walk_lines`) shared by the
CLI subcommands; the `gc-dump` CLI keeps its line-based `gc_walk_lines` path
(parallel to the tree builder).

## Dependencies

- **`wasmtime` / `wasmtime-wasi`**: component runtime + WASI.
- **`anyhow`**: CLI errors.
- **`clap`**: argument parsing (see **`Args`** in `main.rs`; the derive still names the command `yel-run` in metadata — align names if it causes confusion).
- **`ratatui` / `crossterm`**: the `tui` subcommand's terminal UI.
- **`tracing` / `tracing-subscriber` / `tui-logger`**: the `tui` Log panel.

## Relationship to the rest of the repo

- **yel-wasm-codegen** emits the guest component + WIT-facing exports.
- **yel-host** is only for **manual or scripted smoke tests** of those artifacts; CI integration tests for codegen typically live under **`yel-wasm-codegen/tests/`**, not necessarily here.
