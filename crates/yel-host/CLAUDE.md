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

## Dependencies

- **`wasmtime` / `wasmtime-wasi`**: component runtime + WASI.
- **`anyhow`**: CLI errors.
- **`clap`**: argument parsing (see **`Args`** in `main.rs`; the derive still names the command `yel-run` in metadata — align names if it causes confusion).

## Relationship to the rest of the repo

- **yel-wasm-codegen** emits the guest component + WIT-facing exports.
- **yel-host** is only for **manual or scripted smoke tests** of those artifacts; CI integration tests for codegen typically live under **`yel-wasm-codegen/tests/`**, not necessarily here.
