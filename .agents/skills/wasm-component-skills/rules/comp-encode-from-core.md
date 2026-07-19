# comp-encode-from-core

> Emit a core module, then wrap it into a component with an encoder + embedded WIT metadata; never hand-assemble component sections

## Why It Matters

A WebAssembly *component* is not a bigger core module — it is a separate binary format that wraps one or more core modules, declares typed component-model imports/exports, and carries the canonical-ABI adapters that bridge the two. Hand-emitting the component layer (component type section, canonical-function section, instance/alias plumbing) means re-implementing the canonical ABI by hand, and it rots the instant the spec or your WIT changes. Instead: build a *core* module with `wasm-encoder`, embed the WIT world into it as a custom section, and let a component encoder synthesise the component wrapper. yel does exactly this in `wasm/mod.rs` — it assembles a core module, calls `wit_component::embed_component_metadata(&mut bytes, &resolve, world_id, StringEncoding::UTF8)`, then `ComponentEncoder::default().module(&bytes)?.validate(true).encode()`.

## Bad

```rust
// hand-rolling the component wrapper: component type section, canonical
// lower/lift entries, instance exports — all by hand
let mut component = ComponentSection::new();
component.ty().function(/* re-derive the canonical ABI yourself */);
component.canonical().lower(core_func_idx, /* options */);
// every spec tweak or WIT change silently desyncs this from reality
```

## Good

```rust
// 1. emit a plain core module (wasm-encoder)
let core_bytes = self.encode_core_module()?;
// 2. stamp the WIT world into it as a custom section
let mut bytes = core_bytes;
wit_component::embed_component_metadata(&mut bytes, &resolve, world_id, StringEncoding::UTF8)?;
// 3. let the encoder synthesise the component + canonical adapters
let component = ComponentEncoder::default().module(&bytes)?.validate(true).encode()?;
```

## See Also

- [comp-validate-on-encode](comp-validate-on-encode.md) - Keep `.validate(true)` on the encoder
- [comp-string-encoding-explicit](comp-string-encoding-explicit.md) - The `StringEncoding` passed at embed time
- [anti-hand-rolled-component](anti-hand-rolled-component.md) - The failure mode this rule prevents
