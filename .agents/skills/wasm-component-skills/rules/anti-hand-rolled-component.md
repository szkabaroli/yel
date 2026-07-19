# anti-hand-rolled-component

> Don't hand-emit component sections or canonical-function entries — drive it through the encoder

## Why It Matters

The component wrapper — its component type section, the canonical `lower`/`lift` function entries, instance and alias plumbing — *is* the canonical ABI made concrete. Re-implementing it by hand duplicates that spec, and the duplicate rots the instant the ABI or your WIT changes, producing a component that no longer matches the adapters the host expects. The supported path is always core-module-first: emit a plain core module, stamp the WIT world in as metadata, and let the encoder synthesize the wrapper. yel does exactly this in `wasm/mod.rs` — build the core module, call `wit_component::embed_component_metadata(...)`, then `ComponentEncoder::default().module(&bytes)?.validate(true).encode()`. This is the inverse of `comp-encode-from-core`.

## Bad

```rust
// hand-assemble the component layer and its canonical adapters
let mut component = ComponentSection::new();
component.ty().function(/* re-derive the canonical ABI by hand */);
component.canonical().lower(core_func_idx, /* options */);
// desyncs from the spec / your WIT on the next change
```

## Good

```rust
// core module -> embed WIT -> let the encoder build the component
wit_component::embed_component_metadata(&mut bytes, &resolve, world_id, StringEncoding::UTF8)?;
let component = ComponentEncoder::default()
    .module(&bytes)?
    .validate(true)
    .encode()?;
```

## See Also

- [comp-encode-from-core](comp-encode-from-core.md) - The pipeline this rule is the inverse of
- [abi-lift-lower-shims](abi-lift-lower-shims.md) - Let the encoder own the canonical entries
