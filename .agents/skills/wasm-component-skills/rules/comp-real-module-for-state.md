# comp-real-module-for-state

> Emit a real core module (memory, allocator, start) whenever there's state; only stub truly empty modules with a dummy

## Why It Matters

A `wit_component::dummy_module` satisfies a WIT world's *shape* but exports nothing real — no linear memory, no allocator, no initialized globals. That is the right placeholder for a world with no implementation yet, but it is silently wrong the moment the module actually owns state, because the start logic that seeds that state never runs. Decide which to emit from whether there *is* state, not from convenience. yel's `wasm/mod.rs::generate_wasm_module_with_wit` computes `has_module_state = !module.components.is_empty() || !module.global_defaults.is_empty()`; only when that is false does it fall back to `dummy_module(&resolve, world_id, ManglingAndAbi::Standard32)`. Otherwise it builds a `WasmPackageBuilder`, lays out linear memory, emits the allocator (`AllocatorFuncs`), and writes a start function that stores every `global_defaults` entry into its backing slot before any export can observe it.

## Bad

```rust
// always stub — globals-only "library" modules ship with their
// default-seeding start function missing, so every global reads as zero
let dummy = dummy_module(&resolve, world_id, ManglingAndAbi::Standard32);
embed_component_metadata(&mut dummy, &resolve, world_id, StringEncoding::UTF8)?;
ComponentEncoder::default().module(&dummy)?.validate(true).encode()
```

## Good

```rust
// stub only when there is genuinely nothing to initialize
let has_module_state =
    !module.components.is_empty() || !module.global_defaults.is_empty();
if !has_module_state {
    return encode_dummy(&resolve, world_id); // truly empty world
}
// real core module: memory + allocator + start that seeds global_defaults
let mut builder = WasmPackageBuilder::new(&module.components, ctx);
builder.set_global_defaults(module.global_defaults.clone());
let core = builder.build_core_module()?; // start fn writes defaults first
```

## See Also

- [comp-encode-from-core](comp-encode-from-core.md) - The encoder both paths feed into
- [mem-cabi-realloc](mem-cabi-realloc.md) - The allocator a stateful module must export
- [wit-world-as-contract](wit-world-as-contract.md) - Both stub and real module satisfy the same world
