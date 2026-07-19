# abi-respect-flattening-limit

> The canonical ABI flattens only up to a fixed count; past it, values spill through a memory return-area pointer — compute signatures accordingly

## Why It Matters

The canonical ABI flattens aggregates into core scalars only up to a fixed budget — `MAX_FLAT_PARAMS = 16` core values for parameters and `MAX_FLAT_RESULTS = 1` for results (the current constants in the canonical-ABI reference implementation). Beyond the limit the adapter stops flattening: an over-budget parameter list is replaced with a single pointer to a caller-built memory area, and over-budget results spill to a pointer too — returned via a pointer when lifting, or via an appended out-param pointer when lowering — rather than travelling on the stack. A guest that computes its core signature from `canonical_flat_valtypes` (`wasm/mod.rs`) — which always returns the *fully* flattened sequence — must apply the same spill rule, or the core function it emits won't match the shape the host calls. In yel the `ComponentEncoder` synthesises the adapter from the embedded WIT, so the guest's job is to present the spilled core shape the adapter expects (a pointer in, a pointer out), not to re-flatten past the limit.

## Bad

```rust
// blindly turn every flattened valtype into a core param, ignoring the limit
let params = self.canonical_flat_valtypes(ty); // may be 30+ scalars
ty_section.function(params, results);
// over-budget signatures won't line up with the adapter the encoder
// generates — it spills to a pointer, your core fn still expects N scalars
```

## Good

```rust
// past the flatten budget, present the spilled shape the adapter uses
let flat = self.canonical_flat_valtypes(ty);
let params = if flat.len() > MAX_FLAT_PARAMS {
    vec![ValType::I32]      // pointer to a linear-memory params area
} else {
    flat
};
ty_section.function(params, results); // matches the synthesised adapter
```

## See Also

- [abi-lift-lower-shims](abi-lift-lower-shims.md) - The adapter that does the spilling
- [mem-return-buffer-ownership](mem-return-buffer-ownership.md) - Who owns the spilled return area
- [mem-canonical-alignment](mem-canonical-alignment.md) - Laying out the spilled memory area
