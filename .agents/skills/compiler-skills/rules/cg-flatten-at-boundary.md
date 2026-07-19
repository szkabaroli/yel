# cg-flatten-at-boundary

> Keep the internal representation typed; flatten to the flat ABI only at FFI / ABI boundaries

## Why It Matters

An ABI (a C calling convention, the WASM component-model canonical ABI) demands aggregates be *flattened* into a sequence of scalar slots. But flattening everywhere internally is both slower and bug-prone: code that manipulates a record as a tuple of loose scalars is harder to write correctly than code that passes a single typed reference. Keep values in their rich internal representation throughout the body and flatten **only** at the boundary where you cross the ABI — the lift/lower shims. yel keeps records/tuples/lists as typed WASM-GC refs internally and confines the flat decomposition (`canonical_flat_valtypes` / `flatten_core_valtypes`) to the WIT export boundary-shim generator; the rule is even documented as an invariant in `wasm/repr.rs`.

## Bad

```rust
// every internal call passes records as their flattened ABI scalars
fn call_internal(&mut self, rec: &Expr) {
    for slot in self.canonical_flat_valtypes(rec.ty) { /* push each field */ }
    // slow, and every caller/callee must agree on the flattening by hand
}
```

## Good

```rust
// internal: one typed GC ref
fn call_internal(&mut self, rec: &Expr) { self.push_gc_ref(rec); }

// boundary only: lower the typed value into ABI scalars in the export shim
fn lower_export_param(&mut self, ty: Ty) {
    for vt in self.canonical_flat_valtypes(ty) { /* WIT lift/lower */ }
}
```

## See Also

- [cg-repr-single-source](cg-repr-single-source.md) - The classifier that decides internal vs flat
- [intern-types](intern-types.md) - Internal values stay as typed handles
- [ir-handles-over-boxes](ir-handles-over-boxes.md) - Pass a handle, not a decomposed aggregate
