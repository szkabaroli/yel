# abi-despecialize-first

> Despecialize tuple / enum / option / result / map down to record and variant before computing layout or flattening

## Why It Matters

The canonical ABI treats several types as *specialized* sugar over two general forms — `record` and `variant` — and defines every layout, size, alignment, and flattening rule on the general form only. The spec's `despecialize()` rewrites them first: `tuple → record` (fields labelled `"0"`, `"1"`, …), `enum → variant` of payload-less cases, `option<T> → variant { none, some(T) }`, `result<o,e> → variant { ok(o), error(e) }`, and `map<k,v> → list<tuple<k,v>>`. If a generator hand-rolls a separate layout for each sugar it drifts from the canonical form (a stray byte of padding, a differently-sized tag) and the host lifts garbage. Despecialize once, then run the record/variant machinery. In yel this is exactly why `option`/`result`/`variant` all classify to a single `FlatGcStruct` ref (the despecialized variant shape) in the `InternalRepr` classifier (`wasm/repr.rs`) rather than each getting bespoke handling.

## Bad

```rust
// hand-rolled option layout: 1-byte tag then payload, no despecialization
fn option_layout(&self, inner: Ty) -> Layout {
    Layout { tag: 1, payload_at: 1, .. } // diverges from variant{none,some} rules
}
```

## Good

```rust
// rewrite to the general variant, then use the one variant layout path
let variant = despecialize(ty); // option<T> -> variant { none, some(T) }
let layout = self.variant_layout(&variant); // canonical discriminant + payload
```

## See Also

- [abi-variant-discriminant](abi-variant-discriminant.md) - The layout the despecialized variant uses
- [abi-flatten-variant-join](abi-flatten-variant-join.md) - How that variant then flattens
- [gc-classify-once](gc-classify-once.md) - One classifier, after despecialization
