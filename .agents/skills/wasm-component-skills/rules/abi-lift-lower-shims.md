# abi-lift-lower-shims

> The WIT signature is not the core signature; generate explicit lift (host→guest) and lower (guest→host) shims per exported/imported function

## Why It Matters

A WIT function's component-level type and the core-wasm function's signature are two different things. A function declared in WIT as `take: func(s: string) -> record { … }` is, at the core level, something like `(i32, i32) -> i32` (a `(ptr, len)` string in, a pointer to a return area out). The canonical ABI's *lift* (host value → guest core values) and *lower* (guest core values → host value) adapters bridge the two. In yel the `ComponentEncoder` synthesises these adapters from the WIT embedded into the core module (see `comp-encode-from-core`), and the guest emits core functions whose flattened signatures — computed via `canonical_flat_valtypes` (`wasm/mod.rs`) — match what those adapters expect. Conflating the WIT signature with the core signature produces a module the encoder can't adapt.

## Bad

```rust
// emit a core fn whose params mirror the WIT type literally
// WIT: take: func(s: string)  →  but a core fn can't take a `string`
ty_section.function([/* a single "string" */], []);
// there is no core `string` type; this never matches the lowered call
func_section.function(ty_idx);
```

## Good

```rust
// core signature = the WIT type LOWERED to canonical-ABI scalars
// WIT `take: func(s: string)` lowers to (ptr: i32, len: i32) -> ()
let params = self.canonical_flat_valtypes(string_ty); // [I32, I32]
ty_section.function(params, []);
// the ComponentEncoder then synthesises the lift/lower adapter that
// turns the component-level `string` into these two core values
```

## See Also

- [abi-flatten-at-boundary](abi-flatten-at-boundary.md) - Flattening belongs in these shims and nowhere else
- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - How a `string` becomes the `(ptr,len)` the shim passes
- [comp-encode-from-core](comp-encode-from-core.md) - The encoder that synthesises the adapters
