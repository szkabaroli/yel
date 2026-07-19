# host-versioned-imports

> Import host capability from a versioned WIT interface (`pkg:iface@x.y.z`) so host and guest can evolve compatibly

## Why It Matters

A component's link to its host is the WIT interface it imports, and that interface should carry a semantic version: `pkg:iface@x.y.z`. Pinning the version lets the host and guest evolve under semver — a host can add a function or ship a `0.2.0` without silently breaking a guest built against `0.1.0`, because the mismatch is a named, detectable contract violation rather than a wrong import index. yel's guest imports its DOM capability from the versioned interface `yel:ui/dom@0.1.0` (the WIT world declared via `wit.rs`, gated by `WitOptions.include_dom_interface`; the dev host lives in `crates/yel-host`). The `@0.1.0` is load-bearing, not decoration.

## Bad

```rust
// import an unversioned interface; any host change is an invisible break
world.import_interface("yel:ui/dom"); // which revision? host & guest can't tell
```

## Good

```rust
// pin the versioned interface so host/guest evolve under semver
let opts = WitOptions { include_dom_interface: true, ..Default::default() };
// world imports `yel:ui/dom@0.1.0` — a `@0.2.0` host is a named, detectable mismatch
let wit = generate_wit(&components, ctx, &opts)?;
```

## See Also

- [host-import-roundtrip](host-import-roundtrip.md) - Resolving each imported function to a wasm index
- [wit-world-as-contract](wit-world-as-contract.md) - The world that declares this import
