# wit-world-as-contract

> The WIT world *is* the component's contract; generate it from the IR and always produce one well-formed package, even for empty/library modules

## Why It Matters

For a component, the WIT world is not documentation — it is the linkable contract: it names the interfaces the component exports and the host interfaces it imports, and the encoder binds core exports/imports to it by canonical name. So the world must be derived from the same IR that produced the code, and it must always be *well-formed*, including the degenerate cases — a module with no exported entity is still a valid component (a library) and still needs a package and a world. Special-casing "empty" at the call site is where drift and crashes creep in. yel's `wit.rs::generate_wit` (and the AST builder behind it, `WitAstBuilder::build_wit_with_all`) projects a WIT package straight from the LIR: each exported component becomes a resource interface, globals get their own interfaces, and when there is no exported component it emits a `library` world rather than bailing. The resulting `(Resolve, WorldId)` is what `embed_component_metadata` stamps into the core module.

## Bad

```rust
// bail when there's nothing exported — callers must now special-case "no WIT",
// and globals-only library modules can never be encoded
if exported.is_empty() {
    return Err(CodegenError::InvalidIR("no exported component".into()));
}
```

## Good

```rust
// always project a package from the IR; no exported component => library world
let exported: Vec<&LirResource> = components.iter().filter(|c| c.is_export).collect();
let all: Vec<&LirResource> = components.iter().collect();
let mut builder = WitAstBuilder::new(ctx, ns, name, ver);
builder.build_wit_with_all(&exported, &all)?; // emits a `library` world if empty
let (resolve, world_id) = builder.into_resolve_and_world();
```

## See Also

- [wit-resource-for-handles](wit-resource-for-handles.md) - How exported entities map into the world
- [host-versioned-imports](host-versioned-imports.md) - The imported side of the contract
- [comp-encode-from-core](comp-encode-from-core.md) - The world gets embedded into the core module
