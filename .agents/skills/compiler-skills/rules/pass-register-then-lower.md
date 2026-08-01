# pass-register-then-lower

> Register all declarations before lowering any body, so forward references resolve

## Why It Matters

A body can reference a component or type declared later in the same file, so every name must exist in the definition table before any body is lowered. yel's `lower_file` in `crates/yel-core/src/hir/lower.rs` runs in phases: (1) register top-level type definitions, (1b) register elements / extern components / globals, (2) register component declarations WITHOUT bodies, then (3) lower the component bodies. Lowering bodies in source order would fail the moment a declaration referenced something defined further down.

## Bad

```rust
fn lower_file(file: &File, ctx: &mut Ctx) {
    for item in &file.items {
        // declares AND lowers in one pass, in source order
        let def = ctx.register(item);
        lower_body(item, def, ctx); // fails: callee not registered yet
    }
}
```

## Good

```rust
fn lower_file(file: &File, ctx: &mut Ctx) {
    // 1. register type defs, then elements/externs/globals
    for ty in file.types() { ctx.register_type(ty); }
    for g in file.globals() { ctx.register_global(g); }
    // 2. register component headers WITHOUT bodies
    for c in file.components() { ctx.register_component_decl(c); }
    // 3. now every name exists -> lower bodies, forward refs resolve
    for c in file.components() { lower_component_body(c, ctx); }
}
```

## See Also

- [res-namespaced-defs](res-namespaced-defs.md) - Names live in a shared definition table
- [pass-explicit-phases](pass-explicit-phases.md) - Splitting work into ordered phases
