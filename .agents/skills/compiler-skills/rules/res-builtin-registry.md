# res-builtin-registry

> Register builtins/prelude into the same def table and cache their ids in a known-definitions struct

## Why It Matters

Builtins and prelude items should resolve like any user definition — special-casing them in the resolver leaks ad-hoc string matching into every phase. yel's `lookup_known_definitions(ctx)` registers builtin types, enums, variants, elements, and functions into the normal `Definitions` table at startup, so they share one id space with user defs and resolution/codegen treat them uniformly. The freshly minted ids are cached in a `KnownDefinitions` struct as `Option<DefId>` fields, turning hot checks like `is_builtin(def_id)` or "is this `append`?" into cheap id compares instead of string lookups.

## Bad

```rust
// Builtins live outside the def table; every check is a string match.
fn is_append(ctx: &Ctx, def_id: DefId) -> bool {
    ctx.defs.name_of(def_id) == "append"
}
fn resolve(name: &str) -> Option<Resolved> {
    match name { "append" => Some(Resolved::Builtin(Builtin::Append)), _ => /* user defs */ }
}
```

## Good

```rust
// At startup, register builtins into the SAME Definitions table.
fn lookup_known_definitions(ctx: &mut Ctx) -> KnownDefinitions {
    let append = register_function(ctx, "append", &[Ty::ERROR, Ty::ERROR], Ty::ERROR);
    // ... register builtin types, enums, variants, elements ...
    KnownDefinitions { append: Some(append), /* ... */ }
}

struct KnownDefinitions { append: Option<DefId>, /* ... */ }
impl KnownDefinitions {
    fn append(&self) -> DefId { self.append.expect("append registered") }
}

// Hot path: id compare, no strings.
fn is_append(known: &KnownDefinitions, def_id: DefId) -> bool {
    known.append == Some(def_id)
}
```

## See Also

- [res-namespaced-defs](res-namespaced-defs.md) - Builtins register into this `(name, namespace)` table
- [ctx-central-context](ctx-central-context.md) - `KnownDefinitions` is cached on the shared context
