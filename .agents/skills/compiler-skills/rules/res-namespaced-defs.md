# res-namespaced-defs

> Key the definition table by `(name, namespace)` so types, values, and components can share a name

## Why It Matters

Real languages let one identifier mean different things in different namespaces — Rust lets a type and a value share a name, and yel does the same for types, values, components, and globals. Keying the def table by `(Name, Namespace)` instead of `Name` alone lets resolution disambiguate by context without spurious collisions. yel stores defs in `Definitions { items: IndexVec<DefId, DefItem>, by_name: HashMap<(Name, Namespace), DefId> }` and exposes `lookup(name, namespace) -> Option<DefId>`, plus structured helpers like `find_field`, `find_signal`, and `find_member` that return `(FieldIdx, DefId)`.

## Bad

```rust
// One flat space: a type and a value with the same name collide.
struct Definitions {
    items: IndexVec<DefId, DefItem>,
    by_name: HashMap<Name, DefId>,
}
fn lookup(&self, name: Name) -> Option<DefId> {
    self.by_name.get(&name).copied()
}
```

## Good

```rust
enum Namespace { Type, Value, Component, Global }

struct Definitions {
    items: IndexVec<DefId, DefItem>,
    by_name: HashMap<(Name, Namespace), DefId>,
}

impl Definitions {
    fn lookup(&self, name: Name, ns: Namespace) -> Option<DefId> {
        self.by_name.get(&(name, ns)).copied()
    }
    // Structured members resolve to a typed index plus the owning def.
    fn find_field(&self, ty: DefId, name: Name) -> Option<(FieldIdx, DefId)> { /* ... */ }
}
```

## See Also

- [id-indexvec](id-indexvec.md) - `items` is keyed by `DefId`
- [res-scope-stack](res-scope-stack.md) - Locals layer on top of the def table
- [res-builtin-registry](res-builtin-registry.md) - Builtins register into this same table
