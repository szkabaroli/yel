# wit-single-type-owner

> Each named type/resource is owned by exactly one interface; others `use`/alias it — redefining mints two distinct types

## Why It Matters

In WIT, a named type's identity is its definition site. Two separate `record foo { ... }` definitions — even byte-for-byte identical — are two structurally-distinct types to the encoder, so a value produced against one cannot satisfy a signature written against the other, and interop breaks. The fix is to give every canonical type a single owning interface and have every other interface reach it through a `use`/alias instead of redefining it. yel enforces this in `wit_ast.rs`: canonical ADTs live in one types interface (and resources in their `{component}-resource` interfaces), and `use_type_in` / `use_resource_in` first inspect `self.resolve.types[type_id].owner` — if it is a *different* `TypeOwner::Interface`, they allocate a `TypeDefKind::Type` alias owned by the importing interface (memoized in `alias_map`) rather than minting a fresh definition.

## Bad

```rust
// redefine "the same" record in a second interface — now there are two
// incompatible `foo` types and values can't cross between interfaces
let foo_again = self.resolve.types.alloc(TypeDef {
    name: Some("foo".into()),
    kind: TypeDefKind::Record(same_fields.clone()),
    owner: TypeOwner::Interface(other_iface),
});
```

## Good

```rust
// one owner; everyone else aliases via `use`
fn use_type_in(&mut self, ty: Ty, in_interface: InterfaceId) -> Result<Type, _> {
    let type_id = self.canonical_type_id(ty)?;
    if let TypeOwner::Interface(owner) = self.resolve.types[type_id].owner
        && owner == in_interface { return Ok(Type::Id(type_id)); } // already here
    if let Some(&a) = self.alias_map.get(&(in_interface, type_id)) {
        return Ok(Type::Id(a)); // reuse memoized alias
    }
    let alias = self.resolve.types.alloc(TypeDef {
        kind: TypeDefKind::Type(Type::Id(type_id)),  // alias, not a redefinition
        owner: TypeOwner::Interface(in_interface), ..
    });
    Ok(Type::Id(alias))
}
```

## See Also

- [wit-resource-for-handles](wit-resource-for-handles.md) - Resources are owned + aliased the same way
- [wit-world-as-contract](wit-world-as-contract.md) - Type identity makes the world's interfaces link
