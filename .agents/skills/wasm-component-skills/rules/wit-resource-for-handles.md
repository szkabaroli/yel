# wit-resource-for-handles

> Model stateful exported entities as WIT `resource`s with a constructor + methods; the component holds the state, the host holds an opaque handle

## Why It Matters

The component model has no way to hand a host a live, mutable object by value — value types are copied across the boundary. A WIT `resource` is the mechanism for shared, stateful entities: the guest owns the real state, the host receives an opaque handle, and every interaction goes through the resource's constructor and methods. So anything with identity and lifecycle (a component instance, a stateful exported object) should be a `resource`, not a record. yel's `wit_ast.rs::create_component_interface` emits each exported component as exactly this: a `TypeDefKind::Resource` plus a constructor and methods inside a `{component}-component` interface. Canonical ADTs (record / enum / variant) that need handle identity likewise get bare `resource X;` declarations in dedicated `{component}-resource` interfaces tracked in `resource_interface_ids`, which other interfaces alias in rather than redefine.

## Bad

```rust
// expose stateful component state as a plain record — the host gets a
// detached copy, mutations never reach the live guest instance
let counter = TypeDef {
    name: Some("counter".into()),
    kind: TypeDefKind::Record(Record { fields: vec![count_field] }),
    owner: TypeOwner::Interface(iface),
};
```

## Good

```rust
// a resource: guest owns the state, host holds a handle, access via methods
let resource_ty = self.resolve.types.alloc(TypeDef {
    name: Some(resource_name.into()),
    kind: TypeDefKind::Resource,
    owner: TypeOwner::Interface(interface_id),
});
// + a constructor and `increment` / `get` methods on this interface
```

## See Also

- [wit-own-vs-borrow](wit-own-vs-borrow.md) - Handle ownership on the resource's methods
- [wit-single-type-owner](wit-single-type-owner.md) - One interface owns the resource; others alias it
