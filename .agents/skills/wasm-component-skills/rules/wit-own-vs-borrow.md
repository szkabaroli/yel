# wit-own-vs-borrow

> Transfer ownership with `own<T>`, lend transient access with `borrow<T>`; getting it wrong leaks or double-frees handles

## Why It Matters

The component model tracks resource-handle ownership explicitly. `own<T>` transfers the handle — the receiver becomes responsible for dropping it; `borrow<T>` lends it for the duration of one call — the lender keeps ownership and drops it later. Pick the wrong one and you get a real lifetime bug: a `borrow` that should have been an `own` leaks the handle (nobody drops it), and an `own` that should have been a `borrow` causes a double-drop when both sides try to free it. The distinction is per-parameter and per-return, not per-type. yel's `wit_ast.rs::create_component_interface` follows the convention exactly: the constructor returns `Handle::Own(resource)` (the host now owns the new instance), while the implicit `self` parameter on each method is `Handle::Borrow(resource)` (the method only reads/mutates for that call). Callback and host-boundary interfaces likewise take `borrow<X>` via `use_resource_in` because they touch a resource they do not own.

## Bad

```rust
// every method takes `self` by `own` — the host loses its handle after the
// first method call, then double-drops at end of scope
let self_ty = self.resolve.types.alloc(TypeDef {
    kind: TypeDefKind::Handle(Handle::Own(resource_type_id)), // should be Borrow
    owner: TypeOwner::Interface(interface_id),
    ..
});
```

## Good

```rust
// constructor hands ownership out; methods only borrow self for the call
let own_ty = TypeDefKind::Handle(Handle::Own(resource_type_id));    // ctor result
let borrow_ty = TypeDefKind::Handle(Handle::Borrow(resource_type_id)); // self param
// callbacks that read a foreign resource also borrow, never own:
let borrowed = self.use_resource_in(resource_ty, callback_iface);
```

## See Also

- [wit-resource-for-handles](wit-resource-for-handles.md) - The resources these handles point at
- [wit-single-type-owner](wit-single-type-owner.md) - Aliasing the resource into a borrowing interface
