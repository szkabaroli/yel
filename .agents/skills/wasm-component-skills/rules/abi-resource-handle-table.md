# abi-resource-handle-table

> Cross resources as i32 handle-table indices, not pointers; lift/lower through the per-instance table and honor own-vs-borrow lifetimes

## Why It Matters

`own<T>` and `borrow<T>` are **not** pointers — they cross the ABI as an `i32` index into a per-component-instance handle table, which is what keeps the resource's representation opaque to the other side. The spec's `lower_own`/`lower_borrow` add a `ResourceHandle` to `inst.handles` and return its index; `lift_own` *removes* the entry and traps unless it is the right resource type, actually owned, and has no outstanding lends (`trap_if(h.num_lends != 0)`); `lift_borrow` looks the entry up and registers a lend on the current call's borrow scope. A borrow is scoped to the call: it bumps a borrow count that must return to zero before the owner may drop the resource (`trap_if(num_borrows > 0)` at task return). Represent a resource as a raw linear-memory pointer and you throw away both the opacity and the lifetime guarantees the model enforces. In yel each exported component and canonical ADT is modelled as a WIT `resource` precisely so it travels as a handle, not a pointer (see [wit-resource-for-handles](wit-resource-for-handles.md)).

## Bad

```rust
// hand the host a raw pointer into the component's linear-memory state
let state_ptr = self.alloc_component_state();
return state_ptr; // host can forge/alias it; no ownership tracking at all
```

## Good

```rust
// register the rep in the instance handle table; hand out the index
let handle = ResourceHandle::new(rt, rep, /* own */ true);
let idx = self.inst.handles.add(handle); // i32 index, opaque to the host
return idx;
// lift_own later: remove(idx), trap unless right type, owned, num_lends == 0
```

## See Also

- [wit-resource-for-handles](wit-resource-for-handles.md) - The WIT side that makes it a resource
- [wit-own-vs-borrow](wit-own-vs-borrow.md) - Choosing own vs borrow in the interface
- [abi-trap-on-invalid-lift](abi-trap-on-invalid-lift.md) - Lifting a handle validates type/ownership/lends
