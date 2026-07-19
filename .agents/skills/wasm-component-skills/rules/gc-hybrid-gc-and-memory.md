# gc-hybrid-gc-and-memory

> GC and linear memory coexist in one component; use GC for typed internal state and linear memory at the ABI boundary, and keep dual-backed values coherent

## Why It Matters

A component-model module is not forced to pick one memory model: WASM-GC typed heaps and a linear memory can live side by side. The right split is GC for typed internal state (records/tuples/lists as `struct`/`array` refs) and linear memory for the canonical ABI surface, where strings and lists must be materialized as `(ptr, len)` for the host. yel embraces this hybrid: pointer-repr signals are backed in *both* a `$Comp_<i>` GC struct field and a linear-memory cell. `lir/signal_layout.rs::SignalStorage` models the two halves as independent `gc: Option<GcSlot>` and `mem: Option<MemSlot>` — "a signal may have one, both, or neither." When both exist they describe the same value, so any write must update both or they silently desync.

## Bad

```rust
// update only the GC field; the linear-memory copy now lies
// a later canonical lower reads stale (ptr, len) from memory
self.struct_set(comp_struct, storage.gc.unwrap().field_start, value);
// storage.mem left untouched -> boundary sees the old string
```

## Good

```rust
// a dual-backed signal updates both halves to stay coherent
if let Some(gc) = storage.gc { self.struct_set(comp_struct, gc.field_start, value); }
if let Some(mem) = storage.mem { self.store_to_linear_mem(mem.offset, value); }
// neither half is authoritative on its own — write both
```

## See Also

- [gc-typed-internal](gc-typed-internal.md) - GC-side typed state
- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The linear-memory `(ptr, len)` side
- [mem-canonical-alignment](mem-canonical-alignment.md) - Laying out the linear-memory half
