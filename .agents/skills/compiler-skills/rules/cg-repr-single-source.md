# cg-repr-single-source

> Funnel "how is a value represented on the target" through one module, never per-emit-site

## Why It Matters

The most insidious backend bugs come from *representation drift*: one emit site decides a `string` is `(ptr, len)` (two slots) while another treats it as one, and the producer/consumer stack shapes no longer match — yielding code that fails validation or computes garbage, with a root cause that's brutal to trace. The fix is a single classifier that every emit site calls: when "how is this type represented?" has exactly one answer, it can't become inconsistent. yel centralizes this in `wasm/repr.rs::InternalRepr` (`Scalar`, `FatPointer`, `GcRef`, `GcArrayRef`, `FlatGcStruct`, …); its docstring states the rule outright — "every emit site that used to independently decompose a type into flat slots must funnel through this module."

## Bad

```rust
// each emit site re-derives the representation inline…
fn emit_arg(&mut self, e: &Expr) {
    if e.ty == self.string_ty { /* push ptr, push len */ }   // 2 slots here
    else { /* push scalar */ }
}
fn emit_return(&mut self, e: &Expr) {
    /* …and this one forgot strings are two slots */          // 1 slot — mismatch!
}
```

## Good

```rust
match self.internal_repr(ty) {
    InternalRepr::Scalar(v)   => /* 1 slot */,
    InternalRepr::FatPointer  => /* (i32, i32) */,
    InternalRepr::GcRef(idx)  => /* 1 typed ref */,
    InternalRepr::Zero        => /* 0 slots */,
}
// one classifier; producer and consumer can never disagree
```

## See Also

- [cg-flatten-at-boundary](cg-flatten-at-boundary.md) - The internal repr stays typed; flatten only at the edge
- [ir-handles-over-boxes](ir-handles-over-boxes.md) - Typed handles keep the internal repr compact
- [intern-types](intern-types.md) - The type the classifier dispatches on
