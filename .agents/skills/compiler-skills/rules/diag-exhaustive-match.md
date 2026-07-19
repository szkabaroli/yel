# diag-exhaustive-match

> When matching on an op / node / kind enum, never let a catch-all arm silently swallow an unhandled variant — make the gap explicit with `todo!()`/`unreachable!()` or drop the wildcard so the compiler forces exhaustiveness

## Why It Matters

The dangerous failure mode is a `match` over a `LirOp`, `*ExprKind`, or `*Node`
that ends in `_ => { … }` doing something *plausible but wrong* — emitting
nothing, falling through to a default width, returning `i32`. When a new variant
is added to the enum, the catch-all absorbs it silently: no compile error, no
panic, just subtly-wrong IR or WASM that surfaces hundreds of lines downstream as
a bad stack shape or a validation failure. This is the dispatch-site twin of
[diag-no-silent-fallback](diag-no-silent-fallback.md): that rule bans fake
*values*, this one bans fake *coverage*.

A wildcard arm is only legitimate when it handles a genuine, intended default
that is *correct for every current and future variant* (e.g. "all remaining
numeric widths load as `i32`"). If the arm exists because the other variants
"aren't done yet", it is a silent fallback — spell it out.

Two safe shapes:

1. **No wildcard.** List every variant. Adding a new one then fails to compile at
   exactly the sites that must handle it — the compiler becomes the checklist.
2. **Explicit dead/unimplemented arm.** Keep `_` but make it loud:
   `_ => todo!("…")` for not-yet-lowered, `_ => unreachable!("…")` for
   genuinely-impossible-by-construction (with a note on *why*).

## Bad

```rust
fn emit_op(&mut self, op: &LirOp) -> Result<(), CodegenError> {
    match op {
        LirOp::Const(c)    => self.emit_const(c),
        LirOp::FieldGet(f) => self.emit_field_get(f),
        // everything else: do nothing for now
        _ => Ok(()),
        // a freshly-added LirOp::ListPush compiles fine and emits NOTHING —
        // the bug shows up as a stack underflow in a hex dump, not here
    }
}
```

## Good

```rust
fn emit_op(&mut self, op: &LirOp) -> Result<(), CodegenError> {
    match op {
        LirOp::Const(c)    => self.emit_const(c),
        LirOp::FieldGet(f) => self.emit_field_get(f),
        // not lowered yet — crashes at a clear, greppable location
        LirOp::ListPush(_) => todo!("LirOp::ListPush lowering"),
        // impossible here: structural ops are lowered away before codegen
        LirOp::TreeShape(_) => unreachable!("TreeShape removed in lir::flatten"),
    }
    // no `_` arm: adding LirOp::Foo is a compile error until handled
}
```

## See Also

- [diag-no-silent-fallback](diag-no-silent-fallback.md) - The value-level twin: don't emit placeholder IR for unimplemented paths
- [diag-error-type-recovery](diag-error-type-recovery.md) - For *user* errors, recover with an error type instead of `todo!()`
- [anti-duplicate-walker](anti-duplicate-walker.md) - A single shared walker means one exhaustive `match` to keep honest, not N
