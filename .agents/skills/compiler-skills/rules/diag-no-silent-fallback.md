# diag-no-silent-fallback

> Never emit placeholder/dummy IR for unimplemented paths — fail loudly with `todo!()`

## Why It Matters

This is a hard rule in `crates/yel-core/CLAUDE.md`. During lowering, never emit a dummy value (e.g. `LirExprKind::Literal(Bool(false))`) as a stand-in for an unimplemented feature: it produces type-incorrect IR where the expression kind contradicts its declared type, yielding broken WASM that is nearly impossible to trace back from a hex dump. An unimplemented path must fail at compile time, never silently produce subtly-wrong output.

## Bad

```rust
fn lower_global_read(&mut self, global: GlobalId) -> LirExpr {
    // not implemented yet — emit a placeholder so it "compiles"
    LirExpr::new(LirExprKind::Literal(Literal::Bool(false)), Ty::I32)
    // type-incorrect IR; broken WASM that lies about what it computes
}
```

## Good

```rust
fn lower_global_read(&mut self, global: GlobalId) -> LirExpr {
    todo!("GlobalRead not yet lowered: global={global:?}")
    // crashes immediately at a clear, greppable location
}
```

## See Also

- [diag-accumulate-continue](diag-accumulate-continue.md) - User errors accumulate; compiler gaps abort
- [test-known-bugs-ignore](test-known-bugs-ignore.md) - Mark not-yet-supported paths in tests instead of faking output
