# CLAUDE.md - yel-core

## No Silent Fallbacks

Never emit dummy/placeholder values (e.g. `Literal(Bool(false))`) as a fallback for unimplemented features during lowering. These cause silent type-incorrect IR where the expression kind doesn't match its declared type — leading to broken WASM that is extremely hard to diagnose.

```rust
// ❌ BAD — silent placeholder hides the missing feature
ThirExprKind::GlobalRead { .. } => {
    LirExprKind::Literal(LirLiteral::Bool(false))  // type says string, emits bool
}

// ✅ GOOD — fails loudly at compile time
ThirExprKind::GlobalRead { global, field, .. } => {
    todo!("GlobalRead not yet lowered to LIR: global={:?}, field={:?}", global, field)
}
```

Use `todo!()` with a descriptive message so unimplemented paths crash the compiler immediately with a clear location, instead of producing subtly broken output that takes hours to trace from hex dumps.
