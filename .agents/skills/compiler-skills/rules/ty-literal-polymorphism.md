# ty-literal-polymorphism

> Keep untyped literals polymorphic and resolve them against the expected type at the boundary

## Why It Matters

A bare numeric literal like `0` has no inherent width or signedness — it could be `s32`, `u8`, or `f32`. If you pin it to a default type during synthesis, you get spurious "expected u8, found i32" errors; if you silently coerce real typed values, you risk lossy conversions. yel keeps literals polymorphic and resolves them from the `Check(expected)` context, while requiring already-typed *variables* to convert explicitly (matching Rust, Swift, and Go). This pairs naturally with bidirectional checking, which is what supplies the expected type.

## Bad

```rust
// Literal eagerly defaulted to i32, then compared.
fn infer_lit(n: i64) -> Ty { Ty::I32 }

let x: u8 = 0; // error: expected u8, found i32  -- but 0 is fine as u8!
```

## Good

```rust
// Literal stays polymorphic; expected type decides its concrete form.
(Expr::IntLit(n), Mode::Check(expected)) if expected.is_integral() => {
    self.check_int_fits(*n, expected)?;
    (ThirKind::IntLit(*n), expected)
}
// A *typed* value, by contrast, must convert explicitly.
(Expr::Var(v), Mode::Check(expected)) => {
    let found = self.var_ty(v);
    if found != expected {
        return Err(self.needs_explicit_cast(found, expected));
    }
    // ...
}
```

## See Also

- [ty-bidirectional](ty-bidirectional.md) - Supplies the expected type that resolves the literal
- [intern-types](intern-types.md) - Comparing the resolved literal type is a cheap interned-handle compare
