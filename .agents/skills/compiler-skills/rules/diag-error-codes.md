# diag-error-codes

> Give diagnostics stable error codes so users, docs, and tests can refer to them

## Why It Matters

A free-text message is the only handle users and tests have on an error — and message wording changes constantly. A stable code (`E0001`) survives rewording: users can search it, docs can cross-reference it, `#[allow]`/suppression can target it, and golden tests can assert on the code instead of brittle substrings. yel's `Diagnostic` carries `code: Option<ErrorCode>` — a *typed* enum (`diagnostic.rs`) whose `code()` maps each variant to a stable `E####`/`W####` string — set via the `with_code` builder, with the convenience `Diagnostics::error(span, code, msg)` making a code mandatory at every emission site. The renderer prints `error[E0308]: …`. Add (or reuse) an `ErrorCode` variant rather than inventing a string, so the code set stays centralized and exhaustive.

## Bad

```rust
diags.push(Diagnostic::error("type mismatch: expected s32, found string"));
// tests match the exact message → break the moment you reword it
```

## Good

```rust
// Typed code from the central `ErrorCode` enum — not a stringly-typed literal.
diags.push(
    Diagnostic::error("expected `s32`, found `string`")
        .with_code(ErrorCode::TypeMismatch) // renders as `error[E0001]`
        .with_span(expr.span),
);
// Or, at a convenience site, the code is a required argument:
self.ctx
    .diagnostics
    .error(expr.span, ErrorCode::TypeMismatch, "expected `s32`, found `string`");
// docs and tests reference the code; the wording is free to improve
```

## See Also

- [diag-builder-messages](diag-builder-messages.md) - The fluent builder that carries the code
- [diag-spans-everywhere](diag-spans-everywhere.md) - Pair the code with a location
- [test-diagnostic-fixtures](test-diagnostic-fixtures.md) - Assert on codes, not fragile wording
