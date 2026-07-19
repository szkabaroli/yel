# diag-builder-messages

> Build diagnostics fluently; write lowercase, punctuation-free messages with notes for detail

## Why It Matters

Hundreds of call sites emit errors, so construction must be readable and consistent. yel exposes a fluent builder — `Diagnostic::error(msg).with_span(span).with_code("E0001").with_note("…")` — plus a `Severity` enum (Error/Warning/Note). Keep the primary message short and lowercase with no trailing period (matching rustc/Rust convention) and push elaboration into `notes`, so the headline stays scannable while detail stays available.

## Bad

```rust
let mut d = Diagnostic::new(Severity::Error);
d.message = "Type Mismatch: Expected i32 but got bool.".to_string(); // capitalized, period, detail crammed in
d.span = Some(span);
self.diags.push(d);
```

## Good

```rust
self.diags.push(
    Diagnostic::error("type mismatch") // lowercase, no trailing period
        .with_span(span)
        .with_code("E0001")
        .with_note("expected `i32`, found `bool`"), // detail goes in notes
);
```

## See Also

- [diag-spans-everywhere](diag-spans-everywhere.md) - `with_span` anchors the message to source
- [diag-accumulate-continue](diag-accumulate-continue.md) - The sink these builders are pushed into
