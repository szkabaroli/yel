# diag-spans-everywhere

> Attach a source span to every diagnostic; a message without a location is half a bug report

## Why It Matters

A diagnostic that says *what* is wrong but not *where* makes the user hunt through their whole file. yel's `Diagnostic { severity, message, span: Option<Span>, code, notes }` carries a span, and `render(&SourceMap)` turns it into a `--> name:line:col` header plus a source snippet via `Source::line_col` / `snippet`. Because every IR node preserves its span, a diagnostic from any phase can point straight at the user's code. `span: None` should be the rare exception, reserved for whole-file errors.

## Bad

```rust
self.diags.push(Diagnostic::error("type mismatch")); // where?? user has to guess
```

## Good

```rust
self.diags.push(
    Diagnostic::error("type mismatch").with_span(expr.span), // node kept its span
);
// render(&source_map) ->  --> main.yel:12:7  with a highlighted snippet
// span: None only for genuinely file-wide errors (e.g. "no entry point")
```

## See Also

- [ir-preserve-spans](ir-preserve-spans.md) - Every IR node must carry its span for this to work
- [diag-builder-messages](diag-builder-messages.md) - `with_span` is part of the fluent builder
