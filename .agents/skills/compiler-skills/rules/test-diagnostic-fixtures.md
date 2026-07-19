# test-diagnostic-fixtures

> Pin error messages with `source + expected` fixture pairs

## Why It Matters

A compiler's diagnostics are user-facing API: their wording, span, and presence all matter, and regressions in them are easy to ship silently. Fixture pairs make each diagnostic a two-file drop-in test. In yel, `crates/yel-wasm-codegen/tests/` walks `fixtures/diagnostics/*.yel` paired with `.expected` files (one expected substring per line), compiles each source, and asserts the emitted diagnostics contain every expected substring — so a reworded or dropped error is caught and reviewed.

## Bad

```rust
// Inlined, brittle, and only checks that *some* error happened
let res = compile("let x: Int = \"s\";");
assert!(res.is_err()); // wording can rot freely; no span coverage
```

## Good

```rust
// fixtures/diagnostics/type_mismatch.yel   (the source)
// fixtures/diagnostics/type_mismatch.expected:
//   expected `Int`, found `String`
//   --> type_mismatch.yel
for (src, expected) in load_fixture_pairs("fixtures/diagnostics") {
    let diags = compile(&src).diagnostics();
    for line in expected.lines() {
        assert!(diags.iter().any(|d| d.render().contains(line)), "missing: {line}");
    }
}
```

## See Also

- [test-snapshot-golden](test-snapshot-golden.md) - Snapshot full artifacts the same way
- [diag-builder-messages](diag-builder-messages.md) - Build the messages these fixtures pin
