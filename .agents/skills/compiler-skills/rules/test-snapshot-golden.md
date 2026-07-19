# test-snapshot-golden

> Snapshot deterministic textual outputs (IR dumps, generated code) so drift shows up as a diff

## Why It Matters

A compiler emits large structured artifacts (IR dumps, generated WIT, DOT graphs) that no human will assert on field-by-field. Snapshot tests capture the full textual output once and turn every future change into a reviewable diff. In yel, `crates/yelc/tests/snapshot.rs` runs the real `yelc` binary on a fixed source and feeds its stdout into `insta::assert_snapshot!`, so any unintended codegen drift surfaces as a failing diff instead of slipping through unnoticed.

## Bad

```rust
// Asserts a few hand-picked substrings; misses everything else that changed
let wit = compile_to_wit(SRC);
assert!(wit.contains("interface dom"));
assert!(wit.contains("func render"));
```

## Good

```rust
// Capture the whole deterministic output; review intentional changes
// via `cargo insta review` or accept with `INSTA_UPDATE=always cargo test`.
let wit = run_yelc(&["--emit=wit", "fixtures/app.yel"]);
insta::assert_snapshot!(wit); // snapshot lives in tests/snapshots/*.snap
```

## See Also

- [test-deterministic-output](test-deterministic-output.md) - Snapshots only work if output is byte-stable
- [test-diagnostic-fixtures](test-diagnostic-fixtures.md) - Pin error wording the same way
