# test-known-bugs-ignore

> Track known bugs with `#[ignore]` + a reference; never soften an assertion to match wrong output

## Why It Matters

Weakening an assertion to match known-wrong output turns the test suite into a ratchet that locks the bug in: the suite goes green and the next person assumes the behaviour is intended. The fix is to keep every assertion describing *correct* behaviour and quarantine the failure instead. yel's `crates/yel-wasm-codegen/tests/execution.rs` states this rule explicitly — known-buggy cases stay asserting the correct result and are marked `#[ignore]` with a reference, or are tracked via the `known_bugs` fixtures with a `.failure` file, so they stay red-on-correctness rather than green-on-wrong.

## Bad

```rust
#[test]
fn reactive_text_updates() {
    let recorded = run(SRC);
    // BUG: emits "count: 0" instead of "count: 1" after increment.
    assert_eq!(recorded.last_text(), "count: 0"); // locks in the bug
}
```

## Good

```rust
#[test]
#[ignore] // KNOWN BUG: see issue #214 — effect re-runs with stale value
fn reactive_text_updates() {
    let recorded = run(SRC);
    assert_eq!(recorded.last_text(), "count: 1"); // still asserts CORRECT behaviour
}
```

## See Also

- [test-execution-e2e](test-execution-e2e.md) - Running the artifact is what surfaces these bugs
- [diag-no-silent-fallback](diag-no-silent-fallback.md) - Don't mask wrongness at runtime either
