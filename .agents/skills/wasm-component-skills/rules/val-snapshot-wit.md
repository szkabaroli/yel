# val-snapshot-wit

> Snapshot the generated WIT (and any debug graph) so interface drift surfaces as a reviewable diff

## Why It Matters

The WIT is the component's public contract; a one-line change to it can break every consumer, yet without a baseline it slips through review invisibly. Snapshotting the generated WIT text turns any accidental interface change into a concrete, reviewable diff — the reviewer sees exactly which function signature or type moved. yel snapshot-tests the generated `wit_code` (and the `dot_code` debug graph) with insta — `crates/yelc/tests/snapshot.rs` drives the real `yelc` binary and asserts the output against committed `.snap` files; intentional changes are accepted with `INSTA_UPDATE=always cargo test -p yelc --test snapshot`. This only works because generation is deterministic (sort + dedup before output), so the snapshot is byte-stable.

## Bad

```rust
// assert only that "some WIT was produced" — any signature drift passes silently
assert!(!result.wit_code.is_empty());
```

## Good

```rust
// pin the exact interface text; drift becomes a reviewable diff
#[test]
fn wit_for_a_reactive_component() {
    assert_snapshot!(compile_to(COUNTER, "wit", "counter-wit"));
}
```

## See Also

- [val-validate-component](val-validate-component.md) - Validating the binary, the other half of CI
- [wit-world-as-contract](wit-world-as-contract.md) - Why the WIT is a contract worth pinning
