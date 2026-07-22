# verify-roundtrip-not-validate

> Validates-but-wrong needs an execution round-trip assertion, not a compile-only check

## Why It Matters

`wasm-tools validate` proves the module is *well-typed*, not that it *computes
the right thing*. A whole class of bugs — wrong element stride, a stub that
returns zeros, a swapped discriminant, a wrong offset — produces perfectly valid
WASM that round-trips the wrong value. The fuzzer's validate-only loop is blind
to all of them; it counts them as PASS. So a fuzzer PASS is necessary but not
sufficient: any fix that touches how a *value* is laid out or copied must be
pinned by an **execution round-trip** — instantiate, `set` a known value, `get`
it back, assert equality — not merely "it compiles". This session's
`list<tuple>` fix validated on the first try but round-tripped `pairs[0].1` as
`30` (a stride bug); only the round-trip caught it.

## Good

```rust
#[test]
fn list_of_tuples_roundtrip() {
    let bytes = compile_to_component(SRC);          // validation alone would stop here
    let (mut h, _dom) = instantiate(&bytes, &[]);
    let r = ctor_and_mount(&mut h, iface, "app");

    call_setter(&mut h, iface, "app", "pairs", &r,
        Val::List(vec![Val::Tuple(vec![Val::S32(10), Val::S32(20)])]));   // known input

    let out = get(&mut h, iface, "get-pairs", &r);
    assert_eq!(out, /* Some(Ok([(10,20)])) */);      // prove the value, not just the shape
}
```

Guidelines: choose distinct, non-default values (not `0`/empty) so a stub that
returns defaults is caught; assert element-by-element for lists; test both arms
of an option/result (`some`/`none`, `ok`/`err`); and for deep nesting use small
`let some = |v| …; let ok = |v| …;` helpers so the `Val` literal's paren-nesting
stays readable and correct.

When a round-trip fails, print the actual value (`got {:?}`) — `[0,0,0]` vs
`[8,9,7]` vs `[7,8]` each point at a different bug (stub vs shifted vs off-by-one
length).

## See Also

- [symptom-to-suspect](symptom-to-suspect.md) - The zeros-that-validate and shifted-data shapes this rule catches
- [fuzz-measure-clean-build](fuzz-measure-clean-build.md) - Fuzzer PASS is validation-only; don't mistake it for correctness
