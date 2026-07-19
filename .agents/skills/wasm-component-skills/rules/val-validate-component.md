# val-validate-component

> Validate the artifact with a component-model-aware validator (`wasm-tools` / `Validator`), not a magic-number check

## Why It Matters

A component can start with the right `\0asm` magic bytes and still be structurally invalid — wrong canonical-ABI adapters, a core type that doesn't match its component-level signature, a malformed instance section. A magic-number or "is it non-empty?" check passes all of those straight through to a host that then rejects or traps on them. The only meaningful gate is a validator that understands the *component model*. yel's tests run `wasmparser::Validator::new().validate_all(bytes)` (see `crates/yelc/tests/compile.rs::validate_wasm`), decoding any failure so the test names the offending offset rather than just "invalid". Pair it with `.validate(true)` on the encoder so bad bytes never escape the emitter.

## Bad

```rust
// "looks like wasm" is not "is a valid component"
assert_eq!(&bytes[0..4], b"\0asm"); // a structurally broken component passes this
```

## Good

```rust
// component-model-aware validation; surface the real error
fn validate_wasm(bytes: &[u8]) -> Result<(), String> {
    wasmparser::Validator::new()
        .validate_all(bytes)
        .map(|_| ())
        .map_err(|e| e.to_string()) // names the offending offset/section
}
```

## See Also

- [comp-validate-on-encode](comp-validate-on-encode.md) - Validating at the emitter, before bytes escape
- [val-snapshot-wit](val-snapshot-wit.md) - Snapshotting the interface as the other half of CI
