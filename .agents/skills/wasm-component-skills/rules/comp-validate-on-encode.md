# comp-validate-on-encode

> Encode with validation enabled; a component that fails to validate is worse than no output

## Why It Matters

A component that encodes but does not validate is a latent failure: it will either be rejected by the host's loader or trap at the first canonical-ABI mismatch, far from the emitter that produced the bad bytes. Validating *at encode time* moves that failure to the moment of creation, where you still have the IR and the core module in hand and can attach context. yel calls `.validate(true)` on the `ComponentEncoder` in every encode path (`wasm/mod.rs::generate_wasm_module_with_wit` and `wasm/functions.rs::generate_component`), and on failure unwinds the full `anyhow` source chain and maps the deepest byte offset back to a core function via `augment_with_context` / `locate_function_at_offset`, turning an opaque "failed to validate component output" into a pointer at the misbehaving function. Pair this with an external `wasmparser::Validator` / `wasm-tools validate` pass in tests as defense in depth.

## Bad

```rust
// skip validation to "save time" — ships a component that traps in the host
let bytes = ComponentEncoder::default()
    .module(&core_bytes)?
    .validate(false)
    .encode()?;
```

## Good

```rust
// validate at the source; enrich the error with the offending core function
let bytes = ComponentEncoder::default()
    .module(&core_bytes)?
    .validate(true)
    .encode()
    .map_err(|e| {
        let mut msg = format!("Failed to encode component: {e}");
        msg.push_str(&augment_with_context(&core_bytes, &msg)); // offset -> func
        CodegenError::EncodingError(msg)
    })?;
```

## See Also

- [val-validate-component](val-validate-component.md) - External validation pass in tests
- [comp-encode-from-core](comp-encode-from-core.md) - The encode pipeline this guards
