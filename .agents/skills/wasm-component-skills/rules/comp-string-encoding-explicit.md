# comp-string-encoding-explicit

> Pin the canonical string encoding (UTF-8) at the embed step and match it everywhere strings are lowered

## Why It Matters

The canonical ABI does not assume a string encoding — the component declares one, and `wit_component::embed_component_metadata` records it. That declared encoding tells the host how to interpret the `(ptr, len)` a guest hands back across a `string` boundary. If the embedded choice disagrees with how the guest actually stores characters, every string silently corrupts: lengths are read in the wrong unit, bytes are re-decoded as the wrong code units, and nothing traps to tell you why. yel always passes `StringEncoding::UTF8` to `embed_component_metadata` (in both `wasm/mod.rs::generate_wasm_module_with_wit` and `wasm/functions.rs::generate_component`), and its runtime stores strings as UTF-8 `(ptr, len)` byte buffers in linear memory (`wasm/runtime/strings.rs`). The two must be chosen together, not independently.

## Bad

```rust
// guest stores UTF-8 bytes, but the component is told its strings are UTF-16
embed_component_metadata(&mut bytes, &resolve, world_id, StringEncoding::UTF16)?;
// no validation error — the host just decodes garbage on every string
```

## Good

```rust
// declared encoding == how runtime/strings.rs actually lays bytes out
embed_component_metadata(&mut bytes, &resolve, world_id, StringEncoding::UTF8)?;
// guest writes UTF-8 (ptr, len); host reads UTF-8 (ptr, len) — they agree
```

## See Also

- [comp-encode-from-core](comp-encode-from-core.md) - The embed step where the encoding is fixed
- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The `(ptr, len)` representation that must match
