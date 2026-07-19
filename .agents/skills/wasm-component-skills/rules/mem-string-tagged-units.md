# mem-string-tagged-units

> Encode strings as (ptr, code-units) under the negotiated encoding; the byte length and unit depend on utf8 / utf16 / latin1+utf16

## Why It Matters

A `string` crosses as `(ptr, tagged_code_units)`, and the second field is a count of **code units**, not bytes — the byte length depends on the encoding fixed at the embed step. The spec's `load_string_from_range` shows all three cases: `utf8` → `byte_length = units`, alignment 1; `utf16` → `byte_length = 2 * units`, alignment 2; `latin1+utf16` → a high bit of the count tags the contents — set means UTF-16 (`byte_length = 2 * (units ^ tag)`), clear means Latin-1 (`byte_length = units`). The guest must store strings in exactly the encoding it declared and report the matching code-unit count; report a byte length where a code-unit count is expected (or store UTF-16 while declaring UTF-8) and every string silently corrupts. yel declares and stores **UTF-8** (see [comp-string-encoding-explicit](comp-string-encoding-explicit.md)), so its code-unit count equals the byte length — but that equality is a property of UTF-8, not a universal one to assume.

## Bad

```rust
// declared utf16 at embed time, but report byte length as the unit count
emit_store_fat_ptr(addr, ptr, byte_len); // host expects code units = byte_len/2
// host reads twice the intended length -> garbage tail / OOB
```

## Good

```rust
// UTF-8: units == bytes, so the count is the byte length (consistent w/ embed)
let code_units = utf8_byte_len;
emit_store_fat_ptr(addr, ptr, code_units);
// for utf16 you would report bytes/2; for latin1+utf16, set the high-bit tag
```

## See Also

- [comp-string-encoding-explicit](comp-string-encoding-explicit.md) - Pinning the encoding the units are counted in
- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The (ptr, len) carrier for strings
- [mem-list-elem-count](mem-list-elem-count.md) - The analogous count-not-bytes rule for lists
