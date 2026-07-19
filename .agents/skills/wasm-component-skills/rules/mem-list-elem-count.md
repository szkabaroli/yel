# mem-list-elem-count

> A list is (ptr, length) where length counts elements, not bytes; store elements contiguously at the element's size and alignment

## Why It Matters

A `list<T>` crosses as a `(ptr, length)` pair, and the canonical ABI defines `length` as the **element count** — the byte span is `length × elem_size(T)`, computed on demand, never stored. The spec's `load_list` reads `(begin, length)` and hands them to `load_list_from_range`, which derives the byte length itself. Elements are laid out contiguously, each at `T`'s canonical size and alignment, and the base pointer must be aligned to `T`'s alignment. Two classic bugs follow from getting this wrong: storing a *byte* length where an element count is expected (the host then reads `elem_size×` too many elements), and packing elements without per-element alignment padding (every element after the first is misread). yel carries lists as a `(ptr, len)` fat pointer like strings (see [mem-fat-pointer-strings](mem-fat-pointer-strings.md)); for `list<T>` the `len` field is the count of `T`, and per-element stride follows the canonical size/alignment.

## Bad

```rust
// store the byte length and pack elements with no per-element alignment
let byte_len = count * elem_size;
emit_store_fat_ptr(addr, base, byte_len);   // host treats byte_len as a count
// elements written back-to-back, ignoring elem alignment -> misaligned reads
```

## Good

```rust
// length is the element count; stride respects element size + alignment
let stride = align_to(elem_size, elem_align);
debug_assert_eq!(base % elem_align, 0);     // base aligned to T
emit_store_fat_ptr(addr, base, count);      // count, not bytes
// element i lives at base + i * stride
```

## See Also

- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The shared (ptr, len) carrier
- [mem-canonical-alignment](mem-canonical-alignment.md) - The element size/alignment rules
- [abi-trap-on-invalid-lift](abi-trap-on-invalid-lift.md) - Bounds/alignment checks when loading the list
