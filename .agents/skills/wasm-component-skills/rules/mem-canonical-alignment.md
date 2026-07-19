# mem-canonical-alignment

> Lay out aggregates in linear memory with the canonical ABI's size/alignment rules, or the host reads garbage

## Why It Matters

The canonical ABI specifies an exact size and alignment for every type when it is stored in linear memory. The primitive alignments are fixed: `bool`/`s8`/`u8` → 1, `s16`/`u16` → 2, `s32`/`u32`/`f32`/`char` → 4, `s64`/`u64`/`f64` → 8, and `string`/`list`/handles → the pointer size (4 on wasm32). A record's alignment is the **maximum** of its field alignments, and its size is computed the way the spec's `elem_size_record` does it: walk the fields, round the running offset up to each field's alignment (`align_to`), add the field's size, then round the *total* up to the record's alignment so arrays of the record tile correctly. When a guest writes an aggregate (a record, tuple, or list element) to memory for the host to lift, its byte layout — field offsets, inter-field padding, total stride — must match these rules byte-for-byte, or the host lifts wrong values from the wrong offsets. yel's memory writers bake the canonical offsets into shared helpers: `emit_store_fat_ptr` (`wasm/runtime/memory.rs`) stores a fat pointer as `ptr` at offset 0 and `len` at offset 4 (`align: 2`, i.e. 4-byte aligned), and the `emit_alloc` allocator rounds every allocation up to its requested alignment. Compute field offsets from the canonical size/alignment of each field type — never from ad-hoc packing that "looks contiguous."

## Bad

```rust
// pack a {i32, string} record by just summing widths, no alignment
let mut off = 0;
for field in record.fields {
    store_field(addr, off, field);
    off += field_byte_width(field); // ignores alignment + padding
}
// the host lifts each field at the ABI's aligned offset, not yours →
// it reads the i32's high bytes as the string ptr → garbage
```

## Good

```rust
// advance the cursor by the canonical alignment of each field
let mut off = 0u32;
for field in record.fields {
    let align = canonical_align(field.ty);
    off = align_up(off, align);          // pad to the field's alignment
    store_field(addr, off, field);
    off += canonical_size(field.ty);     // canonical stride, not raw width
}
```

## See Also

- [mem-fat-pointer-strings](mem-fat-pointer-strings.md) - The `(ptr @ +0, len @ +4)` layout this rule keeps consistent
- [abi-variant-discriminant](abi-variant-discriminant.md) - Variant layout uses these same align_to / round-up rules
- [abi-respect-flattening-limit](abi-respect-flattening-limit.md) - Spilled params/results land in a canonically-laid-out memory area
- [gc-hybrid-gc-and-memory](gc-hybrid-gc-and-memory.md) - When values live in both GC structs and linear memory
