# abi-variant-discriminant

> Size a variant's discriminant by its case count, then place the payload at the maximum case alignment

## Why It Matters

A variant in linear memory is a discriminant followed by the active case's payload, and the canonical ABI fixes both pieces exactly. The discriminant's integer type is chosen by case count — `discriminant_type(cases)` returns `u8` for up to 256 cases, `u16` up to 65 536, else `u32` (it computes `ceil(log2(n)/8)`). The payload does *not* start right after the discriminant: it starts at the offset rounded up to `max_case_alignment` (the largest alignment of any case payload), and the whole variant's size is the discriminant plus the largest case payload, rounded up to the variant's overall alignment. The spec's `elem_size_variant` is the authority. Hardcode a 4-byte tag, or skip the max-case-alignment padding, and the host reads the payload from the wrong offset for every case.

## Bad

```rust
// fixed i32 tag, payload glued on immediately after — wrong offset whenever
// a case payload needs >4-byte alignment (e.g. an f64 or u64 field)
let tag_size = 4;
let payload_off = tag_size; // ignores max_case_alignment
```

## Good

```rust
let disc = discriminant_type(cases.len());      // u8 / u16 / u32 by count
let payload_off = align_to(size_of(disc), max_case_alignment(&cases));
let size = align_to(payload_off + max_case_payload_size(&cases),
                    variant_alignment(&cases)); // = max(disc, case aligns)
```

## See Also

- [abi-despecialize-first](abi-despecialize-first.md) - Get to the variant form first
- [abi-flatten-variant-join](abi-flatten-variant-join.md) - The flattened (non-memory) form
- [mem-canonical-alignment](mem-canonical-alignment.md) - The align_to / round-up rules used here
