# abi-flatten-variant-join

> Flatten a variant to its discriminant plus the positional join of all case payloads, and reinterpret values into the joined slots

## Why It Matters

When a variant is *flattened* (not stored in memory) its payload is not just one case's flat types — it is the per-position `join` of every case's flattened payload, so a single set of core locals can hold whichever case is active. The spec's `flatten_variant` walks each case and folds position `i` with `join(flat[i], case_flat[i])`, where `join(a, b)` is: equal → `a`; `{i32, f32}` → `i32`; anything else → `i64`. So if case A's first slot is `f32` and case B's is `i64`, the shared slot is `i64`. Because the slot type may differ from a case's native type, lowering a payload into it must **reinterpret**, not bitcopy: `f32 → i32` via `encode_float_as_i32`, `i32 → i64` by zero-extension, `f64 → i64` reinterpret; lifting does the inverse (`decode_i32_as_float`, `wrap_i64_to_i32`). Emit each case's own flat types directly and the locals won't line up across cases — the adapter reads the wrong bits.

## Bad

```rust
// use the active case's flat types as the variant's flattened payload
let payload = self.canonical_flat_valtypes(active_case_ty);
// case B (i64 first slot) and case A (f32 first slot) now disagree on the
// shared local's type — one of them reads reinterpreted garbage
```

## Good

```rust
// fold every case into a joined slot list, then coerce on lower/lift
let mut joined = vec![];
for case in &cases {
    for (i, ft) in self.canonical_flat_valtypes(case.ty).iter().enumerate() {
        joined[i] = if i < joined.len() { join(joined[i], *ft) } else { joined.push(*ft); *ft };
    }
}
// lowering case A: f32 -> i32 (reinterpret), i32 -> i64 (zero-extend), ...
```

## See Also

- [abi-variant-discriminant](abi-variant-discriminant.md) - The in-memory counterpart to this flat form
- [abi-flatten-at-boundary](abi-flatten-at-boundary.md) - Only flatten like this at the boundary
- [abi-respect-flattening-limit](abi-respect-flattening-limit.md) - The joined list still obeys the flatten budget
