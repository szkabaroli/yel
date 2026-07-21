# Plan: strings as GC byte arrays (`String` → `(ref $str_bytes)`, reuse the typed-list infra)

Status: **Stages 1–4 DONE** (flag `STRINGS_AS_GC = true`, tree green). Strings are
`$str_bytes = (array (mut i8))` end-to-end internally; `(ptr,len)` conversion is
confined to the WIT/host boundary via `emit_str_bytes_materialize` /
`emit_str_bytes_unmaterialize` + a dedicated `$str_bytes` (un)materializer registered
in the existing `gc_list_(un)materializer_fn_indices` tables. String ops reuse the
existing runtime helpers wrapped so every string VALUE between ops is a single ref.
`option<string>` stays a FlatGcStruct (a null `$str_bytes` would alias `none`).
Verified: full workspace green, positive fixtures 77/77, fuzz 69/100 (identical
baseline set), `list<string>.append` works, WIT boundary still `string`.
**Remaining: Stage 5** (delete `$fat_value` + the fat-pointer runtime helpers if no
internal user survives) and **Stage 6** (docs/ARCHITECTURE, remove the flag). Baseline:
fuzz 69/100 (identical failing set is the regression guard).

## Goal

Replace `InternalRepr::FatPointer` for `String` with a **typed GC byte array** —
`$str_bytes = (array (mut i8))` — so a string is a single GC ref everywhere
internally, exactly like a `list<u8>`. Linear-memory `(ptr, len)` survives **only
at the WIT host boundary** (canonical ABI), lifted/lowered through the same
materializer the typed-list getters/setters already use.

This makes `String` "just another typed list" internally and lets it reuse the
whole typed-array path (construct / index / len / diff / boundary lift-lower)
instead of the parallel fat-pointer machinery.

## Why (the payoff — this is a cleanup lever, not just a repr change)

- **Deletes `$fat_value` and ~40 box/unbox sites.** Today a string is boxed into
  `$fat_value` whenever it's a list/record/tuple element (`gc_types.rs`,
  `record_list.rs`, `accessors.rs`, `expr.rs` — ~31 `fat_value_type_idx` sites +
  ~9 struct.new/get). When a string *is* a GC ref, storing it as an element is a
  plain ref store — the box disappears. **This also fixes the `list<string>.append`
  bug** (docs/TECH_DEBT.md §4): the element is a ref, so `ArraySet` just works.
- **Deletes the fat-pointer internal repr** (`InternalRepr::FatPointer`) and its
  2-slot special-casing (~15 sites in `repr.rs`/`expr.rs`), plus the
  `store_fat_ptr` / `load_fat_ptr` / `pack_fat_ptr_to_i64` runtime helpers once no
  internal consumer remains.
- **Uniform signals:** a string signal becomes **1 GC ref field** on `$Comp_<i>`
  (like a list), not 2 i32 fields — the last multi-slot signal shape goes away.
- **Shrinks linear memory:** the string constant pool (`runtime/strings.rs`)
  becomes a passive data segment feeding `array.new_data`; combined with the
  already-removed per-signal memory and (future) globals-in-memory work, this is
  a step toward linear memory hosting *only* transient scratch — eventually
  letting `MemoryLayout` itself shrink.

## Current state (verified)

- `list<u8>` compiles/validates/round-trips today as `(array (mut i32))` — the
  typed-array list path is solid (for-loop diff, `list_append`, index, len).
- Strings are `FatPointer` `(ptr,len)`: 2 stack slots, 2 i32 signal fields, boxed
  to `$fat_value` as an aggregate element. Full surface map in the session notes
  (repr classification `repr.rs:127/183`; literals `expr.rs:2692/2710`; ops in
  `runtime/strings.rs` — concat/`*_to_string`/`starts_with`; boundary is a no-op
  pass-through because internal == canonical today).
- Reusable infra: `list_array_type_idx: HashMap<Ty,u32>` (array type per list Ty),
  `ArrayNewFixed` (expr.rs:1464/1508), the list<scalar> boundary getter
  (`cabi_realloc` + copy loop, accessors.rs ~830/1080) and setter unmaterializer,
  packed `StorageType::I8` (gc_types.rs:1688).

## Design decisions

1. **`$str_bytes = (array (mut i8))`** — packed i8 (byte semantics, half the memory
   of `array i32`). Distinct from `list<u8>`'s current `array i32`; optionally
   re-point `list<u8>` at the same packed type later (separate, optional).
2. **`internal_repr(String) = GcArrayRef($str_bytes)`**; `signal_storage_valtypes(String)
   = [ref $str_bytes]` (1 slot, was 2). Strings flow through every existing
   `GcArrayRef` producer/consumer branch.
3. **String literals** → passive data segment + **`array.new_data`** (new: add the
   instruction + a `DataSection` passive entry; the byte source is today's
   `StringData` pool). Fallback if we defer passive-data support: `ArrayNewFixed`
   with per-byte `i32.const` (exists, but bloats code for long literals).
4. **String ops rewritten on GC arrays**, reusing array primitives:
   `concat` = `array.new_default (sum len)` + `array.copy` per part;
   `starts_with` = `array.get_u` byte loop; `*_to_string` = build a `$str_bytes`
   from digits. Logic mirrors the current linear-memory helpers.
5. **WIT boundary** lift `(ref $str_bytes)` → `(ptr,len)` = **reuse the list<scalar>
   materializer** (`cabi_realloc` + `array.copy`-to-memory); lower `(ptr,len)` →
   array = the unmaterializer. String getters/setters/host-arg lowering
   (`set-attribute`, text content) route through it. This is the one place
   linear memory stays.
6. **Delete `$fat_value`** once no string is a fat pointer — element storage for a
   string field returns `(ref $str_bytes)` directly.

## Stages (each ends green: full suite + fuzz failing-set ⊇ baseline, i.e. ≥ 69/100)

- **Stage 0 — baseline.** Done this session: 69/100, 31-seed failing set recorded
  (`scratchpad/base_nums.txt`), workspace green.

- **Stage 1 — type + boundary, behind a flag.** Register `$str_bytes`. Add
  `internal_repr(String) = GcArrayRef` gated by a `strings_as_gc` bool so the
  fat-pointer path still works. Implement literal→array (`array.new_data`) and the
  boundary lift/lower reusing the list materializer. Target: one exported string
  signal round-trips through get/set with the flag on, for a single fixture.

- **Stage 2 — signals.** Flip string signals to the 1-slot GC ref field
  (`signal_storage_valtypes`, `slot_count_for_signal_ty`, the `SignalLayout` mirror
  + `debug_assert_eq!`). Reuse the GC struct read/write already used for list/record
  signals. Interpolation `"{s}"` reads the array.

- **Stage 3 — ops.** Rewrite `concat` / `starts_with` / `*_to_string` /
  interpolation glue onto `$str_bytes`. Delete the linear-memory versions in
  `runtime/strings.rs` as each migrates.

- **Stage 4 — strings-as-element.** Record/tuple/list string fields store
  `(ref $str_bytes)` instead of boxing to `$fat_value`. Update
  `record_field_storage_type` / `list_element_storage_type`. **Delete `$fat_value`**
  and the ~40 box/unbox sites. Verify `list<string>.append` now works → graduate
  the drag_drop fixture to `items.append(payload)` and delete the TECH_DEBT §4 entry.

- **Stage 5 — delete the fat-pointer path.** Remove `InternalRepr::FatPointer` (or
  reduce it to boundary-only), the 2-slot special-casing, `store_fat_ptr` /
  `load_fat_ptr` / `pack_fat_ptr_to_i64` if unused, and the `strings_as_gc` flag.
  Shrink/retire the linear string data pool (keep only what feeds `array.new_data`).

- **Stage 6 — cleanup + verify.** Fuzz ≥ baseline (aim to *improve* it — the
  append-boxing class of bugs is gone). Update `docs/ARCHITECTURE.md` (string repr),
  `docs/TECH_DEBT.md` §1.5/§4, and the string-repr session memory.

## Risks

1. **Boundary byte-exactness.** Host expects canonical `(ptr,len)` UTF-8 bytes; the
   lift must copy array bytes verbatim. Guard: the WIT snapshot + execution tests
   that assert getter output.
2. **`array.new_data` support.** New instruction + passive `DataSection`; if the
   encoder path needs work, Stage 1 falls back to `ArrayNewFixed`.
3. **Interpolation / concat hot paths** touch many sites; do Stage 3 op-by-op with
   fuzz after each.
4. **`char` vs byte** — a `char` is a scalar (not a string); unaffected, but audit
   any `char`↔string coercion.
5. **Snapshot churn** — type-section + WAT/DOT shift; regenerate per stage, eyeball
   a sample before bulk-accepting.

## Verification (every stage)

- `cargo test` (workspace) green; `cargo test -p yel-wasm-codegen` green.
- `cargo build --release -p yelc` then the 100-seed `yel-smith` fuzz; failing-seed
  set must stay ⊆ baseline (no new failures). Improvements welcome.
- `wasm-tools validate` + `wasm-tools component wit` on `positive/` fixtures.
