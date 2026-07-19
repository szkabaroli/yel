# id-invalid-sentinel

> Reserve an explicit invalid sentinel and an `is_valid()` check instead of overloading `0`

## Why It Matters

Inside hot tables you sometimes need a "placeholder, to be filled later" id before the real value exists. Reaching for `0` is a trap: `0` is a genuine, valid index, so a stale placeholder reads the first entry instead of failing. A reserved sentinel value is unambiguous and keeps the common path branch-free. yel uses `DefId::INVALID = DefId(u32::MAX)` with `is_valid(self) -> bool { self.0 != u32::MAX }` (`crates/yel-core/src/ids.rs:18`). Where absence is part of the public API, prefer `Option<Id>`; reserve the sentinel for placeholders inside dense tables.

## Bad

```rust
let parent = DefId(0); // "no parent yet" — but def#0 is a real definition
if parent.0 != 0 { resolve(parent); } // silently skips def#0
```

## Good

```rust
impl DefId {
    pub const INVALID: DefId = DefId(u32::MAX);
    pub fn is_valid(self) -> bool { self.0 != u32::MAX }
}

let mut parent = DefId::INVALID; // unambiguous placeholder
// ... later, patched to the real id ...
if parent.is_valid() { resolve(parent); }

// when absence is part of the API, use Option instead:
fn parent_of(id: DefId) -> Option<DefId> { /* ... */ }
```

## See Also

- [id-newtype-index](id-newtype-index.md) - The newtype the sentinel lives on
- [diag-error-type-recovery](diag-error-type-recovery.md) - Placeholder values that keep a pass going after an error
