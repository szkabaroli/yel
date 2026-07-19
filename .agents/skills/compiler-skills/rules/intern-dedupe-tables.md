# intern-dedupe-tables

> Back every interner with a cache map so equal inputs return the same handle

## Why It Matters

Interning only pays off if equal inputs deterministically map to the same handle — otherwise you get duplicate entries and handle equality stops meaning value equality. The invariant is always "look up first, insert only on miss," which requires a cache map alongside the storage vector. yel applies this shape uniformly: the type interner, the string `Interner`, and the LIR string table (`string_map: HashMap<String, StringId>`) all dedupe this way. Note that interning is opt-in per table — yel's LIR *expression* table does NOT yet dedup (a documented TODO), so don't assume every table is deduplicated.

## Bad

```rust
impl TypeInterner {
    pub fn intern(&mut self, kind: InternedTyKind) -> Ty {
        let ty = Ty(self.types.len() as u32);
        self.types.push(kind); // never checks for an existing entry
        ty                     // equal kinds get DIFFERENT handles -> eq breaks
    }
}
```

## Good

```rust
// crates/yel-core/src/types/interner.rs
impl TypeInterner {
    pub fn intern(&mut self, kind: InternedTyKind) -> Ty {
        if let Some(&ty) = self.cache.get(&kind) {
            return ty; // look up first
        }
        let ty = Ty(self.types.len() as u32);
        self.types.push(kind.clone());
        self.cache.insert(kind, ty); // insert only on miss
        ty
    }
}

// same shape backs the string Interner and the LIR string table:
//   string_map: HashMap<String, StringId>
```

## See Also

- [intern-strings](intern-strings.md) - The string interner uses the identical lookup-then-insert shape
- [intern-types](intern-types.md) - What deduped type handles buy you (O(1) equality)
- [pass-postpass-dedupe](pass-postpass-dedupe.md) - Deduping tables that aren't interned up front (e.g. the LIR expression TODO)
