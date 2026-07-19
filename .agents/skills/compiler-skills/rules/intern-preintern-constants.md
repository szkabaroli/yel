# intern-preintern-constants

> Pre-intern common types/symbols at fixed indices and assert the constants on startup

## Why It Matters

Hot paths constantly reference a handful of primitive types — `bool`, `s32`, `string`, the error type. Looking those up in the interner map every time is wasteful when they could be compile-time constants. yel pre-interns them in `TypeInterner::new()` at fixed indices (`Ty::ERROR = Ty(0)`, `UNIT = 1`, `BOOL = 2`, `S32 = 3`, `STRING = 4`) and immediately asserts each insertion landed on the expected handle. This lets code write `Ty::BOOL` as a plain const, while the asserts guarantee the constants never drift out of sync with the table.

## Bad

```rust
const BOOL: Ty = Ty(2); // magic number; nothing guarantees index 2 is bool

impl TypeInterner {
    pub fn new() -> Self {
        let mut t = Self::default();
        t.intern(InternedTyKind::Bool); // order could change; BOOL silently wrong
        t
    }
}
```

## Good

```rust
// crates/yel-core/src/types/interner.rs
impl Ty {
    pub const ERROR: Ty = Ty(0);
    pub const UNIT: Ty = Ty(1);
    pub const BOOL: Ty = Ty(2);
    pub const S32: Ty = Ty(3);
    pub const STRING: Ty = Ty(4);
}

impl TypeInterner {
    pub fn new() -> Self {
        let mut t = Self::default();
        // pre-intern in order, and prove the handles match the constants
        assert_eq!(t.intern(InternedTyKind::Error), Ty::ERROR);
        assert_eq!(t.intern(InternedTyKind::Unit), Ty::UNIT);
        assert_eq!(t.intern(InternedTyKind::Bool), Ty::BOOL);
        assert_eq!(t.intern(InternedTyKind::S32), Ty::S32);
        assert_eq!(t.intern(InternedTyKind::String), Ty::STRING);
        t
    }
}

// hot code now uses `Ty::BOOL` with no map lookup
```

## See Also

- [intern-types](intern-types.md) - The underlying type-interning scheme these constants index into
- [diag-error-type-recovery](diag-error-type-recovery.md) - `Ty::ERROR` as the recovery sentinel for type errors
