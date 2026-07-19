# id-newtype-index

> Wrap every entity index in a `u32` newtype (`DefId`, `ExprId`, …), never pass raw `usize`

## Why It Matters

A compiler juggles many parallel index spaces — definitions, expressions, locals, blocks — and a raw `usize` from one space silently indexes into another's table, producing wrong results with no compile error. A per-space newtype turns that mistake into a type error at zero runtime cost. yel defines one newtype per index space in `crates/yel-core/src/ids.rs` (`DefId(pub u32)`, `FieldIdx`, `VariantIdx`, `LocalId`, `ExprId`, `NodeId`, `BlockId`, `ForId`, `IfId`, `TreeBoundaryId`), each deriving `Copy`/`Eq`/`Hash` with `new(u32)`, `index() -> usize`, and a `Display` like `def#42`.

## Bad

```rust
fn lookup(items: &[DefItem], locals: &[LocalInfo], i: usize) -> &DefItem {
    &items[i] // nothing stops `i` being a local index, or an expr index
}
```

## Good

```rust
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

impl DefId {
    pub fn new(v: u32) -> Self { DefId(v) }
    pub fn index(self) -> usize { self.0 as usize }
}

impl std::fmt::Display for DefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "def#{}", self.0)
    }
}

fn lookup(items: &[DefItem], id: DefId) -> &DefItem {
    &items[id.index()] // a LocalId here is a type error
}
```

## See Also

- [id-indexvec](id-indexvec.md) - Store entities in a typed container keyed by the matching id
- [intern-strings](intern-strings.md) - The same handle idea applied to identifiers
