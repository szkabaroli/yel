# id-indexvec

> Store entities in a typed `IndexVec<I, T>` keyed by their own id type

## Why It Matters

Once every index space has its own newtype, the container that holds those entities should accept only the matching id, so you cannot read a `DefId` table with a `LocalId`. yel's `crates/yel-core/src/index_vec.rs` defines `IndexVec<I: Idx, T>` as a `Vec<T>` plus `PhantomData<fn(I) -> I>`; `push(value) -> I` returns the new index, `Index<I>`/`IndexMut<I>` only accept that id type, and `iter_enumerated()` yields `(I, &T)`. It is used for `items: IndexVec<DefId, DefItem>` in `definitions.rs` and `locals: IndexVec<LocalId, LocalInfo>`, with the `Idx` trait implemented for each newtype via a macro.

## Bad

```rust
let items: Vec<DefItem> = Vec::new();
let locals: Vec<LocalInfo> = Vec::new();
// both are plain Vec; nothing ties the index back to a space
let info = &locals[def_id.index()]; // wrong table, compiles fine
```

## Good

```rust
pub struct IndexVec<I: Idx, T> {
    raw: Vec<T>,
    _marker: PhantomData<fn(I) -> I>,
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn push(&mut self, value: T) -> I {
        let idx = I::new(self.raw.len());
        self.raw.push(value);
        idx
    }
}

let mut items: IndexVec<DefId, DefItem> = IndexVec::new();
let id: DefId = items.push(item);   // id space comes from the container
let info = &locals[id]; // type error: locals is IndexVec<LocalId, _>
```

## See Also

- [id-newtype-index](id-newtype-index.md) - The newtypes that key the container
- [res-scope-stack](res-scope-stack.md) - Resolving names into these id-keyed tables
