# intern-strings

> Intern identifiers/strings to small handles for O(1) equality and shared storage

## Why It Matters

A compiler compares identifiers constantly — name resolution, lookups, diagnostics. Comparing `String`s is O(length) and duplicates the same bytes across the program. Interning maps each distinct string to a small integer handle once, so equality becomes a single integer compare and the bytes are stored exactly once and shared. yel's `Interner` lives behind `Arc<Interner>` on the context, is thread-safe, and hands out `Name` handles that index back into shared `ArcStr` storage.

## Bad

```rust
struct Ident(String); // every clone copies bytes; eq is O(len)

fn same(a: &Ident, b: &Ident) -> bool {
    a.0 == b.0 // byte-by-byte comparison, repeated everywhere
}
```

## Good

```rust
// crates/yel-core/src/interner.rs
pub struct Name(usize);

struct Internal {
    map: FxHashMap<ArcStr, Name>,
    vec: Vec<ArcStr>,
}

pub struct Interner(Mutex<Internal>); // shared as Arc<Interner> on the context

impl Interner {
    pub fn intern(&self, s: &str) -> Name {
        let mut inner = self.0.lock();
        if let Some(&name) = inner.map.get(s) {
            return name; // existing handle
        }
        let name = Name(inner.vec.len());
        let arc: ArcStr = Arc::from(s);
        inner.vec.push(arc.clone());
        inner.map.insert(arc, name);
        name
    }

    pub fn str(&self, name: Name) -> ArcStr {
        self.0.lock().vec[name.0].clone() // recover the bytes
    }
}

// equality of identifiers is now just `name_a == name_b` (usize compare)
```

## See Also

- [intern-dedupe-tables](intern-dedupe-tables.md) - The cache-map invariant that makes intern return the same handle
- [id-newtype-index](id-newtype-index.md) - Wrap the returned handle (`Name`) in its own newtype
