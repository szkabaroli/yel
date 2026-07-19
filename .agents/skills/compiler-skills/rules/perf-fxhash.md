# perf-fxhash

> Use a fast non-cryptographic hasher (`rustc-hash` / `FxHashMap`) for internal compiler maps — they have no DoS surface

## Why It Matters

A compiler builds an enormous number of internal hash maps — interner caches, definition tables, dependency sets, dedup tables. The standard library's default hasher (SipHash) is DoS-resistant, which matters for maps keyed by untrusted network input but is pure overhead for compiler-internal maps whose keys are your own ids and strings. Swapping in `FxHashMap` (the hasher rustc itself uses) is a free, measurable speedup. yel uses `rustc_hash::FxHashMap` for its string interner and type-interner cache, and the workspace `Cargo.toml` documents the choice: "Fast non-cryptographic hasher for internal compiler maps (no DoS surface)."

## Bad

```rust
use std::collections::HashMap; // SipHash — DoS-resistant but slow

struct TypeInterner {
    cache: HashMap<InternedTyKind, Ty>, // hashed on a hot path, no adversary
}
```

## Good

```rust
use rustc_hash::FxHashMap as HashMap; // fast, deterministic, non-cryptographic

struct TypeInterner {
    cache: HashMap<InternedTyKind, Ty>,
}
```

## Caveat

Only for **internal** maps. Any map keyed by untrusted external input (a network request, an uploaded file's contents) should keep a DoS-resistant hasher.

## See Also

- [intern-dedupe-tables](intern-dedupe-tables.md) - The cache maps this speeds up
- [intern-strings](intern-strings.md) - String interner backed by an FxHashMap
- [intern-types](intern-types.md) - Type interner cache
