# intern-types

> Intern types so structural equality collapses to an integer comparison

## Why It Matters

Type checking asks "are these two types equal?" millions of times, and types are recursive (`list<option<s32>>`). Structurally walking two type trees on every comparison is slow and allocation-heavy. By interning, each distinct type structure gets a single `Ty` handle, so equal kinds share one handle and `list<s32> == list<s32>` reduces to `u32 == u32`. yel's `Ty(pub u32)` is `Copy`, recursive types hold handles instead of boxes, and `kind(ty)` recovers the structure when you actually need to inspect it.

## Bad

```rust
#[derive(Clone, PartialEq)]
enum Ty {
    List(Box<Ty>),                 // heap node per level
    Tuple(Vec<Ty>),
    Func { params: Vec<Ty>, ret: Option<Box<Ty>> },
}
// `a == b` recursively walks both trees every time
```

## Good

```rust
// crates/yel-core/src/types/interner.rs
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(pub u32); // a handle, Copy, cheap to compare

enum InternedTyKind {
    List(Ty),                       // children are handles, not boxes
    Option(Ty),
    Tuple(Vec<Ty>),
    Func { params: Vec<Ty>, ret: Option<Ty> },
}

pub struct TypeInterner {
    cache: FxHashMap<InternedTyKind, Ty>,
    types: Vec<InternedTyKind>,
}

impl TypeInterner {
    pub fn kind(&self, ty: Ty) -> &InternedTyKind {
        &self.types[ty.0 as usize] // recover structure on demand
    }
}

// list<s32> == list<s32>  ==>  Ty(7) == Ty(7)  ==>  u32 == u32
```

## See Also

- [intern-preintern-constants](intern-preintern-constants.md) - Reserve fixed `Ty` handles for common types like `BOOL`
- [ir-handles-over-boxes](ir-handles-over-boxes.md) - Recursive IR nodes reference children by handle
- [ty-literal-polymorphism](ty-literal-polymorphism.md) - How literal types unify under this scheme
