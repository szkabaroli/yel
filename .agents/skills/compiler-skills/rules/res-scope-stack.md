# res-scope-stack

> Manage locals with a push/pop scope stack that supports shadowing

## Why It Matters

Lexical scoping needs nested blocks to define locals that vanish at block exit while inner names shadow outer ones. But once a `LocalId` is minted it must stay valid forever (later phases still reference it), so visibility and storage have to be tracked separately. yel's `LocalScope` keeps `locals: IndexVec<LocalId, LocalInfo>` holding *every* local ever defined, while `current: HashMap<Name, LocalId>` and `stack: Vec<HashMap<Name, LocalId>>` track only what is visible right now.

## Bad

```rust
// Single map: exiting a block can't restore shadowed names,
// and removing entries would invalidate ids other phases hold.
struct Scope { names: HashMap<Name, LocalId> }
fn define(&mut self, name: Name, id: LocalId) {
    self.names.insert(name, id); // shadow lost forever on overwrite
}
```

## Good

```rust
struct LocalScope {
    locals: IndexVec<LocalId, LocalInfo>,   // never shrinks; ids stay valid
    current: HashMap<Name, LocalId>,
    stack: Vec<HashMap<Name, LocalId>>,
}

impl LocalScope {
    fn push_scope(&mut self) { self.stack.push(std::mem::take(&mut self.current)); }
    fn pop_scope(&mut self) { self.current = self.stack.pop().unwrap_or_default(); }

    fn define(&mut self, name: Name, ty: Ty, span: Span) -> LocalId {
        let id = self.locals.push(LocalInfo { ty, span });
        self.current.insert(name, id);
        id
    }
    fn lookup(&self, name: Name) -> Option<LocalId> {
        self.current.get(&name).or_else(|| self.stack.iter().rev().find_map(|s| s.get(&name))).copied()
    }
}

// The type checker brackets each loop/branch body, e.g. patching the
// loop item's type once the iterable type is known.
scope.push_scope();
let item = scope.define(item_name, Ty::INFER, span);
check_body(&mut scope, body);
scope.locals[item].ty = element_ty_of(iterable_ty);
scope.pop_scope();
```

## See Also

- [res-namespaced-defs](res-namespaced-defs.md) - Top-level defs that locals shadow
- [ty-bidirectional](ty-bidirectional.md) - The checker drives push/pop around bodies
