# ctx-thread-through-passes

> Thread `&ctx` / `&mut ctx` explicitly through every phase; avoid global mutable state

## Why It Matters

Once global state lives on one context, the data flow stays visible only if you pass that context explicitly. yel's `Compiler` owns a single `CompilerContext`, and each phase (`parse`, `lower_to_hir`, `type_check`, `lower_to_lir`) takes `&mut self` / `&CompilerContext` and reads or writes it; lowering structs hold `ctx: &'ctx mut CompilerContext` or `ctx: &'a CompilerContext`. Explicit threading keeps ordering and mutation legible and testable, and sidesteps the reentrancy and initialization-order hazards of `static mut` and thread-locals.

## Bad

```rust
static mut CTX: Option<CompilerContext> = None;

fn type_check() {
    let ctx = unsafe { CTX.as_mut().unwrap() }; // hidden ordering & reentrancy hazards
}
```

## Good

```rust
struct Compiler { ctx: CompilerContext }

impl Compiler {
    fn type_check(&mut self) {
        let mut tc = TypeChecker { ctx: &mut self.ctx };
        tc.run();
    }
}

struct LowerToLir<'ctx> { ctx: &'ctx mut CompilerContext }
```

## See Also

- [ctx-central-context](ctx-central-context.md) - The single context being threaded
- [pass-explicit-phases](pass-explicit-phases.md) - Each phase is a discrete method over the context
