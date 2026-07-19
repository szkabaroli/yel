# cg-arena-traits

> Abstract IR storage behind read traits so multiple frontends can reuse one backend

## Why It Matters

If the backend reads a concrete IR type, only that one IR shape can ever feed it. yel's codegen reads LIR through small read-only traits (crates/yel-core/src/lir/arena.rs) — `LirExprArena`, `LirStringArena`, `LirSlotArena`, the aggregate `LirComponentArena`, and `LirFunctionLike` — so the WASM body emitter is shared by both `LirResource` (the UI component) and the flow-graph frontend's per-function adapter without baking in the `LirResource` shape. Splitting exprs from strings means a caller with no strings simply doesn't implement `LirStringArena` instead of stubbing a panicking method.

## Bad

```rust
// Backend hard-wired to one concrete IR type
fn emit_body(res: &LirResource) { ... } // only LirResource can ever feed codegen
```

## Good

```rust
trait LirExprArena   { fn expr(&self, id: ExprId) -> &LirExpr; }
trait LirStringArena { fn string(&self, id: StrId) -> &str; }
trait LirSlotArena   { fn slot(&self, id: SlotId) -> &SlotInfo; }
trait LirComponentArena: LirExprArena + LirStringArena + LirSlotArena {}

fn emit_body(c: &impl LirComponentArena) { ... } // shared by every frontend
```

## See Also

- [cg-late-binding-refs](cg-late-binding-refs.md) - Symbolic refs the shared emitter resolves at codegen time
- [ctx-central-context](ctx-central-context.md) - Same decoupling, applied to compiler-wide state
