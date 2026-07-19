# pass-explicit-phases

> Make the pipeline a sequence of named phases, each consuming one IR and producing the next

## Why It Matters

A compiler that exposes each phase as its own method with a clear input/output type makes the data flow obvious and each phase independently testable. yel's `crates/yel-core/src/compiler.rs` does exactly this: `parse` (source→AST), `lower_to_hir` (AST→HIR), `type_check` (HIR→THIR), `lower_to_lir` (THIR→LIR), plus globals variants, with the multi-file driver `lower_all` in `crates/yelc/src/pipeline.rs` sequencing them. A single tangled `compile()` hides the IR boundaries and makes it impossible to test or reuse one stage.

## Bad

```rust
fn compile(src: &str) -> Wasm {
    // parse, name-resolve, typecheck, lower, and emit all inlined here,
    // with intermediate state living in local variables nobody else can see
    let mut ast = /* ... */;
    /* 800 lines later */
    emit(&ast)
}
```

## Good

```rust
impl Compiler {
    pub fn parse(&mut self, src: &str) -> Ast { /* source -> AST */ }
    pub fn lower_to_hir(&mut self, ast: Ast) -> Hir { /* AST -> HIR */ }
    pub fn type_check(&mut self, hir: Hir) -> Thir { /* HIR -> THIR */ }
    pub fn lower_to_lir(&mut self, thir: Thir) -> Lir { /* THIR -> LIR */ }
}

// pipeline.rs sequences the named phases per file
fn lower_all(files: &[File]) -> Lir { /* parse -> hir -> thir -> lir */ }
```

## See Also

- [ir-layered-lowering](ir-layered-lowering.md) - Each phase targets a distinct IR layer
- [ctx-thread-through-passes](ctx-thread-through-passes.md) - Pass shared state explicitly between phases
