# ir-layered-lowering

> Use a distinct IR per abstraction level; each lowering adds information and removes ambiguity

## Why It Matters

A compiler that tries to typecheck and emit code directly off the parse tree forces every pass to re-derive facts the previous one already knew — resolution, types, control flow — repeatedly and inconsistently. A staged pipeline of purpose-built IRs lets each layer make the program strictly more explicit, so later passes are simpler and harder to get wrong. yel lowers AST → HIR → THIR → LIR → WASM: HIR (`crates/yel-core/src/hir/`) is a near-AST tree with names still as strings and no types; THIR (`crates/yel-core/src/thir/`) is fully resolved and typed; LIR (`crates/yel-core/src/lir/`) is block-based and close to the WASM target.

## Bad

```rust
// One IR (the raw AST) used for everything: types are looked up
// on the fly during codegen, names re-resolved at every use.
fn codegen(ast: &AstExpr, scope: &Scope) -> Wasm {
    match ast {
        AstExpr::Name(s) => {
            let def = scope.resolve(s)?;          // resolving during codegen
            let ty = infer_type(def, scope)?;     // typechecking during codegen
            emit_load(def, ty)
        }
        // every backend arm re-does frontend work, inconsistently
    }
}
```

## Good

```rust
// Each stage consumes the previous IR and produces a more explicit one.
fn lower_ast_to_hir(ast: &AstModule) -> HirModule { /* tree, names are strings */ }
fn check_hir_to_thir(hir: &HirModule) -> ThirModule { /* resolved + typed */ }
fn lower_thir_to_lir(thir: &ThirModule) -> LirModule { /* blocks + flat ops */ }
fn codegen_lir(lir: &LirModule) -> Wasm { /* mechanical: types & ids already decided */ }

// Codegen never resolves a name or infers a type — THIR already did.
```

## See Also

- [ir-kind-span-struct](ir-kind-span-struct.md) - shape each node within these IRs
- [pass-explicit-phases](pass-explicit-phases.md) - make the stage boundaries explicit phases
- [ir-handles-over-boxes](ir-handles-over-boxes.md) - how the late, flat IR references children
