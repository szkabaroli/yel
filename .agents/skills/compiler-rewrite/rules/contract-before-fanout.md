# contract-before-fanout

> The seam types land on `main` first; agents implement against them, never invent them

## Why It Matters

Two agents rewriting adjacent stages will each design the IR between them. Both
designs will be reasonable. Neither will match. The mismatch surfaces at
integration, when both stages are finished and neither author is available — and
the merge is not a merge, it is a third rewrite done by whoever is holding the
branch.

The fix is boring and it is the whole game: **the interface is written, reviewed,
and merged to `main` before either side is implemented.** A stage brief then
says "here is your input type, here is your output type, here are the invariants
you may assume and the ones you must establish" — and the two sides compose by
construction because they were never free to disagree.

A contract is not a paragraph of prose. It is compiling Rust plus a short list of
invariants, because prose seams drift and `cargo check` does not.

## Bad

```markdown
Agent A brief: "Rewrite HIR. Output a clean typed-ready tree."
Agent B brief: "Rewrite typeck. Consume the HIR and produce THIR."
```

Agent A returns a tree with interned `Name`s and scope info attached to nodes.
Agent B wrote a checker expecting a flat arena with a separate scope table. Both
are done, tested against their own fixtures, and mutually unusable.

## Good

Land this **before** either brief goes out:

```rust
// crates/yel-core/src/hir/mod.rs — SEAM: AST → HIR → typeck. Frozen for stage 2/3.
pub struct HirModule {
    pub items: IndexVec<HirItemId, HirItem>,
    pub exprs: IndexVec<HirExprId, HirExpr>,
    pub scopes: ScopeTable,          // resolution side table, keyed by HirExprId
}

// Invariants HIR lowering ESTABLISHES (typeck may assume all of them):
//   H1. Every HirExprId in `exprs` is reachable from exactly one item.
//   H2. Every node carries a Span that maps into the SourceMap.
//   H3. Names are interned; no `String` survives past lowering.
//   H4. Forward references resolve: all items registered before any body lowers.
//   H5. Ill-formed input produces a diagnostic AND a recovery node — never a panic.
//
// Invariants typeck MUST NOT rely on (explicitly out of contract):
//   - No ordering guarantee between sibling items beyond source order.
//   - Scope table is not valid after any HIR mutation.
```

The brief then reduces to: *"Establish H1–H5. Do not change the types in this
file; if you need to, stop and file a seam-change request."*

Practical rules for writing one:

- **Types first, prose second.** If the invariant cannot be stated against a
  named type, the type is wrong.
- **State what the consumer may *not* assume.** Under-specified guarantees get
  depended on accidentally and become permanent.
- **Put the invariants next to the types**, not in a doc that rots separately.
- **One seam per PR**, reviewed by whoever owns both sides.
- **Assert the cheap ones** in a `debug_assert` validator the producing stage
  runs in tests — an invariant nothing checks is a comment.

## See Also

- [orchestrate-integrator-owns-seams](orchestrate-integrator-owns-seams.md) - Handling a justified seam-change request
- [orchestrate-one-agent-one-stage](orchestrate-one-agent-one-stage.md) - What the brief looks like around the contract
- [anti-spec-from-tech-debt](anti-spec-from-tech-debt.md) - The prohibitions that ship with it
