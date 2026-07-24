# greenfield-never-touch-old-code

> The existing compiler is frozen, read-only reference. The rewrite is new crates beside it — not edits inside it.

## Why It Matters

The instinct is to rewrite in place: replace `hir/lower.rs`, keep the call
sites, migrate gradually. It feels lower-risk and it destroys the thing the
whole method depends on. **The old compiler is the specification and the
differential baseline.** The moment an agent edits it, the reference has been
modified by the same process that is supposed to be checked against it, and
every number measured afterward compares the new code to a moving target.

Freezing the old tree buys four things at once:

- **A stable oracle.** `YEL_OLD` behaves identically in week 1 and week 20, so a
  divergence found in stage 5 is unambiguously stage 5's.
- **`main` never breaks.** Nothing the rewrite does can regress the shipping
  compiler, because the shipping compiler is not being edited.
- **Clean agent boundaries.** "Do not modify any file under `crates/yel-core/`
  or `crates/yel-wasm-codegen/`" is a rule an agent cannot accidentally violate
  and a reviewer can check with `git diff --name-only`.
- **No merge conflicts between the rewrite and ongoing bug fixes.** They live in
  different directories.

The cost is one final cutover instead of a gradual one. That is the right trade:
gradual in-place migration is exactly how the current codebase acquired its
transitional bridges (`docs/TECH_DEBT.md §1`).

## Bad

```rust
// Stage 2 agent, "migrating" HIR lowering in place:
 pub fn lower_to_hir(&mut self, ctx: &mut CompilerContext) -> HirModule {
-    self.lower_items(ctx)
+    if cfg!(feature = "new-hir") { self.lower_items_v2(ctx) } else { self.lower_items(ctx) }
 }
```

`yel-core` is now partly rewritten and partly not. The differential baseline
lives in the same crate as the thing being tested, `main` carries a half-migrated
compiler, and every subsequent stage inherits a tree that no longer matches the
frozen corpus's provenance.

## Good

New crates, beside the old ones, never inside them:

```
crates/
  yel-core/           ← FROZEN. Read constantly, edited never.
  yel-wasm-codegen/   ← FROZEN.
  yelc/               ← FROZEN.

  yel2-syntax/        ← stage 1: lexer, parser, green tree, AST
  yel2-hir/           ← stage 2
  yel2-thir/          ← stage 3
  yel2-lir/           ← stage 4
  yel2-codegen/       ← stage 5
  yelc2/              ← the new driver; also hosts the differential runner
```

Rules that keep the freeze real:

- **The agent brief says it explicitly**, in the definition of done: *"This diff
  must not contain a single change under `crates/yel-core/`,
  `crates/yel-wasm-codegen/`, or `crates/yelc/`. If you believe you need one,
  file a seam-change request."*
- **Reviewers check it mechanically**, before reading any code:
  `git diff --name-only main... | grep -E '^crates/(yel-core|yel-wasm-codegen|yelc)/' && echo VIOLATION`
- **Reading the old code is not just allowed, it is the job.** Frozen means
  unmodified, not off-limits. Every stage brief should point at the specific old
  files that encode the behaviour being reproduced.
- **The two trees share nothing mutable.** If the new tree wants `diagnostic.rs`,
  it gets a *copy* in `yel2-*` (see
  [`keep-diagnostics-infrastructure`](keep-diagnostics-infrastructure.md)), not a
  dependency on the frozen crate — a shared dependency is an edge along which
  the freeze eventually breaks.
- **Bug fixes to the old compiler still happen** if the shipping product needs
  them. They are ordinary work on frozen-for-*the-rewrite* code; when one lands,
  regenerate the corpus and note it in the ratchet as a new baseline row.
- **The old tree is deleted exactly once**, in the final cutover, after every
  stage is at parity — see [`cutover-switch-then-delete`](cutover-switch-then-delete.md).

## See Also

- [cutover-switch-then-delete](cutover-switch-then-delete.md) - How the frozen tree eventually goes away
- [verify-differential-not-review](verify-differential-not-review.md) - What the frozen tree is for
- [orchestrate-integrator-owns-seams](orchestrate-integrator-owns-seams.md) - The other hard boundary on agent edits
