# orchestrate-integrator-owns-seams

> Agents may not edit shared types — a needed seam change comes back as a request, not a commit

## Why It Matters

A stage agent hits a wall: the contract's `ThirExpr` has no place to record the
one fact its lowering needs. The locally rational move is to add a field. It
compiles, its tests pass, it reports done.

Meanwhile the agent on the other side of that seam is working from the merged
contract, which no longer describes reality. Every parallel stage silently
forked at that commit, and nobody will find out until integration — where the
symptom is a type error whose cause is three weeks and two agents away.

The rule is a hard boundary: **shared types are orchestrator-owned.** Inside its
stage an agent has total freedom; at the seam it has none. A wall at the seam is
useful information — usually that the contract is genuinely wrong — and it needs
to be routed to the one participant who can see both sides.

This is also the mechanism that keeps the frozen/free split enforceable. If
agents can edit seam types, they can change the language, the WIT, or the stage
decomposition as a side effect of an implementation detail.

## Bad

```rust
// Agent working on lower_to_lir, blocked on missing info:
 pub struct ThirExpr {
     pub id: ExprId,
     pub kind: ThirExprKind,
     pub ty: Ty,
     pub span: Span,
+    pub lowering_hint: Option<LoweringHint>,   // "just need this one thing"
 }
```

Nothing stops it, nothing catches it, and the typeck agent's branch — which
produces `ThirExpr` — has no idea the field exists, let alone how to populate
it. Note also what the field *is*: a side channel, straight off the anti-spec.

## Good

The agent stops and files a seam-change request instead:

```markdown
## Seam change request — THIR → LIR

**Blocked on:** lowering `for` needs to know whether the iterated expression is
a signal read, to decide between a static and a reactive block.

**Why the contract doesn't cover it:** T4 guarantees every expr carries a `Ty`,
but signal-ness is a property of the *binding*, not the type.

**Options considered**
  A. New field on ThirExpr — rejected, side channel, and only meaningful for
     one variant.
  B. Recompute in lowering by walking bindings — works, but duplicates
     signalck's traversal (anti-duplicate-walker).
  C. Extend the existing SignalDependencies side table with a
     `reads_signal(ExprId) -> bool` query, populated by signalck.

**Recommendation:** C. It reuses the analysis that already exists and keeps the
fact in the table that owns signal knowledge.

**Blocking?** No — proceeding on the non-reactive path; the `for` arm is a
`todo!("await seam decision C")` so it fails loudly rather than lowering wrong.
```

The orchestrator decides, lands the contract change on `main`, and notifies
every in-flight agent that the seam moved. Three properties matter here: the
agent kept working, the gap fails loudly rather than silently, and the decision
was made once by someone holding both sides.

Practically:

- Seam files carry a header comment: `// SEAM — orchestrator-owned. Changes go
  through a seam-change request, not a commit.`
- A stage PR touching a seam file is rejected on sight, regardless of quality.
- Contract changes land as their own commit on `main`, never inside a stage PR.
- When a seam moves, tell every in-flight agent explicitly — a rebase is not a
  notification.

## See Also

- [contract-before-fanout](contract-before-fanout.md) - The contract this rule protects
- [scope-frozen-vs-free](scope-frozen-vs-free.md) - The wider set of decisions that are not the agent's
- [`anti-side-channel-ir`](../../compiler-skills/rules/anti-side-channel-ir.md) - Why the "one extra field" instinct is usually wrong
