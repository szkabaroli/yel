# Stage 3 — `yelc-thir`                                status: not started

Replaces (frozen, never edited): `crates/yel-core/src/thir/`
Base: — · Started: — · Landed: —

> **Stub.** Written out fully before the stage is briefed.

## Brief

*To be written.* Bidirectional type checking, HIR → THIR.

Must honour:

- **Identical diagnostic *meaning* on the 46 diagnostic fixtures.** Same
  rejection, same reason, same construct. Wording may improve, with the diff read
  and recorded in [`goldens-changed.md`](goldens-changed.md) — never re-blessed.
- **Accumulate and continue.** Recover with `Ty::ERROR` and keep checking; the
  driver bails between phases on `has_errors()`.
  [keep-list §6](keep-list.md#6--accumulate-and-continue-error-policy).
- The THIR visitor split already landed in the frozen tree
  (`thir/visit.rs` — `ThirVisitor` + `walk_expr`/`walk_stmt`, exhaustive, with a
  `visit_closure` descent hook). It is **the model to carry forward**, not debt:
  it is the one place §6.1's duplicated-walker problem was actually solved.
- Split `typeck.rs` (2.8k in the frozen tree) —
  [anti-spec A2](anti-spec.md#a2--no-god-pass).

Known gaps in the frozen tree that this stage inherits as *decisions*, not
copies — each needs a written call before the agent starts:

| Gap | Frozen behaviour | Question for stage 3 |
|---|---|---|
| Closure capture analysis | stubbed (`typeck.rs:978,1652` "TODO: capture analysis") | implement, or keep stubbed with a loud `todo!`? |
| Function-type inference | stubbed (`:1655`) | same |
| `match` | does not exist; conditionals special-cased | model the general form now so lowering has one path — [anti-spec B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists) |
| `color`/`brush` as property types | rejected — two storage shapes for one name | unify, or keep rejecting *with the same diagnostic*? — [anti-spec C4](anti-spec.md#c4--no-type-whose-storage-shape-depends-on-where-it-appears) |

Implementing any of these **changes output** and therefore breaks the
differential. If chosen, it lands as its own enumerated divergence set with
fixtures, not as a side effect of the rewrite.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*
