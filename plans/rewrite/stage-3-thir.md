# Stage 3 — `yelc-thir`                                status: not started

Replaces (frozen, never edited): `crates/yel-core/src/thir/`
Base: — · Started: — · Landed: —

> **Stub.** Written out fully before the stage is briefed.

## Brief

*To be written.* Bidirectional type checking, HIR → THIR.

Must honour:

- **Identical diagnostic *meaning* on the 23 diagnostic fixtures.** Same
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
| Closure capture analysis | `ThirClosure.captures` always `vec![]`; no LIR counterpart; capturing an enclosing local **panics** in codegen (`wasm/expr.rs:192`) — signals work via `$Comp` | model the value form regardless ([directions §4](directions.md#4--closures-are-a-value-and-the-new-irs-are-shaped-for-one)). Implementing is a separate scope call: there is no corpus program and no output to match |
| Function-type inference | stubbed (`:1655`) | same |
| Generics | none — `InternedTyKind` has no `TyVar`; `option` is registered with `payload: Ty::ERROR` and a "generic placeholder" comment | adopt [directions §3](directions.md#3--generics-are-monomorphization-by-name-not-a-type-system-feature) (monomorphization by name, no type variables), or keep the placeholder? |
| `match` | does not exist; conditionals special-cased | model the general form now so lowering has one path — [anti-spec B4](anti-spec.md#b4--no-special-cased-control-flow-where-a-general-form-exists) |
| `color`/`brush` as property types | rejected — two storage shapes for one name | unify, or keep rejecting *with the same diagnostic*? — [anti-spec C4](anti-spec.md#c4--no-type-whose-storage-shape-depends-on-where-it-appears) |

Implementing any of these **changes output** and therefore breaks the
differential. If chosen, it lands as its own enumerated divergence set with
fixtures, not as a side effect of the rewrite.

Open directions, to accept or reject when this stage is briefed. Neither is a
requirement — a direction is binding only once written into this brief.

- [§1 — builtins are a table, not a field per builtin](directions.md#1--builtins-are-a-table-not-a-field-per-builtin).
  Typeck holds 24 references to `ctx.known.*` named fields in the frozen tree;
  the direction is that it asks a table for the type scheme instead.
- [§2 — the stdlib is yel source, embedded in the binary](directions.md#2--the-stdlib-is-yel-source-embedded-in-the-binary)
  is **gated on the closure and generics rows of the table above**. Whichever way
  those gaps are decided, decide them knowing a source stdlib is the intended
  consumer. That is a reason to answer them, not a reason to implement them here.
- [§5 — handlers and closures are one concept, split by trigger](directions.md#5--handlers-and-closures-are-one-concept-split-by-trigger)
  is the **cheapest and safest** of the five: it emits the same blocks, so it
  changes no output at all. `ThirHandler` and `ThirClosure` differ only in
  degree, and the type system already disbelieves the split —
  `Child { bumped: { count += 1; } }` typechecks against `bumped: func()`.
  Stage 1 already removed the handler/binding split from the AST, so the
  classification lands here whichever way this goes.
  **The sub-decision is how the trigger is determined**, and §5 records two
  options: a keyword on the closure (explicit, but a frozen-surface change —
  required breaks every handler in the corpus, optional keeps the very inference
  it was meant to replace), or the trigger on the slot's *function type*,
  propagated by the `Check` direction typeck already dispatches on (no surface
  change, composes through bindings, but a function type gains a component that
  may reach the WIT boundary). Picking by default is how a language change gets
  made without anyone deciding to make one. Either choice owes the same test:
  the dependency set of a body of each trigger kind, asserted on a fixture.
- [§3 — generics are monomorphization by name](directions.md#3--generics-are-monomorphization-by-name-not-a-type-system-feature)
  is the cheap answer to the generics row, and **this stage is where it is
  decided**: it adds no `Ty` variant, no unification, and nothing below the
  frontend seam. Adopting it moves `len`/`some`/`none`/`list.get`/`append` out of
  "blocked" for §2; `filter` stays blocked on closures-as-values, which §3 does
  not address.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*
