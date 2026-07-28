# Seam changes — contract change log

> **Append-only.** Rule:
> [`orchestrate-integrator-owns-seams`](../../.agents/skills/compiler-rewrite/rules/orchestrate-integrator-owns-seams.md)

Agents implement against seam types; they never edit them. When an agent finds
the contract genuinely wrong, it **stops and files a request here** — it does not
work around the seam locally and it does not change it unilaterally. Two agents
that each "fixed" the IR between them produce two reasonable designs that do not
compose, and the merge is not a merge; it is a third rewrite done by whoever is
holding the branch.

The integrator (rewrite owner) decides. The decision is recorded here whether it
was granted or refused — a refused request is as useful to the next agent as a
granted one, and stops the same request arriving three times.

## Format

```markdown
## YYYY-MM-DD — <seam> — <one-line summary>

**Requested by:** <agent / stage>
**Request:** what the agent hit and what it wants changed.
**Options considered:** at least two, with what each costs.
**Decision:** granted / refused / modified — and the reasoning.
**Blast radius:** which stages must be re-checked; which landed code changes.
```

## Log

## 2026-07-25 — stage 1 / `yelc-syntax` dependencies — `stacker` admitted

**Requested by:** stage-1 implementer, after review round 2.

**Request.** `yelc-syntax` takes a third-party dependency on `stacker` (0.1.24)
to guard `ast::visit::walk_expr`. This departs from the stage-1 definition of
done, which says the crate "depends only on `yelc-base`".

**Background.** `parse_binary`/`parse_postfix` are iterative loops, so
`a.b.b.b…` builds an arbitrarily long `Box<Expr>` spine from **valid,
diagnostic-free** input while `MAX_NESTING_DEPTH` reads 2 — anti-spec A11. Three
consumers overflowed: `green.text()`, green `Drop`, and `Expr` `Drop` were made
iterative (now pass at n=500,000). `walk_expr` could not be: the recursion runs
through the overridable `Visitor::visit_expr` hook, so flattening it into a
worklist would stop calling that hook on spine nodes, breaking the single-walker
rule (A3).

**Options considered.**
1. **`stacker`.** Same mechanism and same reason as rustc's own
   `rustc_data_structures::stack::ensure_sufficient_stack`. Cost: one
   third-party dependency on a frontend crate.
2. **Dependency-free restructuring** (`#[inline(never)]` frame splitting).
   Measured, not assumed: reached n=12,986 — still below the frozen parser's
   ~14,544 — so it would ship a ceiling under the oracle.
3. **Bound the chain length.** Rejected: on the parse-only comparison it is a
   narrowing (see the correction below).

**Decision: granted.** Three reasons. The DoD line meant **internal crate-graph
discipline** — no edge to the frozen tree, and no path from `yelc-lir`/
`yelc-codegen` to a frontend crate — not a ban on third-party crates;
`yelc-base` itself carries `serde`, `rustc-hash`, and `parking_lot`, so the
strict reading was never the operative one. The precedent is exact: rustc solves
this identical problem (unbounded recursion over user-controlled structure in a
compiler frontend) with this identical mechanism. And option 2 was measured to be
a known narrowing, which A10/A11 exist to prevent.

The DoD wording is corrected in `stage-1-syntax.md` to say what it meant.

**Blast radius.** `yelc-syntax` only. The rule that matters downstream is
unchanged and is checked per stage: `yelc-lir` and `yelc-codegen` have no
dependency path to any frontend crate.

### Correction to the measurement this decision rests on

The implementer reported "frozen aborts at n≈14,544, so every available bound
would be a narrowing." Verified, with a caveat the integrator found and the
implementer did not report:

| what was measured | threshold (`a.b` chain, release CLI) |
|---|---|
| frozen **parse only** (`yelc ast`) | survives 14,000, aborts by 16,000 |
| frozen **full pipeline** (`yelc check`) | survives 1,600, **aborts by 2,000** |

Both numbers are real; they measure different things. Parse-only is the right
oracle for a parser stage, so the decision stands. But the stronger claim —
"bounding would narrow the language" — does not survive intact: for any chain
between ~1,800 and ~14,544 the frozen *parser* returns an AST while the frozen
*product* crashes, so no such program has ever compiled. A crash is neither
acceptance nor rejection, and rejecting cleanly where the frozen compiler aborts
is an improvement, not a narrowing.

This does not change the outcome — iterative consumers plus `stacker` are
strictly better than any bound, because they handle inputs the frozen compiler
cannot. It changes the *justification*, and the distinction is recorded because
the next stage will face the same question about its own recursion and should not
inherit an argument that is stronger than its evidence.

---

*Below: entries pre-loaded at stage 0, before the questions were asked.*

---

Two entries are pre-loaded as **anticipated** requests, so the answer exists
before the question is asked:

### Anticipated — `TokenSet(u128)` capacity

Ark's `TokenSet` is a `u128` bitset, capping the grammar at 128 token kinds.
Yel's grammar is larger than ark's (elements, attributes, bindings,
interpolation, ranges, unit suffixes). **If the kind count exceeds 128, the seam
becomes `TokenSet([u64; N])` with the same `const fn` API** — granted in advance,
because it is a capacity fact about yel's grammar, not a design preference. The
agent counts the kinds *before* implementing and reports the number either way.

### Anticipated — diagnostics API shape

An agent porting ark's frontend will want ark's flat `ParseError` enum and its
`Vec<ParseErrorWithLocation>` return channel, because that is what the reference
does. **Refused in advance.** Yel's `diagnostic.rs` — builder, `ErrorCode`,
accumulating sink, `render(&SourceMap)` — is frozen infrastructure
([keep-list §1](keep-list.md#1--diagnostics--yel-coresrcdiagnosticrs)). Adding a
new `ErrorCode` variant is expected and needs no request. Changing the API shape
does, and the answer is no.
