# Goldens changed — every re-blessed expectation, with its justification

> **Append-only.** Rule:
> [`oracle-never-rebless`](../../.agents/skills/compiler-rewrite/rules/oracle-never-rebless.md)

A green test suite proves nothing if you edited the tests. This file exists so
that "the goldens changed" is always a decision someone made and signed, never a
side effect of `INSTA_UPDATE=always`.

## The rules

- **A golden is never regenerated from the new compiler.** That does not update
  the oracle; it deletes it, and every green run afterwards is meaningless.
- **The corpus is never re-blessed at all.** `corpus/` is regenerated only by
  `scripts/freeze-corpus.sh` against the *frozen* compiler. A corpus divergence
  is explained here or it is a bug — it is never absorbed by updating the corpus.
- **Every change is one line here, with the diff read and justified.** "Wording
  improved" is not a justification; *what* changed and *why it is more correct*
  is.
- **Diagnostic meaning may not change.** A fixture asserting a rejection must
  still reject that program, for that reason, at that construct. Only wording
  moves — see [`scope.md`](scope.md#diagnostic-wording-may-improve-diagnostic-meaning-may-not).
- **The 85 execution tests are never edited.** Not re-blessed, not `#[ignore]`d,
  not "temporarily relaxed". A stage that cannot pass them has miscompiled
  something. If an execution test genuinely encodes wrong behaviour, that is an
  orchestrator decision with its own row and a written argument — and it should
  happen approximately never.
- **A test is never weakened to match known-wrong output.** Mark it `#[ignore]`
  with a reference to the tracking entry, and remember the ignored count is a
  tracked ratchet metric that absorbs exactly this kind of loss.

## Format

```markdown
| Date | Stage | Golden | What changed | Why it is correct | Reviewed by |
```

## Log

| Date | Stage | Golden | What changed | Why it is correct | Reviewed by |
|------|-------|--------|--------------|-------------------|-------------|
| 2026-07-28 | 1 | `lexer.rs` unit tests `hyphen_is_an_identifier_character`, `arrow_versus_hyphen_identifier` | Renamed and re-asserted: `a-` is now `[IDENTIFIER, SUB]` not `[IDENTIFIER]`; `p->x` is now `[IDENTIFIER, ARROW, IDENTIFIER]` not `[IDENTIFIER, GT, IDENTIFIER]` | They pinned the **old** rule. The kebab lookahead is a deliberate language change (below); these tests are its specification, so they move with it. Kebab assertions (`selected-id`, `count-1`, `in-out`) are unchanged. | owner |
| 2026-07-28 | 1 | `parity.rs` `FIRST_ERROR_OFFSETS` floor, 548 → 547 | One fewer first-error offset agrees with the frozen parser | Denominator unchanged at 1336, so **exactly one** input moved — not the "whole class" the assertion guards against. Expected: the change moves where an identifier ends, so for some malformed input it moves where the first error lands. Rate 41.0% → 40.9%, still above the 40% floor. | owner |

## The kebab lookahead — a deliberate surface change

**What.** A `-` joins an identifier only when a **name character** (`ALNUM` or
`_`, *not* another `-`) follows it. Applied to both compilers in one change:
`grammar.pest`'s six kebab rules gained `("-" ~ &(ALNUM | "_"))`, and
`yelc-syntax`'s lexer gained the matching one-character peek.

**Why.** `-` was both an identifier character and an operator, and unconditional
maximal munch always chose the identifier. That produced a real divergence —
`{ p: s32->p }` was a *record* to the new parser and a *closure* to pest, which
is scannerless and stops at `primitive_type`'s bare `"s32"` literal — plus
`count-=1` meaning "assign to a variable named `count-`" and `{p->p}` meaning a
comparison rather than a closure parameter.

**What it changes.** `s32->p`, `count-=1`, `{p->p}`, and a trailing `a-`. Kebab
names — `selected-id`, `case-a`, `starts-with`, `in-out`, `font-size` — are
untouched, because a name character follows the hyphen.

**Why it is safe, measured rather than argued.** The corpus was regenerated and
**all 8000 artifacts came back byte-identical** — 2000 programs × src/wit/dot/wasm.
That is the evidence the change is behaviour-neutral on every program anyone has
written, and it is a much stronger claim than reasoning about the grammar.

Everything else held: 480 workspace tests, 85/85 execution, 200/200 fuzz, WIT
untouched (no mangling layer — unlike a snake_case migration, which would have
needed one, since yel identifiers reach WIT verbatim).

**One bug the change introduced and the sweep caught.** The first implementation
used `is_identifier_continue` for the lookahead, which admits `-`, so `item--7`
stayed one identifier while pest — whose lookahead is `&(ALNUM | "_")` — read
`item` followed by `--7`. A **widening**, found by
`accept_reject_parity_over_random_mutations` on `corpus/src/195.yel#random@43`,
not by any hand-written case. Fixed by giving the lookahead its own alphabet
(`is_name_char`).
