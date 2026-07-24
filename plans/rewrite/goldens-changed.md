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

*Empty at stage 0. Baseline goldens are those at freeze `ccf2086`.*
