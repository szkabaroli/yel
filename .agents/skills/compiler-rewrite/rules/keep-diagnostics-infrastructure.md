# keep-diagnostics-infrastructure

> "The internals are garbage" is not uniformly true — name the parts that are good and carry them over intact

## Why It Matters

A rewrite brief that says "the current code is a mess" gives an agent licence to
replace everything it touches, including the parts that are better than what it
would write. Diagnostics is the clearest case in yel: `diagnostic.rs` is 285
lines of genuinely good design — a builder API, a real `ErrorCode` enum, an
accumulating sink, span-aware rendering against the `SourceMap` — and it is
load-bearing for 23 diagnostic fixtures whose exact meaning is frozen.

An agent rewriting a stage will hit the error-reporting call sites first and
often replace them with whatever the reference implementation it was given uses.
That is how you end up porting *away* from a good design toward a flat
`ParseError` enum with `fn message(&self) -> String`, losing error codes,
structured notes, and multi-error accumulation in a change whose title says
"parser".

The general rule: **an inventory of what to keep is as much a part of the brief
as the anti-spec.** Otherwise "rewrite" reads as "replace", and the good parts
are the ones with the least defence, because nobody has written down why they
are good.

## Bad

```rust
// Stage-1 agent, porting a reference parser, replaces the error path wholesale:
pub enum ParseError { ExpectedToken(String), ExpectedType, /* … */ }
impl ParseError { pub fn message(&self) -> String { /* … */ } }

// returned as: (Ast, Vec<ParseErrorWithLocation>)
```

Lost in one commit: `ErrorCode` (so no stable `E0xxx` to test or document
against), `with_note` (so no secondary explanation), the shared accumulating
sink (so parser errors now travel on a different channel from every other
phase), and `render(&SourceMap)` (so the CLI has two error formats). Several of
the 23 diagnostic fixtures now fail on wording, and the fix looks like "update
the `.expected` files" — which is a re-bless.

## Good

Put a keep-list in every brief, alongside the anti-spec:

```markdown
## Carry over unchanged (do NOT redesign; these are frozen infrastructure)

- `diagnostic.rs` — Severity, ErrorCode, the Diagnostic builder
  (`error().with_span().with_code().with_note()`), the accumulating
  `Diagnostics` sink, `render(&SourceMap)`. Your stage REPORTS through this.
  Adding a new `ErrorCode` variant is expected and fine; changing the shape
  of the API is a seam-change request.
- `source.rs` — SourceMap, Span, byte offsets, file ids.
- Interning: `Interner`/`Name`, `TypeInterner`/`Ty`.
- `ids.rs` + `index_vec.rs` — the typed-id/IndexVec discipline.
- The `CompilerContext` threading model: one context, `&`/`&mut` through
  every phase.
- The accumulate-and-continue error policy: never early-return on the first
  user error; recover with `Ty::ERROR` and keep going.
```

Two consequences worth spelling out in the brief:

- **A reference implementation is a source of structure, not of policy.** When
  ark and yel disagree about error handling, yel wins — ark is being consulted
  for how to build a parser, not for how to report errors.
- **Adding to the keep-list is cheap; discovering it late is not.** When review
  finds a good thing an agent nearly deleted, add it to the list before the next
  stage is briefed.

## See Also

- [anti-spec-from-tech-debt](anti-spec-from-tech-debt.md) - The mirror list: what may not be reproduced
- [frontend-follow-ark-reference](frontend-follow-ark-reference.md) - The reference this rule qualifies
- [`diag-accumulate-continue`](../../compiler-skills/rules/diag-accumulate-continue.md), [`diag-error-codes`](../../compiler-skills/rules/diag-error-codes.md), [`diag-builder-messages`](../../compiler-skills/rules/diag-builder-messages.md) - Why the existing design is worth keeping
