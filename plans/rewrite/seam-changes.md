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

*Empty. The first entry will come from stage 1.*

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
