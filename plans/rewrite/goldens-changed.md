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

| 2026-07-28 | 1 | `yelc-syntax` — 12 lib/integration tests, listed in "the keyword word boundary" below | Renamed and re-asserted for the word boundary: `ife { div {} }` is an element, `letx = 1;` an assignment, `forx in xs {…}` rejected; the `if`/element speculation probes re-pointed from `ife {` to `if {` | Every one pinned the **old** rule. The word boundary is a deliberate language change applied to **both** compilers (below); these tests are its specification, so they move with it. No assertion was weakened — the two that changed position (`HANDWRITTEN` → `parity.rs`) moved because they stopped being *construct-identity* cases and became *accept/reject* ones. | owner |

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


## The keyword word boundary — a deliberate surface change

**What.** A keyword ends only where an identifier could not continue. Applied to
both compilers in one change, to the **construct** keywords: `record enum
variant element extern component global package export func callback if else for
in key let set bind in-out out`.

Before, `grammar.pest` matched every one as a bare string literal with no
boundary, so `ifa { div {} }` was `if a { div {} }`, `recordFoo { … }` a record
named `Foo`, `forx in xs { … }` a `for` over `x`, `exportcomponent A { … }` an
exported component, and — the quietest of them — `global G { input: s32; }` an
`in` property named `put`. None of it was designed; `LANGUAGE.md` documents no
keyword rules at all, and stage 1 found the class by accident.

**Deliberately not applied** to `unit_suffix`, which is an ordered *prefix* match
by design (`10second` is `10s` followed by `econd`), or to `primitive_type`,
which is handled separately by `parser/types.rs::type_keyword_prefix_of` — the
widening half of the keyword-prefix class is unchanged, `s32x` included.

**How, on the frozen side.** Not the way [directions §7](directions.md#7--keywords-get-a-word-boundary--at-cutover-by-deletion)
predicted. It named two approaches, both measured and both bad: a shared
non-atomic `WB` rule (pest inserts implicit whitespace before it, so `record Foo`
stops parsing) and an atomic `kw_record = @{ "record" ~ !IDENT_CONT }` (correct,
but it **emits a pair**, so all ~20 productions' `into_inner()` walking in
`syntax/parser.rs` would have to change — "the expensive part").

There is a third form, and it costs nothing:

```pest
GLUED_RECORD = @{ "record" ~ IDENT_CONT }
record_decl  = { !GLUED_RECORD ~ "record" ~ identifier ~ "{" ~ … }
```

The atomic rule pins the keyword and the character after it together, so no
implicit whitespace can be skipped between them. It is used under a **negative
predicate**, which consumes nothing and produces no pair — so the pair tree is
byte-for-byte what it was and **`syntax/parser.rs` was not touched at all**.
The negative form is also the safe one: were a site ever entered with the cursor
still on whitespace, `!GLUED_x` succeeds vacuously and the keyword still matches,
where the positive `&KEYWORD_x` would reject. There is no input this
construction can falsely reject.

**How, on the new side.** Deletion, as predicted. `at_keyword_prefix`,
`eat_keyword`, `assert_keyword`, `Follow`, `starts_identifier`,
`keyword_prefix_of`, `item_keyword_prefix`/`ITEM_KEYWORDS`, `next_starts_with_in`,
`at_glued_else_if`, `condition_here_is_followed_by_a_block`, the text-prefix
halves of `after_export`, `global_property_direction` and `at_named_prop` — all
gone, with every call site. The lexer's natural behaviour simply stands.

**What survived, against the prediction.** `split_token` and `partial_offset`
stay, as §7 said — `expect_type_close` still takes the `>` out of a `>=` so
`list<s32>=1` closes the generic. But `try_parse`, `Speculation`,
`failed_attempts` and the `Checkpoint` machinery **also stay**, which §7 expected
to retire. The `if`/element ambiguity is not only about gluing: `if` followed
directly by `{` is still two live readings after the boundary —
`if { a: 1 } { div {} }` is an if-node over a record-literal condition, and
`if { span { "x" } }` is an element literally called `if`. Deciding that by
lookahead is exactly the guess round 2 removed. The three speculation probes were
re-pointed from `ife {` to `if {` rather than deleted.

**Why it is safe, measured rather than argued.** The corpus was regenerated and
all 8000 artifacts came back **byte-identical** — 2000 programs × src/wit/dot/wasm.
Real yel writes `if a {`, not `ifa {`. Independently, before the commit, both
`yelc` binaries were built side by side and all 2000 corpus sources compiled
through each: 0/2000 WIT, 0/2000 DOT and 0/2000 WASM digests moved.

Everything else held: 480 workspace tests / 0 failed / 2 ignored, 85/85
execution, 200/200 fuzz, `clippy -p yelc-syntax -p yelc-base` clean, and the
first-error-offset floor unmoved at 547 of 1336 (40%).

**Every assertion that moved, and why.**

| where | what moved | why it is correct |
|---|---|---|
| `parser.rs::speculative_failure_restores_across_a_keyword_split` | renamed `…_across_a_token_split`; subject moved from `eat_keyword(RECORD_KW, …)` on `recordFoo` to `expect_type_close()` on `>=` | The property under test is unchanged — a failed speculation restores a **non-zero** `partial_offset`. `eat_keyword` no longer exists; `expect_type_close` is now the only thing that can produce a non-zero one, which is precisely why `partial_offset` survives. |
| `parser.rs::glued_if_keeps_the_frozen_parsers_construct_identity` | renamed `a_keyword_glued_to_a_name_is_one_identifier`; `ife { div {} }` If → Element, `forx in xs { "a" }` For → Error | Read out of the frozen parser's own AST dump after the change, as the original was before it. The third row also changes the accept/reject bit — in both compilers together, which is the point. |
| `parser.rs::parsing_is_deterministic` | the speculation-heavy source moved from `ife {…} iflex {…}` to `if { a: 1 } { div {} } if { span { "x" } }` | The old source no longer reaches `try_parse` at all, so it would have tested determinism over a path with no hash set in it — a vacuous assertion. The new one is the surviving speculation site. |
| `parser.rs::the_glued_if_decision_does_not_depend_on_the_rest_of_the_file` | renamed `the_if_versus_element_decision_…`; subject `ife { <deep> }` → `if { <deep> }` | Same reason. The depth-latch bug this probe was built for lives in `try_parse`; pointing it at input that no longer speculates would retire the regression test silently. |
| `nodes.rs::parse_glued_if_takes_the_if_reading_when_the_body_is_nodes` | renamed `a_name_beginning_with_if_is_an_element`; asserted an if-node, now asserts an element named `ife` | The specification of the change, inverted deliberately. The frozen compiler agrees (verified on the binary). |
| `nodes.rs::parse_glued_if_takes_the_element_reading_when_the_body_has_a_prop` | renamed `a_name_beginning_with_if_is_an_element_whatever_the_body_holds`; **assertions unchanged** | Only the *reason* changed: it reached the element reading by backtracking out of `if_body` before, and reaches it directly now. Kept because the pair is what proves the boundary is not body-sensitive. |
| `nodes.rs::parse_glued_if_with_a_prop_body_leaves_the_else_to_be_an_element` | renamed `an_else_after_an_iflex_element_is_an_element_too`; **assertions unchanged** | The name said "glued"; nothing is glued any more. Renamed so the name matches what it asserts. |
| `stmts.rs::parse_glued_let_binds_the_remainder` | renamed `a_name_beginning_with_let_is_not_a_binding`; asserted `Stmt::Let` binding `x`, now asserts `Stmt::Assign` to `letx` | The specification of the change. The frozen compiler now parses `letx = 1;` as an assignment — it reports `E0011: unknown function \`letx\``, a *semantic* error, which is the proof the parse succeeded that way. |
| `stmts.rs::parse_glued_if_statement` | renamed `parse_if_statement`; subject `ifa > 0 { g(); }` → `if a > 0 { g(); }` | Both compilers now **reject** the old subject, so it can no longer test what an if-statement parses to. The property (an if-statement with a one-statement branch) is unchanged; the old text moved to `parity.rs` as a rejection row. |
| `exprs.rs::parse_record_literal_versus_closure_with_statements` | subject `{ lets: s32 = 1; }` → `{ let s: s32 = 1; }`; name unchanged | Same: both compilers reject the old text. The property — a depth-zero `;` proves the block is a closure body and not a record literal — is untouched and is what the case was always for. |
| `identity.rs::HANDWRITTEN` | 39 rows → 28; eleven moved to `parity.rs` | The table's stated precondition is *accepted by both parsers with no diagnostic* — that is what makes it a construct-**identity** table rather than an accept/reject one, and the harness asserts it per row. Those eleven are now rejected by both, so they are accept/reject cases and belong in `parity.rs`. Nothing was dropped. |
| `identity.rs::the_projection_catches_an_injected_misidentification` | probe pair `ife { div {} }` / `iflex { color: red }` → `if { a: 1 } { div {} }` / `if { span { "x" } }` | The probe needs one input that projects `node:if` and one that projects `node:element` **at the same offset**, or it cannot prove the projection distinguishes them. `ife` projects as an element now; the replacement pair is the surviving if/element ambiguity, at offset 14 in both. |
| `parity.rs::accept_reject_parity_over_the_keyword_prefix_class` | 75 rows → 82 | Seven distinct rows migrated in from `identity.rs`. **No expectation moved**: every row in this test is compared against the frozen parser at run time, not against a written verdict, so the whole class — 82 rows, including all the former "narrowing" ones that flipped accept → reject — passes because both compilers flipped together. |
| `parity.rs::accept_reject_parity_over_the_let_and_if_keyword_class` | **nothing** — 44 rows, unchanged, still green | Recorded because roughly a third of its rows silently changed their accept/reject bit and the test still passes, which is the strongest single piece of evidence that the two compilers moved in step. Only the doc comment was refreshed. |
| `parity.rs::FIRST_ERROR_OFFSET_AGREEMENTS` | **nothing** — re-measured at 547 of 1336 (40%) | Unlike the kebab lookahead, which moved exactly one input's first-error offset, this change moved none. The floor is not touched. |

---

## 2026-07-29 — `global_filter_default.yel` moves to `known_bugs/`

**Stage 2a phase 0** (oracle hygiene). A fixture that guarded nothing became a
fixture that pins a real panic. No compiler source changed — this is fixture data
and test-harness bookkeeping only, so the corpus is untouched and needs no
regeneration.

### What was wrong

`positive/global_filter_default.yel` existed to guard the module-scope
`.filter(…)` carrier. It wrote:

```yel
evens: list<s32> = [1, 2, 3, 4].filter(|x| x > 2);
```

`|` is not an operator in this grammar. `global_property` fails, `BLOCK_LEVEL_CATCH_ALL`
eats the line, `parse_global`'s trailing `_ => {}` says nothing, and `yelc check`
prints OK. **`evens` appears zero times in both goldens** — verified before
touching anything, not inferred. The guard never guarded.

### What the corrected program does

```yel
evens: list<s32> = [1, 2, 3, 4].filter({ x -> x > 2 });
```

**Panics the frozen compiler** at `hir/local_scope.rs:73` — `index out of bounds:
the len is 0 but the index is 0`. So it could not be re-blessed as a positive
fixture; there is no output to bless.

Scoped by experiment, because "the module-scope path is broken" is a claim:

| position | result |
|---|---|
| `component App { evens: list<s32> = […].filter({ x -> x > 2 }); }` | **OK** |
| `global Store { func go() { let e = […].filter({ x -> x > 2 }); } }` | **OK** |
| `global Store { evens: list<s32> = […].filter({ x -> x > 2 }); }` | **PANIC** |

The bug is exactly the global property default — precisely the path the fixture
claimed to cover.

### What changed

| where | change |
|---|---|
| `positive/global_filter_default.{yel,wit,dot}` | **deleted** — 91 positive fixtures → **90** |
| `known_bugs/global_filter_default.yel` | **added**, corrected syntax, with the scoping experiment in a comment |
| `known_bugs/global_filter_default.failure` | **added** — `panicked: index out of bounds: the len is 0 but the index is 0` |
| `known_bugs/README.md` | inventory row; a note that a panicking fixture's signature is coarse |

**No coverage was lost.** `.filter(…)` is exercised by three other positive
fixtures — `list_filter.yel`, `list_filter_basic.yel`, `for_filter_over_signal.yel`
— so the only thing unique to this fixture was the broken path, which is now
pinned rather than silently skipped.

### Four guards fired, and each was updated deliberately

Worth recording in full: this single fixture move tripped four independent
assertions in stage 1's suite. None was weakened or deleted.

| assertion | change | why it is correct |
|---|---|---|
| `support::POSITIVE_FIXTURE_COUNT` | 91 → **90** | The count guard exists so a sweep cannot silently shrink. It shrank on purpose; the constant carries the reason in a doc comment. |
| `support::catch_all::DIVERGENCES` | the **only whole-file entry** removed | This was the sole checked-in fixture the frozen catch-all excused. Every remaining entry is a generated mutation, so the new parser now reports zero error nodes across every hand-written fixture **with no exceptions**. |
| `support::catch_all::DIVERGENCE_COUNT` | 19 → **18** | Down by deleting an excuse — the only direction it may move without a per-entry justification ([A10](anti-spec.md#a10)). |
| `identity.rs::INCOMPARABLE_SOURCES` | 2 names → **1** (`examples/counter/counter.yel`) | `global_filter_default.yel` was incomparable because the frozen parser had no accepted parse to compare against. |
| `identity.rs` sweep size | 2095 → **2094**; `COMPARABLE_SOURCES` **unchanged at 2093** | One fewer file swept, one fewer excused. Removing a file that could not be compared costs no comparison — which is why the comparable count, not the sweep size, is the number to watch. |
| `parity.rs::accept_reject_parity_over_the_fixtures` | 118 → **117** | Same fixture, one fewer row. |

### Numbers after

480 workspace / 0 failed / 2 ignored · execution **85 / 85** · fuzz **200 / 200**
· corpus untouched (no compiler source changed).
