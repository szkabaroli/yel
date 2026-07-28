# Stage 1 — `yelc-syntax`                       status: implemented, in review

Replaces (frozen, never edited): `crates/yel-core/src/syntax/`
— `grammar.pest` (516), `parser.rs` (3,266), `ast.rs` (952), `ids.rs` (20).

Base: `main@ccf2086` · Started: — · Landed: —

---

## Brief

Replace the pest grammar and its 3.3k-line wrapper with a **hand-written lexer +
recursive-descent parser that builds a lossless green tree alongside the typed
AST**, ported from [`szkabaroli/ark`](https://github.com/szkabaroli/ark)
(`compiler/arkc-parser`).

Read first, in this order:

1. [`scope.md`](scope.md) — frozen vs. free. The grammar is frozen; the
   technology is free and **expected** to change.
2. [`anti-spec.md`](anti-spec.md) — A1–A7 and B1–B6 apply to this stage.
3. [`keep-list.md`](keep-list.md) — §1 diagnostics, §2 source/spans, §3
   interning, §4 typed ids, §6 accumulate-and-continue. **Do not redesign these.**
4. The contract below.

The point of the change is not tidiness. Pest gives you a parse or a failure;
the green tree gives you a **complete tree for broken input**, which is the state
a file is in most of the time in an editor. That is what makes error recovery,
incremental reparse, and a serious LSP possible, and it is why a competent
recursive-descent parser returning `Result<Ast, Vec<Error>>` would be a
regression despite being smaller ([anti-spec B1](anti-spec.md#b1--no-lossy-parse)).

### Where yel deliberately diverges from ark

State these in the brief so the agent does not "fix" them back:

| | ark | yel — **use this** |
|---|---|---|
| Errors | flat `ParseError` enum, `fn message(&self) -> String`, returned as `Vec<ParseErrorWithLocation>` | `ctx.diagnostics` sink, `Diagnostic` builder, `ErrorCode`, `render(&SourceMap)` |
| Names | `name_as_string` on the ident | interned `Name` via `Interner` |
| Spans | `Span { file_id, start, len }` | yel's `Span { source, start, end }` + `merge` |
| Vocabulary | `fn` / `struct` / `trait` | components / globals / elements / templates |
| Sharing | `Arc` everywhere (cross-thread) | match yel's ownership model unless the LSP needs otherwise |

Ark supplies **mechanism, not policy**. Where the two disagree about how to
report an error, yel wins.

---

## Contract — the stage-1 seam

> **Proposed. Lands on `main` as compiling Rust before the agent starts**
> ([`contract-before-fanout`](../../.agents/skills/compiler-rewrite/rules/contract-before-fanout.md)).
> A needed change is a request in [`seam-changes.md`](seam-changes.md), not an
> agent decision.

### Layer 1 — lexer (`yelc-syntax::lexer`)

```rust
pub struct LexerResult {
    pub tokens: Vec<TokenKind>,   // parallel arrays, not a Vec<Token>
    pub widths: Vec<u32>,         // byte width of tokens[i]
}
pub fn lex(source: SourceId, content: &str, diags: &mut Diagnostics) -> LexerResult;
```

- **No absolute offsets in tokens** — widths only; the parser accumulates
  `offset`. This is what makes incremental reparse tractable later.
- Trivia (whitespace, line + block comments) are **real tokens**, not skipped.
- Lexer errors go to `ctx.diagnostics` and lexing continues — an unknown char
  becomes an `UNKNOWN` token, never a bail.

### Layer 2 — tokens (`yelc-syntax::token`)

```rust
#[repr(u8)]
pub enum TokenKind { /* … < EOF are real tokens; > EOF are node kinds … */ }

pub struct TokenSet(u128);          // const-fn bitset
impl TokenSet {
    pub const fn new(kinds: &[TokenKind]) -> TokenSet;
    pub const fn union(&self, other: TokenSet) -> TokenSet;
    pub fn contains(&self, kind: TokenKind) -> bool;
}
```

FIRST and recovery sets are `const` and drive **both** prediction and recovery —
one declaration, two uses. Yel's sets replace ark's: `ITEM_FIRST`
(`component`/`global`/`package`/`import`), `EXPRESSION_FIRST`, `STATEMENT_FIRST`,
`ELEMENT_FIRST`, `ATTRIBUTE_FIRST`, `PROPERTY_FIRST`, and the corresponding
`*_RECOVERY` sets.

> ⚠️ `u128` caps the set at 128 kinds. Yel's grammar is larger than ark's
> (elements, attributes, bindings, interpolation, ranges, units). **Count the
> kinds before implementing.** If it exceeds 128, the seam changes to
> `TokenSet([u64; N])` with the same `const fn` API — that is a contract
> decision to make now, not a discovery to make at 80% done.

### Layer 3 — green tree (`yelc-syntax::green`)

```rust
pub enum GreenElement { Node(GreenNode), Token(GreenToken) }

pub struct GreenNodeData  { kind: TokenKind, len: u32, children: Vec<GreenElement> }
pub struct GreenTokenData { kind: TokenKind, len: u32, value: /* interned or String */ }

pub struct GreenTreeBuilder { /* … */ }
impl GreenTreeBuilder {
    pub fn start_node(&mut self);
    pub fn finish_node(&mut self, kind: TokenKind) -> GreenNode;
    pub fn finish_node_starting_at(&mut self, kind: TokenKind, marker: Marker) -> GreenNode;
    pub fn abandon_node(&mut self);
    pub fn create_marker(&mut self) -> Marker;   // retroactive node starts
    pub fn token(&mut self, kind: TokenKind, value: &str);
    pub fn create_tree(self) -> GreenNode;
}
```

**Length-based, no absolute offsets** — a node knows its width, not its
position, which is what lets a subtree be reused after an edit. `Marker` enables
retroactive node starts (needed for left-associative binary expressions, where
you learn the node kind after parsing the left operand).

### Layer 4 — parser (`yelc-syntax::parser`)

```rust
pub fn parse(source: SourceId, content: &str, ctx: &mut CompilerContext) -> ParsedFile;

pub struct ParsedFile {
    pub source: SourceId,
    pub green: GreenNode,          // lossless — always present, even for broken input
    pub ast:   ast::File,          // typed view — may contain Error nodes
}
```

The predicate layer over `current()` / `nth(i)`:
`is` · `is2` · `is_set` · `nth_is_set` · `eat` · `expect` · `assert`.
`advance()` = `raw_advance()` + `skip_trivia()`; `raw_advance` is what pushes
into the green builder, so **trivia lands in the tree while the parser never
sees it**.

`start_node` / `finish_node` compute AST spans that **exclude trailing trivia** —
the green node covers the trivia, the AST span does not.

Recovery, carried over verbatim in shape:

```rust
fn parse_list<F, R>(&mut self, start: TokenKind, sep: TokenKind, stop: TokenKind,
                    recovery_set: TokenSet, msg: ErrorCode, node: TokenKind,
                    parse: F) -> Vec<R>
where F: FnMut(&mut Parser) -> Option<R>;
```

including the **`assert!(token_idx > pos_before)` no-progress guard**. Never bail
on the first error; emit an `Error` AST node, report, and continue
([keep-list §6](keep-list.md#6--accumulate-and-continue-error-policy)).

### Layer 5 — AST (`yelc-syntax::ast`)

```rust
pub struct NodeId(pub u32);        // distinct from HirId; stage 2 maps between them

pub struct File { pub source: SourceId, pub green: GreenNode, pub items: Vec<Item> }

pub enum Item { Package(..), Import(..), Component(..), Global(..), Error(..) }
// … Property, Function, Callback, Template, Element, Attribute, Binding,
//    Statement, Expr, Type — each carrying { id: NodeId, span: Span }
```

Every node carries `NodeId` + `Span`. `Error` variants exist at every recovery
point. Names are `Name` (interned), never `String`.

`ast/visit.rs` — `trait Visitor` + free `walk_*` functions, **exhaustive, no `_`
arm** ([anti-spec A3](anti-spec.md#a3--no-duplicated-walkers)).

### Invariants stage 1 ESTABLISHES (stage 2 may assume all of them)

- **S1.** `green` reconstructs the source **byte-for-byte**:
  `green.text() == content`. Asserted in tests over every corpus program.
- **S2.** `green.len() == content.len()`.
- **S3.** Every AST node carries a `Span` that maps into the `SourceMap`, and a
  `NodeId` unique within the file.
- **S4.** Names are interned; no `String` survives parsing.
- **S5.** Ill-formed input produces **a diagnostic AND an `Error` node** — never
  a panic, never a silently-dropped subtree.
- **S6.** Parsing always terminates and always returns a `ParsedFile`. There is
  no failure return.

### What stage 2 may NOT assume (explicitly out of contract)

- No ordering guarantee among sibling items beyond source order.
- `NodeId` values are **not** stable across reparses of an edited file.
- The green tree is not incremental yet — S1/S2 make it *possible*, nothing
  more. Do not build on a reuse API that does not exist.
- Trivia attachment (which comment "belongs to" which item) is **not** decided
  by this stage. Doc comments are a stage-2 concern reading the green tree.

---

## Reference

| Read | For |
|---|---|
| `ark/compiler/arkc-parser/src/lexer.rs` (585) | parallel token/width arrays, keyword map, no offsets |
| `ark/compiler/arkc-parser/src/token.rs` (304) | `TokenSet(u128)` const bitset, FIRST/recovery sets |
| `ark/compiler/arkc-parser/src/green.rs` (185) | `GreenTreeBuilder`, `Marker`, length-based nodes |
| `ark/compiler/arkc-parser/src/parser.rs` (2,581) | predicate layer, `finish_node` trailing-trivia trim, `parse_list` |
| `ark/compiler/arkc-parser/src/ast.rs` (1,273) | node shapes, `NodeId` discipline |
| **frozen** `yel-core/src/syntax/grammar.pest` (516) | **the grammar being preserved — this is the spec** |
| **frozen** `yel-core/src/syntax/parser.rs` (3,266) | every behaviour the pest wrapper encodes beyond the grammar |
| `LANGUAGE.md` | the surface language, normative |

The grammar is frozen at what `grammar.pest` **accepts**, including whatever it
accepts by accident. Where `grammar.pest` and `LANGUAGE.md` disagree, that is a
**Surprise** to record and an orchestrator decision — not an agent one.

---

## Definition of done

- [ ] `yelc-syntax` builds; depends only on `yelc-base`.
- [ ] Green tree round-trips **byte-for-byte** on all 2000 corpus programs, all
      91 positive fixtures, all 46 diagnostic fixtures, and every `examples/*.yel`
      (invariant S1).
- [ ] Every corpus program produces an AST with **zero `Error` nodes** — they all
      parse today, so any Error node is a grammar regression.
- [ ] The 46 diagnostic fixtures still reject, with the same `ErrorCode` at the
      same construct. Wording changes are listed in `goldens-changed.md`.
- [ ] Deliberately broken inputs (a new negative-fixture set) produce a complete
      green tree, ≥1 diagnostic, and no panic (S5/S6).
- [ ] Inline `#[test] fn parse_*` per construct, in the parser file, ark-style.
- [ ] No `_` arm in `walk_*`.
- [ ] `#![deny]` on the std-`HashMap` disallowed-types lint.
- [ ] Ratchet row filled: workspace ≥ 315, execution 85/85, fuzz ≥ 200/200,
      corpus divergences 0, ignored ≤ 2.
- [ ] Freeze check clean — no diff under `crates/{yel-core,yel-wasm-codegen,yelc}`.
- [ ] Adversarial review panel passed.
- [ ] Stage file closed out: Numbers, Decision log, **Surprises**.

### How stage 1 is differentially verified before stage 2 exists

Artifact-level diff is not available yet — nothing downstream consumes the new
AST. Two checks stand in, and both are stronger than they look:

1. **Byte-for-byte green round-trip** over 2000 corpus programs. A parser that
   round-trips every one of them has, by construction, tokenized and structured
   the whole corpus without losing a byte.
2. **Accept/reject parity**: the old and new parsers agree on *which* programs
   parse and which fail, over the corpus, the fixtures, and a mutation set
   (truncations and single-token deletions of corpus programs). This is where a
   silently-tightened grammar shows up.

No `new AST → old AST` adapter is written. It would be a throwaway bridge
([anti-spec A4](anti-spec.md#a4--no-permanent-bridge)) whose only consumer is a
test that (1) and (2) already cover.

---

## Numbers

| | baseline | round 1 | round 2 | **round 3 (final)** |
|---|---|---|---|---|
| workspace tests | 315 / 0 failed | 436 / 0 | 447 / 0 | **479 / 0 failed** |
| execution | 85 / 85 | 85 / 85 | 85 / 85 | **85 / 85** |
| fuzz / 200 | 200 / 200 | 200 / 200 | 200 / 200 | 200 / 200¹ |
| corpus divergences | — | 0 | 0 | **0** (2000 / 2000) |
| ignored | 2 | 2 | 2 | **2** — the two baseline doctests² |
| green round-trip | — | 2118 | 2118 | **2118 / 2118**, byte-for-byte |
| construct-identity divergences | — | — | — | **0** |

¹ Unchanged because the frozen tree is byte-identical to the freeze SHA for the
fuzz path; the `import component` → `extern component` work the owner landed is
a language change on the frozen side and gets its own baseline row in
`ratchet.md` when it is committed.
² `yel-core/src/lir/layout.rs` and `yel-smith/src/lib.rs`. No test was newly
ignored at any point in the stage.

### Verification detail — every row re-measured at close (anti-spec A19)

Each number below is emitted by a named test. Nothing is carried forward from an
earlier round; the round-2 table had three figures that had drifted.

| Check | Command | Result |
|---|---|---|
| green round-trip (S1/S2) | `--test corpus` | 2000 corpus + 91 positive + 23 diagnostic + 4 example = **2118 / 2118** |
| construct identity, checked-in programs | `--test identity` | **2093 compared**, 0 mismatches, 2 incomparable³ |
| construct identity, mutations | `--test identity` | **5264 cases**, 0 mismatches |
| known identity divergences | `--test identity` | **0** — the list is empty |
| accept/reject — corpus | `--test parity` | **2000 checked**, 0 divergences |
| accept/reject — fixtures + examples | `--test parity` | 118 checked, **1** known |
| accept/reject — deterministic mutations | `--test parity` | 2204 checked, **18** known |
| accept/reject — random mutations | `--test parity` | 3060 checked, 34 catch-all, **0 unexplained** |
| accept/reject — hand-written | `--test parity` | **105**, 0 |
| accept/reject — keyword-prefix class | `--test parity` | **75**, 0 |
| accept/reject — unreserved `let` / `if` | `--test parity` | **44**, 0 |
| accept/reject — PEG `X?` boundaries | `--test parity` | **98**, 0 |
| S5 per construct — deterministic | `--test corpus` | **2204** inputs, 0 violations |
| S5 per construct — randomized (seeded) | `--test corpus` | **9100** inputs, 0 violations |
| S5 — hand-written recovery positions | `--test corpus` | **26**, all report **and** mark |
| brace dispatch stays linear | `--lib nested_braces` | 12 levels in **96 µs**; asserts < 50 ms |
| determinism — reparse is identical | `--lib parsing_is_deterministic` | green text, **whole AST incl. `NodeId`s and spans**, and diagnostics all identical across three sources |

³ `global_filter_default.yel` (the frozen compiler silently drops its global —
`docs/TECH_DEBT.md`) and `examples/counter/counter.yel` (opens with a `use`
line neither parser accepts). Both are named in `INCOMPARABLE_SOURCES`, which is
asserted exactly, so a program that *stops* being compared fails the test.

### The speculation primitive, and where it is deliberately not used

`Parser::try_parse` — checkpoint, attempt, restore-exactly-on-failure, with a
`(site, position, depth)` memo. Rollback is proven field-by-field: deleting any
one of the six restored fields makes a test fail (mutation-tested at close).

It is used at **one** site: the `if`/element ambiguity, glued (`ife { … }`) and
unglued (`if { a: 1 } { … }`). Both were previously decided by lookahead that
was provably *guessing wrong* — `glued_if_body_is_all_nodes` treated any
depth-zero colon as proof of a `named_prop`, and a ternary's colon is depth-zero
too. Deleted, along with `if_condition_is_a_record`, `kind_after` and
`closer_of`.

It is **not** used for `primary`'s brace-led alternatives, and that is a measured
decision. Speculating there in grammar order was implemented and reverted: it is
correct and **exponential**, because each `{` re-parses its body once per
alternative and the alternatives multiply with nesting — 341 µs → 4.1 ms from 2
to 12 levels, ~1.6× per two levels, and the corpus sweep stopped terminating. A
packrat memo does not rescue it: the memo would have to cache a *successful*
parse, and a success here is already appended to the green tree, so replaying it
means rebuilding the tree rather than reusing a value.

So `classify_brace` and its side tables are not hacks that avoid backtracking —
they are what keeps this parser linear where pest is not. The difference between
the two sites is that one had a demonstrated **defect** and the other has a
demonstrated **cost**; `nested_braces_do_not_blow_up` pins the latter so the
removal cannot be retried silently.

The divergence allow-list in `crates/yelc-syntax/tests/parity.rs` is **exact**
in both directions: a new divergence fails, and so does an entry that stops
diverging (`every_known_divergence_still_diverges`). It also checks its own
stated root cause against the **frozen** AST, and carries a hard length ratchet
(`KNOWN_DIVERGENCE_COUNT`) — see "Review round 2" below.

## Decision log

- *2026-07-24* — Crate named `yelc-syntax`, ark convention, permanent name (no
  rename at cutover). Decided by rewrite owner.
- *2026-07-24* — Stage 1 depends only on `yelc-base`; `CompilerContext` lives in
  `yelc-sema`, so the parser takes `&mut Diagnostics` + `&Interner` rather than
  the whole context. Keeps `yelc-syntax` usable by the LSP without dragging in
  type-checking state.
- *2026-07-24* — **`TokenSet` stays a `u128`.** Counted before implementing: 74
  token kinds, `EOF` at discriminant 73. The anticipated `TokenSet([u64; N])`
  seam change is **not** needed. Node kinds (76) live above `EOF` and never
  enter a set; 150 total discriminants fit the `u8` comfortably.
- *2026-07-24* — **Every keyword is contextual.** `grammar.pest` reserves
  nothing: `identifier` matches `component`, `if`, `for`, … and pest spells
  keywords as bare string literals inside the rules that need them. Lexing them
  as distinct kinds is what makes `const` FIRST sets possible, so `NAME_FIRST`
  (= `IDENTIFIER` ∪ `KEYWORD_FIRST`) and `Parser::is_name` accept a keyword
  token wherever the grammar wanted an `identifier`.
- *2026-07-24* — **`parse_list` gains one parameter beyond ark's signature:**
  `trailing: TrailingSep`. The frozen grammar allows a trailing separator in six
  lists (record fields, enum/variant cases, list/tuple/record literals) and
  forbids it in five (function params, call args, type lists, closure params,
  result types). Accepting it everywhere would widen the language, which
  `scope.md` forbids outright. The no-progress `assert!` is unchanged.
- *2026-07-24* — **No handler/binding split in the AST.** The frozen parser
  inspected a binding's value and re-filed closure-valued ones as `Handler`s.
  That is analysis stored on the node it describes (anti-spec B3); every
  `name: value` inside an element is one `NamedProp` and stage 2 classifies.
- *2026-07-24* — **A closure parameter with no written type is `None`,** not a
  placeholder. The frozen parser used `TyKind::Unknown` "to be inferred later" —
  exactly the shape anti-spec B2 names.
- *2026-07-24* — **`yel-core` is a `[dev-dependencies]` entry of `yelc-syntax`,**
  used by `tests/parity.rs` only. It is the accept/reject oracle, not a bridge:
  nothing in the library links it, and it is deleted with the frozen tree at
  cutover phase 4. Shelling out to `yelc check` 2000× measures the same thing an
  order of magnitude slower.

### Review round 1

Two adversarial reviews found six blocking defects. What changed, and why.

- **`MAX_NESTING_DEPTH = 256`, with `measure_max_depth` to keep it honest.**
  Invariant S6 said parsing always returns; it did not. ~1500 nested `(`
  `SIGABRT`ed a debug build at roughly six stack frames per level, which no
  `catch_unwind` and no accumulate-and-continue policy survives. 256 was chosen
  by measurement in both directions: the deepest of all 2118 checked-in `.yel`
  inputs nests **21** (`corpus/src/469.yel`), so the guard sits 12x above real
  programs, and it trips roughly an order of magnitude below the stack. The five
  guarded entry points are `parse_expr`, `parse_unary`, `parse_type`,
  `parse_ui_node` and `parse_stmt` — every unbounded recursion in the grammar
  passes through one of them. `parse_stmt` rather than `parse_stmt_block`,
  because the block's loop is where the no-progress check lives.
  `real_programs_stay_well_under_the_depth_limit` asserts the 4x margin so the
  constant can never quietly become a language restriction.

- **`Recovered<T>` replaces `synthetic_ident`.** The old hole interned `""` and
  returned a real `ast::Ident`, so `package ;` produced a `PackageDecl` whose
  namespace and name were both a `Name` **and equal to each other**, two lines
  under a comment claiming the parser "does not guess at what was meant". A
  consumer that never read `Diagnostics` could not tell the hole from a name.
  Every fillable position is now `Recovered<T>`: `MaybeIdent` for names,
  `Block<T>` for a braced body, `Recovered<FuncSignature>` for a `func` keyword
  that was not there, `Recovered<Vec<_>>` for a parameter list whose `(` was
  never opened. The spans point at the **current token**, not at the enclosing
  declaration's start — `package ;` reports its hole at offset 8, where it used
  to report offset 0. Six sibling fabrications went with it: a missing `func`
  keyword no longer yields a zero-parameter function with the written type
  orphaned; `if x "a"` is no longer indistinguishable from `if x { }`;
  `result<a, b, c>` keeps all three arguments instead of `truncate(2)`;
  `a.b.` keeps `a.b` instead of collapsing to one `Error`; a nameless closure
  parameter still appears in `params`; and `parse_primary`'s fallthrough emits a
  zero-width green `ERROR` node, closing the one recovery point the green tree
  carried no marker for.

- **One member list per declaration.** `ComponentDecl`, `GlobalDecl`,
  `ExternComponentDecl` and `ElementDecl` each carry a single `Vec` of members in
  source order with an `Error` variant; `properties()` / `functions()` / `body()`
  are accessors. Sorting members into three `Vec`s as the parser went left
  nowhere to put a recovery node, which is why `global G { 42 }`,
  `component A { 42; }`, `element E { 42 }` and `extern component C { 42 }` each
  produced a diagnostic and **zero** `Error` nodes. Over 300k random token soups,
  446 inputs had that shape.

- **`parse_list` lost `code: ErrorCode` and gained `R: ast::Recovery`.** The
  `code` parameter took `SyntaxError` at all eight call sites while
  `error_here` was already the single E0060 idiom (anti-spec B6). The bound is
  the load-bearing half: the `None` arm now reports **and** pushes
  `R::recovery(..)`, so a list production whose failure path drops the element is
  no longer expressible. A related gap needed its own mechanism — a missing
  *token* (`>` in `list<s32`, `in` in `for x xs { … }`) has no slot in an AST
  that models nodes and not tokens, so `expect` records an unattached hole and
  every construct owning a list of recoverable children drains it; `parse_file`'s
  item loop is the outermost backstop.

- **`token::EMPTY` deleted, `GreenTreeBuilder::abandon_node` wired up.** Both
  were dead. `EMPTY` was documented as "the recovery set where a caller has no
  synchronising tokens to offer" and had zero references. `abandon_node` is now
  the mechanism behind `parse_export_modifier`, which starts a green `MODIFIER`
  node speculatively and abandons it when the keyword is not there — ark's
  `parse_modifiers` shape. `ITEM_FIRST` and `MEMBER_FIRST` now gate `parse_item`
  and `parse_component_member` before their `match`, so the doc claim "one
  declaration, two uses" is true of the two sets it is true of, and the doc says
  which. The gates report and recover rather than `unreachable!()`, because a set
  that drifts out of sync with its `match` must not break S6. `ITEM_FIRST`
  contained `PACKAGE_KW` with no arm to match it; a misplaced `package` now says
  so instead of "expected a top-level declaration".

- **The parity allow-list checks its own claim.** `KNOWN_DIVERGENCES` was
  documented as "every entry has the same single root cause" and nobody asserted
  it, so appending a real regression made the suite pass.
  `every_known_divergence_still_diverges` now requires
  `new_member_count > frozen_member_count` for every entry in
  `CATCH_ALL_DIVERGENCES` — the frozen parser must demonstrably have lost a
  `global`/`record` member the new parser keeps. One entry failed that check on
  first run and was **not** absorbed by loosening it: `corpus/src/292.yel` with
  its 7th chunk deleted leaves `let  = 'b';` inside a closure body, which PEG
  backtracks away without any catch-all. It lives in its own
  `DROPPED_LET_DIVERGENCES` list with its own mechanical check (the new parser
  must find a `let` whose name is a `Recovered::Missing`).

- **The mutation generator is now one copy.** `parity.rs` and `corpus.rs` each
  had their own, with *different* constants, while `parity.rs` recorded
  divergences by labels (`<path>#delete@7`) that index into the derived list.
  Both stayed green while every allow-list entry pointed at a different program
  than its name claimed. The generator moved to `tests/support/mod.rs`; the
  labels were re-derived from the shared constants, which is why the list is now
  15 entries rather than 22 and names different chunks.

- **`has_depth_zero_arrow` is no longer quadratic.** It walked to
  end-of-token-stream on an unterminated `{` while its module doc called the scan
  "bounded". `Parser::bracket_close` matches every bracket once in `Parser::new`,
  and the scan is bounded by the matching `}` and **jumps over** nested groups
  instead of counting through them. Total parse time for N unterminated
  `{ k:` groups went from 1.1 / 3.8 / 14.6 ms (500 / 1000 / 2000, superlinear) to
  1.8 / 3.6 / 7.8 / 30.0 ms (500 / 1000 / 2000 / 8000, linear).

- **`exprs.rs` was split.** Statement productions moved to `parser/stmts.rs`;
  every parser module is now under anti-spec A2's ~800-line threshold excluding
  tests (`exprs` 795, `items` 776, `parser` 722, `nodes` 311, `stmts` 211,
  `types` 187), and each carries its own inline `parse_*` cases.

- **`rustc-hash` removed** from `crates/yelc-syntax/Cargo.toml`; it was never
  used. The `[lints.clippy] disallowed_types = "deny"` guard stays.

Two test premises were **wrong**, and were corrected rather than worked around —
both provably, against the frozen parser as oracle:

1. `corpus.rs` asserted that every truncation of a 16-line program other than the
   empty prefix "lands inside a construct" and must therefore report. Fifteen do
   not: they land on a declaration boundary, and the frozen pest parser accepts
   all fifteen. Rejecting them would be a language change. The test now asserts
   S5 itself at every cut (report ⟺ mark) plus the **exact** set of clean cuts,
   which is strictly sharper in one direction and honest in the other.
2. `component A { x: R = { 42 }; }` was listed as a record-literal recovery
   position. `record_literal` needs at least one `name:` field, so `{ 42 }` is a
   `closure_no_params` whose body is the trailing expression `42` — the frozen
   parser accepts it with zero catch-alls. Replaced with
   `component A { x: R = { a: 1, 42 }; }`, which really is a record literal with
   an unreadable field, and which the frozen parser really does reject.

One seam-adjacent change was unavoidable: `ast::TypeKind::Func(FuncSignature)`
as landed does not compile. `FuncSignature` holds `Option<TypeRef>`, so
`TypeRef → TypeKind → FuncSignature → TypeRef` is a cycle with no indirection and
`rustc` rejects it with E0072. The variant is now `Func(Box<FuncSignature>)`,
matching `UiNode::If(Box<IfNode>)` and `ExprKind::Closure(Box<ClosureExpr>)`.
`ast::File`, `NodeId`, `ParsedFile`, `parse`'s signature and the S1–S6 comments
are untouched.

### Review round 2

Round 2 found that three of round 1's fixes were **checks that could not fail**,
and that two silent grammar narrowings had survived both rounds. What changed.

#### 1. `let` and `if` were reserved in statement position

`parse_stmt` committed on the keyword. The frozen grammar reserves nothing:
`let_statement` needs an `identifier`, so PEG backtracks and `assign_statement`
matches with `let` as an ordinary target. `{ let = 1; }` is **two** statements in
the frozen AST and nothing is dropped.

`at_let_statement` is now `let` + a name. `at_if_statement` is `if` + something
in `EXPRESSION_FIRST` + a **depth-zero `{`** after the expression — because
`if (a) { … }` is a condition and `if(x);` is a call on a variable called `if`,
and pest separates them by backtracking. The `{` lookahead is `O(1)`: one
backward pass (`condition_scan_table`) answers it for every position at once,
so there is no per-`if` scan and nothing runs to end of input on unterminated
source (anti-spec B8).

`DROPPED_LET_DIVERGENCES` and `nameless_let_count` are **deleted**. That entry
was never a divergence — it was this bug — and its check was circular: it
asserted a property of *our* parser, so it could not fail while the bug existed.
44 `let`/`if` rows are now pinned against the frozen parser.

#### 2. The keyword-prefix class, both directions

pest spells every keyword as a bare string literal with **no word boundary**, so
a keyword matches a *prefix* of what a hand-written lexer produces as one
identifier. Both directions are now reproduced; the class is enumerated in
`accept_reject_parity_over_the_keyword_prefix_class` (70 rows).

| member | before round 2 | after |
|---|---|---|
| `x: s32x` — and `strings`, `charx`, `int8`, `colorx`, `boolean`, `f32x`, `lengthy`, `physical-lengthx`, `relative-font-sizes`, `brushes`, `imagex`, `easingx`, `u8s`, `floats`, `intx`, `percenty`, `durations`, `angles` | **widened** (we accepted) | rejected, as frozen does |
| `x: resultx` | **widened** | rejected — `result_type`'s `<…>` is optional, so `result` alone is a complete match |
| `x: listx` / `optionx` / `tuplex` | agreed | agreed — each needs a `<`, so the prefix match fails and `named_type` matches |
| `x: bool = trueish` / `falsey` | **widened** | rejected — `literal` precedes `identifier` in `primary`, and `bool_literal` is `"true" \| "false"` |
| `recordFoo`, `componentFoo`, `enumFoo`, `variantFoo`, `elementFoo` | **narrowed** (we rejected) | accepted, as frozen does |
| `externcomponent C { }`, `exportcomponent A { }`, `exportglobal G { }`, `packagea:b;` | **narrowed** | accepted |
| `forx in xs { … }`, `forx iny { … }`, `for x iny { … }` | **narrowed** | accepted |
| `callbackc(a: s32);` in a `global` | **narrowed** | accepted |
| `callback: func();` in a `global` | **narrowed** (pre-existing, found here) | accepted — `function_decl` is the earlier alternative and `callback` is not reserved |
| `input:` / `outputs:` in a `global`, `settings:` / `bindings:` / `keyx:` in an element, `funcx()`, `letx` | agreed | agreed — the frozen **AST** differs (direction `in` + property `put`), but both parsers accept, and the oracle records one bit per program |

**2a** (widening) is `parser/types.rs::type_keyword_prefix_of` plus
`exprs.rs::at_bool_literal_prefix`: an identifier that is not exactly a keyword
but has one as a proper prefix reports and yields a recovery node. No keyword is
a prefix of another, so at most one can match — asserted.

**2b** (narrowing) **landed**, contrary to the brief's expectation that it might
have to be reverted. The cursor gained `partial_offset`, and `eat_keyword` pushes
the prefix as its own green token, advances `offset`, and leaves `token_idx`
alone until the remainder is consumed. The token arrays are never mutated, so
`bracket_close`, `condition_scan` and lossless-by-construction all hold — the
same bytes reach the green tree in the same order, under two kinds instead of one.

Three things had to change with it, and each was a latent defect the split merely
exposed:

- **The no-progress guards counted the wrong thing.** Every loop asserted
  `token_idx > before`; a keyword split consumes bytes without advancing the
  token index, so real progress read as none. They now use `position()` — the
  byte offset — which is strictly monotonic under every consuming operation and
  is the correct progress measure regardless.
- **`span_between` collapsed to a point** when `token_idx <= start_token`, which
  a split makes true while bytes *have* been consumed. It now checks offsets.
- **Prediction, not just consumption, needed the prefix.** This is what the brief
  expected to be fatal, and the resolution is that the ambiguous sites are
  decidable with bounded lookahead: `parse_item` predicts by *text*
  (`item_keyword_prefix`), the `for` site requires a following `in`, and the
  `callback` site requires a following `(`. Those guards are not arbitrary — they
  are exactly the tokens pest would have **backtracked out of `for_node` /
  `global_callback` for**, so `format { … }` stays an element and
  `callbacks: s32;` stays a property. Both are pinned.

#### 3. The depth guard bounded the wrong quantity

`MAX_NESTING_DEPTH` bounds recursion inside `parse_*`. `parse_binary` and
`parse_postfix` are **loops** that enter and leave nesting per operand, so
`a.b.b.b…` builds an N-deep `Box`/green chain while `measure_max_depth` reports
**2**. Measured in a debug `cargo test` thread on
`component A { x: s32 = <chain>; }` — valid, diagnostic-free input:

| consumer | before | after |
|---|---|---|
| `parse()` | ~193,000 | unchanged |
| `green.text()` — the S1 check | abort at n ≈ 12,983 | worklist; passes at n = 500,000 |
| `Drop` for the green tree | abort at n ≈ 4,979 | iterative + `Arc::into_inner`; passes at n = 500,000 |
| `Drop` for `ast::Expr` | (same chain) | iterative over `ExprKind`; passes at n = 500,000 |
| `ast::visit::walk_expr` | abort at n ≈ 3,126 | `stacker::maybe_grow`; passes at n = 500,000 |
| **frozen pest parser** | **abort at n ≈ 14,544** | — |

**The frozen ceiling is the number that decided the walker.** Option (c) —
bounding the chain — was only permissible below a *higher* frozen ceiling, and
14,544 is well above what the walker could reach. Restructuring `walk_expr` to
shrink its debug stack frame was measured and rejected: splitting the wide match
arms into `#[inline(never)]` helpers moved the ceiling 3,126 → 12,986, still
**below** the oracle, and bought that with an `unreachable!()` panic path in a
walker. Option (b), a worklist-driven `Visitor`, cannot be done without an API
change: the recursion runs `walk_expr → v.visit_expr → walk_expr` through an
overridable hook, so flattening it locally stops calling that hook on spine
nodes — the silent skip anti-spec A3 forbids.

So option (a): `stacker::maybe_grow`, the mechanism rustc uses for exactly this
shape (`rustc_data_structures::stack::ensure_sufficient_stack`). It costs one
dependency and no API change, and it removes the cliff rather than moving it.
**This is a deliberate departure from the definition-of-done line "depends only
on `yelc-base`"** — `yelc-base` itself depends on `serde`, `rustc-hash` and
`parking_lot`, so that line reads as crate-graph discipline within the rewrite,
but it is recorded here as an orchestrator decision to confirm.

The two tests that "asserted headroom" by measuring `max_depth_seen` — provably
constant-2 on the shapes that abort — are replaced:
`real_programs_stay_well_under_both_depth_limits` measures parse depth **and**
`green.max_depth()` (21 and 32 respectively, on `corpus/src/204.yel`), and
`flat_operator_chains_survive_past_the_frozen_parsers_ceiling` exercises all four
consumers at n = 40,000.

#### 4. The allow-list root-cause check was a tautology

`new_member_count > frozen_member_count` holds for **any** over-rejection inside
a `global`/`record` body, because the recovery model always materialises an
unreadable element as a member. A reviewer proved it by flipping record field
lists to `TrailingSep::Forbidden` — a pure tightening — and the check stayed
green (anti-spec A10).

The replacement, `frozen_silently_dropped_a_member`, is **evidence about the
frozen parser only**: the frozen parse succeeds, emits no `CatchedError`, and
somewhere inside a `global` or `record` body there is a non-trivia,
non-separator token that **no** frozen member span covers. That is
`BLOCK_LEVEL_CATCH_ALL` itself. Separators are excluded because a comma between
two members is outside every member's span by construction — counting it would
make the check true for every well-formed declaration, and specifically for the
trailing-comma regression.

**Verified to fail the regression.** The tightening was re-applied in a scratch
edit, one of the resulting divergences (`corpus/src/1.yel#delete@3`) was appended
to `CATCH_ALL_DIVERGENCES`, and `every_known_divergence_still_diverges` rejected
it with "…but every byte inside every `global`/`record` body is covered by a
member the frozen AST kept". Scratch deleted.

A hard `KNOWN_DIVERGENCE_COUNT` ratchet was added **regardless**. It is 20: the
one fixture, plus 19 mutations. It was 14 before the mutation seed set was
strided (see below); the six new entries are the same class and each satisfies
the characterization check.

#### 5. Four more S5 clusters — diagnostic, zero recovery nodes

All four now report **and** mark:

- `element E { a: s32 = 1; }` and `extern component D { a: s32 = 1; }` — the
  "default value not allowed" report had no hole; the *grammaticality* of writing
  one has no field, so it is a `RecoveryMark`.
- `package a:b@;` — `parse_package_version` returned `(None, None)` with no mark.
  `package a:b@ 1;` marked it via the atomic-gap check, which made the hole
  version-specific.
- `"{}"` and `"{ }"` — the empty interpolation reported and pushed nothing, so
  `parts.len() == 0` collapsed the whole string to `Str("")`: a diagnostic with
  no recovery node *and* a plausible value standing in for one (anti-spec B9). It
  now pushes an `ExprKind::Error` part.
- `package a:b;/*` — an unterminated block comment is **trivia**, so nothing in
  the AST recorded it. `raw_advance` now checks `is_closed(kind, text)` for every
  delimited token, which covers unterminated strings, template segments and
  character literals in the same stroke — an unterminated `"abc` used to produce
  an ordinary `Str`.

#### 6. `unattached_holes` corrupted list arity

A missing *token* drained into whichever list closed **first** — usually an
inner, well-formed one — as a real element, at an index chosen by drain timing:

| input | was | now |
|---|---|---|
| `f: func(a list<s32>, b: s32)` | 3 parameters | 2 |
| `f: func(a tuple<s32, string>)` | `tuple<S32, ERR, String>` | `tuple<S32, String>` |
| `record R { a: list<s32 }` | 2 fields | 1 |

They are now `ast::File::recovery_marks`: a side table of `(NodeId, Span)` sorted
by span, visited by `walk_file`, belonging to no list. S5 still holds — the marks
are recovery nodes — and no construct lies about its arity. `ast::File` gains one
field; `NodeId`, `ParsedFile`, `parse`'s signature and the S1–S6 comments are
untouched.

#### 7. Span fidelity — the deeper gap

The parity oracle records **one bit** per program and every diagnostic is
`SyntaxError`, so the DoD row "same `ErrorCode` at the same construct" was
vacuous on the code half and unchecked on the construct half. Round 1's
`synthetic_ident` defect was a *span* regression (`package ;` reporting at offset
0 instead of 8) and re-introducing it left every test green.

Two checks now, deliberately different in kind:

- `first_error_lands_on_the_construct_that_is_wrong` — an exact table of
  (input, byte offset) over 20 recovery positions. Sharp and readable.
- `first_error_offset_agrees_with_the_frozen_parser_as_often_as_before` — a floor
  (586 of 1422) on how many mutually-rejected mutations report the first error at
  *exactly* the frozen parser's byte offset. Wide rather than sharp: no
  hand-written expectation to get wrong, and a systematic drift falls through the
  floor.

**Verified to fail a pure span regression.** `expect_name`'s hole span was
scratch-changed to a fixed `0..1`. Both span tests failed; all nine accept/reject
tests stayed green — which is the point. Scratch reverted.

#### 8. Should-fixes

- **S5 asserted per construct.** The file-level biconditional
  `(diagnostics > 0) != (error_nodes > 0)` let a report-without-mark and a
  mark-without-report cancel; a reviewer's mutation stayed green over 2225
  inputs. It is now: every diagnostic has a recovery node at the same place, and
  every recovery node has a diagnostic there — "same place" being overlap, or a
  gap that is entirely trivia. Re-verified by deleting `expect`'s mark: **1636 of
  2264** deterministic and **477 of 9200** randomized inputs then fail.
- **A real randomized generator is committed**, seeded (`RANDOM_SEED`), with
  byte/char-level mutation *and* token soups — 9,200 inputs per run.
  `single_token_deletions` splits on whitespace and can never build `"{}"` from
  `"v={value}"`, which is why four S5 clusters survived underneath it (A13).
- **git-lfs pointer stubs are detected**: `assert_corpus_content` requires ≥ 4 MB
  total and that sampled files contain a `component` declaration. A count alone
  passed over 2000 ~130-byte stubs (A14).
- **Exact counts everywhere**: mutation sweep 2264, randomized sweep 9200,
  truncation set 524, hand-written parity cases 105, keyword-prefix 70, `let`/`if`
  44, recovery positions 27, fixtures+examples 118.
- **Mutation seeds are strided, not `take(30)`.** The fixture list is name-sorted,
  so the prefix never reached past `f` and `externed_components.yel` — the only
  `extern component` fixture — had never been mutated. The stride re-pointed the
  fixture `#delete@N` labels and surfaced six new Surprise-1 divergences.
- **Dead API deleted**: `GreenElement::{to_token, is_empty, len}`,
  `GreenNodeData::is_empty`, `GreenTokenData::{kind, len, is_empty, value}`,
  `Recovered::into_present`, `TokenKind::is_eof`.
- **`todo!("not a property direction")` replaced** with a report + recovery. It
  sat on a path whose contract is that parsing always returns (S6).
- **`ELEMENT_ITEM_FIRST` and `MEMBER_FIRST` no longer union kinds already in the
  set.** Both equal `NODE_FIRST`, because every keyword is a legal identifier; the
  unions read as intent the sets did not carry. A test asserts the aliases stay
  sound if a keyword ever stops being one.
- **Uncovered constructs added**: block comments (0 of 2118 checked-in files
  contain one), the `bind` modifier, the legacy `callback name(…);` form, and
  unit literals — 26 new hand-written parity rows.
- **Surprise 1's stale "21"** corrected to 19 (the mutation count after striding).

## Surprises

### 1. A catch-all inside `global` or `record` is silently discarded

`grammar.pest` recovers from a malformed member with `BLOCK_LEVEL_CATCH_ALL`,
which eats the offending line so the enclosing declaration still matches.
`syntax/parser.rs` reports that recovery in exactly **two** places —
`parse_component` (:823) and `parse_element_node` (:1186). `parse_global`
iterates its members with a trailing `_ => {}`; `parse_record` filters its pairs
with `if field_pair.as_rule() == Rule::record_field` (parser.rs:321). Different
spellings, same effect: the catch-all goes on the floor.

So a `global` body or a `record` field list containing text the grammar cannot
parse can be accepted with **no diagnostic at all**, and the member vanishes.
This is silent only when the garbage leaves the surrounding structure intact —
`BLOCK_LEVEL_CATCH_ALL` has to swallow a whole member, terminator included.
Garbage that disturbs the comma structure still fails the enclosing rule and is
reported: `record R { a: s32, 42 }` and `global G { 42 }` are both rejected by
the frozen parser today. It is the well-formed-looking member that disappears.

This is not hypothetical. The positive fixture `global_filter_default.yel` — whose
comment says it is the regression guard for module-scope `.filter(…)` — writes:

```yel
evens: list<s32> = [1, 2, 3, 4].filter(|x| x > 2);
```

`|` is not an operator in this grammar (and `LANGUAGE.md` spells the closure
`{ x -> x > 2 }`). pest fails `global_property`, the catch-all eats the line,
`parse_global` says nothing, and `yelc check` prints `OK: 1 component(s)
checked`. **The property it is testing is never parsed, so the guard has never
guarded anything.** All **19** mutation divergences are the same root cause,
checked mechanically against the frozen AST — see Review round 2 §4.

The rewritten parser reports it, which invariant S5 and anti-spec A5 require.
Two follow-ups for the orchestrator, neither an agent decision:

- the fixture should be rewritten to `{ x -> x > 2 }` and re-blessed, at which
  point the module-scope filter path is actually exercised for the first time;
- the two silent `_ => {}` arms are a `known_bugs` entry the rewrite fixes.

### 2. `if` followed directly by `{` is an element named `if`

`node = if_node | for_node | children_node | element_node | string_node`, and
none of the keywords are reserved. When `if` is followed immediately by `{`,
`if_node` cannot match (it wants a condition), so the grammar falls through to
`element_node`, whose `element_name` happily matches `if`. `if { Foo { … } }`
parses today as an element called `if`. Same for `for {`, `else {`, and every
other keyword. Reproduced deliberately (`parser/nodes.rs`); found only because a
single-token deletion produced it.

One residual mismatch is *not* reproduced: `if { a: 1 } { … }` — a record-literal
condition — is a real `if_node` in pest, because it tries `if_node` first and the
alternative *succeeds*. Distinguishing that from the element case needs
backtracking. It requires a record literal as a bare `if` condition and has never
appeared in 2000 corpus programs, 91 fixtures, or 2256 mutations.

### 3. `-` is an identifier character, and there is no word boundary anywhere

`identifier = @{ (ALPHA|"_") ~ (ALNUM|"_"|"-")* }`, so `count-1` is **one
identifier** and `count - 1` is a subtraction. A trailing hyphen is legal: `a-`
is a name, which makes `count-=1;` parse as `count- = 1` and `p->x` parse as
`p- > x`. All reproduced.

More broadly, pest matches keywords as bare string literals with no word
boundary, so `recordFoo { }` is a `record` named `Foo`, `input: s32;` inside a
global is direction `in` on a property named `put`, and `s32x` is the primitive
`s32` followed by a stray `x`. Round 1 recorded these as unreproducible without
abandoning tokenization. **That was wrong in both directions and round 2
reproduced the whole accept/reject class** — see Review round 2 §2 for the table
and the `partial_offset` cursor. `input:` remains an *AST* divergence and not an
accept/reject one: both parsers accept it, and the oracle records one bit.

### 4. Strings have no escape sequences; character literals do

`string_text = @{ (!("\"" | "{") ~ ANY)+ }`. A backslash is an ordinary
character and the first `"` ends the string, so `"a\"b"` is the string `a\`
followed by the identifier `b`. `char_inner` *does* have `escape_seq`. Ark's
lexer escapes both; that difference is a real accept/reject divergence and the
grammar wins.

`char_literal = { "'" ~ char_inner ~ "'" }` is also **non-atomic**, so pest's
implicit whitespace is skipped on both sides of the inner character: `'x '` is
the character `x`, and `' '` (a literal space) matches nothing at all.

### 5. `name: func(…)` means opposite things in a component and in a global

`component_member` orders `property_decl | function_decl | node`, and
`type_annotation` includes `func_type` — so an unexported `on-click: func(a:
s32);` is a **property** whose type is a function, and only the `export`-prefixed
form is a `function_decl`. `global_member` orders `function_decl |
global_callback | global_property`, so the identical text inside a `global` is a
**callback**. Both reproduced.

### 6. `LANGUAGE.md` is silent on three constructs the grammar supports

No mention of `extern component`, of the legacy `callback name(…);` form, or of
the `bind` prop modifier — all three are in `grammar.pest` and `extern component`
is used by checked-in fixtures. Conversely, `examples/counter/counter.yel` opens
with `use yel:ui/dom@0.1.0;`, which appears in neither the grammar nor
`LANGUAGE.md`; the frozen compiler rejects that example with `E0060` today. Both
parsers agree, so it is not a divergence — but the example is broken and nobody
noticed.

### 7. `unit_suffix` is an ordered prefix match, not a token

`unit_literal` is atomic and `unit_suffix` is an ordered choice of string
literals, so it matches a *prefix* of whatever follows the digits: `10second` is
the unit literal `10s` followed by the identifier `econd`, and `10inch` is `10in`
followed by `ch`. Order is load-bearing — `ms` must be tried before `s`.
Reproduced verbatim in the lexer.

### 8. `package_version` is atomic, so the lexer cannot own it

`package_version = @{ "@" ~ DIGIT+ ~ ("." ~ DIGIT+)* }` accepts `1.0.0`, which a
number lexer necessarily splits into `FLOAT_LITERAL DOT INT_LITERAL`. The parser
stitches adjacent tokens back together and stops at the first gap, rather than
teaching the lexer about versions. `package_id` itself is *non*-atomic, so
`package yel : counter @ 1.0.0 ;` parses today; only the version run is tight.
