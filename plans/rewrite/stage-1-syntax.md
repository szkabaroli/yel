# Stage 1 — `yelc-syntax`                              status: not started

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

*Filled in at close-out.*

| | baseline | stage 1 |
|---|---|---|
| workspace tests | 315 / 0 failed | |
| execution | 85 / 85 | |
| fuzz / 200 | 200 / 200 | |
| corpus divergences | — | |
| ignored | 2 | |
| green round-trip | — | / 2000 |

## Decision log

- *2026-07-24* — Crate named `yelc-syntax`, ark convention, permanent name (no
  rename at cutover). Decided by rewrite owner.
- *2026-07-24* — Stage 1 depends only on `yelc-base`; `CompilerContext` lives in
  `yelc-sema`, so the parser takes `&mut Diagnostics` + `&Interner` rather than
  the whole context. Keeps `yelc-syntax` usable by the LSP without dragging in
  type-checking state.

## Surprises

*Behaviour discovered in the old compiler that nobody knew about. Record it even
when it changes nothing — thirty seconds here, a week at stage 5.*
