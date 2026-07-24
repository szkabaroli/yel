# frontend-follow-ark-reference

> The parser and HIR are ports of a known-good design, not fresh inventions — follow `szkabaroli/ark`

## Why It Matters

The frontend is the one part of the rewrite where the target design already
exists and is owned by the same author: [`ark`](https://github.com/szkabaroli/ark)
(`compiler/arkc-parser`, `compiler/arkc-hir`). Handing an agent a blank
"rewrite the parser, make it clean" brief when a working reference is one
directory away wastes the most valuable input available and guarantees a
different set of trade-offs than the ones already chosen.

The concrete change is large: yel's parser is a ~3.3k-line wrapper over a
**pest** grammar. Ark's is a hand-written lexer plus recursive-descent parser
that builds a **lossless green tree** alongside the typed AST. That difference
is not stylistic — it is what makes error recovery, incremental reparse, and a
serious LSP possible. Pest gives you a parse or a failure; the green tree gives
you a complete tree for broken input, which is the state a file is in most of
the time in an editor.

The brief for stage 1 and stage 2 is therefore *"port this design to yel's
grammar"*, with the reference read first and the divergences argued.

## Bad

> **Brief:** Rewrite the parser without pest. Hand-written recursive descent,
> clean structure, good error messages.

The agent produces a competent recursive-descent parser that returns
`Result<Ast, Vec<Error>>` and throws away trivia. It is better than what it
replaced and it forecloses the LSP work permanently, because the tree cannot
represent a file that does not parse.

## Good

Brief stage 1 and 2 against the reference, naming the patterns to carry over:

**Parser — `arkc-parser/src`**

| Pattern | What to port |
|---|---|
| `lexer.rs` | Hand-written lexer returning parallel `Vec<TokenKind>` + `Vec<u32>` widths + errors. **No absolute offsets in tokens** — widths only; the parser accumulates `offset`. |
| `token.rs` | `TokenKind` as a plain `u8`-ish enum, plus `TokenSet(u128)` — a `const fn` bitset. FIRST/recovery sets (`EXPRESSION_FIRST`, `ELEM_FIRST`, `PARAM_LIST_RS`) are `const` and drive both prediction and recovery. |
| `green.rs` | `GreenNode`/`GreenToken` behind `Arc`, length-based, with a `GreenTreeBuilder` and `Marker` for retroactive node starts. Trivia is *in* the tree. |
| `parser.rs` | `is`/`is2`/`is_set`/`eat`/`expect`/`assert` predicate layer over `current()`/`nth()`. `start_node`/`finish_node` pair computing spans that exclude trailing trivia. |
| recovery | `ElemData::Error` nodes + the generic `parse_list(start, sep, stop, recovery_set, msg, node, parse)` helper, including its `assert!(token_idx > pos_before)` no-progress guard. Never bail on the first error. |
| `ast/visit.rs` | `trait Visitor` + free `walk_*` functions — the walk/visit split. One traversal, overridable arms. |
| tests | Inline `#[test] fn parse_*` cases per construct, in the parser file. |

**HIR — `arkc-hir/src`**

| Pattern | What to port |
|---|---|
| `hir/hir_id.rs` | `HirId(u32)` / `FnBodyId(u32)` newtypes, distinct from the parser's `NodeId`. |
| `hir_map.rs` | `HirMap` — a **bidirectional** `HirId ↔ NodeId` map. This is what lets a HIR-level diagnostic point back at source, and what the LSP needs. |
| `hir/hir_node.rs` | `NodeMap<V>` side tables (`idents`, `map_vars`, `map_calls`) keyed by `HirId`, with an `assert!(old.is_none())` on insert. Analysis results live in side tables, not on nodes. |
| `hir/module.rs` | `Module` owning `node_types`, `bodies`, `elements` — bodies separated from items by id. |
| `parsety.rs` | `ParsedType` — the AST type and its resolved `Type` in one lazily-filled cell, so name resolution can run after construction without a second tree. |
| `hir/visit.rs` | Same walk/visit split, again — one walker per IR. |

**Where yel deliberately diverges** (state these in the brief so the agent
doesn't "fix" them):

- **Diagnostics stay yel's.** Ark's flat `ParseError` enum is a downgrade from
  yel's builder + `ErrorCode` + accumulating sink. Port the parser *structure*;
  route its errors into `ctx.diagnostics`. See
  [`keep-diagnostics-infrastructure`](keep-diagnostics-infrastructure.md).
- **Yel's grammar, not ark's.** Elements are components/globals/elements, not
  `fn`/`struct`/`trait`. The reference supplies mechanism, not vocabulary.
- **Interning stays.** Yel interns names to `Name` and types to `Ty`; ark keeps
  `name_as_string` on the ident. Keep yel's — see `intern-strings`.
- `Arc` everywhere is ark's choice for cross-thread sharing; match yel's
  existing ownership model unless the LSP needs otherwise.

Read the reference before writing the brief, and read it again before reviewing
the result — "did they port the recovery set discipline or just the shape?" is a
review question you can only ask with the original in hand.

## See Also

- [keep-diagnostics-infrastructure](keep-diagnostics-infrastructure.md) - The part of yel that beats the reference
- [contract-before-fanout](contract-before-fanout.md) - The green tree / AST seam is a contract before either side is built
- [`pass-visitor-recurse`](../../compiler-skills/rules/pass-visitor-recurse.md), [`anti-duplicate-walker`](../../compiler-skills/rules/anti-duplicate-walker.md) - Ark's `visit`/`walk` split is exactly the fix for `TECH_DEBT.md §6.1`
