# Stage 2 — `yelc-driver`                                       status: **landed**

New crate, binary `yelc2`. Replaces nothing frozen — it is the **observation
instrument** the rewrite has been missing.

Base: `2505f8d` · Started: 2026-07-28 · Landed: 2026-07-28

> **Why it moved to stage 2.** It was previously unscheduled: the plan mentioned
> `yelc-driver` only in its own obituary (cutover phase 3 flips its binary to
> `yelc`, phase 4 deletes its stage-selection seam) and never said when it is
> built. Stage 1 showed why that is wrong — every review round had reviewers
> building throwaway scratch harnesses to observe the new parser, and **two of
> the integrator's own measurements were wrong** because a `#[cfg(test)]` probe
> is a bad instrument: one used a handler position where no `Closure` node
> exists, another omitted a component so `record Foo` "rejected" for an
> unrelated reason. A CLI would have made both obvious in one command.

## What shipped

```
yelc2 [OPTIONS] <FILE>

  --emit-ast[=<ITEM>]   the typed AST; optional value filters to one top-level
                        item by name
  --emit-green          the green tree — kinds and widths, trivia included
  --emit-green-text     green.text(): the source reconstructed from the tree
  --identified          include NodeId on every AST node
  --spans               include byte spans on every AST node
```

Exit codes: `0` clean · `1` the program has errors · `2` the driver could not
run. Diagnostics go to stderr through `yelc-base`'s renderer — the same one
`yelc check` uses — and dumps go to stdout, so a dump can be piped while errors
stay readable.

| file | lines | what |
|---|---|---|
| `src/main.rs` | 64 | the clap `Parser` struct, and nothing else |
| `src/driver.rs` | 15 | module root |
| `src/driver/run.rs` | 53 | the pipeline, in a straight line |
| `src/driver/emit.rs` | 331 | the two dumpers |

**320 code lines**, 463 including doc comments. The "under 400" budget was about
code — a dump that is 100 lines of `match` arms plus its rationale is the
intended shape, and padding the budget by deleting the rationale would be the
wrong trade.

## Following `arkc`

`arkc` has **two** drivers and only one is alive.

- `arkc/src/main.rs` — 131 lines, clap derive, one flat args struct,
  `--emit-ast=<fct>` / `--emit-ir`, and a straight-line `compile(args)` that runs
  the phases in order and emits at points along the way. **This is what
  `yelc-driver` follows.**
- `arkc/src/driver/cmd.rs` — a 40-field docopt-era `Args` struct with a
  hand-written `USAGE` string and a matching `Default` impl, inherited from dora.
  It is **not wired into `main.rs`**; ark moved to clap and left it behind. It is
  reference for what not to build.

What came over:

| from ark | here | why |
|---|---|---|
| clap derive, one `Parser` struct | same | frozen `yelc` already uses clap; the workspace has one arg-parsing idiom |
| `--emit-<ir>` per stage, one invocation | same | see below |
| `--emit-ast=<fct>` takes a **filter** | `--emit-ast=<ITEM>` | the whole AST of a real file is not readable, and the question is almost always about one declaration |
| `main.rs` is arguments only | same | the phase-running lives in `driver/`, so the surface and the behaviour move independently |
| straight-line `compile()`, emit points interleaved | `driver::run()` | as stages land it grows lines in the middle, not branches |

### The command surface changed, and the previous rationale was wrong

The earlier brief specified subcommands — `yelc2 ast`, `yelc2 green`, `yelc2
hir` — on the argument that **"`yelc ast` and `yelc2 ast` become directly
comparable, which is exactly what the differential wants."**

That argument does not survive contact. The two ASTs are structurally different
*by construction* — the frozen one is a pest parse tree, the new one is a green
tree plus a typed view over it — so the dumps were never going to be
byte-comparable, and nothing in the differential reads them. `tests/parity.rs`
compares accept/reject **verdicts**; the artifact differential compares WIT/DOT.
Matching subcommand names bought symmetry in the invocation and nothing at all
in the comparison.

Ark's flag form buys something real instead: `--emit-ast --emit-green` dumps both
views of **one** parse. When two views disagree, they cannot have come from
different runs — which is precisely the failure mode that made two stage-1
measurements wrong.

### Deliberately not taken

**`--unpretty`.** rustc's name is an accident: there was once a stable `--pretty`
for pretty-printing source, the structural dumps went behind `-Z unpretty` to
contrast with it, and then `--pretty` was removed — leaving a flag named against
something that no longer exists.

**`identified` the capability, not the flag spelling.** Node ids in the dump is
worth having; `-Z unpretty=ast-tree,identified` is not a shape to copy.

**`expanded`.** In rustc it means "after macro and `#[derive]` expansion" and yel
has neither. Yel's analogous idea is desugaring — `if`/`for` becoming block
structure — which does not happen until LIR lowering.

**A filter on `--emit-green`.** The green tree has no names to filter on; it is
kinds and widths. A flag that silently did nothing is worse than its absence.

**`--emit-hir` / `--emit-ir`.** They would print nothing today. They arrive with
`yelc-hir` (stage 3) and `yelc-lir` (stage 3) as one line each in `run()`.

## Deferred: `diff`

The brief listed a `diff` subcommand — moving the differential runner out of
`yelc-syntax/tests/parity.rs` so it is runnable by hand. **Not built, and the
reason is worth keeping.**

`diff` is the one command that would make `yelc-driver` depend on the frozen
tree. `parity.rs` already carries that dependency, but it carries it as a
`[dev-dependencies]` entry that vanishes when the test file does; a `diff`
subcommand would put it in the shipping binary's dependency graph, which is
exactly the bridge [anti-spec A4](anti-spec.md#a4--no-permanent-bridge) is about.
`parity.rs` runs in CI today and answers the same question. Revisit if a review
round actually needs it by hand — that is a real cost, but a smaller one than the
bridge.

## Constraints (carried forward)

- **Depends on `yelc-base` + `yelc-syntax`**, gaining crates as stages land. It
  does **not** depend on the frozen tree, and per the section above that is now a
  property to preserve rather than a temporary state.
- **No stage selection.** There is one implementation of each stage; a selector
  is dead weight until there are two.
- **The switch selects an implementation, never a behaviour**
  ([`cutover-switch-then-delete`](../../.agents/skills/compiler-rewrite/rules/cutover-switch-then-delete.md)).
- Output is for humans; it is **not** a golden. Nothing in `tests/` asserts on
  its text, or the driver becomes a thing that must not change.
- **Emission is unconditional on diagnostics.** A dump that only prints for input
  that parses is useless for the case it was built to serve.

## Definition of done

- [x] `--emit-ast` and `--emit-green`, with `--identified` / `--spans` /
      `--emit-green-text`, over any `.yel` file.
- [x] Diagnostics render through `yelc-base`'s renderer — shared code, so this is
      a wiring check, not a reimplementation.
- [ ] ~~`diff` reproduces `parity.rs`'s verdict~~ — deferred, see above.
- [x] `cargo test --workspace` ≥ the [ratchet](ratchet.md); execution **85/85**.
- [x] Freeze check clean — `scripts/freeze-check.sh` exit 0.
- [x] Under 400 lines of code (320).

## Numbers

| measure | command | result |
|---|---|---|
| corpus, driver failures (exit ≥ 2) | `for f in corpus/src/*.yel; do yelc2 $f --emit-green-text; done` | **0 / 2000** |
| corpus, S1 round-trip through the CLI | `cmp $f <(yelc2 $f --emit-green-text)` | **2000 / 2000 byte-identical** |
| execution tests | `cargo test -p yel-wasm-codegen --test execution` | **85 / 85** |
| workspace | `cargo test --workspace` | green |
| freeze | `scripts/freeze-check.sh` | exit 0 |

The round-trip number is the one worth having: invariant S1 was asserted inside
`yelc-syntax`'s own tests, and this is the first time it has been checked from
outside the crate, through the shipping binary, over the whole corpus.

## Decision log

1. **Flags over subcommands** — reversed the earlier brief; the comparability
   argument behind subcommands was false (see above).
2. **Followed `arkc/src/main.rs`, not `arkc/src/driver/cmd.rs`** — the latter is
   dead code ark itself abandoned.
3. **Every `id`+`span` AST type gets a generated `visit_*` arm**, all 31 listed
   explicitly. A dump that silently omits a node type is worse than no dump,
   because the reader concludes the node is not in the tree — the stage-1 lesson
   applied to the instrument itself.
4. **Kind labels are `match`es, not `{:?}` scraped for a variant name.** A
   `Debug` string is a representation; reading structure out of one is what
   anti-spec A3 is about.
5. **`--emit-ast=<ITEM>` matches the item's first `Ident`**, read off the walk
   rather than off a `match` over `ItemKind` — so the driver does not carry a
   second copy of the item list that would silently go stale.
6. **Green tree prints widths, not offsets.** The tree stores widths; printing
   offsets would be the driver computing something the tree does not have, which
   is how a dump starts lying.

## Surprises

**`File.span` ends one byte before the green tree does.** On
`extern_components.yel` (506 bytes) the AST root spans `0..505` while
`SOURCE_FILE` has width `506`. The trailing newline is trivia: it is in the green
tree and outside the last item's extent. Correct, and a good demonstration that
the two views answer different questions — which is the argument for `--emit-ast`
and `--emit-green` being one invocation rather than two.

**`RecoveryMark` is visible in the dump and reads well.** `component C { x:
list<s32 }` prints a complete AST plus two `RecoveryMark` entries at `47..47` —
the missing `>` and the missing `;`. That side table was the fix for the
list-arity corruption described in `ast.rs`, and it had never been *looked* at
outside a unit test.
