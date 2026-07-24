# scope-frozen-vs-free

> Name what may not change before anyone starts, or the rewrite becomes a redesign

## Why It Matters

"Rewrite the internals, keep the language and the stages" is a clear instruction
to a human who already holds the project in their head. To an agent it is
ambiguous at exactly the boundaries that matter: is the WIT world part of the
language? Is `SignalDependencies` a stage boundary or an internal detail? May
the surface syntax gain a keyword if it makes the parser dramatically simpler?

Every ambiguity resolves in the direction of scope growth, because redesigning
is more interesting than transcribing. A rewrite that also changes the language
cannot be differentially tested — the corpus stops compiling — and the moment
that happens, every rule in this skill loses its teeth simultaneously.

Write the frozen/free split into the plan as a table, and make it the first
thing in every agent brief. When an agent proposes crossing the line, that is an
orchestrator decision, not an agent one.

## Bad

> **Brief:** Rewrite the parser. Keep the language the same.

The agent finds that `pest` makes error recovery awkward, switches to a
hand-written recursive-descent parser (fine, that's internal), and along the way
makes trailing commas mandatory in attribute lists because it simplifies the
grammar (not fine). Eleven fixtures now fail to parse. The diff looks like a
parser rewrite; the breakage is a language change buried inside it.

## Good

| Frozen — changing it is a separate, approved decision | Free — expected to change |
|---|---|
| Surface syntax as specified in `LANGUAGE.md` | Parser implementation, grammar technology, AST node shapes |
| Stage names and their order: AST → HIR → THIR → LIR → WASM | Every type, pass, and helper inside a stage |
| Exported WIT world and `yel:ui/dom@0.1.0` host contract | How WIT is constructed and emitted |
| Observable DOM-op behaviour asserted by execution tests | The lowering that produces it |
| Diagnostic *meaning* for the 23 diagnostic fixtures, and the `diagnostic.rs` API | Which stage reports what; new `ErrorCode` variants |
| CLI surface: `yelc compile -o {wasm,wit,dot}`, `ir`, `check` | Driver internals, `pipeline.rs` structure |
| Determinism of all output | Which data structures produce it |

Three clarifications worth stating explicitly, because they come up every time:

- **Diagnostic wording may improve; diagnostic *meaning* may not.** A fixture
  asserting `cannot infer type` must still reject that program, for that reason.
  Improved wording is a golden update with the diff read and justified. The
  diagnostic *infrastructure* is frozen outright — see
  [`keep-diagnostics-infrastructure`](keep-diagnostics-infrastructure.md).
- **The stage boundary is frozen; the stage's data model is not.** "Keep THIR"
  means a typed IR still exists between HIR and LIR with a documented contract —
  not that `ThirExprKind` keeps its variants.
- **Parser technology is free, and expected to change.** Dropping pest for a
  hand-written lexer + recursive-descent parser over a green tree is the plan,
  not a scope violation — see
  [`frontend-follow-ark-reference`](frontend-follow-ark-reference.md). What stays
  frozen is the *grammar it accepts*.

Anything not in the table defaults to **frozen**. An agent that wants to move an
item to the free column asks; it does not decide.

## See Also

- [contract-before-fanout](contract-before-fanout.md) - Turning the frozen column into per-seam types
- [orchestrate-integrator-owns-seams](orchestrate-integrator-owns-seams.md) - Who gets to say yes
