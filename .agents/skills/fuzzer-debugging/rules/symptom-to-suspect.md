# symptom-to-suspect

> Map a symptom to the general bug *shape* to check — not to a specific function, because those move

## Why It Matters

Once you have a minimized repro and (if it validates) the failing instruction,
the fix goes faster if you recognize the *shape* of the bug. These shapes recur
across any multi-stage compiler / codegen; they are described here by symptom and
general cause, deliberately without internal function names, so this stays true
as the code moves. Match the symptom, then go read the actual code the current
implementation uses for that path.

## The heuristics

**Value looks like the neighbor's / length right but data shifted / a free-path
hang** → **wrong element stride.** Something walks an array by `base + idx *
size` (or frees `len * size`) with a size that's the scalar default instead of
the element's real width. Composite elements (tuples, tagged unions, nested
records) are the usual victims. Compute size from the layout, not a table with a
catch-all default. Validates fine — only a round-trip or a hang exposes it.

**Order- or type-dependent validation failure with no obvious call site** →
**dead-but-validated code.** WASM validates *every* function, reachable or not. A
helper left behind by a migration but never called still has to type-check; if
its stale logic is only correct for some element types (e.g. it stores the first
field as one fixed type), it fails only for the others. When a path is replaced,
stop *generating* the old one.

**`expected i32, found f32/f64/(ref …)` at a `local.get`/store, or a scalar that
round-trips as garbage** → **slot valtype vs read-mode mismatch.** A local
carries two coupled choices: its WASM valtype must match the value it holds, and
its read must match how it was written (the value *is* the slot → `local.get`;
the slot is a *pointer* → load through it). Derive the valtype from the value's
type; make the writer and reader agree.

**`values remaining on stack`, or a WIT/import signature missing params/fields
you can see in the source** → **dropped def payload.** A definition and the type
describing it are built separately; the construction site captured the type but
forgot to populate the def's params/fields. Downstream (signatures, WIT) emits a
truncated arity while the call site pushes the full one. Cross-check the two
representations.

**Value comes back structurally correct but zeroed / empty / default** → **stub
fall-through.** A routine handles a few cases then falls through to a bland
default ("handle the rest later"). It looked complete because only the handled
cases reached it — until a new caller routes an unhandled case through and gets
a silent, plausible-but-wrong default. Implement the case, or make the gap a
loud error.

**Bug only appears at nesting depth ≥ 2, or only when an inner payload is itself
composite** → **fixed-depth recursion.** Handling written for "one level down"
covers a scalar/leaf but not a composite leaf, so the second level falls off the
end. Don't add "the nested version" — make the existing routine recurse through
its own general path (a composable "how to reach this value" abstraction is the
enabler).

**A fix makes one direction work but the round-trip still fails with the
*opposite* signature, or passes the round-trip yet hangs** → **you fixed one of
several mirror paths.** A composite value is produced, consumed, and cleaned up
by separate code that must agree on layout. Treat "produce / consume / free" as a
checklist; the cleanup path is the one no round-trip assertion looks at, so it's
where hangs hide.

## How to use

This is an index, not a spec. It tells you *what kind* of bug you likely have and
*what to check*; the current code's function names and file layout you get from
reading the code (and `docs/ARCHITECTURE.md` / `docs/TECH_DEBT.md`), not from
this rule.

## See Also

- [fuzz-dump-core-module](fuzz-dump-core-module.md) - Get the failing function/offset before matching a shape
- [verify-roundtrip-not-validate](verify-roundtrip-not-validate.md) - Most of these validate; a round-trip is what proves the fix
- [verify-fuel-in-tests](verify-fuel-in-tests.md) - Surfaces the cleanup-path hang the mirror-paths heuristic warns about
