# Stage 2 — `yelc-driver`                            status: brief written · not briefed

New crate. Replaces nothing frozen — it is the **observation instrument** the
rewrite has been missing, and the one place allowed to know both trees exist.

Base: — · Started: — · Landed: —

> **Why it moved to stage 2.** It was previously unscheduled: the plan mentioned
> `yelc-driver` only in its own obituary (cutover phase 3 flips its binary to
> `yelc`, phase 4 deletes its stage-selection seam) and never said when it is
> built. Stage 1 showed why that is wrong — every review round had reviewers
> building throwaway scratch harnesses to observe the new parser, and **two of
> the integrator's own measurements were wrong** because a `#[cfg(test)]` probe
> is a bad instrument: one used a handler position where no `Closure` node
> exists, another omitted a component so `record Foo` "rejected" for an
> unrelated reason. A CLI would have made both obvious in one command.

## Brief

A **thin** driver over the new crates: one subcommand per IR, in the shape
`yelc` already uses.

Thin is the requirement, not an aspiration. The moment it grows behaviour of its
own it becomes a third implementation to keep in sync with two others. It
formats and routes; it does not decide anything about the language.

### The command surface

`yelc` already has a subcommand shape, and the driver extends it rather than
bolting a new flag idiom onto the side:

```
yelc compile -o {wasm,wit,dot} · yelc ast · yelc ir · yelc check      (frozen)
```

so:

```
yelc2 ast    [--identified] [--spans]   <file>
yelc2 green  [--text]                   <file>
yelc2 hir    [--typed] [--identified]   <file>
yelc2 ir                                <file>
yelc2 check                             <file>
yelc2 diff                              <file>
```

| command | dumps | available from |
|---|---|---|
| `ast` | the typed AST, structurally | **stage 1** |
| `green` | the lossless green tree, kinds + widths | **stage 1** |
| `green --text` | `green.text()` — the S1 round-trip, for eyeballing | **stage 1** |
| `hir` | HIR; `--typed` dumps after phase 2b instead of 2a | stage 3a |
| `ir` | LIR blocks and ops — the name yel already uses | stage 4a |

| flag | effect |
|---|---|
| `--identified` | include `NodeId` / `HirId` on every node |
| `--spans` | include byte spans |
| `--typed` | `hir` only: after phase 2b, type map total rather than empty |

Three reasons this shape beats a rustc-style `--unpretty=<mode>,<modifier>`:

1. **`yelc ast` and `yelc2 ast` become directly comparable**, which is exactly
   what the differential wants. A flag idiom the frozen CLI does not share makes
   every side-by-side invocation asymmetric.
2. Modifiers are ordinary flags rather than comma-separated values inside a
   value.
3. `ir` keeps the name yel already uses for LIR, instead of introducing `lir` as
   a second name for one thing.

**`--unpretty` was considered and rejected.** It is a historical accident in
rustc: there was once a stable `--pretty` for pretty-printing source, the
unstable structural dumps went behind `-Z unpretty` to contrast with it, and then
`--pretty` was removed — leaving a flag named against something that no longer
exists. Copying it would import the baggage and none of the meaning.

What *is* worth taking from rustc is `identified` — node ids in the dump, so a
diagnostic or an LSP request can be pointed at a node. That is a capability, not
a naming convention.

**`expanded` is deliberately absent.** In rustc it means "after macro and
`#[derive]` expansion" and yel has neither. Yel's analogous idea is *desugaring*
— `if`/`for` nodes becoming block structure — which does not happen until LIR
lowering and is already visible as `ir`. An `expanded` that meant something
different from rustc's would be worse than not having one.

`--typed` is the yel-specific one, and the reason the merged HIR needs a driver
at all: **phase 2a's output is a public surface**
([`seam-changes.md`](seam-changes.md)), so `hir` and `hir --typed` are the two
positions lints and the LSP will read. If the driver cannot show both, nothing
else can either.

### Also

`check` renders diagnostics through `yelc-base`'s renderer — the same one `yelc
check` uses, so the two are comparable line for line.

`diff` is the differential runner. It currently lives inside
`yelc-syntax/tests/parity.rs` as a dev-dependency on the frozen crate — a harness
living in a test file of the crate under test. Moving it here makes it runnable
by hand, which is what every review round actually wanted.

## Constraints

- **Depends on `yelc-base` + `yelc-syntax`** today, gaining crates as stages
  land. It is the **only** crate permitted to depend on the frozen tree
  (`yel-core`, for `diff`), and that dependency is deleted at cutover phase 4
  ([anti-spec A4](anti-spec.md#a4--no-permanent-bridge)).
- **No stage selection yet.** There is one implementation of each stage; a
  selector between implementations is dead weight until there are two. It
  arrives when the first stage has both an old and a new path to choose between.
- **The switch selects an implementation, never a behaviour**
  ([`cutover-switch-then-delete`](../../.agents/skills/compiler-rewrite/rules/cutover-switch-then-delete.md)).
  The moment anyone writes `if new { emit_extra_op() }` the differential numbers
  stop meaning anything.
- Output is for humans and for `diff`; it is **not** a golden. Nothing in
  `tests/` asserts on its text, or the driver becomes a thing that must not
  change.

## Definition of done

- [ ] `ast` and `green`, each with `--identified` / `--spans` / `--text`, over
      any `.yel` file.
- [ ] `check` renders diagnostics identically to `yelc check` for the same input
      — same `ErrorCode`, same span. It shares `yelc-base`'s renderer, so this is
      a wiring check, not a reimplementation.
- [ ] `diff` reproduces `tests/parity.rs`'s accept/reject verdict on the corpus,
      and `tests/identity.rs`'s construct-identity verdict.
- [ ] `cargo test --workspace` ≥ the [ratchet](ratchet.md); execution 85/85.
- [ ] Freeze check clean — `scripts/freeze-check.sh`.
- [ ] Under 400 lines. If it is bigger, it stopped being thin.

## Numbers · Decision log · Surprises

*Filled in at close-out.*
