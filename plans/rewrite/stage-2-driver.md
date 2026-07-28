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

A **thin** driver over the new crates, with rustc-style IR dump flags.

Thin is the requirement, not an aspiration. The moment it grows behaviour of its
own it becomes a third implementation to keep in sync with two others. It
formats and routes; it does not decide anything about the language.

### The flag surface

Modelled on `rustc -Z unpretty=…`, which is the closest prior art and already
familiar. One flag, a value, and optional comma-separated modifiers:

```
yelc2 --unpretty=<mode>[,<modifier>…] <file>
```

| mode | dumps | available from |
|---|---|---|
| `ast-tree` | the typed AST, structurally | **stage 1** |
| `green` | the lossless green tree, kinds + widths | **stage 1** |
| `green-text` | `green.text()` — the S1 round-trip, for eyeballing | **stage 1** |
| `hir` | HIR, pretty-printed as source-like text | stage 3a |
| `hir-tree` | HIR, structurally | stage 3a |
| `lir` | LIR blocks and ops | stage 4a |

| modifier | effect |
|---|---|
| `identified` | include `NodeId` / `HirId` on every node — rustc's `expanded,identified` |
| `spans` | include byte spans |
| `typed` | HIR only: dump after phase 2b, with the type map total rather than empty |

`typed` is the yel-specific one and it is the reason the merged HIR needs a
driver at all: **phase 2a's output is a public surface**
([`seam-changes.md`](seam-changes.md)), so `--unpretty=hir` and
`--unpretty=hir,typed` are the two positions lints and the LSP will read. If the
driver cannot show both, nothing else can either.

Deliberately **not** copied from rustc: `expanded`. It means "after macro and
`#[derive]` expansion", and yel has neither. Yel's analogous idea is
*desugaring* — `if`/`for` nodes becoming block structure — which does not happen
until LIR lowering, and is already visible as `lir`. Adding an `expanded` that
means something different from rustc's would be worse than not having it.

### Also

```
yelc2 check <file>     parse + report through Diagnostics::render(&SourceMap)
yelc2 diff  <file>     run the frozen and new front ends, print what differs
```

`diff` is the differential runner. It currently lives inside
`yelc-syntax/tests/parity.rs` as a dev-dependency on the frozen crate — a
harness living in a test file of the crate under test. Moving it here makes it
runnable by hand, which is what every review round actually wanted.

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

- [ ] `--unpretty=ast-tree`, `green`, `green-text`, each with `identified` and
      `spans`, over any `.yel` file.
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
