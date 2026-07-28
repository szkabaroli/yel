# Stage 4 — `yelc-codegen`                             status: not started

Replaces (frozen, never edited): `crates/yel-wasm-codegen/`
Base: — · Started: — · Landed: —

> **Stub**, except for the final-deletion task, which is
> [recorded now](#final-deletion--cutover-phase-4) — a coexistence without a
> scheduled deletion is a fork.

## Brief

*To be written.* LIR → WASM component + WIT + DOT.

Depends on `{ yelc-lir, yelc-base }` **and nothing else**. No dependency path to
any frontend crate; see [stage 3](stage-3-lir.md#why-it-is-split).

Must honour:

- **Byte-identical WIT and DOT for the 91 positive fixtures.**
- **The 85 execution tests pass, unmodified.** They are the only semantic oracle
  in the project — real DOM-op behaviour under Wasmtime. A stage that passes WIT
  snapshots but drops an execution test has miscompiled something; the snapshot
  just was not looking.
- `wasm-tools validate` clean on all 2000 corpus components, and byte-identical
  to the frozen `corpus/wasm/`.
- The `yel:ui/dom@0.1.0` import surface matches instruction-for-instruction —
  the host on the other side is not part of this rewrite.
- Determinism: `FxHashMap`/`FxHashSet`, sort before emitting, the
  disallowed-types lint denied.
  [Anti-spec A6](anti-spec.md#a6--no-random-seeded-iteration-reaching-output) —
  this was ~35/200 seeds emitting byte-different modules run-to-run.
- Carry the `WitBoundary` witness ([keep-list §10](keep-list.md#10--the-witboundary-witness-stage-4)).

Open debt this stage is expected to *fix*, not port — each changes behaviour, so
each lands as its own enumerated divergence with an execution test:

- **`[resource-drop]` missing** — freed component handles never return to the
  registry → handle leak over a long session.
- **Callback-arg buffers never freed** — `string`/`list` arguments materialized
  into linear memory for a host import are never released; there is no
  `cabi_post` equivalent on the *import* (lower) side. Per-invocation leak.
  Both are [anti-spec C3](anti-spec.md#c3--no-resource-acquired-without-a-release-path).
- **Host-imported callbacks not wired** (`wasm/expr.rs:406`).
- **`lir_rust.rs`** — the LIR→Rust generator, commented out of `lib.rs`. Delete
  it; do not port it ([anti-spec C7](anti-spec.md#c7--no-output-format-generator-that-is-not-wired-up)).
- Stringify arms that `todo!()` on unexpected arity; the numeric-repr scalar
  fast-path falling back to S32.

Frozen-for-now, deliberately: the WIT package version hard-defaults to `0.1.0`
when a source omits it. Changing it changes output — separate approved decision.

## Contract · Reference · Definition of done · Numbers · Decision log · Surprises

*To be written.*

---

# Final deletion — cutover phase 4

> **Named and scheduled before phase 1 merges**, per
> [`cutover-switch-then-delete`](../../.agents/skills/compiler-rewrite/rules/cutover-switch-then-delete.md).
> A coexistence without a scheduled deletion is a fork. This section is written
> now, at stage 0, and is not "to be written."

**Task:** *Delete the frozen compiler.*
**Owner:** rewrite owner (not delegated to a stage agent).
**Scheduled:** after stage 4 closes + a **14-day bake period** from the flip.
The bake is a fixed number of days decided up front — "until we feel confident"
is not a date, and confidence is not an event.

## The four phases

| # | Phase | State |
|---|-------|-------|
| 1 | **coexist** | New crates land beside the frozen ones. Old is the default and the shipping compiler. CI builds and tests both. ← **we are here** |
| 2 | **parity** | All five stages complete. Differential sweep over the full 2000-seed corpus shows **zero unexplained divergences**; every ratchet number met or beaten. |
| 3 | **flip** | `yelc-driver`'s binary becomes `yelc`. The frozen tree is still present and still built in CI, for the 14-day bake. |
| 4 | **delete** | One PR, almost entirely deletions. |

## What phase 4 deletes

```
crates/yel-core/                 (front-end + IRs)
crates/yel-wasm-codegen/         (back-end)
crates/yelc/                     (old CLI; binary name already moved at flip)
```

Plus, in the same PR:

- Every throwaway adapter written during the rewrite. Each is named in its stage
  file; the reviewer checks each one is gone
  ([anti-spec A4](anti-spec.md#a4--no-permanent-bridge)).
- The stage-selection seam in `yelc-driver` — once there is only one
  implementation, a selector between implementations is dead weight.
- Workspace members, path deps, and CI matrix entries for the deleted crates.

## Preconditions — all must hold, checked in the deletion PR

- [ ] Stage 4 closed out; ratchet row landed and met.
- [ ] Zero unexplained corpus divergences across all 2000 seeds.
- [ ] 85/85 execution tests pass against the new compiler, **unmodified**.
- [ ] 91 positive fixtures byte-identical; 23 diagnostic fixtures same meaning.
- [ ] Ignored-test count ≤ 2.
- [ ] Bake period elapsed with no revert.
- [ ] **Corpus provenance resolved.** After deletion the corpus can never be
      regenerated — the compiler that produced it is gone. The corpus is tracked
      in full via git-lfs precisely so this is survivable; the PR verifies
      `shasum -c corpus/SHA256SUMS` passes and that LFS content is fetchable, and
      `corpus.md` gains a line stating that `scripts/freeze-corpus.sh` is now
      historical and cannot be re-run.
- [ ] `docs/ARCHITECTURE.md` and `docs/PIPELINE.md` **rewritten** — they describe
      the deleted compiler, so they are replaced from `plans/rewrite/`, not
      patched. `docs/TECH_DEBT.md` restarted as an inventory of the new tree's
      debt. `CLAUDE.md` and every crate `CLAUDE.md` updated in the same PR.
- [ ] `scripts/freeze-corpus.sh` marked historical (it references `yelc` from the
      deleted tree).

## Rules that keep the cutover honest

- **The switch selects an implementation, never a behaviour.** The moment anyone
  writes `if new { emit_extra_op() }`, the two paths have diverged semantically
  and every differential number measured since becomes meaningless.
- **The switch lives in the new driver**, the one place that knows both trees
  exist. Never a flag threaded into the frozen tree — that would violate the
  freeze *and* fork the code.
- **The corpus is regenerated from the old tree only**, which is why the old tree
  is deleted **last**, not first.
- **No adapter outlives the stage that needed it.**
