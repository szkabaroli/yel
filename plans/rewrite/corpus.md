# The frozen corpus — the oracle

> Rules: [`oracle-freeze-behaviour`](../../.agents/skills/compiler-rewrite/rules/oracle-freeze-behaviour.md),
> [`oracle-never-rebless`](../../.agents/skills/compiler-rewrite/rules/oracle-never-rebless.md)

The old compiler is unreadable and correct in thousands of small ways nobody has
written down: which programs are rejected and with what wording, how a nested
`for` inside an `if` orders its DOM ops, what the WIT looks like for an
option-of-record. That knowledge lives only in the binary's behaviour, and a
rewrite loses it **silently** — a stage that drops an unwritten behaviour still
compiles and still passes the tests someone happened to keep.

This corpus converts that behaviour into artifacts on disk. It was captured
while the old compiler was the *only* compiler, so there was no temptation to
"fix" a captured output.

The 137 fixtures cover what someone thought to test. The 2000 seeds cover what
nobody did, and they are the larger half.

---

## Provenance

| | |
|---|---|
| **Freeze SHA** | `33e5c7147a84f634eafb3303de495300f27ef585` |
| **Previous freeze** | `ccf2086` — superseded, see below |
| **Date** | 2026-07-28 (regenerated) |
| **Generator** | [`scripts/freeze-corpus.sh`](../../scripts/freeze-corpus.sh) |
| **Seeds** | 1 … 2000 (`yel-smith --seed N`) |
| **Toolchain** | release build of `yelc` + `yel-smith` at the freeze SHA; `wasm-tools 1.227.1` |

`corpus/MANIFEST` records the exact SHA, per-format counts, failure tallies, and
a `compiler_sources_clean=yes|NO` flag. If that flag ever reads `NO`, the corpus
is **not** reproducible and must be regenerated from a clean checkout before it
is trusted as a baseline.

## Why it was regenerated

The first corpus was frozen at `ccf2086`. Commit `c51b51d` then renamed
`import component` to `extern component` — a **surface language change on the
frozen tree**, which is legitimate shipping work but moves the freeze point. The
`ccf2086` artifacts described a compiler that no longer exists, and diffing
against a moving target is the one thing the freeze exists to prevent.

The manifest names `33e5c71` (the stage-1 commit) rather than `c51b51d`, because
that is the tree the generating binary was built from. The **frozen half is
identical between them** — stage 1 only added `yelc-syntax` and `yelc-base`, and
`cargo test --workspace --exclude yelc-syntax --exclude yelc-base` gives the same
315 / 0 / 2 at both. So the corpus provenance is the frozen compiler at
`c51b51d`, which is the baseline row it is compared against.

## Layout

```
corpus/                                                209 MB on disk, mostly untracked
  src/N.yel      × 2000   generated programs — the frozen INPUTS   [LOCAL ONLY]
  wit/N.wit      × 2000   old compiler's WIT                       [LOCAL ONLY]
  dot/N.dot      × 2000   old compiler's DOT                       [LOCAL ONLY]
  wasm/N.wasm    × 2000   old compiler's component                 [LOCAL ONLY]
  SHA256SUMS              sha256 of all 8000 files above           [git, plain]
  known-failures.txt      seed|stage|first error line — EMPTY      [git, plain]
  MANIFEST                provenance + counts                      [git, plain]
```

### The bodies are local-only — changed 2026-07-30

They were tracked in full via git-lfs. That was removed from history, for two
reasons that turned out to be the same reason:

- **The first push never completed.** 8000 LFS objects, none previously on the
  remote, hit GitHub's LFS batch API rate limit partway through and failed.
- **8000 entries make a PR unreviewable** regardless of content — the diff is
  rendering 8000 rows, and no reviewer reads past that.

**What this costs, stated plainly, because the argument for tracking has not
stopped being true:**

Once the frozen tree is deleted (cutover phase 4) the artifact bodies **cannot
be regenerated at all**. Before then they can only be regenerated at the freeze
SHA — and *"seed N does not reliably reproduce the same program, because
`yel-smith` is part of the workspace and its generator will change."* A
regenerated corpus is a **different corpus** unless the freeze SHA matches
exactly. There is now no backup of these 8000 files anywhere but this working
tree.

**What survives, and why it is the part that mattered most:**

`SHA256SUMS`, `MANIFEST` and `known-failures.txt` stay tracked — 617 KB, 0.3% of
the corpus. One `shasum -c` still answers *"did anything move?"* across all 8000
files, and `MANIFEST` still pins the freeze SHA, the toolchain and the counts.
Losing the bodies costs the ability to **diff** a divergence; losing these would
have cost the ability to **detect** one, which is strictly worse and was avoided
for a rounding error of the size.

**Unresolved: where the bodies live.** A release artifact, a second repository,
and an object store are all viable and none is chosen. Until one is, the corpus
exists on exactly one machine and the differential dies with that disk. That is
a real gap, recorded here rather than discovered later.

`SHA256SUMS` is the cheap check — one `shasum -c` answers "did anything move?"
over all 8000 files, and it stays diffable and reviewable in a PR.

Note `src/` in particular is not optional: seed N does **not** reliably reproduce
the same program, because `yel-smith` is part of the workspace and its generator
will change. The `.yel` files, not the seeds, are the corpus.

## Measured at the freeze (regenerated 2026-07-28)

```
freeze_sha=33e5c7147a84f634eafb3303de495300f27ef585
compiler_sources_clean=yes
seeds=2000
yelc_version=yelc 0.1.0 (33e5c71 2026-07-28 10:35:53 +00:00)
rustc: rustc 1.96.0 (ac68faa20 2026-05-25)
wasm_tools_version=wasm-tools 1.227.1
src_count=2000   wit_count=2000   dot_count=2000   wasm_count=2000
known_failures=0
fail_generate=0  fail_wit=0  fail_dot=0  fail_wasm=0  fail_validate=0
```

**Zero known failures.** All 2000 seeds generate, compile to WIT, DOT and WASM,
and every component passes `wasm-tools validate`.

That is a stronger baseline than expected and it has a direct consequence for
how the rewrite is judged, spelled out in [`ratchet.md`](ratchet.md): the pass
rate is **saturated**, so it can only ever detect regressions — it cannot reward
the rewrite for fixing latent bugs. The metrics that carry the "did this find
anything?" signal are the **corpus divergence count** and the `known_bugs`
fixture directory, not the fuzz number.

It also raises the bar on what a divergence means. With no failing seeds to hide
behind, *any* artifact difference at any stage is a real behavioural change that
has to be explained — there is no "that seed was already broken" escape hatch.

## Known failures at baseline — none

`corpus/known-failures.txt` is **empty**. The format, for when it is not:

```
seed|stage|first line of stderr        # stage ∈ generate | wit | dot | wasm | validate
```

The file exists even while empty because the comparison it enables is
**set-vs-set, never count-vs-count**. A rewrite that fixes a seed is a win; a
rewrite that fails a *different* seed is a regression wearing the same pass-rate
number. At this baseline the set is empty, so the rule reduces to: any stage that
produces a non-empty `known-failures.txt` has regressed, full stop.

## Regeneration

```bash
# Full regeneration from the frozen tree. Destroys and rebuilds corpus/.
scripts/freeze-corpus.sh 2000 corpus
```

The script forces a clean release relink (`touch crates/yelc/src/main.rs`) before
measuring, stamps `MANIFEST` with the SHA it ran at, and refuses to claim
reproducibility if `crates/` or `Cargo.*` are dirty.

**Regenerate only from the old compiler.** Regenerating from a rewritten
compiler does not update the oracle — it deletes it, and every green run
afterwards is meaningless. If a stage's output legitimately differs, the change
is recorded in [`goldens-changed.md`](goldens-changed.md) with the diff read and
justified; the corpus itself is not re-blessed.

## Using it — the differential sweep

After stage N, compile every corpus program both ways and diff artifacts. This
is the primary gate; review cannot clear a rewrite.

```bash
# Artifact-level differential: old binary vs new binary over the frozen inputs.
touch crates/yelc/src/main.rs && cargo build --release -p yelc -p yelc2
diverged=0
for f in corpus/src/*.yel; do
  s=$(basename "$f" .yel)
  for fmt in wit dot; do
    ./target/release/yelc2 compile -o $fmt "$f" > /tmp/new.$fmt 2>/dev/null
    cmp -s "corpus/$fmt/$s.$fmt" /tmp/new.$fmt || { echo "DIVERGE $fmt seed=$s"; diverged=$((diverged+1)); }
  done
done
echo "divergences: $diverged"
```

Three properties make this cheap:

- **Attribution.** Exactly one stage is in flight, so a divergence has exactly
  one suspect. This is the entire reason the stages are sequential.
- **Artifact-level, not IR-level.** The IRs differ by construction; the outputs
  must not. Comparing final artifacts is the only comparison that stays valid as
  the internals are replaced.
- **Inputs neither implementation was tuned for.** The fixtures were written
  against the old compiler's behaviour; the seeds were not written against
  anything.

When a divergence appears, switch to
[`/fuzzer-debugging`](../../.agents/skills/fuzzer-debugging/SKILL.md) — measure,
categorize, delta-minimize to a one-line repro, locate. That skill is the
debugging loop; this file is the input to it.

## Widening

2000 seeds is the baseline width, not a ceiling. A stage that touches
representation (4 and 5 especially) should sweep wider — the seeds cost nothing
but wall-clock, and the corpus generator is deterministic given `yel-smith` at
the freeze SHA:

```bash
scripts/freeze-corpus.sh 10000 corpus-wide   # ad-hoc, not committed
```

Record the width actually swept in the stage file. A stage that reports "0
divergences" over 200 seeds and a stage that reports it over 10000 are not
making the same claim.
