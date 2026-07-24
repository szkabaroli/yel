# cutover-switch-then-delete

> The two trees coexist only to be diffed — the deletion of the old one is scheduled before the first new line is written

## Why It Matters

The rewrite needs the old compiler alive: it is the differential baseline and it
is what ships while the new one is being built. But "two compilers in the
repository" is also the exact shape of the debt the rewrite is escaping —
`docs/TECH_DEBT.md §1` is a catalogue of transitional bridges that outlived
their transitions. What separates a cutover from a permanent fork is whether the
deletion was planned at the same moment as the coexistence.

Because the old tree is frozen and never edited
([`greenfield-never-touch-old-code`](greenfield-never-touch-old-code.md)), the
switch cannot live inside it. It lives in the **new** driver, which is the one
place that knows about both. This is a better arrangement than an in-place flag:
the old path cannot rot from edits, and the branch point is a single function in
new code rather than a hundred `if` statements scattered through the tree being
replaced.

## Bad

```rust
// A flag threaded into the frozen tree — violates the freeze AND forks the code.
 pub fn lower_to_hir(&mut self, ctx: &mut CompilerContext) -> HirModule {
+    if std::env::var("YEL_HIR_NEW").is_ok() { return self.lower_to_hir_v2(ctx); }
     self.lower_items(ctx)
 }
```

The baseline now contains the thing being tested against it, and by stage 5
nobody dares touch either branch.

## Good

**Per stage** — the new stage runs against the old pipeline's neighbours, so it
can be diffed in isolation. The seam is in the new driver:

```rust
// yelc2/src/pipeline.rs — the ONLY place that knows both trees exist.
// Each stage independently selectable, so a divergence is attributable.
pub struct StageSelection { pub syntax: Impl, pub hir: Impl, pub thir: Impl, /* … */ }

// YEL_STAGES=hir=new,thir=old … one binary runs every combination.
```

Under a strict freeze the new stage cannot feed the *old* downstream stages
directly — the IRs differ. Two workable arrangements, chosen per stage and
recorded in the stage file:

- **Artifact-level diff** (preferred, no adapter): run the whole old pipeline
  and the partially-new pipeline over the same corpus and compare final
  artifacts — WIT, DOT, WASM, diagnostics. Needs every stage up to N to be new.
  This is why the stage order is the pipeline order.
- **Throwaway adapter** (when an earlier stage is not yet rewritten): a
  new-IR → old-IR shim living in the *new* tree, written to be deleted when the
  next stage lands. It ships with its deletion commit, in the same PR series,
  and it is an anti-spec violation to still exist one stage later.

**Globally** — the four-phase sequence, planned up front:

```
1. coexist    New crates land beside the frozen ones. Old is the default and
              the shipping compiler. CI builds and tests both.
2. parity     All five stages complete. Differential sweep over the full corpus
              shows zero unexplained divergences; ratchet numbers met or beaten.
3. flip       yelc2 becomes yelc. The old tree is still present, still built in
              CI, for one fixed bake period.
4. delete     The frozen tree is removed. One PR, almost entirely deletions.
```

Rules that keep it honest:

- **Phase 4 is a named, assigned task before phase 1 merges**, listing the
  directories it will delete. A coexistence without a scheduled deletion is a
  fork.
- **The bake period is a fixed number of days, decided up front.** "Until we
  feel confident" is not a date, and confidence is not an event.
- **The switch selects an implementation, never a behaviour.** The moment
  someone writes `if new { emit_extra_op() }`, the two paths have diverged
  semantically and every differential number since becomes meaningless.
- **No adapter outlives the stage that needed it.** Track them in the stage file
  by name so the next stage's reviewer can check they are gone.
- **The corpus is regenerated from the old tree only.** After the flip, the
  frozen tree is still the provenance of every recorded baseline — which is why
  it is deleted last, not first.

## See Also

- [greenfield-never-touch-old-code](greenfield-never-touch-old-code.md) - Why the switch lives in the new driver
- [stage-gate-sequential](stage-gate-sequential.md) - The per-stage gates that lead to phase 2
- [verify-differential-not-review](verify-differential-not-review.md) - What coexistence exists to enable
- [`anti-permanent-bridge`](../../compiler-skills/rules/anti-permanent-bridge.md) - The general form of this failure
