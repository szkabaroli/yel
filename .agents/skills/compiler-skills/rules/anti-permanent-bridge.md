# anti-permanent-bridge

> When migrating a representation, converge — a `legacy_*` shim or flag-gated second codepath only stays healthy while it's shrinking

## Why It Matters

Incremental migration is the right way to change an IR without a big-bang rewrite: introduce a bridge (a shim like `legacy_u32()`, a flag that falls through to the "old path", two coexisting representations), migrate call sites, delete the bridge. The failure mode is leaving the bridge in place **indefinitely**. With no finish line and no tracked count of remaining call sites, the old path never dies, both representations must be maintained forever, snapshots straddle two encodings, and new code copies the legacy idiom because it's still there. Treat every bridge as debt with an explicit burn-down to zero. yel is mid-migration on several of these — `LirSlotId::legacy_u32()` (every call site is a migration site), a context flag that "suppresses the legacy `LirOp::TriggerEffects` emission … otherwise falls through to the legacy path", and the canonical-flat ↔ typed-GC dual representation. `Phase N` labels in comments track progress; the risk is they stall (`docs/TECH_DEBT.md §1.1, §1.4, §1.5`).

## Bad

```rust
impl LirSlotId {
    // "temporary" bridge to the old raw-u32 world… called in 200 places, for two years
    pub fn legacy_u32(self) -> u32 { /* … */ }
}
fn emit_signal_write(&mut self, ..) {
    if self.use_new_path { /* typed path */ } else { /* legacy path, kept "for now" */ }
    // no inventory of remaining legacy call sites → neither path ever wins
}
```

## Good

```rust
// A migration with an exit criterion: the bridge count trends to zero and is deleted.
// 1. introduce the typed API; 2. migrate call sites (track the remaining count);
// 3. when `legacy_u32()` has zero callers, delete it and the old path.
#[deprecated = "migrating to typed LirSlotId; 12 call sites left — see TECH_DEBT §1.1"]
fn legacy_u32(self) -> u32 { /* … */ }
```

## See Also

- [ir-lower-away-domain](ir-lower-away-domain.md) - A migration this discipline keeps on track
- [cg-flatten-at-boundary](cg-flatten-at-boundary.md) - The typed-GC target the dual-repr bridge converges toward
- [test-snapshot-golden](test-snapshot-golden.md) - Snapshots regenerate as a bridge burns down; keep them green throughout
