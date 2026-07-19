# test-deterministic-output

> Sort and dedup collections before emitting so output is byte-stable across runs

## Why It Matters

Snapshot and golden tests are only meaningful if the compiler's output is deterministic; anything derived from `HashMap`/`HashSet` iteration order will flap from run to run and make snapshots useless. yel enforces stable order wherever iteration would otherwise be nondeterministic: dependency collection sorts and dedups before use (`deps.sort_by_key(|d| d.0); deps.dedup();` in `lower_to_lir/component.rs`; `out.sort_by_key(|d| d.index())` in `thir/signalck.rs`), and the block-dedupe pass normalizes slot ids so structurally-equal blocks hash identically.

## Bad

```rust
// Emits deps in HashSet iteration order — snapshot diffs flap run-to-run
let deps: HashSet<DepId> = collect_deps(node);
for d in deps { emit_dep(d); }
```

## Good

```rust
// Impose a stable total order before anything reaches the output
let mut deps: Vec<DepId> = collect_deps(node).into_iter().collect();
deps.sort_by_key(|d| d.0);
deps.dedup();
for d in &deps { emit_dep(*d); }
```

## See Also

- [test-snapshot-golden](test-snapshot-golden.md) - Why byte-stability is non-negotiable
- [pass-postpass-dedupe](pass-postpass-dedupe.md) - The dedupe pass relies on normalized ids
