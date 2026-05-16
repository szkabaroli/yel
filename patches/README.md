# patches/

Out-of-tree patches against external dependencies that yel needs to expose
APIs the upstream crate doesn't ship. The patches mutate the cargo
registry source cache (`~/.cargo/registry/src/index.crates.io-*/<crate>-<ver>/`)
in place, so no vendored copies live in this repo and no
`[patch.crates-io]` override is needed in the workspace manifest.

Run after a fresh checkout, after `cargo update`, after wiping
`~/.cargo/registry`, or after bumping a patched dependency's version.

## Apply

```sh
patches/apply.sh
```

Idempotent — re-running after a successful apply does nothing. To check
the patches still match upstream without modifying anything:

```sh
PATCH_DRY_RUN=1 patches/apply.sh
```

To revert (e.g., before reporting a bug to the upstream crate):

```sh
patches/apply.sh --reverse
```

## What each patch does

| File | Crate | Adds | Used by |
|---|---|---|---|
| `wasmtime-44-core-instance.patch` | `wasmtime@44.0.0` | `wasmtime::component::Instance::core_instance(store, idx) -> Option<crate::Instance>` | `yel-host gc-dump` — walks component-internal Wasm-GC structs/arrays for runtime debugging. Components otherwise hide their core instances. |

## When upgrading wasmtime

1. Bump the version pin in the workspace `Cargo.toml`.
2. Update `WASMTIME_VERSION` at the top of `apply.sh` to match.
3. Re-roll the patch against the new source — the line offsets in
   `wasmtime-44-core-instance.patch` will likely shift.
   - Easiest: revert with `--reverse`, edit the source file directly,
     `diff -u original modified > new.patch`, replace the file.
4. Run `PATCH_DRY_RUN=1 patches/apply.sh` to confirm clean apply.
5. Run the full test suite.

## Why not `[patch.crates-io]`?

Two reasons:

1. **No vendored wasmtime in the repo.** A `[patch.crates-io]` entry
   needs a path-or-git source. Path means committing a wasmtime fork
   (~hundreds of files, gigabytes of build cache); git means a hard
   network dependency at every clone.
2. **Source-level patches diff cleanly against upstream.** Reading
   `wasmtime-44-core-instance.patch` shows exactly what changed. A
   vendored copy hides that.

The cost is that `apply.sh` mutates a shared cache. If a teammate
uses the same `~/.cargo/registry` for an unrelated wasmtime project,
they'll inherit our patches. In practice cargo treats the registry
as read-only and our touched files have unique markers in their doc
comments (`# Yel-host patch`), so the bleed is detectable.
