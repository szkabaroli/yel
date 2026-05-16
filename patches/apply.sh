#!/usr/bin/env bash
#
# Apply local wasmtime patches to the cargo registry source cache.
#
# These patches add APIs the upstream wasmtime crate doesn't expose
# (most notably `wasmtime::component::Instance::core_instance` for GC
# dumping). They live out-of-tree on purpose: the project doesn't
# vendor wasmtime, and `[patch.crates-io]` would still need a copy of
# the source. The cleanest tradeoff is to mutate the existing cargo
# cache so cargo's regular dep resolution picks up the patched build.
#
# Re-run after `cargo update`, after a clean `~/.cargo/registry`, or
# after upgrading the wasmtime version in the workspace `Cargo.toml`.
#
# Usage:
#   patches/apply.sh                # patch the canonical 44.0.0 cache
#   PATCH_DRY_RUN=1 patches/apply.sh  # check that the patches still apply
#   patches/apply.sh --reverse      # undo
set -euo pipefail

REPO_ROOT=$(cd "$(dirname "$0")/.." && pwd)
PATCH_DIR="$REPO_ROOT/patches"

# Pin the exact crate version we patch. Bump in lockstep with the
# workspace Cargo.toml.
WASMTIME_VERSION="44.0.0"

# Locate the registry source dir. Cargo does NOT keep a stable name
# for the registry root (it depends on the registry index hash), so
# we glob.
REGISTRY_ROOTS=("$HOME"/.cargo/registry/src/index.crates.io-*)
if [ ! -d "${REGISTRY_ROOTS[0]}" ]; then
    echo "error: no cargo registry source dir found under ~/.cargo/registry/src/" >&2
    echo "       make sure you've run 'cargo build' at least once first." >&2
    exit 1
fi
REGISTRY_ROOT="${REGISTRY_ROOTS[0]}"

REVERSE=""
if [ "${1:-}" = "--reverse" ]; then
    REVERSE="--reverse"
fi

DRY_RUN=""
if [ -n "${PATCH_DRY_RUN:-}" ]; then
    DRY_RUN="--dry-run"
fi

apply_one() {
    local patch_file="$1"
    local target_dir="$2"
    if [ ! -d "$target_dir" ]; then
        echo "skip: $patch_file (target dir missing: $target_dir)"
        return
    fi
    echo "==> $(basename "$patch_file") -> $target_dir"
    # `patch -N` makes re-applying a no-op (else 'previously applied' bails).
    # `-p1` strips the leading 'a/' / 'b/' prefix from the unified diff.
    if [ -n "$REVERSE" ]; then
        patch $DRY_RUN -R -p1 -d "$target_dir" -i "$patch_file"
    else
        patch $DRY_RUN -N -p1 -d "$target_dir" -i "$patch_file" || {
            local rc=$?
            # `patch -N` returns 1 when the patch was already applied;
            # treat that as success.
            if [ $rc -eq 1 ]; then
                echo "    already applied (skipping)"
            else
                exit $rc
            fi
        }
    fi
}

apply_one \
    "$PATCH_DIR/wasmtime-44-core-instance.patch" \
    "$REGISTRY_ROOT/wasmtime-$WASMTIME_VERSION"

# Force cargo to rebuild the patched crate without nuking sibling
# build-script artifacts (wasmtime ships native static helpers via
# build.rs that mustn't be deleted). `cargo clean -p` is targeted —
# only `wasmtime`'s own .rlib gets removed; helpers stay intact.
if [ -z "$REVERSE" ] && [ -z "$DRY_RUN" ]; then
    (cd "$REPO_ROOT" && cargo clean -p wasmtime >/dev/null 2>&1) || true
fi

echo "done. run 'cargo build -p yel-host' to pick up the change."
