#!/usr/bin/env bash
#
# freeze-check.sh — is the frozen compiler unmodified?
#
# The rewrite's first invariant: crates/{yel-core,yel-wasm-codegen,yelc} are
# read-only reference — the differential baseline and the shipping product.
# See plans/rewrite/README.md and the greenfield-never-touch-old-code rule.
#
# Usage:
#   scripts/freeze-check.sh              # working tree vs HEAD
#   scripts/freeze-check.sh <base-ref>   # working tree vs a named base
#
# Exit 0 = clean, 1 = violation. Prints the offending paths.
#
# WHY THIS IS A SCRIPT AND NOT A ONE-LINER
#
# The obvious inline form is cwd-dependent and fails OPEN:
#
#     git status --porcelain -- crates/yel-core … | wc -l
#
# Run from crates/ (a normal place to be in this repo), the pathspec resolves
# to crates/crates/yel-core, matches nothing, and prints 0 — indistinguishable
# from "clean". git emits a warning, but `| wc -l` discards it. That is a
# count-based assertion that passes vacuously: anti-spec A8/A14, the exact shape
# the review panel enforces on stage agents. It went undetected here for a full
# stage because the number it printed was the number expected.
#
# This script anchors every path to the repo root, and treats an empty match set
# as an error to investigate rather than a pass.
set -uo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "freeze-check: not inside a git repository" >&2
  exit 2
}
cd "$ROOT" || exit 2

BASE="${1:-HEAD}"

FROZEN=(
  "crates/yel-core"
  "crates/yel-wasm-codegen"
  "crates/yelc"
)

# Guard against the failure mode above: if a frozen path does not exist, the
# pathspec is wrong (or the tree moved) and every check below is vacuous.
for p in "${FROZEN[@]}"; do
  if [ ! -d "$ROOT/$p" ]; then
    echo "freeze-check: FATAL — frozen path '$p' does not exist under $ROOT." >&2
    echo "  The check cannot pass or fail meaningfully. Fix the path list." >&2
    exit 2
  fi
done

uncommitted="$(git status --porcelain -- "${FROZEN[@]}")"
# Commits since BASE, excluding the working tree (which `uncommitted` covers).
# With the default BASE=HEAD this is empty by construction.
committed="$(git diff --name-only "$BASE" HEAD -- "${FROZEN[@]}" 2>/dev/null)"

if [ -z "$uncommitted" ] && [ -z "$committed" ]; then
  echo "freeze-check: clean — frozen tree unmodified (base: $BASE)"
  exit 0
fi

echo "freeze-check: FREEZE VIOLATION (base: $BASE)" >&2
[ -n "$uncommitted" ] && { echo "  uncommitted:" >&2; sed 's/^/    /' <<<"$uncommitted" >&2; }
[ -n "$committed" ]   && { echo "  committed vs $BASE:" >&2; sed 's/^/    /' <<<"$committed" >&2; }
cat >&2 <<'EOF'

  The frozen tree is the differential baseline and the shipping compiler.
  If this is deliberate work on the shipping product (a bug fix, a feature),
  it is legitimate — but it invalidates the freeze point, so it requires:
    1. commit it,
    2. regenerate the corpus:  scripts/freeze-corpus.sh 2000 corpus
    3. add a NEW baseline row to plans/rewrite/ratchet.md citing the new SHA,
    4. re-check any in-flight stage against the moved grammar/behaviour.
  If it is a rewrite agent editing the old tree, it is a stage failure.
EOF
exit 1
