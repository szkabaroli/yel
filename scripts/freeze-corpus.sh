#!/usr/bin/env bash
#
# freeze-corpus.sh — regenerate the compiler-rewrite oracle corpus.
#
# Compiles N yel-smith seeds with the FROZEN (old) compiler and records its
# WIT / DOT / WASM artifacts plus the set of seeds that fail today.
#
# This corpus is the differential baseline for the internals rewrite. It is
# generated from the old compiler ONLY — never from yelc2. Regenerating it from
# a rewritten compiler deletes the oracle. See plans/rewrite/corpus.md and
# .agents/skills/compiler-rewrite/rules/oracle-never-rebless.md.
#
# Usage:
#   scripts/freeze-corpus.sh [SEED_COUNT] [OUT_DIR]
#
# Defaults: 2000 seeds into ./corpus
#
set -uo pipefail

SEEDS="${1:-2000}"
OUT="${2:-corpus}"
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

YELC="./target/release/yelc"
SMITH="./target/release/yel-smith"

# Rule fuzz-measure-clean-build: a stale binary reports the previous run's
# behaviour. Force a relink before measuring anything.
touch crates/yelc/src/main.rs
cargo build --release -p yelc -p yel-smith || exit 1

FREEZE_SHA="$(git rev-parse HEAD)"
# Provenance only depends on what the compiler was built from. Untracked plan
# docs or scratch files do not change the artifacts; modified compiler sources do.
COMPILER_DIRT="$(git status --porcelain -- crates/ Cargo.toml Cargo.lock)"
DIRTY=""
[ -n "$COMPILER_DIRT" ] && DIRTY=" (DIRTY COMPILER SOURCES — corpus provenance is NOT reproducible)"

rm -rf "$OUT"
mkdir -p "$OUT"/{src,wit,dot,wasm}

echo "freezing $SEEDS seeds from $FREEZE_SHA$DIRTY into $OUT/"

: > "$OUT/known-failures.txt"
gen_fail=0
wit_fail=0
dot_fail=0
wasm_fail=0
validate_fail=0

for s in $(seq 1 "$SEEDS"); do
  if ! "$SMITH" --seed "$s" > "$OUT/src/$s.yel" 2>/dev/null; then
    rm -f "$OUT/src/$s.yel"
    echo "$s|generate|yel-smith failed to generate" >> "$OUT/known-failures.txt"
    gen_fail=$((gen_fail+1))
    continue
  fi

  for fmt in wit dot; do
    err="$("$YELC" compile -o "$fmt" "$OUT/src/$s.yel" 2>&1 >"$OUT/$fmt/$s.$fmt")"
    if [ -n "$err" ] || [ ! -s "$OUT/$fmt/$s.$fmt" ]; then
      rm -f "$OUT/$fmt/$s.$fmt"
      echo "$s|$fmt|$(printf '%s' "$err" | head -1)" >> "$OUT/known-failures.txt"
      [ "$fmt" = wit ] && wit_fail=$((wit_fail+1)) || dot_fail=$((dot_fail+1))
    fi
  done

  err="$("$YELC" compile -o wasm "$OUT/src/$s.yel" 2>&1 >"$OUT/wasm/$s.wasm")"
  if [ -n "$err" ] || [ ! -s "$OUT/wasm/$s.wasm" ]; then
    rm -f "$OUT/wasm/$s.wasm"
    echo "$s|wasm|$(printf '%s' "$err" | head -1)" >> "$OUT/known-failures.txt"
    wasm_fail=$((wasm_fail+1))
  elif ! verr="$(wasm-tools validate "$OUT/wasm/$s.wasm" 2>&1)"; then
    echo "$s|validate|$(printf '%s' "$verr" | head -1)" >> "$OUT/known-failures.txt"
    validate_fail=$((validate_fail+1))
  fi
done

# Digest manifest — the durable, reviewable form of the WASM oracle. The .wasm
# blobs themselves are gitignored (**/*.wasm); the digests are what a stage's
# differential sweep compares against, and what survives the old tree's deletion.
( cd "$OUT" && find src wit dot wasm -type f | LC_ALL=C sort | xargs shasum -a 256 ) > "$OUT/SHA256SUMS"

{
  echo "freeze_sha=$FREEZE_SHA"
  echo "compiler_sources_clean=$([ -z "$COMPILER_DIRT" ] && echo yes || echo NO)"
  echo "seeds=$SEEDS"
  echo "generated_by=scripts/freeze-corpus.sh"
  echo "yelc_version=$("$YELC" --version 2>/dev/null || echo unknown)"
  echo "wasm_tools_version=$(wasm-tools --version 2>/dev/null || echo unknown)"
  echo "src_count=$(ls -1 "$OUT/src" | wc -l | tr -d ' ')"
  echo "wit_count=$(ls -1 "$OUT/wit" | wc -l | tr -d ' ')"
  echo "dot_count=$(ls -1 "$OUT/dot" | wc -l | tr -d ' ')"
  echo "wasm_count=$(ls -1 "$OUT/wasm" | wc -l | tr -d ' ')"
  echo "known_failures=$(wc -l < "$OUT/known-failures.txt" | tr -d ' ')"
  echo "fail_generate=$gen_fail"
  echo "fail_wit=$wit_fail"
  echo "fail_dot=$dot_fail"
  echo "fail_wasm=$wasm_fail"
  echo "fail_validate=$validate_fail"
} > "$OUT/MANIFEST"

cat "$OUT/MANIFEST"
