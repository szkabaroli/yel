#!/usr/bin/env bash
# Collect line + region coverage for the yel-wasm-codegen crate.
#
# First-time setup:
#   cargo install cargo-llvm-cov
#   rustup component add llvm-tools-preview
#
# Usage:
#   scripts/coverage.sh                # summary on stdout
#   scripts/coverage.sh --html          # writes HTML report to target/llvm-cov/html/
#   scripts/coverage.sh --lcov <path>   # writes lcov for CI upload
#
# Tip: running with `--all-features` if/when we add gated modules.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

if ! command -v cargo-llvm-cov >/dev/null 2>&1; then
    echo "error: cargo-llvm-cov is not installed." >&2
    echo "       install with: cargo install cargo-llvm-cov" >&2
    exit 1
fi

cd "$ROOT_DIR"

MODE="summary"
LCOV_OUT=""
case "${1:-}" in
    --html)
        MODE="html"
        ;;
    --lcov)
        MODE="lcov"
        LCOV_OUT="${2:-target/llvm-cov/lcov.info}"
        mkdir -p "$(dirname "$LCOV_OUT")"
        ;;
    --help|-h)
        sed -n '2,14p' "$0"
        exit 0
        ;;
esac

# Scope to just yel-wasm-codegen — that's the crate the test suite
# targets. Workspace-wide coverage would sweep in yel-core, yel-lsp,
# etc. which have their own test stories.
COMMON_ARGS=(
    --package yel-wasm-codegen
    --tests
)

case "$MODE" in
    summary)
        cargo llvm-cov "${COMMON_ARGS[@]}" --summary-only
        ;;
    html)
        cargo llvm-cov "${COMMON_ARGS[@]}" --html
        echo ""
        echo "HTML report: target/llvm-cov/html/index.html"
        ;;
    lcov)
        cargo llvm-cov "${COMMON_ARGS[@]}" --lcov --output-path "$LCOV_OUT"
        echo ""
        echo "lcov written to: $LCOV_OUT"
        ;;
esac
