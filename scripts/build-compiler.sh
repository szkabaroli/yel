#!/bin/bash
set -e

# Build the Yel compiler for WebAssembly
# This script compiles yelc to WASM and transpiles it to JS for the web viewer

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$ROOT_DIR/yel-viewer/src/lib/compiler"

echo "🔨 Building yelc in release mode for wasm32-wasip2..."
cd "$ROOT_DIR"
cargo build -p yelc --release --target wasm32-wasip2

WASM_PATH="$ROOT_DIR/target/wasm32-wasip2/release/yelc.wasm"

if [ ! -f "$WASM_PATH" ]; then
    echo "❌ Error: WASM file not found at $WASM_PATH"
    exit 1
fi

echo "📦 WASM size: $(du -h "$WASM_PATH" | cut -f1)"

# Optional: Run wasm-opt for additional size reduction
if command -v wasm-opt &> /dev/null; then
    echo "🔧 Running wasm-opt for additional optimization..."
    wasm-opt -Oz "$WASM_PATH" -o "$WASM_PATH.opt"
    mv "$WASM_PATH.opt" "$WASM_PATH"
    echo "📦 Optimized WASM size: $(du -h "$WASM_PATH" | cut -f1)"
fi

echo "🔄 Transpiling WASM component to JS with jco..."
cd "$ROOT_DIR/yel-viewer"

# Clean previous output
rm -f "$OUTPUT_DIR/yelc.js" "$OUTPUT_DIR/yelc.d.ts" "$OUTPUT_DIR"/*.wasm

# Transpile with jco
npx jco transpile "$WASM_PATH" \
    --name yelc \
    --out-dir "$OUTPUT_DIR" \
    --minify \
    --valid-lifting-optimization \
    --no-nodejs-compat

echo ""
echo "✅ Build complete! Output files:"
ls -lh "$OUTPUT_DIR"/yelc* 2>/dev/null || true
echo ""
echo "📍 Output directory: $OUTPUT_DIR"
