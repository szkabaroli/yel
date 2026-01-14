#!/bin/bash
set -e

# Source cargo
. "$HOME/.cargo/env"

# Build yelc compiler
cargo build -p yelc --release --target wasm32-wasip2

# Transpile WASM to JS
cd yel-viewer
pnpm exec jco transpile \
    ../target/wasm32-wasip2/release/yelc.wasm \
    --name yelc \
    --out-dir src/lib/compiler \
    --minify \
    --valid-lifting-optimization \
    --no-nodejs-compat

# Build Vite app
pnpm build
