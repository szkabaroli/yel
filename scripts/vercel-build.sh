#!/bin/bash
set -e

# Source cargo
. "$HOME/.cargo/env"

# Build yelc compiler LIBRARY (not binary - library exports the compiler interface)
cargo build --lib -p yelc --release --target wasm32-wasip2

# Transpile WASM to JS
cd yel-viewer
npx jco transpile \
    ../target/wasm32-wasip2/release/yelc.wasm \
    --name yelc \
    --out-dir src/lib/compiler \
    --optimize \
    --minify \
    --valid-lifting-optimization \
    --no-nodejs-compat

# Build Vite app
npm run build
