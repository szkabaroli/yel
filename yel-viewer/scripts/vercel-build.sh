#!/bin/bash
set -e

# Source cargo env
. $HOME/.cargo/env

# Build yelc compiler
cd ..
cargo build -p yelc --release --target wasm32-wasip2

# Transpile to JS
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
