#!/bin/bash
set -e

# Install Rust (minimal profile for speed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable --profile minimal
. "$HOME/.cargo/env"
rustup target add wasm32-wasip2

# Enable corepack for pnpm
corepack enable
corepack prepare pnpm@latest --activate

# Install Node dependencies
cd yel-viewer
pnpm install
