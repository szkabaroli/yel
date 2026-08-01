# Yel for Zed

Zed extension wiring `.yel` files to [`yelc-lsp`](../../crates/yelc-lsp), the
language server built on the rewrite frontend. The extension is a launcher
only — diagnostics, document symbols and hover come from the server.

## Install (dev extension)

1. Build the server:

   ```sh
   cargo build -p yelc-lsp --release
   ```

2. In Zed: `zed: install dev extension` (command palette) and pick this
   directory (`editors/zed`). Zed compiles the extension to WASM itself
   (target `wasm32-wasip2`); with a rustup toolchain Zed installs that target
   automatically.

3. Tell the extension where the server is — either put `yelc-lsp` on PATH, or
   point Zed at the build output in `settings.json`:

   ```json
   {
     "lsp": {
       "yelc-lsp": {
         "binary": { "path": "/absolute/path/to/yel/target/release/yelc-lsp" }
       }
     }
   }
   ```

## Syntax highlighting

Comes from [`editors/tree-sitter-yel`](../tree-sitter-yel), a deliberately
flat highlighting-grade grammar (see its `grammar.js` header for why it is
not a third yel parser), with the queries in `languages/yel/highlights.scm`.
Verified to parse the whole 2000-file corpus plus stdlib and examples with
zero ERROR nodes.

The grammar is referenced from `extension.toml` by a `file://` URL and a
**commit SHA**, so the edit loop is: change the grammar → `npx -y
tree-sitter-cli@0.25.6 generate --abi=14` → commit in `editors/tree-sitter-yel`
→ paste the new SHA into `extension.toml`'s `rev` → reinstall the dev
extension. Query-only changes (`highlights.scm`) skip the commit/SHA steps —
just reinstall. If the grammar gets its own GitHub repository later, swap the
`file://` URL for it.
