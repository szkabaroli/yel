//! Yel language server library.
//!
//! Shared by the `yel-lsp` binary and suitable for reuse from WASM embeddings (e.g. CodeMirror + LSP in the browser).

pub mod ast_hover;
pub mod builtins_catalog;
pub mod completions;
pub mod diagnostics;
pub mod document;
pub mod hover;
pub mod semantic_tokens;
pub mod server;

pub use diagnostics::{convert_compile_error, convert_yel_diagnostic};
