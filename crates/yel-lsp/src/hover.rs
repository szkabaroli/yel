//! Hover information provider for Yel DSL.

use tower_lsp::lsp_types::*;

use crate::ast_hover;

/// Get hover information - AST-based only.
pub fn get_hover_info_with_ast(content: &str, offset: usize, _word: &str) -> Option<Hover> {
    ast_hover::get_ast_hover(content, offset)
}
