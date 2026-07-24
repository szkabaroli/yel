//! Hand-written lexer.
//!
//! # Contract
//!
//! - Returns **parallel arrays** of kinds and byte widths, not a `Vec<Token>`.
//! - **No absolute offsets in tokens.** Widths only; the parser accumulates
//!   `offset`. This is what keeps token data position-independent.
//! - Trivia (whitespace, `//` line comments, `/* */` block comments) are real
//!   tokens, emitted like any other.
//! - Errors go to `Diagnostics` and lexing **continues**: an unknown character
//!   becomes an `UNKNOWN` token, never a bail.

use crate::token::TokenKind;
use yelc_base::{Diagnostics, SourceId};

pub struct LexerResult {
    pub tokens: Vec<TokenKind>,
    /// `widths[i]` is the byte width of `tokens[i]`. Sums to `content.len()`.
    pub widths: Vec<u32>,
}

pub fn lex(_source: SourceId, _content: &str, _diags: &mut Diagnostics) -> LexerResult {
    todo!("stage 1: implement the lexer")
}
