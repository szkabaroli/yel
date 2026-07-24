//! Recursive-descent parser over the token stream, emitting a green tree and a
//! typed AST simultaneously.
//!
//! # Contract
//!
//! The predicate layer over `current()` / `nth(i)`:
//! `is` · `is2` · `is_set` · `nth_is_set` · `eat` · `expect` · `assert`.
//!
//! `advance()` = `raw_advance()` + `skip_trivia()`. `raw_advance` is what pushes
//! into the green builder, so **trivia lands in the tree while the parser never
//! sees it**.
//!
//! `start_node`/`finish_node` compute AST spans that **exclude trailing
//! trivia** — the green node covers the trivia, the AST span does not.
//!
//! # Recovery
//!
//! `parse_list` is the generic recovery helper, carried over in shape:
//!
//! ```ignore
//! fn parse_list<F, R>(&mut self, start: TokenKind, sep: TokenKind, stop: TokenKind,
//!                     recovery_set: TokenSet, code: ErrorCode, node: TokenKind,
//!                     parse: F) -> Vec<R>
//! where F: FnMut(&mut Parser) -> Option<R>;
//! ```
//!
//! It **must** keep the no-progress guard — `assert!(token_idx > pos_before)` —
//! or a callback that consumes nothing loops forever on malformed input.
//!
//! Never bail on the first error: emit an `Error` AST node, report to
//! `Diagnostics`, and continue (invariant S5, keep-list §6).
