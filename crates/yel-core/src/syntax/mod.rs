//! Syntax module - Lexing, parsing, and AST definitions.

pub mod ast;
pub mod parser;

pub use ast::*;
pub use parser::{parse, ParseError};
