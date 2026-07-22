//! Operator semantics — shared across IR levels.
//!
//! `BinOp` and `UnaryOp` are pure operator identities (Add, Lt, Not,
//! …) with no dependency on any IR layer. They're used by HIR, THIR,
//! and LIR alike, plus by foreign frontends (the flow graph language)
//! that produce LIR directly without going through HIR/THIR.
//!
//! Living at the crate root (rather than inside `hir::expr`, where
//! they used to be) keeps LIR — which targets multiple frontends —
//! from depending on HIR. `hir::expr` re-exports both for backward
//! compatibility; existing call sites that read `crate::hir::expr::BinOp`
//! keep compiling.

use serde::{Deserialize, Serialize};

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
}

impl BinOp {
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "+" => Some(BinOp::Add),
            "-" => Some(BinOp::Sub),
            "*" => Some(BinOp::Mul),
            "/" => Some(BinOp::Div),
            "%" => Some(BinOp::Mod),
            "==" => Some(BinOp::Eq),
            "!=" => Some(BinOp::Ne),
            "<" => Some(BinOp::Lt),
            "<=" => Some(BinOp::Le),
            ">" => Some(BinOp::Gt),
            ">=" => Some(BinOp::Ge),
            "&&" => Some(BinOp::And),
            "||" => Some(BinOp::Or),
            "&" => Some(BinOp::BitAnd),
            "|" => Some(BinOp::BitOr),
            "^" => Some(BinOp::BitXor),
            _ => None,
        }
    }

    /// The source-level operator symbol (inverse of [`BinOp::parse`]). Used in
    /// diagnostics so users see `==` rather than the `Eq` variant name.
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "-" => Some(UnaryOp::Neg),
            "!" => Some(UnaryOp::Not),
            _ => None,
        }
    }

    /// The source-level operator symbol (inverse of [`UnaryOp::parse`]).
    pub fn symbol(self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        }
    }
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}
