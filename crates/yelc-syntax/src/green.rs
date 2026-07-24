//! The lossless green tree.
//!
//! # Contract
//!
//! - **Length-based, no absolute offsets.** A node knows its width, not its
//!   position. That is what allows a subtree to be reused after an edit later.
//! - **Trivia is IN the tree.** Whitespace and comments are `GreenToken`s like
//!   any other. A green tree that drops trivia cannot satisfy invariant S1 and
//!   is a reference-fidelity failure, not a port.
//! - **`Marker` enables retroactive node starts** — needed for left-associative
//!   binary expressions, where the node kind is only known after the left
//!   operand has been parsed.

use crate::token::TokenKind;
use std::sync::Arc;

pub type GreenNode = Arc<GreenNodeData>;
pub type GreenToken = Arc<GreenTokenData>;

#[derive(Clone, Debug)]
pub enum GreenElement {
    Node(GreenNode),
    Token(GreenToken),
}

#[derive(Clone, Debug)]
pub struct GreenNodeData {
    pub kind: TokenKind,
    pub len: u32,
    pub children: Vec<GreenElement>,
}

impl GreenNodeData {
    /// Byte width of this subtree, trivia included. Invariant S2.
    pub fn len(&self) -> u32 {
        self.len
    }

    /// Reconstruct the exact source text of this subtree. Invariant S1:
    /// for the root, this equals the input byte-for-byte.
    pub fn text(&self) -> String {
        todo!("stage 1: concatenate children depth-first")
    }
}

#[derive(Clone, Debug)]
pub struct GreenTokenData {
    pub kind: TokenKind,
    pub len: u32,
    pub value: String,
}

/// Builds a green tree as the parser walks. `start_node`/`finish_node` nest;
/// `create_marker`/`finish_node_starting_at` handle retroactive starts.
pub struct GreenTreeBuilder {
    _private: (),
}

#[derive(Clone)]
pub struct Marker {
    _private: (),
}

impl GreenTreeBuilder {
    pub fn new() -> GreenTreeBuilder {
        todo!("stage 1")
    }
    pub fn start_node(&mut self) {
        todo!("stage 1")
    }
    pub fn finish_node(&mut self, _kind: TokenKind) -> GreenNode {
        todo!("stage 1")
    }
    pub fn finish_node_starting_at(&mut self, _kind: TokenKind, _marker: Marker) -> GreenNode {
        todo!("stage 1")
    }
    pub fn abandon_node(&mut self) {
        todo!("stage 1")
    }
    pub fn create_marker(&mut self) -> Marker {
        todo!("stage 1")
    }
    pub fn token(&mut self, _kind: TokenKind, _value: &str) {
        todo!("stage 1")
    }
    pub fn create_tree(self) -> GreenNode {
        todo!("stage 1")
    }
}
