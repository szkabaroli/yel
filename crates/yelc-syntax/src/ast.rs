//! Typed AST — the view stage 2 consumes.
//!
//! # Contract
//!
//! - Every node carries `NodeId` + `Span` (invariant S3).
//! - Names are interned `Name`, never `String` (invariant S4).
//! - `Error` variants exist at every recovery point (invariant S5).
//!
//! The node *set* is stage 1's to design against the frozen grammar
//! (`yel-core/src/syntax/grammar.pest`) — AST node shapes are explicitly free in
//! `plans/rewrite/scope.md`. Only `File` and the invariants above are the seam.
//!
//! `visit.rs` carries the walk/visit split: a `Visitor` trait whose `visit_*`
//! methods default to free `walk_*` functions. `walk_*` is **exhaustive with no
//! `_` arm**, so a new node variant is a compile error at the one place that
//! must learn the new shape (anti-spec A3).

use crate::{green::GreenNode, NodeId};
use yelc_base::{SourceId, Span};

/// Root of one parsed file.
pub struct File {
    pub id: NodeId,
    pub source: SourceId,
    pub span: Span,
    pub green: GreenNode,
    pub items: Vec<Item>,
}

/// Top-level declarations, per the frozen grammar's `top_level_item`:
/// records, enums, variants, elements, imported components, globals,
/// components — plus the package declaration and a recovery variant.
pub enum Item {
    /// Recovery node: the parser could not match a top-level item here.
    /// Carries the span it consumed so the text is still attributable.
    Error { id: NodeId, span: Span },
    // stage 1: the real item variants
}
