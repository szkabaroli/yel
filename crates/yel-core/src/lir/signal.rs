//! LIR reactive signals and effects.

use serde::{Deserialize, Serialize};

use crate::ids::{DefId, NodeId};
use crate::types::Ty;

use super::expr::LirExpr;

/// A reactive signal (property with change tracking).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LirSignal {
    /// DefId of the property.
    pub def_id: DefId,
    /// Signal type.
    pub ty: Ty,
    /// Default value expression.
    pub default: Option<LirExpr>,
}

/// A reactive effect (binding that updates DOM on signal change).
#[derive(Debug, Clone)]
pub struct LirEffect {
    /// Unique effect ID.
    pub id: u32,
    /// Dependencies: signals that trigger this effect.
    pub dependencies: Vec<DefId>,
    /// Target node to update.
    pub target_node: NodeId,
    /// What kind of update to perform.
    pub update_kind: UpdateKind,
    /// Expression to evaluate.
    pub expr: LirExpr,
}

/// Kind of DOM update for an effect.
#[derive(Debug, Clone)]
pub enum UpdateKind {
    /// Update a property/attribute.
    Property(String),
    /// Update text content.
    TextContent,
    /// Update class list.
    Class(String),
    /// Update style property.
    Style(String),
    /// Re-evaluate the source expression and write the result to a signal.
    /// Emitted for derived signals like `doubled: s32 = count * 2` — when
    /// any dep changes, rerun `count * 2` and store into `doubled`'s slot,
    /// which in turn triggers any effects observing `doubled`.
    DerivedSignal(DefId),
}
