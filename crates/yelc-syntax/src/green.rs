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
//!
//! # Nothing here recurses over the tree
//!
//! Green depth is **not** bounded by `MAX_NESTING_DEPTH`. That constant bounds
//! recursion inside `parse_*`, and the two productions that build the deepest
//! trees — `parse_binary` and `parse_postfix` — are *loops*: `a.b.b.b…` nests one
//! `MEMBER_EXPR` per link while the parser's depth counter reads 2. A 6 KB file
//! of valid, diagnostic-free source therefore produces a ~13,000-deep green tree.
//!
//! So [`GreenNodeData::text`] — which is the invariant-S1 check itself — and the
//! `Drop` glue both walk an explicit worklist. Measured before the change, in a
//! debug `cargo test` thread: `text()` aborted at n≈12,983 and `drop` at
//! n≈4,979 on `component A { x: s32 = a.b.b…; }`. See anti-spec A11.

use crate::token::TokenKind;
use std::sync::Arc;

pub type GreenNode = Arc<GreenNodeData>;
pub type GreenToken = Arc<GreenTokenData>;

#[derive(Clone, Debug)]
pub enum GreenElement {
    Node(GreenNode),
    Token(GreenToken),
}

impl GreenElement {
    pub fn kind(&self) -> TokenKind {
        match self {
            GreenElement::Node(node) => node.kind,
            GreenElement::Token(token) => token.kind,
        }
    }

    pub fn to_node(&self) -> Option<GreenNode> {
        match self {
            GreenElement::Node(node) => Some(node.clone()),
            GreenElement::Token(..) => None,
        }
    }
}

impl From<GreenNode> for GreenElement {
    fn from(value: GreenNode) -> Self {
        GreenElement::Node(value)
    }
}

impl From<GreenToken> for GreenElement {
    fn from(value: GreenToken) -> Self {
        GreenElement::Token(value)
    }
}

#[derive(Clone, Debug)]
pub struct GreenNodeData {
    pub kind: TokenKind,
    pub len: u32,
    pub children: Vec<GreenElement>,
}

/// Iterative teardown.
///
/// The derived glue drops `children` recursively, one frame per level, which
/// `abort()`s the process on a green tree deeper than a few thousand — and a
/// flat `a.b.b.b…` chain builds exactly that from valid input. `Arc::into_inner`
/// is what keeps this correct under sharing: it hands back the payload **only**
/// when this is the last reference, so a subtree someone else still holds is
/// left alone.
impl Drop for GreenNodeData {
    fn drop(&mut self) {
        let mut worklist = std::mem::take(&mut self.children);
        while let Some(element) = worklist.pop() {
            let GreenElement::Node(node) = element else {
                continue;
            };
            if let Some(mut data) = Arc::into_inner(node) {
                worklist.append(&mut data.children);
                // `data` now owns nothing; its own `drop` walks an empty vec.
            }
        }
    }
}

impl GreenNodeData {
    pub fn new(kind: TokenKind, len: u32, children: Vec<GreenElement>) -> GreenNodeData {
        GreenNodeData {
            kind,
            len,
            children,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    /// Byte width of this subtree, trivia included. Invariant S2.
    ///
    /// No companion `is_empty`: this is a byte width, not a container length,
    /// and `is_empty()` on a green node would read as "has no children" — a
    /// different question with a different answer for the zero-width `ERROR`
    /// nodes the recovery paths emit.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }

    /// Depth of the deepest path through this subtree, counting this node as 1.
    ///
    /// This is the number that actually bounds every consumer — a walk, a
    /// `Drop` chain, a serializer — and it is **not** what `MAX_NESTING_DEPTH`
    /// bounds. `parse_binary`/`parse_postfix` are loops, so `a.b.b.b…` nests one
    /// node per link while the parser's own counter reads 2 (anti-spec A11).
    /// Worklist, for the same reason [`GreenNodeData::text`] is one.
    pub fn max_depth(&self) -> usize {
        let mut deepest = 1;
        let mut worklist: Vec<(&GreenElement, usize)> =
            self.children.iter().map(|child| (child, 2)).collect();
        while let Some((element, depth)) = worklist.pop() {
            deepest = deepest.max(depth);
            if let GreenElement::Node(node) = element {
                worklist.extend(node.children.iter().map(|child| (child, depth + 1)));
            }
        }
        deepest
    }

    /// Reconstruct the exact source text of this subtree. Invariant S1:
    /// for the root, this equals the input byte-for-byte.
    ///
    /// Worklist, not recursion: this is the S1 check, and it must not be the
    /// thing that dies on the deep trees S1 is supposed to certify.
    pub fn text(&self) -> String {
        let mut out = String::with_capacity(self.len as usize);
        let mut worklist: Vec<&GreenElement> = self.children.iter().rev().collect();
        while let Some(element) = worklist.pop() {
            match element {
                GreenElement::Node(node) => worklist.extend(node.children.iter().rev()),
                GreenElement::Token(token) => out.push_str(&token.value),
            }
        }
        out
    }
}

/// A leaf. Its byte width is `value.len()` — there is no separate `len` field,
/// because a second copy of a number that is already there is a second thing
/// that can be wrong (anti-spec A9). The predecessor wrote one at construction
/// and never read it.
#[derive(Clone, Debug)]
pub struct GreenTokenData {
    pub kind: TokenKind,
    pub value: String,
}

impl GreenTokenData {
    pub fn new(kind: TokenKind, value: String) -> GreenTokenData {
        GreenTokenData { kind, value }
    }
}

/// Builds a green tree as the parser walks. `start_node`/`finish_node` nest;
/// `create_marker`/`finish_node_starting_at` handle retroactive starts.
pub struct GreenTreeBuilder {
    /// `(index into `children`, byte offset)` for each open node.
    nodes: Vec<(usize, u32)>,
    children: Vec<GreenElement>,
    offset: u32,
}

/// A remembered position in the builder, so a node can be *started* after its
/// first children have already been pushed.
#[derive(Clone)]
pub struct Marker {
    children: usize,
    offset: u32,
}

/// The builder's **whole** state at one instant, so a speculative parse can be
/// undone exactly.
///
/// [`Marker`] captures two of the three fields; the open-node stack is the
/// third, and without it a rollback would leave nodes that the abandoned
/// attempt started still open. Undoing is three `truncate`s and an assignment —
/// the tree is never rebuilt, because `children` is one flat vec and everything
/// an attempt appended sits at its tail.
#[derive(Clone, Copy)]
pub struct Checkpoint {
    children: usize,
    nodes: usize,
    offset: u32,
}

impl Checkpoint {
    /// Number of appended elements at the instant this was taken. Exposed so a
    /// rollback test can assert the count as well as the text.
    pub fn children_len(&self) -> usize {
        self.children
    }
}

impl GreenTreeBuilder {
    pub fn new() -> GreenTreeBuilder {
        GreenTreeBuilder {
            nodes: Vec::new(),
            children: Vec::new(),
            offset: 0,
        }
    }

    pub fn start_node(&mut self) {
        self.nodes.push((self.children.len(), self.offset));
    }

    pub fn finish_node(&mut self, kind: TokenKind) -> GreenNode {
        assert!(kind > TokenKind::EOF, "{kind:?} is not a node kind");
        let (children_start, start) = self.nodes.pop().expect("missing node start");
        self.finish_node_common(kind, children_start, start)
    }

    pub fn finish_node_starting_at(&mut self, kind: TokenKind, marker: Marker) -> GreenNode {
        assert!(kind > TokenKind::EOF, "{kind:?} is not a node kind");
        self.finish_node_common(kind, marker.children, marker.offset)
    }

    fn finish_node_common(
        &mut self,
        kind: TokenKind,
        children_start: usize,
        start: u32,
    ) -> GreenNode {
        let children = self.children.drain(children_start..).collect::<Vec<_>>();
        let len = self.offset - start;
        let node = Arc::new(GreenNodeData::new(kind, len, children));
        self.children.push(GreenElement::Node(node.clone()));
        node
    }

    pub fn abandon_node(&mut self) {
        self.nodes.pop().expect("missing node start");
    }

    /// Remember everything, so [`GreenTreeBuilder::rewind`] can put it back.
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            children: self.children.len(),
            nodes: self.nodes.len(),
            offset: self.offset,
        }
    }

    /// Discard everything appended since `checkpoint`.
    ///
    /// Truncating the flat `children` vec is what drops the abandoned subtrees:
    /// a finished node owns its children (they were drained out of this vec),
    /// so dropping the one element that node was pushed as drops all of them.
    pub fn rewind(&mut self, checkpoint: Checkpoint) {
        assert!(
            self.children.len() >= checkpoint.children && self.nodes.len() >= checkpoint.nodes,
            "a speculative attempt closed a node it did not open"
        );
        self.children.truncate(checkpoint.children);
        self.nodes.truncate(checkpoint.nodes);
        self.offset = checkpoint.offset;
    }

    /// Text of everything appended so far, in order.
    ///
    /// For asserting that a rollback put the tree back *byte-for-byte* — the
    /// counts matching is not the same claim, and S1 is the invariant a
    /// near-miss would corrupt first.
    pub fn text_so_far(&self) -> String {
        let mut out = String::new();
        let mut worklist: Vec<&GreenElement> = self.children.iter().rev().collect();
        while let Some(element) = worklist.pop() {
            match element {
                GreenElement::Node(node) => worklist.extend(node.children.iter().rev()),
                GreenElement::Token(token) => out.push_str(&token.value),
            }
        }
        out
    }

    pub fn create_marker(&mut self) -> Marker {
        Marker {
            children: self.children.len(),
            offset: self.offset,
        }
    }

    pub fn token(&mut self, kind: TokenKind, value: &str) {
        assert!(kind < TokenKind::EOF, "{kind:?} is not a token kind");
        let len: u32 = value.len().try_into().expect("token width overflows u32");
        self.offset += len;
        self.children
            .push(Arc::new(GreenTokenData::new(kind, value.to_string())).into());
    }

    pub fn create_tree(self) -> GreenNode {
        assert!(self.nodes.is_empty(), "unfinished green nodes");
        assert_eq!(self.children.len(), 1, "green tree must have one root");
        let child = self.children.into_iter().next().expect("missing element");
        child.to_node().expect("root must be a node")
    }
}

impl Default for GreenTreeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind::*;

    #[test]
    fn round_trips_text_and_length() {
        let mut b = GreenTreeBuilder::new();
        b.start_node();
        b.token(IDENTIFIER, "abc");
        b.token(WHITESPACE, " ");
        b.start_node();
        b.token(INT_LITERAL, "42");
        b.finish_node(LITERAL_EXPR);
        let tree = b.finish_node(SOURCE_FILE);
        assert_eq!(tree.text(), "abc 42");
        assert_eq!(tree.len(), 6);
    }

    #[test]
    fn marker_starts_a_node_retroactively() {
        let mut b = GreenTreeBuilder::new();
        b.start_node();
        let m = b.create_marker();
        b.token(INT_LITERAL, "1");
        b.token(ADD, "+");
        b.token(INT_LITERAL, "2");
        b.finish_node_starting_at(BINARY_EXPR, m);
        let tree = b.finish_node(SOURCE_FILE);
        assert_eq!(tree.text(), "1+2");
        assert_eq!(tree.children().len(), 1);
        assert_eq!(tree.children()[0].kind(), BINARY_EXPR);
    }

    /// The invariant-S1 half of rollback: the bytes a rewound attempt pushed
    /// are gone, and the bytes before it are untouched.
    #[test]
    fn rewind_restores_the_builder_byte_for_byte() {
        let mut b = GreenTreeBuilder::new();
        b.start_node();
        b.token(IDENTIFIER, "keep");
        let checkpoint = b.checkpoint();

        // A whole speculative subtree: open nodes, finish some, leave one open.
        b.start_node();
        b.token(IDENTIFIER, "throw");
        b.start_node();
        b.token(INT_LITERAL, "42");
        b.finish_node(LITERAL_EXPR);
        b.finish_node(IF_NODE);
        b.start_node();
        b.token(WHITESPACE, "  ");

        b.rewind(checkpoint);
        b.token(IDENTIFIER, "-more");
        let tree = b.finish_node(SOURCE_FILE);
        assert_eq!(tree.text(), "keep-more");
        assert_eq!(tree.len(), 9);
        assert_eq!(tree.children().len(), 2);
    }

    #[test]
    fn rewind_to_the_very_start_leaves_an_empty_builder() {
        let mut b = GreenTreeBuilder::new();
        let checkpoint = b.checkpoint();
        b.start_node();
        b.token(IDENTIFIER, "x");
        b.finish_node(IDENT_EXPR);
        b.rewind(checkpoint);
        b.start_node();
        let tree = b.finish_node(SOURCE_FILE);
        assert_eq!(tree.text(), "");
        assert_eq!(tree.len(), 0);
        assert!(tree.children().is_empty());
    }

    #[test]
    #[should_panic(expected = "did not open")]
    fn rewind_rejects_an_unbalanced_attempt() {
        let mut b = GreenTreeBuilder::new();
        b.start_node();
        b.start_node();
        b.token(IDENTIFIER, "x");
        let checkpoint = b.checkpoint();
        b.finish_node(IDENT_EXPR);
        b.rewind(checkpoint);
    }

    #[test]
    fn abandon_node_leaves_children_in_place() {
        let mut b = GreenTreeBuilder::new();
        b.start_node();
        b.start_node();
        b.token(IDENTIFIER, "x");
        b.abandon_node();
        let tree = b.finish_node(SOURCE_FILE);
        assert_eq!(tree.text(), "x");
        assert_eq!(tree.children().len(), 1);
    }
}
