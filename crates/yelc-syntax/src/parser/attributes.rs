//! `@name` / `@name(key = value, …)` before a declaration.
//!
//! # The `@children` collision, and why this is not a name lookahead
//!
//! `@` was already taken. `AT` is a token, it is a member of [`NODE_FIRST`], and
//! `@children` is a **UI node** — two of the 23 diagnostic fixtures are about
//! it. In a component body the two constructs occupy the *same* position:
//! `component A { @children }` is a node member, and `component A { @unsafe x:
//! s32; }` is an attributed property member. "Decide by which parse function you
//! are in" therefore does not settle it on its own, because it is one parse
//! function.
//!
//! What settles it is a single, **total** rule stated once, here:
//!
//! > An `AT` whose next *raw* token is `CHILDREN_KW` is the children marker.
//! > Every other `AT` in a declaration position opens an attribute list.
//!
//! Three properties make that safe where a lookahead list would not be:
//!
//! * It reads one token **kind** the lexer already assigned, not a table of
//!   attribute spellings. Adding an attribute never touches it, so it cannot
//!   drift out of sync with the set of attributes the way `parse_type`'s
//!   `(`-only lookahead drifted out of sync with `func<T>` — a *silent misparse*
//!   that S1 could not see (`plans/rewrite/seam-changes.md`, 2026-07-29).
//! * It is total. There is no third outcome: `@` is either the children marker
//!   or an attribute, so nothing falls through to a different production and
//!   quietly means something else.
//! * The predicate is [`Parser::at_children_marker`], the crate's existing and
//!   only definition of the marker — including its no-trivia rule (`@ children`,
//!   `@/*c*/children` and `@\nchildren` are *not* markers, because
//!   `children_node` is one atomic string literal in the frozen grammar).
//!
//! The consequence, stated so it is a decision and not an accident:
//! **`@children` cannot be spelled as an attribute.** It is a node wherever it
//! is legal, in every position, exactly as before.
//!
//! # Unknown attributes are reported
//!
//! An attribute that is silently dropped is the `_ => {}` shape
//! ([`findings.md` F20]) with a friendlier face — the user writes `@unsfae`,
//! gets working code, and the gate they thought they applied is absent. So
//! [`KNOWN_ATTRIBUTES`] is checked here and an unrecognised name is an error
//! **plus** a recovery mark, which is what makes invariant S5 hold for it.
//!
//! What is *not* checked here is whether a given attribute accepts a given
//! argument: that is a question about the language, and the parser accepts the
//! grammar. See `lib.rs`'s closing note.
//!
//! [`findings.md` F20]: ../../../../plans/rewrite/findings.md
//! [`NODE_FIRST`]: crate::token::NODE_FIRST

use super::{Parser, TrailingSep};
use crate::ast;
use crate::token::{ITEM_RECOVERY, TokenKind::*, TokenSet};
use yelc_base::Span;

// `ITEM_RECOVERY` is read only by `ATTRIBUTE_ARG_RECOVERY` below — the argument
// list stops at the next top-level declaration rather than swallowing it, the
// same way `PARAM_RECOVERY` does. No `TokenSet` is *modified* by this module.

/// Every attribute the language defines, in the order they were decided.
///
/// **This is a registry, not a hint.** A name that is not here is reported, and
/// the check is the whole reason unknown attributes cannot be dropped silently.
///
/// Both entries come from `plans/rewrite/scope.md` § *attributes on items, and
/// `unsafe`* (2026-07-29): `@unsafe` gates the primitive/cast machinery the
/// uniform-ref stdlib needs, and `@primitive` is what lets a primitive be a
/// bodyless declaration carrying an attribute rather than a top-level item form
/// of its own.
///
/// Nothing else is listed. The WIT feature gates (`@since`, `@unstable`,
/// `@deprecated`) motivated the **argument form** — see [`ast::AttributeArg`] —
/// but no decision has landed that yel *has* them, and a registry entry with no
/// decision behind it is a shape-only port (anti-spec A9).
pub(crate) const KNOWN_ATTRIBUTES: &[&str] = &[
    "unsafe",
    "primitive",
    // The WIT-boundary trio the desugar artifact writes and
    // `plans/desugar/README.md` §1 measured as absent from this list —
    // `@interface(name = …)` on a module, `@import`/`@export` on functions.
    // Recognised here so the artifact's own vocabulary is not a typo;
    // what each *means* is stage 6's (`plans/modules.md` §6).
    "interface",
    "import",
    "export",
];

/// Recovery set for an attribute argument list.
const ATTRIBUTE_ARG_RECOVERY: TokenSet =
    TokenSet::new(&[R_PAREN, SEMICOLON, R_BRACE]).union(ITEM_RECOVERY);

impl<'a> Parser<'a> {
    /// `attribute_list = attribute+`, or `None` when no attribute is written.
    ///
    /// Called only from declaration positions: `parse_item`,
    /// `parse_component_member` and `parse_global_member`.
    pub(super) fn parse_attribute_list(&mut self) -> Option<ast::AttributeList> {
        if !self.at_attribute() {
            return None;
        }

        self.start_node();
        let mut attributes = Vec::new();
        while self.at_attribute() {
            let before = self.position();
            attributes.push(self.parse_attribute());
            assert!(self.position() > before, "attribute consumed nothing");
        }
        let span = self.finish_node(ATTRIBUTE_LIST);

        Some(ast::AttributeList {
            id: self.new_node_id(),
            span,
            attributes,
        })
    }

    /// An `@` that is not the `@children` marker. See the module docs.
    fn at_attribute(&self) -> bool {
        self.is(AT) && !self.at_children_marker()
    }

    /// `attribute = "@" ~ identifier ~ ("(" ~ attribute_args? ~ ")")?`
    ///
    /// `Missing` when the `@` had no name after it at all. That is the one
    /// recovery position the enclosing list can express, and it is reachable:
    /// `component A { @ }` produces it.
    fn parse_attribute(&mut self) -> ast::Recovered<ast::Attribute> {
        self.start_node();
        self.assert(AT);

        if !self.is_name() {
            self.error_here(format!(
                "expected an attribute name after `@`, found `{}`",
                self.current().spelling()
            ));
            let span = self.finish_node(ERROR);
            return ast::Recovered::Missing {
                id: self.new_node_id(),
                span,
            };
        }

        let name_span = self.current_span();
        let name_text = self.current_text();
        let known = KNOWN_ATTRIBUTES.contains(&name_text);
        if !known {
            let message = format!("unknown attribute `@{name_text}`");
            self.error_at(name_span, message);
            // A diagnostic without a node is half of invariant S5. The name is
            // present and well-formed, so there is no hole in `Attribute` to
            // put this in — it is a recovery *position* with no slot, which is
            // exactly what `RecoveryMark` is for (see `parser.rs` module docs).
            self.record_recovery_mark(name_span);
        }
        let name = self.expect_name();

        let args = if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ATTRIBUTE_ARG_RECOVERY,
                ATTRIBUTE_ARG_LIST,
                TrailingSep::Forbidden,
                |p| p.parse_attribute_arg().map(ast::Recovered::Present),
            )
        } else {
            Vec::new()
        };

        let span = self.finish_node(ATTRIBUTE);
        ast::Recovered::Present(ast::Attribute {
            id: self.new_node_id(),
            span,
            name,
            args,
        })
    }

    /// `attribute_arg = identifier ~ "=" ~ expr` — named, never positional.
    fn parse_attribute_arg(&mut self) -> Option<ast::AttributeArg> {
        if !self.is_name() {
            return None;
        }
        self.start_node();
        let name = self.expect_name();
        self.expect(EQ);
        let value = self.parse_expr();
        let span = self.finish_node(ATTRIBUTE_ARG);
        Some(ast::AttributeArg {
            id: self.new_node_id(),
            span,
            name,
            value,
        })
    }

    /// An attribute list with no declaration after it.
    ///
    /// The typed tree has no slot for attributes on a recovery node, so the
    /// `Error` this produces **spans the attributes as well** — which is what
    /// `ItemKind::Error`'s "carries the span it consumed so the text is still
    /// attributable" means. Reporting and leaving the attribute text uncovered
    /// would be a silently dropped subtree.
    ///
    /// # It consumes nothing
    ///
    /// Unlike every other recovery path here, this one does not advance and does
    /// not resynchronise. It does not have to: the attribute list itself already
    /// moved the cursor, so every enclosing `assert!(position() > before)` is
    /// satisfied without eating a token. Eating one would be actively wrong —
    /// `component A { @unsafe VStack { "x" } }` would lose the `VStack`, and the
    /// element that *is* well-formed would be reported twice more as its `{` and
    /// its `"x"` were re-read as members.
    pub(super) fn orphaned_attributes<R: ast::Recovery>(
        &mut self,
        attributes: &ast::AttributeList,
        message: &str,
    ) -> R {
        self.error_here(message);
        let tail = self.zero_width_error_node();
        let span = Span::new(
            attributes.span.source,
            attributes.span.start,
            tail.end.max(attributes.span.end),
        );
        R::recovery(self.new_node_id(), span)
    }
}
