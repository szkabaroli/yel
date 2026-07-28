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
//! ```text
//! fn parse_list<F, R>(&mut self, start: TokenKind, sep: TokenKind, stop: TokenKind,
//!                     recovery_set: TokenSet, node: TokenKind,
//!                     trailing: TrailingSep, parse: F) -> Vec<R>
//! where R: ast::Recovery, F: FnMut(&mut Parser) -> Option<R>;
//! ```
//!
//! It **must** keep the no-progress guard — `assert!(position() > pos_before)` —
//! or a callback that consumes nothing loops forever on malformed input.
//!
//! The `R: ast::Recovery` bound is what makes it *impossible* to write a list
//! production whose failure path pushes nothing: the `None` arm reports **and**
//! pushes `R::recovery(..)`. An earlier revision reported and dropped, which is
//! how six recovery positions ended up with a diagnostic and no `Error` node.
//!
//! There is no `code: ErrorCode` parameter: all eight call sites passed
//! `SyntaxError`, and `error_here` is already the single E0060 idiom (anti-spec
//! B6).
//!
//! The one addition to ark's signature is `trailing`: the frozen grammar allows
//! a trailing separator in six of its lists (record fields, enum/variant cases,
//! list and tuple literals, record-literal fields) and forbids it in five
//! (function parameters, call arguments, type lists, closure parameters, result
//! types). Accepting a trailing comma everywhere would widen the language, which
//! `plans/rewrite/scope.md` forbids outright.
//!
//! Never bail on the first error: emit an `Error` AST node, report to
//! `Diagnostics`, and continue (invariant S5, keep-list §6).
//!
//! # Recovery marks
//!
//! A missing *token* — the `>` in `list<s32`, the `in` in `for x xs { … }` —
//! has no slot in the typed AST, which models nodes and not tokens. Neither does
//! an over-long `result<a, b, c>` argument list. Reporting such a position and
//! stopping there satisfies "a diagnostic" but not "an `Error` node", and
//! invariant S5 requires both. So `expect` records the position, and
//! `parse_file` hands the whole set to `ast::File::recovery_marks` sorted by
//! span — a side table, visited by `walk_file`.
//!
//! An earlier revision drained these into the nearest enclosing *list*, as a
//! real element. That corrupted arity — `func(a list<s32>, b: s32)` reported
//! three parameters for the two written, and `tuple<s32, string>` grew an error
//! element *between* its two types — at an index chosen by drain timing rather
//! than by source position. See [`ast::RecoveryMark`].
//!
//! # Recursion depth
//!
//! Invariant S6 says parsing always *returns*. A recursive-descent parser on
//! `((((…` does not: ~1500 nested parentheses `SIGABRT`ed a debug build, which
//! no `catch_unwind` and no accumulate-and-continue policy survives.
//! [`MAX_NESTING_DEPTH`] bounds it, and the five recursive entry points —
//! `parse_expr`, `parse_unary`, `parse_type`, `parse_ui_node`, `parse_stmt` —
//! return their type's `Recovery` node when the guard trips.
//!
//! It bounds **recursion in `parse_*`, not the depth of the tree that builds**.
//! `parse_binary` and `parse_postfix` are loops that enter and leave nesting per
//! operand, so `a.b.b.b…` nests one node per link while the counter reads 2
//! (anti-spec A11). Nothing bounds that, and nothing should: the frozen parser
//! accepts such chains up to its own stack limit. What had to change instead is
//! every consumer — `green.text()`, both `Drop` impls and `ast::visit::walk_expr`
//! — none of which recurses over an unbounded structure any more.
//!
//! # Layout
//!
//! This file holds the machinery — cursor, predicates, spans, recovery. The
//! productions live in sibling modules so no single unit becomes the god pass
//! anti-spec A2 warns about:
//!
//! | Module | Productions |
//! |---|---|
//! | [`items`] | file, package, record/enum/variant/element, extern component, global, component, members |
//! | [`types`] | `type_annotation` and everything under it |
//! | [`nodes`] | the UI tree: elements, text, `if`, `for`, `@children` |
//! | [`stmts`] | statement blocks: `let`, `if`, assignment, expression statements |
//! | [`exprs`] | closures, literals, and the expression grammar |

mod exprs;
mod items;
mod nodes;
mod stmts;
mod types;

use crate::green::{Checkpoint as GreenCheckpoint, GreenTreeBuilder, Marker};
use rustc_hash::FxHashSet;
use crate::lexer::lex;
use crate::token::{TokenKind, TokenKind::*, TokenSet};
use crate::{NodeId, ParsedFile, ast};
use yelc_base::{Diagnostics, ErrorCode, Interner, Name, SourceId, Span};

/// Maximum nesting the parser descends before it stops and reports.
///
/// Sized from measurement, not taste. `measure_max_depth` over all 2118 `.yel`
/// inputs in the repository tops out far below a quarter of this — asserted by
/// `tests/corpus.rs::real_programs_stay_well_under_both_depth_limits` — while a
/// debug build overflows its stack somewhere past ~1500 nested parentheses at
/// roughly six frames per level. The guard therefore trips with an order of
/// magnitude of headroom on the language side and on the stack side alike.
pub const MAX_NESTING_DEPTH: usize = 256;

/// Parse `content` and report the deepest nesting the parser reached.
///
/// Exists so the headroom between real programs and [`MAX_NESTING_DEPTH`] is a
/// measured number rather than a belief.
pub fn measure_max_depth(content: &str) -> usize {
    let interner = Interner::new();
    let mut diags = Diagnostics::new();
    let mut parser = Parser::new(SourceId(0), content, &interner, &mut diags);
    let _ = parser.parse_file();
    parser.max_depth_seen
}

/// Whether a list production accepts a separator before its terminator.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum TrailingSep {
    Allowed,
    Forbidden,
}

/// What a bracketed block holds at **depth zero** — not nested inside a further
/// bracket or a string interpolation. Filled in once by [`shallow_marks_table`].
#[derive(Copy, Clone, Default, PartialEq, Eq, Debug)]
pub(crate) struct ShallowMarks {
    /// A `;`. No `expr` can contain one, so a `{ … }` that holds one at depth
    /// zero is not a `record_literal`.
    pub(crate) semicolon: bool,
}

/// A remembered builder position, so a node can be *started* retroactively —
/// which is what left-associative binary expressions need, since the node kind
/// is only known after the left operand has been parsed.
#[derive(Clone)]
pub(crate) struct Mark {
    token_idx: usize,
    offset: u32,
    green: Marker,
}

pub(crate) struct Parser<'a> {
    source: SourceId,
    content: &'a str,
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    /// For every opener token, the index of its matching closer — or
    /// `tokens.len()` when it is unmatched. Computed once; see
    /// [`bracket_close_table`].
    bracket_close: Vec<u32>,
    /// For every token, the index of the token that decides whether an
    /// expression starting there is followed by a block. See
    /// [`condition_scan_table`].
    condition_scan: Vec<u32>,
    /// For every bracket, what its block holds at depth zero. See
    /// [`shallow_marks_table`].
    shallow_marks: Vec<ShallowMarks>,
    token_idx: usize,
    offset: u32,
    /// Open AST nodes: `(token index, byte offset)` at the point they started.
    nodes: Vec<(usize, u32)>,
    builder: GreenTreeBuilder,
    interner: &'a Interner,
    diags: &'a mut Diagnostics,
    next_node_id: u32,
    /// Current nesting of the guarded recursive entry points.
    depth: usize,
    /// High-water mark of `depth`, reported by [`measure_max_depth`].
    max_depth_seen: usize,
    /// The depth limit is reported once per parse; the input that trips it
    /// trips it thousands of times.
    reported_depth_limit: bool,
    /// How many times the depth limit has been hit, *reported or not*. The
    /// diagnostic is latched; this is not, so a speculative attempt can tell
    /// that it overflowed even when the latch was already spent.
    depth_limit_hits: usize,
    /// Spans of syntax the typed AST has no recovery slot for. Handed to
    /// `ast::File::recovery_marks` at the end of the parse; see the module docs.
    recovery_marks: Vec<Span>,
    /// Bytes already consumed from `tokens[token_idx]` by a token split.
    ///
    /// `grammar.pest` has no token layer, so a lexer's maximal munch can glue
    /// together two things the character-level grammar reads apart: `>=` is a
    /// `>` closing a type-argument list followed by an `=` (`list<s32>=[1]`).
    /// The token arrays are **never** mutated — `bracket_close`,
    /// `condition_scan` and the lossless-by-construction green tree all index
    /// them — so a split is recorded here instead: the prefix is pushed as its
    /// own green token, `offset` moves past it, and `token_idx` stays put until
    /// the remainder is consumed too. See [`Parser::expect_type_close`], the
    /// only site that splits.
    partial_offset: u32,
    /// Diagnostics produced while a speculative attempt is running.
    ///
    /// `yelc_base::Diagnostics` is frozen seam and has no `truncate`, and one
    /// should not be added for this: routing through a buffer makes "an
    /// abandoned attempt reports nothing" **structural** rather than a cleanup
    /// step that can be forgotten. Empty whenever `speculating == 0`.
    buffered_diagnostics: Vec<(Span, ErrorCode, String)>,
    /// Nesting of [`Parser::try_parse`]. Zero means every diagnostic goes
    /// straight to the sink.
    speculating: usize,
    /// `(site, byte offset, nesting depth)` triples a speculative attempt has
    /// already failed at. See [`Parser::try_parse`] — this is the bound.
    failed_attempts: FxHashSet<(Speculation, u32, usize)>,
}

/// The named points where this parser speculates.
///
/// There is no ambient backtracking mode: every variant here is one reviewable
/// call site, and the identity is what [`Parser::failed_attempts`] is keyed on.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum Speculation {
    /// `node = if_node | … | element_node` — the `if_node` alternative.
    IfNode,
}

/// Everything a speculative attempt can change, in one value.
///
/// `max_depth_seen` is deliberately **not** here: it is a high-water
/// measurement of how deep the parser went, not a parse result, and
/// [`measure_max_depth`] wants the honest maximum including the paths that were
/// tried and dropped. The interner is not here either — `Name`s are
/// content-addressed, so interning during an abandoned attempt is invisible.
#[derive(Clone)]
pub(crate) struct Checkpoint {
    token_idx: usize,
    offset: u32,
    partial_offset: u32,
    nodes: usize,
    green: GreenCheckpoint,
    recovery_marks: usize,
    depth: usize,
    next_node_id: u32,
    reported_depth_limit: bool,
    depth_limit_hits: usize,
    buffered_diagnostics: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(
        source: SourceId,
        content: &'a str,
        interner: &'a Interner,
        diags: &'a mut Diagnostics,
    ) -> Parser<'a> {
        let result = lex(source, content, diags);
        let bracket_close = bracket_close_table(&result.tokens);
        let condition_scan = condition_scan_table(&result.tokens, &bracket_close);
        let shallow_marks = shallow_marks_table(&result.tokens, &bracket_close);
        Parser {
            source,
            content,
            tokens: result.tokens,
            token_widths: result.widths,
            bracket_close,
            condition_scan,
            shallow_marks,
            token_idx: 0,
            offset: 0,
            nodes: Vec::new(),
            builder: GreenTreeBuilder::new(),
            interner,
            diags,
            next_node_id: 0,
            depth: 0,
            max_depth_seen: 0,
            reported_depth_limit: false,
            depth_limit_hits: 0,
            recovery_marks: Vec::new(),
            partial_offset: 0,
            buffered_diagnostics: Vec::new(),
            speculating: 0,
            failed_attempts: FxHashSet::default(),
        }
    }

    pub(crate) fn parse(mut self) -> ParsedFile {
        let ast_file = self.parse_file();
        debug_assert!(self.nodes.is_empty(), "unbalanced AST node stack");

        let green = self.builder.create_tree();
        debug_assert_eq!(
            green.len() as usize,
            self.content.len(),
            "green tree lost bytes"
        );

        ParsedFile {
            source: self.source,
            green,
            ast: ast_file,
        }
    }

    // -- cursor ------------------------------------------------------------

    /// Kind of the token the parser is looking at.
    ///
    /// When a prefix has been split off the current token
    /// ([`Parser::partial_offset`]), the *remainder* is what is left, and its
    /// kind is re-derived from its text.
    fn current(&self) -> TokenKind {
        if self.partial_offset > 0 {
            // The only split there is: `>=` cut into `>` and `=`, so a
            // type-argument list can close (`option<s32>=none`).
            if self.nth(0) == GE {
                debug_assert_eq!(self.current_text(), "=");
                return EQ;
            }
            return crate::token::keyword_kind(self.current_text()).unwrap_or(IDENTIFIER);
        }
        self.nth(0)
    }

    fn nth(&self, idx: usize) -> TokenKind {
        if self.token_idx + idx < self.tokens.len() {
            self.tokens[self.token_idx + idx]
        } else {
            EOF
        }
    }

    /// Byte width of token `idx` *as the parser still sees it* — the current
    /// token shrinks by whatever a split has already consumed.
    fn width_at(&self, idx: usize) -> u32 {
        let width = self.token_widths[idx];
        if idx == self.token_idx {
            width - self.partial_offset
        } else {
            width
        }
    }

    /// The `n`-th token *after skipping trivia*, where `n == 0` is `current()`.
    ///
    /// `advance` already skips trivia, so `current()` is never trivia; the
    /// lookahead positions are not, which is why this exists.
    fn nth_non_trivia(&self, n: usize) -> TokenKind {
        match self.nth_non_trivia_at(n) {
            Some((idx, _)) if idx == self.token_idx => self.current(),
            Some((idx, _)) => self.tokens[idx],
            None => EOF,
        }
    }

    /// `(raw token index, byte offset)` of the `n`-th non-trivia token.
    fn nth_non_trivia_at(&self, n: usize) -> Option<(usize, u32)> {
        let mut idx = self.token_idx;
        let mut offset = self.offset;
        let mut seen = 0;
        while idx < self.tokens.len() {
            if !self.tokens[idx].is_trivia() {
                if seen == n {
                    return Some((idx, offset));
                }
                seen += 1;
            }
            offset += self.width_at(idx);
            idx += 1;
        }
        None
    }

    /// Whether an expression starting at token `index` is followed by a
    /// depth-zero `{` — i.e. whether it is a *condition* rather than a whole
    /// statement. `O(1)`; see [`condition_scan_table`].
    fn expression_is_followed_by_a_block(&self, index: usize) -> bool {
        let decisive = self.condition_scan[index] as usize;
        decisive < self.tokens.len() && self.tokens[decisive] == L_BRACE
    }

    /// `children_node = { "@children" ~ ";"? }` — **one** string literal, so
    /// pest's implicit whitespace never runs between the `@` and the word.
    /// `@ children`, `@/*c*/children` and `@\nchildren` are all rejected by the
    /// frozen parser, and every `is2(AT, CHILDREN_KW)` accepted them, because
    /// `is2` looks at the next *non-trivia* token. This looks at the next raw
    /// one.
    pub(super) fn at_children_marker(&self) -> bool {
        self.is(AT) && self.nth(1) == CHILDREN_KW
    }

    /// What the block opened by token `index` holds at depth zero. `O(1)`; see
    /// [`shallow_marks_table`].
    pub(super) fn shallow_marks(&self, index: usize) -> ShallowMarks {
        self.shallow_marks.get(index).copied().unwrap_or_default()
    }

    /// The same, for the block the cursor is sitting on the `{` of.
    pub(super) fn shallow_marks_here(&self) -> ShallowMarks {
        self.shallow_marks(self.token_idx)
    }



    /// Source text of the `n`-th non-trivia token, or `""` past the end.
    fn nth_text(&self, n: usize) -> &'a str {
        match self.nth_non_trivia_at(n) {
            Some((idx, offset)) => {
                let start = offset as usize;
                &self.content[start..start + self.width_at(idx) as usize]
            }
            None => "",
        }
    }

    fn current_text(&self) -> &'a str {
        self.nth_text(0)
    }

    fn current_span(&self) -> Span {
        if self.token_idx < self.tokens.len() {
            let width = self.width_at(self.token_idx) as usize;
            Span::new(
                self.source,
                self.offset as usize,
                self.offset as usize + width,
            )
        } else {
            Span::point(self.source, self.offset as usize)
        }
    }

    fn text(&self, span: Span) -> &'a str {
        &self.content[span.start..span.end]
    }

    /// The parser's byte position — the progress measure every loop guard uses.
    ///
    /// **Not** `token_idx`: a keyword split (`recordFoo` → `record` + `Foo`)
    /// consumes bytes without advancing the token index, so a `token_idx`-based
    /// guard would report "consumed nothing" on real progress. `offset` is
    /// strictly monotonic under every consuming operation.
    fn position(&self) -> u32 {
        self.offset
    }

    fn is_eof(&self) -> bool {
        self.current() == EOF
    }

    // -- predicates --------------------------------------------------------

    fn is(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    /// `current()` is `fst` and the next non-trivia token is `snd`.
    fn is2(&self, fst: TokenKind, snd: TokenKind) -> bool {
        self.is(fst) && self.nth_non_trivia(1) == snd
    }

    fn is_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    fn nth_is_set(&self, n: usize, set: TokenSet) -> bool {
        set.contains(self.nth_non_trivia(n))
    }

    /// Anything the frozen grammar's `identifier` rule accepts — including
    /// every keyword, none of which this language reserves.
    fn is_name(&self) -> bool {
        self.is_set(crate::token::NAME_FIRST)
    }

    fn nth_is_name(&self, n: usize) -> bool {
        self.nth_is_set(n, crate::token::NAME_FIRST)
    }

    // -- consumption -------------------------------------------------------

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.is(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Push the first `len` bytes of the current token into the green tree as
    /// `kind`, leaving the remainder current.
    ///
    /// The single primitive behind [`Parser::expect_type_close`]: the token
    /// arrays are never mutated (`bracket_close`, `condition_scan` and
    /// invariants S1/S2 all index them), so the consumed prefix is recorded in
    /// [`Parser::partial_offset`] instead. No trivia can live *inside* a token,
    /// so this does not skip any.
    fn split_token(&mut self, kind: TokenKind, len: usize) {
        debug_assert!(
            len < self.width_at(self.token_idx) as usize,
            "a split must leave a remainder"
        );
        let content = self.content;
        let start = self.offset as usize;
        self.builder.token(kind, &content[start..start + len]);
        self.offset += len as u32;
        self.partial_offset += len as u32;
    }

    /// The closer of a type-argument list, taking the `>` out of a `>=` when
    /// the lexer's maximal munch glued them together.
    ///
    /// `grammar.pest` has no token layer at all: the `>` in `list_type` is a
    /// bare `">"` matched against the *character* stream, so `list<s32>= [1]`
    /// closes the list and leaves an `=`. A hand-written lexer cannot see that
    /// without splitting, and every `T<…>=` written without a space —
    /// `option<s32>=none`, `option<list<s32>>= some([1])` — was silently
    /// rejected.
    fn expect_type_close(&mut self) -> bool {
        if self.eat(GT) {
            return true;
        }
        if self.is(GE) {
            self.split_token(GT, 1);
            return true;
        }
        self.expect(GT)
    }

    /// [`Parser::is`] for a list terminator, seeing the `>` inside a `>=`.
    fn at_stop(&self, stop: TokenKind) -> bool {
        self.is(stop) || (stop == GT && self.is(GE))
    }

    /// [`Parser::expect`] for a list terminator, splitting a `>=` if that is
    /// what closes it.
    fn expect_stop(&mut self, stop: TokenKind) -> bool {
        if stop == GT {
            self.expect_type_close()
        } else {
            self.expect(stop)
        }
    }

    /// Consume `kind`, which the caller has already established is present.
    fn assert(&mut self, kind: TokenKind) {
        assert!(
            self.eat(kind),
            "assert {:?}, found {:?}",
            kind,
            self.current()
        );
    }

    /// Consume `kind`, or report a hole where it should have been.
    ///
    /// The hole is *recorded*, not only reported: a missing token has no slot in
    /// the typed AST, so [`Parser::record_recovery_mark`] is what turns it into
    /// an entry in [`ast::File::recovery_marks`]. Without that,
    /// `record R { a: list<s32` produced three diagnostics and zero `Error`
    /// nodes, which invariant S5 forbids.
    fn expect(&mut self, kind: TokenKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error_here(format!(
            "expected `{}`, found `{}`",
            kind.spelling(),
            self.current().spelling()
        ));
        let span = self.zero_width_error_node();
        self.record_recovery_mark(span);
        false
    }

    /// Mark the current position in the green tree as a recovery point without
    /// consuming anything, so S1/S2 are unaffected.
    fn zero_width_error_node(&mut self) -> Span {
        self.start_node();
        self.finish_node(ERROR)
    }

    /// Record a recovery position the typed AST has no slot for. It reaches the
    /// tree as an [`ast::RecoveryMark`], **not** as an element of any list.
    fn record_recovery_mark(&mut self, span: Span) {
        self.recovery_marks.push(span);
    }

    /// Hand every recorded position to the file node, in source order.
    ///
    /// Sorted rather than emitted in discovery order: the seam promises source
    /// order among siblings, and determinism (anti-spec A6) makes "whatever
    /// order the parser happened to notice them" not good enough.
    fn take_recovery_marks(&mut self) -> Vec<ast::RecoveryMark> {
        let mut spans = std::mem::take(&mut self.recovery_marks);
        spans.sort_by_key(|span| (span.start, span.end));
        spans
            .into_iter()
            .map(|span| ast::RecoveryMark {
                id: self.new_node_id(),
                span,
            })
            .collect()
    }

    fn advance(&mut self) {
        self.raw_advance();
        self.skip_trivia();
    }

    fn skip_trivia(&mut self) {
        while self.current().is_trivia() {
            self.raw_advance();
        }
    }

    /// The only place a token reaches the green tree. Trivia goes through here
    /// too, which is how it lands in the tree without the parser ever seeing it.
    fn raw_advance(&mut self) {
        if self.token_idx < self.tokens.len() {
            let kind = self.current();
            let width = self.width_at(self.token_idx);
            let start = self.offset as usize;
            let value = &self.content[start..start + width as usize];
            debug_assert!(kind < EOF);
            if !is_closed(kind, value) {
                // The lexer already reported it, but a token that was never
                // closed has no *node* to be the recovery point: an unterminated
                // block comment is trivia, so it leaves no trace in the AST at
                // all, and `package a:b;/*` produced a diagnostic and zero
                // recovery nodes (invariant S5). An unterminated string is worse
                // — it produced a perfectly ordinary `Str` (anti-spec B9).
                let span = Span::new(self.source, start, start + width as usize);
                self.recovery_marks.push(span);
            }
            self.builder.token(kind, value);
            self.offset += width;
            self.token_idx += 1;
            self.partial_offset = 0;
        }
    }

    // -- nodes and spans ---------------------------------------------------

    fn new_node_id(&mut self) -> NodeId {
        let id = NodeId(self.next_node_id);
        self.next_node_id += 1;
        id
    }

    fn start_node(&mut self) {
        self.nodes.push((self.token_idx, self.offset));
        self.builder.start_node();
    }

    /// Close the innermost open node as `kind` and return its AST span, which
    /// **excludes trailing trivia** even though the green node covers it.
    fn finish_node(&mut self, kind: TokenKind) -> Span {
        let (start_token, start_offset) = self.nodes.pop().expect("missing node start");
        self.builder.finish_node(kind);
        self.span_between(start_token, start_offset)
    }

    fn mark(&mut self) -> Mark {
        Mark {
            token_idx: self.token_idx,
            offset: self.offset,
            green: self.builder.create_marker(),
        }
    }

    /// Close a node that started retroactively at `mark`.
    fn finish_marked(&mut self, kind: TokenKind, mark: &Mark) -> Span {
        self.builder
            .finish_node_starting_at(kind, mark.green.clone());
        self.span_between(mark.token_idx, mark.offset)
    }

    fn span_between(&self, start_token: usize, start_offset: u32) -> Span {
        if self.offset <= start_offset {
            return Span::point(self.source, start_offset as usize);
        }
        if self.token_idx == 0 || self.token_idx <= start_token {
            // Bytes were consumed without the token index moving, which only a
            // keyword split does. There is no trailing trivia to trim inside a
            // token, so the byte range is the span.
            return Span::new(self.source, start_offset as usize, self.offset as usize);
        }

        let mut end_token = self.token_idx - 1;
        let mut end_offset = self.offset;
        while end_token > start_token {
            if !self.tokens[end_token].is_trivia() {
                break;
            }
            end_offset -= self.token_widths[end_token];
            end_token -= 1;
        }
        // A node that consumed only trivia collapses to a point.
        if self.tokens[end_token].is_trivia() {
            end_offset -= self.token_widths[end_token];
        }
        Span::new(
            self.source,
            start_offset as usize,
            end_offset.max(start_offset) as usize,
        )
    }

    // -- speculation -------------------------------------------------------
    //
    // Commit by default; speculate at named points. Every [`Speculation`]
    // variant is one reviewable call site, which is what keeps this from being
    // an ambient backtracking mode.
    //
    // That distinction is a scope constraint, not taste. pest backtracks
    // everywhere, which is exactly why every frozen parse failure collapses to
    // a single E0060 pointing at the whole construct. Diagnostic *meaning* on
    // the diagnostic fixtures is frozen (`scope.md`) and the recovery-set +
    // `Error`-node design that improves on pest only works where the parser
    // commits. So: speculate where the grammar's ordered choice genuinely
    // needs it, commit everywhere else.

    /// Everything an abandoned attempt must put back.
    fn checkpoint(&self) -> Checkpoint {
        Checkpoint {
            token_idx: self.token_idx,
            offset: self.offset,
            partial_offset: self.partial_offset,
            nodes: self.nodes.len(),
            green: self.builder.checkpoint(),
            recovery_marks: self.recovery_marks.len(),
            depth: self.depth,
            next_node_id: self.next_node_id,
            reported_depth_limit: self.reported_depth_limit,
            depth_limit_hits: self.depth_limit_hits,
            buffered_diagnostics: self.buffered_diagnostics.len(),
        }
    }

    /// Undo everything since `cp`.
    ///
    /// Every field is restored by assignment or `truncate`; nothing is
    /// recomputed. The token arrays are immutable, so there is nothing to undo
    /// there — a keyword split lives entirely in `partial_offset`.
    fn restore(&mut self, cp: Checkpoint) {
        self.token_idx = cp.token_idx;
        self.offset = cp.offset;
        self.partial_offset = cp.partial_offset;
        self.nodes.truncate(cp.nodes);
        self.builder.rewind(cp.green);
        self.recovery_marks.truncate(cp.recovery_marks);
        self.depth = cp.depth;
        self.next_node_id = cp.next_node_id;
        self.reported_depth_limit = cp.reported_depth_limit;
        self.depth_limit_hits = cp.depth_limit_hits;
        self.buffered_diagnostics.truncate(cp.buffered_diagnostics);
    }

    /// Try `f`; on `None`, restore exactly and report nothing.
    ///
    /// # The bound
    ///
    /// Unmemoized PEG backtracking is exponential. `failed_attempts` keys on
    /// `(site, byte position, depth)`, so the same alternative is never retried
    /// at the same place twice — the packrat trick, narrowed to the handful of
    /// sites that speculate. Without it, nested brace-led forms would re-scan
    /// their tails once per enclosing attempt.
    ///
    /// # Why success must consume
    ///
    /// A speculative attempt that returns `Some` without advancing would defeat
    /// every `assert!(position() > before)` no-progress guard downstream: the
    /// enclosing loop would see a successful parse and no movement. That is a
    /// hang, so it is checked here rather than left to the call sites.
    fn try_parse<T>(
        &mut self,
        site: Speculation,
        f: impl FnOnce(&mut Self) -> Option<T>,
    ) -> Option<T> {
        let key = (site, self.position(), self.depth);
        if self.failed_attempts.contains(&key) {
            return None;
        }

        let cp = self.checkpoint();
        let before = self.position();

        self.speculating += 1;
        let result = f(self);
        self.speculating -= 1;

        match result {
            Some(value) => {
                debug_assert!(
                    self.position() > before,
                    "{site:?} succeeded without consuming input at {before}"
                );
                // Only the outermost attempt commits to the sink; a nested one
                // leaves its diagnostics buffered for the attempt above it,
                // which may still be abandoned.
                if self.speculating == 0 {
                    for (span, code, message) in std::mem::take(&mut self.buffered_diagnostics) {
                        self.diags.error(span, code, message);
                    }
                }
                Some(value)
            }
            None => {
                self.restore(cp);
                self.failed_attempts.insert(key);
                None
            }
        }
    }

    // -- diagnostics -------------------------------------------------------
    //
    // One idiom, per anti-spec B6: every parse diagnostic is an E0060
    // `SyntaxError` reported through `Diagnostics::error`.

    fn error_at(&mut self, span: Span, message: impl Into<String>) {
        if self.speculating > 0 {
            self.buffered_diagnostics
                .push((span, ErrorCode::SyntaxError, message.into()));
            return;
        }
        self.diags.error(span, ErrorCode::SyntaxError, message);
    }

    fn error_here(&mut self, message: impl Into<String>) {
        let span = self.current_span();
        self.error_at(span, message);
    }

    // -- names -------------------------------------------------------------

    fn intern(&self, text: &str) -> Name {
        self.interner.intern(text)
    }

    fn make_ident(&mut self, span: Span, text: &str) -> ast::Ident {
        ast::Ident {
            id: self.new_node_id(),
            span,
            name: self.intern(text),
        }
    }

    /// Consume an `identifier`. Keywords are contextual, so any keyword token
    /// is accepted here.
    ///
    /// A name that is not there becomes a [`ast::Recovered::Missing`] **hole**,
    /// not a value. The predecessor interned `""` and handed back a real
    /// `ast::Ident`, so `package ;` produced a `PackageDecl` whose namespace and
    /// name were both a `Name` and equal to each other. The span is a point at
    /// the **current token**, which is where the name is missing — the old code
    /// reported that hole at the enclosing declaration's start, offset 0.
    fn expect_name(&mut self) -> ast::MaybeIdent {
        if self.is_name() {
            let span = self.current_span();
            let text = self.current_text();
            self.advance();
            ast::Recovered::Present(self.make_ident(span, text))
        } else {
            let span = self.current_span();
            self.error_at(
                span,
                format!(
                    "expected an identifier, found `{}`",
                    self.current().spelling()
                ),
            );
            ast::Recovered::Missing {
                id: self.new_node_id(),
                span: Span::point(span.source, span.start),
            }
        }
    }

    // -- nesting depth -----------------------------------------------------

    /// Claim one level of nesting. `false` means the limit is reached and the
    /// caller must **not** recurse.
    fn enter_nesting(&mut self) -> bool {
        if self.depth >= MAX_NESTING_DEPTH {
            return false;
        }
        self.depth += 1;
        self.max_depth_seen = self.max_depth_seen.max(self.depth);
        true
    }

    fn leave_nesting(&mut self) {
        self.depth -= 1;
    }

    /// Report the depth limit **once** per parse, consume exactly one token so
    /// every enclosing loop makes progress, and return the span of a green
    /// `ERROR` node covering it.
    fn nesting_limit_node(&mut self) -> Span {
        // Counted on **every** hit, not just the reported one. The diagnostic is
        // latched to once per parse, so a speculative attempt that overflows
        // after the latch is already spent buffers nothing — and a criterion of
        // "parsed without reporting" would then accept a body full of `ERROR`
        // nodes as a clean match. See `Speculation` and the `if`-site in
        // `nodes.rs`.
        self.depth_limit_hits += 1;
        if !self.reported_depth_limit {
            self.reported_depth_limit = true;
            self.error_here(format!(
                "expression nests deeper than {MAX_NESTING_DEPTH} levels"
            ));
        }
        self.start_node();
        // At end of input there is nothing left to consume; every enclosing loop
        // also tests `is_eof`, so progress is still guaranteed.
        if !self.is_eof() {
            self.advance();
        }
        self.finish_node(ERROR)
    }

    // -- modifiers ---------------------------------------------------------

    /// `export_modifier?` — the one place a green node is started *speculatively*
    /// and abandoned when the modifier turns out not to be there.
    fn parse_export_modifier(&mut self) -> bool {
        self.builder.start_node();
        if self.eat(EXPORT_KW) {
            self.builder.finish_node(MODIFIER);
            true
        } else {
            self.builder.abandon_node();
            false
        }
    }

    // -- recovery ----------------------------------------------------------

    /// Skip tokens until one of `set` (or end of file). Returns whether
    /// anything was skipped.
    fn recover_to(&mut self, set: TokenSet) -> bool {
        let before = self.position();
        while !self.is_eof() && !self.is_set(set) {
            self.advance();
        }
        self.position() > before
    }

    /// The generic list production, carried over from ark in shape.
    ///
    /// The caller guarantees `start` is the current token — every call site
    /// checks `is(start)` first, which is what keeps `assert(start)` from being
    /// a panic path (invariant S6).
    // The arity is the point: `start`/`sep`/`stop`/`recovery_set`/`node` are the
    // ported ark signature, and `trailing` is the one grammar-fidelity addition.
    // Bundling them into a struct would hide which list production is being
    // described at the call site, which is the only place it can be checked.
    #[allow(clippy::too_many_arguments)]
    fn parse_list<F, R>(
        &mut self,
        start: TokenKind,
        sep: TokenKind,
        stop: TokenKind,
        recovery_set: TokenSet,
        node: TokenKind,
        trailing: TrailingSep,
        mut parse: F,
    ) -> Vec<R>
    where
        R: ast::Recovery,
        F: FnMut(&mut Parser<'a>) -> Option<R>,
    {
        let mut data = Vec::new();
        self.builder.start_node();
        self.assert(start);

        while !self.at_stop(stop) && !self.is_eof() {
            let pos_before_element = self.position();
            let entry = parse(self);

            match entry {
                Some(entry) => {
                    // The callback must advance by at least one token, or this
                    // loops forever on malformed input.
                    assert!(
                        self.position() > pos_before_element,
                        "list callback consumed nothing at {:?}",
                        self.current()
                    );
                    data.push(entry);
                }
                None => {
                    if self.is_set(recovery_set) {
                        break;
                    }
                    // Report **and** mark: an element the parser could not read
                    // is a recovery node in the list, not a gap in it.
                    self.start_node();
                    self.error_here(format!(
                        "unexpected `{}` in list",
                        self.current().spelling()
                    ));
                    self.advance();
                    let span = self.finish_node(ERROR);
                    let id = self.new_node_id();
                    data.push(R::recovery(id, span));
                }
            }

            if !self.at_stop(stop) {
                self.expect(sep);
                if trailing == TrailingSep::Forbidden && self.at_stop(stop) {
                    self.error_here(format!(
                        "a trailing `{}` is not allowed here",
                        sep.spelling()
                    ));
                    // The separator is one token too many, which no element
                    // slot can express — so it becomes a hole the drain below
                    // turns into a recovery element.
                    let span = self.zero_width_error_node();
                    self.record_recovery_mark(span);
                }
            }
        }

        self.expect_stop(stop);
        self.builder.finish_node(node);

        data
    }

    /// Span of everything consumed since `mark`, trailing trivia excluded.
    fn span_from(&self, mark: &Mark) -> Span {
        self.span_between(mark.token_idx, mark.offset)
    }
}

/// Match every bracket in the token stream once, so the `{`-disambiguation scan
/// can jump over a nested group instead of counting its way through it.
///
/// `has_depth_zero_arrow` used to walk to end-of-stream on an unterminated `{`,
/// which is quadratic in the number of open braces — 14.6 ms for 2000 of them.
/// Closers pop whatever is on top, which is exactly the depth counting the scan
/// did before; an unmatched opener maps to `tokens.len()`, so a scan bounded by
/// it stops at end of input.
fn bracket_close_table(tokens: &[TokenKind]) -> Vec<u32> {
    let end = tokens.len() as u32;
    let mut close = vec![end; tokens.len()];
    let mut open: Vec<usize> = Vec::new();
    for (index, kind) in tokens.iter().enumerate() {
        match kind {
            L_PAREN | L_BRACKET | L_BRACE | TEMPLATE_LITERAL => open.push(index),
            R_PAREN | R_BRACKET | R_BRACE | TEMPLATE_END_LITERAL => {
                if let Some(opener) = open.pop() {
                    close[opener] = index as u32;
                }
            }
            // `TEMPLATE_MIDDLE_LITERAL` is neutral: `}…{` closes and reopens the
            // same interpolation, so it changes no depth.
            _ => {}
        }
    }
    close
}

/// Whether a delimited token reached its closing delimiter.
///
/// The lexer reports each of these and carries on, which is right — but the
/// token it produces is indistinguishable from a well-formed one downstream, and
/// for a block comment it is *trivia*, so nothing in the AST records that
/// anything went wrong. `raw_advance` turns a `false` here into an
/// [`ast::RecoveryMark`], which is what makes invariant S5 hold for the
/// unterminated cases.
fn is_closed(kind: TokenKind, text: &str) -> bool {
    match kind {
        // `/*` … `*/` — `/*/` ends with `*/` but is three bytes long and open.
        MULTILINE_COMMENT => text.len() >= 4 && text.ends_with("*/"),
        // `"` … `"` and `}` … `"`.
        STRING_LITERAL | TEMPLATE_END_LITERAL => text.len() >= 2 && text.ends_with('"'),
        // `"` … `{` and `}` … `{`; the lexer only emits these having seen the `{`.
        TEMPLATE_LITERAL | TEMPLATE_MIDDLE_LITERAL => text.len() >= 2 && text.ends_with('{'),
        CHAR_LITERAL => text.len() >= 2 && text.ends_with('\''),
        _ => true,
    }
}

/// For each token index, the index of the first token at **depth zero** from
/// there that decides whether what follows is a condition or a whole statement:
/// a `{` (condition — a block follows) or a statement terminator (not).
///
/// # Why a table
///
/// `if` is not reserved, so `if x { … }` is an if-statement and `if(x);` is a
/// call on a variable called `if`; pest tells them apart by backtracking, and
/// the observable difference is whether a `{` follows the expression. Scanning
/// for it per `if` is the `has_depth_zero_arrow` shape anti-spec B8 flagged:
/// `{ if if if … }` would rescan the whole tail once per `if`, quadratically,
/// and an unterminated block would scan to end of input every time.
///
/// One **backward** pass builds the answer for every position instead. Each
/// entry depends only on entries strictly to its right — `i + 1`, or the token
/// after the group `i` opens — so the whole table costs `O(tokens)` and every
/// query is `O(1)`, bounded by construction.
fn condition_scan_table(tokens: &[TokenKind], bracket_close: &[u32]) -> Vec<u32> {
    let end = tokens.len() as u32;
    let mut scan = vec![end; tokens.len()];
    for index in (0..tokens.len()).rev() {
        let next = |at: usize| if at < tokens.len() { scan[at] } else { end };
        scan[index] = match tokens[index] {
            // A depth-zero `{` is the answer itself: a block follows.
            L_BRACE => index as u32,
            // Anything that cannot appear inside an expression ends the scan.
            SEMICOLON
            | R_BRACE
            | COMMA
            | R_PAREN
            | R_BRACKET
            | TEMPLATE_MIDDLE_LITERAL
            | TEMPLATE_END_LITERAL => index as u32,
            // Jump over a bracketed group rather than descending into it.
            L_PAREN | L_BRACKET | TEMPLATE_LITERAL => {
                let close = bracket_close[index] as usize;
                next(close.saturating_add(1))
            }
            _ => next(index + 1),
        };
    }
    scan
}

/// For every `{`, whether the block it opens holds a `;` at **depth zero** —
/// that is, one not nested inside a further bracket or a string interpolation.
///
/// # What it is for
///
/// One consumer: `exprs.rs`'s record-vs-closure decision. `record_literal_fields`
/// is `field ("," field)* ","?` and a `field` is `name ":" expr`, and no `expr`
/// can contain a `;`. So a depth-zero `;` proves the block is **not** a record
/// literal — pest backtracks out of `record_literal` and `closure_no_params`
/// takes it. `{ lets: s32 = 1; }` is the statement `let s: s32 = 1;`, not a
/// record whose first field ran into an `=`.
///
/// This table used to carry a `colon` flag as well, for the glued-`if`
/// disambiguation. That site **speculates** now (`nodes.rs`), the flag became
/// write-only, and `#[derive(Debug)]` was suppressing the dead-code lint that
/// would have said so — anti-spec A9. Both are gone.
///
/// # Why a table
///
/// Anti-spec B8: no per-query scan. Openers come off the stack by
/// [`bracket_close_table`]'s matching rather than by counting, so an unmatched
/// `{` never drags the scan to end of input.
fn shallow_marks_table(tokens: &[TokenKind], bracket_close: &[u32]) -> Vec<ShallowMarks> {
    let mut holds = vec![ShallowMarks::default(); tokens.len()];
    let mut open: Vec<usize> = Vec::new();
    for (index, kind) in tokens.iter().enumerate() {
        while open
            .last()
            .is_some_and(|&top| bracket_close[top] as usize <= index)
        {
            open.pop();
        }
        let Some(&top) = open.last() else {
            if matches!(kind, L_PAREN | L_BRACKET | L_BRACE | TEMPLATE_LITERAL) {
                open.push(index);
            }
            continue;
        };
        match kind {
            L_PAREN | L_BRACKET | L_BRACE | TEMPLATE_LITERAL => open.push(index),
            SEMICOLON => holds[top].semicolon = true,
            _ => {}
        }
    }
    holds
}

/// `attr_name`, `enum_case` and `variant_case_name` are lowercase kebab in the
/// frozen grammar — `ASCII_ALPHA_LOWER ~ (ASCII_ALPHA_LOWER | ASCII_DIGIT | "-")*`.
///
/// The lexer produces one `IDENTIFIER` for `fontSize`, but pest's `attr_name`
/// would have matched only `font` and then failed on the `S`. Checking the
/// shape here is what keeps `fontSize: 24px` rejected, as it is today.
pub(crate) fn is_kebab_lower(text: &str) -> bool {
    let mut chars = text.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::ast::visit::ErrorNodeCounter;

    // -- speculation: rollback must be EXACT -------------------------------
    //
    // A rollback that is nearly exact is worse than none: it fails silently,
    // and what it corrupts first is the byte-for-byte green tree (S1) that
    // every other invariant rests on. So these assert on the whole checkpoint,
    // field by field, rather than on "the parse still worked".

    /// Every field `Checkpoint` captures, read back off a live parser.
    #[allow(clippy::type_complexity)]
    fn state(
        p: &Parser<'_>,
    ) -> (
        usize,
        u32,
        u32,
        usize,
        usize,
        usize,
        usize,
        u32,
        bool,
        usize,
        String,
    ) {
        (
            p.token_idx,
            p.offset,
            p.partial_offset,
            p.nodes.len(),
            p.builder.checkpoint().children_len(),
            p.recovery_marks.len(),
            p.depth,
            p.next_node_id,
            p.reported_depth_limit,
            p.depth_limit_hits,
            p.builder.text_so_far(),
        )
    }

    /// Drive a failing speculation over `src` and assert nothing moved.
    fn assert_rollback_is_exact(src: &str, consume: usize) {
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let mut p = Parser::new(SourceId(0), src, &interner, &mut diags);

        // Get off offset 0 so a checkpoint that silently resets to the start
        // cannot masquerade as a correct restore.
        p.skip_trivia();
        for _ in 0..consume {
            p.advance();
        }

        let before = state(&p);
        let diags_before = p.diags.len();

        let outcome = p.try_parse(Speculation::IfNode, |p| {
            // Move **every** field the checkpoint captures, then fail. An
            // earlier version of this closure was node- and depth-*balanced*, so
            // `nodes.len()`, `depth`, `reported_depth_limit` and
            // `depth_limit_hits` never changed and the `assert_eq!(state(..))`
            // below compared four values against themselves. Deleting any of
            // those four restore lines left the whole suite green — the exact
            // vacuous-assertion shape anti-spec A8 names, found by review.
            //
            // The imbalance is deliberate: a *failed* attempt is allowed to
            // leave the parser mid-construct, and putting that back is what
            // `restore` is for.
            p.start_node();
            p.builder.start_node();
            for _ in 0..3 {
                if !p.is_eof() {
                    p.advance();
                }
            }
            let _ = p.new_node_id();
            p.error_here("speculative diagnostic that must never be seen");
            let span = p.current_span();
            p.recovery_marks.push(span);
            // Unbalanced on purpose: `depth` and the AST node stack must both be
            // left dirty for the restore to have anything to undo.
            assert!(p.enter_nesting(), "the probe needs depth headroom");
            p.start_node();
            // Trips the depth latch and the hit counter without reporting twice.
            let _ = p.nesting_limit_node();
            None::<()>
        });

        assert!(outcome.is_none(), "the attempt was supposed to fail");
        assert_eq!(state(&p), before, "speculative attempt leaked parser state");
        assert_eq!(p.diags.len(), diags_before, "abandoned attempt reported");
        assert!(
            p.buffered_diagnostics.is_empty(),
            "abandoned attempt left a buffered diagnostic"
        );
        assert_eq!(p.speculating, 0, "speculation depth leaked");
    }

    /// Discriminant name of the first UI node in the first component.
    fn first_node_kind(source: &str) -> String {
        node_kind(&parse_ok(source))
    }

    /// [`first_node_kind`] for input both parsers reject.
    fn first_node_kind_err(source: &str) -> String {
        node_kind(&parse_err(source))
    }

    fn node_kind(parsed: &Parsed) -> String {
        let component = parsed.component(1);
        match component.body().next() {
            Some(ast::UiNode::If(_)) => "If".into(),
            Some(ast::UiNode::For(_)) => "For".into(),
            Some(ast::UiNode::Element(_)) => "Element".into(),
            Some(ast::UiNode::Text(_)) => "Text".into(),
            Some(ast::UiNode::Children { .. }) => "Children".into(),
            Some(ast::UiNode::Error { .. }) => "Error".into(),
            None => "none".into(),
        }
    }

    /// A keyword glued to a name is one identifier, in both compilers.
    ///
    /// These have the **same accept/reject bit** as the frozen parser in the
    /// `{`-terminated cases, so the one-bit parity oracle cannot see them
    /// (anti-spec A18). Each expectation below was read out of the frozen
    /// parser's own AST dump, not decided here.
    ///
    /// This test used to assert the opposite for the first case — `ife { … }`
    /// was an if-node over a condition called `e`. It is the specification of
    /// the keyword word boundary and moved with it; see
    /// `plans/rewrite/goldens-changed.md`.
    #[test]
    fn a_keyword_glued_to_a_name_is_one_identifier() {
        // Frozen: element `ife`. `if_node` needs `!GLUED_IF ~ "if"` and the `e`
        // is an identifier character, so the alternative never opens.
        assert_eq!(
            first_node_kind("package a:b@0.1.0;\ncomponent A { ife { div {} } }"),
            "Element",
            "`ife {{ div {{}} }}` is an element named `ife`, not an if-node"
        );
        // Frozen: element `iflex`, for the same reason. It reached this reading
        // by backtracking before; now it never leaves it.
        assert_eq!(
            first_node_kind("package a:b@0.1.0;\ncomponent A { iflex { color: red } }"),
            "Element",
        );
        // `forx` is a name, and a name not followed by `{` is not a node at
        // all — so unlike the two above this one changes the accept/reject bit,
        // in both compilers together.
        assert_eq!(
            first_node_kind_err("package a:b@0.1.0;\ncomponent A { forx in xs { \"a\" } }"),
            "Error",
        );
    }

    /// Prints this parser's accept/reject for the open divergence set, so the
    /// table in the stage file is measured rather than remembered (A19).
    /// `cargo test -p yelc-syntax --lib open_divergences -- --nocapture`
    #[test]
    fn open_divergences_report() {
        let cases = [
            ("f: func;", "accept"),
            ("x: list<s32>=1;", "accept"),
            ("if { a: 1 } { div {} }", "accept"),
            ("x: result<>;", "REJECT"),
            ("x: tuple<>;", "REJECT"),
            ("x: s32 = (1,,);", "accept"),
            ("@ children", "REJECT"),
            ("if a { \"a\" } elseif b { \"c\" }", "accept"),
        ];
        for (body, frozen) in cases {
            let src = format!("package a:b@0.1.0;\ncomponent A {{ {body} }}");
            let interner = Interner::new();
            let mut diags = Diagnostics::new();
            let parsed = crate::parse(SourceId(0), &src, &interner, &mut diags);
            let errors = ErrorNodeCounter::run(&parsed.ast).count;
            let ours = if diags.is_empty() && errors == 0 {
                "accept"
            } else {
                "REJECT"
            };
            let verdict = if ours == frozen { "agree" } else { "DIVERGE" };
            println!("{body:<32} frozen={frozen:<7} ours={ours:<7} {verdict}");
        }
    }

    /// Identical input, identical output — green text, node ids, spans, and
    /// diagnostics.
    ///
    /// The parser holds a hash set (`failed_attempts`) and allocates `NodeId`s
    /// from a counter, so both of the classic determinism hazards are present:
    /// map iteration order reaching output (anti-spec A6) and an id space whose
    /// values depend on history. `FxHashSet` and per-file id allocation are what
    /// make this hold; nothing asserted it until now.
    #[test]
    fn parsing_is_deterministic() {
        let sources = [
            "package a:b@0.1.0;\ncomponent A { x: s32 = 1; div { \"t\" } }",
            // speculation-heavy: the `if {` site attempts and backtracks — the
            // record-literal condition commits, the element reading does not
            "package a:b@0.1.0;\ncomponent A { if { a: 1 } { div {} } if { span { \"x\" } } }",
            // ill-formed, so the diagnostic and recovery paths run too
            "package a:b@;\ncomponent A { f: func( ; record R { a: list<s32 }",
        ];

        for src in sources {
            let run = || {
                let interner = Interner::new();
                let mut diags = Diagnostics::new();
                let parsed = crate::parse(SourceId(0), src, &interner, &mut diags);
                // The whole typed tree, `NodeId`s and `Span`s included — not a
                // summary. Counts would agree even if ids were allocated in a
                // different order, which is exactly the hazard being tested.
                let tree = format!("{:?}", parsed.ast.items);
                let rendered: Vec<String> = diags.iter().map(|d| format!("{d:?}")).collect();
                (parsed.green.text(), tree, rendered)
            };
            let (first, second) = (run(), run());
            assert_eq!(first.0, second.0, "green text differed for {src:?}");
            assert_eq!(first.1, second.1, "AST (ids/spans) differed for {src:?}");
            assert_eq!(first.2, second.2, "diagnostics differed for {src:?}");
        }
    }

    /// The `if {` decision must not depend on the rest of the file.
    ///
    /// Found by review. The depth diagnostic is latched to once per parse, so a
    /// speculative `parse_if_node` that overflows *after* something else spent
    /// the latch buffers no diagnostic at all — and a criterion of "parsed
    /// without reporting" accepted it. The same block then read as an element on
    /// its own and as an `if` when preceded by an unrelated deeply-nested
    /// declaration in a different component.
    ///
    /// The subject was `ife { … }` while keywords had no word boundary. That is
    /// now unambiguously an element, so the probe moved to the ambiguity that
    /// survives: `if` followed directly by `{`, which is either a record-literal
    /// condition or an element literally called `if`.
    #[test]
    fn the_if_versus_element_decision_does_not_depend_on_the_rest_of_the_file() {
        // The window is narrow — the two readings only disagree right at the
        // depth boundary — so scan across it rather than guessing one value.
        // A hard-coded 300 sits past the window and passes even when the bug is
        // present.
        let spender = format!(
            "component Z {{ y: s32 = {}1{}; }}",
            "(".repeat(MAX_NESTING_DEPTH + 144),
            ")".repeat(MAX_NESTING_DEPTH + 144)
        );

        let kind_of = |src: &str| {
            let interner = Interner::new();
            let mut diags = Diagnostics::new();
            let parsed = crate::parse(SourceId(0), src, &interner, &mut diags);
            let component = parsed
                .ast
                .items
                .iter()
                .find_map(|item| match item {
                    ast::ItemKind::Component(c) if c.body().next().is_some() => Some(c),
                    _ => None,
                })
                .expect("a component with a body");
            match component.body().next() {
                Some(ast::UiNode::If(_)) => "If",
                Some(ast::UiNode::Element(_)) => "Element",
                other => panic!("unexpected node: {}", other.is_some()),
            }
        };

        for depth in (MAX_NESTING_DEPTH - 8)..=(MAX_NESTING_DEPTH + 8) {
            let deep = format!("{}\"x\"{}", "dv { ".repeat(depth), " }".repeat(depth));
            let subject = format!("component A {{ if {{ {deep} }} }}");
            assert_eq!(
                kind_of(&subject),
                kind_of(&format!("{spender}\n{subject}")),
                "at nesting {depth}, the same `if {{ … }}` was read as two \
                 different constructs depending on an unrelated declaration \
                 elsewhere in the file"
            );
        }
    }

    #[test]
    fn speculative_failure_restores_every_field() {
        for consume in [0, 1, 3, 7] {
            assert_rollback_is_exact(
                "package a:b@0.1.0;\ncomponent A { x: s32 = 1; div { \"t\" } }",
                consume,
            );
        }
    }

    #[test]
    fn speculative_failure_restores_across_a_token_split() {
        // The checkpoint must be taken with `partial_offset` NON-ZERO, or the
        // field is never exercised. An earlier version of this test just parsed
        // from offset 0 and passed even with the `partial_offset` restore
        // deleted — a vacuous assertion of exactly the kind anti-spec A8 names.
        // Splitting a token first is what makes it bite.
        //
        // The subject used to be a keyword split (`recordFoo` → `record` +
        // `Foo`). Keywords have a word boundary now, so the only split left is
        // `expect_type_close` taking the `>` out of a `>=` — a separate
        // scannerless artifact, and the reason `partial_offset` survives.
        let src = ">= 1";
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let mut p = Parser::new(SourceId(0), src, &interner, &mut diags);
        p.skip_trivia();

        assert!(
            p.expect_type_close(),
            "expected `>=` to split into `>` and `=`"
        );
        assert_ne!(p.partial_offset, 0, "the split left no partial offset");

        let before = state(&p);
        let split_before = p.partial_offset;

        let outcome = p.try_parse(Speculation::IfNode, |p| {
            p.advance();
            None::<()>
        });

        assert!(outcome.is_none());
        assert_eq!(
            p.partial_offset, split_before,
            "restore lost the token-split cursor"
        );
        assert_eq!(state(&p), before, "speculative attempt leaked parser state");
    }

    #[test]
    fn the_memo_table_stops_a_second_attempt_at_the_same_place() {
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let mut p = Parser::new(SourceId(0), "component A { }", &interner, &mut diags);
        p.skip_trivia();

        let mut ran = 0;
        for _ in 0..5 {
            let _ = p.try_parse(Speculation::IfNode, |p| {
                ran += 1;
                p.advance();
                None::<()>
            });
        }
        assert_eq!(ran, 1, "the same failed attempt was retried");
    }

    #[test]
    fn a_successful_speculation_flushes_its_diagnostics() {
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let mut p = Parser::new(SourceId(0), "component A { }", &interner, &mut diags);
        p.skip_trivia();

        let out = p.try_parse(Speculation::IfNode, |p| {
            p.error_here("a real diagnostic from a committed attempt");
            p.advance();
            Some(())
        });

        assert!(out.is_some());
        assert_eq!(p.diags.len(), 1, "committed diagnostic did not reach the sink");
        assert!(p.buffered_diagnostics.is_empty(), "buffer not drained");
    }

    pub(crate) struct Parsed {
        pub(crate) file: ParsedFile,
        pub(crate) diags: Diagnostics,
        pub(crate) interner: Interner,
    }

    impl Parsed {
        pub(crate) fn errors(&self) -> usize {
            ErrorNodeCounter::run(&self.file.ast).count
        }

        pub(crate) fn name(&self, name: Name) -> String {
            self.interner.str(name).to_string()
        }

        pub(crate) fn items(&self) -> &[ast::ItemKind] {
            &self.file.ast.items
        }

        pub(crate) fn component(&self, index: usize) -> &ast::ComponentDecl {
            match &self.file.ast.items[index] {
                ast::ItemKind::Component(c) => c,
                other => panic!("item {index} is not a component: {:?}", other.span()),
            }
        }
    }

    /// Parse and assert the two invariants every test depends on: the green
    /// tree round-trips byte-for-byte (S1) and its length matches (S2).
    pub(crate) fn parse_ok(source: &str) -> Parsed {
        let parsed = check(source);
        assert_eq!(parsed.errors(), 0, "unexpected Error nodes in {source:?}");
        assert!(
            !parsed.diags.has_errors(),
            "unexpected diagnostics in {source:?}: {}",
            parsed.diags
        );
        parsed
    }

    pub(crate) fn check(source: &str) -> Parsed {
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let file = crate::parse(SourceId(0), source, &interner, &mut diags);
        assert_eq!(file.green.text(), source, "green tree did not round-trip");
        assert_eq!(file.green.len() as usize, source.len());
        Parsed {
            file,
            diags,
            interner,
        }
    }

    /// Ill-formed input: a diagnostic **and** an `Error` node, never one alone
    /// (invariant S5).
    pub(crate) fn parse_err(source: &str) -> Parsed {
        let parsed = check(source);
        assert!(
            parsed.diags.has_errors(),
            "expected a diagnostic for {source:?}"
        );
        assert!(
            parsed.errors() > 0,
            "expected an Error node for {source:?}: a diagnostic alone violates S5"
        );
        parsed
    }

    // ==================== files and items ====================

    #[test]
    fn parse_empty_file() {
        let p = parse_ok("");
        assert!(p.items().is_empty());
    }

    #[test]
    fn parse_only_trivia() {
        let p = parse_ok("// just a comment\n/* and a block */\n");
        assert!(p.items().is_empty());
    }

    #[test]
    fn parse_package_decl() {
        let p = parse_ok("package yel:counter@1.0.0;\ncomponent A {}");
        let ast::ItemKind::Package(pkg) = &p.items()[0] else {
            panic!("expected package")
        };
        assert_eq!(p.name(pkg.namespace.present().unwrap().name), "yel");
        assert_eq!(p.name(pkg.name.present().unwrap().name), "counter");
        assert_eq!(p.name(pkg.version.unwrap()), "1.0.0");
    }

    #[test]
    fn parse_package_without_version() {
        let p = parse_ok("package my-namespace:my-package;\ncomponent A {}");
        let ast::ItemKind::Package(pkg) = &p.items()[0] else {
            panic!("expected package")
        };
        assert_eq!(
            p.name(pkg.namespace.present().unwrap().name),
            "my-namespace"
        );
        assert!(pkg.version.is_none());
    }

    #[test]
    fn parse_record_decl() {
        let p = parse_ok("record R {\n  field-a: list<s32>,\n  b: string,\n}");
        let ast::ItemKind::Record(r) = &p.items()[0] else {
            panic!("expected record")
        };
        assert_eq!(p.name(r.name.present().unwrap().name), "R");
        assert_eq!(r.fields.len(), 2);
        let first = r.present_fields().next().unwrap();
        assert_eq!(p.name(first.name.present().unwrap().name), "field-a");
    }

    #[test]
    fn parse_enum_decl() {
        let p = parse_ok("enum Status { case-a, case-b, case-c }");
        let ast::ItemKind::Enum(e) = &p.items()[0] else {
            panic!("expected enum")
        };
        assert_eq!(e.cases.len(), 3);
        assert_eq!(p.name(e.cases[2].present().unwrap().name), "case-c");
    }

    #[test]
    fn parse_enum_rejects_uppercase_case() {
        // `enum_case` is lowercase kebab; `Foo` never matched in the frozen
        // grammar either.
        parse_err("enum E { Foo }");
    }

    #[test]
    fn parse_variant_decl() {
        let p = parse_ok("variant M { kind-a, kind-b(list<string>), kind-c(s32), }");
        let ast::ItemKind::Variant(v) = &p.items()[0] else {
            panic!("expected variant")
        };
        assert_eq!(v.cases.len(), 3);
        assert!(v.cases[0].present().unwrap().payload.is_none());
        assert!(v.cases[1].present().unwrap().payload.is_some());
    }

    #[test]
    fn parse_element_decl() {
        let p = parse_ok("element HStack { alignment: option<Alignment>; gap: s32; }");
        let ast::ItemKind::Element(e) = &p.items()[0] else {
            panic!("expected element")
        };
        assert_eq!(e.properties().count(), 2);
    }

    #[test]
    fn parse_extern_component() {
        let p = parse_ok(
            "extern component Dialog { name: string; func show(a: s32) -> bool; @children }",
        );
        let ast::ItemKind::ExternComponent(ec) = &p.items()[0] else {
            panic!("expected extern component")
        };
        assert_eq!(ec.properties().count(), 1);
        assert_eq!(ec.methods().count(), 1);
        assert!(ec.children_slot().is_some());
    }

    #[test]
    fn parse_global_decl() {
        let p = parse_ok(
            "export global Theme {\n  in dark-mode: bool;\n  out ratio: f32 = 1.0;\n  \
             toggle: func(on: bool);\n  callback legacy(a: s32) -> s32;\n}",
        );
        let ast::ItemKind::Global(g) = &p.items()[0] else {
            panic!("expected global")
        };
        assert!(g.is_export);
        let properties: Vec<_> = g.properties().collect();
        assert_eq!(properties.len(), 2);
        assert_eq!(properties[0].direction, Some(ast::PropertyDirection::In));
        assert_eq!(properties[1].direction, Some(ast::PropertyDirection::Out));
        assert_eq!(g.callbacks().count(), 2);
        // Source order is preserved on the one spine.
        assert!(matches!(g.members[0], ast::GlobalMember::Property(_)));
        assert!(matches!(g.members[2], ast::GlobalMember::Callback(_)));
    }

    #[test]
    fn parse_global_in_out_direction() {
        let p = parse_ok("global S { in-out count: s32; }");
        let ast::ItemKind::Global(g) = &p.items()[0] else {
            panic!("expected global")
        };
        assert_eq!(
            g.properties().next().unwrap().direction,
            Some(ast::PropertyDirection::InOut)
        );
    }

    #[test]
    fn parse_component_properties_and_functions() {
        let p = parse_ok(
            "export component App {\n  count: s32 = 0;\n  on-click: func(a: s32);\n  \
             export on-change: func() -> s32;\n}",
        );
        let c = p.component(0);
        assert!(c.is_export);
        // A non-exported `name: func(..)` is a *property* with a func type —
        // `property_decl` precedes `function_decl` in the frozen grammar.
        assert_eq!(c.properties().count(), 2);
        assert_eq!(c.functions().count(), 1);
        assert!(c.functions().next().unwrap().is_export);
    }

    #[test]
    fn parse_multiple_components() {
        let p = parse_ok("component A {} component B {} component C {}");
        assert_eq!(p.items().len(), 3);
    }

    // ==================== types ====================

    #[test]
    fn parse_primitive_types() {
        let p = parse_ok(
            "component T { a: bool; b: s8; c: s64; d: u32; e: f64; f: char; g: string; \
             h: int; i: float; j: length; k: physical-length; l: relative-font-size; \
             m: color; n: brush; o: image; p: easing; q: angle; r: duration; s: percent; }",
        );
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        assert_eq!(properties.len(), 19);
        assert!(matches!(
            properties[0].ty.kind,
            ast::TypeKind::Primitive(ast::PrimitiveType::Bool)
        ));
        // `int` is an alias for `s32`.
        assert!(matches!(
            properties[7].ty.kind,
            ast::TypeKind::Primitive(ast::PrimitiveType::S32)
        ));
    }

    #[test]
    fn parse_compound_types() {
        let p = parse_ok(
            "component T { a: list<list<s32>>; b: option<string>; c: result<s32, string>; \
             d: result<string>; e: result; f: tuple<s32, string, bool>; g: MyRecord; \
             h: func(x: s32) -> bool; }",
        );
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        assert!(matches!(properties[0].ty.kind, ast::TypeKind::List(_)));
        assert!(matches!(properties[1].ty.kind, ast::TypeKind::Option(_)));
        match &properties[4].ty.kind {
            ast::TypeKind::Result { args } => assert!(args.is_empty(), "bare `result`"),
            _ => panic!("expected result"),
        }
        assert!(matches!(properties[6].ty.kind, ast::TypeKind::Named(_)));
        assert!(matches!(properties[7].ty.kind, ast::TypeKind::Func(_)));
    }

    // ==================== ui nodes ====================

    #[test]
    fn parse_element_node_with_content() {
        let p = parse_ok("component A { div { class: \"row\", span { \"hi\" }, \"tail\" } }");
        let c = p.component(0);
        let ast::UiNode::Element(div) = c.body().next().unwrap() else {
            panic!("expected element")
        };
        assert_eq!(div.props.len(), 1);
        assert_eq!(div.children.len(), 2);
    }

    #[test]
    fn parse_element_content_commas_are_optional() {
        parse_ok("component A { div { span { \"a\" } span { \"b\" } } }");
        parse_ok("component A { div { span { \"a\" }, span { \"b\" }, } }");
    }

    #[test]
    fn parse_for_over_ranges() {
        parse_ok("component A { for i in 0..5 { \"x\" } }");
        parse_ok("component A { for i in 0..=10 { \"x\" } }");
    }

    // ==================== expressions ====================

    #[test]
    fn parse_arithmetic_precedence() {
        let p = parse_ok("component A { x: s32 = 1 + 2 * 3; }");
        let c = p.component(0);
        let default = c.properties().next().unwrap().default.as_ref().unwrap();
        let ast::ExprKind::Binary { op, rhs, .. } = &default.kind else {
            panic!("expected binary")
        };
        assert_eq!(*op, ast::BinaryOp::Add);
        assert!(matches!(
            rhs.kind,
            ast::ExprKind::Binary {
                op: ast::BinaryOp::Mul,
                ..
            }
        ));
    }

    #[test]
    fn parse_comparison_and_logic() {
        parse_ok("component A { a: bool = 1 < 2 && 3 >= 4 || 5 == 6 && 7 != 8; b: bool = !true; }");
    }

    #[test]
    fn parse_unary_minus_is_not_a_signed_literal() {
        let p = parse_ok("component A { x: s32 = -42; }");
        let c = p.component(0);
        assert!(matches!(
            c.properties()
                .next()
                .unwrap()
                .default
                .as_ref()
                .unwrap()
                .kind,
            ast::ExprKind::Unary {
                op: ast::UnaryOp::Neg,
                ..
            }
        ));
    }

    #[test]
    fn parse_hyphenated_identifier_is_one_name() {
        // `count-1` is a single identifier: `-` is an identifier character.
        let p = parse_ok("component A { x: s32 = count-1; }");
        let c = p.component(0);
        let ast::ExprKind::Ident(name) = c
            .properties()
            .next()
            .unwrap()
            .default
            .as_ref()
            .unwrap()
            .kind
        else {
            panic!("expected an identifier, not a subtraction")
        };
        assert_eq!(p.name(name), "count-1");
    }

    #[test]
    fn parse_ternary_is_right_associative() {
        let p = parse_ok("component A { x: s32 = a ? b : c ? d : e; }");
        let c = p.component(0);
        let default = c.properties().next().unwrap().default.as_ref().unwrap();
        let ast::ExprKind::Ternary { else_expr, .. } = &default.kind else {
            panic!("expected ternary")
        };
        assert!(matches!(else_expr.kind, ast::ExprKind::Ternary { .. }));
    }

    #[test]
    fn parse_postfix_chain() {
        parse_ok("component A { x: s32 = a.b[0].c(1, 2)?.d; }");
    }

    #[test]
    fn parse_call_forms() {
        let p = parse_ok("component A { x: s32 = f(1); y: s32 = M.case(2); }");
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        assert!(matches!(
            properties[0].default.as_ref().unwrap().kind,
            ast::ExprKind::Call { .. }
        ));
        assert!(matches!(
            properties[1].default.as_ref().unwrap().kind,
            ast::ExprKind::PathCall { .. }
        ));
    }

    #[test]
    fn parse_invalid_call_base_is_rejected() {
        // Only identifiers and member expressions can be called.
        parse_err("component A { x: s32 = (1)(2); }");
    }

    #[test]
    fn parse_literals() {
        let p = parse_ok(
            "component A { a: s32 = 42; b: f64 = 1.5; c: length = 8px; d: duration = 100ms; \
             e: angle = 45deg; f: percent = 50%; g: color = #ff0000; h: char = 'x'; \
             i: bool = true; j: string = \"s\"; k: list<s32> = [1, 2, 3,]; \
             l: tuple<s32, s32> = (1, 2); }",
        );
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        assert!(matches!(
            properties[0].default.as_ref().unwrap().kind,
            ast::ExprKind::Int(42)
        ));
        assert!(matches!(
            properties[2].default.as_ref().unwrap().kind,
            ast::ExprKind::Unit { .. }
        ));
        assert!(matches!(
            properties[7].default.as_ref().unwrap().kind,
            ast::ExprKind::Char('x')
        ));
        assert!(matches!(
            properties[10].default.as_ref().unwrap().kind,
            ast::ExprKind::List(_)
        ));
        assert!(matches!(
            properties[11].default.as_ref().unwrap().kind,
            ast::ExprKind::Tuple(_)
        ));
    }

    #[test]
    fn parse_escape_sequences_in_char_literals() {
        let p = parse_ok("component A { a: char = '\\n'; b: char = '\\\\'; }");
        let c = p.component(0);
        assert!(matches!(
            c.properties()
                .next()
                .unwrap()
                .default
                .as_ref()
                .unwrap()
                .kind,
            ast::ExprKind::Char('\n')
        ));
    }

    #[test]
    fn parse_one_tuple_needs_a_comma() {
        let p = parse_ok("component A { a: tuple<s32> = (1,); b: s32 = (1); }");
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        assert!(matches!(
            properties[0].default.as_ref().unwrap().kind,
            ast::ExprKind::Tuple(_)
        ));
        // `(1)` is a parenthesised expression, not a 1-tuple.
        assert!(matches!(
            properties[1].default.as_ref().unwrap().kind,
            ast::ExprKind::Int(1)
        ));
    }

    #[test]
    fn parse_nested_string_in_interpolation() {
        parse_ok("component A { div { \"{ [\"a\", \"b\"][1] }\" } }");
    }

    // ==================== recovery ====================

    #[test]
    fn parse_possessive_optional_modifiers() {
        // PEG's `?` does not backtrack: once `prop_modifier` matches `set`, the
        // named_prop alternative is committed and `set: 5` cannot fall back to
        // being a property called `set`.
        parse_err("component A { div { set: 5 } }");
        // But `export` is just a name when it is followed by `:`, because
        // `property_decl` is the *earlier* alternative and matches it.
        let p = parse_ok("component A { export: s32 = 0; }");
        let c = p.component(0);
        assert_eq!(c.properties().count(), 1);
        assert_eq!(c.functions().count(), 0);
        let name = c.properties().next().unwrap().name.present().unwrap().name;
        assert_eq!(p.name(name), "export");
    }

    #[test]
    fn parse_error_node_for_garbage_item() {
        let p = parse_err("!!! garbage\ncomponent A {}");
        assert!(p.errors() >= 1);
        // Recovery resynchronises: the component after the garbage still parses.
        assert!(
            p.items()
                .iter()
                .any(|i| matches!(i, ast::ItemKind::Component(_)))
        );
    }

    #[test]
    fn parse_unterminated_element_block() {
        parse_err("component A { VStack { Text { \"ok\"\n}");
    }

    #[test]
    fn parse_recovers_inside_a_component_body() {
        let p = parse_err("component A { @@@ ;;; div { \"x\" } }");
        assert!(p.errors() >= 1);
        let c = p.component(0);
        assert!(
            c.body().any(|n| matches!(n, ast::UiNode::Element(_))),
            "recovery should still find the div"
        );
    }

    #[test]
    fn parse_never_panics_on_truncations() {
        let source = "package a:b@1.0.0;\nrecord R { f: list<s32>, }\n\
                      export component App {\n  c: s32 = 0;\n  \
                      div { on-click: { c += 1; }, \"v={c}\" }\n}\n";
        for cut in 0..=source.len() {
            if !source.is_char_boundary(cut) {
                continue;
            }
            let prefix = &source[..cut];
            let interner = Interner::new();
            let mut diags = Diagnostics::new();
            let parsed = crate::parse(SourceId(0), prefix, &interner, &mut diags);
            assert_eq!(
                parsed.green.text(),
                prefix,
                "truncation at {cut} lost bytes"
            );
        }
    }

    #[test]
    fn parse_trailing_comma_rules_match_the_grammar() {
        // Allowed: record fields, enum/variant cases, list/tuple/record literals.
        parse_ok("record R { a: s32, }");
        parse_ok("enum E { a, }");
        parse_ok("component A { x: list<s32> = [1,]; }");
        // Forbidden: function parameters, call arguments, type lists.
        parse_err("component A { f: func(a: s32,); }");
        parse_err("component A { x: s32 = f(1,); }");
        parse_err("component A { x: tuple<s32,>; }");
    }

    #[test]
    fn node_ids_are_unique_and_start_at_zero() {
        let p = parse_ok("component A { x: s32 = 1; div { \"a\" } }");
        struct Ids(Vec<u32>);
        impl crate::ast::visit::Visitor for Ids {
            fn visit_ident(&mut self, node: &ast::Ident) {
                self.0.push(node.id.0);
            }
            fn visit_expr(&mut self, node: &ast::Expr) {
                self.0.push(node.id.0);
                crate::ast::visit::walk_expr(self, node);
            }
        }
        let mut ids = Ids(Vec::new());
        crate::ast::visit::Visitor::visit_file(&mut ids, &p.file.ast);
        let mut sorted = ids.0.clone();
        sorted.sort_unstable();
        sorted.dedup();
        assert_eq!(sorted.len(), ids.0.len(), "NodeIds must be unique");
        assert_eq!(p.file.ast.id.0, 0, "the file node is allocated first");
    }

    // ==================== machinery ====================

    /// A missing **token** is marked without corrupting anybody's arity.
    ///
    /// The predecessor drained these holes into whichever list closed first, as
    /// a real element, at an index chosen by drain timing. Every case below is
    /// one it got wrong; the counts are the number of things the user *wrote*.
    #[test]
    fn a_missing_token_is_a_recovery_mark_and_not_a_list_element() {
        let p = parse_err("record R { a: list<s32 }");
        assert!(p.errors() >= 1, "the missing `>` must be marked");
        let ast::ItemKind::Record(record) = &p.items()[0] else {
            panic!("expected a record")
        };
        assert_eq!(record.fields.len(), 1, "one field was written");
        assert_eq!(p.file.ast.recovery_marks.len(), 1);

        // Two parameters written, one of them missing its `:`.
        let p = parse_err("component A { f: func(a list<s32>, b: s32); }");
        let ast::TypeKind::Func(signature) = &p
            .component(0)
            .properties()
            .next()
            .expect("a property")
            .ty
            .kind
        else {
            panic!("expected a func type")
        };
        assert_eq!(signature.present_params().count(), 2);

        // Two tuple elements written; the recovery must not appear *between*
        // them, which is what `tuple<S32, ERR, String>` used to be.
        let p = parse_err("component A { f: func(a tuple<s32, string>); }");
        let ast::TypeKind::Func(signature) = &p
            .component(0)
            .properties()
            .next()
            .expect("a property")
            .ty
            .kind
        else {
            panic!("expected a func type")
        };
        let param = signature.present_params().next().expect("a parameter");
        let ast::TypeKind::Tuple(items) = &param.ty.kind else {
            panic!("expected a tuple type")
        };
        assert_eq!(items.len(), 2, "two element types were written");
        assert!(
            items
                .iter()
                .all(|item| !matches!(item.kind, ast::TypeKind::Error))
        );
    }

    /// Recovery marks arrive in source order, like every other sibling.
    #[test]
    fn recovery_marks_are_sorted_by_span() {
        let p = parse_err("record R { a: list<s32, b: option<u8 }");
        let starts: Vec<usize> = p
            .file
            .ast
            .recovery_marks
            .iter()
            .map(|mark| mark.span.start)
            .collect();
        assert!(starts.len() >= 2, "both `>`s are missing: {starts:?}");
        assert!(
            starts.windows(2).all(|pair| pair[0] <= pair[1]),
            "marks are not in source order: {starts:?}"
        );
    }

    #[test]
    fn the_depth_guard_reports_once_and_still_returns() {
        let source = format!(
            "component A {{ x: s32 = {}1; }}",
            "(".repeat(MAX_NESTING_DEPTH * 4)
        );
        let parsed = check(&source);
        assert!(parsed.diags.has_errors());
        assert!(parsed.errors() >= 1, "the depth limit must be marked");
        assert_eq!(parsed.file.green.text(), source);
    }

    #[test]
    fn measure_max_depth_tracks_the_guarded_entry_points() {
        // `x: s32 = 1;` costs parse_type + parse_expr + parse_unary.
        let shallow = measure_max_depth("component A { x: s32 = 1; }");
        let deeper = measure_max_depth("component A { x: s32 = ((((1)))); }");
        assert!(shallow > 0);
        assert!(
            deeper > shallow,
            "nesting must move the needle: {shallow} vs {deeper}"
        );
        assert!(measure_max_depth("") == 0);
    }

    #[test]
    fn the_depth_guard_is_never_reached_by_ordinary_input() {
        let deepest = measure_max_depth(
            "component A { div { if a { for i in 0..(1 + (2 * (3 - 4))) { \"x\" } } } }",
        );
        assert!(
            deepest * 4 <= MAX_NESTING_DEPTH,
            "an ordinary program reached depth {deepest}"
        );
    }

    /// `MAX_NESTING_DEPTH` bounds recursion in `parse_*`. It does **not** bound
    /// the depth of the tree those functions build, and the gap is not small:
    /// `parse_binary` and `parse_postfix` are loops that enter and leave nesting
    /// once per operand, so a flat chain nests one node per link while the
    /// counter reads 2 (anti-spec A11).
    ///
    /// Every consumer — the walk, the `Drop` glue, `green.text()` — recurses
    /// over the *tree*, so this is the number that has to be measured.
    #[test]
    fn the_depth_guard_does_not_bound_the_tree_it_builds() {
        let source = format!("component A {{ x: s32 = a{}; }}", ".b".repeat(400));
        let parsed = check(&source);
        assert!(
            measure_max_depth(&source) < 8,
            "the parser's own recursion counter stays flat on a flat chain"
        );
        assert!(
            parsed.file.green.max_depth() > 400,
            "…while the green tree it built is 400+ levels deep, at {}",
            parsed.file.green.max_depth()
        );
    }

    #[test]
    fn bracket_matching_is_computed_once_and_survives_mismatches() {
        let table = bracket_close_table(&[L_PAREN, L_BRACKET, R_BRACKET, R_PAREN]);
        assert_eq!(table[0], 3);
        assert_eq!(table[1], 2);

        // Unmatched openers point past the end, so a bounded scan stops at EOF.
        let table = bracket_close_table(&[L_BRACE, L_BRACE, IDENTIFIER]);
        assert_eq!(table[0], 3);
        assert_eq!(table[1], 3);

        // `TEMPLATE_MIDDLE_LITERAL` is neutral.
        let table = bracket_close_table(&[
            TEMPLATE_LITERAL,
            IDENTIFIER,
            TEMPLATE_MIDDLE_LITERAL,
            IDENTIFIER,
            TEMPLATE_END_LITERAL,
        ]);
        assert_eq!(table[0], 4);
    }
}

#[cfg(test)]
mod spec_perf {
    use super::*;

    /// Brace dispatch must not go super-linear.
    ///
    /// # Why this shape and why warm-up
    ///
    /// The predecessor of this test timed **one un-warmed sample per point** and
    /// reported `nest=2` at 341 µs — process start-up, not parse time — which
    /// made a cubic curve look exponential and put a false claim in
    /// `exprs.rs`. It also swept only 2..12, a range over which n³ and 1.265ⁿ
    /// differ by under 2×, and used `{ a: { a: … } }`, the shape *least*
    /// sensitive to the decision being guarded.
    ///
    /// So: warm up, take a median of repeats, sweep far enough to separate the
    /// curves, and use `{ lets: X = 1; }` — the one shape review could not make
    /// linear without `shallow_marks.semicolon`, i.e. the case the machinery
    /// actually exists for.
    fn median_parse_micros(src: &str, reps: u32) -> u128 {
        let interner = Interner::new();
        for _ in 0..3 {
            let mut warm = Diagnostics::new();
            let _ = crate::parse(SourceId(0), src, &interner, &mut warm);
        }
        let mut samples: Vec<u128> = (0..reps)
            .map(|_| {
                let mut diags = Diagnostics::new();
                let t = std::time::Instant::now();
                let _ = crate::parse(SourceId(0), src, &interner, &mut diags);
                t.elapsed().as_micros()
            })
            .collect();
        samples.sort_unstable();
        samples[samples.len() / 2]
    }

    fn nested_semicolon_block(n: usize) -> String {
        let mut body = String::from("{ lets: s32 = 1; }");
        for _ in 0..n {
            body = format!("{{ lets: s32 = {body}; }}");
        }
        format!("package a:b@0.1.0;\ncomponent A {{ x: s32 = {body}; }}")
    }

    #[test]
    fn brace_dispatch_stays_linear() {
        let (small, large) = (16usize, 128usize);
        let t_small = median_parse_micros(&nested_semicolon_block(small), 25).max(1);
        let t_large = median_parse_micros(&nested_semicolon_block(large), 25).max(1);

        let size_ratio = large as f64 / small as f64;
        let time_ratio = t_large as f64 / t_small as f64;
        let exponent = time_ratio.ln() / size_ratio.ln();

        println!("n={small}: {t_small}µs  n={large}: {t_large}µs  exponent={exponent:.2}");
        assert!(
            exponent < 1.6,
            "brace dispatch is growing as n^{exponent:.2} ({t_small}µs → {t_large}µs \
             across {small} → {large} levels). Lookahead makes this ~linear; a \
             speculative rewrite of `classify_brace` makes it cubic. See the note \
             in exprs.rs."
        );
    }
}
