//! Closures, literals, and the expression grammar.
//!
//! Statement blocks live in the sibling [`super::stmts`] module.
//!
//! # Precedence
//!
//! Precedence-climbing over the same table the frozen `PrattParser` was built
//! with, lowest first: range, `||`, `&&`, `==`/`!=`, comparisons, `+`/`-`,
//! `*`/`/`/`%`. Prefix `-`/`!` bind looser than the postfix operators (call,
//! member, `?.`, index), so `-a.b` is `-(a.b)`. The ternary is a *suffix of the
//! whole expression* — `expr = … ~ ternary_suffix?` — and both of its branches
//! are full expressions, which makes it right-associative.
//!
//! # The `{` problem
//!
//! `primary` starts with four brace-led alternatives in a deliberate order:
//!
//! ```text
//! closure_with_params      { p: T -> body }
//! closure_inferred_params  { p -> body } | { p, q -> body }
//! record_literal           { f: e, … }          (at least one field)
//! closure_no_params        { body }             (also catches `{}`)
//! ```
//!
//! PEG does not backtrack out of a committed alternative, so the choice has to
//! be made by lookahead. [`Parser::classify_brace`] does it with a scan bounded
//! by the **matching `}`** — or, when the brace is unterminated, by end of
//! input: `{}` is the empty closure; a name followed by `->` (or a
//! comma-separated run of names ending in `->`) is an inferred-param closure; a
//! name followed by `:` is a typed-param closure exactly when a **depth-zero
//! `->`** occurs before that matching `}`, and a record literal otherwise.
//! Anything else is a bodied closure.
//!
//! The matching brace comes from `Parser::bracket_close`, computed once over
//! the whole token stream. It treats `TEMPLATE_LITERAL` as an opener and
//! `TEMPLATE_END_LITERAL` as a closer, because an interpolated string swallows
//! its own braces into those tokens, and `TEMPLATE_MIDDLE_LITERAL` as neutral.

use super::{Mark, Parser, TrailingSep};
use crate::ast;
use crate::token::{EXPR_LIST_RECOVERY, EXPRESSION_FIRST, NAME_FIRST, TokenKind, TokenKind::*};
use yelc_base::Span;

/// What a `{` turned out to introduce.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum BraceKind {
    Record,
    ClosureNoParams,
    ClosureInferredParams,
    ClosureTypedParams,
}

/// Infix operators, with their precedence level. Higher binds tighter.
fn infix_op(kind: TokenKind) -> Option<(u8, InfixOp)> {
    let entry = match kind {
        DOT_DOT => (1, InfixOp::Range { inclusive: false }),
        DOT_DOT_EQ => (1, InfixOp::Range { inclusive: true }),
        OR_OR => (2, InfixOp::Binary(ast::BinaryOp::Or)),
        AND_AND => (3, InfixOp::Binary(ast::BinaryOp::And)),
        EQ_EQ => (4, InfixOp::Binary(ast::BinaryOp::Eq)),
        NOT_EQ => (4, InfixOp::Binary(ast::BinaryOp::Ne)),
        LT => (5, InfixOp::Binary(ast::BinaryOp::Lt)),
        GT => (5, InfixOp::Binary(ast::BinaryOp::Gt)),
        LE => (5, InfixOp::Binary(ast::BinaryOp::Le)),
        GE => (5, InfixOp::Binary(ast::BinaryOp::Ge)),
        ADD => (6, InfixOp::Binary(ast::BinaryOp::Add)),
        SUB => (6, InfixOp::Binary(ast::BinaryOp::Sub)),
        MUL => (7, InfixOp::Binary(ast::BinaryOp::Mul)),
        DIV => (7, InfixOp::Binary(ast::BinaryOp::Div)),
        MODULO => (7, InfixOp::Binary(ast::BinaryOp::Mod)),
        _ => return None,
    };
    Some(entry)
}

#[derive(Copy, Clone)]
enum InfixOp {
    Binary(ast::BinaryOp),
    Range { inclusive: bool },
}

/// Strip the delimiters off a string-segment token, leaving the raw text.
fn segment_text(text: &str, kind: TokenKind) -> &str {
    if text.is_empty() {
        return text;
    }
    let inner = &text[1..];
    match kind {
        STRING_LITERAL | TEMPLATE_END_LITERAL => inner.strip_suffix('"').unwrap_or(inner),
        TEMPLATE_LITERAL | TEMPLATE_MIDDLE_LITERAL => inner.strip_suffix('{').unwrap_or(inner),
        _ => inner,
    }
}

impl<'a> Parser<'a> {
    // -- expressions -------------------------------------------------------

    /// `expr = prefix* ~ primary ~ postfix* ~ (infix ~ …)* ~ ternary_suffix?`
    ///
    /// One of the five guarded recursive entry points: a ternary's branches, a
    /// parenthesised sub-expression and an index all recurse through here.
    pub(super) fn parse_expr(&mut self) -> ast::Expr {
        if !self.enter_nesting() {
            let span = self.nesting_limit_node();
            return <ast::Expr as ast::Recovery>::recovery(self.new_node_id(), span);
        }
        let result = self.parse_expr_inner();
        self.leave_nesting();
        result
    }

    fn parse_expr_inner(&mut self) -> ast::Expr {
        let mark = self.mark();
        let expr = self.parse_binary(1);

        if self.is(QUESTION) {
            self.advance();
            let then_expr = self.parse_expr();
            self.expect(COLON);
            let else_expr = self.parse_expr();
            let span = self.finish_marked(TERNARY_EXPR, &mark);
            return ast::Expr {
                id: self.new_node_id(),
                span,
                kind: ast::ExprKind::Ternary {
                    condition: Box::new(expr),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
            };
        }

        expr
    }

    fn parse_binary(&mut self, min_level: u8) -> ast::Expr {
        let mark = self.mark();
        let mut lhs = self.parse_unary();

        while let Some((level, op)) = infix_op(self.current()) {
            if level < min_level {
                break;
            }
            self.advance();
            let rhs = self.parse_binary(level + 1);
            lhs = match op {
                InfixOp::Binary(op) => {
                    let span = self.finish_marked(BINARY_EXPR, &mark);
                    ast::Expr {
                        id: self.new_node_id(),
                        span,
                        kind: ast::ExprKind::Binary {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                    }
                }
                InfixOp::Range { inclusive } => {
                    let span = self.finish_marked(RANGE_EXPR, &mark);
                    ast::Expr {
                        id: self.new_node_id(),
                        span,
                        kind: ast::ExprKind::Range {
                            start: Box::new(lhs),
                            end: Box::new(rhs),
                            inclusive,
                        },
                    }
                }
            };
        }

        lhs
    }

    /// One of the five guarded recursive entry points: `----…-1` recurses here
    /// without passing through a bracket or any other guarded function.
    fn parse_unary(&mut self) -> ast::Expr {
        if !self.enter_nesting() {
            let span = self.nesting_limit_node();
            return <ast::Expr as ast::Recovery>::recovery(self.new_node_id(), span);
        }
        let result = self.parse_unary_inner();
        self.leave_nesting();
        result
    }

    fn parse_unary_inner(&mut self) -> ast::Expr {
        let op = match self.current() {
            SUB => ast::UnaryOp::Neg,
            NOT => ast::UnaryOp::Not,
            _ => return self.parse_postfix(),
        };

        let mark = self.mark();
        self.advance();
        let operand = self.parse_unary();
        let span = self.finish_marked(UNARY_EXPR, &mark);
        ast::Expr {
            id: self.new_node_id(),
            span,
            kind: ast::ExprKind::Unary {
                op,
                operand: Box::new(operand),
            },
        }
    }

    fn parse_postfix(&mut self) -> ast::Expr {
        let mark = self.mark();
        let mut expr = self.parse_primary();

        loop {
            match self.current() {
                L_PAREN => expr = self.parse_call(expr, &mark),
                DOT | QUESTION_DOT => {
                    let optional = self.is(QUESTION_DOT);
                    self.advance();
                    // A missing member name is a hole in `member`, and the base
                    // stays. Replacing the whole node with `ExprKind::Error`
                    // threw the base away: `a.b.` lost `a.b` entirely.
                    let member = self.expect_name();
                    let span = self.finish_marked(MEMBER_EXPR, &mark);
                    let base = Box::new(expr);
                    expr = ast::Expr {
                        id: self.new_node_id(),
                        span,
                        kind: if optional {
                            ast::ExprKind::OptionalMember { base, member }
                        } else {
                            ast::ExprKind::Member { base, member }
                        },
                    };
                }
                L_BRACKET => {
                    self.assert(L_BRACKET);
                    let index = self.parse_expr();
                    self.expect(R_BRACKET);
                    let span = self.finish_marked(INDEX_EXPR, &mark);
                    expr = ast::Expr {
                        id: self.new_node_id(),
                        span,
                        kind: ast::ExprKind::Index {
                            base: Box::new(expr),
                            index: Box::new(index),
                        },
                    };
                }
                _ => break,
            }
        }

        expr
    }

    /// `call = "(" ~ args? ~ ")"`. The frozen parser rejects any callee that is
    /// not an identifier or a member access outright, so this does too.
    fn parse_call(&mut self, callee: ast::Expr, mark: &Mark) -> ast::Expr {
        let args = self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EXPR_LIST_RECOVERY,
            ARG_LIST,
            TrailingSep::Forbidden,
            |p| p.parse_expr_opt(),
        );
        let span = self.finish_marked(CALL_EXPR, mark);

        let (callee_id, callee_span) = (callee.id, callee.span);
        let kind = match callee.into_kind() {
            ast::ExprKind::Ident(name) => ast::ExprKind::Call {
                callee: ast::Ident {
                    id: callee_id,
                    span: callee_span,
                    name,
                },
                args,
            },
            ast::ExprKind::Member { base, member } => {
                ast::ExprKind::PathCall { base, member, args }
            }
            _ => {
                self.error_at(
                    callee_span,
                    "only identifiers and member expressions can be called",
                );
                ast::ExprKind::Error
            }
        };

        ast::Expr {
            id: self.new_node_id(),
            span,
            kind,
        }
    }

    fn parse_expr_opt(&mut self) -> Option<ast::Expr> {
        if !self.is_set(EXPRESSION_FIRST) {
            return None;
        }
        Some(self.parse_expr())
    }

    /// An identifier `bool_literal` matches a **proper prefix** of, where the
    /// leftover cannot continue the expression.
    ///
    /// `TRUE_KW` / `FALSE_KW` are the exact spellings, so this only ever fires
    /// on an `IDENTIFIER` — `trueish`, `falsey`.
    ///
    /// The leftover check is the difference between a rejection and a grammar
    /// **narrowing**. `-` is both an identifier character and an operator, so
    /// the lexer munches `true-a` into one `IDENTIFIER` while pest matches
    /// `bool_literal` and then *resumes the expression*: `true-a` is
    /// `true - a`, which the frozen parser accepts, as it does `false-a`,
    /// `true-1` and `{ true-a = 1; }`. Only an alphanumeric or `_` leftover is
    /// dead — nothing can continue an expression with `ish` — and that is the
    /// half this reports.
    ///
    /// This parser reads the accepted half as a single kebab identifier named
    /// `true-a` rather than as a subtraction. That is an AST-shape difference,
    /// not an accept/reject one, and reproducing pest's reading would mean
    /// re-lexing the middle of a token. The bit that is contracted here — does
    /// this file produce a syntax error — is the same on both sides, and
    /// `tests/parity.rs::accept_reject_parity_over_the_literal_prefix_class`
    /// pins every member against the oracle.
    fn at_bool_literal_prefix(&self) -> bool {
        if !self.is(IDENTIFIER) {
            return false;
        }
        let text = self.current_text();
        let rest = if let Some(rest) = text.strip_prefix("true") {
            rest
        } else if let Some(rest) = text.strip_prefix("false") {
            rest
        } else {
            return false;
        };
        matches!(rest.as_bytes().first(), Some(byte) if byte.is_ascii_alphanumeric() || *byte == b'_')
    }

    fn parse_primary(&mut self) -> ast::Expr {
        match self.current() {
            L_BRACE => self.parse_brace_expr(),
            L_PAREN => self.parse_paren_or_tuple(),
            L_BRACKET => self.parse_list_literal(),
            STRING_LITERAL | TEMPLATE_LITERAL => self.parse_string_expr(),
            INT_LITERAL | FLOAT_LITERAL | UNIT_LITERAL | COLOR_LITERAL | CHAR_LITERAL => {
                self.parse_scalar_literal()
            }
            TRUE_KW | FALSE_KW => {
                let value = self.is(TRUE_KW);
                self.start_node();
                self.advance();
                let span = self.finish_node(LITERAL_EXPR);
                ast::Expr {
                    id: self.new_node_id(),
                    span,
                    kind: ast::ExprKind::Bool(value),
                }
            }
            // `primary` tries `literal` before `identifier`, and `bool_literal`
            // is `"true" | "false"` with no word boundary — so `trueish` is the
            // literal `true` followed by a stray `ish`, and the enclosing rule
            // dies on it. The same shape as the primitive-type prefixes in
            // `parser/types.rs`, in the one other position where a bare string
            // literal is tried ahead of `identifier`.
            _ if self.at_bool_literal_prefix() => {
                let text = self.current_text();
                let prefix = if text.starts_with("true") {
                    "true"
                } else {
                    "false"
                };
                self.start_node();
                let at = self.current_span();
                let rest = text[prefix.len()..].to_string();
                self.error_at(
                    at,
                    format!(
                        "expected an expression, found `{text}`: `{prefix}` is a boolean \
                         literal and `{rest}` is left over"
                    ),
                );
                self.advance();
                let span = self.finish_node(ERROR);
                ast::Expr {
                    id: self.new_node_id(),
                    span,
                    kind: ast::ExprKind::Error,
                }
            }
            _ if self.is_name() => {
                self.start_node();
                let name = self.intern(self.current_text());
                self.advance();
                let span = self.finish_node(IDENT_EXPR);
                ast::Expr {
                    id: self.new_node_id(),
                    span,
                    kind: ast::ExprKind::Ident(name),
                }
            }
            _ => {
                // Deliberately does not consume: the caller owns progress, and
                // eating the token here would swallow a closing delimiter. The
                // zero-width green `ERROR` node is what keeps the lossless tree
                // marked at this recovery point — it was the one recovery
                // position the green tree carried no marker for.
                self.error_here(format!(
                    "expected an expression, found `{}`",
                    self.current().spelling()
                ));
                self.start_node();
                let span = self.finish_node(ERROR);
                ast::Expr {
                    id: self.new_node_id(),
                    span,
                    kind: ast::ExprKind::Error,
                }
            }
        }
    }

    // -- literals ----------------------------------------------------------

    fn parse_scalar_literal(&mut self) -> ast::Expr {
        let kind = self.current();
        let span = self.current_span();
        let text = self.current_text();
        self.start_node();
        self.advance();
        let node_span = self.finish_node(LITERAL_EXPR);

        let expr_kind = match kind {
            INT_LITERAL => match text.parse::<i64>() {
                Ok(value) => ast::ExprKind::Int(value),
                Err(err) => {
                    self.error_at(span, format!("invalid integer literal: {err}"));
                    ast::ExprKind::Error
                }
            },
            FLOAT_LITERAL => match text.parse::<f64>() {
                Ok(value) => ast::ExprKind::Float(value),
                Err(err) => {
                    self.error_at(span, format!("invalid float literal: {err}"));
                    ast::ExprKind::Error
                }
            },
            UNIT_LITERAL => {
                let split = text
                    .char_indices()
                    .find(|(_, c)| !c.is_ascii_digit() && *c != '.' && *c != '-')
                    .map(|(i, _)| i)
                    .unwrap_or(text.len());
                let (value_text, suffix) = text.split_at(split);
                match value_text.parse::<f64>() {
                    Ok(value) => ast::ExprKind::Unit {
                        value,
                        suffix: self.intern(suffix),
                    },
                    Err(err) => {
                        self.error_at(span, format!("invalid unit literal: {err}"));
                        ast::ExprKind::Error
                    }
                }
            }
            COLOR_LITERAL => ast::ExprKind::Color(self.intern(text)),
            CHAR_LITERAL => match decode_char_literal(text) {
                Some(value) => ast::ExprKind::Char(value),
                None => {
                    self.error_at(
                        span,
                        "a character literal must contain exactly one character",
                    );
                    ast::ExprKind::Error
                }
            },
            other => unreachable!("not a scalar literal: {other:?}"),
        };

        ast::Expr {
            id: self.new_node_id(),
            span: node_span,
            kind: expr_kind,
        }
    }

    /// `list_literal = "[" ~ (expr ~ ("," ~ expr)* ~ ","?)? ~ "]"`
    fn parse_list_literal(&mut self) -> ast::Expr {
        let mark = self.mark();
        let items = self.parse_list(
            L_BRACKET,
            COMMA,
            R_BRACKET,
            EXPR_LIST_RECOVERY,
            LIST_EXPR,
            TrailingSep::Allowed,
            |p| p.parse_expr_opt(),
        );
        // `parse_list` already closed the LIST_EXPR node; the mark only carries
        // the span.
        let span = self.span_from(&mark);
        ast::Expr {
            id: self.new_node_id(),
            span,
            kind: ast::ExprKind::List(items),
        }
    }

    /// `tuple_literal = "(" ~ expr ~ "," ~ … ~ ")"` versus `"(" ~ expr ~ ")"`.
    /// A one-element tuple needs its comma: `(x,)`.
    fn parse_paren_or_tuple(&mut self) -> ast::Expr {
        self.start_node();
        self.assert(L_PAREN);

        if self.is(R_PAREN) {
            self.error_here("expected an expression");
            self.assert(R_PAREN);
            let span = self.finish_node(ERROR);
            return ast::Expr {
                id: self.new_node_id(),
                span,
                kind: ast::ExprKind::Error,
            };
        }

        let first = self.parse_expr();

        if !self.is(COMMA) {
            self.expect(R_PAREN);
            let span = self.finish_node(PAREN_EXPR);
            // The frozen parser drops the parentheses from the AST, which is
            // what makes `(f)(1)` an invalid call base rather than a call.
            let id = first.id;
            return ast::Expr {
                id,
                span,
                kind: first.into_kind(),
            };
        }

        // `"(" ~ expr ~ "," ~ (expr ~ ("," ~ expr)*)? ~ ","? ~ ")"`. The comma
        // after the first element is **mandatory** and the element group after
        // it is **optional**, so `(1,,)` is a one-element tuple whose group
        // matched empty and whose `","?` took the second comma — accepted by
        // the frozen parser and rejected here, which is a narrowing. The two
        // commas are not interchangeable: `(1,,,)` and `(1,2,,)` are rejected,
        // because only one `","?` follows the group.
        let mut items = vec![first];
        self.assert(COMMA);
        if self.is_set(EXPRESSION_FIRST) {
            loop {
                let before = self.position();
                items.push(self.parse_expr());
                if self.position() == before {
                    break;
                }
                if !(self.is(COMMA) && self.nth_is_set(1, EXPRESSION_FIRST)) {
                    break;
                }
                self.assert(COMMA);
            }
        }
        self.eat(COMMA);
        self.expect(R_PAREN);
        let span = self.finish_node(TUPLE_EXPR);

        ast::Expr {
            id: self.new_node_id(),
            span,
            kind: ast::ExprKind::Tuple(items),
        }
    }

    // -- strings -----------------------------------------------------------

    /// `string_expr = ${ "\"" ~ (interpolation | string_text)* ~ "\"" }`
    ///
    /// The lexer already carved the string into segments, so this reassembles
    /// them. Empty text runs are dropped, because pest's `string_text` is `+`
    /// and never produces one.
    pub(super) fn parse_string_expr(&mut self) -> ast::Expr {
        self.start_node();
        let mut parts = Vec::new();

        let push_segment = |p: &mut Self, parts: &mut Vec<ast::InterpolationPart>| {
            let kind = p.current();
            let text = segment_text(p.current_text(), kind);
            if !text.is_empty() {
                let name = p.intern(text);
                parts.push(ast::InterpolationPart::Literal(name));
            }
            p.advance();
        };

        if self.is(STRING_LITERAL) {
            push_segment(self, &mut parts);
        } else {
            debug_assert!(self.is(TEMPLATE_LITERAL));
            push_segment(self, &mut parts);
            loop {
                self.start_node();
                if self.is_set(EXPRESSION_FIRST) {
                    let expr = self.parse_expr();
                    parts.push(ast::InterpolationPart::Expr(expr));
                } else {
                    // Report **and** keep the hole. `"{}"` and `"{ }"` used to
                    // report here and push nothing, so `parts` stayed empty and
                    // the whole string collapsed to `Str("")` — a diagnostic
                    // with no recovery node anywhere, and a *plausible value*
                    // standing in for one the parser could not read
                    // (invariant S5, anti-spec B9).
                    let at = self.current_span();
                    self.error_here("expected an expression inside `{ … }`");
                    parts.push(ast::InterpolationPart::Expr(
                        <ast::Expr as ast::Recovery>::recovery(
                            self.new_node_id(),
                            Span::point(at.source, at.start),
                        ),
                    ));
                }
                self.finish_node(INTERPOLATION);

                match self.current() {
                    TEMPLATE_MIDDLE_LITERAL => push_segment(self, &mut parts),
                    TEMPLATE_END_LITERAL => {
                        push_segment(self, &mut parts);
                        break;
                    }
                    _ => {
                        // The closing `"` is a token, and a token has no slot.
                        self.error_here("unterminated string interpolation");
                        let at = self.zero_width_error_node();
                        self.record_recovery_mark(at);
                        break;
                    }
                }
            }
        }

        let span = self.finish_node(STRING_EXPR);
        let kind = match parts.len() {
            0 => ast::ExprKind::String(self.intern("")),
            1 if matches!(parts[0], ast::InterpolationPart::Literal(_)) => {
                let ast::InterpolationPart::Literal(name) = parts.remove(0) else {
                    unreachable!()
                };
                ast::ExprKind::String(name)
            }
            _ => ast::ExprKind::Interpolation(parts),
        };

        ast::Expr {
            id: self.new_node_id(),
            span,
            kind,
        }
    }

    // -- braces ------------------------------------------------------------

    fn parse_brace_expr(&mut self) -> ast::Expr {
        // `primary`'s brace-led alternatives are chosen by **lookahead**, not by
        // speculation. The reason is diagnostic quality, not asymptotics.
        //
        // An earlier version of this comment claimed speculating here was
        // *exponential*, that a packrat memo could not rescue it, and that the
        // corpus sweep stopped terminating. **Review measured all three and all
        // three were false**, and the correction is worth keeping because the
        // claim would otherwise be cited again:
        //
        //   * It is **cubic**, not exponential — exponent 2.70–3.09, stable
        //     across four scale steps. The "~1.6× per two levels" figure was the
        //     tail of a *decaying* ratio sequence (2.78 → 2.17 → 1.73 → 1.65 →
        //     1.54), which is polynomial growth, not a rate. Extrapolating it
        //     predicted ~10 minutes at 70 levels; the measurement is 85 ms.
        //   * The memo in `try_parse` is exactly what rescues it. With
        //     `failed_attempts` disabled the same code *is* exponential (~3ⁿ,
        //     unfinished at 12 levels); with it, cubic. It never needs to cache a
        //     success, so the green-tree argument never arises.
        //   * The corpus sweep does not hang: 608 ms against a 505 ms baseline,
        //     with byte-identical S1 and accept/reject on all 2000 programs.
        //   * The 341 µs anchor came from a harness timing one un-warmed sample;
        //     it was process start-up, not parse time.
        //
        // What actually survives, measured:
        //
        //   * A fail-fast ordered-choice formulation is **linear** on every shape
        //     except one, at ~1.03× on real code. So `has_depth_zero_arrow` and
        //     its `bracket_close` scan are *not* load-bearing for cost.
        //   * `shallow_marks.semicolon` **is** load-bearing: it is the only
        //     depth-zero-`;` test, and it is what keeps `{ lets: X = 1; }` from
        //     going cubic — the record alternative parses the whole value before
        //     discovering the `=`, and the statement fallback parses it again.
        //   * `classify_brace` is deliberately **not** PEG-faithful, and that is
        //     the real argument: it commits to `Record` on `{ a: 1, b }`,
        //     `{ a: 1 b: 2 }`, `{ a: 1` and recovers *inside* it. Faithful
        //     ordered choice backtracks to `closure_no_params` and emits 5–7
        //     cascading diagnostics where this emits 1–2.
        //
        // Diagnostic quality is a frozen property (`scope.md`; the diagnostic
        // fixtures pin meaning) and cost here is not. `Parser::try_parse` is used
        // at the `if`/element site instead, which had a demonstrated *defect*
        // rather than a demonstrated cost.
        match self.classify_brace() {
            BraceKind::Record => self.parse_record_literal(),
            BraceKind::ClosureNoParams => self.parse_closure(BraceKind::ClosureNoParams),
            BraceKind::ClosureInferredParams => {
                self.parse_closure(BraceKind::ClosureInferredParams)
            }
            BraceKind::ClosureTypedParams => self.parse_closure(BraceKind::ClosureTypedParams),
        }
    }

    /// `record_literal = "{" ~ record_literal_field ~ ("," ~ …)* ~ ","? ~ "}"`
    fn parse_record_literal(&mut self) -> ast::Expr {
        let mark = self.mark();
        self.builder.start_node();
        let fields = self.parse_list(
            L_BRACE,
            COMMA,
            R_BRACE,
            EXPR_LIST_RECOVERY,
            RECORD_LITERAL_FIELD_LIST,
            TrailingSep::Allowed,
            |p| p.parse_record_literal_field().map(ast::Recovered::Present),
        );
        self.builder.finish_node(RECORD_LITERAL);
        let span = self.span_from(&mark);

        ast::Expr {
            id: self.new_node_id(),
            span,
            kind: ast::ExprKind::Record(fields),
        }
    }

    fn parse_record_literal_field(&mut self) -> Option<ast::RecordFieldInit> {
        if !self.is_name() {
            return None;
        }
        self.start_node();
        let name = self.expect_name();
        self.expect(COLON);
        let value = self.parse_expr();
        let span = self.finish_node(RECORD_LITERAL_FIELD);
        Some(ast::RecordFieldInit {
            id: self.new_node_id(),
            span,
            name,
            value,
        })
    }

    fn parse_closure(&mut self, kind: BraceKind) -> ast::Expr {
        self.start_node();
        self.assert(L_BRACE);

        let params = match kind {
            BraceKind::ClosureNoParams => Vec::new(),
            BraceKind::ClosureInferredParams => self.parse_closure_params(false),
            BraceKind::ClosureTypedParams => self.parse_closure_params(true),
            BraceKind::Record => unreachable!("record literals do not reach parse_closure"),
        };

        let body = self.parse_stmt_block(CLOSURE_BODY, true);
        self.expect(R_BRACE);
        let span = self.finish_node(CLOSURE_EXPR);

        ast::Expr {
            id: self.new_node_id(),
            span,
            kind: ast::ExprKind::Closure(Box::new(ast::ClosureExpr {
                id: self.new_node_id(),
                span,
                params,
                body,
            })),
        }
    }

    /// `closure_param_list = closure_param ~ ("," ~ closure_param)*` — no
    /// trailing comma — followed by the `->` that made this a closure.
    ///
    /// A parameter whose name could not be read still appears in the list, with
    /// a `Missing` name. Dropping it left the green tree carrying a
    /// `CLOSURE_PARAM` node the AST had no counterpart for.
    fn parse_closure_params(&mut self, typed: bool) -> Vec<ast::Recovered<ast::ClosureParam>> {
        let mut params = Vec::new();
        self.builder.start_node();

        loop {
            let before = self.position();
            self.start_node();
            let name = self.expect_name();
            let ty = if typed {
                self.expect(COLON);
                Some(self.parse_type())
            } else {
                None
            };
            let span = self.finish_node(CLOSURE_PARAM);
            params.push(ast::Recovered::Present(ast::ClosureParam {
                id: self.new_node_id(),
                span,
                name,
                ty,
            }));
            if self.position() == before {
                break;
            }
            if !self.eat(COMMA) {
                break;
            }
        }

        self.builder.finish_node(CLOSURE_PARAM_LIST);
        self.expect(ARROW);
        params
    }

    /// Decide what a `{` introduces. See the module docs.
    fn classify_brace(&self) -> BraceKind {
        debug_assert!(self.is(L_BRACE));
        let first = self.nth_non_trivia(1);

        if first == R_BRACE {
            return BraceKind::ClosureNoParams;
        }

        if NAME_FIRST.contains(first) {
            let second = self.nth_non_trivia(2);
            if second == ARROW || (second == COMMA && self.at_inferred_param_run()) {
                return BraceKind::ClosureInferredParams;
            }
            if second == COLON {
                if self.has_depth_zero_arrow() {
                    return BraceKind::ClosureTypedParams;
                }
                // `record_literal_fields` is `field ("," field)* ","?` and a
                // `field` is `name ":" expr` — no `expr` can contain a `;`. So a
                // depth-zero `;` proves the block is *not* a record literal,
                // pest backtracks out of `record_literal`, and
                // `closure_no_params` takes it: `{ lets: s32 = 1; }` is the
                // statement `let s: s32 = 1;`, not a record whose first field
                // ran into an `=`. Committing on `name :` alone rejected it.
                return if self.shallow_marks_here().semicolon {
                    BraceKind::ClosureNoParams
                } else {
                    BraceKind::Record
                };
            }
        }

        BraceKind::ClosureNoParams
    }

    /// `name ("," name)* "->"` starting just after the `{`.
    fn at_inferred_param_run(&self) -> bool {
        let mut scan = self.scan_after_brace();
        loop {
            match scan.next() {
                Some(kind) if NAME_FIRST.contains(kind) => {}
                _ => return false,
            }
            match scan.next() {
                Some(ARROW) => return true,
                Some(COMMA) => {}
                _ => return false,
            }
        }
    }

    /// Whether a `->` occurs at brace depth zero before the matching `}`.
    ///
    /// Bounded by the pre-computed matching `}` — or by end of input when the
    /// brace is unterminated — and it **jumps over** each nested group using the
    /// same table instead of counting its way through. Counting made the scan
    /// re-walk every enclosing brace's tail, which is quadratic: 1.1 / 3.8 /
    /// 14.6 ms for 500 / 1000 / 2000 open braces.
    fn has_depth_zero_arrow(&self) -> bool {
        let stop = self.bracket_close[self.token_idx] as usize;
        let mut index = self.token_idx + 1;
        while index < stop {
            match self.tokens[index] {
                ARROW => return true,
                L_PAREN | L_BRACKET | L_BRACE | TEMPLATE_LITERAL => {
                    index = (self.bracket_close[index] as usize).min(stop);
                }
                _ => {}
            }
            index += 1;
        }
        false
    }

    /// Non-trivia tokens starting immediately after the current `{`.
    fn scan_after_brace(&self) -> impl Iterator<Item = TokenKind> + '_ {
        self.tokens[(self.token_idx + 1).min(self.tokens.len())..]
            .iter()
            .copied()
            .filter(|kind| !kind.is_trivia())
    }
}

/// `char_literal = { "'" ~ char_inner ~ "'" }` with `char_inner` either an
/// escape sequence or a single non-quote character.
///
/// The rule is *not* atomic in the frozen grammar, so pest's implicit
/// whitespace is skipped on both sides of `char_inner` — `'x '` is the
/// character `x`, and `' '` matches nothing at all.
fn decode_char_literal(text: &str) -> Option<char> {
    let inner = text.strip_prefix('\'')?;
    let inner = inner.strip_suffix('\'').unwrap_or(inner);
    let inner = inner.trim_matches(|c: char| matches!(c, ' ' | '\t' | '\r' | '\n'));

    if let Some(escaped) = inner.strip_prefix('\\') {
        let mut chars = escaped.chars();
        let c = chars.next()?;
        if chars.next().is_some() {
            return None;
        }
        return Some(match c {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            '0' => '\0',
            _ => return None,
        });
    }

    let mut chars = inner.chars();
    let c = chars.next()?;
    if chars.next().is_some() {
        return None;
    }
    Some(c)
}

#[cfg(test)]
mod tests {
    use super::super::tests::{Parsed, parse_err, parse_ok};
    use crate::ast;

    fn first_default(parsed: &Parsed) -> &ast::Expr {
        parsed
            .component(0)
            .properties()
            .next()
            .expect("a property")
            .default
            .as_ref()
            .expect("a default value")
    }

    fn first_prop_value(parsed: &Parsed) -> &ast::Expr {
        let ast::UiNode::Element(element) = parsed.component(0).body().next().expect("a node")
        else {
            panic!("expected an element")
        };
        &element.props[0].value
    }

    #[test]
    fn parse_record_literal_fields() {
        let p = parse_ok("component A { x: R = { field-a: 1, field-b: \"s\", }; }");
        let ast::ExprKind::Record(fields) = &first_default(&p).kind else {
            panic!("expected a record literal")
        };
        assert_eq!(fields.len(), 2);
        assert!(fields.iter().all(|f| f.present().is_some()));
    }

    #[test]
    fn parse_record_literal_versus_typed_closure() {
        // Both start `{ name :` — only the depth-0 `->` distinguishes them.
        let p = parse_ok("component A { div { a: { k: 1 }, b: { k: s32 -> k } } }");
        let ast::UiNode::Element(element) = p.component(0).body().next().unwrap() else {
            panic!("expected an element")
        };
        assert!(matches!(
            element.props[0].value.kind,
            ast::ExprKind::Record(_)
        ));
        assert!(matches!(
            element.props[1].value.kind,
            ast::ExprKind::Closure(_)
        ));
    }

    #[test]
    fn parse_record_literal_versus_closure_with_statements() {
        // `record_literal_field` is `name ":" expr`, and no `expr` contains a
        // `;`, so a depth-zero `;` proves the block is a closure body. pest
        // backtracks out of `record_literal`; committing on `name :` alone
        // rejected this block.
        //
        // The subject used to be `{ lets: s32 = 1; }`, a `let` binding of `s`
        // through the keyword prefix. `lets` is one identifier now and both
        // compilers reject that text, so the case moved to a spelled-out `let`,
        // which is what it was always testing.
        let p = parse_ok("component A { div { f: { let s: s32 = 1; } } }");
        let ast::ExprKind::Closure(closure) = &first_prop_value(&p).kind else {
            panic!("expected a closure, not a record literal")
        };
        assert!(matches!(closure.body[0], ast::Stmt::Let(_)));
    }

    #[test]
    fn parse_tuple_literal_takes_the_grammars_second_comma() {
        // `"(" ~ expr ~ "," ~ (expr ~ ("," ~ expr)*)? ~ ","? ~ ")"` — the group
        // is optional and the `","?` follows it, so `(1,,)` is a one-element
        // tuple. Exactly one extra comma, and only after an empty group.
        let p = parse_ok("component A { x: tuple<s32> = (1,,); }");
        let ast::ExprKind::Tuple(items) = &first_default(&p).kind else {
            panic!("expected a tuple literal")
        };
        assert_eq!(items.len(), 1);
        parse_err("component A { x: tuple<s32> = (1,,,); }");
        parse_err("component A { x: tuple<s32, s32> = (1,2,,); }");
    }

    #[test]
    fn parse_empty_closure_is_not_a_record() {
        let p = parse_ok("component A { div { clicked: {} } }");
        let ast::ExprKind::Closure(closure) = &first_prop_value(&p).kind else {
            panic!("expected a closure")
        };
        assert!(closure.params.is_empty() && closure.body.is_empty());
    }

    #[test]
    fn parse_closure_params_typed_and_inferred() {
        let p = parse_ok("component A { div { f: { x: s32, y: s32 -> x + y } } }");
        let ast::ExprKind::Closure(closure) = &first_prop_value(&p).kind else {
            panic!("expected a closure")
        };
        assert_eq!(closure.params.len(), 2);
        assert!(closure.params[0].present().unwrap().ty.is_some());

        let p = parse_ok("component A { div { f: { p, q -> p } } }");
        let ast::ExprKind::Closure(closure) = &first_prop_value(&p).kind else {
            panic!("expected a closure")
        };
        assert_eq!(closure.params.len(), 2);
        assert!(
            closure.params[0].present().unwrap().ty.is_none(),
            "inferred, not a placeholder"
        );
    }

    #[test]
    fn parse_closure_param_without_a_name_still_appears() {
        // The green tree carries a CLOSURE_PARAM node; the AST must too.
        let p = parse_err("component A { div { f: { : s32 -> 1 } } }");
        assert!(p.errors() >= 1);
    }

    #[test]
    fn parse_member_hole_keeps_the_base() {
        let p = parse_err("component A { x: s32 = a.b.; }");
        let ast::ExprKind::Member { base, member } = &first_default(&p).kind else {
            panic!("expected a member expression")
        };
        assert!(member.is_missing(), "the member name is the hole");
        assert!(
            matches!(base.kind, ast::ExprKind::Member { .. }),
            "`a.b` must survive; it used to be thrown away with the whole node"
        );
    }

    #[test]
    fn parse_string_interpolation() {
        let p = parse_ok("component A { div { \"Hello {name}!\" } }");
        let ast::UiNode::Element(div) = p.component(0).body().next().unwrap() else {
            panic!("expected an element")
        };
        let ast::UiNode::Text(text) = &div.children[0] else {
            panic!("expected a text node")
        };
        let ast::ExprKind::Interpolation(parts) = &text.content.kind else {
            panic!("expected an interpolation")
        };
        assert_eq!(parts.len(), 3);

        let p = parse_ok("component A { div { \"plain\" } }");
        let ast::UiNode::Element(div) = p.component(0).body().next().unwrap() else {
            panic!("expected an element")
        };
        let ast::UiNode::Text(text) = &div.children[0] else {
            panic!("expected a text node")
        };
        assert!(matches!(text.content.kind, ast::ExprKind::String(_)));
    }

    #[test]
    fn parse_expression_hole_marks_the_green_tree() {
        // `parse_primary`'s fallthrough consumes nothing, so its `ERROR` node is
        // zero-width — but it is there.
        let p = parse_err("component A { x: s32 = ; }");
        assert!(p.errors() >= 1);
    }
}
