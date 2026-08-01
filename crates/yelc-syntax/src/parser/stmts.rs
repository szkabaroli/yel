//! Statement blocks: `let`, `if`, `for`, `return`, assignment, and expression
//! statements.
//!
//! Split out of `exprs.rs`, which had grown past anti-spec A2's ~800-line
//! threshold for one unit.
//!
//! # One block shape, with a switch on the tail
//!
//! Every statement block is an [`ast::Block`] — `statement* ~ trailing_expr?`
//! with the trailing expression in [`ast::Block::tail`]. Four positions build
//! one: a closure body, a function body, an `if`-statement branch and a
//! `for`-statement body.
//!
//! `allow_trailing` says whether writing that trailing expression is *legal*
//! here, not whether it is representable. A closure or function body's tail is
//! its value; an `if` branch and a `for` body produce nothing, so a
//! semicolon-less final expression there is reported — and still recorded in
//! `tail`, because dropping the subtree the user wrote is what invariant S5
//! forbids.
//!
//! A branch with no `{` is `Recovered::Missing`, not an empty block — the same
//! rule the UI-tree bodies follow in `nodes.rs`.

use super::Parser;
use crate::ast;
use crate::token::{EXPRESSION_FIRST, STATEMENT_FIRST, TokenKind, TokenKind::*};
use yelc_base::Span;

/// What [`Parser::parse_stmt`] read: a statement, or the block's trailing
/// expression, which may only appear last.
///
/// Was `(ast::Stmt, bool)`, where the `bool` meant "that `Stmt` was actually the
/// trailing expression". The two are different things now — one goes in
/// `Block::stmts`, the other in `Block::tail` — so the return type says so.
enum StmtOrTail {
    Stmt(ast::Stmt),
    Tail(ast::Expr),
}

fn assign_op(kind: TokenKind) -> Option<ast::AssignOp> {
    Some(match kind {
        EQ => ast::AssignOp::Assign,
        ADD_EQ => ast::AssignOp::Add,
        SUB_EQ => ast::AssignOp::Sub,
        MUL_EQ => ast::AssignOp::Mul,
        DIV_EQ => ast::AssignOp::Div,
        _ => return None,
    })
}

impl<'a> Parser<'a> {
    /// `block_body = statement* ~ trailing_expr?`
    ///
    /// The caller has already consumed the `{` and closes the `}` itself.
    ///
    /// The green node `node` spans exactly what it did before this returned an
    /// [`ast::Block`] — `mark`/`finish_marked` is `builder.start_node`/
    /// `builder.finish_node` with the span the AST node now needs.
    pub(super) fn parse_stmt_block(&mut self, node: TokenKind, allow_trailing: bool) -> ast::Block {
        let mark = self.mark();
        let mut stmts = Vec::new();
        let mut tail = None;

        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();

            if !self.is_set(STATEMENT_FIRST) {
                self.start_node();
                self.error_here(format!(
                    "expected a statement, found `{}`",
                    self.current().spelling()
                ));
                self.advance();
                let span = self.finish_node(ERROR);
                stmts.push(ast::Stmt::Error {
                    id: self.new_node_id(),
                    span,
                });
                debug_assert!(self.position() > before);
                continue;
            }

            let parsed = self.parse_stmt(allow_trailing);
            assert!(self.position() > before, "statement consumed nothing");
            match parsed {
                StmtOrTail::Stmt(stmt) => stmts.push(stmt),
                StmtOrTail::Tail(expr) => {
                    // A semicolon-less expression can only be last: the caller
                    // now expects the `}`.
                    tail = Some(expr);
                    break;
                }
            }
        }

        let span = self.finish_marked(node, &mark);
        ast::Block {
            id: self.new_node_id(),
            span,
            stmts,
            tail,
        }
    }

    /// One of the five guarded recursive entry points: `if a { if a { …` is
    /// unbounded recursion through here and back into `parse_stmt_block`.
    ///
    /// The guard sits on `parse_stmt` rather than on `parse_stmt_block` because
    /// the block's loop is where the no-progress check lives — the recovery node
    /// this returns has consumed a token, so the loop still advances.
    ///
    /// Returns the statement, or the semicolon-less `trailing_expr` — which may
    /// only appear last, and becomes [`ast::Block::tail`].
    fn parse_stmt(&mut self, allow_trailing: bool) -> StmtOrTail {
        if !self.enter_nesting() {
            let span = self.nesting_limit_node();
            let id = self.new_node_id();
            return StmtOrTail::Stmt(<ast::Stmt as ast::Recovery>::recovery(id, span));
        }
        let result = self.parse_stmt_inner(allow_trailing);
        self.leave_nesting();
        result
    }

    fn parse_stmt_inner(&mut self, allow_trailing: bool) -> StmtOrTail {
        if self.at_let_statement() {
            return StmtOrTail::Stmt(self.parse_let_stmt());
        }
        if self.at_if_statement() {
            return StmtOrTail::Stmt(self.parse_if_stmt());
        }
        if self.at_for_statement() {
            return StmtOrTail::Stmt(self.parse_for_stmt());
        }
        if self.is(RETURN_KW) {
            return StmtOrTail::Stmt(self.parse_return_stmt());
        }

        let mark = self.mark();
        let target = self.parse_expr();

        if let Some(op) = assign_op(self.current()) {
            self.advance();
            let value = self.parse_expr();
            self.expect(SEMICOLON);
            let span = self.finish_marked(ASSIGN_STMT, &mark);
            return StmtOrTail::Stmt(ast::Stmt::Assign(ast::AssignStmt {
                id: self.new_node_id(),
                span,
                op,
                target,
                value,
            }));
        }

        let has_semicolon = self.eat(SEMICOLON);
        if !has_semicolon && !allow_trailing {
            // `if_statement` and `for_statement` bodies are `statement*` — no
            // trailing expression. Reported here; the expression is still kept,
            // as the block's `tail`.
            self.expect(SEMICOLON);
        }
        // The green node stays `EXPR_STMT` in both cases. The trailing
        // expression *is* `expr_statement` in the concrete grammar — the `;` is
        // what is optional — and inventing a second green kind for it would
        // rewrite the green tree of every closure in the corpus to record
        // something the token stream already says.
        let span = self.finish_marked(EXPR_STMT, &mark);
        if !has_semicolon {
            return StmtOrTail::Tail(target);
        }
        StmtOrTail::Stmt(ast::Stmt::Expr(ast::ExprStmt {
            id: self.new_node_id(),
            span,
            expr: target,
        }))
    }

    /// Whether `let` here starts a `let_statement`, or is an ordinary name.
    ///
    /// `let` is not reserved. `statement = let_statement | assign_statement |
    /// expr_statement`, and PEG **backtracks**: `let_statement` needs an
    /// `identifier` after the keyword, so `let = 1;` fails that alternative and
    /// `assign_statement` matches with `let` as the assignment target. The frozen
    /// parser reads two statements out of `{ let = 1; }` and drops nothing.
    ///
    /// Committing on the keyword instead was a silent grammar narrowing: it made
    /// `let`, alone among identifiers, unusable as a variable name.
    ///
    /// `let` is a word, so `letx = 1;` is an assignment to a variable called
    /// `letx` and never reaches `let_statement` at all.
    fn at_let_statement(&self) -> bool {
        self.is(LET_KW) && self.nth_is_name(1)
    }

    /// Whether `if` here starts an `if_statement`, or is an ordinary name.
    ///
    /// Same mechanism as [`Parser::at_let_statement`], one alternative wider:
    /// `if_statement` needs an `expr` after the keyword, so the keyword is a name
    /// exactly when what follows cannot begin a condition. `if = 1;`, `if();`,
    /// `if.a = 1;` and `if;` are all statements about a variable called `if`.
    ///
    /// What follows must be able to *start* an expression at all, which rules
    /// out `=`, `+=`, `;`, `}`, `,`, `.`, `?.` and end of input — `if.a = 1;`
    /// and `if;` are statements about a variable called `if`.
    ///
    /// The rest is genuinely ambiguous and pest resolves it by backtracking:
    /// `if (a) { … }` is an if-statement whose condition is parenthesised, while
    /// `if(x);` is a *call* on a variable called `if`. What separates them is
    /// whether a **depth-zero `{`** follows the expression — the branch — and
    /// that is what is asked here, in `O(1)`, off the table
    /// `condition_scan_table` builds in one pass (anti-spec B8: no per-`if`
    /// scan, and nothing that runs to end of input on unterminated source).
    ///
    /// `if` is a word, so `ifx { }` is the expression statement `ifx` followed
    /// by a stray block, exactly as any other identifier would be.
    fn at_if_statement(&self) -> bool {
        if !self.is(IF_KW) || !self.nth_is_set(1, EXPRESSION_FIRST) {
            return false;
        }
        match self.nth_non_trivia_at(1) {
            Some((index, _)) => self.expression_is_followed_by_a_block(index),
            None => false,
        }
    }

    /// Whether `for` here starts a `for_statement`, or is an ordinary name.
    ///
    /// Same mechanism as [`Parser::at_let_statement`], and the tightest of the
    /// three: `for_statement` is `"for" ~ identifier ~ "in" ~ expr ~ …`, so all
    /// three head tokens are fixed by the grammar and the predicate asks for
    /// exactly them. It is **not** a maintained list of spellings — the shape it
    /// matches is the production, and it cannot drift the way `parse_type`'s
    /// `(`-only lookahead drifted out of sync with `func<T>`.
    ///
    /// It has to be tighter than the *template* `for`'s guard
    /// (`nth_non_trivia(1) != L_BRACE`, in `nodes.rs`), because `for` is a legal
    /// **expression** in statement position and is not one in node position:
    /// `{ for + 1 }` and `{ for = 1; }` are about a variable called `for`, and a
    /// looser guard would steal them. Every text this does claim —
    /// `for <name> in …` inside a block — was a syntax error before, on both
    /// parsers, which is what makes the addition purely additive.
    ///
    /// `for` is a word, so `forx in xs { }` never reaches here at all.
    fn at_for_statement(&self) -> bool {
        self.is(FOR_KW) && self.nth_is_name(1) && self.nth_non_trivia(2) == IN_KW
    }

    /// `for_statement = "for" ~ identifier ~ "in" ~ expr ~ key_clause? ~ "{" ~ statement* ~ "}"`
    ///
    /// The **same** parser the template `for` uses — see `Parser::parse_for`.
    /// Only the body differs.
    fn parse_for_stmt(&mut self) -> ast::Stmt {
        let node = self.parse_for(FOR_STMT, |p| {
            ast::ForBody::Statements(p.parse_braced_stmt_block())
        });
        ast::Stmt::For(Box::new(node))
    }

    /// `return_statement = "return" ~ expr? ~ ";"`
    ///
    /// # There is no `at_return_statement`, and that is the decision
    ///
    /// `let`, `if` and `for` each have a guard because each of them is a name
    /// the frozen grammar reads as one — `{ let = 1; }` is an assignment to a
    /// variable called `let`, and a guard is what keeps it one. `return` cannot
    /// be given the same treatment, because the frozen grammar has **no**
    /// `return` production at all: every `return` it accepts is a name, so
    /// *any* commitment narrows. Measured against the frozen parser, in
    /// statement position it accepts `return;` · `return` (as a trailing
    /// expression) · `return - 1;` · `return(x);` · `return [0];` ·
    /// `return = 1;` · `return += 1;` · `return.x = 1;` · `return?.x;` — and
    /// rejects `return x;`, `return 1;`, `return false;`, `return "s";`. There
    /// is no guard that keeps the accepted list and adds the rejected one; the
    /// two overlap. `tests/returns.rs::the_frozen_parser_accepts_every_shape_we_now_narrow`
    /// pins that measurement against the oracle so it cannot rot into a claim.
    ///
    /// So the rule commits on `RETURN_KW`, unconditionally, in statement
    /// position: **one token kind the lexer already assigned**, no lookahead
    /// list to maintain and no third outcome. The alternative considered was
    /// "commit only when what follows is in `EXPRESSION_FIRST ∪ {;}`", which
    /// preserves `return = 1;` and `return.x;` while still taking `return;` and
    /// `return -1;`. It was rejected: it buys a strictly smaller narrowing at
    /// the cost of a rule nobody can state — a variable you may assign to but
    /// never read — and it is the maintained-lookahead shape that silently
    /// misparsed `func<T>`.
    ///
    /// The narrowing is confined to statement position. `RETURN_KW` is in
    /// `KEYWORD_FIRST` ⊆ `NAME_FIRST`, so `return` is still a property name, a
    /// record field, an element name, a `let` binder and a member — every
    /// position that reads an `identifier`. See
    /// `plans/rewrite/seam-changes.md` (2026-07-29).
    ///
    /// The value is optional, and its presence is decided by `EXPRESSION_FIRST`
    /// rather than by "is the next token `;`". That is the same question a
    /// FIRST set exists to answer, and it keeps `return }` from being read as a
    /// return of a broken expression: the `}` cannot start one, so the value is
    /// absent and the missing `;` is what gets reported.
    fn parse_return_stmt(&mut self) -> ast::Stmt {
        self.start_node();
        self.assert(RETURN_KW);
        let value = if self.is_set(EXPRESSION_FIRST) {
            Some(self.parse_expr())
        } else {
            None
        };
        self.expect(SEMICOLON);
        let span = self.finish_node(RETURN_STMT);

        ast::Stmt::Return(ast::ReturnStmt {
            id: self.new_node_id(),
            span,
            value,
        })
    }

    /// `let_statement = "let" ~ identifier ~ (":" ~ type_annotation)? ~ "=" ~ expr ~ ";"`
    fn parse_let_stmt(&mut self) -> ast::Stmt {
        self.start_node();
        self.assert(LET_KW);
        let name = self.expect_name();
        let ty = if self.eat(COLON) {
            Some(self.parse_type())
        } else {
            None
        };
        self.expect(EQ);
        let value = self.parse_expr();
        self.expect(SEMICOLON);
        let span = self.finish_node(LET_STMT);

        ast::Stmt::Let(ast::LetStmt {
            id: self.new_node_id(),
            span,
            name,
            ty,
            value,
        })
    }

    /// `if_statement = "if" ~ expr ~ "{" ~ statement* ~ "}" ~ ("else" ~ "{" ~ statement* ~ "}")?`
    fn parse_if_stmt(&mut self) -> ast::Stmt {
        self.start_node();
        self.assert(IF_KW);
        let condition = self.parse_expr_no_typed_record();
        let then_branch = self.parse_braced_stmt_block();

        // `("else" ~ "{" ~ statement* ~ "}")?` — the `{` is *inside* the
        // optional, so an `else` with no block does not make the branch
        // malformed: pest backtracks out of the whole option and the `else`
        // becomes the next statement. `{ if a { } else }` is an if-statement
        // followed by the trailing expression `else`, which the frozen parser
        // accepts and consuming the keyword unconditionally rejected.
        let else_branch = if self.is2(ELSE_KW, L_BRACE) {
            self.assert(ELSE_KW);
            Some(self.parse_braced_stmt_block())
        } else {
            None
        };

        let span = self.finish_node(IF_STMT);
        ast::Stmt::If(Box::new(ast::IfStmt {
            id: self.new_node_id(),
            span,
            condition,
            then_branch,
            else_branch,
        }))
    }

    /// `"{" ~ statement* ~ "}"` — `Missing` when the `{` is absent, so a branch
    /// that was never opened stays distinguishable from an empty one.
    ///
    /// Shared by `if`-statement branches and `for`-statement bodies. Both are
    /// `statement*`: neither produces a value, so `allow_trailing` is false and
    /// a semicolon-less final expression is reported.
    pub(super) fn parse_braced_stmt_block(&mut self) -> ast::Recovered<ast::Block> {
        if !self.is(L_BRACE) {
            let at = self.current_span();
            self.error_here(format!(
                "expected `{{`, found `{}`",
                self.current().spelling()
            ));
            return ast::Recovered::Missing {
                id: self.new_node_id(),
                span: Span::point(at.source, at.start),
            };
        }
        self.assert(L_BRACE);
        let body = self.parse_stmt_block(STMT_BLOCK, false);
        self.expect(R_BRACE);
        ast::Recovered::Present(body)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::{Parsed, parse_err, parse_ok};
    use crate::ast;

    /// The closure that every case below writes as the first prop of the first
    /// element — the only place a statement block is reachable from.
    fn closure_body(parsed: &Parsed) -> &ast::ClosureExpr {
        let component = parsed.component(0);
        let ast::UiNode::Element(element) = component.body().next().expect("a node") else {
            panic!("expected an element")
        };
        let ast::ExprKind::Closure(closure) = &element.props[0].value.kind else {
            panic!("expected a closure")
        };
        closure
    }

    /// How many statements an `if`-statement branch holds, or `None` when its
    /// `{` was missing.
    fn branch_len(branch: &ast::Recovered<ast::Block>) -> Option<usize> {
        branch.present().map(|block| block.stmts.len())
    }

    #[test]
    fn parse_every_statement_form() {
        let p = parse_ok(
            "component A { div { clicked: { let x: s32 = 1; count += 1; count = x; \
             if x > 0 { count -= 1; } else { count *= 2; } f(x); x } } }",
        );
        let closure = closure_body(&p);
        // Five statements and a tail, where this used to read six statements
        // with a `has_semicolon: false` flag on the last: the trailing `x` is
        // the block's *value* and now lives in `Block::tail`.
        assert_eq!(closure.body.stmts.len(), 5);
        assert!(matches!(closure.body.stmts[0], ast::Stmt::Let(_)));
        assert!(matches!(closure.body.stmts[1], ast::Stmt::Assign(_)));
        assert!(matches!(closure.body.stmts[3], ast::Stmt::If(_)));
        let tail = closure.body.tail.as_ref().expect("a trailing expression");
        assert!(matches!(tail.kind, ast::ExprKind::Ident(_)));
    }

    #[test]
    fn parse_let_with_an_inferred_type() {
        let p = parse_ok("component A { div { f: { let x = 1; x } } }");
        let closure = closure_body(&p);
        let ast::Stmt::Let(stmt) = &closure.body.stmts[0] else {
            panic!("expected a let")
        };
        assert!(stmt.ty.is_none(), "no type was written, none is invented");
    }

    #[test]
    fn parse_if_statement_branches() {
        let p = parse_ok("component A { div { f: { if a { b(); } else { c(); } } } }");
        let closure = closure_body(&p);
        let ast::Stmt::If(stmt) = &closure.body.stmts[0] else {
            panic!("expected an if statement")
        };
        assert_eq!(branch_len(&stmt.then_branch), Some(1));
        assert_eq!(stmt.else_branch.as_ref().and_then(branch_len), Some(1));
    }

    #[test]
    fn parse_if_statement_without_a_block_is_a_missing_block() {
        let p = parse_err("component A { div { f: { if a b(); } } }");
        assert!(p.errors() >= 1, "the missing `{{` must be marked");
    }

    #[test]
    fn parse_if_statement_body_forbids_a_trailing_expression() {
        // `if_statement`'s body is `statement*`; only a closure body has a
        // `trailing_expr`.
        parse_err("component A { div { f: { if a { b } } } }");
    }

    #[test]
    fn a_name_beginning_with_let_is_not_a_binding() {
        // `let_statement` is `!GLUED_LET ~ "let" ~ identifier`, so `letx = 1;`
        // is an assignment to a variable called `letx`. This used to assert the
        // opposite — it bound `x` — and it is the specification of the keyword
        // word boundary, so it moved with it; see
        // `plans/rewrite/goldens-changed.md`.
        let p = parse_ok("component A { div { f: { letx = 1; x } } }");
        let closure = closure_body(&p);
        let ast::Stmt::Assign(stmt) = &closure.body.stmts[0] else {
            panic!("expected an assignment to a variable called `letx`")
        };
        let ast::ExprKind::Ident(name) = stmt.target.kind else {
            panic!("expected the assignment target to be a plain name")
        };
        assert_eq!(p.name(name), "letx");
    }

    #[test]
    fn parse_glued_let_needs_the_binder_to_be_followed_by_a_type_or_a_value() {
        // `letx;` has no `:` and no `=`, so `let_statement` fails and the
        // **unsplit** reading survives: an expression statement about `letx`.
        let p = parse_ok("component A { div { f: { letx; } } }");
        let closure = closure_body(&p);
        let ast::Stmt::Expr(stmt) = &closure.body.stmts[0] else {
            panic!("expected an expression statement")
        };
        assert!(matches!(stmt.expr.kind, ast::ExprKind::Ident(_)));
    }

    #[test]
    fn parse_glued_let_does_not_split_before_a_digit_or_a_dash() {
        // `identifier` cannot start with either, so pest's possessive `?` kills
        // `let_statement` and `assign_statement` matches the whole name.
        for source in [
            "component A { div { f: { let8 = 1; } } }",
            "component A { div { f: { let-x = 1; } } }",
        ] {
            let p = parse_ok(source);
            let closure = closure_body(&p);
            assert!(
                matches!(closure.body.stmts[0], ast::Stmt::Assign(_)),
                "{source:?} is an assignment to a variable of that name"
            );
        }
    }

    #[test]
    fn parse_if_statement() {
        // The subject used to be `ifa > 0 { … }`, which read as `if a > 0`
        // through the keyword prefix. `ifa` is one identifier now and both
        // compilers reject that text — `expr_statement` meets a `{` where it
        // wants a `;` — so the case moved to a spelled-out `if`.
        let p = parse_ok("component A { div { f: { if a > 0 { g(); } } } }");
        let closure = closure_body(&p);
        let ast::Stmt::If(stmt) = &closure.body.stmts[0] else {
            panic!("expected an if statement")
        };
        assert_eq!(branch_len(&stmt.then_branch), Some(1));
    }

    #[test]
    fn parse_dangling_else_is_the_next_statement() {
        // `("else" ~ "{" ~ statement* ~ "}")?` — the `{` is inside the optional,
        // so an `else` with no block is not a malformed branch.
        let p = parse_ok("component A { div { f: { if a { } else } } }");
        let closure = closure_body(&p);
        let ast::Stmt::If(stmt) = &closure.body.stmts[0] else {
            panic!("expected an if statement")
        };
        assert!(stmt.else_branch.is_none());
        let tail = closure
            .body
            .tail
            .as_ref()
            .expect("the `else` becomes the trailing expression");
        assert!(matches!(tail.kind, ast::ExprKind::Ident(_)));
    }

    #[test]
    fn parse_statement_garbage_becomes_an_error_statement() {
        let p = parse_err("component A { div { f: { : s32 -> 1 } } }");
        assert!(p.errors() >= 1);
    }
}
