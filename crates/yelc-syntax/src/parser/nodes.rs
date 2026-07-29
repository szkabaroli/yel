//! The UI tree: elements, text, `if`, `for`, `@children`.
//!
//! Three shapes here are unusual enough to be worth naming:
//!
//! * **`element_content` separates items with *optional* commas** —
//!   `element_item ~ (","? ~ element_item)* ~ ","?` — so `parse_list` does not
//!   fit and this file carries its own loop, with the same no-progress guard.
//! * **`named_prop` wins over everything else** when the text is
//!   `[set|bind] attr-name :`, because it is the first PEG alternative. The
//!   `attr_name` shape check is what keeps `fontSize: 24px` and `Foo: 1`
//!   rejected: pest's `attr_name` is lowercase kebab and would have stopped
//!   before the `S`, failing the whole alternative.
//! * **A body with no `{` is `Recovered::Missing`, not an empty `Vec`.** An `if`
//!   whose block was never opened is not an `if` with an empty block, and
//!   `component A { if x "a" }` used to be indistinguishable from `if x { }`.

use super::{Parser, Speculation, is_kebab_lower};
use crate::ast;
use crate::token::{ELEMENT_ITEM_FIRST, NODE_FIRST, RESYNC_MEMBER, TokenKind, TokenKind::*};
use yelc_base::Span;

impl<'a> Parser<'a> {
    /// `node = if_node | for_node | children_node | element_node | string_node`
    ///
    /// `if` and `for` are not reserved words, and `node` tries `if_node` and
    /// `for_node` before `element_node`. When the keyword is followed directly
    /// by `{` neither can match — `if_node` wants a condition, `for_node` wants
    /// `ident in expr` — so the grammar falls through to `element_node` and the
    /// keyword becomes an *element name*. `if { Foo { … } }` really is an
    /// element called `if` today, and the guards below preserve that.
    ///
    /// Keywords are words, so a longer identifier that merely starts with one
    /// (`ifa`, `iflex`, `format`, `elsewhere`) never reaches these arms at all —
    /// the lexer produced a single `IDENTIFIER` and it falls through to
    /// `element_node` like any other name.
    ///
    /// One of the five guarded recursive entry points: `div { div { div { …` is
    /// unbounded recursion through this function alone.
    pub(super) fn parse_ui_node(&mut self) -> ast::UiNode {
        if !self.enter_nesting() {
            let span = self.nesting_limit_node();
            return <ast::UiNode as ast::Recovery>::recovery(self.new_node_id(), span);
        }
        let result = self.parse_ui_node_inner();
        self.leave_nesting();
        result
    }

    fn parse_ui_node_inner(&mut self) -> ast::UiNode {
        match self.current() {
            // A real `if` keyword with something other than `{` after it: only
            // `if_node` can match, so there is nothing to decide.
            IF_KW if self.nth_non_trivia(1) != L_BRACE => {
                ast::UiNode::If(Box::new(self.parse_if_node()))
            }
            FOR_KW if self.nth_non_trivia(1) != L_BRACE => {
                ast::UiNode::For(Box::new(self.parse_for_node()))
            }
            AT => {
                if self.at_children_marker() {
                    let span = self.parse_children_marker();
                    ast::UiNode::Children {
                        id: self.new_node_id(),
                        span,
                    }
                } else {
                    self.parse_error_node("expected `@children`")
                }
            }
            STRING_LITERAL | TEMPLATE_LITERAL => {
                let content = self.parse_string_expr();
                ast::UiNode::Text(ast::TextNode {
                    id: self.new_node_id(),
                    span: content.span,
                    content,
                })
            }
            // A real `if` immediately followed by `{`. Both readings are live
            // text: the brace may be a record-literal *condition*
            // (`if { a: 1 } { … }`), or the body of an element literally called
            // `if` (`if { span { "x" } }`). pest tries `if_node` first and keeps
            // it unless the alternative chokes, so do exactly that.
            //
            // This used to be lookahead, and it was wrong in a way the
            // accept/reject oracle could not see: `if_condition_is_a_record`
            // guessed at the shape of the block. Speculating makes the question
            // mechanical instead of argued, and retires the `shallow_marks`
            // table's `colon` half, which existed only for this site.
            IF_KW => {
                let attempt = self.try_parse(Speculation::IfNode, |p| {
                    let diagnostics = p.buffered_diagnostics.len();
                    let overflows = p.depth_limit_hits;
                    let node = p.parse_if_node();
                    // "the alternative matched" == it parsed without reporting
                    // **and** without overflowing the depth limit.
                    //
                    // The second half is not redundant. The depth diagnostic is
                    // latched to once per parse, so once anything has spent it
                    // an over-deep attempt buffers *nothing* while still filling
                    // its body with `ERROR` nodes — and a diagnostics-only test
                    // would call that a clean match. Review found the witness:
                    // the same `ife { <256 deep> }` read as an element alone and
                    // as an `if` when preceded by an unrelated deeply-nested
                    // declaration in another component. Construct identity must
                    // not depend on what else is in the file.
                    let clean = p.buffered_diagnostics.len() == diagnostics
                        && p.depth_limit_hits == overflows;
                    clean.then_some(node)
                });
                match attempt {
                    Some(node) => ast::UiNode::If(Box::new(node)),
                    None => ast::UiNode::Element(self.parse_element_node()),
                }
            }
            _ if self.is_name() && self.nth_non_trivia(1) == L_BRACE => {
                ast::UiNode::Element(self.parse_element_node())
            }
            _ => self.parse_error_node("expected a node"),
        }
    }

    fn parse_error_node(&mut self, message: &str) -> ast::UiNode {
        self.start_node();
        self.error_here(message);
        if !self.is_eof() {
            self.advance();
        }
        self.recover_to(RESYNC_MEMBER);
        let span = self.finish_node(ERROR);
        ast::UiNode::Error {
            id: self.new_node_id(),
            span,
        }
    }

    /// `children_node = "@children" ~ ";"?`
    pub(super) fn parse_children_marker(&mut self) -> Span {
        self.start_node();
        self.assert(AT);
        self.assert(CHILDREN_KW);
        self.eat(SEMICOLON);
        self.finish_node(CHILDREN_NODE)
    }

    /// `element_node = element_name ~ "{" ~ element_content? ~ "}"`
    fn parse_element_node(&mut self) -> ast::ElementNode {
        self.start_node();
        let name = self.expect_name();
        self.assert(L_BRACE);

        let mut props = Vec::new();
        let mut children = Vec::new();

        self.builder.start_node();
        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();

            if self.at_named_prop() {
                props.push(self.parse_named_prop());
            } else if self.is_set(ELEMENT_ITEM_FIRST) {
                children.push(self.parse_ui_node());
            } else {
                children.push(self.parse_error_node("expected a property or a child node"));
            }

            // `element_content` separates with an *optional* comma, and allows
            // one trailing comma.
            self.eat(COMMA);

            assert!(self.position() > before, "element item consumed nothing");
        }
        self.builder.finish_node(ELEMENT_CONTENT);
        self.expect(R_BRACE);

        let span = self.finish_node(ELEMENT_NODE);
        ast::ElementNode {
            id: self.new_node_id(),
            span,
            name,
            props,
            children,
        }
    }

    /// `named_prop = prop_modifier? ~ attr_name ~ ":" ~ expr`
    ///
    /// PEG's `?` is **possessive**: once `prop_modifier` matches, pest does not
    /// backtrack out of it if the rest of the alternative fails. So `set: 5` is
    /// not a property called `set` — the modifier is consumed, `attr_name` then
    /// faces `:` and the whole alternative dies. That is why `set`/`bind` at the
    /// head commits here instead of falling through to the no-modifier case.
    /// The modifier spellings are words, so a longer identifier that merely
    /// starts with one — `settings`, `set8`, `set-a`, `bindings` — never
    /// reaches the possessive `?` and is an ordinary `attr_name`.
    fn at_named_prop(&self) -> bool {
        let name_index = if matches!(self.current(), SET_KW | BIND_KW) {
            if !self.nth_is_name(1) || self.nth_non_trivia(2) != COLON {
                return false;
            }
            1
        } else if self.is_name() && self.nth_non_trivia(1) == COLON {
            0
        } else {
            return false;
        };
        is_kebab_lower(self.nth_text(name_index))
    }

    fn parse_named_prop(&mut self) -> ast::NamedProp {
        self.start_node();
        let modifier = match self.current() {
            SET_KW if self.nth_is_name(1) => {
                self.start_node();
                self.assert(SET_KW);
                self.finish_node(MODIFIER);
                ast::PropModifier::Set
            }
            BIND_KW if self.nth_is_name(1) => {
                self.start_node();
                self.assert(BIND_KW);
                self.finish_node(MODIFIER);
                ast::PropModifier::Bind
            }
            _ => ast::PropModifier::None,
        };

        let name = self.expect_name();
        self.expect(COLON);
        let value = self.parse_expr();
        let span = self.finish_node(NAMED_PROP);

        ast::NamedProp {
            id: self.new_node_id(),
            span,
            modifier,
            name,
            value,
        }
    }

    /// `if_node = "if" ~ expr ~ "{" ~ if_body ~ "}" ~ else_if_branch* ~ else_branch?`
    fn parse_if_node(&mut self) -> ast::IfNode {
        self.start_node();
        self.assert(IF_KW);
        let condition = self.parse_expr();
        let then_branch = self.parse_node_body();

        let mut else_if_branches = Vec::new();
        let mut else_branch = None;

        while self.is(ELSE_KW) {
            self.start_node();
            self.assert(ELSE_KW);
            if self.eat(IF_KW) {
                let branch_condition = self.parse_expr();
                let body = self.parse_node_body();
                let span = self.finish_node(ELSE_IF_BRANCH);
                else_if_branches.push(ast::ElseIfBranch {
                    id: self.new_node_id(),
                    span,
                    condition: branch_condition,
                    body,
                });
            } else {
                else_branch = Some(self.parse_node_body());
                self.finish_node(ELSE_BRANCH);
                break;
            }
        }

        let span = self.finish_node(IF_NODE);
        ast::IfNode {
            id: self.new_node_id(),
            span,
            condition,
            then_branch,
            else_if_branches,
            else_branch,
        }
    }

    /// `for_node = "for" ~ identifier ~ "in" ~ expr ~ key_clause? ~ "{" ~ for_body ~ "}"`
    ///
    /// # The only `for` parser in the crate
    ///
    /// `for` is legal in a **template** and, since 2026-07-29, in a **statement
    /// block**. The head is identical in both — the positions differ only in
    /// what the body holds, which is exactly what [`ast::ForBody`] is — so the
    /// head is read here once and the body comes in as a closure. A second
    /// `parse_for_stmt` that re-read `for x in e key(k)?` would be the
    /// duplicated walker anti-spec A3 forbids, and the two copies would drift on
    /// the first change to the head.
    ///
    /// `node` is the green kind to close: `FOR_NODE` in a template, `FOR_STMT`
    /// in a block. Which position is being parsed is decided by *the caller* —
    /// `parse_ui_node` versus `parse_stmt_inner` — never by lookahead inside
    /// here.
    pub(super) fn parse_for(
        &mut self,
        node: TokenKind,
        parse_body: impl FnOnce(&mut Self) -> ast::ForBody,
    ) -> ast::ForNode {
        self.start_node();
        self.assert(FOR_KW);
        let item = self.expect_name();
        self.expect(IN_KW);
        let iterable = self.parse_expr();

        let key = if self.is2(KEY_KW, L_PAREN) {
            self.start_node();
            self.assert(KEY_KW);
            self.assert(L_PAREN);
            let key = self.parse_expr();
            self.expect(R_PAREN);
            self.finish_node(KEY_CLAUSE);
            Some(key)
        } else {
            None
        };

        let body = parse_body(self);
        let span = self.finish_node(node);

        ast::ForNode {
            id: self.new_node_id(),
            span,
            item,
            iterable,
            key,
            body,
        }
    }

    fn parse_for_node(&mut self) -> ast::ForNode {
        self.parse_for(FOR_NODE, |p| ast::ForBody::Nodes(p.parse_node_body()))
    }

    /// `if_body` / `for_body = (node ~ ","?)*`, delimited by braces.
    ///
    /// Returns `Recovered::Missing` when the opening `{` is absent — the block
    /// was never opened, which is a different fact from its being empty.
    fn parse_node_body(&mut self) -> ast::Braced<ast::UiNode> {
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

        let mut body = Vec::new();
        self.builder.start_node();
        while !self.is(R_BRACE) && !self.is_eof() {
            let before = self.position();
            if self.is_set(NODE_FIRST) {
                body.push(self.parse_ui_node());
            } else {
                body.push(self.parse_error_node("expected a node"));
            }
            self.eat(COMMA);
            assert!(self.position() > before, "node body item consumed nothing");
        }
        self.builder.finish_node(NODE_BODY);
        self.expect(R_BRACE);

        ast::Recovered::Present(body)
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::{parse_err, parse_ok};
    use crate::ast;

    fn first_element(component: &ast::ComponentDecl) -> &ast::ElementNode {
        match component.body().next().expect("a body node") {
            ast::UiNode::Element(element) => element,
            other => panic!("expected an element, got a {:?}", other.span()),
        }
    }

    #[test]
    fn parse_element_node_with_props_and_children() {
        let p = parse_ok("component A { div { class: \"row\", span { \"hi\" }, \"tail\" } }");
        let div = first_element(p.component(0));
        assert_eq!(div.props.len(), 1);
        assert_eq!(div.children.len(), 2);
    }

    #[test]
    fn parse_prop_modifiers() {
        let p = parse_ok("component A { input { set value: { x = 1; }, bind text: y } }");
        let element = first_element(p.component(0));
        assert_eq!(element.props[0].modifier, ast::PropModifier::Set);
        assert_eq!(element.props[1].modifier, ast::PropModifier::Bind);
    }

    #[test]
    fn parse_if_else_chain() {
        let p = parse_ok(
            "component A { if a { \"x\" } else if b { \"y\" } else if c { \"z\" } else { \"w\" } }",
        );
        let ast::UiNode::If(node) = p.component(0).body().next().unwrap() else {
            panic!("expected an if node")
        };
        assert_eq!(node.else_if_branches.len(), 2);
        assert!(node.else_branch.is_some());
        assert!(!node.then_branch.is_missing());
    }

    #[test]
    fn parse_if_without_a_block_is_a_missing_block() {
        // Distinguishable from `if x { }`, which it used to collapse into.
        let p = parse_err("component A { if x \"a\" }");
        let ast::UiNode::If(node) = p.component(0).body().next().unwrap() else {
            panic!("expected an if node")
        };
        assert!(node.then_branch.is_missing());
    }

    #[test]
    fn parse_for_node_with_key() {
        let p = parse_ok("component A { for item in items key(item.id) { \"x\" } }");
        let ast::UiNode::For(node) = p.component(0).body().next().unwrap() else {
            panic!("expected a for node")
        };
        assert_eq!(p.name(node.item.present().unwrap().name), "item");
        assert!(node.key.is_some());
    }

    #[test]
    fn parse_for_without_a_binding_leaves_a_hole() {
        let p = parse_err("component A { for in xs { \"a\" } }");
        assert!(p.errors() >= 1, "the missing `in` must be marked");
    }

    #[test]
    fn parse_children_node() {
        let p = parse_ok("component A { VStack { @children } }");
        let element = first_element(p.component(0));
        assert!(matches!(element.children[0], ast::UiNode::Children { .. }));
    }

    #[test]
    fn a_name_beginning_with_if_is_an_element() {
        // `if_node` is `!GLUED_IF ~ "if"`, so the alternative never opens on
        // `ife` and `element_node` takes it. This used to assert the opposite —
        // `ife { div { } }` was `if e { … }` — and it is the specification of
        // the keyword word boundary, so it moved with it; see
        // `plans/rewrite/goldens-changed.md`.
        let p = parse_ok("component A { ife { div { } } }");
        let element = first_element(p.component(0));
        assert_eq!(p.name(element.name.present().unwrap().name), "ife");
    }

    #[test]
    fn a_name_beginning_with_if_is_an_element_whatever_the_body_holds() {
        // The companion to the case above. This one reached `element_node` by
        // backtracking out of `if_body` before the boundary — a `named_prop` is
        // not a `node` — and reaches it directly now. Same tree either way,
        // which is why it is worth keeping both.
        let p = parse_ok("component A { iflex { color: red } }");
        let element = first_element(p.component(0));
        assert_eq!(p.name(element.name.present().unwrap().name), "iflex");
        assert_eq!(element.props.len(), 1);
    }

    #[test]
    fn an_else_after_an_iflex_element_is_an_element_too() {
        // `iflex` is an element, so the `else` that follows heads a node of its
        // own — and `else` is not reserved, so it is an `element_node`.
        let p = parse_ok("component A { iflex { color: red } else { \"x\" } }");
        let mut body = p.component(0).body();
        let ast::UiNode::Element(first) = body.next().unwrap() else {
            panic!("expected an element")
        };
        assert_eq!(p.name(first.name.present().unwrap().name), "iflex");
        let ast::UiNode::Element(second) = body.next().unwrap() else {
            panic!("expected a second element")
        };
        assert_eq!(p.name(second.name.present().unwrap().name), "else");
    }

    #[test]
    fn parse_if_whose_condition_is_a_record_literal() {
        // `record_literal` is a `primary`, so the first `{` is the condition and
        // the second opens the body.
        let p = parse_ok("component A { if { a: 1 } { div { } } }");
        let ast::UiNode::If(node) = p.component(0).body().next().unwrap() else {
            panic!("expected an if node")
        };
        assert!(matches!(node.condition.kind, ast::ExprKind::Record(_)));
        assert_eq!(node.then_branch.present().map(Vec::len), Some(1));
    }

    #[test]
    fn parse_keyword_as_an_element_name() {
        // `if` followed directly by `{` cannot be an `if_node`, so the grammar
        // falls through to `element_node` and `if` becomes the element name.
        let p = parse_ok("component A { if { span { \"x\" } } }");
        let element = first_element(p.component(0));
        assert_eq!(p.name(element.name.present().unwrap().name), "if");
    }
}
