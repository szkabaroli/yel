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
//! * **A body with no `{` is `Block::Missing`, not an empty `Vec`.** An `if`
//!   whose block was never opened is not an `if` with an empty block, and
//!   `component A { if x "a" }` used to be indistinguishable from `if x { }`.

use super::{Follow, Parser, Speculation, is_kebab_lower};
use crate::ast;
use crate::token::{ELEMENT_ITEM_FIRST, NODE_FIRST, RESYNC_MEMBER, TokenKind::*};
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
            // `for_node = "for" ~ identifier ~ "in" ~ …`, and pest has no word
            // boundary, so `forx in xs { … }` is a `for` over a binding called
            // `x`. The `in` is what makes this safe to predict: `format { … }`
            // also starts with `for`, but pest *backtracks* out of `for_node`
            // there — no `in` follows — and matches `element_node`. Requiring
            // the `in` reproduces the outcome of that backtracking without
            // backtracking.
            _ if self.at_keyword_prefix("for", Follow::Name) && self.next_starts_with_in() => {
                ast::UiNode::For(Box::new(self.parse_for_node()))
            }
            // `if_node = "if" ~ expr ~ "{" ~ …`, and `if` is a bare literal too,
            // so `ifcount == 0 { … }` is `if count == 0 { … }`.
            //
            // No `{` glued to the keyword: `element_node` needs a brace, so it
            // is not a live alternative and there is nothing to decide.
            _ if self.at_keyword_prefix("if", Follow::Expr)
                && self.nth_non_trivia(1) != L_BRACE =>
            {
                ast::UiNode::If(Box::new(self.parse_if_node()))
            }
            // `if` immediately followed by `{`, glued (`ife { … }`) or not
            // (`if { a: 1 } { … }`). Both readings are live text: the brace may
            // be a record-literal *condition*, the body of an element literally
            // called `if`, or — when glued — the body of an if whose condition
            // is the suffix. pest tries `if_node` first and keeps it unless the
            // alternative chokes, so do exactly that.
            //
            // Both halves of this used to be lookahead, and both were wrong in
            // ways the accept/reject oracle could not see:
            // `glued_if_body_is_all_nodes` asked whether the block held a
            // depth-zero colon, on the theory that a colon means `named_prop`
            // and therefore an element — but a *ternary's* colon is depth-zero
            // too, so `ife { if a ? b : c { "x" } }` was read as an element
            // while pest read an if-node. Same accept/reject bit, different
            // tree (anti-spec A18). `if_condition_is_a_record` was the
            // companion guess for the unglued form. Speculating makes the
            // question mechanical instead of argued, and retires the
            // `shallow_marks` table's `colon` half, which existed only for this site.
            _ if self.is(IF_KW) || self.at_keyword_prefix("if", Follow::Expr) => {
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

    /// Whether the token after the current one is `in`, or an identifier `in`
    /// is a proper prefix of — `for x iny { … }`.
    ///
    /// `"in" ~ expr`, so the leftover only has to start an *expression*
    /// ([`Follow::Expr`]) — which is also what `parse_for_node`'s
    /// `eat_keyword(IN_KW, "in", Follow::Expr)` does. The two used to disagree:
    /// this one excluded `in-out` and that one did not, so `forx in-out { }` was
    /// rejected here while the identical `for x in-out { }` was accepted three
    /// lines later. The frozen parser accepts both — `in-out` is `in` followed
    /// by the expression `-out`.
    fn next_starts_with_in(&self) -> bool {
        self.nth_non_trivia(1) == IN_KW || {
            let text = self.nth_text(1);
            text.len() > 2 && text.starts_with("in")
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
    /// The modifier spellings are bare literals with no word boundary, so the
    /// possessive `?` also fires on a *prefix*: `settings: 1` is the modifier
    /// `set` on an attribute called `tings`. That reading only survives when the
    /// leftover is a legal `attr_name` — `@{ ASCII_ALPHA_LOWER ~ … }`
    /// (grammar.pest:290), which is exactly [`is_kebab_lower`]. When it is not,
    /// pest has already committed to the modifier and the alternative dies:
    /// `set8:`, `set-a:`, `set0:`, `bind8:`, `bind-a:` and `bind0:` are all
    /// **rejected**, and reading them as ordinary attribute names accepted every
    /// one.
    fn at_named_prop(&self) -> bool {
        for spelling in ["set", "bind"] {
            let text = self.current_text();
            if !self.is_name() || text.len() <= spelling.len() || !text.starts_with(spelling) {
                continue;
            }
            return is_kebab_lower(&text[spelling.len()..]) && self.nth_non_trivia(1) == COLON;
        }
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
        self.assert_keyword(IF_KW, "if", Follow::Expr);
        let condition = self.parse_expr();
        let then_branch = self.parse_node_body();

        let mut else_if_branches = Vec::new();
        let mut else_branch = None;

        while self.is(ELSE_KW) || self.at_glued_else_if() {
            self.start_node();
            self.assert_keyword(ELSE_KW, "else", Follow::Expr);
            // `else if` is two bare literals, so both joins occur: `elseif b`
            // glues the second to the first, `else iftrue` glues the condition
            // to the second. Matching `nth(1) == IF_KW` saw neither.
            if self.eat_keyword(IF_KW, "if", Follow::Expr) {
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



    /// Whether the current token glues `else` to the `if` of an `else if`.
    ///
    /// Only the `else if` form can be glued: `else_branch = "else" ~ "{"`, and a
    /// `{` can never be part of an identifier. That matters — `else` is not a
    /// reserved word, so splitting it out of *any* longer identifier would turn
    /// `if a { "x" } elsewhere: s32 = 0;` into a malformed else-branch, while
    /// pest backtracks out of the optional `else_branch` and reads `elsewhere`
    /// as the next member.
    fn at_glued_else_if(&self) -> bool {
        self.at_keyword_prefix("else", Follow::Expr)
            && self.current_text()["else".len()..].starts_with("if")
    }

    /// `for_node = "for" ~ identifier ~ "in" ~ expr ~ key_clause? ~ "{" ~ for_body ~ "}"`
    fn parse_for_node(&mut self) -> ast::ForNode {
        self.start_node();
        self.assert_keyword(FOR_KW, "for", Follow::Name);
        let item = self.expect_name();
        // `in` is a bare string literal too, so `for x iny { … }` binds `x` and
        // iterates `y`. `"in" ~ expr`, so the leftover only has to start an
        // expression — `for x in8 { … }` iterates the literal `8`.
        if !self.eat_keyword(IN_KW, "in", Follow::Expr) {
            self.expect(IN_KW);
        }
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

        let body = self.parse_node_body();
        let span = self.finish_node(FOR_NODE);

        ast::ForNode {
            id: self.new_node_id(),
            span,
            item,
            iterable,
            key,
            body,
        }
    }

    /// `if_body` / `for_body = (node ~ ","?)*`, delimited by braces.
    ///
    /// Returns [`ast::Block::Missing`] when the opening `{` is absent — the
    /// block was never opened, which is a different fact from its being empty.
    fn parse_node_body(&mut self) -> ast::Block<ast::UiNode> {
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
    fn parse_glued_if_takes_the_if_reading_when_the_body_is_nodes() {
        // `if_node` is tried before `element_node`, and `if_body` swallows this
        // block, so pest never backtracks: `ife { div { } }` is `if e { … }`.
        let p = parse_ok("component A { ife { div { } } }");
        let ast::UiNode::If(node) = p.component(0).body().next().unwrap() else {
            panic!("expected an if node, got an element named `ife`")
        };
        assert!(!node.then_branch.is_missing());
    }

    #[test]
    fn parse_glued_if_takes_the_element_reading_when_the_body_has_a_prop() {
        // A `named_prop` is not a `node`, so `if_body` cannot swallow this and
        // pest backtracks to `element_node`.
        let p = parse_ok("component A { iflex { color: red } }");
        let element = first_element(p.component(0));
        assert_eq!(p.name(element.name.present().unwrap().name), "iflex");
        assert_eq!(element.props.len(), 1);
    }

    #[test]
    fn parse_glued_if_with_a_prop_body_leaves_the_else_to_be_an_element() {
        // Both readings are live and the element one wins, so the `else` that
        // follows is an `element_node` of its own — `else` is not reserved.
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
