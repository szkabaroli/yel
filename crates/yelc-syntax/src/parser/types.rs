//! `type_annotation` and everything under it.
//!
//! The frozen grammar orders the alternatives
//! `primitive | list | option | result | tuple | func | named`, and every one of
//! the leading spellings is written as a bare string literal — they are **not**
//! reserved words. So the dispatch here is by *text*, and anything unrecognised
//! falls through to `named_type`. That is what keeps `component T { color: … }`
//! and a user type called `list` working.

use super::{Parser, TrailingSep};
use crate::ast;
use crate::token::{TYPE_LIST_RECOVERY, TokenKind::*};
use yelc_base::Span;

impl<'a> Parser<'a> {
    /// One of the five guarded recursive entry points: `list<list<list<…` is
    /// unbounded recursion through this function alone.
    pub(super) fn parse_type(&mut self) -> ast::TypeRef {
        if !self.enter_nesting() {
            let span = self.nesting_limit_node();
            return <ast::TypeRef as ast::Recovery>::recovery(self.new_node_id(), span);
        }
        let result = self.parse_type_inner();
        self.leave_nesting();
        result
    }

    fn parse_type_inner(&mut self) -> ast::TypeRef {
        // `func_type = "func" ~ "(" ~ func_params? ~ ")" ~ func_return?`. The
        // `(` is **mandatory**, so `func` on its own fails `func_type` and falls
        // through to `named_type = { identifier }` — `func` is not a reserved
        // word, and `f: func;`, `x: func = 0;`, `record R { a: func }` and
        // `list<func>` are all accepted by the frozen parser. Committing on the
        // keyword alone rejected every one of them.
        if self.is(FUNC_KW) && self.nth_non_trivia(1) == L_PAREN {
            return self.parse_func_type();
        }

        if !self.is_name() {
            self.start_node();
            self.error_here(format!(
                "expected a type, found `{}`",
                self.current().spelling()
            ));
            let span = self.finish_node(ERROR);
            return ast::TypeRef {
                id: self.new_node_id(),
                span,
                kind: ast::TypeKind::Error,
            };
        }

        match self.current_text() {
            "list" if self.nth_non_trivia(1) == LT => self.parse_wrapper_type(LIST_TYPE),
            "option" if self.nth_non_trivia(1) == LT => self.parse_wrapper_type(OPTION_TYPE),
            "tuple" if self.nth_non_trivia(1) == LT => self.parse_tuple_type(),
            "result" => self.parse_result_type(),
            text => match ast::PrimitiveType::from_spelling(text) {
                Some(primitive) => {
                    self.start_node();
                    self.advance();
                    let span = self.finish_node(PRIMITIVE_TYPE);
                    ast::TypeRef {
                        id: self.new_node_id(),
                        span,
                        kind: ast::TypeKind::Primitive(primitive),
                    }
                }
                None => match type_keyword_prefix_of(text) {
                    Some(prefix) => self.parse_prefix_matched_type(text, prefix),
                    None => {
                        self.start_node();
                        let name = self.intern(text);
                        self.advance();
                        let span = self.finish_node(NAMED_TYPE);
                        ast::TypeRef {
                            id: self.new_node_id(),
                            span,
                            kind: ast::TypeKind::Named(name),
                        }
                    }
                },
            },
        }
    }

    /// An identifier that a type keyword matches a **proper prefix** of.
    ///
    /// `primitive_type` and `result_type` are ordered choices of bare string
    /// literals with no word boundary, so `s32x` is the primitive `s32` followed
    /// by a stray `x`: `type_annotation` *succeeds* having consumed three
    /// characters, and the enclosing `property_decl` then dies on the `x`. The
    /// frozen parser rejects every member of this class, so accepting them as
    /// named types was a silent grammar **widening**.
    ///
    /// The whole identifier is consumed here — it is one token, and leaving part
    /// of it unconsumed would break invariant S1. What is reproduced is the
    /// rejection, not pest's byte position.
    fn parse_prefix_matched_type(&mut self, text: &str, prefix: &str) -> ast::TypeRef {
        self.start_node();
        let span = self.current_span();
        let rest = &text[prefix.len()..];
        self.error_at(
            span,
            format!("expected a type, found `{text}`: `{prefix}` is a built-in type name and `{rest}` is left over"),
        );
        self.advance();
        let span = self.finish_node(ERROR);
        ast::TypeRef {
            id: self.new_node_id(),
            span,
            kind: ast::TypeKind::Error,
        }
    }

    /// `list<T>` / `option<T>` — exactly one type argument.
    fn parse_wrapper_type(&mut self, node: crate::token::TokenKind) -> ast::TypeRef {
        self.start_node();
        self.advance(); // `list` / `option`
        self.assert(LT);
        let inner = Box::new(self.parse_type());
        self.expect_type_close();
        let span = self.finish_node(node);
        let kind = if node == LIST_TYPE {
            ast::TypeKind::List(inner)
        } else {
            ast::TypeKind::Option(inner)
        };
        ast::TypeRef {
            id: self.new_node_id(),
            span,
            kind,
        }
    }

    /// `tuple_type = "tuple" ~ "<" ~ type_list ~ ">"`, and `type_list` forbids
    /// a trailing comma.
    fn parse_tuple_type(&mut self) -> ast::TypeRef {
        self.start_node();
        self.advance(); // `tuple`
        let items = self.parse_list(
            LT,
            COMMA,
            GT,
            TYPE_LIST_RECOVERY,
            TYPE_ARG_LIST,
            TrailingSep::Forbidden,
            |p| p.parse_type_arg(),
        );
        let span = self.finish_node(TUPLE_TYPE);
        self.require_a_type_argument(&items, span, "tuple");
        ast::TypeRef {
            id: self.new_node_id(),
            span,
            kind: ast::TypeKind::Tuple(items),
        }
    }

    /// `type_list` and `result_types` both start with a mandatory
    /// `type_annotation`, so an **empty** argument list is not a zero-arity
    /// generic — it is a failed `tuple_type` / `result_type`, and pest then has
    /// nothing but `named_type` to fall back on, which leaves the `<` unconsumed
    /// and kills the enclosing declaration. `x: tuple<>;` and `x: result<>;` are
    /// rejected by the frozen parser and were accepted here.
    ///
    /// `parse_list` has no minimum arity, and it should not grow one: this is
    /// the same shape as `result`'s *maximum* arity, and the same answer — the
    /// arity is the recovery position, `TypeKind` has no slot for it, so it
    /// becomes an [`ast::RecoveryMark`] rather than an invented element.
    ///
    /// The position is the **end** of the argument list, not the start of the
    /// type. `result<` truncated at end of input already carries a diagnostic
    /// there; anchoring this one on the `result` instead pulled the file's first
    /// reported offset backwards past it, and
    /// `first_error_offset_agrees_with_the_frozen_parser_as_often_as_before`
    /// caught the drift (548 → 546).
    fn require_a_type_argument(&mut self, items: &[ast::TypeRef], span: Span, what: &str) {
        if !items.is_empty() {
            return;
        }
        let at = Span::point(span.source, span.end);
        self.error_at(at, format!("`{what}` needs at least one type argument"));
        self.record_recovery_mark(at);
    }

    /// `result_type = "result" ~ ("<" ~ result_types ~ ">")?` where
    /// `result_types = type ~ ("," ~ type)?` — one or two, never more.
    ///
    /// A third argument is **reported and kept**. Truncating the list to two
    /// would drop a subtree the user wrote, which invariant S5 forbids, and it
    /// left `result<s32, string, bool>` with a diagnostic and no `Error` node
    /// anywhere in the tree.
    fn parse_result_type(&mut self) -> ast::TypeRef {
        self.start_node();
        self.advance(); // `result`

        let written = self.is(LT);
        let args = if written {
            self.parse_list(
                LT,
                COMMA,
                GT,
                TYPE_LIST_RECOVERY,
                TYPE_ARG_LIST,
                TrailingSep::Forbidden,
                |p| p.parse_type_arg(),
            )
        } else {
            Vec::new()
        };

        let span = self.finish_node(RESULT_TYPE);
        // `result` alone is a complete `result_type`; `result<>` is not.
        if written {
            self.require_a_type_argument(&args, span, "result");
        }
        if args.len() > 2 {
            self.error_at(
                span,
                format!(
                    "`result` takes at most two type arguments, found {}",
                    args.len()
                ),
            );
            // Every argument the user wrote stays in `args`; a `truncate(2)`
            // here is a silently dropped subtree. The arity itself is the
            // recovery position and `TypeKind` has no slot for it, so it
            // becomes an `ast::RecoveryMark` — never an extra list element.
            self.record_recovery_mark(span);
        }

        ast::TypeRef {
            id: self.new_node_id(),
            span,
            kind: ast::TypeKind::Result { args },
        }
    }

    fn parse_type_arg(&mut self) -> Option<ast::TypeRef> {
        if !self.is_set(crate::token::TYPE_FIRST) {
            return None;
        }
        Some(self.parse_type())
    }

    /// `func_type = "func" ~ "(" ~ func_params? ~ ")" ~ func_return?`
    fn parse_func_type(&mut self) -> ast::TypeRef {
        self.start_node();
        self.assert(FUNC_KW);
        let signature = self.parse_func_signature();
        let span = self.finish_node(FUNC_TYPE);
        ast::TypeRef {
            id: self.new_node_id(),
            span,
            kind: ast::TypeKind::Func(Box::new(signature)),
        }
    }
}

/// The type keywords that match a bare prefix, in `grammar.pest`'s order.
///
/// `primitive_type`'s alternatives, then `result` — `result_type`'s
/// `("<" ~ result_types ~ ">")?` is optional, so `result` alone is a complete
/// match and `resultx` prefix-matches exactly as `s32x` does.
///
/// `list`, `option` and `tuple` are deliberately absent: each is followed by a
/// mandatory `<`, so a prefix match fails the whole alternative and the
/// identifier falls through to `named_type`. `listx` is a legal named type
/// today and stays one.
const PREFIX_MATCHING_TYPE_KEYWORDS: &[&str] = &[
    "bool", "s64", "s32", "s16", "s8", "u64", "u32", "u16", "u8", "f64", "f32", "char", "string",
    "int", "float", "length", "physical-length", "angle", "duration", "percent",
    "relative-font-size", "color", "brush", "image", "easing", "result",
];

/// The keyword pest would have matched, when `text` is not one of them outright.
///
/// At most one can match: no entry above is a prefix of another, so two prefixes
/// of the same string would have to be prefixes of each other.
fn type_keyword_prefix_of(text: &str) -> Option<&'static str> {
    PREFIX_MATCHING_TYPE_KEYWORDS
        .iter()
        .copied()
        .find(|keyword| text.len() > keyword.len() && text.starts_with(keyword))
}

#[cfg(test)]
mod tests {
    use super::super::tests::{parse_err, parse_ok};
    use crate::ast;

    #[test]
    fn no_prefix_matching_type_keyword_is_a_prefix_of_another() {
        // The `find` in `type_keyword_prefix_of` returns the first match; this
        // is why "first" and "only" are the same thing.
        for (index, outer) in super::PREFIX_MATCHING_TYPE_KEYWORDS.iter().enumerate() {
            for (other, inner) in super::PREFIX_MATCHING_TYPE_KEYWORDS.iter().enumerate() {
                assert!(
                    index == other || !inner.starts_with(outer),
                    "`{outer}` is a prefix of `{inner}`; the ordered choice now matters"
                );
            }
        }
    }

    #[test]
    fn parse_prefix_of_a_primitive_is_not_a_named_type() {
        // pest matches `s32` and the leftover `x` kills `property_decl`.
        for source in [
            "component A { x: s32x = 0; }",
            "component A { x: strings = 0; }",
            "component A { x: charx = 0; }",
            "component A { x: int8 = 0; }",
            "component A { x: resultx = 0; }",
            "component A { x: physical-lengthx = 0; }",
        ] {
            parse_err(source);
        }
        // No primitive is a prefix of these, and `list`/`option`/`tuple` need a
        // `<`, so they really are named types.
        parse_ok("component A { x: listx = 0; }");
        parse_ok("component A { x: optionx = 0; }");
        parse_ok("component A { x: tuplex = 0; }");
    }

    #[test]
    fn parse_primitive_and_named_types() {
        let p = parse_ok("component T { a: bool; b: int; c: MyRecord; }");
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        assert!(matches!(
            properties[0].ty.kind,
            ast::TypeKind::Primitive(ast::PrimitiveType::Bool)
        ));
        // `int` is an alias for `s32`.
        assert!(matches!(
            properties[1].ty.kind,
            ast::TypeKind::Primitive(ast::PrimitiveType::S32)
        ));
        assert!(matches!(properties[2].ty.kind, ast::TypeKind::Named(_)));
    }

    #[test]
    fn parse_nested_wrapper_types() {
        let p = parse_ok("component T { a: list<list<s32>>; b: option<string>; }");
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        let ast::TypeKind::List(inner) = &properties[0].ty.kind else {
            panic!("expected a list type")
        };
        assert!(matches!(inner.kind, ast::TypeKind::List(_)));
        assert!(matches!(properties[1].ty.kind, ast::TypeKind::Option(_)));
    }

    #[test]
    fn parse_result_arity() {
        let p = parse_ok("component T { a: result; b: result<s32>; c: result<s32, string>; }");
        let c = p.component(0);
        let properties: Vec<_> = c.properties().collect();
        for (index, expected) in [0usize, 1, 2].into_iter().enumerate() {
            let ast::TypeKind::Result { args } = &properties[index].ty.kind else {
                panic!("expected a result type")
            };
            assert_eq!(args.len(), expected);
        }
    }

    #[test]
    fn parse_over_long_result_keeps_every_argument() {
        let p = parse_err("component T { a: result<s32, string, bool>; }");
        let c = p.component(0);
        let ast::TypeKind::Result { args } = &c.properties().next().unwrap().ty.kind else {
            panic!("expected a result type")
        };
        assert_eq!(args.len(), 3, "the third argument must not be truncated");
        assert!(p.errors() >= 1, "and it must be marked");
    }

    #[test]
    fn parse_func_type_is_a_signature() {
        let p = parse_ok("component T { h: func(x: s32) -> bool; }");
        let c = p.component(0);
        let ast::TypeKind::Func(signature) = &c.properties().next().unwrap().ty.kind else {
            panic!("expected a func type")
        };
        assert_eq!(signature.present_params().count(), 1);
        assert!(signature.return_type.is_some());
    }

    #[test]
    fn parse_empty_type_argument_list_is_not_a_zero_arity_generic() {
        // `type_list` and `result_types` both open with a mandatory
        // `type_annotation`, so `<>` is a *failed* generic. `result` alone is
        // fine — its whole argument list is optional.
        parse_err("component A { x: tuple<>; }");
        parse_err("component A { x: result<>; }");
        parse_ok("component T { a: result; }");
    }

    #[test]
    fn parse_type_list_rejects_a_trailing_comma() {
        parse_err("component A { x: tuple<s32,>; }");
        parse_ok("component A { x: tuple<s32, string>; }");
    }

    #[test]
    fn parse_primitive_names_are_not_reserved() {
        // `color` is a legal attribute name and a legal identifier.
        parse_ok("component T { div { color: #ff0000 } }");
        parse_ok("component T { string: s32 = 0; }");
    }
}
