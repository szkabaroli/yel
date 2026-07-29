//! `func<T>(…)` — LANGUAGE.md § Type Parameters.
//!
//! Regression cover for the dispatch bug this feature was built through: the
//! top of `parse_type` looked ahead for `(` only, so `func<T>(…)` fell through
//! to the named-type branch and `func` was read as an ordinary type name. That
//! is a **silent misparse** — the green tree held `NAMED_TYPE(FUNC_KW)` with
//! `<T>` stranded as sibling `ERROR` nodes — so nothing but a shape assertion
//! catches it.

mod support;

use yelc_base::{Diagnostics, Interner, SourceMap};

fn parse(source: &str) -> (String, usize) {
    let mut map = SourceMap::new();
    let id = map.add_inline(source.to_string());
    let interner = Interner::new();
    let mut diags = Diagnostics::new();
    let parsed = yelc_syntax::parse(id, source, &interner, &mut diags);
    assert_eq!(
        parsed.green.text(),
        source,
        "S1: green tree must round-trip"
    );
    (format!("{:?}", parsed.green.kind()), diags.error_count())
}

const GENERIC: &str = "package a:b@0.1.0;\nexport component App {\n  f: func<T>(a: T) -> T;\n  VStack { Text { \"x\" } }\n}\n";
const TWO_PARAMS: &str = "package a:b@0.1.0;\nexport global S {\n  m: func<T, U>(items: list<T>, f: func(item: T) -> U) -> list<U>;\n}\nexport component App { VStack { Text { \"x\" } } }\n";
const PLAIN: &str = "package a:b@0.1.0;\nexport component App {\n  f: func(a: s32) -> s32;\n  VStack { Text { \"x\" } }\n}\n";

#[test]
fn a_generic_signature_parses_without_diagnostics() {
    let (_, errors) = parse(GENERIC);
    assert_eq!(errors, 0, "`func<T>(a: T) -> T` must parse cleanly");
}

#[test]
fn several_type_parameters_parse() {
    let (_, errors) = parse(TWO_PARAMS);
    assert_eq!(errors, 0, "`func<T, U>` must parse cleanly");
}

/// The addition must be purely additive: a non-generic signature is unaffected.
#[test]
fn a_plain_signature_is_unchanged() {
    let (_, errors) = parse(PLAIN);
    assert_eq!(errors, 0);
}
