//! The .yelir subset (user-approved surface, 2026-07-31): `module` blocks,
//! `use` lists, member-form items with optional `extern`, surface `match`,
//! typed record literals, bitwise operators and hex literals.
//! `plans/desugar/counter.yelir` is the motivating artifact; the cases here
//! are its constructs, minimized — and the whole artifact is swept last, so
//! the subset's definition of done stays checked in.

use yelc_base::{Diagnostics, NameInterner, SourceId};

fn parse_clean(content: &str) {
    let interner = NameInterner::new();
    let mut diagnostics = Diagnostics::new();
    let _ = yelc_syntax::parse(SourceId(0), content, &interner, &mut diagnostics);
    assert!(
        !diagnostics.has_errors(),
        "expected a clean parse of {content:?}"
    );
}

fn parse_error_count(content: &str) -> usize {
    let interner = NameInterner::new();
    let mut diagnostics = Diagnostics::new();
    let _ = yelc_syntax::parse(SourceId(0), content, &interner, &mut diagnostics);
    diagnostics.iter().count()
}

#[test]
fn a_module_holds_items() {
    parse_clean("module Dom {\n    variant color { red, rgba(tuple<u8, u8, u8, u8>), }\n}\n");
}

#[test]
fn a_module_holds_use_lists_and_state() {
    parse_clean("module M {\n    use Dom.{ create-text, remove, };\n    registry: list<s32>;\n}\n");
}

/// Module-level state is legal only *inside* a module: at the root it would
/// make any `garbage: garbage;` line parse, which the keyword-prefix parity
/// class caught (`packagea:b;`).
#[test]
fn root_level_state_still_errors() {
    assert!(parse_error_count("registry: list<s32>;\n") > 0);
}

#[test]
fn extern_functions_carry_attributes() {
    parse_clean(
        "@import(name = \"create-element\")\ncreate-element: extern func(tag: string) -> u32;\n",
    );
}

#[test]
fn match_bit_operators_and_hex_parse() {
    parse_clean(
        "f: func(h: u32) -> u32 {\n    let a = h >> 16;\n    match h & 0xFFFF {\n        0 -> a\n        1 -> { let b = a; b }\n        2 -> match a > 1 { true -> 1 false -> 0 }\n    }\n}\n",
    );
}

#[test]
fn a_match_arm_may_be_one_assignment() {
    parse_clean("f: func(x: bool) { match x { true -> x = false false -> x = true } }\n");
}

#[test]
fn typed_record_literals_parse() {
    parse_clean("g: func() -> s32 {\n    let p = Pair { first: 1, second: 2 };\n    p.first\n}\n");
}

/// `match x {` — with typed record literals gated in scrutinee position, the
/// `{` opens the arms, not a record named `x`.
#[test]
fn a_match_scrutinee_brace_is_the_arm_block() {
    parse_clean("f: func(x: bool) -> s32 { match x { true -> 1 false -> 0 } }\n");
}

/// An empty `Name {}` is NOT a typed record literal: it would make any stray
/// identifier before braces parse (`elsex { }` — the let/if parity class).
#[test]
fn an_empty_typed_record_is_not_a_form() {
    assert!(parse_error_count("component A { div { f: { if a { } elsex { } } } }") > 0);
}

/// Nested generics still close as two `>` tokens: shifts are joined from
/// ADJACENT `<`/`>` pairs in the expression parser, never lexed.
#[test]
fn nested_generics_survive_the_shift_operators() {
    parse_clean("g: func(x: list<list<s32>>) -> s32 { 0 }\n");
}

#[test]
fn the_whole_desugar_artifact_parses() {
    let content = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../plans/desugar/counter.yelir"
    ))
    .expect("counter.yelir");
    parse_clean(&content);
}
