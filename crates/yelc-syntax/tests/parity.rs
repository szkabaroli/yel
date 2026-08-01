//! Accept/reject parity with the frozen pest parser.
//!
//! Stage 1 has no downstream consumer yet, so artifact-level differential
//! testing is not available. This is the stand-in the stage file specifies: run
//! both front-ends over the corpus, the fixtures, and a mutation set, and assert
//! they agree on **which** inputs produce a syntax error. A silently-tightened
//! (or silently-widened) grammar shows up here and nowhere else.
//!
//! # What "rejects" means, on each side
//!
//! *Frozen*: `yel-core`'s driver turns both outcomes of
//! `parse_file_with_source_id` into the same `E0060` diagnostic — a hard pest
//! failure, and any `CATCH_ALL` node it recovered from. So "rejects" is
//! `Err(_) || !catched_errors.is_empty()`, which is exactly what `yelc check`
//! surfaces.
//!
//! *New*: at least one diagnostic in the sink. Parsing never fails, by
//! construction (invariant S6).
//!
//! # Why a dev-dependency on the frozen crate
//!
//! This is a differential harness, not a bridge: it exists only under
//! `cfg(test)`, nothing in `yelc-syntax` links it, and it is deleted with the
//! frozen tree at cutover phase 4. Shelling out to `yelc check` two thousand
//! times would measure the same thing an order of magnitude slower.

use yelc_base::{Diagnostics, NameInterner, SourceId};

mod support;
use support::{
    RANDOM_SEED, Rng, corpus_sources, diagnostic_fixtures, example_sources, mutation_seeds,
    positive_fixtures, random_mutations, single_token_deletions, truncation_offsets,
};

/// Divergences this run is allowed to have. The list is exact in both
/// directions: a new divergence fails the test, and so does one that stops
/// diverging.
///
/// One class, one list, one evidence check — all three in
/// [`support::catch_all`], because `corpus.rs` needs the same three and used to
/// carry its own weaker copy.
fn known_divergences() -> Vec<&'static str> {
    support::catch_all::DIVERGENCES.to_vec()
}

#[test]
fn the_divergence_allow_list_does_not_grow() {
    assert_eq!(
        known_divergences().len(),
        support::catch_all::DIVERGENCE_COUNT,
        "the allow-list changed size; every entry is a place the rewrite \
         knowingly disagrees with the frozen parser and needs its own written \
         justification in plans/rewrite/stage-1-syntax.md"
    );
}

fn frozen_rejects(content: &str) -> bool {
    match yel_core::syntax::parser::parse_file_with_source_id(content, yel_core::SourceId(0)) {
        Ok(result) => !result.catched_errors.is_empty(),
        Err(_) => true,
    }
}

fn new_rejects(content: &str) -> bool {
    let interner = NameInterner::new();
    let mut diags = Diagnostics::new();
    let _ = yelc_syntax::parse(SourceId(0), content, &interner, &mut diags);
    diags.has_errors()
}

struct Report {
    checked: usize,
    diverged: Vec<(String, bool, bool)>,
}

impl Report {
    fn new() -> Report {
        Report {
            checked: 0,
            diverged: Vec::new(),
        }
    }

    fn compare(&mut self, label: &str, content: &str) {
        self.checked += 1;
        let frozen = frozen_rejects(content);
        let new = new_rejects(content);
        if frozen != new {
            self.diverged.push((label.to_string(), frozen, new));
        }
    }

    /// Exact-set assertion: unexplained divergences fail, and so do entries in
    /// [`KNOWN_DIVERGENCES`] that stopped diverging. Both directions matter —
    /// a silently *fixed* divergence means the allow-list is now lying.
    fn assert_agrees(&self, what: &str) {
        let known = known_divergences();
        let unexplained: Vec<_> = self
            .diverged
            .iter()
            .filter(|(label, _, _)| !known.contains(&label.as_str()))
            .collect();
        assert!(
            unexplained.is_empty(),
            "{} of {} {what} disagree (label, frozen_rejects, new_rejects): {:#?}",
            unexplained.len(),
            self.checked,
            &unexplained[..unexplained.len().min(25)]
        );
        eprintln!(
            "{what}: {} checked, {} known divergence(s)",
            self.checked,
            self.diverged.len()
        );
    }
}

/// Label a path relative to the workspace root, so the allow-list is portable.
fn label(path: &std::path::Path) -> String {
    path.strip_prefix(support::workspace_root())
        .unwrap_or(path)
        .display()
        .to_string()
}

#[test]
fn accept_reject_parity_over_the_corpus() {
    let mut report = Report::new();
    for path in corpus_sources() {
        let content = std::fs::read_to_string(&path).expect("corpus file");
        report.compare(&label(&path), &content);
    }
    assert_eq!(report.checked, 2000, "the corpus should hold 2000 programs");
    report.assert_agrees("corpus programs");
}

#[test]
fn accept_reject_parity_over_the_fixtures() {
    let mut report = Report::new();
    for path in positive_fixtures()
        .into_iter()
        .chain(diagnostic_fixtures())
        .chain(example_sources())
    {
        let content = std::fs::read_to_string(&path).expect("fixture");
        report.compare(&label(&path), &content);
    }
    // 118 until 2026-07-29, when `global_filter_default.yel` moved from
    // `positive/` to `known_bugs/` — see `plans/rewrite/goldens-changed.md`.
    assert_eq!(report.checked, 117);
    report.assert_agrees("fixtures and examples");
}

/// The half that actually finds tightened grammars: inputs neither parser has
/// ever seen. Truncations and single-token deletions of real programs.
#[test]
fn accept_reject_parity_over_mutations() {
    let mut report = Report::new();

    // The generator lives in `support` and is shared with `corpus.rs`. It used
    // to be duplicated here with *different* constants, which silently re-pointed
    // every `#delete@N` label in KNOWN_DIVERGENCES at a different program while
    // both test binaries stayed green.
    for path in mutation_seeds() {
        let content = std::fs::read_to_string(&path).expect("seed");
        let name = label(&path);

        for cut in truncation_offsets(&content) {
            report.compare(&format!("{name}#truncate@{cut}"), &content[..cut]);
        }
        for (index, mutated) in single_token_deletions(&content).into_iter().enumerate() {
            report.compare(&format!("{name}#delete@{index}"), &mutated);
        }
    }

    assert_eq!(
        report.checked, 2204,
        "the mutation sweep changed size; the `#delete@N` labels in \
         support::catch_all::DIVERGENCES are indices into a derived list and move with it"
    );
    report.assert_agrees("mutations");
}

/// Randomized mutations per seed program. 51 seeds × 60 = the sweep size below.
const RANDOM_PARITY_MUTATIONS_PER_SEED: usize = 60;

/// Cases the randomized parity sweep produces. Exact, so a sweep that quietly
/// shrinks fails instead of reporting "zero divergences" over fewer inputs.
const RANDOM_PARITY_CASES: usize = 3_060;

/// The randomized generator, wired to the **oracle** — not only to S5.
///
/// `corpus.rs` already ran `random_mutations` against invariant S5 and the
/// round-trip; this file only ever compared against the frozen parser over the
/// *deterministic* truncate/delete sweep. That split is the defect anti-spec
/// A13 names: the generator that finds a class must be the one that ships,
/// wired to the property that matters. Run against the oracle for the first
/// time these 3,060 cases produced **42** divergences — 34 in the documented
/// catch-all class and 8 outside it, which were five separate real grammar
/// defects (the keyword split not checking its remainder, `func` unusable as a
/// name, `>=` closing no generic, `export componentApp`, a glued `else if`, a
/// glued `if`, and a trailing dot in a package version). After the fixes: 34
/// and **0**.
///
/// Token soups are deliberately **not** here. Both parsers reject essentially
/// every soup, so 30,000 of them yield zero divergences: soups are a good S5
/// generator and a useless parity generator. Mutations of real programs sit on
/// the accept/reject boundary, which is where disagreement lives.
///
/// # Why this has no label allow-list
///
/// A `#delete@7`-style label is a stable name for a deterministic mutation. A
/// randomized one has no such name, and a list of 34 opaque strings would be
/// unreadable and unmaintainable. So the excuse is a **checked property** of
/// the frozen parse instead: [`support::catch_all::explains_our_report`], the same
/// evidence `every_known_divergence_still_diverges` demands of every entry in
/// `support::catch_all::DIVERGENCES`. An over-rejection of ours cannot satisfy it.
#[test]
fn accept_reject_parity_over_random_mutations() {
    let mut rng = Rng::new(RANDOM_SEED);
    let mut checked = 0usize;
    let mut catch_all = 0usize;
    let mut widened = 0usize;
    let mut unexplained: Vec<(String, bool, bool)> = Vec::new();

    for path in mutation_seeds() {
        let content = std::fs::read_to_string(&path).expect("seed");
        let name = label(&path);
        for (index, mutated) in
            random_mutations(&content, RANDOM_PARITY_MUTATIONS_PER_SEED, &mut rng)
                .into_iter()
                .enumerate()
        {
            checked += 1;
            let frozen = frozen_rejects(&mutated);
            let new = new_rejects(&mutated);
            if frozen == new {
                continue;
            }
            if !frozen && new && support::catch_all::explains_our_report(&mutated) {
                catch_all += 1;
                continue;
            }
            // The opposite direction: the mutation landed a token the surface
            // gained after the freeze, checked against the new lexer.
            if frozen && !new && support::widenings::explains_frozen_rejection(&mutated) {
                widened += 1;
                continue;
            }
            unexplained.push((format!("{name}#random@{index}: {mutated:?}"), frozen, new));
        }
    }

    assert_eq!(
        checked, RANDOM_PARITY_CASES,
        "the randomized parity sweep changed size"
    );
    eprintln!(
        "random parity: {checked} checked, {catch_all} catch-all divergence(s), \
         {widened} widening(s), {} unexplained",
        unexplained.len()
    );
    assert!(
        unexplained.is_empty(),
        "{} of {checked} randomized mutations disagree with the frozen parser \
         outside the catch-all class (label, frozen_rejects, new_rejects): {:#?}",
        unexplained.len(),
        &unexplained[..unexplained.len().min(25)]
    );
}

/// Hand-written cases aimed at the places where a hand-rolled lexer is most
/// likely to disagree with pest: unreserved keywords, the possessive `?`, the
/// `{` alternatives, kebab identifiers, and trailing separators.
///
/// The corpus is machine-generated and stylistically uniform, so it exercises
/// almost none of this. These are checked against the frozen parser rather than
/// against an expectation the author wrote down, so a wrong belief about the
/// grammar fails the test instead of being encoded into it.
#[test]
fn accept_reject_parity_over_handwritten_edge_cases() {
    let cases = [
        // unreserved keywords in name positions
        "component A { string: s32 = 0; }",
        "component A { color: s32 = 0; }",
        "component A { export: s32 = 0; }",
        "component A { div { color: #ff0000 } }",
        "component A { if { span { \"x\" } } }",
        "component A { set { span { \"x\" } } }",
        "component A { for { span { \"x\" } } }",
        // possessive `?` on prop_modifier
        "component A { div { set: 5 } }",
        "component A { div { bind: 5 } }",
        "component A { div { set value: 5 } }",
        "component A { div { bind text: y } }",
        // attr_name shape
        "component A { div { count-1: 5 } }",
        "component A { div { fontSize: 5 } }",
        "component A { div { my_prop: 5 } }",
        "component A { div { Foo: 5 } }",
        // kebab identifiers versus subtraction
        "component A { x: s32 = count-1; }",
        "component A { x: s32 = count - 1; }",
        "component A { x: s32 = count- 1; }",
        "component A { x: s32 = a--b; }",
        // the `{` alternatives
        "component A { div { f: {} } }",
        "component A { div { f: { 10 } } }",
        "component A { div { f: { k: 1 } } }",
        "component A { div { f: { k: s32 -> k } } }",
        "component A { div { f: { p -> p } } }",
        "component A { div { f: { p, q -> p } } }",
        "component A { div { f: { p q -> p } } }",
        "component A { div { f: { a: 1, b: { c: 2 } } } }",
        // trailing separators
        "record R { a: s32, }",
        "enum E { a, }",
        "enum E { A }",
        "variant V { a(s32), }",
        "component A { x: list<s32> = [1,]; }",
        "component A { x: s32 = f(1,); }",
        "component A { f: func(a: s32,); }",
        "component A { x: tuple<s32,>; }",
        "component A { x: tuple<s32> = (1,); }",
        "component A { x: s32 = (1); }",
        "component A { x: s32 = (); }",
        // strings, escapes, interpolation
        "component A { div { \"a{b}c\" } }",
        "component A { div { \"{ [\"a\", \"b\"][1] }\" } }",
        "component A { div { \"a}b\" } }",
        "component A { div { \"a\\\"b\" } }",
        "component A { div { \"{}\" } }",
        "component A { x: char = 'x'; }",
        "component A { x: char = '\\n'; }",
        "component A { x: char = ' '; }",
        "component A { x: char = 'ab'; }",
        // numbers and units
        "component A { x: length = 8px; }",
        "component A { x: duration = 10s; }",
        "component A { x: s32 = 10second; }",
        "component A { x: percent = 50%; }",
        "component A { x: s32 = 50 % 3; }",
        "component A { x: color = #fff; }",
        "component A { x: color = #ff; }",
        // ranges and ternaries
        "component A { for i in 0..5 { \"x\" } }",
        "component A { for i in 0..=5 { \"x\" } }",
        "component A { x: s32 = a ? b : c ? d : e; }",
        "component A { x: s32 = a?.b; }",
        // call bases
        "component A { x: s32 = (1)(2); }",
        "component A { x: s32 = a.b(1); }",
        "component A { x: s32 = a[0](1); }",
        // packages
        "package a:b@1.0.0;\ncomponent A {}",
        "package a:b;\ncomponent A {}",
        "package a:b@1;\ncomponent A {}",
        "component A {}\npackage a:b;",
        // statements
        "component A { div { f: { let x: s32 = 1; x } } }",
        "component A { div { f: { if a { b(); } else { c(); } } } }",
        "component A { div { f: { if a { b } } } }",
        "component A { div { f: { count-=1; } } }",
        "component A { div { f: { count -= 1; } } }",
        // globals invert property/function order
        "global S { f: func(); }",
        "global S { in x: s32; }",
        "global S { in-out x: s32; }",
        "global S { callback c(a: s32); }",
        // Block comments. **Zero** of the 2118 checked-in `.yel` files contain
        // one, so nothing else in this repository exercises the lexer's
        // `/* … */` path or its unterminated case.
        "/* a block comment */\ncomponent A {}",
        "component /* between */ A {}",
        "component A { /* inside */ }",
        "component A { x: s32 = 1 /* mid-expression */ + 2; }",
        "component A {} /* unterminated",
        "/**/component A {}",
        "/*/ component A {}",
        "component A { div { \"a\" /* in an element */ } }",
        // The `bind` prop modifier, which `LANGUAGE.md` does not mention and no
        // fixture uses.
        "component A { div { bind text: y } }",
        "component A { div { bind value: a.b } }",
        "component A { div { bind: 5 } }",
        // The legacy `callback name(…);` form inside a `global` — likewise
        // undocumented and unfixtured.
        "global S { callback c(a: s32); }",
        "global S { callback c(); }",
        "global S { callback c(a: s32, b: string) -> bool; }",
        "global S { callback c }",
        // Unit literals: no corpus file contains one.
        "component A { x: length = 8px; }",
        "component A { x: length = 1.5rem; }",
        "component A { x: duration = 100ms; }",
        "component A { x: duration = 10s; }",
        "component A { x: angle = 45deg; }",
        "component A { x: percent = 50%; }",
        "component A { x: physical-length = 3phx; }",
        "component A { x: s32 = 10second; }",
        "component A { x: s32 = 10inch; }",
        "component A { x: s32 = 8px + 2px; }",
        // empty and degenerate files
        "",
        "// only a comment",
        "/* only a block comment */",
        "{",
        "}",
        "\u{1F600}",
    ];
    // Exact, so a case cannot be quietly dropped when it starts failing.
    assert_eq!(cases.len(), 105);

    let mut report = Report::new();
    for (index, case) in cases.iter().enumerate() {
        report.compare(&format!("handwritten#{index}: {case:?}"), case);
    }
    report.assert_agrees("hand-written edge cases");
}

// ---------------------------------------------------------------------------
// Span fidelity
// ---------------------------------------------------------------------------
//
// The accept/reject oracle above records **one bit** per program, and every
// diagnostic this parser emits is `ErrorCode::SyntaxError`. So the definition of
// done's "same `ErrorCode` at the same construct" is vacuous on the code half
// and, until this section existed, unchecked on the construct half: round 1's
// `synthetic_ident` defect was *a span regression* — `package ;` reported its
// hole at offset 0 instead of 8 — and re-introducing it left every test green.
//
// Two checks, deliberately different in kind:
//
//  1. `first_error_lands_on_the_construct_that_is_wrong` — an exact table of
//     (input, byte offset). Sharp, readable, and it fails the moment a
//     diagnostic moves by one byte.
//  2. `first_error_offset_agrees_with_the_frozen_parser_as_often_as_before` — a
//     floor on how many mutated inputs report at *exactly* the frozen parser's
//     offset. Wide rather than sharp: no hand-written expectation to get wrong,
//     and a systematic drift shows up as the count falling through the floor.

/// Byte offset of the earliest diagnostic the **new** parser reports.
fn new_first_error_offset(content: &str) -> Option<usize> {
    let interner = NameInterner::new();
    let mut diags = Diagnostics::new();
    let _ = yelc_syntax::parse(SourceId(0), content, &interner, &mut diags);
    diags
        .iter()
        .filter_map(|diagnostic| diagnostic.span)
        .map(|span| span.start)
        .min()
}

/// Byte offset of the earliest failure the **frozen** parser reports — a
/// `CATCH_ALL` it recovered from, or the position pest gave up at.
fn frozen_first_error_offset(content: &str) -> Option<usize> {
    match yel_core::syntax::parser::parse_file_with_source_id(content, yel_core::SourceId(0)) {
        Ok(result) => result
            .catched_errors
            .iter()
            .map(|error| error.span.start)
            .min(),
        Err(yel_core::syntax::parser::ParseError::Syntax { span, .. }) => {
            span.map(|span| span.start)
        }
        Err(_) => None,
    }
}

/// Every recovery position, with the byte offset its diagnostic must point at.
///
/// The offsets are *ours*, not the frozen parser's: pest reports where the whole
/// grammar gave up, which is a different question. What this pins is that each
/// diagnostic names the construct that is actually wrong — the property round 1
/// fixed and nothing then checked.
const FIRST_ERROR_OFFSETS: &[(&str, usize)] = &[
    //           0123456789
    ("package ;", 8),
    ("package a:;", 10),
    ("package a:b@;", 11),
    ("component A { export x: s32; }", 24),
    ("component A { : s32; }", 14),
    ("component A { x: s32 = a.; }", 25),
    ("component A { x: s32x = 0; }", 17),
    ("component A { x: bool = trueish; }", 24),
    ("component A { x: tuple<s32,>; }", 27),
    ("component A { x: s32 = f(1,); }", 27),
    ("record R { a: list<s32 }", 23),
    ("record R { a: s32, 42 }", 19),
    ("enum E { Foo }", 9),
    ("component A { for in xs { \"a\" } }", 21),
    ("component A { div { \"{}\" } }", 22),
    ("element E { a: s32 = 1; }", 19),
    ("global G { 42 }", 11),
    ("component A { 42; }", 14),
    ("extern component C { 42 }", 21),
];

#[test]
fn first_error_lands_on_the_construct_that_is_wrong() {
    let mut wrong = Vec::new();
    for (source, expected) in FIRST_ERROR_OFFSETS {
        match new_first_error_offset(source) {
            Some(actual) if actual == *expected => {}
            other => wrong.push((source, *expected, other)),
        }
    }
    assert!(
        wrong.is_empty(),
        "the first diagnostic moved off the construct it describes \
         (input, expected offset, actual): {wrong:#?}"
    );
}

/// How many mutated inputs both parsers reject *at the same byte offset*.
///
/// A floor, not an equality: the two front-ends are allowed to disagree about
/// where a broken program went wrong — pest reports where the whole grammar gave
/// up, recursive descent reports where the production it was in gave up. What is
/// not allowed is for that agreement to quietly erode, which is what a
/// systematic span regression looks like at scale.
///
/// Measured at the value below; raise it when it goes up, never lower it.
///
/// # Re-derived in review round 3, and why that is not a lowered floor
///
/// The previous value, 586, was measured over the *name-strided* seed set. That
/// set no longer exists: `mutation_seeds` now selects fixtures by content hash
/// so a rename cannot move the sweep, and the rename that prompted the change
/// had already re-sampled the old stride. There is no run that produces 586 any
/// more, in either parser, so it is not a number this suite can be held to.
///
/// What replaced it is **two** floors, because a bare count can be satisfied by
/// shrinking the denominator:
///
/// * this count, re-measured at 548 of 1336 mutually rejected inputs, and
/// * [`FIRST_ERROR_OFFSET_AGREEMENT_PERCENT`], the *rate*.
///
/// For calibration the same parser scores 561 of 1408 (39.8%) over the legacy
/// name-strided selection, against 547 of 1336 (40.9%) here — so the input set
/// moved, the agreement rate did not fall.
///
/// Re-pinned 548 → 547 when the kebab lookahead landed. The denominator is
/// unchanged at 1336, so **exactly one** input's first-error offset moved — not
/// the "whole class" the assertion below warns about. Expected: the change moves
/// where an identifier ends, so for some malformed inputs it moves where the
/// first error is reported. Recorded in `plans/rewrite/goldens-changed.md`.
const FIRST_ERROR_OFFSET_AGREEMENTS: usize = 547;

/// Floor on the agreement *rate*, in whole percent.
///
/// A count alone is not a ratchet: halving the sweep halves the numerator and
/// the assertion still reads as "fewer disagreements". This is the half that
/// notices.
const FIRST_ERROR_OFFSET_AGREEMENT_PERCENT: usize = 40;

#[test]
fn first_error_offset_agrees_with_the_frozen_parser_as_often_as_before() {
    let mut both_reject = 0usize;
    let mut agreed = 0usize;

    for path in mutation_seeds() {
        let content = std::fs::read_to_string(&path).expect("seed");
        let mut check = |subject: &str| {
            if !frozen_rejects(subject) || !new_rejects(subject) {
                return;
            }
            both_reject += 1;
            if frozen_first_error_offset(subject) == new_first_error_offset(subject) {
                agreed += 1;
            }
        };
        for cut in truncation_offsets(&content) {
            check(&content[..cut]);
        }
        for mutated in single_token_deletions(&content) {
            check(&mutated);
        }
    }

    let percent = agreed * 100 / both_reject;
    eprintln!(
        "first-error offset: {agreed} of {both_reject} mutually rejected inputs agree ({percent}%)"
    );
    assert!(
        agreed >= FIRST_ERROR_OFFSET_AGREEMENTS,
        "first-error offsets now agree on {agreed} of {both_reject} inputs, \
         down from {FIRST_ERROR_OFFSET_AGREEMENTS}. Something moved a whole \
         class of diagnostics; do not lower the floor to make this pass."
    );
    assert!(
        percent >= FIRST_ERROR_OFFSET_AGREEMENT_PERCENT,
        "first-error offsets agree on {percent}% of {both_reject} inputs, down \
         from {FIRST_ERROR_OFFSET_AGREEMENT_PERCENT}%. The count floor above can \
         be met by shrinking the sweep; this one cannot."
    );
}

/// The **keyword-prefix class**, in both directions, one case per member.
///
/// `grammar.pest` spells its *type* keywords as bare string literals with no
/// word boundary, so one matches a *prefix* of the identifier the lexer would
/// produce. That is a **widening** risk (frozen rejects, a naive hand-written
/// lexer accepts): `primitive_type` and `result_type` are complete matches on
/// their own, so `s32x` is `s32` followed by a stray `x` and the enclosing
/// production dies. Reproduced — `parser/types.rs::type_keyword_prefix_of`.
///
/// The *construct* keywords used to behave the same way in the other direction
/// — `recordFoo { }` was a `record` named `Foo`, which no FIRST set over token
/// kinds could predict — and the parser carried `eat_keyword` and a text-based
/// `parse_item` predictor to reproduce it. Both compilers now give those
/// keywords a **word boundary**, so `recordFoo` is one identifier and the rows
/// below record the rejection instead. The boundary was deliberately *not*
/// applied to `primitive_type` or to `unit_suffix` (an ordered prefix match by
/// design — `10second` is `10s` + `econd`), which is why the widening half of
/// this table is unchanged.
///
/// Every row is checked against the frozen parser rather than against a written
/// expectation, so this test states where each member *currently sits* and fails
/// the moment one moves — in either direction, and a wrong belief about the
/// grammar fails the test instead of being encoded into it.
#[test]
fn accept_reject_parity_over_the_keyword_prefix_class() {
    let cases = [
        // -- widening half: a primitive/`result` prefix kills the enclosing rule
        "component A { x: s32x = 0; }",
        "component A { x: strings = 0; }",
        "component A { x: charx = 0; }",
        "component A { x: int8 = 0; }",
        "component A { x: resultx = 0; }",
        "component A { x: colorx = 0; }",
        "component A { x: boolean = 0; }",
        "component A { x: f32x = 0; }",
        "component A { x: lengthy = 0; }",
        "component A { x: physical-lengthx = 0; }",
        "component A { x: relative-font-sizes = 0; }",
        "component A { x: brushes = 0; }",
        "component A { x: imagex = 0; }",
        "component A { x: easingx = 0; }",
        "component A { x: u8s = 0; }",
        "component A { x: floats = 0; }",
        "component A { x: intx = 0; }",
        "component A { x: percenty = 0; }",
        "component A { x: durations = 0; }",
        "component A { x: angles = 0; }",
        // …and the three that are *not* in the class: `list`/`option`/`tuple`
        // each need a `<`, so a prefix match fails and `named_type` matches.
        "component A { x: listx = 0; }",
        "component A { x: optionx = 0; }",
        "component A { x: tuplex = 0; }",
        // the class reaches every position a type can appear in
        "global G { x: s32x = 0; }",
        "record R { a: s32x }",
        "record R { a: s32x, }",
        "element E { a: s32x; }",
        "extern component C { a: s32x; }",
        "component A { f: func(a: s32x); }",
        "component A { x: list<s32x>; }",
        // `bool_literal = "true" | "false"` is tried before `identifier` in
        // `primary`, so this half of the class reaches expressions too.
        "component A { x: bool = trueish; }",
        "component A { x: bool = falsey; }",
        // -- rows where a keyword-shaped prefix does *not* change the outcome.
        //
        // These used to be the subtle half: `input:` inside a `global` was
        // direction `in` on a property called `put`, and `settings: 1` was the
        // modifier `set` on an attribute called `tings` — same accept/reject
        // bit, different tree. The word boundary makes each of them the plain
        // name it looks like, in both compilers.
        "global S { input: s32; }",
        "global S { outputs: s32; }",
        "component A { div { settings: 1 } }",
        "component A { div { bindings: 1 } }",
        "component A { div { keyx: 1 } }",
        "component A { f: funcx(); }",
        "component A { div { f: { letx = 1; } } }",
        "component A { for i in 0..2 keyx(i) { \"a\" } }",
        // `if` and `else` were the two keywords with no prefix row here at all,
        // and they were the two with no prefix *split* in the parser: `ifx` was
        // an element, `letx` an assignment, and neither moved a bit this test
        // can see. Their accept/reject rows go here; the misreading they used to
        // cause is `tests/identity.rs`'s job.
        "component A { ifx { div { } } }",
        "component A { div { f: { ifx { g(); } } } }",
        "component A { elsex: s32 = 0; }",
        "component A { if a { \"x\" } elsex { \"y\" } }",
        "component A { if a { \"x\" } elseif b { \"y\" } }",
        // Moved here from `tests/identity.rs` when the keyword word boundary
        // landed: each one used to be accepted by both parsers (as a *glued*
        // construct) and is now rejected by both, so it is an accept/reject row
        // rather than a construct-identity one.
        "component A { ifo > 0 { \"a\" } }",
        "component A { div { iff (a) { \"\" } } }",
        "component A { if a { \"a\" } elseif b { \"c\" } }",
        "component A { iftrue { \"x\" } else if false { \"y\" } }",
        "component A { div { f: { lets: s32 = 1; } } }",
        "component A { div { f: { ifa > 0 { b(); } } } }",
        "component A { div { f: { ifx.a { b(); } } } }",
        // -- the former narrowing half: a keyword site used to take the prefix
        // and read on. Every row here was **accepted** before the word boundary
        // and is **rejected** now, by both compilers together.
        "recordFoo { a: s32, }",
        "componentFoo { }",
        "enumFoo { a }",
        "variantFoo { a }",
        "elementFoo { a: s32; }",
        "externcomponent C { }",
        "exportcomponent A { }",
        "exportglobal G { }",
        "packagea:b;",
        "component A { forx in xs { \"a\" } }",
        // `in` is a bare literal too, so the split can nest: `for x iny { … }`
        // binds `x` and iterates `y`.
        "component A { forx iny { \"a\" } }",
        "component A { for x iny { \"a\" } }",
        // …and the alternatives pest would backtrack *out* of, which the guards
        // must not take: no `in` after `format`, no `(` after `callbacks`.
        "component A { format { \"a\" } }",
        "component A { div { format { \"a\" } } }",
        "global G { callbackc(a: s32); }",
        "global G { callbacks: s32; }",
        "global G { callbackx: func(); }",
        "global G { callback: func(); }",
        "component A { importantThing: s32 = 0; }",
        "component A { exports: s32 = 0; }",
        "component A { elsewhere: s32 = 0; }",
        "component A { intable: s32 = 0; }",
        "globalish { }",
        "recordy { a: s32, }",
        "elementary { a: s32; }",
        "enumerate { a }",
        "variants { a }",
        "componentcomponent A { }",
        "exportexport A { }",
        "recordrecord { }",
    ];

    let mut report = Report::new();
    for (index, case) in cases.iter().enumerate() {
        report.compare(&format!("keyword-prefix#{index}: {case:?}"), case);
    }
    assert_eq!(report.checked, 82);
    report.assert_agrees("keyword-prefix cases");
}

/// The `let` / `if` half of the keyword class, one case per member.
///
/// `let` and `if` are the two keywords the *statement* grammar spells, and they
/// were the last two to get a prefix split in the parser: while the frozen
/// grammar had no word boundary, `letx = 1;` was a binding called `x` and
/// `ifx { }` was `if x { }`. Both keywords now have a boundary in both
/// compilers, so `letx` and `ifx` are ordinary names — and about a third of the
/// rows below moved their accept/reject bit together as a result. `if` also
/// reaches *node* position, where `if_node` and `element_node` are both live,
/// and `else` reaches it through `else_if_branch* ~ else_branch?`.
///
/// Every row is checked against the frozen parser rather than against a written
/// expectation, so a wrong belief about the grammar fails the test instead of
/// being encoded into it. Accept/reject is only half of what these rows can go
/// wrong in — the other half, which construct each one *is*, is
/// `tests/identity.rs`.
#[test]
fn accept_reject_parity_over_the_let_and_if_keyword_class() {
    /// A statement block is only reachable through a closure prop.
    fn stmt(body: &str) -> String {
        format!("component A {{ div {{ f: {{ {body} }} }} }}")
    }
    /// A node position is a component body.
    fn node(body: &str) -> String {
        format!("component A {{ {body} }}")
    }

    let cases: Vec<String> = [
        // -- `let_statement = !GLUED_LET ~ "let" ~ identifier ~ (":" ~ type)? ~ "=" ~ expr ~ ";"`
        // The boundary means a name that merely begins with `let` is a name:
        // `letx`, `letters` and `lets` are all variables, not bindings.
        "let x = 1;",
        "letx = 1;",
        "letters = 1;",
        "lets: s32 = 1;",
        "letx: s32 = 1;",
        "let x: s32 = 1;",
        "let8 = 1;",
        "let-x = 1;",
        "let_x = 1;",
        // …and where `let` itself is used as an ordinary name, because no
        // `identifier` follows it: `assign_statement` matches instead.
        "let = 1;",
        "let;",
        "let.a = 1;",
        "let(1);",
        "let + 1;",
        "letx;",
        // the keyword against itself, and against the other one
        "let let = 1;",
        "letlet = 1;",
        "letif = 1;",
        "let x = let;",
        "letx = letx;",
        // -- `if_statement = !GLUED_IF ~ "if" ~ expr ~ "{" ~ statement* ~ "}" ~ (… "else" …)?`
        "if a { b(); }",
        "ifa { b(); }",
        "ifx > 0 { b(); }",
        "ifx.a { b(); }",
        "if8 { b(); }",
        "if-a { b(); }",
        "if_a { b(); }",
        "if { a: 1 } { b(); }",
        // …and where `if` is an ordinary name, because no `{` follows the
        // expression it would have started.
        "if = 1;",
        "if;",
        "if.a = 1;",
        "if(x);",
        "ifx = 1;",
        "iflet = 1;",
        // the `else` tail. `else_branch`'s `{` is *inside* the optional, so a
        // dangling `else` is not a malformed branch — pest backtracks out of the
        // whole option and reads it as the next statement.
        "if a { } else { }",
        "if a { } else }",
        "if a { } elsex { }",
        "if a { } elseb;",
        // -- node position, where `if_node` and `element_node` are both live
        "ifx { div { } }",
        "ife { div { } }",
        "iflex { color: red }",
        "iflex { color: red } else { \"x\" }",
        "if a { \"x\" } elseif b { \"y\" }",
        "letx { div { } }",
    ]
    .iter()
    .enumerate()
    // The last six rows are node-position; everything before them is a
    // statement. Split by count so a row cannot silently change position.
    .map(|(index, body)| if index < 38 { stmt(body) } else { node(body) })
    .collect();

    // Exact: this is the "44-case `let`/`if` parity check" the stage file cites.
    assert_eq!(cases.len(), 44);

    let mut report = Report::new();
    for (index, case) in cases.iter().enumerate() {
        report.compare(&format!("let-if#{index}: {case:?}"), case);
    }
    assert_eq!(report.checked, 44);
    report.assert_agrees("let/if keyword-class cases");
}

/// Every PEG optional in `grammar.pest`, at the boundary where its
/// **possessive commit** actually bites.
///
/// pest's `X?` does not backtrack once `X` has matched — but the *enclosing
/// alternative* still can, and which of the two happens is decided by what is
/// left after `X`. The suite already covered the easy side of that: an optional
/// followed by more input, where the leftover is obviously legal or obviously
/// not. What it never covered is the **empty-remainder** side — the optional
/// matches and then the production it belongs to has nothing left, or has only
/// its terminator. That is where four widenings and one narrowing were hiding:
///
/// * `result_type`'s `("<" ~ result_types ~ ">")?` and `tuple_type`'s
///   `type_list` both start with a **mandatory** `type_annotation`, so
///   `result<>` and `tuple<>` are *failed* generics, not empty ones. Both were
///   accepted here.
/// * `children_node = { "@children" ~ ";"? }` is one string literal, so pest's
///   implicit whitespace never runs inside it. `@ children` was accepted here.
/// * `tuple_literal`'s `","?` sits after an **optional** element group, so
///   `(1,,)` is a legal one-element tuple — and only `(1,,)`: `(1,,,)` and
///   `(1,2,,)` are not. It was rejected here.
///
/// Every row is checked against the frozen parser, not against an expectation.
#[test]
fn accept_reject_parity_over_the_peg_optional_boundaries() {
    let cases = [
        // `result_type = "result" ~ ("<" ~ result_types ~ ">")?`
        // `result_types = type_annotation ~ ("," ~ type_annotation)?`
        "component A { x: result; }",
        "component A { x: result<>; }",
        "component A { x: result<,>; }",
        "component A { x: result<s32>; }",
        "component A { x: result<s32,>; }",
        // `tuple_type = "tuple" ~ "<" ~ type_list ~ ">"`, `type_list` is `+`
        "component A { x: tuple<>; }",
        "component A { x: tuple<,>; }",
        "component A { x: tuple<s32,>; }",
        "component A { x: tuple<s32,,>; }",
        // `list`/`option` take exactly one, and have no optional at all
        "component A { x: list<>; }",
        "component A { x: option<>; }",
        "component A { x: list<,>; }",
        // `children_node = { "@children" ~ ";"? }` — one literal, no gap
        "component A { @children }",
        "component A { @children; }",
        "component A { @ children }",
        "component A { @ children; }",
        "component A { div { @ children } }",
        "component A { @/*c*/children }",
        "component A { @\nchildren }",
        "extern component C { @ children }",
        // `tuple_literal = "(" ~ expr ~ "," ~ (expr ~ ("," ~ expr)*)? ~ ","? ~ ")"`
        "component A { x: s32 = (1,); }",
        "component A { x: s32 = (1,,); }",
        "component A { x: s32 = (1,,,); }",
        "component A { x: s32 = (1,2,); }",
        "component A { x: s32 = (1,2,,); }",
        "component A { x: s32 = (1,2,3,); }",
        "component A { x: s32 = (,); }",
        // `list_literal = "[" ~ (expr ~ ("," ~ expr)* ~ ","?)? ~ "]"`
        "component A { x: s32 = []; }",
        "component A { x: s32 = [1,]; }",
        "component A { x: s32 = [1,,]; }",
        "component A { x: s32 = [,]; }",
        // `record_literal_fields = field ~ ("," ~ field)* ~ ","?`
        "component A { x: R = { a: 1, }; }",
        "component A { x: R = { a: 1,, }; }",
        // call arguments and `func_params` forbid a trailing separator
        "component A { x: s32 = f(); }",
        "component A { x: s32 = f(1,); }",
        "component A { x: s32 = f(,); }",
        "component A { f: func(,); }",
        "component A { f: func(a: s32,); }",
        // record / enum / variant lists allow exactly one
        "record R { , }",
        "record R { a: s32,, }",
        "enum E { , }",
        "enum E { a,, }",
        "variant V { , }",
        "variant V { a,, }",
        // `package_version = @{ "@" ~ ASCII_DIGIT+ ~ ("." ~ ASCII_DIGIT+)* }`
        "package a:b@;",
        "package a:b@1;",
        "package a:b@1.;",
        "package a:b@1.0;",
        "package a:b@1.0.;",
        "package a:b@1.0.0;",
        "package a:b@1.0.0.;",
        "package a:b@1.0.0.0;\ncomponent A {}",
        "package a:b@1.;\ncomponent A {}",
        "package a:b@1.0.;\ncomponent A {}",
        "package a:b@1.0.0.;\ncomponent A {}",
        "package a:b@x;\ncomponent A {}",
        "package a:b@1.x;\ncomponent A {}",
        // …and truncated to end of input, which is the boundary itself
        "package a:b@",
        "package a:b@1",
        "package a:b@1.",
        // `property_direction?` — `"in-out" | "in" | "out"`, possessive
        "global G { in x: s32; }",
        "global G { in: s32; }",
        "global G { in }",
        "global G { in",
        "global G { out }",
        "global G { in-out }",
        "global G { in-out: s32; }",
        "global G { inx: s32; }",
        "global G { in 8: s32; }",
        "global G { in-outx: s32; }",
        // `prop_modifier?` — `"set" | "bind"`, possessive
        "component A { div { set } }",
        "component A { div { set",
        "component A { div { bind } }",
        "component A { div { set x } }",
        "component A { div { set x: 1 } }",
        "component A { div { set: 1 } }",
        // `key_clause?` — `"key" ~ "(" ~ expr ~ ")"`
        "component A { for i in xs key { \"a\" } }",
        "component A { for i in xs key(i) { \"a\" } }",
        "component A { for i in xs key( { \"a\" } }",
        "component A { for i in xs key",
        "component A { for i in xs key() { \"a\" } }",
        "component A { for i in xs keys { \"a\" } }",
        // `("=" ~ expr)?` on a property declaration
        "component A { x: s32; }",
        "component A { x: s32 = ; }",
        "component A { x: s32 =; }",
        "component A { x: s32 =",
        "global G { x: s32 = ; }",
        // `func_return?` — `"->" ~ type_annotation`
        "component A { f: func(); }",
        "component A { f: func() -> s32; }",
        "component A { f: func() -> ; }",
        "component A { f: func() ->",
        "global G { callback c() -> ; }",
        "global G { callback c() ->; }",
        "extern component C { func f() -> ; }",
        "extern component C { func f() -> s32; }",
        // `closure_param_list` forbids a trailing separator
        "component A { div { f: { p, -> p } } }",
        "component A { div { f: { p, q -> p } } }",
        "component A { div { f: { p: s32, -> p } } }",
    ];
    // Exact, so a case cannot be quietly dropped when it starts failing.
    assert_eq!(cases.len(), 98);

    let mut report = Report::new();
    for (index, case) in cases.iter().enumerate() {
        report.compare(&format!("optional-boundary#{index}: {case:?}"), case);
    }
    report.assert_agrees("PEG optional boundaries");
}

/// The other direction of the ratchet: every allow-listed entry must still
/// diverge, **and must diverge for the reason its list documents**.
///
/// An allow-list that outlives the thing it allows is a silently weakened
/// assertion (anti-spec A7) — but so is one whose stated root cause nobody
/// checks. Without the assertions below, appending a real regression to the
/// list is enough to make this suite pass.
#[test]
fn every_known_divergence_still_diverges() {
    for entry in known_divergences() {
        let relative = entry.split('#').next().expect("a path");
        let path = support::workspace_root().join(relative);
        let content =
            std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{relative} is gone: {e}"));

        let subject = support::catch_all::subject(entry, &content);

        assert!(
            !frozen_rejects(&subject),
            "{entry}: the frozen parser now rejects this — drop the entry"
        );
        assert!(
            new_rejects(&subject),
            "{entry}: the new parser no longer reports this — drop the entry"
        );

        // …and the divergence is the one its list documents — evidence about
        // the frozen parser, not about ours.
        assert!(
            support::catch_all::explains_our_report(&subject),
            "{entry}: support::catch_all::DIVERGENCES says the frozen parser \
             accepted this input while a `BLOCK_LEVEL_CATCH_ALL` ate a \
             `global`/`record` member *at the place we reported*. Either no \
             byte inside a `global`/`record` body went uncovered, or the \
             uncovered run is somewhere else in the file than our diagnostic. \
             This entry has a different root cause — investigate it and give it \
             its own characterisation; do not loosen this check."
        );
    }
}
