//! Corpus-scale verification of the stage-1 invariants.
//!
//! Every property here is **asserted**. An earlier revision of this file
//! printed two of them — `eprintln!("round-trip: {checked}/{checked}")`, and a
//! count of S5 counterexamples collected into a `silent` vector — and asserted
//! only `checked > 0`. Both passed while the property they named was false. The
//! rule this file now follows: if a number appears in a message, an assertion
//! above it has already constrained that number.
//!
//! 1. **S1/S2 — byte-for-byte green round-trip** over an asserted 2118 files.
//! 2. **Zero `Error` nodes** on inputs that parse today.
//! 3. **S5 — a diagnostic AND an `Error` node** for every ill-formed input.
//! 4. **S6 — parsing always terminates and always returns**, including on input
//!    nested past the recursion limit.
//!
//! Accept/reject parity against the frozen pest parser lives in `parity.rs`.

use yelc_base::{Diagnostics, NameInterner, SourceId};
use yelc_syntax::ast::visit::ErrorNodeCounter;

mod support;
use support::{
    ALL_SOURCE_COUNT, CORPUS_COUNT, POSITIVE_FIXTURE_COUNT, RANDOM_SEED, Rng, all_sources,
    corpus_sources, label, mutation_seeds, positive_fixtures, random_mutations, random_token_soups,
    read, single_token_deletions, truncation_offsets,
};

struct Outcome {
    round_tripped: bool,
    error_nodes: usize,
    diagnostics: usize,
    /// Byte offsets of every diagnostic, and of every recovery node.
    diagnostic_spans: Vec<(usize, usize)>,
    recovery_spans: Vec<(usize, usize)>,
}

impl Outcome {
    /// Invariant S5, **per construct**.
    ///
    /// The file-level form — `(diagnostics > 0) != (error_nodes > 0)` — is not
    /// the property: a file with one report-without-mark *and* one
    /// mark-without-report satisfies it, and a reviewer's deleted recovery mark
    /// stayed green across all 2225 mutated inputs because the file's other
    /// recovery positions supplied the missing count (anti-spec A12).
    ///
    /// So: every diagnostic must have a recovery node **at the same place**, and
    /// every recovery node must have a diagnostic there.
    ///
    /// "Same place" is: the spans overlap, or everything between them is
    /// trivia. The slack is not a fudge — the two sides genuinely have different
    /// natural widths. A recovery node covers the construct the parser gave up
    /// on (`record` at 24..30), while the diagnostic points at the token that is
    /// missing (offset 31, past the space). Requiring literal overlap would
    /// reject that correct pairing; allowing an arbitrary gap would let a
    /// diagnostic in one declaration answer for a mark in the next. Whitespace
    /// and comments are exactly the bytes that carry no construct.
    fn s5_violations(&self, content: &str) -> Vec<String> {
        let trivia = trivia_bytes(content);
        let near = |a: (usize, usize), b: (usize, usize)| {
            if a.0 <= b.1 && b.0 <= a.1 {
                return true;
            }
            let gap = if a.1 < b.0 { a.1..b.0 } else { b.1..a.0 };
            gap.clone()
                .all(|byte| trivia.get(byte).copied().unwrap_or(true))
        };

        let mut out = Vec::new();
        for diagnostic in &self.diagnostic_spans {
            if !self
                .recovery_spans
                .iter()
                .any(|mark| near(*diagnostic, *mark))
            {
                out.push(format!("diagnostic at {diagnostic:?} has no recovery node"));
            }
        }
        for mark in &self.recovery_spans {
            if !self
                .diagnostic_spans
                .iter()
                .any(|diagnostic| near(*mark, *diagnostic))
            {
                out.push(format!("recovery node at {mark:?} has no diagnostic"));
            }
        }
        out
    }
}

/// `out[i]` is true when byte `i` belongs to a whitespace or comment token.
fn trivia_bytes(content: &str) -> Vec<bool> {
    let mut diags = Diagnostics::new();
    let lexed = yelc_syntax::lexer::lex(SourceId(0), content, &mut diags);
    let mut out = vec![false; content.len()];
    let mut offset = 0usize;
    for (kind, width) in lexed.tokens.iter().zip(&lexed.widths) {
        let end = offset + *width as usize;
        if kind.is_trivia() {
            let stop = end.min(content.len());
            out[offset.min(stop)..stop].fill(true);
        }
        offset = end;
    }
    out
}

fn parse_source(content: &str) -> Outcome {
    let interner = NameInterner::new();
    let mut diags = Diagnostics::new();
    let parsed = yelc_syntax::parse(SourceId(0), content, &interner, &mut diags);

    let counter = ErrorNodeCounter::run(&parsed.ast);
    Outcome {
        round_tripped: parsed.green.text() == content
            && parsed.green.len() as usize == content.len(),
        error_nodes: counter.count,
        diagnostics: diags.len(),
        diagnostic_spans: diags
            .iter()
            .filter_map(|diagnostic| diagnostic.span)
            .map(|span| (span.start, span.end))
            .collect(),
        recovery_spans: counter
            .spans
            .iter()
            .map(|span| (span.start, span.end))
            .collect(),
    }
}

/// S1 + S2 over every `.yel` file in the repository.
#[test]
fn green_tree_round_trips_every_source_file() {
    let sources = all_sources();
    assert_eq!(
        sources.len(),
        ALL_SOURCE_COUNT,
        "the sweep must cover every input, not whatever happened to be readable"
    );

    let failures: Vec<String> = sources
        .iter()
        .filter(|path| !parse_source(&read(path)).round_tripped)
        .map(|path| label(path))
        .collect();

    assert!(
        failures.is_empty(),
        "green tree did not round-trip on {} of {ALL_SOURCE_COUNT} files: {:?}",
        failures.len(),
        &failures[..failures.len().min(12)]
    );
}

/// Inputs that parse today must produce no recovery nodes at all.
///
/// # The one exception, and how it is characterized
///
/// A "positive fixture" is positive for the *back-end*: it is a program the
/// compiler is supposed to compile. One of them is not grammatical.
/// `global_filter_default.yel` writes `[1, 2, 3, 4].filter(|x| x > 2)`, `|` is
/// not an operator in this grammar, and the frozen parser's
/// `BLOCK_LEVEL_CATCH_ALL` eats the whole member while `parse_global` discards
/// the recovery without a word — so `yelc check` prints OK and the regression
/// the fixture documents is never parsed at all.
///
/// The predecessor excused it by **file name**, and guarded the exception with
/// `diagnostics > 0 && error_nodes > 0` — a statement about *this* parser, which
/// any over-rejection of ours satisfies, and the literal shape anti-spec A10
/// names. It also lived here, outside `catch_all::DIVERGENCE_COUNT`, so the
/// ratchet could not see it.
///
/// Now: a fixture is excused only when [`support::catch_all::explains_our_report`]
/// says the **frozen** parser silently dropped a member and that drop is what we
/// are reporting — the same evidence `parity.rs` demands of every allow-list
/// entry — and the excused set is asserted to be exactly the whole-file entries
/// of that one shared list.
#[test]
fn corpus_and_positive_fixtures_have_no_error_nodes() {
    let sources: Vec<_> = corpus_sources()
        .into_iter()
        .chain(positive_fixtures())
        .collect();
    assert_eq!(sources.len(), CORPUS_COUNT + POSITIVE_FIXTURE_COUNT);

    let mut error_nodes = Vec::new();
    let mut unexpected_diagnostics = Vec::new();
    let mut excused = Vec::new();

    for path in &sources {
        let content = read(path);
        let outcome = parse_source(&content);
        if outcome.error_nodes == 0 && outcome.diagnostics == 0 {
            continue;
        }
        if support::catch_all::explains_our_report(&content) {
            excused.push(label(path));
            continue;
        }
        if outcome.error_nodes > 0 {
            error_nodes.push((label(path), outcome.error_nodes));
        }
        if outcome.diagnostics > 0 {
            unexpected_diagnostics.push((label(path), outcome.diagnostics));
        }
    }

    assert!(
        error_nodes.is_empty(),
        "{} known-good files produced Error nodes: {:?}",
        error_nodes.len(),
        &error_nodes[..error_nodes.len().min(12)]
    );
    assert!(
        unexpected_diagnostics.is_empty(),
        "{} known-good files produced diagnostics: {:?}",
        unexpected_diagnostics.len(),
        &unexpected_diagnostics[..unexpected_diagnostics.len().min(12)]
    );

    // The excused set is the ratchet's, exactly — not a second list that can
    // drift away from it, and not a count that can absorb a new member.
    assert_eq!(
        excused,
        support::catch_all::whole_file_divergences(),
        "the set of files excused by the frozen catch-all changed. Every entry \
         is under support::catch_all::DIVERGENCE_COUNT; adding one is a diff to \
         that number and needs its own justification in \
         plans/rewrite/stage-1-syntax.md"
    );
}

/// S5, asserted for every mutated input: a diagnostic and an `Error` node
/// travel together, or neither is present.
///
/// The previous revision collected the counterexamples into a `silent` vector
/// and printed the count. It was 89 of 750, and the test passed.
#[test]
fn mutations_satisfy_s5_and_never_lose_bytes() {
    let seeds = mutation_seeds();
    assert!(
        seeds.len() >= 50,
        "mutation seed set shrank: {}",
        seeds.len()
    );

    let mut cases = 0usize;
    let mut violations = Vec::new();
    let mut check = |label: String, subject: &str| {
        let outcome = parse_source(subject);
        assert!(outcome.round_tripped, "{label} lost bytes");
        for violation in outcome.s5_violations(subject) {
            violations.push((label.clone(), violation));
        }
        cases += 1;
    };

    for path in &seeds {
        let content = read(path);
        for cut in truncation_offsets(&content) {
            check(format!("{}#truncate@{cut}", label(path)), &content[..cut]);
        }
        for (index, mutated) in single_token_deletions(&content).into_iter().enumerate() {
            check(format!("{}#delete@{index}", label(path)), &mutated);
        }
    }

    assert_eq!(
        cases, MUTATION_SWEEP_CASES,
        "the mutation sweep changed size"
    );
    assert!(
        violations.is_empty(),
        "S5 violated per-construct on {} of {cases} mutated inputs: {:#?}",
        violations.len(),
        &violations[..violations.len().min(20)]
    );
}

/// Cases the deterministic truncate/delete sweep produces. Exact, so a sweep
/// that quietly shrinks fails instead of reporting "zero violations" over fewer
/// inputs (anti-spec A13).
///
/// **Rename-invariant.** `mutation_seeds` selects its fixtures by content hash,
/// not by position in a name-sorted listing, so renaming a fixture cannot move
/// this number. The predecessor could: an `imported_components.yel` →
/// `extern_components.yel` rename re-sampled the whole stride and turned three
/// headline numbers red without a line of parser code changing.
const MUTATION_SWEEP_CASES: usize = 2204;

/// Cases the randomized sweep produces.
const RANDOM_SWEEP_CASES: usize = 9_100;

/// The randomized half — byte-level mutations and token soups, fixed seed.
///
/// This is the generator that finds the *next* member of a class, not a
/// regression test for the members already found. `single_token_deletions`
/// splits on whitespace and so cannot construct `"{}"` from `"v={value}"`; four
/// S5 clusters lived underneath a passing sweep because of it (anti-spec A13).
#[test]
fn randomized_inputs_satisfy_s5_and_never_lose_bytes() {
    let mut rng = Rng::new(RANDOM_SEED);
    let seeds = mutation_seeds();
    let mut cases = 0usize;
    let mut violations = Vec::new();

    let mut check = |label: String, subject: &str| {
        let outcome = parse_source(subject);
        assert!(outcome.round_tripped, "{label} lost bytes on {subject:?}");
        for violation in outcome.s5_violations(subject) {
            violations.push((label.clone(), subject.to_string(), violation));
        }
        cases += 1;
    };

    for (index, path) in seeds.iter().enumerate() {
        let content = read(path);
        for (n, mutated) in random_mutations(&content, 100, &mut rng)
            .into_iter()
            .enumerate()
        {
            check(format!("random#{index}.{n}"), &mutated);
        }
    }
    for (index, soup) in random_token_soups(4_000, 24, &mut rng)
        .into_iter()
        .enumerate()
    {
        check(format!("soup#{index}"), &soup);
    }

    assert_eq!(
        cases, RANDOM_SWEEP_CASES,
        "the randomized sweep changed size"
    );
    assert!(
        violations.is_empty(),
        "S5 violated per-construct on {} of {cases} randomized inputs \
         (label, input, what): {:#?}",
        violations.len(),
        &violations[..violations.len().min(20)]
    );
}

/// The offsets of the program below at which a prefix is a **complete,
/// well-formed file** — the end of each top-level declaration, and the same
/// offset plus its newline, plus the whole program minus its trailing newline.
///
/// The earlier revision of this test asserted that no such offset existed
/// ("every cut other than 0 lands inside a construct"). Fifteen do, and the
/// frozen pest parser accepts all fifteen — so rejecting them would be a
/// language change, not a stricter test. Listing them keeps the assertion
/// sharp in both directions: a cut that stops reporting fails here just as
/// loudly as one that starts.
const WELL_FORMED_PREFIXES: &[usize] = &[
    23, 24, // package yel:demo@1.0.0;
    56, 57, // record R { … }
    82, 83, // enum E { … }
    116, 117, // variant V { … }
    143, 144, // element El { … }
    217, 218, // extern component C { … }
    280, 281, // export global Store { … }
    524, // the whole program, minus its trailing newline
];

/// The sharp half of S5, on a program built to break in every direction:
/// every truncation that lands *inside* a construct must report **and** mark,
/// and every truncation that lands on a construct boundary must do neither.
#[test]
fn truncation_inside_a_construct_always_reports_and_marks() {
    let content = "\
package yel:demo@1.0.0;
record R { field-a: list<s32>, }
enum E { case-a, case-b }
variant V { kind-a(s32), kind-b }
element El { label: s32; }
extern component C { name: string; func show(a: s32) -> bool; @children }
export global Store { in count: u32; toggle: func(on: bool); }
export component App {
    value: s32 = 0;
    export on-click: func(a: s32) -> s32;
    if value > 0 {
        VStack { label: \"v={value}\", set text: { value += 1; }, @children }
    } else {
        for item in 0..5 key(item) { \"x\" }
    }
}
";
    // Cut 0 is the empty file, which is well-formed; so is every offset in
    // WELL_FORMED_PREFIXES. Every *other* cut lands inside a construct and must
    // therefore be reported and marked.
    let mut checked = 0usize;
    let mut clean = Vec::new();
    for cut in 1..content.len() {
        if !content.is_char_boundary(cut) {
            continue;
        }
        let outcome = parse_source(&content[..cut]);
        assert!(outcome.round_tripped, "cut at {cut} lost bytes");
        // S5 itself, per construct, at every single cut.
        let violations = outcome.s5_violations(&content[..cut]);
        assert!(
            violations.is_empty(),
            "cut at {cut} violates S5 ({violations:?}): {:?}",
            &content[..cut]
        );
        if outcome.diagnostics == 0 {
            clean.push(cut);
        } else {
            assert!(
                outcome.error_nodes >= 1,
                "cut at {cut} produced a diagnostic but no Error node: {:?}",
                &content[..cut]
            );
        }
        checked += 1;
    }
    assert_eq!(checked, 524, "the truncation set changed size");
    assert_eq!(
        clean, WELL_FORMED_PREFIXES,
        "the set of truncations that parse cleanly changed"
    );

    // The empty prefix is the one well-formed truncation.
    let empty = parse_source("");
    assert_eq!(empty.diagnostics, 0);
    assert_eq!(empty.error_nodes, 0);
}

/// Number of hand-written recovery positions below. Exact, so a case cannot be
/// quietly dropped when it starts failing.
const RECOVERY_POSITION_COUNT: usize = 26;

/// Hand-written recovery positions, each asserting both halves of S5.
///
/// Every entry below was a measured counterexample at review time: a
/// diagnostic with no `Error` node anywhere in the tree.
#[test]
fn every_recovery_position_produces_an_error_node() {
    let cases = [
        // list positions
        "enum E { Foo, bar }",
        "record R { 42 }",
        "record R { a: s32, 42 }",
        "variant V { 42 }",
        "component A { f: func(42); }",
        "component A { x: s32 = f(*); }",
        "component A { x: list<s32> = [*]; }",
        // A record literal needs at least one `name:` field to *be* a record
        // literal — `{ 42 }` alone is a `closure_no_params` whose body is the
        // trailing expression `42`, which the frozen grammar accepts and this
        // parser must keep accepting. The recovery position is a *bad field* in
        // a list that already committed to being a record.
        "component A { x: R = { a: 1, 42 }; }",
        "component A { x: tuple<*>; }",
        // member positions
        "global G { 42 }",
        "component A { 42; }",
        "element E { 42 }",
        "extern component C { 42 }",
        // holes inside an otherwise-present node
        "package ;",
        "package a:;",
        "component A { export x: s32; }",
        "component A { : s32; }",
        "component A { f: func a: s32); }",
        "component A { for in xs { \"a\" } }",
        "component A { if x \"a\" }",
        "component A { div { f: { if a b(); } } }",
        "component A { x: s32 = a.; }",
        "component A { x: s32 = a.b.; }",
        "component A { div { f: { : s32 -> 1 } } }",
        // arity holes that must not be truncated away
        "component A { x: result<s32, string, bool>; }",
        // an expression that simply ends
        "component A { x: s32 = ",
    ];

    assert_eq!(cases.len(), RECOVERY_POSITION_COUNT);

    let mut missing = Vec::new();
    for case in cases {
        let outcome = parse_source(case);
        assert!(outcome.round_tripped, "{case:?} lost bytes");
        if outcome.diagnostics == 0 || outcome.error_nodes == 0 {
            missing.push((case, outcome.diagnostics, outcome.error_nodes));
        }
        for violation in outcome.s5_violations(case) {
            missing.push((case, outcome.diagnostics, outcome.error_nodes));
            eprintln!("{case:?}: {violation}");
        }
    }
    assert!(
        missing.is_empty(),
        "S5 violated (input, diagnostics, error nodes): {missing:#?}"
    );
}

/// S6 on the input that used to `abort()` the process: deep nesting.
///
/// `parse` has no failure path, so "it returned" is most of the assertion —
/// this test exists because before the depth guard the parser did not return,
/// it `SIGABRT`ed on stack overflow, which no `catch_unwind` and no
/// accumulate-and-continue policy survives.
#[test]
fn deep_nesting_terminates_instead_of_overflowing_the_stack() {
    let limit = yelc_syntax::parser::MAX_NESTING_DEPTH;
    // Around the guard, and far past the ~1500 that overflowed a debug stack.
    let counts = [limit / 2, limit, limit + 1, limit * 4, 20_000];

    for count in counts {
        for (name, open, close) in [
            ("paren", "(", ")"),
            ("bracket", "[", "]"),
            ("record-brace", "{ k: ", " }"),
            ("element", "div { ", " }"),
        ] {
            for closed in [true, false] {
                let mut source = String::from("component A { x: s32 = ");
                source.push_str(&open.repeat(count));
                source.push('1');
                if closed {
                    source.push_str(&close.repeat(count));
                    source.push_str("; }");
                }
                let outcome = parse_source(&source);
                assert!(
                    outcome.round_tripped,
                    "{name}x{count} (closed={closed}) lost bytes"
                );
                if count > limit {
                    assert!(
                        outcome.diagnostics >= 1 && outcome.error_nodes >= 1,
                        "{name}x{count} exceeded the depth limit without reporting"
                    );
                }
            }
        }
    }

    // Prefix operators recurse without passing through a bracket.
    let source = format!("component A {{ x: s32 = {}1; }}", "-".repeat(20_000));
    assert!(parse_source(&source).round_tripped);

    // Nested types, and nested statements inside a closure.
    let source = format!(
        "component A {{ x: {}s32{}; }}",
        "list<".repeat(5_000),
        ">".repeat(5_000)
    );
    assert!(parse_source(&source).round_tripped);
    let source = format!(
        "component A {{ div {{ f: {{ {} }} }} }}",
        "if a { ".repeat(5_000)
    );
    assert!(parse_source(&source).round_tripped);
}

/// Real programs must stay far below the recursion limit — a guard that trips
/// on ordinary input would be a language change, not a safety net.
///
/// **Both** numbers, because they are different numbers. `measure_max_depth` is
/// recursion inside `parse_*`, which is what `MAX_NESTING_DEPTH` bounds.
/// `green.max_depth()` is the depth of the *structure* every consumer then
/// recurses over, which nothing bounds: `parse_binary`/`parse_postfix` are loops,
/// so a flat `a.b.b.b…` chain nests one node per link while the parser's counter
/// reads 2 (anti-spec A11). The predecessor asserted headroom on the first alone,
/// and so reported a 12x margin on exactly the inputs that aborted.
#[test]
fn real_programs_stay_well_under_both_depth_limits() {
    let mut deepest_parse = 0usize;
    let mut deepest_tree = 0usize;
    let mut deepest_tree_file = String::new();

    for path in all_sources() {
        let content = read(&path);
        deepest_parse = deepest_parse.max(yelc_syntax::parser::measure_max_depth(&content));

        let interner = NameInterner::new();
        let mut diags = Diagnostics::new();
        let parsed = yelc_syntax::parse(SourceId(0), &content, &interner, &mut diags);
        let tree = parsed.green.max_depth();
        if tree > deepest_tree {
            deepest_tree = tree;
            deepest_tree_file = label(&path);
        }
    }

    assert!(
        deepest_parse * 4 <= yelc_syntax::parser::MAX_NESTING_DEPTH,
        "deepest real program nests {deepest_parse}, limit is {} — less than 4x headroom",
        yelc_syntax::parser::MAX_NESTING_DEPTH
    );
    assert!(
        deepest_tree <= 100,
        "deepest real green tree is {deepest_tree} ({deepest_tree_file}); this is \
         the number every consumer recurses over, and it has grown unexpectedly"
    );
    eprintln!(
        "deepest real program: parse depth {deepest_parse}, \
         green depth {deepest_tree} ({deepest_tree_file})"
    );
}

/// The flat chains `MAX_NESTING_DEPTH` does **not** bound, at a length past the
/// point the frozen parser `abort()`s.
///
/// Measured, debug `cargo test` thread, on `component A { x: s32 = <chain>; }`:
/// the frozen pest parser overflows its stack at n ≈ 14,544, and before review
/// round 2 this parser's own consumers died first — `ast::visit` at n ≈ 3,126,
/// `Drop` at n ≈ 4,979, `green.text()` — the invariant-S1 check itself — at
/// n ≈ 12,983, all on **valid, diagnostic-free** input that `parse` returned
/// from happily. Each of the four is exercised below.
#[test]
fn flat_operator_chains_survive_past_the_frozen_parsers_ceiling() {
    // Comfortably past the frozen parser's ~14,544, and past every measured
    // consumer ceiling.
    const LINKS: usize = 40_000;

    for (name, body) in [
        ("member", format!("a{}", ".b".repeat(LINKS))),
        ("add", format!("1{}", " + 1".repeat(LINKS))),
        ("index", format!("a{}", "[0]".repeat(LINKS))),
    ] {
        let source = format!("component A {{ x: s32 = {body}; }}");
        let interner = NameInterner::new();
        let mut diags = Diagnostics::new();
        let parsed = yelc_syntax::parse(SourceId(0), &source, &interner, &mut diags);

        assert_eq!(diags.len(), 0, "{name}: a flat chain is valid input");
        // S1, on a tree deeper than the check used to survive.
        assert_eq!(parsed.green.text(), source, "{name}: green tree lost bytes");
        assert!(
            parsed.green.max_depth() > LINKS,
            "{name}: the chain did not actually nest ({} levels)",
            parsed.green.max_depth()
        );
        // The walk.
        assert_eq!(ErrorNodeCounter::run(&parsed.ast).count, 0);
        // …and teardown of both trees.
        drop(parsed);
    }
}
