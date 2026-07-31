//! Shared test support: locating the project's `.yel` inputs, and the **one**
//! mutation generator both test binaries use.
//!
//! # Why the generator lives here
//!
//! `corpus.rs` and `parity.rs` both mutate real programs, and `parity.rs`
//! records divergences by label — `<path>#delete@7`. That label indexes into a
//! *derived* list. Two copies of the generator with different constants would
//! silently re-point every allow-list entry at a different program while both
//! test binaries stayed green. One generator, one set of constants, one place.
//!
//! The corpus bodies are **not tracked** (2026-07-30): a fresh clone has
//! `corpus/src` absent or partial, and a stale one can be the wrong corpus
//! entirely. Historically the same hazard came from git-lfs pointer stubs —
//! 2000 files with the right names and 130 bytes each, satisfying every count.
//! Either way the counts below are asserted *and* so is the content; see
//! [`CORPUS_MIN_BYTES`]. A sweep that silently shrinks to the fixtures, or to
//! 2000 stubs, proves nothing.

#![allow(dead_code)]

use std::path::{Path, PathBuf};

pub const CORPUS_COUNT: usize = 2000;
/// 91 until 2026-07-29, when `global_filter_default.yel` moved to
/// `known_bugs/`. It wrote `filter(|x| x > 2)`; `|` is not an operator, so the
/// grammar's catch-all ate the line and the property it existed to guard was
/// never parsed. Corrected to `{ x -> x > 2 }` it panics the frozen compiler
/// (`hir/local_scope.rs:73`), which is a known bug, not a positive fixture.
/// See `plans/rewrite/goldens-changed.md`.
pub const POSITIVE_FIXTURE_COUNT: usize = 90;
pub const DIAGNOSTIC_FIXTURE_COUNT: usize = 23;
pub const EXAMPLE_COUNT: usize = 4;
pub const ALL_SOURCE_COUNT: usize =
    CORPUS_COUNT + POSITIVE_FIXTURE_COUNT + DIAGNOSTIC_FIXTURE_COUNT + EXAMPLE_COUNT;

pub fn workspace_root() -> PathBuf {
    // CARGO_MANIFEST_DIR is `<root>/crates/yelc-syntax`.
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("crate is two levels below the workspace root")
        .to_path_buf()
}

/// Every `*.yel` file directly inside `dir`, sorted by name for determinism.
///
/// Panics if the directory is unreadable: a silently empty `Vec` here is what
/// turns a 2118-file sweep into a 120-file one without failing anything.
pub fn yel_files(dir: &Path) -> Vec<PathBuf> {
    let entries =
        std::fs::read_dir(dir).unwrap_or_else(|e| panic!("cannot read {}: {e}", dir.display()));
    let mut files: Vec<PathBuf> = entries
        .map(|entry| entry.expect("directory entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "yel"))
        .collect();
    files.sort();
    files
}

/// Total bytes of `corpus/src`, as generated. A **content** check, not a count.
///
/// The corpus is git-lfs tracked, and an unpulled checkout is not empty: it is
/// 2000 pointer stubs of about 130 bytes each, which satisfies every count
/// assertion in this file. Four of the stage's six headline numbers reproduced
/// over pointer stubs, including "2118/2118 round-trip" (anti-spec A14).
const CORPUS_MIN_BYTES: usize = 4_000_000;

/// `corpus/src/*.yel`, sorted numerically so index-based sampling is stable.
pub fn corpus_sources() -> Vec<PathBuf> {
    let mut files = yel_files(&workspace_root().join("corpus/src"));
    files.sort_by_key(|path| {
        path.file_stem()
            .and_then(|stem| stem.to_str())
            .and_then(|stem| stem.parse::<u32>().ok())
            .unwrap_or(u32::MAX)
    });
    assert_eq!(
        files.len(),
        CORPUS_COUNT,
        "corpus/src holds {} programs, expected {CORPUS_COUNT} — regenerate with \
         `scripts/freeze-corpus.sh` at the freeze SHA in corpus/MANIFEST",
        files.len()
    );
    assert_corpus_content(&files);
    files
}

/// Assert the corpus is the corpus, not a directory of lfs pointers.
///
/// Two independent checks, because either alone is cheap to satisfy by accident:
/// the whole set is megabytes rather than kilobytes, and a sampled file actually
/// contains a `component` declaration.
fn assert_corpus_content(files: &[PathBuf]) {
    let total: usize = files
        .iter()
        .map(|path| {
            std::fs::metadata(path)
                .unwrap_or_else(|e| panic!("cannot stat {}: {e}", path.display()))
                .len() as usize
        })
        .sum();
    assert!(
        total >= CORPUS_MIN_BYTES,
        "corpus/src is {total} bytes, expected at least {CORPUS_MIN_BYTES} — \
         this looks like unpulled git-lfs pointer stubs, not Yel source. \
         Regenerate with `scripts/freeze-corpus.sh`."
    );
    for path in files.iter().step_by(499) {
        let content = read(path);
        assert!(
            content.contains("component "),
            "{} does not contain a component declaration — is it an lfs pointer?",
            path.display()
        );
    }
}

pub fn positive_fixtures() -> Vec<PathBuf> {
    let files =
        yel_files(&workspace_root().join("crates/yel-wasm-codegen/tests/fixtures/positive"));
    assert_eq!(
        files.len(),
        POSITIVE_FIXTURE_COUNT,
        "positive fixture count changed"
    );
    files
}

pub fn diagnostic_fixtures() -> Vec<PathBuf> {
    let files =
        yel_files(&workspace_root().join("crates/yel-wasm-codegen/tests/fixtures/diagnostics"));
    assert_eq!(
        files.len(),
        DIAGNOSTIC_FIXTURE_COUNT,
        "diagnostic fixture count changed"
    );
    files
}

/// `examples/**/*.yel` — a recursive walk.
pub fn example_sources() -> Vec<PathBuf> {
    let mut files = Vec::new();
    walk(&workspace_root().join("examples"), &mut files);
    files.sort();
    assert_eq!(files.len(), EXAMPLE_COUNT, "example count changed");
    files
}

fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries =
        std::fs::read_dir(dir).unwrap_or_else(|e| panic!("cannot read {}: {e}", dir.display()));
    for entry in entries {
        let path = entry.expect("directory entry").path();
        if path.is_dir() {
            walk(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "yel") {
            out.push(path);
        }
    }
}

/// Every `.yel` input the project owns.
pub fn all_sources() -> Vec<PathBuf> {
    let files: Vec<PathBuf> = corpus_sources()
        .into_iter()
        .chain(positive_fixtures())
        .chain(diagnostic_fixtures())
        .chain(example_sources())
        .collect();
    assert_eq!(files.len(), ALL_SOURCE_COUNT);
    files
}

/// Label a path relative to the workspace root, so labels are portable.
pub fn label(path: &Path) -> String {
    path.strip_prefix(workspace_root())
        .unwrap_or(path)
        .display()
        .to_string()
}

pub fn read(path: &Path) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()))
}

// ---------------------------------------------------------------------------
// The mutation generator — one copy, shared by corpus.rs and parity.rs
// ---------------------------------------------------------------------------

/// Number of pseudo-random cut points per seed, on top of the two endpoints.
const TRUNCATION_SAMPLES: usize = 24;
/// Number of whitespace-delimited chunks deleted per seed.
const DELETION_SAMPLES: usize = 16;
/// Fixed seed constant: the sweep must be reproducible run to run.
const MUTATION_SEED: u64 = 0x2545_f491_4f6c_dd1d;

/// Deterministic cut points, ascending and deduplicated.
///
/// **Changing `TRUNCATION_SAMPLES`, `DELETION_SAMPLES` or `MUTATION_SEED`
/// re-points every `#truncate@N` / `#delete@N` label** in
/// `parity.rs::KNOWN_DIVERGENCES`. That is why they live next to each other and
/// nowhere else.
pub fn truncation_offsets(content: &str) -> Vec<usize> {
    let len = content.len();
    if len == 0 {
        return vec![0];
    }
    let mut state = MUTATION_SEED ^ len as u64;
    let mut offsets = vec![0, len];
    for _ in 0..TRUNCATION_SAMPLES {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        let mut cut = (state % (len as u64 + 1)) as usize;
        while cut < len && !content.is_char_boundary(cut) {
            cut += 1;
        }
        offsets.push(cut);
    }
    offsets.sort_unstable();
    offsets.dedup();
    offsets
}

/// Drop one whitespace-delimited chunk at a time, at deterministic positions.
pub fn single_token_deletions(content: &str) -> Vec<String> {
    let chunks: Vec<(usize, usize)> = content
        .split_whitespace()
        .map(|chunk| {
            let start = chunk.as_ptr() as usize - content.as_ptr() as usize;
            (start, start + chunk.len())
        })
        .collect();
    if chunks.is_empty() {
        return Vec::new();
    }

    let step = (chunks.len() / DELETION_SAMPLES).max(1);
    chunks
        .iter()
        .step_by(step)
        .map(|&(start, end)| {
            let mut mutated = String::with_capacity(content.len());
            mutated.push_str(&content[..start]);
            mutated.push_str(&content[end..]);
            mutated
        })
        .collect()
}

// ---------------------------------------------------------------------------
// The randomized generator — committed, seeded, and the thing that ships
// ---------------------------------------------------------------------------
//
// `single_token_deletions` splits on **whitespace**, so it can never turn
// `"v={value}"` into `"{}"`. That is not a hypothetical gap: four S5 clusters
// lived under a passing sweep because no generator here could construct them,
// and the 300,000-input run that found the previous batch was never committed.
// Anti-spec A13 — the generator is what ships, not the counterexamples it found.

/// xorshift64*, so the sweep is reproducible run to run and machine to machine.
pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Rng {
        Rng(seed | 1)
    }

    pub fn next_u64(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    pub fn below(&mut self, bound: usize) -> usize {
        if bound == 0 {
            0
        } else {
            (self.next_u64() % bound as u64) as usize
        }
    }

    pub fn pick<'t, T>(&mut self, items: &'t [T]) -> &'t T {
        &items[self.below(items.len())]
    }
}

/// Fixed seed for every randomized sweep in this crate.
pub const RANDOM_SEED: u64 = 0x9E37_79B9_7F4A_7C15;

/// Characters worth inserting: the delimiters and operators whose *pairing* is
/// what the parser reasons about, plus a couple of ordinary ones.
const INTERESTING_BYTES: &[u8] = b"{}()[]<>\"'`,;:.@#$%^&*-+=|\\/!?~ \n\tabz09";

/// Byte- and character-level mutations of `content`, `count` of them.
///
/// Deliberately *not* token-aligned: deleting one byte of `"v={value}"` is how
/// `"{}"`, `"{ }"` and an unterminated `/*` get constructed, and every one of
/// those was an S5 counterexample that whitespace-chunk deletion could not
/// reach.
pub fn random_mutations(content: &str, count: usize, rng: &mut Rng) -> Vec<String> {
    let mut out = Vec::with_capacity(count);
    if content.is_empty() {
        return out;
    }
    for _ in 0..count {
        let mut bytes = content.as_bytes().to_vec();
        // One to three edits, so single-edit and compound breakage both occur.
        for _ in 0..=rng.below(3) {
            if bytes.is_empty() {
                break;
            }
            let at = rng.below(bytes.len());
            match rng.below(3) {
                0 => {
                    bytes.remove(at);
                }
                1 => bytes.insert(at, *rng.pick(INTERESTING_BYTES)),
                _ => bytes[at] = *rng.pick(INTERESTING_BYTES),
            }
        }
        // The parser takes `&str`; a mutation that split a UTF-8 sequence is
        // repaired rather than skipped, so the count is the count.
        out.push(String::from_utf8_lossy(&bytes).into_owned());
    }
    out
}

/// Fragments a token soup is assembled from — every delimiter, every keyword,
/// and a few literals.
const SOUP_TOKENS: &[&str] = &[
    // `extern component`, not `import component` — the keyword was renamed and
    // a soup built from the old spelling exercises a production neither parser
    // has had since.
    "component",
    "global",
    "record",
    "enum",
    "variant",
    "element",
    "extern",
    "package",
    "export",
    "func",
    "callback",
    "let",
    "if",
    "else",
    "for",
    "in",
    "out",
    "in-out",
    "key",
    "set",
    "bind",
    "children",
    "true",
    "false",
    "s32",
    "string",
    "list",
    "option",
    "tuple",
    "result",
    "A",
    "x",
    "a-b",
    "{",
    "}",
    "(",
    ")",
    "[",
    "]",
    "<",
    ">",
    ",",
    ";",
    ":",
    ".",
    "..",
    "..=",
    "@",
    "->",
    "?",
    "?.",
    "=",
    "==",
    "!=",
    "<=",
    ">=",
    "&&",
    "||",
    "!",
    "+",
    "-",
    "*",
    "/",
    "%",
    "+=",
    "-=",
    "1",
    "1.5",
    "8px",
    "50%",
    "#fff",
    "'c'",
    "\"s\"",
    "\"v={",
    "}t\"",
    "//c\n",
    "/*c*/",
    "/*",
    " ",
    "\n",
];

/// `count` random token soups of up to `max_tokens` fragments each.
///
/// The shape that found 446 S5 counterexamples in round 1, committed this time.
pub fn random_token_soups(count: usize, max_tokens: usize, rng: &mut Rng) -> Vec<String> {
    (0..count)
        .map(|_| {
            let len = 1 + rng.below(max_tokens);
            let mut source = String::new();
            for _ in 0..len {
                source.push_str(rng.pick(SOUP_TOKENS));
                if rng.below(3) == 0 {
                    source.push(' ');
                }
            }
            source
        })
        .collect()
}

/// Number of positive fixtures the sweeps mutate, chosen by content hash.
const FIXTURE_SEED_COUNT: usize = 30;

/// FNV-1a over the file's bytes. Any stable, content-only hash would do; this
/// one is four lines and has no dependency.
fn content_hash(bytes: &[u8]) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    hash
}

/// The fixed set of real programs both sweeps mutate.
///
/// # Selection is by **content**, not by position in a name-sorted listing
///
/// The predecessor strided `positive_fixtures()`, which is sorted by *file
/// name*. That made three pinned headline numbers — `MUTATION_SWEEP_CASES`, the
/// first-error-offset floor, and `KNOWN_DIVERGENCE_COUNT` — keyed to a
/// **filename**: renaming `imported_components.yel` to `extern_components.yel`
/// re-sampled the whole stride and turned the suite red for a change that
/// touched no parser code.
///
/// Selecting by content hash removes filenames from the sweep's definition
/// entirely. A rename cannot change which programs are mutated, how many cases
/// the sweep produces, or any number derived from it. (Editing a fixture's
/// *contents* still can — that is a real change to the input set, and one the
/// exact-count assertions exist to surface.)
///
/// The corpus half is already content-addressed in effect: `corpus/src/N.yel`
/// stems are numeric, frozen, and never renamed, so index striding over them is
/// stable. It is left alone so the `corpus/src/…#delete@N` labels in
/// `parity.rs` keep pointing at the same programs.
pub fn mutation_seeds() -> Vec<PathBuf> {
    let mut hashed: Vec<(u64, PathBuf)> = positive_fixtures()
        .into_iter()
        .map(|path| (content_hash(read(&path).as_bytes()), path))
        .collect();
    // Hash order, not path order: iteration order feeds the randomized sweep's
    // RNG stream, so it has to be rename-invariant too.
    hashed.sort_by_key(|(hash, _)| *hash);
    assert!(
        hashed.windows(2).all(|pair| pair[0].0 != pair[1].0),
        "two positive fixtures hash equal; the selection is no longer a total order"
    );

    let mut seeds: Vec<PathBuf> = hashed
        .into_iter()
        .take(FIXTURE_SEED_COUNT)
        .map(|(_, path)| path)
        .collect();
    assert_eq!(seeds.len(), FIXTURE_SEED_COUNT);

    let corpus = corpus_sources();
    for index in (0..corpus.len()).step_by(97) {
        seeds.push(corpus[index].clone());
    }
    seeds
}

// ---------------------------------------------------------------------------
// The `global` / `record` catch-all discard — the one documented divergence
// class, and the evidence check that keeps it honest.
// ---------------------------------------------------------------------------

/// The frozen parser's `BLOCK_LEVEL_CATCH_ALL` recovery, and the two places
/// `yel-core` throws it away.
///
/// # Why this lives in `support` and not in `parity.rs`
///
/// `corpus.rs` had its own copy of the exception — a one-name allow-list called
/// `FROZEN_SWALLOWS_SYNTAX_ERROR` — whose only guard was
/// `diagnostics > 0 && error_nodes > 0`, a statement about *this* parser rather
/// than the frozen one, and the literal shape anti-spec A10 names. It also sat
/// outside `DIVERGENCE_COUNT`, so the ratchet could not see it. One module, one
/// list, one evidence function, visible to both test binaries.
/// Approved surface widenings: inputs the frozen parser rejects and the new
/// one accepts because the language grew. Same discipline as
/// [`catch_all`] — the excuse is a checked property of the input, not a
/// label.
pub mod widenings {
    use yelc_base::{Diagnostics, SourceId};
    use yelc_syntax::token::TokenKind;

    /// 2026-07-31: `&`, `|` and hex integer literals became surface (the
    /// `.yelir` subset, user-approved — `plans/desugar/README.md` §1; the
    /// token-count pin in `token.rs` carries the entry). The frozen lexer
    /// hard-errors on all three ("expected `&&`"), so a mutation landing one
    /// inside otherwise-valid text diverges frozen-rejects/new-accepts. The
    /// evidence is read out of the new lexer: the input really contains a
    /// widened token.
    pub fn explains_frozen_rejection(content: &str) -> bool {
        let mut diags = Diagnostics::new();
        let lexed = yelc_syntax::lexer::lex(SourceId(0), content, &mut diags);
        let mut offset = 0usize;
        lexed.tokens.iter().zip(&lexed.widths).any(|(kind, width)| {
            let start = offset;
            offset += *width as usize;
            matches!(kind, TokenKind::AMP | TokenKind::PIPE)
                || (*kind == TokenKind::INT_LITERAL && content[start..offset].starts_with("0x"))
        })
    }
}

pub mod catch_all {
    use super::single_token_deletions;
    use yelc_base::{Diagnostics, Interner, SourceId};

    /// Divergences whose root cause is the catch-all discard.
    ///
    /// `grammar.pest` recovers from a malformed member with
    /// `BLOCK_LEVEL_CATCH_ALL`, which consumes the offending line so the
    /// surrounding declaration still matches. `yel-core/src/syntax/parser.rs`
    /// then reports that recovery in exactly two places — `parse_component`
    /// (:823) and `parse_element_node` (:1186). `parse_global` iterates its
    /// members with a trailing `_ => {}`, and `parse_record` filters its pairs
    /// with `if field_pair.as_rule() == Rule::record_field` (:321); both
    /// therefore **silently discard the catch-all**.
    ///
    /// So a `global` body or a `record` field list containing text the grammar
    /// cannot parse is accepted with no diagnostic at all, and the member
    /// vanishes.
    ///
    /// **No checked-in fixture is on this list any more.** `global_filter_default.yel`
    /// was, until 2026-07-29 — it wrote `[1, 2, 3, 4].filter(|x| x > 2)`, `|` is
    /// not an operator in this grammar, and the `.filter(…)` regression it
    /// documented was therefore never parsed while `yelc check` printed OK. It
    /// moved to `known_bugs/`, because corrected to `{ x -> x > 2 }` it panics
    /// the frozen compiler at `hir/local_scope.rs:73`. Every remaining entry is
    /// a *generated mutation*, so the new parser now reports zero error nodes
    /// across every hand-written fixture with no exceptions — which is the
    /// state this list should always be trending toward.
    ///
    /// Reproducing this would mean silently dropping a subtree, which invariant
    /// S5 and anti-spec A5 forbid. The new parser reports; the divergence is
    /// recorded.
    ///
    /// The claim "same root cause" is **checked**, not asserted in prose:
    /// [`explains_our_report`] is evidence read out of the *frozen* AST and
    /// tied to the position we reported at, so appending an over-rejection of
    /// ours to this list does not make the suite pass.
    pub const DIVERGENCES: &[&str] = &[
        // Mutations that land inside a `global` body or a `record` field list.
        "corpus/src/195.yel#delete@3",
        "corpus/src/389.yel#delete@4",
        "corpus/src/486.yel#delete@6",
        "corpus/src/680.yel#delete@1",
        "corpus/src/1068.yel#delete@2",
        "corpus/src/1068.yel#delete@7",
        "corpus/src/1165.yel#delete@3",
        "corpus/src/1456.yel#delete@3",
        "corpus/src/1747.yel#delete@11",
        "corpus/src/1747.yel#delete@13",
        "corpus/src/1747.yel#delete@14",
        "corpus/src/1941.yel#delete@1",
        "corpus/src/1941.yel#delete@13",
        // Mutations of the positive fixtures the seed set samples. Same class,
        // same characterization check. This block moved wholesale when
        // `mutation_seeds` stopped striding a name-sorted listing and started
        // selecting by content hash — the *class* is unchanged, the members are
        // whichever `global_*` / `record` fixtures the hash now picks.
        "crates/yel-wasm-codegen/tests/fixtures/positive/global_inout.yel#delete@10",
        "crates/yel-wasm-codegen/tests/fixtures/positive/globals.yel#delete@1",
        "crates/yel-wasm-codegen/tests/fixtures/positive/globals.yel#delete@2",
        "crates/yel-wasm-codegen/tests/fixtures/positive/globals.yel#delete@3",
        "crates/yel-wasm-codegen/tests/fixtures/positive/nested_records.yel#delete@11",
    ];

    /// Length ratchet. Every entry above is one place the new parser knowingly
    /// disagrees with the frozen one, and the count only ever goes **down**.
    ///
    /// Deliberately separate from the characterization: a check can be argued
    /// with, a number cannot. Growing the list is a diff to this line.
    ///
    /// 20 → 19 in review round 3: the rename-invariant `mutation_seeds` samples
    /// one fewer `global`-bodied fixture than the name-strided one did. Every
    /// entry is still the same catch-all class and still carries the same
    /// evidence check.
    ///
    /// 19 → 18 on 2026-07-29: `global_filter_default.yel` left `positive/` for
    /// `known_bugs/`, taking the list's only whole-file entry with it. The
    /// number went **down** by deleting an excuse, which is the only direction
    /// it is allowed to move without a justification per entry.
    pub const DIVERGENCE_COUNT: usize = 18;

    /// The whole-file entries — the checked-in fixtures whose *unmutated* text
    /// the frozen grammar does not accept. `corpus.rs`'s "known-good files
    /// produce no diagnostics" sweep excuses exactly these.
    pub fn whole_file_divergences() -> Vec<&'static str> {
        DIVERGENCES
            .iter()
            .copied()
            .filter(|entry| !entry.contains('#'))
            .collect()
    }

    /// Rebuild the source an entry names: the file, or the mutation of it.
    pub fn subject(entry: &str, content: &str) -> String {
        let Some((_, mutation)) = entry.split_once('#') else {
            return content.to_string();
        };
        let (kind, index) = mutation.split_once('@').expect("malformed mutation label");
        let index: usize = index.parse().expect("malformed mutation index");
        match kind {
            "truncate" => content[..index].to_string(),
            "delete" => single_token_deletions(content)
                .into_iter()
                .nth(index)
                .expect("mutation index out of range"),
            other => panic!("unknown mutation kind {other}"),
        }
    }

    /// Did the **frozen** parser accept this input while silently dropping a
    /// `global`/`record` member, **and is that drop what we are reporting**?
    ///
    /// Evidence about the frozen parser, tied to our report by *cause*. Three
    /// revisions are worth naming, because each fell to the same probe:
    ///
    /// 1. `new_member_count > frozen_member_count` — a tautology. The new
    ///    parser's recovery model materialises *any* unreadable member as a
    ///    member, so the inequality holds for every over-rejection in a `global`
    ///    or `record` body, including a deliberate tightening. A reviewer proved
    ///    it by flipping record field lists to `TrailingSep::Forbidden`, a pure
    ///    regression, and the check stayed green (anti-spec A10).
    /// 2. "some byte inside some `global`/`record` body is covered by no member"
    ///    — a statement about the *file*, not about the divergence. Adding one
    ///    silently-dropped global anywhere in a probe file made the same
    ///    trailing-comma tightening admissible again, because the check never
    ///    asked what our diagnostic had to do with the drop.
    /// 3. this one. Excise exactly the dropped bytes and re-parse: our
    ///    diagnostics must **go away**. That is the causal claim the entry
    ///    makes, stated as an experiment.
    ///
    /// Co-location was tried and is not enough in either direction. It is too
    /// weak — two unrelated defects in one `global` body satisfy it — and too
    /// strong: `corpus/src/680.yel#delete@1` drops `value: result<s32, string> =`
    /// and leaves the *next* member's name to be swallowed as the missing
    /// default, so our first diagnostic lands 11 bytes downstream, inside a
    /// member the frozen AST kept. Excision answers both: the cascade
    /// disappears with its cause, and an unrelated defect does not.
    ///
    /// Separators (`,` `;`) and trivia are not part of a dropped run: a comma
    /// between two members is by construction outside every member's span, so
    /// counting it would make every well-formed declaration look like a drop —
    /// and would hand the trailing-comma tightening exactly the run it needs.
    pub fn explains_our_report(content: &str) -> bool {
        let Some(runs) = dropped_runs(content) else {
            return false;
        };
        if runs.is_empty() || !we_report(content) {
            return false;
        }
        // Causality, not co-location. Excise exactly the bytes the frozen
        // parser threw away and re-parse: if what we were reporting was that
        // member, the report is gone. If it was something else — a trailing
        // comma we stopped accepting, a keyword we stopped splitting — it is
        // still there, and the entry is unexplained.
        !we_report(&excise(content, &runs))
    }

    /// `content` with every dropped run replaced by spaces of the same width,
    /// so every other offset in the file stays where it was.
    ///
    /// The separator that terminated the dropped member goes with it. A run is
    /// *evidence* of a drop and so is built out of real syntax only — a `,` or
    /// `;` on its own would make every well-formed declaration look dropped. But
    /// the member pest ate took its `;` with it, and leaving that `;` behind
    /// turns the experiment into "does an orphan semicolon parse", which it does
    /// not. Only the first separator after the run, and only across whitespace.
    fn excise(content: &str, runs: &[(usize, usize)]) -> String {
        let mut out = content.to_string();
        for &(start, end) in runs {
            let mut stop = end;
            while matches!(content.as_bytes().get(stop), Some(byte) if byte.is_ascii_whitespace()) {
                stop += 1;
            }
            if !matches!(content.as_bytes().get(stop), Some(b';' | b',')) {
                stop = end;
            } else {
                stop += 1;
            }
            let blank = " ".repeat(stop - start);
            out.replace_range(start..stop, &blank);
        }
        out
    }

    /// Did the **new** parser report anything?
    fn we_report(content: &str) -> bool {
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let _ = yelc_syntax::parse(SourceId(0), content, &interner, &mut diags);
        diags.has_errors()
    }

    /// Maximal runs of real syntax inside a `global`/`record` body that **no**
    /// frozen member span covers — `BLOCK_LEVEL_CATCH_ALL`'s leavings.
    ///
    /// `None` when the frozen parser did not accept the input cleanly, which is
    /// the first half of the characterization.
    pub fn dropped_runs(content: &str) -> Option<Vec<(usize, usize)>> {
        let result =
            yel_core::syntax::parser::parse_file_with_source_id(content, yel_core::SourceId(0))
                .ok()?;
        if !result.catched_errors.is_empty() {
            return None;
        }

        let globals = result.file.globals.iter().map(|global| {
            let members: Vec<_> = global
                .node
                .properties
                .iter()
                .map(|property| property.span)
                .chain(global.node.callbacks.iter().map(|callback| callback.span))
                .collect();
            (global.span, members)
        });
        let records = result.file.records.iter().map(|record| {
            let members: Vec<_> = record.node.fields.iter().map(|field| field.span).collect();
            (record.span, members)
        });

        let mut runs = Vec::new();
        for (decl, members) in globals.chain(records) {
            uncovered_runs(content, decl, &members, &mut runs);
        }
        Some(runs)
    }

    /// Append every maximal run of uncovered, non-trivia, non-separator tokens
    /// inside `decl`'s `{ … }`.
    fn uncovered_runs(
        content: &str,
        decl: yel_core::Span,
        members: &[yel_core::Span],
        out: &mut Vec<(usize, usize)>,
    ) {
        let Some(open) = content[decl.start..decl.end]
            .find('{')
            .map(|at| decl.start + at)
        else {
            return;
        };
        let Some(close) = content[open..decl.end].rfind('}').map(|at| open + at) else {
            return;
        };
        let body = open + 1..close;
        if body.is_empty() {
            return;
        }

        // Lex the body with the *new* lexer purely to classify bytes: it is the
        // one at hand that knows which runs are whitespace, comments and
        // separators.
        let mut diags = Diagnostics::new();
        let lexed = yelc_syntax::lexer::lex(SourceId(0), &content[body.clone()], &mut diags);

        let mut offset = body.start;
        let mut run: Option<(usize, usize)> = None;
        for (kind, width) in lexed.tokens.iter().zip(&lexed.widths) {
            let start = offset;
            offset += *width as usize;
            use yelc_syntax::token::TokenKind;
            let ignorable =
                kind.is_trivia() || matches!(kind, TokenKind::COMMA | TokenKind::SEMICOLON);
            let covered = members
                .iter()
                .any(|member| member.start <= start && start < member.end);
            if ignorable || covered {
                // Trivia does not break a run; a *covered* token does, because
                // it belongs to a member the frozen parser kept.
                if !ignorable {
                    out.extend(run.take());
                }
                continue;
            }
            match &mut run {
                Some((_, end)) => *end = offset,
                None => run = Some((start, offset)),
            }
        }
        out.extend(run);
    }
}
