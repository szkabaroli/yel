//! The single-namespace symbol table — the boundary, measured against the
//! frozen compiler.
//!
//! # Why this file has to exist
//!
//! `Definitions` keys names by [`Name`] alone. The frozen compiler keys them by
//! `(Name, Namespace)`, so it **accepts** programs this one now rejects:
//!
//! ```yel
//! record Point { x: s32, y: s32 }
//! export component Point { … }        → OK: 1 component(s) checked
//!
//! record S { x: s32 }
//! global S { y: s32 = 1; }            → OK: 1 component(s) checked
//! ```
//!
//! That is a deliberate, approved narrowing of the surface language
//! (`plans/rewrite/scope.md`, 2026-07-29) and the first that is not additive
//! apart from `return`.
//!
//! **`parity.rs` and `identity.rs` cannot see it**, and a green run there is not
//! evidence about this change. Two independent reasons, both checked here rather
//! than asserted:
//!
//! 1. Both suites compare *parsers*. This change is in name **registration**,
//!    which runs after parsing; every program below parses identically on both
//!    front-ends.
//! 2. Even a checking differential would not see it, because **none of the 2117
//!    corpus, fixture and example `.yel` files reuses a name across kinds** —
//!    measured by [`no_checked_in_program_reuses_a_name_across_kinds`], which is
//!    what makes the mutation sweeps blind rather than weak. Same shape as
//!    `return`, where the word occurred in none of them either.
//!
//! So the boundary is enumerated here, in both directions, with **premise and
//! consequence asserted on both compilers** — the frozen one really does accept
//! the narrowed set, and the new table really does reject it, and neither half
//! is allowed to rot into an unchecked claim.

use std::path::{Path, PathBuf};

use yelc_base::{Interner, SourceId, Span};
use yelc_sema::OverloadKey;
use yelc_sema::Ty;
use yelc_sema::definitions::{DefKind, Definitions, Sym};
use yelc_sema::ids::PackageId;

// ---------------------------------------------------------------------------
// The declaration forms, and the kind each one declares
// ---------------------------------------------------------------------------

/// One top-level declaration form.
///
/// `kind` is not taken on trust: [`each_form_declares_the_kind_this_table_says`]
/// reads it back out of the **frozen** compiler's own definition table, so the
/// pairings generated below cannot be testing a fiction.
struct Form {
    keyword: &'static str,
    kind: DefKind,
    declare: fn(&str) -> String,
}

/// Every form a program can use to claim a top-level name.
///
/// `Namespace::Value` is deliberately absent: the frozen tree registers a
/// source-declared name into `Type`, `Component` or `Global` and never into
/// `Value` — `Value` holds stdlib functions only (`stdlib_lookup.rs:620` is the
/// single `Namespace::Value` registration in the whole crate). So no program can
/// spell a value collision, and a `Value` row here would be a case that cannot
/// occur.
const FORMS: &[Form] = &[
    Form {
        keyword: "record",
        kind: DefKind::Type,
        declare: |name| format!("record {name} {{ x: s32 }}\n"),
    },
    Form {
        keyword: "enum",
        kind: DefKind::Type,
        declare: |name| format!("enum {name} {{ a, b }}\n"),
    },
    Form {
        keyword: "variant",
        kind: DefKind::Type,
        declare: |name| format!("variant {name} {{ a(s32), b }}\n"),
    },
    Form {
        keyword: "element",
        kind: DefKind::Component,
        declare: |name| format!("element {name} {{ x: s32; }}\n"),
    },
    Form {
        keyword: "extern component",
        kind: DefKind::Component,
        declare: |name| format!("extern component {name} {{ x: s32; }}\n"),
    },
    Form {
        keyword: "component",
        kind: DefKind::Component,
        declare: |name| format!("component {name} {{ text {{ content: \"hi\" }} }}\n"),
    },
    Form {
        keyword: "global",
        kind: DefKind::Global,
        declare: |name| format!("global {name} {{ y: s32 = 1; }}\n"),
    },
];

/// The name every generated program fights over.
const CLAIMED: &str = "Shared";

// ---------------------------------------------------------------------------
// The frozen oracle
// ---------------------------------------------------------------------------

/// Run the frozen compiler's `check`: parse, lower (which registers every
/// name), then type-check. Exactly what `yelc check` does — see
/// `crates/yelc/src/main.rs::check`.
///
/// Returns the rendered diagnostics when it rejects.
fn frozen_check(source: &str) -> Result<(), String> {
    let mut compiler = yel_core::Compiler::new();
    let Ok(file) = compiler.parse(source) else {
        return Err(compiler.render_diagnostics());
    };
    let items = compiler.lower_to_hir(&file);
    if compiler.has_errors() {
        return Err(compiler.render_diagnostics());
    }
    for item in &items {
        let _ = compiler.type_check(item);
    }
    if compiler.has_errors() {
        return Err(compiler.render_diagnostics());
    }
    Ok(())
}

/// The frozen namespace corresponding to one of our kinds.
fn frozen_namespace(kind: DefKind) -> yel_core::Namespace {
    match kind {
        DefKind::Type => yel_core::Namespace::Type,
        DefKind::Value => yel_core::Namespace::Value,
        DefKind::Component => yel_core::Namespace::Component,
        DefKind::Global => yel_core::Namespace::Global,
    }
}

// ---------------------------------------------------------------------------
// The new table
// ---------------------------------------------------------------------------

fn span() -> Span {
    Span::new(SourceId::new(0), 0, 1)
}

/// Register a sequence of `(name, kind)` declarations into a fresh symbol
/// table, stopping at the first rejection.
fn register_all(declarations: &[(&str, DefKind)]) -> Result<(), String> {
    let interner = Interner::new();
    let mut defs = Definitions::new(PackageId::LOCAL);
    for &(name, kind) in declarations {
        defs.register(interner.intern(name), kind, span(), false)
            .map_err(|collision| {
                let sources = yelc_base::SourceMap::new();
                collision.diagnostic(&interner, &sources).message
            })?;
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Premise: the form table is not a fiction
// ---------------------------------------------------------------------------

/// Every row of [`FORMS`] declares the kind it claims to, read back out of the
/// frozen compiler's own `Definitions`.
///
/// Without this the enumerations below could pair two forms the frozen tree puts
/// in the *same* namespace, "prove" it accepts the reuse, and be measuring
/// nothing.
#[test]
fn each_form_declares_the_kind_this_table_says() {
    for form in FORMS {
        let source = (form.declare)(CLAIMED);
        let mut compiler = yel_core::Compiler::new();
        let file = compiler
            .parse(&source)
            .unwrap_or_else(|e| panic!("`{}` does not parse: {e}", form.keyword));
        let _ = compiler.lower_to_hir(&file);
        assert!(
            !compiler.has_errors(),
            "`{}` alone must be a clean program, got:\n{}",
            form.keyword,
            compiler.render_diagnostics(),
        );

        let context = compiler.context();
        let name = context.intern(CLAIMED);
        assert!(
            context
                .defs
                .lookup(name, frozen_namespace(form.kind))
                .is_some(),
            "the frozen compiler does not register `{}` in {:?}",
            form.keyword,
            form.kind,
        );
        for &other in DefKind::ALL {
            if other == form.kind {
                continue;
            }
            assert!(
                context.defs.lookup(name, frozen_namespace(other)).is_none(),
                "`{}` also registers in {other:?}; the kind is not single-valued",
                form.keyword,
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Direction 1 — what this change takes away
// ---------------------------------------------------------------------------

/// Every ordered pair of forms declaring **different** kinds: the frozen
/// compiler accepts the reuse, the new table rejects it.
///
/// 30 programs — 3 type forms × 3 component forms, 3 type forms × 1 global form,
/// 3 component forms × 1 global form, each in both orders. Both halves are
/// asserted on every one of them: a frozen compiler that started rejecting these
/// would mean the narrowing had evaporated, and a new table that started
/// accepting them would mean it had been reverted. Neither can pass silently.
#[test]
fn the_frozen_compiler_accepts_every_name_reuse_this_change_rejects() {
    let mut pairs = 0;
    for first in FORMS {
        for second in FORMS {
            if first.kind == second.kind {
                continue;
            }
            pairs += 1;
            let source = format!("{}{}", (first.declare)(CLAIMED), (second.declare)(CLAIMED));

            frozen_check(&source).unwrap_or_else(|diagnostics| {
                panic!(
                    "PREMISE FAILED: the frozen compiler rejects `{}` + `{}` sharing a name, \
                     so this pair is not part of the narrowing:\n{diagnostics}",
                    first.keyword, second.keyword,
                )
            });

            // Control: each declaration alone registers cleanly. Without it,
            // "the new table rejects the pair" is also satisfied by a table
            // that rejects everything, and the consequence half would be
            // vacuous.
            assert!(
                register_all(&[(CLAIMED, first.kind)]).is_ok(),
                "the new table rejects `{}` on its own; the assertion below \
                 would then hold for the wrong reason",
                first.keyword,
            );

            let rejected = register_all(&[(CLAIMED, first.kind), (CLAIMED, second.kind)]);
            assert!(
                rejected.is_err(),
                "CONSEQUENCE FAILED: the new table accepts `{}` + `{}` sharing a name; \
                 the single-namespace narrowing is not in effect",
                first.keyword,
                second.keyword,
            );
        }
    }
    assert_eq!(
        pairs, 30,
        "the cross-kind enumeration changed size; a form was added or removed \
         without the boundary being re-measured",
    );
}

/// The two programs `scope.md` records verbatim, run as themselves rather than
/// as generated pairs.
#[test]
fn the_two_recorded_programs_are_accepted_by_the_frozen_compiler_and_rejected_here() {
    let recorded = [
        (
            "record Point { x: s32, y: s32 }\n\
             export component Point { text { content: \"hi\" } }\n",
            [("Point", DefKind::Type), ("Point", DefKind::Component)],
        ),
        (
            "record S { x: s32 }\n\
             global S { y: s32 = 1; }\n\
             export component App { text { content: \"hi\" } }\n",
            [("S", DefKind::Type), ("S", DefKind::Global)],
        ),
    ];

    for (source, declarations) in recorded {
        frozen_check(source).unwrap_or_else(|diagnostics| {
            panic!(
                "PREMISE FAILED: the frozen compiler no longer accepts a program \
                 `scope.md` records as accepted:\n{source}\n{diagnostics}"
            )
        });
        assert!(
            register_all(&declarations).is_err(),
            "CONSEQUENCE FAILED: the new table still accepts:\n{source}",
        );
    }
}

// ---------------------------------------------------------------------------
// Direction 2 — what is unchanged, and what this change adds
// ---------------------------------------------------------------------------

/// Reuse **within** a kind was already an error and still is: both compilers
/// reject it, so it is not part of the delta.
///
/// Included because the delta is only meaningful if the rest of the boundary is
/// shown to be unmoved — otherwise "the new table rejects it" would be
/// consistent with rejecting everything.
#[test]
fn both_compilers_reject_name_reuse_within_one_kind() {
    let mut pairs = 0;
    for first in FORMS {
        for second in FORMS {
            if first.kind != second.kind {
                continue;
            }
            pairs += 1;
            let source = format!("{}{}", (first.declare)(CLAIMED), (second.declare)(CLAIMED));

            let frozen = frozen_check(&source);
            assert!(
                frozen.is_err(),
                "the frozen compiler accepts `{}` + `{}` sharing a name in one \
                 namespace; that would be a widening, not a narrowing",
                first.keyword,
                second.keyword,
            );
            assert!(
                frozen.unwrap_err().contains("duplicate definition"),
                "`{}` + `{}` is rejected for some other reason than the duplicate",
                first.keyword,
                second.keyword,
            );

            assert!(
                register_all(&[(CLAIMED, first.kind), (CLAIMED, second.kind)]).is_err(),
                "the new table accepts `{}` + `{}` sharing a name",
                first.keyword,
                second.keyword,
            );
        }
    }
    assert_eq!(pairs, 19, "the same-kind enumeration changed size");
}

/// The one thing the new table accepts that the frozen table cannot: an
/// **overload set** — several values under one name, told apart by their
/// parameter types (decision B3).
///
/// Read off the frozen `Definitions` directly rather than through a program,
/// because no yel program can declare a value at the top level (see [`FORMS`]).
/// This is the widening half of the boundary; without it the change would be
/// pure loss.
#[test]
fn the_new_table_accepts_an_overload_set_the_frozen_table_cannot() {
    // Frozen: one slot per `(Name, Namespace)`. A second registration is
    // reported and dropped — there is nowhere for two to live.
    let frozen_interner = yel_core::Interner::new();
    let name = frozen_interner.intern("len");
    let mut frozen = yel_core::Definitions::new();
    let alloc = |frozen: &mut yel_core::Definitions| {
        frozen.alloc(
            name,
            yel_core::definitions::DefKind::Function(yel_core::definitions::FunctionDef {
                def_id: yel_core::DefId::INVALID,
                name,
                params: Vec::new(),
                ret_ty: yel_core::Ty::S32,
                is_export: false,
            }),
            yel_core::Span::new(yel_core::SourceId(0), 0, 1),
        )
    };
    let first = alloc(&mut frozen);
    let second = alloc(&mut frozen);
    assert!(
        frozen
            .register_name(name, yel_core::Namespace::Value, first)
            .is_none(),
        "the first registration is the one that takes the slot",
    );
    assert_eq!(
        frozen.register_name(name, yel_core::Namespace::Value, second),
        Some(first),
        "the frozen table reports the collision — it cannot hold both",
    );
    assert_eq!(
        frozen.lookup(name, yel_core::Namespace::Value),
        Some(first),
        "and the second is dropped, which is what a single slot means",
    );

    // New: the scope is multi-valued, so both live under the one name.
    let interner = Interner::new();
    let mut defs = Definitions::new(PackageId::LOCAL);
    let len = interner.intern("len");
    let on_string = defs
        .register_overload(
            len,
            span(),
            false,
            OverloadKey {
                params: vec![Ty::STRING],
            },
        )
        .expect("an overload set is representable");
    let on_list = defs
        .register_overload(
            len,
            span(),
            false,
            OverloadKey {
                params: vec![Ty::S32],
            },
        )
        .expect("a second, distinct key joins the set");
    assert_eq!(
        defs.lookup(len),
        [Sym::Value(on_string), Sym::Value(on_list)],
        "both overloads must be reachable, in registration order",
    );
}

// ---------------------------------------------------------------------------
// Why the mutation sweeps are blind — measured, not asserted
// ---------------------------------------------------------------------------

const CORPUS_COUNT: usize = 2000;
const POSITIVE_FIXTURE_COUNT: usize = 90;
const DIAGNOSTIC_FIXTURE_COUNT: usize = 23;
const EXAMPLE_COUNT: usize = 4;
const KNOWN_BUG_FIXTURE_COUNT: usize = 3;
const STDLIB_COUNT: usize = 4;
const CORE_EXAMPLE_COUNT: usize = 3;
const EDITOR_SAMPLE_COUNT: usize = 3;

/// Every `.yel` file the repository tracks.
///
/// **The sweep covers all of them.** It used to walk four hard-coded
/// directories — 2117 files — and the 13 it missed included `stdlib/`, which is
/// the one place a name is declared against the *builtin* inventory rather than
/// against another user declaration. A sweep that reports "no program does X"
/// while not reading every program is a sample presented as a census.
const TRACKED_YEL_FILES: usize =
    CORPUS_COUNT + POSITIVE_FIXTURE_COUNT + DIAGNOSTIC_FIXTURE_COUNT + EXAMPLE_COUNT
        + KNOWN_BUG_FIXTURE_COUNT + STDLIB_COUNT + CORE_EXAMPLE_COUNT + EDITOR_SAMPLE_COUNT;

/// The **only** tracked `.yel` file the frozen parser rejects, named rather
/// than skipped.
///
/// It is a diagnostics fixture whose whole job is to be unparseable. Every other
/// file parses, which is what lets the sweep assert `parsed == total` instead of
/// `parsed >= 2000` — the old bound let 117 files fail silently and the sweep
/// would still have reported a clean census.
const UNPARSEABLE: &[&str] = &["invalid_call_base.yel"];

/// The corpus is git-lfs tracked, and an unpulled checkout is 2000 pointer
/// stubs that satisfy every count. Bytes are the content check (anti-spec A14).
const CORPUS_MIN_BYTES: usize = 4_000_000;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("crate is two levels below the workspace root")
        .to_path_buf()
}

fn yel_files(dir: &Path) -> Vec<PathBuf> {
    let entries =
        std::fs::read_dir(dir).unwrap_or_else(|e| panic!("cannot read {}: {e}", dir.display()));
    let mut files: Vec<PathBuf> = entries
        .map(|entry| entry.expect("directory entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "yel"))
        .collect();
    files.sort();
    files
}

fn all_sources() -> Vec<PathBuf> {
    let root = workspace_root();
    let corpus = yel_files(&root.join("corpus/src"));
    assert_eq!(
        corpus.len(),
        CORPUS_COUNT,
        "corpus/src holds {} programs — run `git lfs pull`",
        corpus.len(),
    );
    let bytes: usize = corpus
        .iter()
        .map(|path| std::fs::metadata(path).expect("stat").len() as usize)
        .sum();
    assert!(
        bytes >= CORPUS_MIN_BYTES,
        "corpus/src is {bytes} bytes — these look like git-lfs pointer stubs, \
         not Yel source, and a sweep over them proves nothing",
    );

    let positive = yel_files(&root.join("crates/yel-wasm-codegen/tests/fixtures/positive"));
    assert_eq!(positive.len(), POSITIVE_FIXTURE_COUNT);
    let diagnostics = yel_files(&root.join("crates/yel-wasm-codegen/tests/fixtures/diagnostics"));
    assert_eq!(diagnostics.len(), DIAGNOSTIC_FIXTURE_COUNT);
    let mut examples = Vec::new();
    walk(&root.join("examples"), &mut examples);
    assert_eq!(examples.len(), EXAMPLE_COUNT);

    // The four that used to be missed. `stdlib/` is the one that matters —
    // it is where a declaration meets the *builtin* inventory rather than
    // another user declaration, and it is the only place in the tree where a
    // name is chosen against the language rather than against a program.
    let mut known_bugs = Vec::new();
    walk(
        &root.join("crates/yel-wasm-codegen/tests/fixtures/known_bugs"),
        &mut known_bugs,
    );
    assert_eq!(known_bugs.len(), KNOWN_BUG_FIXTURE_COUNT);
    let stdlib = yel_files(&root.join("stdlib"));
    assert_eq!(stdlib.len(), STDLIB_COUNT);
    let core_examples = yel_files(&root.join("crates/yel-core/examples"));
    assert_eq!(core_examples.len(), CORE_EXAMPLE_COUNT);
    let mut editor_samples = Vec::new();
    walk(&root.join("editors/vscode"), &mut editor_samples);
    assert_eq!(editor_samples.len(), EDITOR_SAMPLE_COUNT);

    let mut all = corpus;
    all.extend(positive);
    all.extend(diagnostics);
    all.extend(examples);
    all.extend(known_bugs);
    all.extend(stdlib);
    all.extend(core_examples);
    all.extend(editor_samples);
    assert_eq!(
        all.len(),
        TRACKED_YEL_FILES,
        "the sweep no longer covers every tracked `.yel` file",
    );
    all
}

/// The declarations one parsed file makes, as `(name, kind)` pairs.
///
/// Extracted so that [`the_extraction_finds_declarations`] can drive it with a
/// program whose answer is known. Without that, the sweep's "nothing reuses a
/// name" conclusion is satisfied just as well by an extraction that returns
/// nothing at all — which is exactly what the mutation `let declared = Vec::new()`
/// demonstrated, passing silently.
fn declarations_of(file: &yel_core::syntax::ast::File) -> Vec<(String, DefKind)> {
    let named = |names: Vec<&str>, kind: DefKind| {
        names
            .into_iter()
            .map(move |name| (name.to_string(), kind))
            .collect::<Vec<_>>()
    };

    let mut declared = Vec::new();
    declared.extend(named(
        file.records.iter().map(|d| d.node.name.as_str()).collect(),
        DefKind::Type,
    ));
    declared.extend(named(
        file.enums.iter().map(|d| d.node.name.as_str()).collect(),
        DefKind::Type,
    ));
    declared.extend(named(
        file.variants.iter().map(|d| d.node.name.as_str()).collect(),
        DefKind::Type,
    ));
    declared.extend(named(
        file.elements.iter().map(|d| d.node.name.as_str()).collect(),
        DefKind::Component,
    ));
    declared.extend(named(
        file.extern_components
            .iter()
            .map(|d| d.node.name.as_str())
            .collect(),
        DefKind::Component,
    ));
    declared.extend(named(
        file.components
            .iter()
            .map(|d| d.node.name.as_str())
            .collect(),
        DefKind::Component,
    ));
    declared.extend(named(
        file.globals.iter().map(|d| d.node.name.as_str()).collect(),
        DefKind::Global,
    ));
    declared
}

fn parse_frozen(source: &str) -> Option<yel_core::syntax::ast::File> {
    yel_core::syntax::parser::parse_file_with_source_id(source, yel_core::SourceId(0))
        .ok()
        .map(|parsed| parsed.file)
}

/// The sweep's instrument, checked against a program whose declarations are
/// known — **including one that reuses a name across kinds**, so the detector is
/// shown to fire as well as to find.
///
/// This is the control the sweep lacked. `no_checked_in_program_reuses_a_name_across_kinds`
/// asserts an empty result over 2130 files; an extraction returning `Vec::new()`
/// satisfies it perfectly, and did.
#[test]
fn the_extraction_finds_declarations() {
    let source = FORMS
        .iter()
        .map(|form| (form.declare)(&form.keyword.replace(' ', "")))
        .collect::<String>();
    let file = parse_frozen(&source).expect("the generated program parses");
    let declared = declarations_of(&file);

    assert_eq!(
        declared.len(),
        FORMS.len(),
        "every form must be extracted; the sweep is blind to whatever is missing",
    );
    for form in FORMS {
        let name = form.keyword.replace(' ', "");
        assert!(
            declared.contains(&(name.clone(), form.kind)),
            "`{}` declares `{name}` as {:?} and the extraction did not find it",
            form.keyword,
            form.kind,
        );
    }

    // And the reuse detector itself: a program that *does* reuse a name must be
    // reported, or "no program reuses a name" is a statement about the detector.
    let reusing = format!(
        "{}{}",
        (FORMS[0].declare)(CLAIMED),
        (FORMS[6].declare)(CLAIMED),
    );
    let file = parse_frozen(&reusing).expect("the reusing program parses");
    assert_eq!(
        cross_kind_reuses(&declarations_of(&file)),
        vec![CLAIMED.to_string()],
        "a program that reuses a name across kinds was not detected",
    );
}

/// The names a file declares in more than one kind.
fn cross_kind_reuses(declared: &[(String, DefKind)]) -> Vec<String> {
    let mut found = Vec::new();
    for (index, (name, kind)) in declared.iter().enumerate() {
        for (other_name, other_kind) in &declared[index + 1..] {
            if name == other_name && kind != other_kind {
                found.push(name.clone());
            }
        }
    }
    found
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
    out.sort();
}

/// **The claim that makes `parity.rs`'s green run meaningless here**, measured
/// rather than believed: not one checked-in program reuses a top-level name
/// across kinds.
///
/// So no mutation of them can produce the construct this change is about —
/// neither sweep can introduce a *second declaration of an existing name*, any
/// more than either could introduce the word `return`. The oracle's coverage is
/// bounded by the corpus's content, not by its grammar
/// ([A13](../../../plans/rewrite/anti-spec.md)).
///
/// If this ever fails, that is good news: the sweeps would then be able to see
/// the change, and this file would stop being the only cover.
#[test]
fn no_checked_in_program_reuses_a_name_across_kinds() {
    let mut scanned = 0;
    let mut reusing = Vec::new();

    for path in all_sources() {
        let source = std::fs::read_to_string(&path).expect("readable");
        let Ok(parsed) =
            yel_core::syntax::parser::parse_file_with_source_id(&source, yel_core::SourceId(0))
        else {
            continue;
        };
        scanned += 1;

        let file = &parsed.file;
        let declared: Vec<(&str, DefKind)> = file
            .records
            .iter()
            .map(|d| (d.node.name.as_str(), DefKind::Type))
            .chain(
                file.enums
                    .iter()
                    .map(|d| (d.node.name.as_str(), DefKind::Type)),
            )
            .chain(
                file.variants
                    .iter()
                    .map(|d| (d.node.name.as_str(), DefKind::Type)),
            )
            .chain(
                file.elements
                    .iter()
                    .map(|d| (d.node.name.as_str(), DefKind::Component)),
            )
            .chain(
                file.extern_components
                    .iter()
                    .map(|d| (d.node.name.as_str(), DefKind::Component)),
            )
            .chain(
                file.components
                    .iter()
                    .map(|d| (d.node.name.as_str(), DefKind::Component)),
            )
            .chain(
                file.globals
                    .iter()
                    .map(|d| (d.node.name.as_str(), DefKind::Global)),
            )
            .collect();

        for (index, (name, kind)) in declared.iter().enumerate() {
            for (other_name, other_kind) in &declared[index + 1..] {
                if name == other_name && kind != other_kind {
                    reusing.push(format!("{}: `{name}`", path.display()));
                }
            }
        }
    }

    assert!(
        scanned >= CORPUS_COUNT,
        "only {scanned} programs parsed; the sweep collapsed",
    );
    assert_eq!(
        reusing,
        Vec::<String>::new(),
        "a checked-in program reuses a name across kinds — the mutation sweeps \
         can now see this change, and this file's blindness claim is stale",
    );
}
