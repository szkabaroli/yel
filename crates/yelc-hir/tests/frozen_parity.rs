//! The stage-3 differential: the definition table and the diagnostic set,
//! frozen vs new, over the whole 2000-program corpus.
//!
//! Stage 3 has no artifact of its own (F14), so this is what is comparable —
//! strongest first: the `Definitions` table, **contents and order**, because
//! `DefId`s are ordinals that reach output ordering; then the diagnostic
//! *sets* (corpus programs are valid, so both should be empty modulo the
//! carve-outs below).
//!
//! # The mapping, stated
//!
//! The frozen table interleaves member-level entries (`Field`, `Signal`,
//! `VariantCase`, `Parameter`, callback `Function`s) between item-level ones,
//! and pre-registers ~130 builtin defs. The new table holds **items only**
//! (members are rows on the owner) and pre-registers only the lang items. So
//! the comparable sequence is:
//!
//! - frozen: defs after the builtin prefix, filtered to item-level kinds
//! - new: defs after `Known::ALL`
//!
//! compared as `(name, coarse kind)` in registration order, with the frozen
//! `Namespace`-era kinds mapped: Record/Enum/Variant → Type; Element /
//! ExternComponent / Component → Component; Global → Global.
//!
//! # Carve-outs, each measured
//!
//! 1. **E0071** — corpus programs predate the every-file-declares-`package`
//!    rule (2026-07-30, `plans/modules.md`). An approved surface break; the
//!    diagnostic comparison excludes exactly this code and nothing else.
//! 2. **Single-namespace narrowing** — none of the corpus programs reuses a
//!    name across kinds (measured in `yelc-sema/tests/single_namespace.rs`),
//!    so no filter is needed; a collision here would fail as a table mismatch,
//!    which is the correct loudness.

use std::path::PathBuf;

use yelc_sema::{CompilerContext, PackageId};
use yelc_syntax::ParsedFile;

fn corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus/src")
}

/// `(name, coarse kind)` for the frozen table's item-level defs, in order.
fn frozen_items(source: &str) -> Result<Vec<(String, &'static str)>, String> {
    let mut compiler = yel_core::Compiler::new();
    let builtin_prefix = compiler.context().defs.len();
    let Ok(file) = compiler.parse(source) else {
        return Err("frozen parse failed".to_string());
    };
    let _ = compiler.lower_to_hir(&file);

    let ctx = compiler.context();
    let mut items = Vec::new();
    for (index, (_, item)) in ctx.defs.iter().enumerate() {
        if index < builtin_prefix {
            continue;
        }
        let kind = match &item.kind {
            yel_core::DefKind::Record(_)
            | yel_core::DefKind::Enum(_)
            | yel_core::DefKind::Variant(_) => "type",
            yel_core::DefKind::Element(_)
            | yel_core::DefKind::ExternComponent(_)
            | yel_core::DefKind::Component(_) => "component",
            yel_core::DefKind::Global(_) => "global",
            // Member-level entries: rows on the owner in the new table.
            yel_core::DefKind::Function(_)
            | yel_core::DefKind::Field(_)
            | yel_core::DefKind::Signal(_)
            | yel_core::DefKind::VariantCase(_)
            | yel_core::DefKind::Parameter(_)
            | yel_core::DefKind::Local => continue,
        };
        items.push((ctx.interner.str(item.name).to_string(), kind));
    }
    Ok(items)
}

/// `(name, coarse kind)` for the new table's defs after the lang items, plus
/// the diagnostic codes the lowering produced.
fn new_items(source: &str) -> (Vec<(String, &'static str)>, Vec<String>) {
    let mut ctx = CompilerContext::with_intrinsics(PackageId::LOCAL);
    let lang_items = ctx.defs.len();
    let id = ctx.sources.add_inline(source);
    let parsed: Vec<ParsedFile> = vec![yelc_syntax::parse(
        id,
        source,
        &ctx.names,
        &mut ctx.diagnostics,
    )];
    let _ = yelc_hir::lower_files(&parsed, &mut ctx);

    let items = ctx
        .defs
        .iter()
        .skip(lang_items)
        .map(|definition| {
            let kind = match definition.kind {
                yelc_sema::DefKind::Type => "type",
                yelc_sema::DefKind::Value => "value",
                yelc_sema::DefKind::Component => "component",
                yelc_sema::DefKind::Global => "global",
            };
            (ctx.names.str(definition.name).to_string(), kind)
        })
        .collect();
    let codes = ctx
        .diagnostics
        .iter()
        .filter_map(|diagnostic| diagnostic.code.map(|code| code.code().to_string()))
        .collect();
    (items, codes)
}

/// The differential proper. Every corpus program: same item-level definition
/// sequence, and no new-compiler diagnostic beyond the package-rule carve-out.
#[test]
fn the_definition_table_matches_the_frozen_compiler_over_the_corpus() {
    let dir = corpus_dir();
    assert!(
        dir.is_dir(),
        "corpus not found at {} — this differential cannot run",
        dir.display()
    );

    let mut paths: Vec<PathBuf> = std::fs::read_dir(&dir)
        .expect("corpus readable")
        .filter_map(|entry| {
            let path = entry.expect("corpus entry").path();
            (path.extension().is_some_and(|extension| extension == "yel")).then_some(path)
        })
        .collect();
    paths.sort();
    assert_eq!(paths.len(), 2000, "the corpus is the corpus");

    let mut compared = 0usize;
    let mut mismatches: Vec<String> = Vec::new();
    for path in &paths {
        let source = std::fs::read_to_string(path).expect("corpus file readable");
        let frozen = match frozen_items(&source) {
            Ok(items) => items,
            // The frozen parser rejecting a corpus program would itself be
            // news — the corpus was generated by it — so it fails loudly
            // rather than being skipped.
            Err(reason) => {
                mismatches.push(format!("{}: {reason}", path.display()));
                continue;
            }
        };
        let (new, codes) = new_items(&source);

        if frozen != new {
            mismatches.push(format!(
                "{}: frozen {frozen:?} != new {new:?}",
                path.display()
            ));
        }
        if codes.iter().any(|code| code != "E0071") {
            mismatches.push(format!(
                "{}: unexpected diagnostics {codes:?}",
                path.display()
            ));
        }
        compared += 1;
    }

    assert_eq!(
        mismatches,
        Vec::<String>::new(),
        "definition-table divergences over {compared} programs"
    );
    assert_eq!(compared, 2000);
}

/// The carve-out is not vacuous: the corpus really does contain programs with
/// and without the `package` clause, so both sides of the rule are exercised.
#[test]
fn the_package_carve_out_covers_a_real_split() {
    let dir = corpus_dir();
    let mut with = 0usize;
    let mut without = 0usize;
    for entry in std::fs::read_dir(&dir).expect("corpus readable") {
        let path = entry.expect("entry").path();
        if path.extension().is_some_and(|extension| extension == "yel") {
            let source = std::fs::read_to_string(&path).expect("readable");
            if source.starts_with("package ") || source.contains("\npackage ") {
                with += 1;
            } else {
                without += 1;
            }
        }
    }
    assert!(with > 0, "some corpus programs declare a package");
    assert!(without > 0, "some corpus programs predate the rule");
    assert_eq!(with + without, 2000);
}
