//! Integration tests: feed hand-written `.yel` fixtures through the full
//! compile pipeline and assert that each one produces the expected WIT,
//! validates as a WebAssembly component, and (for the diagnostics folder)
//! emits the expected error message.
//!
//! Two fixture directories:
//!
//! - `tests/fixtures/positive/*.yel` — each must compile cleanly, produce
//!   a snapshot WIT (`<name>.wit`), and its WASM output must validate.
//!   Missing `.wit` files are auto-written on first run OR whenever
//!   `UPDATE_SNAPSHOTS=1` is set in the environment.
//!
//! - `tests/fixtures/diagnostics/*.yel` — each must FAIL type-check, with
//!   the error output containing every substring listed in the matching
//!   `<name>.expected` file (one substring per line, blank lines skipped).
//!
//! - `tests/fixtures/known_bugs/*.yel` — each is a *correct Yel program
//!   that currently fails to compile*. The harness asserts compilation
//!   fails with every substring listed in the matching `<name>.failure`
//!   file. The moment a bug is fixed, compilation succeeds and the test
//!   reports it loudly so the fixture can graduate to `positive/`.
//!   See `tests/fixtures/known_bugs/README.md`.
//!
//! Run with `cargo test -p yel-wasm-codegen`. To refresh snapshots after
//! an intentional WIT change: `UPDATE_SNAPSHOTS=1 cargo test -p yel-wasm-codegen`.

use std::path::{Path, PathBuf};

use yel_core::Compiler;
use yel_wasm_codegen as codegen;

fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

fn list_yel_fixtures(sub: &str) -> Vec<PathBuf> {
    let dir = fixtures_dir().join(sub);
    let mut out = Vec::new();
    for entry in std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("cannot read {}: {}", dir.display(), e))
    {
        let path = entry.expect("directory entry").path();
        if path.extension().and_then(|s| s.to_str()) == Some("yel") {
            out.push(path);
        }
    }
    out.sort();
    out
}

/// Drive a single `.yel` source all the way through the pipeline, stopping
/// if any stage produces diagnostics. Returns the collected rendered
/// diagnostics on failure so negative-fixture tests can match against them.
struct CompileOutputs {
    wit: String,
    wasm: Vec<u8>,
    dot: String,
}

fn compile_fixture(source: &str) -> Result<CompileOutputs, String> {
    let mut compiler = Compiler::new();
    let file = compiler
        .parse(source)
        .map_err(|e| format!("parse error: {}", e))?;
    let hir = compiler.lower_to_hir(&file);
    if compiler.has_errors() {
        return Err(compiler.render_diagnostics());
    }

    let mut lir_components = Vec::new();
    for h in &hir {
        let thir = compiler.type_check(h);
        if compiler.has_errors() {
            return Err(compiler.render_diagnostics());
        }
        lir_components.push(compiler.lower_to_lir(&thir));
    }
    let thir_globals = compiler.type_check_globals();
    if compiler.has_errors() {
        return Err(compiler.render_diagnostics());
    }
    let lir_globals = compiler.lower_globals_to_lir(&thir_globals);

    // Use the package from the source when available so the WIT output
    // has stable names; fall back to `yel:app` otherwise.
    let (namespace, name, version) = match file.package {
        Some(ref pkg) => (
            pkg.namespace.clone(),
            pkg.name.clone(),
            pkg.version.clone().unwrap_or_else(|| "0.1.0".to_string()),
        ),
        None => ("yel".into(), "app".into(), "0.1.0".into()),
    };

    let ctx = compiler.context();

    let wit_options = codegen::WitOptions {
        namespace: namespace.clone(),
        name: name.clone(),
        version: version.clone(),
        include_dom_interface: true,
    };
    let wit = codegen::generate_wit(&lir_components, ctx, &wit_options)
        .map_err(|e| format!("WIT generation: {}", e))?;

    let module = yel_core::lir::LirModule {
        components: lir_components.clone(),
        global_defaults: lir_globals.clone(),
        package: file.package.clone(),
    };
    let wasm_options = codegen::WasmWithWitOptions {
        namespace,
        name,
        version,
        global_defaults: lir_globals,
    };
    let wasm = codegen::generate_wasm_module(&module, ctx, &wasm_options)
        .map_err(|e| format!("WASM generation: {}", e))?;

    let dot = codegen::generate_dot(&lir_components, ctx, &codegen::DotOptions::new())
        .map_err(|e| format!("DOT generation: {}", e))?;

    Ok(CompileOutputs { wit, wasm, dot })
}

/// Run `wasmparser::Validator` on the bytes. Any validation failure comes
/// back as the decoded error so the test output names the offending
/// instruction / offset, not just a boolean "invalid".
fn validate_wasm(bytes: &[u8]) -> Result<(), String> {
    let mut validator = wasmparser::Validator::new();
    validator
        .validate_all(bytes)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

fn snapshot_update_requested() -> bool {
    std::env::var_os("UPDATE_SNAPSHOTS").is_some()
}

fn read_file(path: &Path) -> Option<String> {
    std::fs::read_to_string(path).ok()
}

#[test]
fn positive_fixtures() {
    let mut failures: Vec<String> = Vec::new();
    for yel_path in list_yel_fixtures("positive") {
        let name = yel_path
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let source = std::fs::read_to_string(&yel_path)
            .unwrap_or_else(|e| panic!("read {}: {}", yel_path.display(), e));

        let outputs = match compile_fixture(&source) {
            Ok(o) => o,
            Err(diagnostics) => {
                failures.push(format!(
                    "[{}] expected clean compile, got diagnostics:\n{}",
                    name, diagnostics
                ));
                continue;
            }
        };

        if let Err(e) = validate_wasm(&outputs.wasm) {
            failures.push(format!("[{}] wasm failed validation: {}", name, e));
            continue;
        }

        // WIT + DOT snapshots live alongside the `.yel` source. Reuse the
        // same auto-write / diff-on-mismatch / UPDATE_SNAPSHOTS=1 refresh
        // policy for both — they're both deterministic text renderings of
        // the same LIR, so the two snapshots move in lockstep.
        for (ext, label, actual) in [
            ("wit", "WIT", &outputs.wit),
            ("dot", "DOT", &outputs.dot),
        ] {
            let snapshot_path = yel_path.with_extension(ext);
            let existing = read_file(&snapshot_path);
            match existing {
                None => {
                    std::fs::write(&snapshot_path, actual).expect("write snapshot");
                    eprintln!(
                        "[{}] wrote new {} snapshot {}",
                        name,
                        label,
                        snapshot_path.display()
                    );
                }
                Some(expected) if &expected == actual => {}
                Some(_expected) if snapshot_update_requested() => {
                    std::fs::write(&snapshot_path, actual).expect("update snapshot");
                    eprintln!(
                        "[{}] refreshed {} snapshot {}",
                        name,
                        label,
                        snapshot_path.display()
                    );
                }
                Some(expected) => {
                    let diff =
                        pretty_assertions::StrComparison::new(&expected, actual).to_string();
                    failures.push(format!(
                        "[{}] {} snapshot mismatch (rerun with UPDATE_SNAPSHOTS=1 to accept):\n{}",
                        name, label, diff
                    ));
                }
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "\n{} positive fixture(s) failed:\n\n{}",
            failures.len(),
            failures.join("\n\n")
        );
    }
}

/// See `tests/fixtures/known_bugs/README.md`. Each fixture encodes a
/// *correct* Yel program that the compiler currently rejects — the
/// accompanying `.failure` file lists substrings that must appear in the
/// error output. Passes as long as the bug still exists; fails (with
/// graduate-me instructions) once the bug is fixed.
#[test]
fn known_bugs_fixtures() {
    let mut failures: Vec<String> = Vec::new();
    for yel_path in list_yel_fixtures("known_bugs") {
        let name = yel_path
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let source = std::fs::read_to_string(&yel_path)
            .unwrap_or_else(|e| panic!("read {}: {}", yel_path.display(), e));

        let failure_path = yel_path.with_extension("failure");
        let failure_spec = std::fs::read_to_string(&failure_path).unwrap_or_else(|e| {
            panic!(
                "[{}] missing .failure file {}: {}",
                name,
                failure_path.display(),
                e
            )
        });
        let needles: Vec<&str> = failure_spec
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .collect();

        // Some bugs panic the compiler instead of returning a
        // diagnostic. `catch_unwind` captures those so the suite stays
        // honest — a panicking compile path is just as "broken" as a
        // returned-Err one from the user's perspective.
        let source_owned = source.clone();
        let compile_outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            compile_fixture(&source_owned)
        }));

        let rendered = match compile_outcome {
            Ok(Ok(_)) => {
                failures.push(format!(
                    "[{}] expected compile to fail (known bug), but it succeeded.\n\
                     The bug appears to be fixed — move this fixture to \
                     tests/fixtures/positive/ and delete the .failure file.",
                    name
                ));
                continue;
            }
            Ok(Err(rendered)) => rendered,
            Err(panic) => {
                // Format the panic payload into something we can grep
                // against. Known-bug fixtures may list `panicked`,
                // `index out of bounds`, a source file name, etc. in
                // their `.failure` spec.
                let message = panic
                    .downcast_ref::<&'static str>()
                    .map(|s| (*s).to_string())
                    .or_else(|| panic.downcast_ref::<String>().cloned())
                    .unwrap_or_else(|| "<non-string panic payload>".to_string());
                format!("panicked: {}", message)
            }
        };

        for needle in &needles {
            if !rendered.contains(needle) {
                failures.push(format!(
                    "[{}] bug signature changed. Expected substring `{}` in output, but got:\n{}",
                    name, needle, rendered
                ));
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "\n{} known_bugs fixture(s) changed state:\n\n{}",
            failures.len(),
            failures.join("\n\n")
        );
    }
}

#[test]
fn diagnostic_fixtures() {
    let mut failures: Vec<String> = Vec::new();
    for yel_path in list_yel_fixtures("diagnostics") {
        let name = yel_path
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let source = std::fs::read_to_string(&yel_path)
            .unwrap_or_else(|e| panic!("read {}: {}", yel_path.display(), e));

        let expected_path = yel_path.with_extension("expected");
        let expected = std::fs::read_to_string(&expected_path).unwrap_or_else(|e| {
            panic!(
                "[{}] missing expected-errors file {}: {}",
                name,
                expected_path.display(),
                e
            )
        });
        let expected_needles: Vec<&str> = expected
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .collect();

        match compile_fixture(&source) {
            Ok(_) => {
                failures.push(format!(
                    "[{}] expected compile to fail, but it succeeded",
                    name
                ));
            }
            Err(rendered) => {
                for needle in &expected_needles {
                    if !rendered.contains(needle) {
                        failures.push(format!(
                            "[{}] missing expected substring `{}` in diagnostics:\n{}",
                            name, needle, rendered
                        ));
                    }
                }
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "\n{} diagnostic fixture(s) failed:\n\n{}",
            failures.len(),
            failures.join("\n\n")
        );
    }
}
