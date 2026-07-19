//! Integration tests for the public `yelc::native` compile API — the surface
//! the JS/TS bindings and embedders drive. They exercise the full
//! parse → HIR → type-check → LIR → codegen pipeline end to end on the host
//! target (no Wasmtime execution; just the produced artifacts).

use yelc::native::{CompileOutcome, CompileResult, OutputFormat, compile, compile_multi};

/// A library-only source (globals, no exported component).
const LIB_ONLY: &str =
    "package yel:libonly@0.1.0;\n\nglobal Store { count: s32 = 42; label: string = \"Hello\"; }\n";

/// A minimal exported component.
const COMPONENT: &str = "package yel:ctnr@0.1.0;\n\nexport component Card {\n    title: string = \"\";\n    VStack {\n        Text { \"{title}\" }\n    }\n}\n";

/// Unwrap a `Success`, or fail loudly with the diagnostics that came back.
fn expect_success(outcome: CompileOutcome) -> CompileResult {
    match outcome {
        CompileOutcome::Success(result) => result,
        CompileOutcome::Failure(diags) => {
            panic!(
                "expected success, got {} diagnostic(s): {:#?}",
                diags.len(),
                diags
            )
        }
    }
}

fn expect_failure(outcome: CompileOutcome) -> Vec<yelc::native::Diagnostic> {
    match outcome {
        CompileOutcome::Failure(diags) => diags,
        CompileOutcome::Success(_) => panic!("expected failure, got success"),
    }
}

/// Validate bytes with `wasmparser::Validator`, which understands the
/// component model. Any failure is returned decoded so a test names the
/// offending offset/instruction rather than just "invalid".
fn validate_wasm(bytes: &[u8]) -> Result<(), String> {
    wasmparser::Validator::new()
        .validate_all(bytes)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

#[test]
fn compiles_a_library_to_wit_with_its_declared_package() {
    let result = expect_success(compile("lib.yel", LIB_ONLY, OutputFormat::Wit));

    assert!(
        result.wit_code.contains("libonly"),
        "WIT should reference the declared package: {}",
        result.wit_code
    );
    assert!(result.wasm_bytes.is_empty(), "WIT format emits no wasm");
}

#[test]
fn compiles_a_component_to_a_valid_wasm_component_binary() {
    let result = expect_success(compile("card.yel", COMPONENT, OutputFormat::Wasm));

    assert!(!result.wasm_bytes.is_empty(), "component should produce wasm");
    assert_eq!(
        &result.wasm_bytes[..4],
        b"\0asm",
        "output should start with the wasm magic number"
    );
    // The real check: the bytes must validate as a well-formed component,
    // not merely start with the right magic.
    if let Err(e) = validate_wasm(&result.wasm_bytes) {
        panic!("generated wasm failed validation: {e}");
    }
}

#[test]
fn rust_format_is_a_disabled_placeholder() {
    // Rust codegen is temporarily disabled; it should still succeed with a
    // placeholder rather than error, so callers can detect the state.
    let result = expect_success(compile("card.yel", COMPONENT, OutputFormat::Rust));

    assert!(result.rust_code.contains("not available"));
    assert!(result.wasm_bytes.is_empty());
}

#[test]
fn reports_failure_with_an_error_diagnostic_on_a_parse_error() {
    let diags = expect_failure(compile("bad.yel", "@@@ not valid yel", OutputFormat::Wit));

    assert!(!diags.is_empty(), "a parse error should surface diagnostics");
    assert!(
        diags.iter().any(|d| d.severity == "error"),
        "at least one diagnostic should be an error"
    );
    // Every diagnostic carries a non-zero span length (the `.max(1)` floor).
    assert!(diags.iter().all(|d| d.length >= 1));
}

#[test]
fn rejects_a_non_kebab_case_package_name() {
    let source = "package yel:bad_name@0.1.0;\n\nexport component App {\n    Text { \"x\" }\n}\n";

    let diags = expect_failure(compile("p.yel", source, OutputFormat::Wit));

    assert!(
        diags
            .iter()
            .any(|d| d.severity == "error" && d.message.contains("package")),
        "underscore package should be rejected with a package diagnostic: {diags:#?}"
    );
}

#[test]
fn rejects_an_integer_literal_that_does_not_fit_its_declared_type() {
    // `300` does not fit `u8`; without the range check this silently
    // truncated to 44 during LIR lowering's unchecked `as u8` cast.
    let src = "package yel:oob@0.1.0;\n\nglobal Store { count: u8 = 300; }\n";
    let diags = expect_failure(compile("oob.yel", src, OutputFormat::Wit));

    assert!(
        diags
            .iter()
            .any(|d| d.severity == "error" && d.message.contains("out of range")),
        "an out-of-range literal should produce an out-of-range error: {diags:#?}"
    );
}

#[test]
fn accepts_an_integer_literal_that_fits_its_declared_type() {
    // The boundary value `255` is the max `u8`; it must still compile.
    let src = "package yel:inrange@0.1.0;\n\nglobal Store { count: u8 = 255; }\n";
    expect_success(compile("inrange.yel", src, OutputFormat::Wit));
}

#[test]
fn multi_file_compilation_takes_the_package_from_the_first_file() {
    let files = vec![
        ("a.yel".to_string(), LIB_ONLY.to_string()),
        ("b.yel".to_string(), COMPONENT.to_string()),
    ];

    let result = expect_success(compile_multi(&files, OutputFormat::Wit));

    assert!(
        result.wit_code.contains("libonly"),
        "the first file's package should win: {}",
        result.wit_code
    );
}
