//! Snapshot tests over the textual codegen outputs (WIT and DOT), driven
//! through the `yelc` CLI. These pin the generated text for representative
//! sources so unintended codegen drift shows up as a reviewable diff rather
//! than slipping through. WIT/DOT output is deterministic, so the snapshots
//! are stable; driving the real binary also covers the `compile` subcommand's
//! output plumbing.
//!
//! To update after an intentional codegen change, run with
//! `INSTA_UPDATE=always cargo test -p yelc --test snapshot` (or
//! `cargo insta review`), then commit the changed `.snap` files.

use std::io::Write;
use std::process::Command;

use insta::assert_snapshot;

/// A reactive component: the `count` signal is interpolated into `Text`, which
/// produces a reactive effect — exercising signals, effects, and the WIT world.
const COUNTER: &str = "package yel:counter@0.1.0;\n\nexport component Counter {\n    count: s32 = 0;\n    VStack {\n        Text { \"{count}\" }\n    }\n}\n";

/// A library-only source (globals, no exported component) — drives the WIT
/// "library world" path.
const LIB_ONLY: &str =
    "package yel:libonly@0.1.0;\n\nglobal Store { count: s32 = 42; label: string = \"Hello\"; }\n";

/// Write `source` to a uniquely-named temp file, run `yelc compile -o <format>`
/// on it, and return stdout (the generated text). Panics with stderr on a
/// non-zero exit so failures are legible.
fn compile_to(source: &str, format: &str, tag: &str) -> String {
    let path = std::env::temp_dir().join(format!("yelc-snap-{}-{}.yel", tag, std::process::id()));
    {
        let mut file = std::fs::File::create(&path).expect("create temp source");
        file.write_all(source.as_bytes())
            .expect("write temp source");
    }

    let output = Command::new(env!("CARGO_BIN_EXE_yelc"))
        .args(["compile", path.to_str().unwrap(), "-o", format])
        .output()
        .expect("run yelc binary");

    let _ = std::fs::remove_file(&path);

    assert!(
        output.status.success(),
        "yelc compile -o {format} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).expect("output is valid UTF-8")
}

#[test]
fn wit_for_a_reactive_component() {
    assert_snapshot!(compile_to(COUNTER, "wit", "counter-wit"));
}

#[test]
fn wit_for_a_library_only_source() {
    assert_snapshot!(compile_to(LIB_ONLY, "wit", "libonly-wit"));
}

#[test]
fn dot_dependency_graph_for_a_reactive_component() {
    assert_snapshot!(compile_to(COUNTER, "dot", "counter-dot"));
}
