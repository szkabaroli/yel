//! End to end over the real binary: `--emit-module` writes a module, `from
//! "…" include Name;` loads it, and resolution crosses the package boundary.
//!
//! These drive `yelc2` itself (`CARGO_BIN_EXE_yelc2`) rather than the library,
//! because the loop under test — write bytes, find them through `--include`,
//! read them back in a fresh process — *is* the driver's. A library test
//! would share one interner and prove less than it appears to.

use std::path::PathBuf;
use std::process::Command;

fn yelc2() -> Command {
    Command::new(env!("CARGO_BIN_EXE_yelc2"))
}

fn workspace() -> PathBuf {
    let root = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("include-e2e");
    std::fs::create_dir_all(root.join("mods")).expect("scratch dirs");
    root
}

/// The whole loop: package A becomes a module, package B includes it, and the
/// dump shows B's names resolving to A's definitions — visibly foreign, with
/// the cross-package dependency recorded.
#[test]
fn a_module_written_by_one_process_resolves_in_another() {
    let root = workspace();
    let geometry = root.join("geometry.yel");
    std::fs::write(
        &geometry,
        "package demo:geometry@0.1.0;\n\nexport global Geometry {\n    origin-x: s32 = 0;\n}\n",
    )
    .expect("write source");

    let module = root.join("mods/geometry.yelmod");
    let wrote = yelc2()
        .arg(format!("--emit-module={}", module.display()))
        .arg(&geometry)
        .output()
        .expect("run yelc2");
    assert!(wrote.status.success(), "emit-module failed: {wrote:?}");
    assert!(module.is_file(), "the module was written");

    let app = root.join("app.yel");
    std::fs::write(
        &app,
        "package demo:app@0.1.0;\n\nfrom \"geometry\" include Geo;\n\ncomponent App {\n    Text { text: \"d: {Geo.Geometry.origin-x}\" }\n}\n",
    )
    .expect("write source");

    let ran = yelc2()
        .arg("--include")
        .arg(root.join("mods"))
        .arg("--emit-hir")
        .arg(&app)
        .output()
        .expect("run yelc2");
    let stdout = String::from_utf8_lossy(&ran.stdout);
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(ran.status.success(), "compile failed:\n{stderr}");

    // The resolution is foreign and says so.
    assert!(
        stdout.contains("Geometry#pkg1"),
        "the include resolves to the loaded package's definition:\n{stdout}"
    );
    // And signalck sees across the boundary.
    assert!(
        stdout.contains("reads(Geometry#pkg1"),
        "the cross-package property read is a recorded dependency:\n{stdout}"
    );
}

/// `std:` resolves from the **embedded** stdlib and is never searched in
/// `--include` directories — proven by planting a garbage decoy `list.yelmod`
/// where a search would find it: the compilation *succeeds*, which it could
/// not have if the decoy's bytes had been read.
#[test]
fn std_specifiers_resolve_embedded_never_searched() {
    let root = workspace();
    std::fs::write(root.join("mods/list.yelmod"), b"decoy").expect("write decoy");
    let source = root.join("std-app.yel");
    std::fs::write(
        &source,
        "package demo:app@0.1.0;\nfrom \"std:list\" include List;\n",
    )
    .expect("write source");

    let ran = yelc2()
        .arg("--include")
        .arg(root.join("mods"))
        .arg(&source)
        .output()
        .expect("run yelc2");
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(
        ran.status.success(),
        "std:list loads from the embedded stdlib, decoy untouched:\n{stderr}"
    );
}

/// The embedded stdlib end to end: `std:num` resolves and `Num.min` is a
/// foreign definition from the module `build.rs` compiled out of
/// `stdlib/num.yel`.
#[test]
fn the_embedded_stdlib_resolves() {
    let root = workspace();
    let source = root.join("std-num.yel");
    std::fs::write(
        &source,
        "package demo:app@0.1.0;\nfrom \"std:num\" include Num;\ncomponent App { Text { text: \"m: {Num.Num.min}\" } }\n",
    )
    .expect("write source");

    let ran = yelc2()
        .arg("--emit-hir")
        .arg(&source)
        .output()
        .expect("run yelc2");
    let stdout = String::from_utf8_lossy(&ran.stdout);
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(ran.status.success(), "compile failed:\n{stderr}");
    assert!(
        stdout.contains("Num#pkg1"),
        "Num.min resolves into the embedded module:\n{stdout}"
    );
}

/// An unknown std module names what actually ships.
#[test]
fn an_unknown_std_module_lists_what_ships() {
    let root = workspace();
    let source = root.join("std-nope.yel");
    std::fs::write(
        &source,
        "package demo:app@0.1.0;\nfrom \"std:nope\" include N;\n",
    )
    .expect("write source");

    let ran = yelc2().arg(&source).output().expect("run yelc2");
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(!ran.status.success());
    assert!(stderr.contains("no std module named `nope`"));
    assert!(
        stderr.contains("array, list, num, string"),
        "the note lists the shipped set:\n{stderr}"
    );
}

#[test]
fn a_module_that_does_not_exist_reports_where_it_looked() {
    let root = workspace();
    let source = root.join("missing-app.yel");
    std::fs::write(
        &source,
        "package demo:app@0.1.0;\nfrom \"nope\" include Nope;\n",
    )
    .expect("write source");

    let ran = yelc2()
        .arg("--include")
        .arg(root.join("mods"))
        .arg(&source)
        .output()
        .expect("run yelc2");
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(!ran.status.success());
    assert!(stderr.contains("cannot find module `nope`"));
    assert!(
        stderr.contains("nope.yelmod"),
        "the note names the searched path:\n{stderr}"
    );
}

/// Garbage bytes wearing the extension: the stamp/decode gate refuses with the
/// include's own span, and the compilation carries on to report it normally.
#[test]
fn a_corrupt_module_is_refused_at_load() {
    let root = workspace();
    std::fs::write(root.join("mods/broken.yelmod"), b"not a module").expect("write");
    let source = root.join("broken-app.yel");
    std::fs::write(
        &source,
        "package demo:app@0.1.0;\nfrom \"broken\" include Broken;\n",
    )
    .expect("write source");

    let ran = yelc2()
        .arg("--include")
        .arg(root.join("mods"))
        .arg(&source)
        .output()
        .expect("run yelc2");
    let stderr = String::from_utf8_lossy(&ran.stderr);
    assert!(!ran.status.success());
    assert!(
        stderr.contains("cannot load module `broken`"),
        "load errors carry the module name:\n{stderr}"
    );
}
