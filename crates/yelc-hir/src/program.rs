//! The package, end to end: discover, parse, identity-check, resolve
//! includes, lower. One entry point, [`check_package`] — the whole frontend
//! pipeline behind one call, ark's `check_program` shape
//! (`arkc-frontend/src/lib.rs`): the driver hands over inputs and formats
//! outputs, and never sees a phase.
//!
//! # Discovery is frontend, revised 2026-07-31
//!
//! This file's discovery half lived in the driver for a day, with a doc
//! arguing *"which files is I/O and belongs to the driver."* Ark draws the
//! line the other way — `ProgramParser` reads the filesystem from inside
//! `arkc-frontend` — and it is the better line: which files constitute a
//! package is a language rule (`plans/modules.md` §4) that happens to touch
//! the disk, not disk-touching that happens to have rules.

use std::path::{Path, PathBuf};

use yelc_base::Name;
use yelc_sema::{CompilerContext, PackageId, PackageRole};

/// Yel source extension. One constant, because "is this a source file" is asked
/// in exactly one place and must stay that way.
const SOURCE_EXTENSION: &str = "yel";

/// The files of the package named by `path`, in a stable order.
///
/// `path` is a **directory** — the package, per Go's model and
/// `plans/modules.md` §4. A plain file is also accepted and read as a one-file
/// package, which is what makes single-file invocations (and the round-trip
/// instrument) keep working.
///
/// # Not recursive
///
/// A subdirectory is a *different* package, so it is not collected. Go needs
/// `./...` to walk down for the same reason.
pub(crate) fn collect(path: &Path) -> Result<Vec<PathBuf>, String> {
    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }
    if !path.is_dir() {
        return Err(format!(
            "{} is neither a file nor a directory",
            path.display()
        ));
    }

    let entries =
        std::fs::read_dir(path).map_err(|err| format!("cannot read {}: {err}", path.display()))?;

    let mut files = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|err| format!("cannot read {}: {err}", path.display()))?;
        let candidate = entry.path();
        // `is_file` and not `!is_dir`: a directory named `x.yel` is not a source
        // file, and neither is a broken symlink.
        if candidate.is_file()
            && candidate
                .extension()
                .is_some_and(|ext| ext == SOURCE_EXTENSION)
        {
            files.push(candidate);
        }
    }

    // `read_dir` yields in whatever order the filesystem does — APFS and ext4
    // disagree, and so do two runs on one of them after a rename. Sorting is
    // what makes "the first file to declare the package" a stable sentence, and
    // that sentence is load-bearing in the mismatch diagnostic
    // (`plans/rewrite/anti-spec.md` A6).
    files.sort();

    if files.is_empty() {
        return Err(format!(
            "no .{SOURCE_EXTENSION} files in {}",
            path.display()
        ));
    }

    Ok(files)
}

// ---------------------------------------------------------------------------
// Package loaders — ark's `ProgramParser` function names, kept
// ---------------------------------------------------------------------------

/// Load one compiler-shipped std module as a package —
/// `add_stdlib_package`'s yel form (one std *module* per package).
pub(crate) fn add_std_package(
    context: &mut CompilerContext,
    std_name: &str,
    bytes: &[u8],
    module_name: Name,
) -> Result<PackageId, String> {
    let name = context.names.intern(std_name);
    add_loaded_package(context, PackageRole::Std(name), bytes, module_name)
}

/// Load one `--include`-located dependency as a package —
/// `add_dependency_packages`' per-package body.
pub(crate) fn add_dependency_package(
    context: &mut CompilerContext,
    specifier: Name,
    bytes: &[u8],
    module_name: Name,
) -> Result<PackageId, String> {
    add_loaded_package(
        context,
        PackageRole::External(specifier),
        bytes,
        module_name,
    )
}

/// The shared load: decode, stamp-check, rebuild the definition table under a
/// fresh [`PackageId`], and give the package its row in the compilation
/// structure. **Loading only** — binding the module name into the program's
/// scope is `includes`' half, ark's split exactly (`ProgramParser` loads;
/// `useck` binds).
fn add_loaded_package(
    context: &mut CompilerContext,
    role: PackageRole,
    bytes: &[u8],
    module_name: Name,
) -> Result<PackageId, String> {
    let artifact = yelc_sema::artifact::decode(bytes).map_err(|error| error.to_string())?;
    let package = PackageId(context.imported.len() as u32 + 1);
    let loaded = artifact
        .load(package, &context.names, &context.types)
        .map_err(|error| error.to_string())?;
    context.imported.push(loaded.into_defs());
    context
        .compilation
        .add_package(package, role, Some(module_name));
    Ok(package)
}
