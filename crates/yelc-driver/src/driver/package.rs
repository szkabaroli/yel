//! Which files are in the package — filesystem only, no language rules.
//!
//! The split is deliberate. *Which files* is I/O and belongs to the driver;
//! *whether they agree on a package* is a language rule and lives in
//! [`yelc_hir::check_package_identity`]. Putting the second here would make the
//! driver a third implementation of something, which its module doc forbids.

use std::path::{Path, PathBuf};

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
pub fn collect(path: &Path) -> Result<Vec<PathBuf>, String> {
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
