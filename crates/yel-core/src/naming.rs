//! Shared name-mangling helpers used across the front-end and back-end.
//!
//! WIT identifiers are kebab-case; Yel source names are typically
//! camel/Pascal-case. `to_kebab_case` is the single source of truth for that
//! conversion — both `yel-core` (building the import contract) and
//! `yel-wasm-codegen` (rendering WIT / the core import section) go through it,
//! so an interface name computed in the frontend and re-derived in the backend
//! can never drift. It is idempotent on strings that are already kebab-case.

/// Convert `s` to kebab-case, splitting camelCase / PascalCase boundaries and
/// keeping acronyms contiguous (`parseURL` → `parse-url`, `HTTPServer` →
/// `http-server`). Underscores become hyphens. Already-kebab input is
/// returned unchanged.
pub fn to_kebab_case(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let mut result = String::new();
    for i in 0..chars.len() {
        let c = chars[i];
        if c.is_uppercase() {
            // Insert a separator before this uppercase letter only when it
            // begins a new word — so acronyms stay contiguous:
            //   * previous char is lowercase/digit → camelCase boundary
            //     (`parseURL` → `parse-url`), or
            //   * previous char is uppercase but the next is lowercase → the
            //     end of an acronym (`HTTPServer` → the `S` starts `server`).
            // A previous separator (`-`/`_`) is already a boundary, so no dash.
            let prev = i.checked_sub(1).map(|p| chars[p]);
            let next = chars.get(i + 1).copied();
            let starts_word = match prev {
                Some(p) if p.is_lowercase() || p.is_ascii_digit() => true,
                Some(p) if p.is_uppercase() => next.is_some_and(char::is_lowercase),
                _ => false,
            };
            if starts_word && !result.ends_with('-') {
                result.push('-');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result.replace('_', "-")
}
