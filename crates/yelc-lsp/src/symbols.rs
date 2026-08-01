//! Copied from ark's `ark-language-server/src/symbols.rs`: the same
//! `SymbolScanner` visitor with `start_children`/`stop_children` nesting, the
//! same request plumbing through the threadpool. Deviations:
//!
//! - The visitor arms are **live** for yel's item kinds — ark's file has most
//!   of its arms commented out awaiting its AST; ours exist.
//! - Ark's hover is a stub that prints the cursor position; the plumbing is
//!   kept and the body is filled from what the frontend built for exactly
//!   this — the definition table, member rows, and the D6 docs side table.

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use lsp_server::{Message, Request, Response};
use lsp_types::*;
use yelc_syntax::ast;
use yelc_syntax::ast::visit::Visitor;

use crate::{HoverConfig, MainLoopTask, STD_MODULES, ServerState, position_from_source};

pub(super) fn document_hover_request(state: &mut ServerState, request: Request) {
    let result = serde_json::from_value::<lsp_types::HoverParams>(request.params);

    match result {
        Ok(result) => {
            let uri = result.text_document_position_params.text_document.uri;
            let Ok(path) = uri.to_file_path() else { return };
            let position = result.text_document_position_params.position;
            let overlay = state.opened_files.get(&path).cloned();
            let sender = state.threadpool_sender.clone();
            let config = state.hover_config;

            state.threadpool.execute(move || {
                let hover = hover_at(&path, overlay, position, config);
                log::info!(
                    "hover {}:{}:{} -> {}",
                    path.display(),
                    position.line + 1,
                    position.character + 1,
                    if hover.is_some() { "hit" } else { "nothing" },
                );
                let response = Response::new_ok(request.id, serde_json::to_value(hover).unwrap());
                sender
                    .send(MainLoopTask::SendResponse(Message::Response(response)))
                    .expect("send failed");
            });
        }
        Err(_) => {
            log::error!("malformed request params");
        }
    }
}

/// The filled-in body of ark's stub: compile the package with the buffer
/// overlaid, find the declaration or member whose *name span* is under the
/// cursor — or, failing that, the smallest *usage* the HIR resolved — and
/// render kind, declared type, and D6 docs.
fn hover_at(
    path: &Path,
    overlay: Option<Arc<str>>,
    position: Position,
    config: HoverConfig,
) -> Option<Hover> {
    let package_root = path.parent()?;
    let mut context = yelc_sema::CompilerContext::with_intrinsics(yelc_sema::PackageId::LOCAL);
    let overlay_pair = overlay.as_deref().map(|content| (path, content));
    let checked = match yelc_hir::check_package_with_overlay(
        package_root,
        overlay_pair,
        &[],
        STD_MODULES,
        &mut context,
    ) {
        Ok(checked) => checked,
        // No package to compile (a directory with no `.yel` files, say) —
        // the file still hovers as its own package.
        Err(_) => return standalone_hover(path, overlay, position, config),
    };

    // The file's SourceId in this run, and the cursor's byte offset.
    let found = checked
        .paths
        .iter()
        .zip(&checked.parsed)
        .find(|(candidate, _)| candidate.as_path() == path)
        .map(|(_, file)| (file.source, file));
    let Some((source_id, file)) = found else {
        // The file is not part of the package — a `.yelir` dump, a stray —
        // so it hovers as a package of its own instead.
        return standalone_hover(path, overlay, position, config);
    };
    hover_in_package(&context, &checked, source_id, file, position, config)
}

/// Everything hover can say about a position in a compiled package's file.
fn hover_in_package(
    context: &yelc_sema::CompilerContext,
    checked: &yelc_hir::CheckedPackage,
    source_id: yelc_base::SourceId,
    file: &yelc_syntax::ParsedFile,
    position: Position,
    config: HoverConfig,
) -> Option<Hover> {
    let source = context.sources.get(source_id)?;
    let offset = offset_from_position(&source.content, position);

    let contains = |span: yelc_base::Span| {
        span.source == source_id && span.start <= offset && offset < span.end.max(span.start + 1)
    };

    // Declaration and member *name* spans first: the definition table already
    // points at exactly the token under the cursor, no tree search needed.
    for definition in context.defs.iter() {
        if contains(definition.span) {
            let value = render_definition(context, checked, definition);
            return Some(hover(source, definition.span, value));
        }
        for member in context.defs.members(definition.id) {
            if contains(member.span) {
                let value = render_member(
                    context,
                    checked,
                    definition.name,
                    member.name,
                    member.ty,
                    member.span,
                );
                return Some(hover(source, member.span, value));
            }
        }
    }

    // The package clause. Not a definition — but the identity is already
    // computed, and silence on the one line every file starts with reads as
    // the server being broken.
    for item in &file.ast.items {
        let ast::ItemKind::Package(decl) = item else {
            continue;
        };
        if contains(decl.span)
            && let Some(identity) = &checked.identity
        {
            let mut text = format!(
                "{}:{}",
                context.names.str(identity.namespace),
                context.names.str(identity.name),
            );
            if let Some(version) = identity.version {
                text.push('@');
                text.push_str(&context.names.str(version));
            }
            return Some(hover(
                source,
                decl.span,
                format!("```yel\npackage {text}\n```"),
            ));
        }
    }

    // Usage sites. The HIR stores what every name resolved to but not where it
    // was written; the AST knows where but not what. The `HirId ↔ SourceNodeId`
    // map is the join, in two sweeps: index every resolution by `HirId`, then
    // walk the file's AST for the smallest node under the cursor whose mapped
    // `HirId` has one.
    let mut index = UsageIndex {
        body: None,
        names: HashMap::new(),
        callees: HashMap::new(),
    };
    for body in checked.module.bodies.iter() {
        index.body = Some(body);
        yelc_hir::visit::Visitor::visit_body(&mut index, body);
    }
    index.body = None;

    let mut cursor = AtCursor {
        offset,
        candidates: Vec::new(),
        type_names: Vec::new(),
    };
    Visitor::visit_file(&mut cursor, &file.ast);
    // Smallest span first; ties keep discovery order, and the loop below tries
    // until one candidate actually resolves, so a parent that maps to nothing
    // (a whole call, a block) never shadows the name inside it.
    cursor
        .candidates
        .sort_by_key(|candidate| candidate.span.end - candidate.span.start);

    for candidate in &cursor.candidates {
        let node = yelc_hir::SourceNodeId {
            source: source_id,
            node: candidate.node,
        };
        let Some(hir) = checked.module.map.hir_of(node) else {
            continue;
        };
        // A callee candidate reads the call's map, but either map may hold the
        // answer — an element name maps from the instantiate node, a bare name
        // from its own expression.
        let target = if candidate.callee {
            index.callees.get(&hir).or_else(|| index.names.get(&hir))
        } else {
            index.names.get(&hir).or_else(|| index.callees.get(&hir))
        };
        if let Some(target) = target {
            let value = render_usage(context, checked, target)?;
            return Some(hover(source, candidate.span, value));
        }
    }

    // Type positions: annotations and typed-record heads. No HIR node exists
    // for them, but under the single root namespace the definition is one
    // lookup away. Smallest span first, same policy as the candidates.
    let mut type_names = cursor.type_names;
    type_names.sort_by_key(|(span, _)| span.end - span.start);
    for (span, name) in type_names {
        let mut value = None;
        for sym in context.defs.lookup(name) {
            if let Some(def) = sym.def() {
                value = Some(render_definition(context, checked, context.defs.get(def)));
                break;
            }
            if matches!(sym, yelc_sema::Sym::Module(_)) {
                // A module binding: no `Definition` behind it, but the head
                // slicer knows the declaration, attributes included.
                let signature = declaration_head(context, checked, span)
                    .unwrap_or_else(|| format!("module {}", context.names.str(name)));
                value = Some(markup_parts(
                    package_container(context, checked).as_deref(),
                    &signature,
                    None,
                ));
                break;
            }
        }
        if let Some(value) = value {
            return Some(hover(source, span, value));
        }
    }

    // Keywords, last. Ported from rust-analyzer's `ide/src/hover.rs` and
    // `ide/src/hover/render.rs`, piece for piece:
    //
    // | rust-analyzer                            | here                        |
    // |------------------------------------------|-----------------------------|
    // | `pick_best_token` over `token_at_offset`  | `pick_best_token`           |
    // | `render::keyword`, last in the `or_else` chain | `keyword`, last fallback |
    // | `keyword_hints` → `KeywordHint`           | the same two                |
    // | `token.parent()` off the lossless CST     | `parent_at` — smallest AST node covering the token |
    // | `find_std_module(std, "if_keyword")`      | `find_doc_item("if_keyword")` |
    // | `std::if_keyword`'s rustdoc               | `KEYWORD_DOCS`              |
    // | `markup()` — fence, rule, docs            | `markup()` — same shape     |
    //
    // The one piece that cannot be copied is *where the prose lives*. r-a
    // resolves a keyword to a real module in `std` — `#[doc(keyword = "if")]
    // mod if_keyword {}` in `library/core/src/keyword_docs.rs`, `include!`d
    // into std's crate root precisely so the lookup can find it there — and
    // renders that module's rustdoc, so the text is library data and the
    // server holds none of it. Yel's stdlib has no doc-carrying declaration to
    // hang keyword prose on, so `KEYWORD_DOCS` *is* that file, keyed the same
    // way. If yel grows one, the table is what moves and `find_doc_item` is
    // the only code that changes.
    //
    // Checked LAST on purpose: no yel keyword is reserved (`token.rs`), so a
    // keyword that resolved as a *name* above already hovered as that name.
    // Literals sit between the two fallbacks: they are token facts like
    // keywords, but `true`/`false` are keyword tokens, so keywords must still
    // get their turn after.
    literal(source, source_id, offset, &file.ast)
        .or_else(|| keyword(config, source, source_id, offset, &file.ast))
}

/// r-a's `render::literal`: the token's own value, restated. r-a shows a
/// numeric literal in the bases the source did not use and a string's cooked
/// value; the yel equivalents below, plus the two literal kinds rust does not
/// have (units, colors).
fn literal(
    source: &yelc_base::Source,
    source_id: yelc_base::SourceId,
    offset: usize,
    file: &ast::File,
) -> Option<Hover> {
    use yelc_syntax::token::TokenKind::*;
    let (kind, mut span) = pick_best_token(source, source_id, offset)?;
    let text = &source.content[span.start..span.end];

    // r-a's `render::literal`: the type fenced, a rule, then
    // `value of literal: …` — integers restated in the bases the source did
    // not use, floats as their bits, strings and chars backticked.
    let (ty, value) = match kind {
        INT_LITERAL => {
            let digits: String = text.chars().filter(|c| *c != '_').collect();
            let n: i64 = digits.parse().ok()?;
            // `int` is an alias for `s32` (LANGUAGE.md § Primitive Types).
            ("s32", format!("{n} (0x{n:X}|0b{n:b})"))
        }
        FLOAT_LITERAL => {
            let digits: String = text.chars().filter(|c| *c != '_').collect();
            let f: f64 = digits.parse().ok()?;
            // `float` is an alias for `f32`.
            ("f32", format!("{f} (bits: 0x{:X})", f.to_bits()))
        }
        UNIT_LITERAL => {
            let suffix_at = text
                .find(|c: char| !c.is_ascii_digit() && c != '.' && c != '_')
                .unwrap_or(text.len());
            let (number, suffix) = text.split_at(suffix_at);
            (unit_type(suffix), format!("{number} ({suffix})"))
        }
        COLOR_LITERAL => ("color", color_value(text)),
        CHAR_LITERAL => {
            let c = text.trim_matches('\'').chars().next()?;
            ("char", format!("`{c}`"))
        }
        STRING_LITERAL => {
            let body = text.strip_prefix('"')?.strip_suffix('"').unwrap_or(text);
            ("string", format!("`{body}`"))
        }
        // A chunk of an interpolated string. The chunk is what the lexer
        // hands over, but the hover is about the whole string — r-a treats a
        // format string as one range the same way — so the span widens to the
        // enclosing interpolation expression, and the value shows the whole
        // string. The `{…}` parts between chunks keep their own hovers by
        // resolving in the usage path above, which runs first.
        TEMPLATE_LITERAL | TEMPLATE_MIDDLE_LITERAL | TEMPLATE_END_LITERAL => {
            if let Some(whole) = interpolation_at(file, offset) {
                span = whole;
            }
            let shown = &source.content[span.start..span.end];
            let body = shown
                .strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
                .unwrap_or(shown);
            ("string", format!("`{body}`"))
        }
        _ => return None,
    };

    Some(hover(
        source,
        span,
        format!("```yel\n{ty}\n```\n---\n\nvalue of literal: {value}"),
    ))
}

/// LANGUAGE.md § UI Types: which type a unit suffix constructs.
fn unit_type(suffix: &str) -> &'static str {
    match suffix {
        "px" | "pt" | "in" | "mm" | "cm" => "length",
        "phx" => "physical-length",
        "deg" | "rad" | "turn" => "angle",
        "ms" | "s" => "duration",
        "%" => "percent",
        "rem" => "relative-font-size",
        _ => "unknown",
    }
}

/// The smallest interpolated-string expression whose span contains `offset` —
/// smallest, because strings nest through interpolation (`"{ ["{x}"][0] }"`),
/// and the innermost string is the one being hovered.
fn interpolation_at(file: &ast::File, offset: usize) -> Option<yelc_base::Span> {
    let mut finder = InterpolationAt { offset, best: None };
    Visitor::visit_file(&mut finder, file);
    finder.best
}

struct InterpolationAt {
    offset: usize,
    best: Option<yelc_base::Span>,
}

impl Visitor for InterpolationAt {
    fn visit_expr(&mut self, node: &ast::Expr) {
        if let ast::ExprKind::Interpolation(_) = &node.kind {
            let contains = node.span.start <= self.offset && self.offset < node.span.end;
            let smaller = self
                .best
                .is_none_or(|best| node.span.end - node.span.start < best.end - best.start);
            if contains && smaller {
                self.best = Some(node.span);
            }
        }
        ast::visit::walk_expr(self, node);
    }
}

/// `#rgb`, `#rgba`, `#rrggbb`, `#rrggbbaa` — the four widths the lexer's
/// 3..8-digit rule is written for; anything else renders without components.
fn color_value(text: &str) -> String {
    let parts = text.strip_prefix('#').and_then(|hex| {
        let nibble = |i: usize| u8::from_str_radix(&hex[i..=i], 16).ok().map(|v| v * 17);
        let byte = |i: usize| u8::from_str_radix(&hex[i..i + 2], 16).ok();
        match hex.len() {
            3 => Some((nibble(0)?, nibble(1)?, nibble(2)?, None)),
            4 => Some((nibble(0)?, nibble(1)?, nibble(2)?, Some(nibble(3)?))),
            6 => Some((byte(0)?, byte(2)?, byte(4)?, None)),
            8 => Some((byte(0)?, byte(2)?, byte(4)?, Some(byte(6)?))),
            _ => None,
        }
    });
    match parts {
        Some((r, g, b, None)) => format!("{text} (rgb({r}, {g}, {b}))"),
        Some((r, g, b, Some(a))) => format!("{text} (rgba({r}, {g}, {b}, {a}))"),
        None => text.to_string(),
    }
}

/// r-a's `render::keyword`: gate on the config, take the token, ask
/// `keyword_hints` what to look up, resolve it, render it.
fn keyword(
    config: HoverConfig,
    source: &yelc_base::Source,
    source_id: yelc_base::SourceId,
    offset: usize,
    file: &ast::File,
) -> Option<Hover> {
    if !config.documentation || !config.keywords {
        return None;
    }
    // Via the real lexer, so a keyword spelled inside a string or a comment is
    // never one.
    let (kind, span) = pick_best_token(source, source_id, offset)?;
    if !yelc_syntax::token::KEYWORD_FIRST.contains(kind) {
        return None;
    }
    let hint = keyword_hints(kind, parent_at(file, span));
    let item = find_doc_item(&hint.doc_item)?;
    Some(hover(source, span, markup(hint.description, item)))
}

/// r-a's `pick_best_token` over `SyntaxNode::token_at_offset`: at a token
/// boundary *two* tokens touch the cursor, and a ranking decides which one the
/// hover is about. Same order as r-a's — a name outranks a keyword, a keyword
/// outranks punctuation, trivia loses to everything — and the same tie-break,
/// `max_by_key`, which keeps the right-hand token.
///
/// The ranking is why `count|` (cursor at the very end of a name) still hovers
/// `count` instead of the whitespace after it.
fn pick_best_token(
    source: &yelc_base::Source,
    source_id: yelc_base::SourceId,
    offset: usize,
) -> Option<(yelc_syntax::token::TokenKind, yelc_base::Span)> {
    let mut scratch = yelc_base::Diagnostics::new();
    let lexed = yelc_syntax::lexer::lex(source_id, &source.content, &mut scratch);

    let mut touching = Vec::new();
    let mut start = 0usize;
    for (kind, width) in lexed.tokens.iter().zip(&lexed.widths) {
        if start > offset {
            break;
        }
        let end = start + *width as usize;
        if end >= offset {
            let span = yelc_base::Span {
                source: source_id,
                start,
                end,
            };
            touching.push((*kind, span));
        }
        start = end;
    }

    touching.into_iter().max_by_key(|(kind, _)| rank(*kind))
}

/// r-a's ranking closure in `hover_offset`, with yel's token kinds. Keywords
/// rank *below* identifiers for the reason r-a has them there and the reason
/// the keyword path runs last: a keyword is what a token is when nothing more
/// specific claimed it.
fn rank(kind: yelc_syntax::token::TokenKind) -> u8 {
    use yelc_syntax::token::{KEYWORD_FIRST, TokenKind::*};
    match kind {
        IDENTIFIER => 4,
        // r-a ranks INT_NUMBER with IDENT; literals stay just under names so
        // `42|;` hovers the number, not the semicolon.
        INT_LITERAL
        | FLOAT_LITERAL
        | UNIT_LITERAL
        | COLOR_LITERAL
        | CHAR_LITERAL
        | STRING_LITERAL
        | TEMPLATE_LITERAL
        | TEMPLATE_MIDDLE_LITERAL
        | TEMPLATE_END_LITERAL => 3,
        kind if KEYWORD_FIRST.contains(kind) => 2,
        kind if kind.is_trivia() => 0,
        _ => 1,
    }
}

/// r-a's `KeywordHint`: what to *show* and what to *look up*, decided together
/// because both depend on the token's parent.
///
/// r-a's third field, `actions`, carries go-to-type targets for a keyword whose
/// expression has a type (`match: Option<i32>`). It is not ported: this server
/// exposes no go-to-type and renders no expression types, and a field nothing
/// can fill is worse than its absence. For the same reason `description` is the
/// spelling and nothing else — r-a appends `: {ty}` there.
struct KeywordHint {
    description: &'static str,
    /// r-a's `keyword_mod` — the name of the item whose docs to render.
    doc_item: String,
}

/// r-a's `keyword_hints`. Same job: one token kind can mean two things, and the
/// parent node says which. r-a splits `fn` into the item keyword `fn_keyword`
/// and the primitive `prim_fn` by whether the parent is an `ast::FnPtrType`;
/// this splits yel's ambiguous keywords on the same principle.
fn keyword_hints(kind: yelc_syntax::token::TokenKind, parent: Option<ParentKind>) -> KeywordHint {
    use yelc_syntax::token::TokenKind::*;

    let description = kind.spelling();
    let doc_item = match (kind, parent) {
        // Exactly r-a's `fn`/`prim_fn` split. `on-click: func(a: s32);` is a
        // property whose *type* is a func; `bump: func(s) { … }` in a global is
        // a declaration. Same keyword, different thing.
        (FUNC_KW, Some(ParentKind::FuncType)) => "prim_func".to_owned(),
        // `if`/`else`/`for` in a template mount and unmount children; in a
        // function body they are ordinary control flow. The whole reason
        // `parent_at` tracks the statement forms too is that a handler's `if`
        // nests inside a template `if` and would otherwise inherit its prose.
        (IF_KW, Some(ParentKind::TemplateIf)) => "if_template".to_owned(),
        (ELSE_KW, Some(ParentKind::TemplateIf)) => "else_template".to_owned(),
        (FOR_KW, Some(ParentKind::TemplateFor)) => "for_template".to_owned(),
        // `in` binds the loop item in `for x in xs`; on a global property it is
        // a direction. `out` and `in-out` are only ever directions.
        (IN_KW, Some(ParentKind::PropertyDirection)) => "in_direction".to_owned(),
        _ => format!("{description}_keyword"),
    };
    KeywordHint {
        description,
        doc_item,
    }
}

/// One documented keyword item — r-a's `#[doc(keyword = "…")] mod …_keyword {}`
/// flattened into a row.
struct KeywordDoc {
    /// What `keyword_hints` composes. r-a matches std's *module name*; this
    /// matches the same string.
    name: &'static str,
    docs: &'static str,
}

/// r-a's `find_std_module`: a linear scan for the item whose name matches.
/// The scan is the point — the caller composes a *name* and the lookup knows
/// nothing about keywords, so moving these docs into the stdlib later replaces
/// this function's body and nothing else.
fn find_doc_item(name: &str) -> Option<&'static KeywordDoc> {
    KEYWORD_DOCS.iter().find(|item| item.name == name)
}

/// r-a's `markup`: the hovered thing as code, a rule, then its docs.
fn markup(description: &str, item: &KeywordDoc) -> String {
    format!("```yel\n{description}\n```\n___\n\n{}", item.docs)
}

/// The stand-in for `library/core/src/keyword_docs.rs`. Prose from
/// `LANGUAGE.md`; one row per *meaning*, not per token, which is what the
/// `_template`/`_direction`/`prim_` keys buy.
static KEYWORD_DOCS: &[KeywordDoc] = &[
    KeywordDoc {
        name: "component_keyword",
        docs: "Declares a UI component: properties (reactive state), functions, and a \
               template of elements that re-renders when the state it reads changes.",
    },
    KeywordDoc {
        name: "global_keyword",
        docs: "Declares a global: shared state and functions, in-tree or at the host \
               boundary, accessed by components.",
    },
    KeywordDoc {
        name: "record_keyword",
        docs: "Declares a record type: named, typed fields.",
    },
    KeywordDoc {
        name: "enum_keyword",
        docs: "Declares an enumeration of bare cases.",
    },
    KeywordDoc {
        name: "variant_keyword",
        docs: "Declares a variant type: cases, each with an optional payload.",
    },
    KeywordDoc {
        name: "element_keyword",
        docs: "Declares a built-in element provided by the runtime.",
    },
    KeywordDoc {
        name: "extern_keyword",
        docs: "Marks a declaration implemented outside this package: an import contract \
               with the host.",
    },
    KeywordDoc {
        name: "package_keyword",
        docs: "Names this file's package: `package ns:name@version;`. Every file in a \
               package directory must declare the same one.",
    },
    KeywordDoc {
        name: "export_keyword",
        docs: "Publishes a declaration in the package interface.",
    },
    KeywordDoc {
        name: "func_keyword",
        docs: "Declares a function — a signature with a body.",
    },
    KeywordDoc {
        name: "prim_func",
        docs: "A function *type*: the signature of a value that can be called. A property \
               of func type is a callback the surrounding code supplies.",
    },
    KeywordDoc {
        name: "callback_keyword",
        docs: "Legacy callback declaration in a global; `name: func(…);` is the current \
               form.",
    },
    KeywordDoc {
        name: "let_keyword",
        docs: "Binds a local in a function body.",
    },
    KeywordDoc {
        name: "if_keyword",
        docs: "Conditional statement: runs its block when the condition holds.",
    },
    KeywordDoc {
        name: "if_template",
        docs: "Conditional template branch: mounts its children while the condition holds \
               and unmounts them when it stops. Re-evaluated when the state it reads \
               changes.",
    },
    KeywordDoc {
        name: "else_keyword",
        docs: "The branch taken when no `if` condition matched.",
    },
    KeywordDoc {
        name: "else_template",
        docs: "The template branch mounted when no `if` condition matched.",
    },
    KeywordDoc {
        name: "for_keyword",
        docs: "Loops over a collection as a statement.",
    },
    KeywordDoc {
        name: "for_template",
        docs: "Repeats its template children once per item of a collection. An optional \
               `key(expr)` gives each item an identity so re-renders reconcile the list \
               instead of rebuilding it.",
    },
    KeywordDoc {
        name: "from_keyword",
        docs: "Names the package an `include` pulls from.",
    },
    KeywordDoc {
        name: "include_keyword",
        docs: "Brings a package's module into scope: `from \"ns:name@version\" include X;`.",
    },
    KeywordDoc {
        name: "return_keyword",
        docs: "Returns from the enclosing function. A block's trailing expression is still \
               its value; the two coexist.",
    },
    KeywordDoc {
        name: "in_keyword",
        docs: "In `for item in items`, separates the binder from the collection.",
    },
    KeywordDoc {
        name: "in_direction",
        docs: "Property direction: the host writes, the component reads.",
    },
    KeywordDoc {
        name: "out_keyword",
        docs: "Property direction: the component writes; the host is notified.",
    },
    KeywordDoc {
        name: "in-out_keyword",
        docs: "Property direction: both sides read and write.",
    },
    KeywordDoc {
        name: "key_keyword",
        docs: "In a template `for`: the expression identifying an item across re-renders \
               (list reconciliation).",
    },
    KeywordDoc {
        name: "set_keyword",
        docs: "The write half of a two-way binding: runs when the bound element value \
               changes.",
    },
    KeywordDoc {
        name: "bind_keyword",
        docs: "Two-way binding modifier: pairs a value binding with an implicit setter, so \
               element and state stay in sync both ways.",
    },
    KeywordDoc {
        name: "children_keyword",
        docs: "The slot where a component instance's children are inserted.",
    },
    KeywordDoc {
        name: "true_keyword",
        docs: "Boolean literal.",
    },
    KeywordDoc {
        name: "false_keyword",
        docs: "Boolean literal.",
    },
];

/// The distinctions `keyword_hints` reads. Only what changes an answer is here
/// — a parent that never disambiguates anything would be a variant no arm
/// matches.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ParentKind {
    TemplateIf,
    TemplateFor,
    StatementIf,
    StatementFor,
    FuncType,
    PropertyDirection,
}

/// r-a reads the parent straight off a lossless CST (`token.parent()`). Yel's
/// AST keeps spans but not every token, so the parent is the *smallest* node
/// whose span covers the keyword — the same narrowest-wins rule `AtCursor`
/// applies to usages, for the same reason: an inner `if` must not be answered
/// by the template `if` it happens to sit inside.
fn parent_at(file: &ast::File, span: yelc_base::Span) -> Option<ParentKind> {
    let mut walk = ParentAt { span, found: None };
    Visitor::visit_file(&mut walk, file);
    walk.found.map(|(_, kind)| kind)
}

struct ParentAt {
    span: yelc_base::Span,
    /// Width and kind of the narrowest covering node seen so far.
    found: Option<(usize, ParentKind)>,
}

impl ParentAt {
    fn consider(&mut self, span: yelc_base::Span, kind: ParentKind) {
        if span.source != self.span.source
            || span.start > self.span.start
            || span.end < self.span.end
        {
            return;
        }
        let width = span.end - span.start;
        let better = match self.found {
            Some((best, _)) => width < best,
            None => true,
        };
        if better {
            self.found = Some((width, kind));
        }
    }
}

impl Visitor for ParentAt {
    fn visit_if_node(&mut self, node: &ast::IfNode) {
        self.consider(node.span, ParentKind::TemplateIf);
        ast::visit::walk_if_node(self, node);
    }

    fn visit_if_stmt(&mut self, node: &ast::IfStmt) {
        self.consider(node.span, ParentKind::StatementIf);
        ast::visit::walk_if_stmt(self, node);
    }

    fn visit_for_node(&mut self, node: &ast::ForNode) {
        // One node for both positions (`ForNode`'s doc); the body is the only
        // thing that says which one this is.
        let kind = match node.body {
            ast::ForBody::Nodes(_) => ParentKind::TemplateFor,
            ast::ForBody::Statements(_) => ParentKind::StatementFor,
        };
        self.consider(node.span, kind);
        ast::visit::walk_for_node(self, node);
    }

    fn visit_type_ref(&mut self, node: &ast::TypeRef) {
        if matches!(node.kind, ast::TypeKind::Func(_)) {
            self.consider(node.span, ParentKind::FuncType);
        }
        ast::visit::walk_type_ref(self, node);
    }

    fn visit_global_property(&mut self, node: &ast::GlobalProperty) {
        // The only place a direction keyword is grammatical (`items.rs`).
        if node.direction.is_some() {
            self.consider(node.span, ParentKind::PropertyDirection);
        }
        ast::visit::walk_global_property(self, node);
    }
}

/// Hover for a file the compiled package does not contain — a `.yelir` dump,
/// a file in a directory with no package. The file is treated as a one-file
/// package of its own: the same pipeline `check_package_with_overlay` runs
/// (identity, includes, lowering), minus the directory walk, over whatever
/// the parser's recovery salvaged. Definitions, members and usages then
/// hover exactly as far as the parse got; keywords and literals always
/// answer.
fn standalone_hover(
    path: &Path,
    overlay: Option<Arc<str>>,
    position: Position,
    config: HoverConfig,
) -> Option<Hover> {
    let content = match overlay {
        Some(content) => content.to_string(),
        None => std::fs::read_to_string(path).ok()?,
    };

    // A fresh context: the caller's has the *real* package registered, and
    // this file deliberately is not part of it.
    let mut context = yelc_sema::CompilerContext::with_intrinsics(yelc_sema::PackageId::LOCAL);
    let program_module = context.compilation.add_package(
        yelc_sema::PackageId::LOCAL,
        yelc_sema::PackageRole::Program,
        None,
    );
    let source_id = context.sources.add_file(path, content.clone());
    context
        .compilation
        .assign_file(source_id, yelc_sema::PackageId::LOCAL, program_module);
    let parsed = yelc_syntax::parse(
        source_id,
        &content,
        &context.names,
        &mut context.diagnostics,
    );

    let files = vec![parsed];
    let identity = yelc_hir::check_package_identity(&files, &mut context);
    yelc_hir::resolve_includes(&files, &[], STD_MODULES, &mut context);
    let module = yelc_hir::lower_files(&files, &mut context);
    let checked = yelc_hir::CheckedPackage {
        paths: vec![path.to_path_buf()],
        parsed: files,
        identity,
        module,
    };
    hover_in_package(
        &context,
        &checked,
        source_id,
        &checked.parsed[0],
        position,
        config,
    )
}

/// r-a's `markup()` in `hover/render.rs`, shape for shape: an optional fenced
/// container path, the fenced signature, and docs after a `___` rule.
fn markup_parts(container: Option<&str>, signature: &str, docs: Option<&str>) -> String {
    let mut buf = String::new();
    if let Some(container) = container {
        buf.push_str(&format!("```yel\n{container}\n```\n\n"));
    }
    buf.push_str(&format!("```yel\n{signature}\n```"));
    if let Some(docs) = docs {
        buf.push_str(&format!("\n___\n\n{docs}"));
    }
    buf
}

fn package_container(
    context: &yelc_sema::CompilerContext,
    checked: &yelc_hir::CheckedPackage,
) -> Option<String> {
    checked.identity.as_ref().map(|identity| {
        format!(
            "{}:{}",
            context.names.str(identity.namespace),
            context.names.str(identity.name)
        )
    })
}

fn render_definition(
    context: &yelc_sema::CompilerContext,
    checked: &yelc_hir::CheckedPackage,
    definition: &yelc_sema::Definition,
) -> String {
    // r-a's definition hover: container path fenced, then the declaration
    // head as the language spells it, then docs. The container is the
    // package, yel's module-path equivalent.
    let name = context.names.str(definition.name).to_string();
    let export = if definition.is_export { "export " } else { "" };
    let signature =
        declaration_head(context, checked, definition.span).unwrap_or(match definition.kind {
            yelc_sema::DefKind::Component => format!("{export}component {name}"),
            yelc_sema::DefKind::Global => format!("{export}global {name}"),
            yelc_sema::DefKind::Value => match definition.ty {
                Some(ty) => format!(
                    "{export}{name}: {}",
                    yelc_hir::emit_hir::render_ty(context, ty)
                ),
                None => format!("{export}{name}"),
            },
            yelc_sema::DefKind::Type => format!("{export}{name}"),
        });
    let container = package_container(context, checked);
    let docs = checked
        .module
        .docs
        .get(&definition.id)
        .map(|doc| context.names.str(*doc).to_string());
    markup_parts(container.as_deref(), &signature, docs.as_deref())
}

/// One member row, rendered as `Owner.member: ty`.
fn render_member(
    context: &yelc_sema::CompilerContext,
    checked: &yelc_hir::CheckedPackage,
    owner: yelc_base::Name,
    member: yelc_base::Name,
    ty: Option<yelc_sema::Ty>,
    name_span: yelc_base::Span,
) -> String {
    // r-a's field hover: the container fenced, then the declaration. The
    // verbatim head keeps what the interned type erases — parameter names,
    // the written return type, attributes; `render_ty` is the fallback when
    // the AST no longer has the declaration (recovery ate it).
    let owner = context.names.str(owner).to_string();
    let member = context.names.str(member).to_string();
    let signature = declaration_head(context, checked, name_span).unwrap_or(match ty {
        Some(ty) => format!("{member}: {}", yelc_hir::emit_hir::render_ty(context, ty)),
        None => member,
    });
    markup_parts(Some(&owner), &signature, None)
}

/// The declaration as written — attributes included, body and default value
/// excluded — sliced verbatim from the source. What r-a reconstructs from
/// HIR, yel reads back out of the file: the definition table's span is the
/// name, and the AST around it knows where the head ends.
fn declaration_head(
    context: &yelc_sema::CompilerContext,
    checked: &yelc_hir::CheckedPackage,
    name_span: yelc_base::Span,
) -> Option<String> {
    // A defaulted span (a member row recovery never filled) would slice an
    // empty head and render a blank fence; the fallback is better.
    if name_span.start == name_span.end {
        return None;
    }
    let file = checked
        .parsed
        .iter()
        .find(|file| file.source == name_span.source)?;
    let mut finder = DeclHeadAt {
        name_span,
        head: None,
    };
    Visitor::visit_file(&mut finder, &file.ast);
    let (start, end) = finder.head?;
    let source = context.sources.get(name_span.source)?;
    let text = source.content.get(start..end)?;
    // Members sit indented; the fence should not carry the indentation.
    Some(text.lines().map(str::trim).collect::<Vec<_>>().join("\n"))
}

struct DeclHeadAt {
    name_span: yelc_base::Span,
    head: Option<(usize, usize)>,
}

impl DeclHeadAt {
    fn named(&mut self, name: &ast::MaybeIdent, start: usize, end: usize) {
        if let Some(ident) = name.present() {
            if ident.span.start == self.name_span.start && ident.span.end == self.name_span.end {
                self.head = Some((start, end));
            }
        }
    }

    fn attributes_start(attributes: &Option<ast::AttributeList>, declaration: usize) -> usize {
        attributes
            .as_ref()
            .map_or(declaration, |list| list.span.start)
    }
}

impl Visitor for DeclHeadAt {
    fn visit_function_decl(&mut self, node: &ast::FunctionDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        let end = node
            .signature
            .present()
            .map_or(self.name_span.end, |signature| signature.span.end);
        self.named(&node.name, start, end);
        ast::visit::walk_function_decl(self, node);
    }

    fn visit_property_decl(&mut self, node: &ast::PropertyDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, node.ty.span.end);
        ast::visit::walk_property_decl(self, node);
    }

    fn visit_global_property(&mut self, node: &ast::GlobalProperty) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, node.ty.span.end);
        ast::visit::walk_global_property(self, node);
    }

    fn visit_component_decl(&mut self, node: &ast::ComponentDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_component_decl(self, node);
    }

    fn visit_global_decl(&mut self, node: &ast::GlobalDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_global_decl(self, node);
    }

    fn visit_record_decl(&mut self, node: &ast::RecordDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_record_decl(self, node);
    }

    fn visit_enum_decl(&mut self, node: &ast::EnumDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_enum_decl(self, node);
    }

    fn visit_variant_decl(&mut self, node: &ast::VariantDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_variant_decl(self, node);
    }

    fn visit_element_decl(&mut self, node: &ast::ElementDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_element_decl(self, node);
    }

    fn visit_extern_component_decl(&mut self, node: &ast::ExternComponentDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_extern_component_decl(self, node);
    }

    fn visit_module_decl(&mut self, node: &ast::ModuleDecl) {
        let start = Self::attributes_start(&node.attributes, node.span.start);
        self.named(&node.name, start, self.name_span.end);
        ast::visit::walk_module_decl(self, node);
    }

    fn visit_variant_case(&mut self, node: &ast::VariantCase) {
        // The whole case — `rgba(tuple<u8, u8, u8, u8>)` as written, not the
        // property-style `rgba: tuple<…>` the interned-type fallback spells.
        self.named(&node.name, node.span.start, node.span.end);
        ast::visit::walk_variant_case(self, node);
    }
}

/// What a usage under the cursor resolved to. Copied out of the HIR during the
/// index sweep — `Name` and `DefId` are plain handles — so rendering borrows
/// nothing from the module.
enum UsageTarget {
    Def(yelc_sema::DefId),
    Member {
        owner: yelc_sema::DefId,
        member: yelc_base::Name,
    },
    Local {
        name: yelc_base::Name,
        /// Parameters render bare, `let` bindings with their keyword.
        param: bool,
    },
    Intrinsic(yelc_base::Name),
    /// An instantiated element with no definition behind it — every builtin
    /// element today, since the element inventory arrives from Yel source
    /// later (`HirInstantiate::target`'s doc). Saying so honestly is not H4's
    /// "Unknown-and-hoped-over": nothing downstream consumes this.
    Element(yelc_base::Name),
}

fn render_usage(
    context: &yelc_sema::CompilerContext,
    checked: &yelc_hir::CheckedPackage,
    target: &UsageTarget,
) -> Option<String> {
    match target {
        UsageTarget::Def(id) => Some(render_definition(context, checked, context.defs.get(*id))),
        UsageTarget::Member { owner, member } => {
            let owner_def = context.defs.get(*owner);
            let row = context
                .defs
                .members(*owner)
                .iter()
                .find(|row| row.name == *member);
            Some(render_member(
                context,
                checked,
                owner_def.name,
                *member,
                row.and_then(|row| row.ty),
                row.map_or(yelc_base::Span::default(), |row| row.span),
            ))
        }
        UsageTarget::Local { name, param } => {
            // r-a's local hover: the binding as written — `let x` for a let,
            // the bare name for a parameter. Types arrive with stage 4.
            let name = context.names.str(*name);
            let signature = if *param {
                name.to_string()
            } else {
                format!("let {name}")
            };
            Some(markup_parts(None, &signature, None))
        }
        UsageTarget::Element(name) => Some(markup_parts(
            None,
            &context.names.str(*name).to_string(),
            Some(
                "builtin element (no declaration yet — the element inventory arrives with the stdlib)",
            ),
        )),
        UsageTarget::Intrinsic(name) => {
            let rows = context.intrinsics.overloads(*name);
            if rows.is_empty() {
                return None;
            }
            let text = context.names.str(*name).to_string();
            // One fence, one overload per line — the nearest yel spelling of
            // r-a's signature fence for a function.
            let signature = rows
                .iter()
                .map(|id| {
                    let row = context.intrinsics.get(*id);
                    let params = row
                        .params
                        .iter()
                        .map(|ty| yelc_hir::emit_hir::render_ty(context, *ty))
                        .collect::<Vec<_>>()
                        .join(", ");
                    match row.ret {
                        Some(ret) => format!(
                            "func {text}({params}) -> {}",
                            yelc_hir::emit_hir::render_ty(context, ret)
                        ),
                        None => format!("func {text}({params})"),
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");
            Some(markup_parts(None, &signature, Some("compiler intrinsic")))
        }
    }
}

/// The index sweep: every resolution the lowering recorded, keyed by the
/// `HirId` the map can reach from an AST node. `names` holds name expressions
/// keyed by themselves; `callees` holds call and instantiate targets keyed by
/// the *call's* id, because `HirCallee` is an enum with no node of its own.
struct UsageIndex<'m> {
    body: Option<&'m yelc_hir::HirBody>,
    names: HashMap<yelc_hir::HirId, UsageTarget>,
    callees: HashMap<yelc_hir::HirId, UsageTarget>,
}

impl UsageIndex<'_> {
    fn callee_target(&self, callee: &yelc_hir::HirCallee) -> Option<UsageTarget> {
        Some(match callee {
            yelc_hir::HirCallee::Local(id) => {
                let body = self.body?;
                UsageTarget::Local {
                    name: body.locals.get(*id)?.name,
                    param: <yelc_hir::LocalId as yelc_base::Idx>::index(*id) < body.params as usize,
                }
            }
            yelc_hir::HirCallee::Def(id) => UsageTarget::Def(*id),
            yelc_hir::HirCallee::Intrinsic(name) => UsageTarget::Intrinsic(*name),
            yelc_hir::HirCallee::Member { base, member } => UsageTarget::Member {
                owner: *base,
                member: *member,
            },
            // H4: unresolved is unresolved — there is nothing to show.
            yelc_hir::HirCallee::Unresolved(_) => return None,
        })
    }
}

impl yelc_hir::visit::Visitor for UsageIndex<'_> {
    fn visit_body(&mut self, body: &yelc_hir::HirBody) {
        // Binders too: a `let` name or parameter maps from its own AST node,
        // so hovering the declaration site resolves through the same index.
        if let Some(body) = self.body {
            for (index, local) in body.locals.iter().enumerate() {
                self.names.insert(
                    local.hir_id,
                    UsageTarget::Local {
                        name: local.name,
                        param: index < body.params as usize,
                    },
                );
            }
        }
        yelc_hir::visit::walk_body(self, body);
    }

    fn visit_expr(&mut self, expr: &yelc_hir::HirExpr) {
        use yelc_hir::HirExprKind;
        match &expr.kind {
            HirExprKind::Local(id) => {
                if let Some(body) = self.body
                    && let Some(local) = body.locals.get(*id)
                {
                    self.names.insert(
                        expr.hir_id,
                        UsageTarget::Local {
                            name: local.name,
                            param: <yelc_hir::LocalId as yelc_base::Idx>::index(*id)
                                < body.params as usize,
                        },
                    );
                }
            }
            HirExprKind::Def(id) => {
                self.names.insert(expr.hir_id, UsageTarget::Def(*id));
            }
            HirExprKind::Prop { owner, member } => {
                self.names.insert(
                    expr.hir_id,
                    UsageTarget::Member {
                        owner: *owner,
                        member: *member,
                    },
                );
            }
            HirExprKind::Intrinsic(name) => {
                self.names
                    .insert(expr.hir_id, UsageTarget::Intrinsic(*name));
            }
            HirExprKind::Call { callee, .. } => {
                if let Some(target) = self.callee_target(callee) {
                    self.callees.insert(expr.hir_id, target);
                }
            }
            HirExprKind::Instantiate(instantiate) => {
                // Unresolved is the *normal* case for an element today (every
                // builtin), and unlike an unresolved call it still says
                // something true: this name instantiates an element.
                let target = match &instantiate.target {
                    yelc_hir::HirCallee::Unresolved(name) => Some(UsageTarget::Element(*name)),
                    other => self.callee_target(other),
                };
                if let Some(target) = target {
                    self.callees.insert(expr.hir_id, target);
                }
            }
            _ => {}
        }
        yelc_hir::visit::walk_expr(self, expr);
    }
}

/// One AST node worth trying at the cursor: the node whose mapped `HirId` to
/// look up, and the span the hover should highlight. `callee` marks spans
/// (a call's name, an element's name) whose resolution lives on the *call*
/// node rather than a node of their own.
struct CursorCandidate {
    node: yelc_syntax::NodeId,
    span: yelc_base::Span,
    callee: bool,
}

struct AtCursor {
    offset: usize,
    candidates: Vec<CursorCandidate>,
    /// Named types under the cursor — annotations and typed-record heads.
    /// A type position has no HIR node of its own (types are not
    /// re-represented — the stage-3 contract), so these resolve by root-scope
    /// lookup instead of through the map.
    type_names: Vec<(yelc_base::Span, yelc_base::Name)>,
}

impl AtCursor {
    fn consider(&mut self, node: yelc_syntax::NodeId, span: yelc_base::Span, callee: bool) {
        let contains = span.start <= self.offset && self.offset < span.end.max(span.start + 1);
        if contains {
            self.candidates.push(CursorCandidate { node, span, callee });
        }
    }
}

impl Visitor for AtCursor {
    fn visit_expr(&mut self, node: &ast::Expr) {
        match &node.kind {
            // The callee name has no expression node of its own; its
            // resolution is on the call. Only the name span is a candidate —
            // hovering an argument must fall through to the argument.
            ast::ExprKind::Call { callee, .. } => self.consider(node.id, callee.span, true),
            ast::ExprKind::PathCall { member, .. } => {
                if let Some(member) = member.present() {
                    self.consider(node.id, member.span, true);
                }
            }
            ast::ExprKind::Member { member, .. } | ast::ExprKind::OptionalMember { member, .. } => {
                if let Some(member) = member.present() {
                    self.consider(node.id, member.span, false);
                }
            }
            ast::ExprKind::Record {
                name: Some(name), ..
            } => {
                if let Some(ident) = name.present() {
                    if ident.span.start <= self.offset && self.offset < ident.span.end {
                        self.type_names.push((ident.span, ident.name));
                    }
                }
                self.consider(node.id, node.span, false);
            }
            // Literals are never usage candidates: a `#ff8000` desugars to
            // `Color.rgba(…)` and its node maps to that call, so letting it
            // resolve here hovers the desugaring instead of the color. The
            // literal fallback owns these.
            ast::ExprKind::Int(_)
            | ast::ExprKind::Float(_)
            | ast::ExprKind::Unit { .. }
            | ast::ExprKind::Color(_)
            | ast::ExprKind::Char(_)
            | ast::ExprKind::Bool(_)
            | ast::ExprKind::String(_) => {}
            _ => self.consider(node.id, node.span, false),
        }
        ast::visit::walk_expr(self, node);
    }

    fn visit_element_node(&mut self, node: &ast::ElementNode) {
        if let Some(name) = node.name.present() {
            self.consider(node.id, name.span, true);
        }
        ast::visit::walk_element_node(self, node);
    }

    fn visit_func_param(&mut self, node: &ast::FuncParam) {
        if let Some(name) = node.name.present() {
            // The binder's local maps from one of these two nodes; the
            // try-until-resolved loop makes guessing wrong free.
            self.consider(node.id, name.span, false);
            self.consider(name.id, name.span, false);
        }
        ast::visit::walk_func_param(self, node);
    }

    fn visit_closure_param(&mut self, node: &ast::ClosureParam) {
        if let Some(name) = node.name.present() {
            self.consider(node.id, name.span, false);
            self.consider(name.id, name.span, false);
        }
        ast::visit::walk_closure_param(self, node);
    }

    fn visit_type_ref(&mut self, node: &ast::TypeRef) {
        if let ast::TypeKind::Named(name) = node.kind {
            if node.span.start <= self.offset && self.offset < node.span.end {
                self.type_names.push((node.span, name));
            }
        }
        ast::visit::walk_type_ref(self, node);
    }

    fn visit_module_decl(&mut self, node: &ast::ModuleDecl) {
        if let Some(ident) = node.name.present() {
            if ident.span.start <= self.offset && self.offset < ident.span.end {
                self.type_names.push((ident.span, ident.name));
            }
        }
        ast::visit::walk_module_decl(self, node);
    }

    fn visit_use_decl(&mut self, node: &ast::UseDecl) {
        for name in std::iter::once(&node.base).chain(node.names.iter()) {
            if let Some(ident) = name.present() {
                if ident.span.start <= self.offset && self.offset < ident.span.end {
                    self.type_names.push((ident.span, ident.name));
                }
            }
        }
        ast::visit::walk_use_decl(self, node);
    }

    fn visit_let_stmt(&mut self, node: &ast::LetStmt) {
        if let Some(name) = node.name.present() {
            // The binder's local maps from one of these two nodes; the
            // try-until-resolved loop makes guessing wrong free.
            self.consider(node.id, name.span, false);
            self.consider(name.id, name.span, false);
        }
        ast::visit::walk_let_stmt(self, node);
    }
}

fn hover(source: &yelc_base::Source, span: yelc_base::Span, value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(Range {
            start: position_from_source(source, span.start),
            end: position_from_source(source, span.end),
        }),
    }
}

fn offset_from_position(content: &str, position: Position) -> usize {
    let mut line = 0u32;
    let mut character = 0u32;
    for (offset, ch) in content.char_indices() {
        if line == position.line && character >= position.character {
            return offset;
        }
        if ch == '\n' {
            if line == position.line {
                return offset;
            }
            line += 1;
            character = 0;
        } else {
            character += ch.len_utf16() as u32;
        }
    }
    content.len()
}

pub(super) fn document_symbol_request(state: &mut ServerState, request: Request) {
    let result = serde_json::from_value::<lsp_types::DocumentSymbolParams>(request.params);

    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");

            if let Some(content) = state.opened_files.get(&path) {
                let content = content.clone();
                let sender = state.threadpool_sender.clone();

                state.threadpool.execute(move || {
                    let symbols = scan_for_symbols(content);
                    let response = DocumentSymbolResponse::Nested(symbols);
                    let response = Response::new_ok(request.id, response);

                    sender
                        .send(MainLoopTask::SendResponse(Message::Response(response)))
                        .expect("send failed");
                });
            } else {
                log::warn!(
                    "document symbols for a file that was never opened: {}",
                    path.display()
                );
            }
        }
        Err(_) => {
            log::error!("malformed request params");
        }
    }
}

/// Ark hands the worker a pre-parsed `Arc<ast::File>`; yel reparses from the
/// content — the tree is not shared across threads and a parse is
/// milliseconds.
pub(crate) fn scan_for_symbols(content: Arc<str>) -> Vec<DocumentSymbol> {
    let interner = yelc_base::NameInterner::new();
    let mut diagnostics = yelc_base::Diagnostics::new();
    let parsed = yelc_syntax::parse(
        yelc_base::SourceId(0),
        &content,
        &interner,
        &mut diagnostics,
    );

    let mut scanner = SymbolScanner {
        symbols: Vec::new(),
        levels: Vec::new(),
        interner,
    };

    scanner.visit_file(&parsed.ast);
    let line_starts = compute_line_starts(&content);
    transform(&line_starts, scanner.symbols)
}

fn transform(line_starts: &[u32], symbols: Vec<Symbol>) -> Vec<DocumentSymbol> {
    symbols
        .into_iter()
        .map(|s| {
            let range = range_from_span(line_starts, s.total_span);
            let selection_range = range_from_span(line_starts, s.name_span);

            let children = s.children.map(|c| transform(line_starts, c));

            #[allow(deprecated)]
            DocumentSymbol {
                name: s.name,
                kind: convert_kind(s.kind),
                tags: None,
                detail: None,
                range,
                deprecated: None,
                selection_range,
                children,
            }
        })
        .collect()
}

fn convert_kind(kind: YelSymbolKind) -> SymbolKind {
    match kind {
        YelSymbolKind::Component => SymbolKind::CLASS,
        YelSymbolKind::Global => SymbolKind::MODULE,
        YelSymbolKind::Record => SymbolKind::STRUCT,
        YelSymbolKind::RecordField => SymbolKind::FIELD,
        YelSymbolKind::Enum => SymbolKind::ENUM,
        YelSymbolKind::EnumCase => SymbolKind::ENUM_MEMBER,
        YelSymbolKind::Variant => SymbolKind::ENUM,
        YelSymbolKind::VariantCase => SymbolKind::ENUM_MEMBER,
        YelSymbolKind::Element => SymbolKind::INTERFACE,
        YelSymbolKind::ExternComponent => SymbolKind::CLASS,
        YelSymbolKind::Property => SymbolKind::PROPERTY,
        YelSymbolKind::Function => SymbolKind::FUNCTION,
        YelSymbolKind::Include => SymbolKind::NAMESPACE,
    }
}

fn compute_line_starts(content: &str) -> Vec<u32> {
    let mut line_starts = vec![0];
    for (offset, ch) in content.char_indices() {
        if ch == '\n' {
            line_starts.push((offset + 1) as u32);
        }
    }
    line_starts
}

fn range_from_span(line_starts: &[u32], span: yelc_base::Span) -> Range {
    let start = position_from_offset(line_starts, span.start as u32);
    let end = position_from_offset(line_starts, span.end as u32);

    Range { start, end }
}

fn position_from_offset(line_starts: &[u32], offset: u32) -> Position {
    let line = line_starts
        .partition_point(|&start| start <= offset)
        .saturating_sub(1);
    let column = offset - line_starts[line];
    Position::new(line as u32, column)
}

#[derive(Debug)]
struct Symbol {
    name: String,
    name_span: yelc_base::Span,
    kind: YelSymbolKind,
    total_span: yelc_base::Span,
    children: Option<Vec<Symbol>>,
}

#[derive(Debug)]
enum YelSymbolKind {
    Component,
    Global,
    Record,
    RecordField,
    Enum,
    EnumCase,
    Variant,
    VariantCase,
    Element,
    ExternComponent,
    Property,
    Function,
    Include,
}

struct SymbolScanner {
    symbols: Vec<Symbol>,
    levels: Vec<usize>,
    interner: yelc_base::NameInterner,
}

impl SymbolScanner {
    fn add_symbol(
        &mut self,
        name: String,
        name_span: yelc_base::Span,
        kind: YelSymbolKind,
        total_span: yelc_base::Span,
    ) {
        self.symbols.push(Symbol {
            name,
            name_span,
            kind,
            total_span,
            children: None,
        });
    }

    fn start_children(&mut self) {
        self.levels.push(self.symbols.len() - 1);
    }

    fn stop_children(&mut self) {
        let parent = self.levels.pop().expect("missing start");
        let children = self.symbols.drain(parent + 1..).collect();
        self.symbols.last_mut().expect("missing parent").children = Some(children);
    }

    fn name_of(&self, ident: &ast::MaybeIdent, default_name: &str) -> (String, yelc_base::Span) {
        ensure_name(&self.interner, ident, default_name)
    }
}

impl Visitor for SymbolScanner {
    fn visit_component_decl(&mut self, node: &ast::ComponentDecl) {
        let (name, name_span) = self.name_of(&node.name, "<component>");
        self.add_symbol(name, name_span, YelSymbolKind::Component, node.span);

        self.start_children();
        for property in node.properties() {
            let (name, name_span) = self.name_of(&property.name, "<property>");
            self.add_symbol(name, name_span, YelSymbolKind::Property, property.span);
        }
        for function in node.functions() {
            let (name, name_span) = self.name_of(&function.name, "<fn>");
            self.add_symbol(name, name_span, YelSymbolKind::Function, function.span);
        }
        self.stop_children();
    }

    fn visit_global_decl(&mut self, node: &ast::GlobalDecl) {
        let (name, name_span) = self.name_of(&node.name, "<global>");
        self.add_symbol(name, name_span, YelSymbolKind::Global, node.span);

        self.start_children();
        for property in node.properties() {
            let (name, name_span) = self.name_of(&property.name, "<property>");
            self.add_symbol(name, name_span, YelSymbolKind::Property, property.span);
        }
        for callback in node.callbacks() {
            let (name, name_span) = self.name_of(&callback.name, "<fn>");
            self.add_symbol(name, name_span, YelSymbolKind::Function, callback.span);
        }
        self.stop_children();
    }

    fn visit_record_decl(&mut self, node: &ast::RecordDecl) {
        let (name, name_span) = self.name_of(&node.name, "<record>");
        self.add_symbol(name, name_span, YelSymbolKind::Record, node.span);

        self.start_children();
        for field in node.present_fields() {
            let (name, name_span) = self.name_of(&field.name, "<field>");
            self.add_symbol(name, name_span, YelSymbolKind::RecordField, field.span);
        }
        self.stop_children();
    }

    fn visit_enum_decl(&mut self, node: &ast::EnumDecl) {
        let (name, name_span) = self.name_of(&node.name, "<enum>");
        self.add_symbol(name, name_span, YelSymbolKind::Enum, node.span);

        self.start_children();
        for case in &node.cases {
            let (name, name_span) = ensure_name(&self.interner, case, "<case>");
            self.add_symbol(name, name_span, YelSymbolKind::EnumCase, name_span);
        }
        self.stop_children();
    }

    fn visit_variant_decl(&mut self, node: &ast::VariantDecl) {
        let (name, name_span) = self.name_of(&node.name, "<variant>");
        self.add_symbol(name, name_span, YelSymbolKind::Variant, node.span);

        self.start_children();
        for case in &node.cases {
            if let Some(case) = case.present() {
                let (name, name_span) = self.name_of(&case.name, "<case>");
                self.add_symbol(name, name_span, YelSymbolKind::VariantCase, case.span);
            }
        }
        self.stop_children();
    }

    fn visit_element_decl(&mut self, node: &ast::ElementDecl) {
        let (name, name_span) = self.name_of(&node.name, "<element>");
        self.add_symbol(name, name_span, YelSymbolKind::Element, node.span);
    }

    fn visit_extern_component_decl(&mut self, node: &ast::ExternComponentDecl) {
        let (name, name_span) = self.name_of(&node.name, "<extern component>");
        self.add_symbol(name, name_span, YelSymbolKind::ExternComponent, node.span);
    }

    fn visit_include_decl(&mut self, node: &ast::IncludeDecl) {
        let (name, name_span) = self.name_of(&node.name, "<include>");
        self.add_symbol(name, name_span, YelSymbolKind::Include, node.span);
    }
}

fn ensure_name(
    interner: &yelc_base::NameInterner,
    ident: &ast::MaybeIdent,
    default_name: &str,
) -> (String, yelc_base::Span) {
    match ident.present() {
        Some(ident) => (interner.str(ident.name).to_string(), ident.span),
        None => (default_name.into(), yelc_base::Span::default()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Render the hover at the `§` marker in `content`, via a real one-file
    /// package on disk plus the same text as an overlay — the code path the
    /// editor exercises.
    fn hover_full(test: &str, content_with_marker: &str) -> Option<Hover> {
        hover_full_named(test, "main.yel", content_with_marker)
    }

    fn hover_full_named(test: &str, filename: &str, content_with_marker: &str) -> Option<Hover> {
        let marker = content_with_marker.find('§').expect("marker");
        let content = content_with_marker.replace('§', "");
        let line = content_with_marker[..marker].matches('\n').count() as u32;
        let line_start = content_with_marker[..marker]
            .rfind('\n')
            .map_or(0, |position| position + 1);
        let character = (marker - line_start) as u32;

        let dir = std::env::temp_dir().join(format!("yelc-lsp-hover-{test}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join(filename);
        std::fs::write(&path, &content).unwrap();

        let hover = hover_at(
            &path,
            Some(Arc::from(content.as_str())),
            Position::new(line, character),
            HoverConfig::default(),
        );
        std::fs::remove_dir_all(&dir).ok();
        hover
    }

    fn hover_value(test: &str, content_with_marker: &str) -> Option<String> {
        hover_full(test, content_with_marker).map(|hover| match hover.contents {
            HoverContents::Markup(markup) => markup.value,
            other => panic!("unexpected hover contents: {other:?}"),
        })
    }

    // A *global*: `name: func(sig) { body }` is a callback with a body there,
    // while the same spelling in a component is a property of func type with
    // no body at all (stage 1's Surprise 5) — the first draft of this fixture
    // was a component and every test below silently walked an `Error` forest.
    const COUNTER: &str = "package my:app;\n\nglobal Counter {\n    count: s32 = 0;\n\n    bump: func(step: s32) -> s32 {\n        let amount = step;\n        count + amount\n    }\n}\n";

    #[test]
    fn declaration_name_still_hovers() {
        let content = COUNTER.replacen("count: s32", "co§unt: s32", 1);
        let value = hover_value("decl", &content).expect("hover");
        assert!(value.contains("count: s32"), "got: {value}");
        assert!(value.contains("s32"), "got: {value}");
    }

    #[test]
    fn property_usage_hovers() {
        let content = COUNTER.replacen("count + amount", "co§unt + amount", 1);
        let value = hover_value("prop-usage", &content).expect("hover");
        assert!(value.contains("count: s32"), "got: {value}");
        assert!(value.contains("s32"), "got: {value}");
    }

    /// The verbatim declaration head: parameter names and the written return
    /// type survive (the interned type erases both), and attributes show.
    #[test]
    fn function_hover_shows_signature_and_attributes() {
        let content = COUNTER.replacen(
            "bump: func(step: s32) -> s32 {",
            "@unsafe\n    bu§mp: func(step: s32) -> s32 {",
            1,
        );
        let value = hover_value("func-head", &content).expect("hover");
        assert!(
            value.contains("@unsafe\nbump: func(step: s32) -> s32"),
            "got: {value}"
        );
    }

    #[test]
    fn local_usage_hovers() {
        let content = COUNTER.replacen("count + amount", "count + amo§unt", 1);
        let value = hover_value("local-usage", &content).expect("hover");
        assert!(value.contains("let amount"), "got: {value}");
    }

    #[test]
    fn operator_position_hovers_nothing() {
        let content = COUNTER.replacen("count + amount", "count §+ amount", 1);
        assert_eq!(hover_value("operator", &content), None);
    }

    // The counter.yel shape: a component with a UI tree — the file a user
    // actually hovers first. Every spot below appeared as `-> nothing` in the
    // first real session's log before these paths existed.
    const COUNTER_UI: &str = "package my:app;\n\nexport component Counter {\n    count: s32 = 0;\n\n    VStack {\n        Text { \"Count: {count}\" }\n        Button {\n            label: \"increment\"\n            clicked: { count = count + 1; }\n        }\n        if count > 10 {\n            Text { \"high!\" }\n        }\n    }\n}\n";

    #[test]
    fn package_clause_hovers() {
        let content = COUNTER_UI.replacen("my:app", "my:a§pp", 1);
        let value = hover_value("package", &content).expect("hover");
        assert!(value.contains("my:app"), "got: {value}");
        assert!(value.contains("package"), "got: {value}");
    }

    #[test]
    fn element_name_hovers() {
        let content = COUNTER_UI.replacen("Button", "But§ton", 1);
        let value = hover_value("element", &content).expect("hover");
        assert!(value.contains("Button"), "got: {value}");
        assert!(value.contains("element"), "got: {value}");
    }

    #[test]
    fn interpolation_usage_hovers() {
        let content = COUNTER_UI.replacen("{count}", "{co§unt}", 1);
        let value = hover_value("interpolation", &content).expect("hover");
        assert!(value.contains("count: s32"), "got: {value}");
    }

    #[test]
    fn handler_usage_hovers() {
        let content = COUNTER_UI.replacen("count = count + 1", "count = co§unt + 1", 1);
        let value = hover_value("handler", &content).expect("hover");
        assert!(value.contains("count: s32"), "got: {value}");
    }

    #[test]
    fn ui_condition_usage_hovers() {
        let content = COUNTER_UI.replacen("count > 10", "co§unt > 10", 1);
        let value = hover_value("ui-condition", &content).expect("hover");
        assert!(value.contains("count: s32"), "got: {value}");
    }

    #[test]
    fn keyword_hovers() {
        let content = COUNTER_UI.replacen("export component", "export comp§onent", 1);
        let value = hover_value("keyword", &content).expect("hover");
        // r-a's markup shape: the hovered thing as code, a rule, then its docs.
        assert!(
            value.starts_with("```yel\ncomponent\n```\n___\n\n"),
            "got: {value}"
        );
        assert!(value.contains("template"), "got: {value}");
    }

    /// The lexer, not a text search, decides what is a keyword — a keyword
    /// spelled inside a string is a string.
    #[test]
    fn keyword_inside_a_string_does_not_hover() {
        let content = COUNTER_UI.replacen("\"increment\"", "\"if § then\"", 1);
        // The token is a *string*, so it hovers as one — never as the keyword
        // spelled inside it.
        let value = hover_value("keyword-in-string", &content).expect("hover");
        assert!(value.starts_with("```yel\nstring\n```"), "got: {value}");
        assert!(!value.contains("conditional"), "got: {value}");
    }

    #[test]
    fn integer_literal_hovers_with_bases() {
        let content = COUNTER_UI.replacen("count > 10", "count > 1§0", 1);
        let value = hover_value("int-literal", &content).expect("hover");
        assert!(value.starts_with("```yel\ns32\n```"), "got: {value}");
        assert!(
            value.contains("value of literal: 10 (0xA|0b1010)"),
            "got: {value}"
        );
    }

    #[test]
    fn string_literal_hovers_with_length() {
        let content = COUNTER_UI.replacen("\"increment\"", "\"incre§ment\"", 1);
        let value = hover_value("string-literal", &content).expect("hover");
        assert!(value.starts_with("```yel\nstring\n```"), "got: {value}");
        assert!(
            value.contains("value of literal: `increment`"),
            "got: {value}"
        );
    }

    #[test]
    fn color_literal_hovers_with_components() {
        let content = COUNTER_UI.replacen(
            "count: s32 = 0;",
            "count: s32 = 0;\n    tint: color = #ff8000;",
            1,
        );
        let content = content.replacen("#ff8000", "#ff80§00", 1);
        let value = hover_value("color-literal", &content).expect("hover");
        assert!(value.contains("rgb(255, 128, 0)"), "got: {value}");
    }

    #[test]
    fn interpolated_string_chunk_hovers_as_string() {
        let content = COUNTER_UI.replacen("\"Count: {count}\"", "\"Cou§nt: {count}\"", 1);
        let hover = hover_full("template-chunk", &content).expect("hover");
        let value = match &hover.contents {
            HoverContents::Markup(markup) => &markup.value,
            other => panic!("unexpected hover contents: {other:?}"),
        };
        assert!(value.starts_with("```yel\nstring\n```"), "got: {value}");
        // The value shows the whole string, not the hovered chunk.
        assert!(
            value.contains("value of literal: `Count: {count}`"),
            "got: {value}"
        );

        // One range for the whole string: from the opening quote through the
        // closing one — the r-a format-string behaviour the chunk span broke.
        let clean = COUNTER_UI;
        let string_start = clean.find("\"Count: {count}\"").unwrap();
        let string_end = string_start + "\"Count: {count}\"".len();
        let line = clean[..string_start].matches('\n').count() as u32;
        let line_offset = clean[..string_start].rfind('\n').map_or(0, |p| p + 1);
        let range = hover.range.expect("range");
        assert_eq!(range.start.line, line);
        assert_eq!(range.start.character, (string_start - line_offset) as u32);
        assert_eq!(range.end.character, (string_end - line_offset) as u32);
    }

    #[test]
    fn keyword_hover_is_off_when_configured_off() {
        let marker = COUNTER_UI.replacen("export component", "export comp§onent", 1);
        let content = marker.replace('§', "");
        let dir = std::env::temp_dir().join("yelc-lsp-hover-keyword-off");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.yel");
        std::fs::write(&path, &content).unwrap();
        let hover = hover_at(
            &path,
            Some(Arc::from(content.as_str())),
            Position::new(2, 11),
            HoverConfig {
                documentation: true,
                keywords: false,
            },
        );
        std::fs::remove_dir_all(&dir).ok();
        assert!(hover.is_none());
    }

    // The `keyword_hints` splits: one token kind, two meanings, told apart by
    // the parent node. Each pair below is the same spelling in both positions.

    #[test]
    fn if_hovers_as_template_or_statement_by_parent() {
        let template = COUNTER_UI.replacen("if count > 10", "i§f count > 10", 1);
        let value = hover_value("if-template", &template).expect("hover");
        assert!(value.contains("mounts its children"), "got: {value}");

        // The same `if` written inside a handler body — nested *within* the
        // template `if` above, which is exactly the case narrowest-wins fixes.
        let statement = COUNTER_UI.replacen(
            "clicked: { count = count + 1; }",
            "clicked: { i§f count > 0 { count = 0; } }",
            1,
        );
        let value = hover_value("if-statement", &statement).expect("hover");
        assert!(value.contains("Conditional statement"), "got: {value}");
    }

    #[test]
    fn func_hovers_as_type_or_declaration_by_parent() {
        // A component property of func type — r-a's `prim_fn` case.
        let ty = COUNTER_UI.replacen(
            "count: s32 = 0;",
            "count: s32 = 0;\n    on-change: fu§nc(value: s32);",
            1,
        );
        let value = hover_value("func-type", &ty).expect("hover");
        assert!(value.contains("function *type*"), "got: {value}");

        // A global callback with a body — the declaration.
        let decl = COUNTER.replacen("bump: func", "bump: fu§nc", 1);
        let value = hover_value("func-decl", &decl).expect("hover");
        assert!(value.contains("signature with a body"), "got: {value}");
    }

    #[test]
    fn in_hovers_as_binder_or_direction_by_parent() {
        let binder = COUNTER.replacen(
            "let amount = step;",
            "for x i§n [1, 2] { count = count + x; }",
            1,
        );
        let value = hover_value("in-binder", &binder).expect("hover");
        assert!(value.contains("separates the binder"), "got: {value}");

        let direction = COUNTER.replacen("count: s32 = 0;", "i§n count: s32 = 0;", 1);
        let value = hover_value("in-direction", &direction).expect("hover");
        assert!(value.contains("Property direction"), "got: {value}");
    }

    /// A `.yelir` dump is not part of any package (`collect` gathers `.yel`
    /// only), so definitions and usages cannot resolve — but keyword and
    /// literal hover are token facts and must still answer.
    #[test]
    fn yelir_dump_hovers_token_facts() {
        // The counter.yelir shape: constructs the compiler does not parse yet,
        // so the AST is partly Error nodes — the lexer does not care.
        let content = "package my:app;\n\nmodule Dom {\n    v§ariant color {\n        red,\n        rgba(tuple<u8, u8, u8, u8>),\n    }\n}\n";
        let value = hover_full_named("yelir-keyword", "main.yelir", content)
            .map(|hover| match hover.contents {
                HoverContents::Markup(markup) => markup.value,
                other => panic!("unexpected hover contents: {other:?}"),
            })
            .expect("hover");
        assert!(value.starts_with("```yel\nvariant\n```"), "got: {value}");

        let content = "module Dom {\n    func f() {\n        let x = 4§2;\n    }\n}\n";
        let value = hover_full_named("yelir-literal", "main.yelir", content)
            .map(|hover| match hover.contents {
                HoverContents::Markup(markup) => markup.value,
                other => panic!("unexpected hover contents: {other:?}"),
            })
            .expect("hover");
        assert!(
            value.contains("value of literal: 42 (0x2A|0b101010)"),
            "got: {value}"
        );

        // Names too: the standalone path lowers the file as its own package,
        // so a declaration recovery salvaged out of the unparsed `module`
        // wrapper is a real definition with a real hover.
        let content = "package my:app;\n\nmodule Dom {\n    variant col§or {\n        red,\n        rgba(tuple<u8, u8, u8, u8>),\n    }\n}\n";
        let value = hover_full_named("yelir-name", "main.yelir", content)
            .map(|hover| match hover.contents {
                HoverContents::Markup(markup) => markup.value,
                other => panic!("unexpected hover contents: {other:?}"),
            })
            .expect("hover");
        assert!(value.contains("color"), "got: {value}");
    }

    /// Modules bind a name (the first surface populator of the module arena),
    /// and `use` lists are name positions — both hover through root lookup.
    #[test]
    fn module_and_use_names_hover() {
        let content = "package my:app;\n\n@interface(name = \"a:b@0.1.0\")\nmodule D§om {\n    ping: func() -> s32 { 1 }\n}\n\nuse Dom.{ ping };\n";
        let value = hover_value("module-name", content).expect("hover");
        assert!(value.contains("module Dom"), "got: {value}");
        assert!(value.contains("@interface"), "got: {value}");

        let content = content
            .replace('§', "")
            .replacen("{ ping }", "{ pi§ng }", 1);
        let value = hover_value("use-name", &content).expect("hover");
        assert!(value.contains("ping: func() -> s32"), "got: {value}");
    }

    /// r-a's ranking, ported: at a boundary the token that *ends* at the cursor
    /// still wins over the trivia that starts there.
    #[test]
    fn keyword_hovers_at_its_trailing_boundary() {
        let content = COUNTER_UI.replacen("export component", "export§ component", 1);
        let value = hover_value("keyword-boundary", &content).expect("hover");
        assert!(value.starts_with("```yel\nexport\n```"), "got: {value}");
    }
}
