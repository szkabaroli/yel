//! Semantic token support for Yel LSP.
//!
//! Provides semantic highlighting for user-defined types (records, enums, variants).

use yel_core::syntax::ast::{Expr, File, Literal, Node, TyKind};
use yel_core::syntax::parser::parse_file;
use tower_lsp::lsp_types::*;

/// Semantic token types we support.
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::TYPE,
    SemanticTokenType::ENUM,
    SemanticTokenType::STRUCT,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::KEYWORD,
];

/// Semantic token modifiers we support.
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::DEFINITION,
];

/// Get the semantic token legend.
pub fn get_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

/// Token type indices.
const TYPE_INDEX: u32 = 0;
const ENUM_INDEX: u32 = 1;
const STRUCT_INDEX: u32 = 2;
const ENUM_MEMBER_INDEX: u32 = 3;
const _VARIABLE_INDEX: u32 = 4;
const _PROPERTY_INDEX: u32 = 5;
const _KEYWORD_INDEX: u32 = 6;

/// Modifier indices.
const DECLARATION_MODIFIER: u32 = 1 << 0;
const _DEFINITION_MODIFIER: u32 = 1 << 1;

/// A semantic token with position and type info.
#[derive(Debug, Clone)]
struct Token {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
    modifiers: u32,
}

/// Provide semantic tokens for a document.
pub fn provide_semantic_tokens(content: &str) -> Option<SemanticTokensResult> {
    let parse_result = parse_file(content).ok()?;
    let file = &parse_result.file;
    let mut tokens = Vec::new();

    // Collect user-defined type names
    let record_names: Vec<&str> = file.records.iter().map(|r| r.node.name.as_str()).collect();
    let enum_names: Vec<&str> = file.enums.iter().map(|e| e.node.name.as_str()).collect();
    let variant_names: Vec<&str> = file.variants.iter().map(|v| v.node.name.as_str()).collect();

    // Collect tokens from the AST
    collect_tokens_from_file(file, content, &record_names, &enum_names, &variant_names, &mut tokens);

    // Sort tokens by position
    tokens.sort_by(|a, b| {
        a.line.cmp(&b.line).then(a.start_char.cmp(&b.start_char))
    });

    // Convert to LSP format (delta encoding)
    let mut data = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_char = 0u32;

    for token in tokens {
        let delta_line = token.line - prev_line;
        let delta_char = if delta_line == 0 {
            token.start_char - prev_char
        } else {
            token.start_char
        };

        data.push(SemanticToken {
            delta_line,
            delta_start: delta_char,
            length: token.length,
            token_type: token.token_type,
            token_modifiers_bitset: token.modifiers,
        });

        prev_line = token.line;
        prev_char = token.start_char;
    }

    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    }))
}

fn collect_tokens_from_file(
    file: &File,
    content: &str,
    record_names: &[&str],
    enum_names: &[&str],
    variant_names: &[&str],
    tokens: &mut Vec<Token>,
) {
    // Token for record declarations
    for record in &file.records {
        let (line, col) = offset_to_line_col(content, record.node.name_span.start);
        tokens.push(Token {
            line,
            start_char: col,
            length: record.node.name.len() as u32,
            token_type: STRUCT_INDEX,
            modifiers: DECLARATION_MODIFIER,
        });

        // Token for field types
        for field in &record.node.fields {
            collect_tokens_from_type(&field.node.ty.kind, field.node.ty.span.start, content, record_names, enum_names, variant_names, tokens);
        }
    }

    // Token for enum declarations
    for enum_def in &file.enums {
        let (line, col) = offset_to_line_col(content, enum_def.node.name_span.start);
        tokens.push(Token {
            line,
            start_char: col,
            length: enum_def.node.name.len() as u32,
            token_type: ENUM_INDEX,
            modifiers: DECLARATION_MODIFIER,
        });

        // Token for enum cases
        for case in &enum_def.node.cases {
            let (line, col) = offset_to_line_col(content, case.span.start);
            tokens.push(Token {
                line,
                start_char: col,
                length: case.node.len() as u32,
                token_type: ENUM_MEMBER_INDEX,
                modifiers: 0,
            });
        }
    }

    // Token for variant declarations
    for variant in &file.variants {
        let (line, col) = offset_to_line_col(content, variant.node.name_span.start);
        tokens.push(Token {
            line,
            start_char: col,
            length: variant.node.name.len() as u32,
            token_type: ENUM_INDEX,
            modifiers: DECLARATION_MODIFIER,
        });

        // Token for variant cases
        for case in &variant.node.cases {
            let (line, col) = offset_to_line_col(content, case.span.start);
            tokens.push(Token {
                line,
                start_char: col,
                length: case.node.name.len() as u32,
                token_type: ENUM_MEMBER_INDEX,
                modifiers: 0,
            });

            // Token for payload type
            if let Some(payload) = &case.node.payload {
                collect_tokens_from_type(&payload.kind, payload.span.start, content, record_names, enum_names, variant_names, tokens);
            }
        }
    }

    // Token for components
    for component in &file.components {
        // Property types
        for prop in &component.node.properties {
            collect_tokens_from_type(&prop.node.ty.kind, prop.node.ty.span.start, content, record_names, enum_names, variant_names, tokens);

            // Check default value for enum cases
            if let Some(default) = &prop.node.default {
                collect_tokens_from_expr(&default.node, default.span.start, content, record_names, enum_names, variant_names, tokens);
            }
        }

        // Body nodes
        for node in &component.node.body {
            collect_tokens_from_node(&node.node, content, record_names, enum_names, variant_names, tokens);
        }
    }
}

fn collect_tokens_from_type(
    ty: &TyKind,
    offset: usize,
    content: &str,
    record_names: &[&str],
    enum_names: &[&str],
    variant_names: &[&str],
    tokens: &mut Vec<Token>,
) {
    match ty {
        TyKind::Named(name) => {
            let (line, col) = offset_to_line_col(content, offset);
            let token_type = if record_names.contains(&name.as_str()) {
                STRUCT_INDEX
            } else if enum_names.contains(&name.as_str()) || variant_names.contains(&name.as_str()) {
                ENUM_INDEX
            } else {
                TYPE_INDEX
            };
            tokens.push(Token {
                line,
                start_char: col,
                length: name.len() as u32,
                token_type,
                modifiers: 0,
            });
        }
        TyKind::List(inner) => {
            collect_tokens_from_type(&inner.kind, inner.span.start, content, record_names, enum_names, variant_names, tokens);
        }
        TyKind::Option(inner) => {
            collect_tokens_from_type(&inner.kind, inner.span.start, content, record_names, enum_names, variant_names, tokens);
        }
        TyKind::Result { ok, err } => {
            if let Some(ok_ty) = ok {
                collect_tokens_from_type(&ok_ty.kind, ok_ty.span.start, content, record_names, enum_names, variant_names, tokens);
            }
            if let Some(err_ty) = err {
                collect_tokens_from_type(&err_ty.kind, err_ty.span.start, content, record_names, enum_names, variant_names, tokens);
            }
        }
        TyKind::Tuple(types) => {
            for t in types {
                collect_tokens_from_type(&t.kind, t.span.start, content, record_names, enum_names, variant_names, tokens);
            }
        }
        _ => {}
    }
}

fn collect_tokens_from_expr(
    expr: &Expr,
    _offset: usize,
    content: &str,
    record_names: &[&str],
    enum_names: &[&str],
    variant_names: &[&str],
    tokens: &mut Vec<Token>,
) {
    match expr {
        Expr::Member(obj, _field) => {
            // Check for EnumName.case pattern
            if let Expr::Ident(name) = &obj.node {
                if enum_names.contains(&name.as_str()) || variant_names.contains(&name.as_str()) {
                    let (line, col) = offset_to_line_col(content, obj.span.start);
                    tokens.push(Token {
                        line,
                        start_char: col,
                        length: name.len() as u32,
                        token_type: ENUM_INDEX,
                        modifiers: 0,
                    });
                }
            }
        }
        Expr::Literal(lit) => {
            match lit {
                Literal::Record { fields } => {
                    // Anonymous record literal - just process field values
                    for (_, value) in fields {
                        collect_tokens_from_expr(&value.node, value.span.start, content, record_names, enum_names, variant_names, tokens);
                    }
                }
                Literal::List(items) => {
                    // Process each item in the list
                    for item in items {
                        collect_tokens_from_expr(&item.node, item.span.start, content, record_names, enum_names, variant_names, tokens);
                    }
                }
                _ => {}
            }
        }
        Expr::Binary(left, _, right) => {
            collect_tokens_from_expr(&left.node, left.span.start, content, record_names, enum_names, variant_names, tokens);
            collect_tokens_from_expr(&right.node, right.span.start, content, record_names, enum_names, variant_names, tokens);
        }
        Expr::Unary(_, inner) => {
            collect_tokens_from_expr(&inner.node, inner.span.start, content, record_names, enum_names, variant_names, tokens);
        }
        Expr::Ternary { condition, then_expr, else_expr } => {
            collect_tokens_from_expr(&condition.node, condition.span.start, content, record_names, enum_names, variant_names, tokens);
            collect_tokens_from_expr(&then_expr.node, then_expr.span.start, content, record_names, enum_names, variant_names, tokens);
            collect_tokens_from_expr(&else_expr.node, else_expr.span.start, content, record_names, enum_names, variant_names, tokens);
        }
        Expr::Call(_, args) => {
            for arg in args {
                collect_tokens_from_expr(&arg.node, arg.span.start, content, record_names, enum_names, variant_names, tokens);
            }
        }
        _ => {}
    }
}

fn collect_tokens_from_node(
    node: &Node,
    content: &str,
    record_names: &[&str],
    enum_names: &[&str],
    variant_names: &[&str],
    tokens: &mut Vec<Token>,
) {
    match node {
        Node::Element(el) => {
            // Check bindings for type references
            for binding in &el.bindings {
                collect_tokens_from_expr(&binding.node.value.node, binding.node.value.span.start, content, record_names, enum_names, variant_names, tokens);
            }

            // Check handlers
            for handler in &el.handlers {
                for stmt in &handler.node.body {
                    if let yel_core::syntax::ast::Statement::Expr(expr) = &stmt.node {
                        collect_tokens_from_expr(&expr.node, expr.span.start, content, record_names, enum_names, variant_names, tokens);
                    }
                }
            }

            // Recurse into children
            for child in &el.children {
                collect_tokens_from_node(&child.node, content, record_names, enum_names, variant_names, tokens);
            }
        }
        Node::If(if_node) => {
            collect_tokens_from_expr(&if_node.condition.node, if_node.condition.span.start, content, record_names, enum_names, variant_names, tokens);
            for child in &if_node.then_branch {
                collect_tokens_from_node(&child.node, content, record_names, enum_names, variant_names, tokens);
            }
            for (cond, branch) in &if_node.else_if_branches {
                collect_tokens_from_expr(&cond.node, cond.span.start, content, record_names, enum_names, variant_names, tokens);
                for child in branch {
                    collect_tokens_from_node(&child.node, content, record_names, enum_names, variant_names, tokens);
                }
            }
            if let Some(else_branch) = &if_node.else_branch {
                for child in else_branch {
                    collect_tokens_from_node(&child.node, content, record_names, enum_names, variant_names, tokens);
                }
            }
        }
        Node::For(for_node) => {
            collect_tokens_from_expr(&for_node.iterable.node, for_node.iterable.span.start, content, record_names, enum_names, variant_names, tokens);
            for child in &for_node.body {
                collect_tokens_from_node(&child.node, content, record_names, enum_names, variant_names, tokens);
            }
        }
        Node::Text(_) => {}
    }
}

/// Convert byte offset to line and column (0-indexed).
fn offset_to_line_col(content: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;

    for (i, ch) in content.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    (line, col)
}
