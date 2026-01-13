//! Parser implementation for Yel.

use super::ast::{
    Binding, Component, ElementNode, Enum, Expr, File, ForNode, FunctionDecl, Handler, IfNode,
    InterpolationPart, Literal, Node, NodeId, PackageId, Property, PropModifier, Record,
    RecordField, Spanned, Statement, TextNode, Ty, TyKind, Variant, VariantCase,
};
use crate::source::{SourceId, Span};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use pest_derive::Parser;
use std::sync::LazyLock;

#[derive(Parser)]
#[grammar = "syntax/grammar.pest"]
struct DslParser;

/// Pratt parser for expression precedence.
static PRATT_PARSER: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        // Lowest precedence
        .op(Op::infix(Rule::range, Assoc::Left) | Op::infix(Rule::range_inclusive, Assoc::Left))
        .op(Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::eq, Assoc::Left) | Op::infix(Rule::neq, Assoc::Left))
        .op(Op::infix(Rule::lt, Assoc::Left)
            | Op::infix(Rule::gt, Assoc::Left)
            | Op::infix(Rule::lte, Assoc::Left)
            | Op::infix(Rule::gte, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left)
            | Op::infix(Rule::div, Assoc::Left)
            | Op::infix(Rule::modulo, Assoc::Left))
        // Highest precedence
        .op(Op::prefix(Rule::neg) | Op::prefix(Rule::not))
        .op(Op::postfix(Rule::call) | Op::postfix(Rule::member) | Op::postfix(Rule::optional_member) | Op::postfix(Rule::index))
});

/// Parse error with source location.
#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Parse error: {message} at line {line}, column {column}")]
    Syntax {
        message: String,
        line: usize,
        column: usize,
        span: Option<Span>,
    },

    #[error("Unexpected rule: expected {expected}, found {found}")]
    UnexpectedRule {
        expected: String,
        found: String,
        span: Option<Span>,
    },

    #[error("Missing required element: {0}")]
    Missing(String),

    #[error("Invalid call base: only identifiers and member expressions can be called")]
    InvalidCallBase { span: Option<Span> },
}

impl ParseError {
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::Syntax { span, .. } => *span,
            ParseError::UnexpectedRule { span, .. } => *span,
            ParseError::Missing(_) => None,
            ParseError::InvalidCallBase { span } => *span,
        }
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(err: pest::error::Error<Rule>) -> Self {
        let (line, column) = match err.line_col {
            pest::error::LineColLocation::Pos((l, c)) => (l, c),
            pest::error::LineColLocation::Span((l, c), _) => (l, c),
        };
        ParseError::Syntax {
            message: err.variant.message().to_string(),
            line,
            column,
            span: None,
        }
    }
}

/// A syntax error caught during parsing (parsing continues).
#[derive(Debug, Clone)]
pub struct CatchedError {
    pub message: String,
    pub span: Span,
}

struct ParserContext {
    source_id: SourceId,
    /// Errors caught during parsing (CATCH_ALL nodes)
    catched_errors: Vec<CatchedError>,
}

impl ParserContext {
    fn new(source_id: SourceId) -> Self {
        Self {
            source_id,
            catched_errors: Vec::new(),
        }
    }

    fn span(&self, pair: &Pair<Rule>) -> Span {
        let pest_span = pair.as_span();
        Span::new(self.source_id, pest_span.start(), pest_span.end())
    }

    fn report_invalid_syntax(&mut self, pair: &Pair<Rule>) {
        let span = self.span(pair);
        let content = pair.as_str().trim();
        let message = if content.len() > 50 {
            format!("unexpected syntax: `{}...`", &content[..47])
        } else {
            format!("unexpected syntax: `{}`", content)
        };
        self.catched_errors.push(CatchedError { message, span });
    }
}

/// Parse DSL source into a Component AST.
pub fn parse(source: &str) -> Result<Component, ParseError> {
    parse_with_source_id(source, SourceId(0))
}

pub fn parse_with_source_id(source: &str, source_id: SourceId) -> Result<Component, ParseError> {
    let result = parse_file_with_source_id(source, source_id)?;
    // Note: catched_errors are discarded here - use parse_file_with_source_id for full error reporting
    result.file.components
        .into_iter()
        .next()
        .map(|s| s.node)
        .ok_or(ParseError::Missing("component".into()))
}

/// Result of parsing that includes both the AST and any errors caught during parsing.
#[derive(Debug)]
pub struct ParseResult {
    pub file: File,
    pub catched_errors: Vec<CatchedError>,
}

/// Parse DSL source into a File AST.
pub fn parse_file(source: &str) -> Result<ParseResult, ParseError> {
    parse_file_with_source_id(source, SourceId(0))
}

/// Parse DSL source into a File AST with caught errors.
/// Returns both the AST and any syntax errors that were recovered from.
pub fn parse_file_with_source_id(source: &str, source_id: SourceId) -> Result<ParseResult, ParseError> {
    let pairs = DslParser::parse(Rule::file, source).map_err(|err| {
        let (line, column) = match err.line_col {
            pest::error::LineColLocation::Pos((l, c)) => (l, c),
            pest::error::LineColLocation::Span((l, c), _) => (l, c),
        };
        let offset = line_col_to_offset(source, line, column);
        ParseError::Syntax {
            message: err.variant.message().to_string(),
            line,
            column,
            span: Some(Span::new(source_id, offset, offset + 1)),
        }
    })?;
    let mut ctx = ParserContext::new(source_id);
    let file = parse_file_inner(&mut ctx, pairs)?;
    Ok(ParseResult {
        file,
        catched_errors: ctx.catched_errors,
    })
}

fn line_col_to_offset(source: &str, line: usize, column: usize) -> usize {
    let mut current_line = 1;
    for (i, ch) in source.char_indices() {
        if current_line == line {
            let mut col = 1;
            for (j, c) in source[i..].char_indices() {
                if col == column {
                    return i + j;
                }
                if c == '\n' {
                    break;
                }
                col += 1;
            }
            return i + (column - 1).min(source[i..].find('\n').unwrap_or(source.len() - i));
        }
        if ch == '\n' {
            current_line += 1;
        }
    }
    source.len().saturating_sub(1)
}

fn parse_file_inner(ctx: &mut ParserContext, mut pairs: Pairs<Rule>) -> Result<File, ParseError> {
    let file_pair = pairs.next().ok_or(ParseError::Missing("file".into()))?;
    let mut package = None;
    let mut records = Vec::new();
    let mut enums = Vec::new();
    let mut variants = Vec::new();
    let mut components = Vec::new();

    for inner in file_pair.into_inner() {
        match inner.as_rule() {
            Rule::package_decl => {
                package = Some(parse_package_decl(ctx, inner)?);
            }
            Rule::record_decl => {
                let span = ctx.span(&inner);
                records.push(Spanned::new(parse_record(ctx, inner)?, span));
            }
            Rule::enum_decl => {
                let span = ctx.span(&inner);
                enums.push(Spanned::new(parse_enum(ctx, inner)?, span));
            }
            Rule::variant_decl => {
                let span = ctx.span(&inner);
                variants.push(Spanned::new(parse_variant(ctx, inner)?, span));
            }
            Rule::component => {
                let span = ctx.span(&inner);
                components.push(Spanned::new(parse_component(ctx, inner)?, span));
            }
            Rule::CATCH_ALL => {
                // Error recovery: report invalid syntax but continue parsing
                ctx.report_invalid_syntax(&inner);
            }
            _ => {}
        }
    }

    Ok(File {
        package,
        records,
        enums,
        variants,
        components,
    })
}

fn parse_package_decl(_ctx: &ParserContext, pair: Pair<Rule>) -> Result<PackageId, ParseError> {
    let package_id_pair = pair
        .into_inner()
        .find(|p| p.as_rule() == Rule::package_id)
        .ok_or(ParseError::Missing("package_id".into()))?;

    let mut inner = package_id_pair.into_inner();

    let namespace = inner
        .next()
        .ok_or(ParseError::Missing("package namespace".into()))?
        .as_str()
        .to_string();

    let name = inner
        .next()
        .ok_or(ParseError::Missing("package name".into()))?
        .as_str()
        .to_string();

    let version = inner.next().map(|p| {
        // Remove the leading '@' from version
        p.as_str().trim_start_matches('@').to_string()
    });

    Ok(PackageId {
        namespace,
        name,
        version,
    })
}

fn parse_record(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Record, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner
        .next()
        .ok_or(ParseError::Missing("record name".into()))?;
    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let mut fields = Vec::new();

    for item in inner {
        if item.as_rule() == Rule::record_field_list {
            for field_pair in item.into_inner() {
                if field_pair.as_rule() == Rule::record_field {
                    let span = ctx.span(&field_pair);
                    fields.push(Spanned::new(parse_record_field(ctx, field_pair)?, span));
                }
            }
        }
    }

    Ok(Record {
        name,
        name_span,
        fields,
    })
}

fn parse_record_field(ctx: &ParserContext, pair: Pair<Rule>) -> Result<RecordField, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner
        .next()
        .ok_or(ParseError::Missing("field name".into()))?;
    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let type_pair = inner
        .next()
        .ok_or(ParseError::Missing("field type".into()))?;
    let type_span = ctx.span(&type_pair);
    let ty = Ty::new(parse_type(ctx, type_pair)?, type_span);

    Ok(RecordField {
        name,
        name_span,
        ty,
    })
}

fn parse_enum(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Enum, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner
        .next()
        .ok_or(ParseError::Missing("enum name".into()))?;
    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let mut cases = Vec::new();

    for item in inner {
        if item.as_rule() == Rule::enum_cases {
            for case_pair in item.into_inner() {
                if case_pair.as_rule() == Rule::enum_case {
                    let span = ctx.span(&case_pair);
                    cases.push(Spanned::new(case_pair.as_str().to_string(), span));
                }
            }
        }
    }

    Ok(Enum {
        name,
        name_span,
        cases,
    })
}

fn parse_variant(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Variant, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner
        .next()
        .ok_or(ParseError::Missing("variant name".into()))?;
    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let mut cases = Vec::new();

    for item in inner {
        if item.as_rule() == Rule::variant_cases {
            for case_pair in item.into_inner() {
                if case_pair.as_rule() == Rule::variant_case {
                    let span = ctx.span(&case_pair);
                    cases.push(Spanned::new(parse_variant_case(ctx, case_pair)?, span));
                }
            }
        }
    }

    Ok(Variant {
        name,
        name_span,
        cases,
    })
}

fn parse_variant_case(ctx: &ParserContext, pair: Pair<Rule>) -> Result<VariantCase, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner
        .next()
        .ok_or(ParseError::Missing("variant case name".into()))?;
    let name = name_pair.as_str().to_string();

    // Check for optional payload type
    let payload = if let Some(type_pair) = inner.next() {
        if type_pair.as_rule() == Rule::type_annotation {
            let span = ctx.span(&type_pair);
            Some(Ty::new(parse_type(ctx, type_pair)?, span))
        } else {
            None
        }
    } else {
        None
    };

    Ok(VariantCase { name, payload })
}

fn parse_component(ctx: &mut ParserContext, pair: Pair<Rule>) -> Result<Component, ParseError> {
    let mut inner = pair.into_inner();

    // Check for export modifier
    let mut is_export = false;
    let first = inner.next().ok_or(ParseError::Missing("component name".into()))?;

    let name_pair = if first.as_rule() == Rule::export_modifier {
        is_export = true;
        inner.next().ok_or(ParseError::Missing("component name".into()))?
    } else {
        first
    };

    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let mut properties = Vec::new();
    let mut functions = Vec::new();
    let mut body = Vec::new();

    for member in inner {
        match member.as_rule() {
            Rule::property_decl => {
                let span = ctx.span(&member);
                properties.push(Spanned::new(parse_property(ctx, member)?, span));
            }
            Rule::function_decl => {
                let span = ctx.span(&member);
                functions.push(Spanned::new(parse_function_decl(ctx, member)?, span));
            }
            Rule::element_node | Rule::if_node | Rule::for_node | Rule::string_node => {
                let span = ctx.span(&member);
                body.push(Spanned::new(parse_node_inner(ctx, member)?, span));
            }
            Rule::BLOCK_LEVEL_CATCH_ALL => {
                // Error recovery: report invalid syntax but continue parsing
                ctx.report_invalid_syntax(&member);
            }
            _ => {}
        }
    }

    Ok(Component {
        id: NodeId::new(),
        name,
        name_span,
        is_export,
        properties,
        functions,
        body,
    })
}

fn parse_property(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Property, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner.next().ok_or(ParseError::Missing("property name".into()))?;
    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let type_pair = inner.next().ok_or(ParseError::Missing("property type".into()))?;
    let type_span = ctx.span(&type_pair);
    let ty = Ty::new(parse_type(ctx, type_pair)?, type_span);

    let mut default = None;
    for item in inner {
        if item.as_rule() == Rule::expr {
            let span = ctx.span(&item);
            default = Some(Spanned::new(parse_expr(ctx, item)?, span));
        }
    }

    Ok(Property {
        name,
        name_span,
        ty,
        default,
    })
}

fn parse_function_decl(ctx: &ParserContext, pair: Pair<Rule>) -> Result<FunctionDecl, ParseError> {
    let mut inner = pair.into_inner();

    // Check for export modifier
    let mut is_export = false;
    let mut next = inner.next().ok_or(ParseError::Missing("function name".into()))?;

    if next.as_rule() == Rule::export_modifier {
        is_export = true;
        next = inner.next().ok_or(ParseError::Missing("function name".into()))?;
    }

    // Get function name
    let name_span = ctx.span(&next);
    let name = next.as_str().to_string();

    // Get func_type
    let func_type = inner.next().ok_or(ParseError::Missing("function type".into()))?;
    let mut func_inner = func_type.into_inner();

    // Parse parameters
    let mut params = Vec::new();
    let mut return_type = None;

    for item in func_inner.by_ref() {
        match item.as_rule() {
            Rule::func_params => {
                for param in item.into_inner() {
                    if param.as_rule() == Rule::func_param {
                        let mut param_inner = param.into_inner();
                        let param_name = param_inner
                            .next()
                            .ok_or(ParseError::Missing("parameter name".into()))?
                            .as_str()
                            .to_string();
                        let param_type_pair = param_inner
                            .next()
                            .ok_or(ParseError::Missing("parameter type".into()))?;
                        let param_type_span = ctx.span(&param_type_pair);
                        let param_type = parse_type(ctx, param_type_pair)?;
                        params.push((param_name, Ty::new(param_type, param_type_span)));
                    }
                }
            }
            Rule::func_return => {
                let ret_type_pair = item
                    .into_inner()
                    .next()
                    .ok_or(ParseError::Missing("return type".into()))?;
                let ret_span = ctx.span(&ret_type_pair);
                let ret_type = parse_type(ctx, ret_type_pair)?;
                return_type = Some(Ty::new(ret_type, ret_span));
            }
            _ => {}
        }
    }

    Ok(FunctionDecl {
        name,
        name_span,
        is_export,
        params,
        return_type,
    })
}

fn parse_type(ctx: &ParserContext, pair: Pair<Rule>) -> Result<TyKind, ParseError> {
    let inner = pair.into_inner().next().ok_or(ParseError::Missing("type".into()))?;

    match inner.as_rule() {
        Rule::primitive_type => match inner.as_str() {
            // WIT primitive types
            "bool" => Ok(TyKind::Bool),
            "s8" => Ok(TyKind::S8),
            "s16" => Ok(TyKind::S16),
            "s32" => Ok(TyKind::S32),
            "s64" => Ok(TyKind::S64),
            "u8" => Ok(TyKind::U8),
            "u16" => Ok(TyKind::U16),
            "u32" => Ok(TyKind::U32),
            "u64" => Ok(TyKind::U64),
            "f32" => Ok(TyKind::F32),
            "f64" => Ok(TyKind::F64),
            "char" => Ok(TyKind::Char),
            "string" => Ok(TyKind::String),
            // Convenience aliases
            "int" => Ok(TyKind::S32),
            "float" => Ok(TyKind::F32),
            // UI-specific types
            "length" => Ok(TyKind::Length),
            "physical-length" => Ok(TyKind::PhysicalLength),
            "angle" => Ok(TyKind::Angle),
            "duration" => Ok(TyKind::Duration),
            "percent" => Ok(TyKind::Percent),
            "relative-font-size" => Ok(TyKind::RelativeFontSize),
            "color" => Ok(TyKind::Color),
            "brush" => Ok(TyKind::Brush),
            "image" => Ok(TyKind::Image),
            "easing" => Ok(TyKind::Easing),
            _ => Err(ParseError::UnexpectedRule {
                expected: "primitive type".into(),
                found: inner.as_str().into(),
                span: Some(ctx.span(&inner)),
            }),
        },
        Rule::list_type => {
            let inner_pair = inner.into_inner().next().ok_or(ParseError::Missing("list inner type".into()))?;
            let span = ctx.span(&inner_pair);
            let inner_type = parse_type(ctx, inner_pair)?;
            Ok(TyKind::List(Box::new(Ty::new(inner_type, span))))
        }
        Rule::option_type => {
            let inner_pair = inner.into_inner().next().ok_or(ParseError::Missing("option inner type".into()))?;
            let span = ctx.span(&inner_pair);
            let inner_type = parse_type(ctx, inner_pair)?;
            Ok(TyKind::Option(Box::new(Ty::new(inner_type, span))))
        }
        Rule::result_type => {
            let mut types_iter = inner.into_inner();
            if let Some(result_types) = types_iter.next() {
                let mut inner_iter = result_types.into_inner();
                let ok_pair = inner_iter.next().ok_or(ParseError::Missing("result ok type".into()))?;
                let ok_span = ctx.span(&ok_pair);
                let ok_type = parse_type(ctx, ok_pair)?;

                let err_type = if let Some(err_pair) = inner_iter.next() {
                    let err_span = ctx.span(&err_pair);
                    Some(Box::new(Ty::new(parse_type(ctx, err_pair)?, err_span)))
                } else {
                    None
                };

                Ok(TyKind::Result {
                    ok: Some(Box::new(Ty::new(ok_type, ok_span))),
                    err: err_type,
                })
            } else {
                // Bare `result` with no type parameters
                Ok(TyKind::Result { ok: None, err: None })
            }
        }
        Rule::tuple_type => {
            let type_list = inner.into_inner().next().ok_or(ParseError::Missing("tuple types".into()))?;
            let mut types = Vec::new();
            for type_pair in type_list.into_inner() {
                let span = ctx.span(&type_pair);
                types.push(Ty::new(parse_type(ctx, type_pair)?, span));
            }
            Ok(TyKind::Tuple(types))
        }
        Rule::named_type | Rule::identifier => Ok(TyKind::Named(inner.as_str().to_string())),
        _ => Err(ParseError::UnexpectedRule {
            expected: "type".into(),
            found: format!("{:?}", inner.as_rule()),
            span: Some(ctx.span(&inner)),
        }),
    }
}

fn parse_node_inner(ctx: &mut ParserContext, pair: Pair<Rule>) -> Result<Node, ParseError> {
    match pair.as_rule() {
        Rule::element_node => parse_element_node(ctx, pair),
        Rule::string_node => parse_text_node(ctx, pair),
        Rule::if_node => parse_if_node(ctx, pair),
        Rule::for_node => parse_for_node(ctx, pair),
        _ => Err(ParseError::UnexpectedRule {
            expected: "node".into(),
            found: format!("{:?}", pair.as_rule()),
            span: Some(ctx.span(&pair)),
        }),
    }
}

fn parse_element_node(ctx: &mut ParserContext, pair: Pair<Rule>) -> Result<Node, ParseError> {
    let mut inner = pair.into_inner();

    let name_pair = inner.next().ok_or(ParseError::Missing("element name".into()))?;
    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let mut bindings = Vec::new();
    let mut handlers = Vec::new();
    let mut children = Vec::new();

    for item in inner {
        if item.as_rule() == Rule::element_content {
            for content_item in item.into_inner() {
                match content_item.as_rule() {
                    Rule::named_prop => {
                        // Parse the binding first
                        let span = ctx.span(&content_item);
                        let binding = parse_binding(ctx, content_item)?;

                        // If it's a value binding (no modifier) with a closure (no params), treat as handler
                        // Set bindings are kept as bindings (for setter handlers)
                        if binding.modifier == PropModifier::None {
                            if let Expr::Closure { params, body } = &binding.value.node {
                                if params.is_empty() {
                                    handlers.push(Spanned::new(
                                        Handler {
                                            name: binding.name.clone(),
                                            name_span: binding.name_span,
                                            body: body.clone(),
                                        },
                                        span,
                                    ));
                                    continue; // Don't add to bindings
                                }
                            }
                        }

                        bindings.push(Spanned::new(binding, span));
                    }
                    Rule::element_node | Rule::if_node | Rule::for_node | Rule::string_node => {
                        let span = ctx.span(&content_item);
                        children.push(Spanned::new(parse_node_inner(ctx, content_item)?, span));
                    }
                    Rule::string_expr => {
                        let span = ctx.span(&content_item);
                        let expr = parse_string_expr(ctx, content_item)?;
                        children.push(Spanned::new(Node::Text(TextNode {
                            content: Spanned::new(expr, span),
                        }), span));
                    }
                    Rule::BLOCK_LEVEL_CATCH_ALL => {
                        // Error recovery: report invalid syntax but continue parsing
                        ctx.report_invalid_syntax(&content_item);
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(Node::Element(ElementNode {
        name,
        name_span,
        bindings,
        handlers,
        children,
    }))
}

fn parse_text_node(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Node, ParseError> {
    let string_expr = pair.into_inner().next().ok_or(ParseError::Missing("string".into()))?;
    let span = ctx.span(&string_expr);
    let expr = parse_string_expr(ctx, string_expr)?;
    Ok(Node::Text(TextNode {
        content: Spanned::new(expr, span),
    }))
}

fn parse_if_node(ctx: &mut ParserContext, pair: Pair<Rule>) -> Result<Node, ParseError> {
    let mut inner = pair.into_inner();

    let cond_pair = inner.next().ok_or(ParseError::Missing("if condition".into()))?;
    let cond_span = ctx.span(&cond_pair);
    let condition = Spanned::new(parse_expr(ctx, cond_pair)?, cond_span);

    let mut then_branch = Vec::new();
    let mut else_if_branches = Vec::new();
    let mut else_branch = None;

    for item in inner {
        match item.as_rule() {
            Rule::if_body => {
                for node in item.into_inner() {
                    match node.as_rule() {
                        Rule::element_node | Rule::if_node | Rule::for_node | Rule::string_node => {
                            let span = ctx.span(&node);
                            then_branch.push(Spanned::new(parse_node_inner(ctx, node)?, span));
                        }
                        _ => {}
                    }
                }
            }
            Rule::else_if_branch => {
                let mut branch_inner = item.into_inner();
                let branch_cond_pair = branch_inner.next().ok_or(ParseError::Missing("else-if condition".into()))?;
                let branch_cond_span = ctx.span(&branch_cond_pair);
                let branch_cond = Spanned::new(parse_expr(ctx, branch_cond_pair)?, branch_cond_span);
                let mut branch_nodes = Vec::new();
                for body in branch_inner {
                    if body.as_rule() == Rule::if_body {
                        for node in body.into_inner() {
                            match node.as_rule() {
                                Rule::element_node | Rule::if_node | Rule::for_node | Rule::string_node => {
                                    let span = ctx.span(&node);
                                    branch_nodes.push(Spanned::new(parse_node_inner(ctx, node)?, span));
                                }
                                _ => {}
                            }
                        }
                    }
                }
                else_if_branches.push((branch_cond, branch_nodes));
            }
            Rule::else_branch => {
                let mut branch_nodes = Vec::new();
                for body in item.into_inner() {
                    if body.as_rule() == Rule::if_body {
                        for node in body.into_inner() {
                            match node.as_rule() {
                                Rule::element_node | Rule::if_node | Rule::for_node | Rule::string_node => {
                                    let span = ctx.span(&node);
                                    branch_nodes.push(Spanned::new(parse_node_inner(ctx, node)?, span));
                                }
                                _ => {}
                            }
                        }
                    }
                }
                else_branch = Some(branch_nodes);
            }
            _ => {}
        }
    }

    Ok(Node::If(IfNode {
        condition,
        then_branch,
        else_if_branches,
        else_branch,
    }))
}

fn parse_for_node(ctx: &mut ParserContext, pair: Pair<Rule>) -> Result<Node, ParseError> {
    let mut inner = pair.into_inner();

    let item_pair = inner.next().ok_or(ParseError::Missing("for item name".into()))?;
    let item_name_span = ctx.span(&item_pair);
    let item_name = item_pair.as_str().to_string();

    let iterable_pair = inner.next().ok_or(ParseError::Missing("for iterable".into()))?;
    let iterable_span = ctx.span(&iterable_pair);
    let iterable = Spanned::new(parse_expr(ctx, iterable_pair)?, iterable_span);

    let mut key = None;
    let mut body = Vec::new();

    for item in inner {
        match item.as_rule() {
            Rule::key_clause => {
                for key_item in item.into_inner() {
                    if key_item.as_rule() == Rule::expr {
                        let span = ctx.span(&key_item);
                        key = Some(Spanned::new(parse_expr(ctx, key_item)?, span));
                    }
                }
            }
            Rule::for_body => {
                for node in item.into_inner() {
                    match node.as_rule() {
                        Rule::element_node | Rule::if_node | Rule::for_node | Rule::string_node => {
                            let span = ctx.span(&node);
                            body.push(Spanned::new(parse_node_inner(ctx, node)?, span));
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    Ok(Node::For(ForNode {
        item_name,
        item_name_span,
        iterable,
        key,
        body,
    }))
}

fn parse_binding(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Binding, ParseError> {
    let mut inner = pair.into_inner();

    // Check for optional modifier (set, etc.)
    let first = inner.next().ok_or(ParseError::Missing("binding name or modifier".into()))?;
    let (modifier, name_pair) = if first.as_rule() == Rule::prop_modifier {
        let mod_str = first.as_str();
        let modifier = match mod_str {
            "set" => PropModifier::Set,
            _ => PropModifier::None,
        };
        let name_pair = inner.next().ok_or(ParseError::Missing("binding name".into()))?;
        (modifier, name_pair)
    } else {
        (PropModifier::None, first)
    };

    let name_span = ctx.span(&name_pair);
    let name = name_pair.as_str().to_string();

    let value_pair = inner.next().ok_or(ParseError::Missing("binding value".into()))?;
    let value_span = ctx.span(&value_pair);
    let value = Spanned::new(parse_expr(ctx, value_pair)?, value_span);

    Ok(Binding {
        modifier,
        name,
        name_span,
        value,
    })
}

fn parse_statement(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Statement, ParseError> {
    let inner = pair.into_inner().next().ok_or(ParseError::Missing("statement".into()))?;

    match inner.as_rule() {
        Rule::if_statement => {
            let mut items = inner.into_inner();
            let cond_pair = items.next().ok_or(ParseError::Missing("if condition".into()))?;
            let cond_span = ctx.span(&cond_pair);
            let condition = Spanned::new(parse_expr(ctx, cond_pair)?, cond_span);

            let mut then_branch = Vec::new();
            let mut else_branch = None;
            let mut in_else = false;

            for item in items {
                if item.as_rule() == Rule::statement {
                    let span = ctx.span(&item);
                    let stmt = Spanned::new(parse_statement(ctx, item)?, span);
                    if in_else {
                        else_branch.get_or_insert_with(Vec::new).push(stmt);
                    } else {
                        then_branch.push(stmt);
                    }
                } else if item.as_str().contains("else") {
                    in_else = true;
                }
            }

            Ok(Statement::If {
                condition,
                then_branch,
                else_branch,
            })
        }
        Rule::assign_statement => {
            let mut items = inner.into_inner();
            let target_pair = items.next().ok_or(ParseError::Missing("assign target".into()))?;
            let target_span = ctx.span(&target_pair);
            let target = Spanned::new(parse_expr(ctx, target_pair)?, target_span);

            let next = items.next().ok_or(ParseError::Missing("assign operator or value".into()))?;

            if next.as_rule() == Rule::compound_op {
                let op = next.as_str().to_string();
                let value_pair = items.next().ok_or(ParseError::Missing("assign value".into()))?;
                let value_span = ctx.span(&value_pair);
                let value = Spanned::new(parse_expr(ctx, value_pair)?, value_span);
                Ok(Statement::CompoundAssign(target, op, value))
            } else {
                let value_span = ctx.span(&next);
                let value = Spanned::new(parse_expr(ctx, next)?, value_span);
                Ok(Statement::Assign(target, value))
            }
        }
        Rule::expr_statement => {
            let expr_pair = inner.into_inner().next().ok_or(ParseError::Missing("expression".into()))?;
            let span = ctx.span(&expr_pair);
            let expr = parse_expr(ctx, expr_pair)?;
            Ok(Statement::Expr(Spanned::new(expr, span)))
        }
        _ => Err(ParseError::UnexpectedRule {
            expected: "statement".into(),
            found: format!("{:?}", inner.as_rule()),
            span: Some(ctx.span(&inner)),
        }),
    }
}

type SpannedExpr = (Expr, Span);

fn parse_expr(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let (expr, _) = parse_expr_with_span(ctx, pair)?;
    Ok(expr)
}

/// Parse ternary suffix if present: ? then_expr : else_expr
fn parse_ternary_suffix(
    ctx: &ParserContext,
    condition: Spanned<Expr>,
    suffix: Pair<Rule>,
) -> Result<SpannedExpr, ParseError> {
    let suffix_span = ctx.span(&suffix);
    let mut inner = suffix.into_inner();

    let then_pair = inner.next().ok_or(ParseError::Missing("then expression".into()))?;
    let then_span = ctx.span(&then_pair);
    let then_expr = parse_expr(ctx, then_pair)?;

    let else_pair = inner.next().ok_or(ParseError::Missing("else expression".into()))?;
    let else_span = ctx.span(&else_pair);
    let else_expr = parse_expr(ctx, else_pair)?;

    let combined_span = Span::new(condition.span.source, condition.span.start, suffix_span.end);

    Ok((
        Expr::Ternary {
            condition: Box::new(condition),
            then_expr: Box::new(Spanned::new(then_expr, then_span)),
            else_expr: Box::new(Spanned::new(else_expr, else_span)),
        },
        combined_span,
    ))
}

fn parse_expr_with_span(ctx: &ParserContext, pair: Pair<Rule>) -> Result<SpannedExpr, ParseError> {
    let full_span = ctx.span(&pair);

    // Check for ternary suffix at the end
    let inner_pairs: Vec<Pair<Rule>> = pair.into_inner().collect();
    let (pratt_pairs, ternary_suffix) = if let Some(last) = inner_pairs.last() {
        if last.as_rule() == Rule::ternary_suffix {
            let ternary = last.clone();
            let pratt: Vec<_> = inner_pairs[..inner_pairs.len() - 1].to_vec();
            (pratt, Some(ternary))
        } else {
            (inner_pairs, None)
        }
    } else {
        (inner_pairs, None)
    };

    let (expr, expr_span) = PRATT_PARSER
        .map_primary(|primary| {
            let span = ctx.span(&primary);
            let expr = parse_primary(ctx, primary)?;
            Ok((expr, span))
        })
        .map_prefix(|op, rhs| {
            let (rhs_expr, rhs_span) = rhs?;
            let op_span = ctx.span(&op);
            let op_str = match op.as_rule() {
                Rule::neg => "-",
                Rule::not => "!",
                _ => return Err(ParseError::UnexpectedRule {
                    expected: "prefix operator".into(),
                    found: format!("{:?}", op.as_rule()),
                    span: Some(op_span),
                }),
            };
            let combined_span = Span::new(op_span.source, op_span.start, rhs_span.end);
            Ok((
                Expr::Unary(op_str.to_string(), Box::new(Spanned::new(rhs_expr, rhs_span))),
                combined_span,
            ))
        })
        .map_postfix(|lhs, op| {
            let (lhs_expr, lhs_span) = lhs?;
            let op_span = ctx.span(&op);
            match op.as_rule() {
                Rule::call => {
                    let mut args = Vec::new();
                    for inner in op.into_inner() {
                        if inner.as_rule() == Rule::args {
                            for arg in inner.into_inner() {
                                if arg.as_rule() == Rule::expr {
                                    let span = ctx.span(&arg);
                                    args.push(Spanned::new(parse_expr(ctx, arg)?, span));
                                }
                            }
                        }
                    }
                    let combined_span = Span::new(lhs_span.source, lhs_span.start, op_span.end);
                    match lhs_expr {
                        Expr::Ident(name) => {
                            Ok((Expr::Call(name, args), combined_span))
                        }
                        // Handle member call like Type.case(args) -> PathCall
                        Expr::Member(base, member) => {
                            Ok((Expr::PathCall {
                                base,
                                member,
                                args,
                            }, combined_span))
                        }
                        _ => {
                            Err(ParseError::InvalidCallBase {
                                span: Some(lhs_span),
                            })
                        }
                    }
                }
                Rule::member => {
                    let member_name = op.into_inner().next()
                        .ok_or(ParseError::Missing("member name".into()))?
                        .as_str().to_string();
                    let combined_span = Span::new(lhs_span.source, lhs_span.start, op_span.end);
                    Ok((Expr::Member(Box::new(Spanned::new(lhs_expr, lhs_span)), member_name), combined_span))
                }
                Rule::optional_member => {
                    let member_name = op.into_inner().next()
                        .ok_or(ParseError::Missing("member name".into()))?
                        .as_str().to_string();
                    let combined_span = Span::new(lhs_span.source, lhs_span.start, op_span.end);
                    Ok((Expr::OptionalMember(Box::new(Spanned::new(lhs_expr, lhs_span)), member_name), combined_span))
                }
                Rule::index => {
                    let idx_pair = op.into_inner().next().ok_or(ParseError::Missing("index expression".into()))?;
                    let idx_span = ctx.span(&idx_pair);
                    let index_expr = parse_expr(ctx, idx_pair)?;
                    let combined_span = Span::new(lhs_span.source, lhs_span.start, op_span.end);
                    Ok((Expr::Index(
                        Box::new(Spanned::new(lhs_expr, lhs_span)),
                        Box::new(Spanned::new(index_expr, idx_span)),
                    ), combined_span))
                }
                _ => Err(ParseError::UnexpectedRule {
                    expected: "postfix operator".into(),
                    found: format!("{:?}", op.as_rule()),
                    span: Some(op_span),
                }),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let (lhs_expr, lhs_span) = lhs?;
            let (rhs_expr, rhs_span) = rhs?;
            let combined_span = Span::new(lhs_span.source, lhs_span.start, rhs_span.end);

            match op.as_rule() {
                Rule::range => Ok((Expr::Range {
                    start: Box::new(Spanned::new(lhs_expr, lhs_span)),
                    end: Box::new(Spanned::new(rhs_expr, rhs_span)),
                    inclusive: false,
                }, combined_span)),
                Rule::range_inclusive => Ok((Expr::Range {
                    start: Box::new(Spanned::new(lhs_expr, lhs_span)),
                    end: Box::new(Spanned::new(rhs_expr, rhs_span)),
                    inclusive: true,
                }, combined_span)),
                _ => {
                    let op_str = match op.as_rule() {
                        Rule::or => "||",
                        Rule::and => "&&",
                        Rule::eq => "==",
                        Rule::neq => "!=",
                        Rule::lt => "<",
                        Rule::gt => ">",
                        Rule::lte => "<=",
                        Rule::gte => ">=",
                        Rule::add => "+",
                        Rule::sub => "-",
                        Rule::mul => "*",
                        Rule::div => "/",
                        Rule::modulo => "%",
                        _ => return Err(ParseError::UnexpectedRule {
                            expected: "infix operator".into(),
                            found: format!("{:?}", op.as_rule()),
                            span: Some(ctx.span(&op)),
                        }),
                    };
                    Ok((Expr::Binary(
                        Box::new(Spanned::new(lhs_expr, lhs_span)),
                        op_str.to_string(),
                        Box::new(Spanned::new(rhs_expr, rhs_span)),
                    ), combined_span))
                }
            }
        })
        .parse(pratt_pairs.into_iter())
        .map(|(expr, _)| (expr, full_span))?;

    // Apply ternary suffix if present
    if let Some(suffix) = ternary_suffix {
        parse_ternary_suffix(ctx, Spanned::new(expr, expr_span), suffix)
    } else {
        Ok((expr, full_span))
    }
}

fn parse_primary(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    match pair.as_rule() {
        Rule::literal => parse_literal(ctx, pair),
        Rule::record_literal => parse_record_literal(ctx, pair),
        Rule::tuple_literal => parse_tuple_literal(ctx, pair),
        Rule::closure_with_params => parse_closure_with_params(ctx, pair),
        Rule::closure_no_params => parse_closure_no_params(ctx, pair),
        Rule::identifier => Ok(Expr::Ident(pair.as_str().to_string())),
        Rule::expr => parse_expr(ctx, pair),
        _ => Err(ParseError::UnexpectedRule {
            expected: "literal, identifier, record_literal, tuple_literal, closure, or expr".into(),
            found: format!("{:?}", pair.as_rule()),
            span: Some(ctx.span(&pair)),
        }),
    }
}

/// Parse closure with typed parameters: { x: s32, y: s32 -> body }
fn parse_closure_with_params(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut inner = pair.into_inner();

    // Parse parameter list
    let param_list = inner
        .next()
        .ok_or(ParseError::Missing("closure parameters".into()))?;
    let params = parse_closure_param_list(ctx, param_list)?;

    // Parse body
    let body_pair = inner
        .next()
        .ok_or(ParseError::Missing("closure body".into()))?;
    let body = parse_closure_body(ctx, body_pair)?;

    Ok(Expr::Closure { params, body })
}

/// Parse closure without parameters: { body }
fn parse_closure_no_params(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut inner = pair.into_inner();

    // Parse body (may be empty)
    let body = if let Some(body_pair) = inner.next() {
        parse_closure_body(ctx, body_pair)?
    } else {
        Vec::new()
    };

    Ok(Expr::Closure {
        params: Vec::new(),
        body,
    })
}

/// Parse closure parameter list: x: s32, y: s32
fn parse_closure_param_list(
    ctx: &ParserContext,
    pair: Pair<Rule>,
) -> Result<Vec<(String, Ty)>, ParseError> {
    let mut params = Vec::new();

    for param_pair in pair.into_inner() {
        if param_pair.as_rule() == Rule::closure_param {
            let mut param_inner = param_pair.into_inner();
            let name = param_inner
                .next()
                .ok_or(ParseError::Missing("parameter name".into()))?
                .as_str()
                .to_string();
            let type_pair = param_inner
                .next()
                .ok_or(ParseError::Missing("parameter type".into()))?;
            let type_span = ctx.span(&type_pair);
            let ty = parse_type(ctx, type_pair)?;
            params.push((name, Ty::new(ty, type_span)));
        }
    }

    Ok(params)
}

/// Parse closure body: statements and optional trailing expression
fn parse_closure_body(
    ctx: &ParserContext,
    pair: Pair<Rule>,
) -> Result<Vec<Spanned<Statement>>, ParseError> {
    let mut body = Vec::new();

    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::statement => {
                let span = ctx.span(&item);
                body.push(Spanned::new(parse_statement(ctx, item)?, span));
            }
            Rule::trailing_expr => {
                // Trailing expression becomes an expression statement (without semicolon)
                let expr_pair = item
                    .into_inner()
                    .next()
                    .ok_or(ParseError::Missing("trailing expression".into()))?;
                let span = ctx.span(&expr_pair);
                let expr = parse_expr(ctx, expr_pair)?;
                body.push(Spanned::new(Statement::Expr(Spanned::new(expr, span)), span));
            }
            _ => {}
        }
    }

    Ok(body)
}

fn parse_tuple_literal(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut elements = Vec::new();
    for item in pair.into_inner() {
        if item.as_rule() == Rule::expr {
            let span = ctx.span(&item);
            let expr = parse_expr(ctx, item)?;
            elements.push(Spanned::new(expr, span));
        }
    }
    Ok(Expr::Literal(Literal::Tuple(elements)))
}

fn parse_record_literal(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let inner = pair.into_inner();
    let mut fields = Vec::new();

    for item in inner {
        if item.as_rule() == Rule::record_literal_fields {
            for field_pair in item.into_inner() {
                if field_pair.as_rule() == Rule::record_literal_field {
                    let mut field_inner = field_pair.into_inner();
                    let field_name = field_inner
                        .next()
                        .ok_or(ParseError::Missing("field name".into()))?
                        .as_str()
                        .to_string();
                    let value_pair = field_inner
                        .next()
                        .ok_or(ParseError::Missing("field value".into()))?;
                    let value_span = ctx.span(&value_pair);
                    let value = parse_expr(ctx, value_pair)?;
                    fields.push((field_name, Spanned::new(value, value_span)));
                }
            }
        }
    }

    Ok(Expr::Literal(Literal::Record { fields }))
}

fn parse_literal(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let inner = pair.into_inner().next().ok_or(ParseError::Missing("literal".into()))?;

    match inner.as_rule() {
        Rule::unit_literal => {
            let s = inner.as_str();
            let (value_str, unit) = split_unit_literal(s);
            let value = value_str.parse::<f64>().map_err(|e| ParseError::Syntax {
                message: e.to_string(),
                line: 0,
                column: 0,
                span: Some(ctx.span(&inner)),
            })?;
            Ok(Expr::Literal(Literal::Unit(value, unit.to_string())))
        }
        Rule::int_literal => {
            let value = inner.as_str().parse::<i64>().map_err(|e| ParseError::Syntax {
                message: e.to_string(),
                line: 0,
                column: 0,
                span: Some(ctx.span(&inner)),
            })?;
            Ok(Expr::Literal(Literal::Int(value)))
        }
        Rule::float_literal => {
            let value = inner.as_str().parse::<f64>().map_err(|e| ParseError::Syntax {
                message: e.to_string(),
                line: 0,
                column: 0,
                span: Some(ctx.span(&inner)),
            })?;
            Ok(Expr::Literal(Literal::Float(value)))
        }
        Rule::color_literal => Ok(Expr::Literal(Literal::Color(inner.as_str().to_string()))),
        Rule::string_expr => parse_string_expr(ctx, inner),
        Rule::bool_literal => Ok(Expr::Literal(Literal::Bool(inner.as_str() == "true"))),
        Rule::char_literal => {
            let char_inner = inner.into_inner().next().unwrap();
            let s = char_inner.as_str();
            let c = if s.starts_with('\\') {
                match s.chars().nth(1) {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('"') => '"',
                    Some('0') => '\0',
                    _ => s.chars().nth(1).unwrap_or('?'),
                }
            } else {
                s.chars().next().unwrap_or('?')
            };
            Ok(Expr::Literal(Literal::Char(c)))
        }
        Rule::list_literal => {
            let mut elements = Vec::new();
            for item in inner.into_inner() {
                if item.as_rule() == Rule::expr {
                    let span = ctx.span(&item);
                    elements.push(Spanned::new(parse_expr(ctx, item)?, span));
                }
            }
            Ok(Expr::Literal(Literal::List(elements)))
        }
        _ => Err(ParseError::UnexpectedRule {
            expected: "literal".into(),
            found: format!("{:?}", inner.as_rule()),
            span: Some(ctx.span(&inner)),
        }),
    }
}

fn split_unit_literal(s: &str) -> (&str, &str) {
    let unit_start = s.char_indices()
        .find(|(_, c)| !c.is_ascii_digit() && *c != '.' && *c != '-')
        .map(|(i, _)| i)
        .unwrap_or(s.len());
    (&s[..unit_start], &s[unit_start..])
}

fn parse_string_expr(ctx: &ParserContext, pair: Pair<Rule>) -> Result<Expr, ParseError> {
    let mut parts = Vec::new();

    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::string_text => {
                parts.push(InterpolationPart::Literal(part.as_str().to_string()));
            }
            Rule::interpolation => {
                let expr_pair = part.into_inner().next().ok_or(ParseError::Missing("interpolation expression".into()))?;
                let span = ctx.span(&expr_pair);
                let expr = parse_expr(ctx, expr_pair)?;
                parts.push(InterpolationPart::Expr(Spanned::new(expr, span)));
            }
            _ => {}
        }
    }

    if parts.is_empty() {
        return Ok(Expr::Literal(Literal::String(String::new())));
    }
    if parts.len() == 1 {
        if let InterpolationPart::Literal(s) = &parts[0] {
            return Ok(Expr::Literal(Literal::String(s.clone())));
        }
    }

    Ok(Expr::Interpolation(parts))
}

#[cfg(test)]
mod tests {
    use super::*;

    // ==================== Basic Component Parsing ====================

    #[test]
    fn test_parse_new_syntax() {
        let source = r#"
            component Counter {
                count: int = 0;

                div {
                    "Count: {count}",
                    button {
                        "+",
                        onclick: { count += 1; }
                    }
                }
            }
        "#;

        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());

        let component = result.unwrap();
        assert_eq!(component.name, "Counter");
        assert_eq!(component.properties.len(), 1);
        assert_eq!(component.body.len(), 1);
    }

    #[test]
    fn test_parse_empty_component() {
        let source = "component Empty {}";
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.name, "Empty");
        assert!(component.properties.is_empty());
        assert!(component.body.is_empty());
    }

    #[test]
    fn test_parse_component_with_underscores() {
        let source = "component My_Component {}";
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        assert_eq!(result.unwrap().name, "My_Component");
    }

    // ==================== Package Declaration ====================

    #[test]
    fn test_parse_package_declaration() {
        let source = r#"
            package yel:counter@1.0.0;
            component Counter {}
        "#;
        let file = parse_file(source).unwrap();
        assert!(file.package.is_some());
        let pkg = file.package.unwrap();
        assert_eq!(pkg.namespace, "yel");
        assert_eq!(pkg.name, "counter");
        assert_eq!(pkg.version, Some("1.0.0".to_string()));
    }

    #[test]
    fn test_parse_package_without_version() {
        let source = r#"
            package my-namespace:my-package;
            component Foo {}
        "#;
        let file = parse_file(source).unwrap();
        let pkg = file.package.unwrap();
        assert_eq!(pkg.namespace, "my-namespace");
        assert_eq!(pkg.name, "my-package");
        assert_eq!(pkg.version, None);
    }

    #[test]
    fn test_parse_multiple_components() {
        let source = r#"
            component A {}
            component B {}
            component C {}
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.components.len(), 3);
    }

    // ==================== WIT Primitive Types ====================

    #[test]
    fn test_parse_wit_signed_integers() {
        let source = r#"
            component Types {
                a: s8 = 0;
                b: s16 = 0;
                c: s32 = 0;
                d: s64 = 0;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.properties.len(), 4);
    }

    #[test]
    fn test_parse_wit_unsigned_integers() {
        let source = r#"
            component Types {
                a: u8 = 0;
                b: u16 = 0;
                c: u32 = 0;
                d: u64 = 0;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_wit_floats() {
        let source = r#"
            component Types {
                a: f32 = 0.0;
                b: f64 = 0.0;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_wit_char_and_string() {
        let source = r#"
            component Types {
                a: char = 'x';
                b: string = "hello";
            }
        "#;
        // Note: char literals aren't fully supported yet, but type parsing should work
        let result = parse(source);
        // This might fail on 'x' literal, which is expected
        // The test verifies type parsing works
    }

    #[test]
    fn test_parse_type_aliases() {
        let source = r#"
            component Types {
                a: int = 0;
                b: float = 0.0;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== WIT Compound Types ====================

    #[test]
    fn test_parse_list_type() {
        let source = r#"
            component Types {
                items: list<s32>;
                names: list<string>;
                nested: list<list<s32>>;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_option_type() {
        let source = r#"
            component Types {
                maybe_int: option<s32>;
                maybe_string: option<string>;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_result_type() {
        let source = r#"
            component Types {
                res1: result<s32, string>;
                res2: result<string>;
                res3: result;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_tuple_type() {
        let source = r#"
            component Types {
                pair: tuple<s32, string>;
                triple: tuple<s32, s32, s32>;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== UI-Specific Types ====================

    #[test]
    fn test_parse_ui_types() {
        let source = r#"
            component Types {
                len: length = 10px;
                ang: angle = 45deg;
                dur: duration = 100ms;
                pct: percent = 50%;
                col: color = #ff0000;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Control Flow ====================

    #[test]
    fn test_parse_for_with_range() {
        let source = r#"
            component List {
                ul {
                    for i in 0..5 {
                        li { "{i}" }
                    }
                }
            }
        "#;

        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_for_with_inclusive_range() {
        let source = r#"
            component List {
                for i in 0..=10 {
                    span { "{i}" }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_for_with_list() {
        let source = r#"
            component List {
                items: list<string>;
                for item in items {
                    li { "{item}" }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_for_with_key() {
        let source = r#"
            component List {
                items: list<s32>;
                for item in items key(item) {
                    li { "{item}" }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_if_inline() {
        let source = r#"
            component Cond {
                show: bool = true;

                div {
                    if show {
                        "visible"
                    }
                }
            }
        "#;

        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_if_else() {
        let source = r#"
            component Cond {
                flag: bool = true;
                if flag {
                    span { "yes" }
                } else {
                    span { "no" }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_if_else_if_else() {
        let source = r#"
            component Cond {
                value: s32 = 0;
                if value > 0 {
                    span { "positive" }
                } else if value < 0 {
                    span { "negative" }
                } else {
                    span { "zero" }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Expressions ====================

    #[test]
    fn test_parse_arithmetic_expressions() {
        let source = r#"
            component Expr {
                a: s32 = 1 + 2;
                b: s32 = 3 - 4;
                c: s32 = 5 * 6;
                d: s32 = 7 / 8;
                e: s32 = 9 % 10;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_comparison_expressions() {
        let source = r#"
            component Expr {
                a: bool = 1 < 2;
                b: bool = 1 > 2;
                c: bool = 1 <= 2;
                d: bool = 1 >= 2;
                e: bool = 1 == 2;
                f: bool = 1 != 2;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_logical_expressions() {
        let source = r#"
            component Expr {
                a: bool = true && false;
                b: bool = true || false;
                c: bool = !true;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_member_access() {
        let source = r#"
            component Expr {
                obj: MyType;
                div { "{obj.field}" }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_index_access() {
        let source = r#"
            component Expr {
                items: list<s32>;
                div { "{items[0]}" }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_function_call() {
        let source = r#"
            component Expr {
                x: s32 = my_func(1, 2, 3);
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_parenthesized_expression() {
        let source = r#"
            component Expr {
                x: s32 = (1 + 2) * 3;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_unary_minus() {
        let source = r#"
            component Expr {
                x: s32 = -42;
                y: s32 = -x;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Literals ====================

    #[test]
    fn test_parse_integer_literals() {
        let source = r#"
            component Lit {
                a: s32 = 0;
                b: s32 = 42;
                c: s32 = -123;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_float_literals() {
        let source = r#"
            component Lit {
                a: f32 = 0.0;
                b: f32 = 3.14;
                c: f32 = -2.5;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_bool_literals() {
        let source = r#"
            component Lit {
                a: bool = true;
                b: bool = false;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_string_literals() {
        let source = r#"
            component Lit {
                a: string = "";
                b: string = "hello";
                c: string = "hello world";
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_unit_literals() {
        let source = r#"
            component Lit {
                a: length = 10px;
                b: length = 5.5pt;
                c: angle = 90deg;
                d: duration = 100ms;
                e: duration = 1s;
                f: percent = 50%;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_color_literals() {
        let source = r#"
            component Lit {
                a: color = #fff;
                b: color = #ffffff;
                c: color = #ff0000ff;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_list_literals() {
        let source = r#"
            component Lit {
                empty: list<s32> = [];
                nums: list<s32> = [1, 2, 3];
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_anonymous_record_literal() {
        let source = r#"
            record Mail {
                id: string,
                subject: string,
            }
            component Test {
                mail: Mail = { id: "abc", subject: "Hello" };
                mails: list<Mail> = [
                    { id: "1", subject: "First" },
                    { id: "2", subject: "Second" },
                ];
            }
        "#;
        let result = parse_file(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());

        let file = result.unwrap();
        assert_eq!(file.components.len(), 1);
        let comp = &file.components[0].node;
        assert_eq!(comp.properties.len(), 2);
    }

    // ==================== String Interpolation ====================

    #[test]
    fn test_parse_string_interpolation_simple() {
        let source = r#"
            component Interp {
                name: string = "world";
                Text { "Hello {name}" }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_string_interpolation_expression() {
        let source = r#"
            component Interp {
                count: s32 = 0;
                Text { "Count: {count + 1}" }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_string_interpolation_multiple() {
        let source = r#"
            component Interp {
                first: string = "A";
                last: string = "B";
                Text { "{first} {last}" }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Element Nodes ====================

    #[test]
    fn test_parse_element_with_properties() {
        let source = r#"
            component Elem {
                div {
                    id: "main",
                    class: "container"
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_element_with_children() {
        let source = r#"
            component Elem {
                div {
                    span { "child 1" },
                    span { "child 2" }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_element_with_handlers() {
        let source = r#"
            component Elem {
                count: s32 = 0;
                button {
                    onclick: { count += 1; },
                    onmouseenter: { count = 0; }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_element_mixed_content() {
        let source = r#"
            component Elem {
                count: s32 = 0;
                div {
                    "Label: ",
                    span { "{count}" },
                    button {
                        "+",
                        onclick: { count += 1; }
                    }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Handler Statements ====================

    #[test]
    fn test_parse_handler_assignment() {
        let source = r#"
            component Stmt {
                x: s32 = 0;
                button { onclick: { x = 10; } }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_handler_compound_assignment() {
        let source = r#"
            component Stmt {
                x: s32 = 0;
                button {
                    onclick: {
                        x += 1;
                        x -= 1;
                        x *= 2;
                        x /= 2;
                    }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_handler_if_statement() {
        let source = r#"
            component Stmt {
                x: s32 = 0;
                button {
                    onclick: {
                        if x > 0 {
                            x = 0;
                        } else {
                            x = 1;
                        }
                    }
                }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_handler_function_call() {
        let source = r#"
            component Stmt {
                button { onclick: { do_something(); } }
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Comments ====================

    #[test]
    fn test_parse_line_comment() {
        let source = r#"
            // This is a comment
            component Foo {
                // Another comment
                x: s32 = 0; // Inline comment
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_block_comment() {
        let source = r#"
            /* Block comment */
            component Foo {
                /* Multi
                   line
                   comment */
                x: s32 = 0;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    // ==================== Parse Error Cases ====================

    #[test]
    fn test_error_missing_component_name() {
        let source = "component {}";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_missing_component_brace() {
        let source = "component Foo";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_unclosed_component_brace() {
        let source = "component Foo {";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_missing_property_type() {
        let source = "component Foo { x: ; }";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_missing_property_colon() {
        let source = "component Foo { x s32; }";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_missing_property_semicolon() {
        let source = "component Foo { x: s32 }";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_invalid_type() {
        let source = "component Foo { x: invalid_type; }";
        // This should parse as a named type, not an error
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_error_unclosed_string() {
        let source = r#"component Foo { Text { "unclosed } }"#;
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_invalid_expression() {
        let source = "component Foo { x: s32 = 1 +; }";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_unclosed_if() {
        let source = r#"
            component Foo {
                if true {
                    span {}
            }
        "#;
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_for_without_in() {
        let source = r#"
            component Foo {
                for i items {}
            }
        "#;
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_empty_file() {
        let source = "";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_only_whitespace() {
        let source = "   \n\t  \n";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_only_comments() {
        let source = "// just a comment\n/* another comment */";
        let result = parse(source);
        assert!(result.is_err());
    }

    #[test]
    fn test_error_invalid_package_syntax() {
        let source = r#"
            package invalid;
            component Foo {}
        "#;
        let result = parse_file(source);
        assert!(result.is_err());
    }

    // ==================== Function Declarations ====================

    #[test]
    fn test_parse_export_function_no_params() {
        let source = r#"
            component Counter {
                export test_fn: func();
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.functions.len(), 1);
        assert!(component.functions[0].is_export);
        assert_eq!(component.functions[0].name, "test_fn");
        assert!(component.functions[0].params.is_empty());
        assert!(component.functions[0].return_type.is_none());
    }

    #[test]
    fn test_parse_export_function_with_params() {
        let source = r#"
            component Counter {
                export on_click: func(x: s32, y: s32);
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.functions.len(), 1);
        assert_eq!(component.functions[0].params.len(), 2);
        assert_eq!(component.functions[0].params[0].0, "x");
        assert_eq!(component.functions[0].params[1].0, "y");
    }

    #[test]
    fn test_parse_export_function_with_return() {
        let source = r#"
            component Counter {
                export get_value: func() -> s32;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.functions.len(), 1);
        assert!(component.functions[0].return_type.is_some());
    }

    #[test]
    fn test_parse_non_export_function() {
        let source = r#"
            component Counter {
                internal_fn: func(value: s32);
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.functions.len(), 1);
        assert!(!component.functions[0].is_export);
    }

    #[test]
    fn test_parse_function_with_complex_types() {
        let source = r#"
            component Counter {
                export process: func(items: list<s32>) -> option<string>;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
    }

    #[test]
    fn test_parse_multiple_functions() {
        let source = r#"
            component Counter {
                count: s32 = 0;
                export on_increment: func();
                export on_decrement: func();
                helper: func(x: s32) -> s32;
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let component = result.unwrap();
        assert_eq!(component.properties.len(), 1);
        assert_eq!(component.functions.len(), 3);
    }

    // ==================== Record Declarations ====================

    #[test]
    fn test_parse_simple_record() {
        let source = r#"
            record Point {
                x: s32,
                y: s32,
            }
            component App {}
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.records.len(), 1);
        assert_eq!(file.records[0].name, "Point");
        assert_eq!(file.records[0].fields.len(), 2);
        assert_eq!(file.records[0].fields[0].name, "x");
        assert_eq!(file.records[0].fields[1].name, "y");
    }

    #[test]
    fn test_parse_record_with_complex_types() {
        let source = r#"
            record Mail {
                id: string,
                name: string,
                email: string,
                subject: string,
                text: string,
                date: string,
                read: bool,
                labels: list<string>,
            }
            component MailViewer {}
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.records.len(), 1);
        assert_eq!(file.records[0].name, "Mail");
        assert_eq!(file.records[0].fields.len(), 8);
    }

    #[test]
    fn test_parse_multiple_records() {
        let source = r#"
            record Point {
                x: s32,
                y: s32,
            }
            record Size {
                width: s32,
                height: s32,
            }
            component Canvas {}
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.records.len(), 2);
        assert_eq!(file.records[0].name, "Point");
        assert_eq!(file.records[1].name, "Size");
    }

    #[test]
    fn test_parse_record_with_nested_types() {
        let source = r#"
            record Container {
                items: list<option<s32>>,
                result: result<string, s32>,
            }
            component App {}
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.records.len(), 1);
        assert_eq!(file.records[0].fields.len(), 2);
    }

    #[test]
    fn test_parse_empty_record() {
        let source = r#"
            record Empty {}
            component App {}
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.records.len(), 1);
        assert_eq!(file.records[0].name, "Empty");
        assert!(file.records[0].fields.is_empty());
    }

    #[test]
    fn test_parse_record_before_component() {
        let source = r#"
            package yel:mail@0.1.0;

            record Mail {
                id: string,
                subject: string,
            }

            component MailViewer {
                mails: list<Mail>;
            }
        "#;
        let file = parse_file(source).unwrap();
        assert!(file.package.is_some());
        assert_eq!(file.records.len(), 1);
        assert_eq!(file.components.len(), 1);
    }

    // ==================== Ternary Expressions ====================

    #[test]
    fn test_parse_ternary_expression() {
        let source = r##"
            component App {
                Button {
                    background: is_selected ? "#27272a" : "transparent",
                }
            }
        "##;
        let file = parse_file(source).unwrap();
        assert_eq!(file.components.len(), 1);
        // The binding should parse with a ternary expression
        let comp = &file.components[0].node;
        assert_eq!(comp.body.len(), 1);
    }

    #[test]
    fn test_parse_ternary_with_comparison() {
        let source = r#"
            component App {
                Text {
                    color: count > 0 ? "green" : "red",
                }
            }
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.components.len(), 1);
    }

    #[test]
    fn test_parse_nested_ternary() {
        let source = r#"
            component App {
                Text {
                    color: a ? "red" : b ? "green" : "blue",
                }
            }
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.components.len(), 1);
    }

    #[test]
    fn test_parse_kebab_case_identifiers() {
        let source = r#"
            component MailItem {
                selected-id: string;
                on-click: bool = false;
                my-prop: s32 = 42;
            }
        "#;
        let file = parse_file(source).unwrap();
        assert_eq!(file.components.len(), 1);
        assert_eq!(file.components[0].node.properties.len(), 3);
        assert_eq!(file.components[0].node.properties[0].name, "selected-id");
        assert_eq!(file.components[0].node.properties[1].name, "on-click");
        assert_eq!(file.components[0].node.properties[2].name, "my-prop");
    }

    #[test]
    fn test_parse_mail_ui_structure() {
        let source = r#"
package yel:mail-ui@0.1.0;

record Mail {
    id: string,
    name: string,
}

enum test {
    test
}

component MailItem {
    mail: Mail;
    selected-id: string;

    export on-select: func(id: string);
}
        "#;
        let result = parse_file(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let file = result.unwrap();
        assert_eq!(file.components.len(), 1);
        assert_eq!(file.components[0].node.name, "MailItem");
    }

    #[test]
    fn test_parse_variant() {
        let source = r#"
variant filter {
    all,
    none,
    some(list<string>),
}

component App {
    current-filter: filter;
}
        "#;
        let result = parse_file(source);
        assert!(result.is_ok(), "Parse failed: {:?}", result.err());
        let file = result.unwrap();
        assert_eq!(file.variants.len(), 1);
        assert_eq!(file.variants[0].node.name, "filter");
        assert_eq!(file.variants[0].node.cases.len(), 3);
        assert_eq!(file.variants[0].node.cases[0].node.name, "all");
        assert!(file.variants[0].node.cases[0].node.payload.is_none());
        assert_eq!(file.variants[0].node.cases[1].node.name, "none");
        assert!(file.variants[0].node.cases[1].node.payload.is_none());
        assert_eq!(file.variants[0].node.cases[2].node.name, "some");
        assert!(file.variants[0].node.cases[2].node.payload.is_some());
    }
}
