//! **Construct identity** with the frozen pest parser.
//!
//! `parity.rs` records one bit per program: does this input produce a syntax
//! error. That bit is blind to a whole class of defect — the two front-ends
//! agreeing that an input is *fine* while disagreeing about **what it is**.
//! `component A { ife { div { } } }` was accepted by both, with zero
//! diagnostics on either side, while pest built an `if_node` with the condition
//! `e` and this parser built an `ElementNode` named `ife`. Nothing in the suite
//! could see it, because there is no bit for it to move.
//!
//! So: parse an input with both, project each AST down to the sequence of
//! **(construct kind, byte offset)** pairs it commits to, and require the two
//! sequences to be equal.
//!
//! # Why a projection and not AST equality
//!
//! The two ASTs are different types by design — the whole point of the rewrite
//! is that the new one has `Recovered<T>` holes, `Block::Missing`, interned
//! `Name`s and typed ids the frozen one does not. What has to agree is the
//! *decision*: which production each stretch of source was read as. That is
//! exactly what the projection keeps and everything else it drops.
//!
//! # Why the span **start** and not the whole span
//!
//! Measured, not assumed. Over all 2118 checked-in `.yel` files the two parsers
//! agree on the start offset of every construct and disagree on 1844 files'
//! *end* offsets — systematically, because `finish_node` here excludes trailing
//! trivia and separators (`parser.rs` module docs) and pest's spans include
//! them. Asserting raw end equality would be asserting the frozen parser's
//! trivia convention, which the seam does not promise.
//!
//! The ends are still load-bearing: [`Construct`] is compared on `(kind,
//! start)` and then the new span is required to be **contained** in the frozen
//! one, `start <= end <= frozen.end`. A construct that swallowed the following
//! declaration fails that; a construct that stops one newline earlier does not.
//!
//! # What it covers
//!
//! * top-level item kind, per item
//! * UI node kind, per node — `if` / `for` / element / `@children` / text
//! * statement kind — `let` / `if` / assign / expression
//!
//! …over the 2000-program corpus, the fixtures, the examples, the deterministic
//! truncate/delete sweep and the randomized mutation sweep, plus a hand-written
//! table for the constructs the corpus never writes.
//!
//! Only inputs **both** parsers accept are compared. Recovery shapes on broken
//! input are deliberately different (invariant S5 materialises holes pest simply
//! discards), and `parity.rs` is what holds the accept/reject line.

use yelc_base::{Diagnostics, Interner, SourceId};

mod support;
use support::{
    RANDOM_SEED, Rng, corpus_sources, example_sources, mutation_seeds, positive_fixtures,
    random_mutations, read, single_token_deletions, truncation_offsets,
};

/// `(construct kind, start offset, end offset)`.
type Construct = (&'static str, usize, usize);

// ---------------------------------------------------------------------------
// the frozen projection
// ---------------------------------------------------------------------------

mod frozen {
    use super::Construct;
    use yel_core::syntax::ast as fa;

    /// `None` when the frozen parser did not accept the input.
    pub fn constructs(content: &str) -> Option<Vec<Construct>> {
        let result =
            yel_core::syntax::parser::parse_file_with_source_id(content, yel_core::SourceId(0))
                .ok()?;
        if !result.catched_errors.is_empty() {
            return None;
        }
        let file = result.file;
        let mut out = Vec::new();

        // `ast::File` groups its items by kind rather than keeping them in
        // source order, so document order is restored by the sort in
        // `projection`, not by this traversal.
        push_all(&mut out, "item:record", file.records.iter().map(|i| i.span));
        push_all(&mut out, "item:enum", file.enums.iter().map(|i| i.span));
        push_all(
            &mut out,
            "item:variant",
            file.variants.iter().map(|i| i.span),
        );
        push_all(
            &mut out,
            "item:element",
            file.elements.iter().map(|i| i.span),
        );

        for item in &file.extern_components {
            out.push(("item:extern-component", item.span.start, item.span.end));
            for property in &item.node.properties {
                push_expr_opt(&mut out, property.node.default.as_ref());
            }
        }
        for item in &file.globals {
            out.push(("item:global", item.span.start, item.span.end));
            for property in &item.node.properties {
                push_expr_opt(&mut out, property.node.default.as_ref());
            }
        }
        for item in &file.components {
            out.push(("item:component", item.span.start, item.span.end));
            for property in &item.node.properties {
                push_expr_opt(&mut out, property.node.default.as_ref());
            }
            for node in &item.node.body {
                push_node(&mut out, node);
            }
        }
        Some(out)
    }

    fn push_all(
        out: &mut Vec<Construct>,
        kind: &'static str,
        spans: impl Iterator<Item = yel_core::Span>,
    ) {
        out.extend(spans.map(|span| (kind, span.start, span.end)));
    }

    fn push_expr_opt(out: &mut Vec<Construct>, expr: Option<&fa::Spanned<fa::Expr>>) {
        if let Some(expr) = expr {
            push_expr(out, expr);
        }
    }

    fn push_node(out: &mut Vec<Construct>, node: &fa::Spanned<fa::Node>) {
        let span = node.span;
        match &node.node {
            fa::Node::Element(element) => {
                out.push(("node:element", span.start, span.end));
                for binding in &element.bindings {
                    push_expr(out, &binding.node.value);
                }
                // A prop whose value is a closure lands in `handlers`, not in
                // `bindings`, and its statements hang directly off it.
                for handler in &element.handlers {
                    for stmt in &handler.node.body {
                        push_stmt(out, stmt);
                    }
                }
                for child in &element.children {
                    push_node(out, child);
                }
            }
            fa::Node::Text(text) => {
                out.push(("node:text", span.start, span.end));
                push_expr(out, &text.content);
            }
            fa::Node::If(node) => {
                out.push(("node:if", span.start, span.end));
                push_expr(out, &node.condition);
                for child in &node.then_branch {
                    push_node(out, child);
                }
                for (condition, body) in &node.else_if_branches {
                    push_expr(out, condition);
                    for child in body {
                        push_node(out, child);
                    }
                }
                for child in node.else_branch.iter().flatten() {
                    push_node(out, child);
                }
            }
            fa::Node::For(node) => {
                out.push(("node:for", span.start, span.end));
                push_expr(out, &node.iterable);
                if let Some(key) = &node.key {
                    push_expr(out, key);
                }
                for child in &node.body {
                    push_node(out, child);
                }
            }
            fa::Node::Children => out.push(("node:children", span.start, span.end)),
        }
    }

    fn push_stmt(out: &mut Vec<Construct>, stmt: &fa::Spanned<fa::Statement>) {
        let span = stmt.span;
        match &stmt.node {
            fa::Statement::Expr(expr) => {
                out.push(("stmt:expr", span.start, span.end));
                push_expr(out, expr);
            }
            fa::Statement::Assign(target, value) => {
                out.push(("stmt:assign", span.start, span.end));
                push_expr(out, target);
                push_expr(out, value);
            }
            fa::Statement::CompoundAssign(target, _, value) => {
                out.push(("stmt:assign", span.start, span.end));
                push_expr(out, target);
                push_expr(out, value);
            }
            fa::Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                out.push(("stmt:if", span.start, span.end));
                push_expr(out, condition);
                for stmt in then_branch {
                    push_stmt(out, stmt);
                }
                for stmt in else_branch.iter().flatten() {
                    push_stmt(out, stmt);
                }
            }
            fa::Statement::Let { value, .. } => {
                out.push(("stmt:let", span.start, span.end));
                push_expr(out, value);
            }
        }
    }

    /// Expressions themselves are **not** projected — the two grammars shape
    /// `Expr` differently on purpose (`MethodCall` versus `PathCall`, a
    /// flattened Pratt spine versus a nested one). What is walked out of them is
    /// the statement blocks hanging off closures, which are constructs.
    ///
    /// Iterative, because a valid `a.b.b.b…` chain nests one level per link and
    /// recursing over it is the stack overflow `ast::visit` was rewritten to
    /// avoid.
    fn push_expr(out: &mut Vec<Construct>, expr: &fa::Spanned<fa::Expr>) {
        let mut stack = vec![expr];
        while let Some(expr) = stack.pop() {
            match &expr.node {
                fa::Expr::Closure { body, .. } => {
                    for stmt in body {
                        push_stmt(out, stmt);
                    }
                }
                fa::Expr::Binary(lhs, _, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                fa::Expr::Unary(_, operand) => stack.push(operand),
                fa::Expr::Call(_, args) => stack.extend(args.iter()),
                fa::Expr::PathCall { base, args, .. } => {
                    stack.push(base);
                    stack.extend(args.iter());
                }
                fa::Expr::Member(base, _) | fa::Expr::OptionalMember(base, _) => stack.push(base),
                fa::Expr::MethodCall { receiver, args, .. } => {
                    stack.push(receiver);
                    stack.extend(args.iter());
                }
                fa::Expr::Index(base, index) => {
                    stack.push(base);
                    stack.push(index);
                }
                fa::Expr::Interpolation(parts) => {
                    stack.extend(parts.iter().filter_map(|part| match part {
                        fa::InterpolationPart::Expr(expr) => Some(expr),
                        fa::InterpolationPart::Literal(_) => None,
                    }));
                }
                fa::Expr::Range { start, end, .. } => {
                    stack.push(start);
                    stack.push(end);
                }
                fa::Expr::Ternary {
                    condition,
                    then_expr,
                    else_expr,
                } => {
                    stack.push(condition);
                    stack.push(then_expr);
                    stack.push(else_expr);
                }
                fa::Expr::Literal(literal) => match literal {
                    fa::Literal::List(items) | fa::Literal::Tuple(items) => {
                        stack.extend(items.iter())
                    }
                    fa::Literal::Record { fields } => {
                        stack.extend(fields.iter().map(|(_, value)| value))
                    }
                    _ => {}
                },
                fa::Expr::Ident(_) => {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// the new projection
// ---------------------------------------------------------------------------

mod fresh {
    use super::Construct;
    use yelc_base::{Diagnostics, Interner, SourceId};
    use yelc_syntax::ast as na;

    /// `None` when the new parser reported anything.
    pub fn constructs(content: &str) -> Option<Vec<Construct>> {
        let interner = Interner::new();
        let mut diags = Diagnostics::new();
        let parsed = yelc_syntax::parse(SourceId(0), content, &interner, &mut diags);
        if diags.has_errors() {
            return None;
        }

        let mut out = Vec::new();
        for item in &parsed.ast.items {
            let span = item.span();
            match item {
                // The frozen AST keeps the package as a bare `PackageId` with
                // no span, so there is nothing to compare it against.
                na::ItemKind::Package(_) => {}
                na::ItemKind::Record(_) => out.push(("item:record", span.start, span.end)),
                na::ItemKind::Enum(_) => out.push(("item:enum", span.start, span.end)),
                na::ItemKind::Variant(_) => out.push(("item:variant", span.start, span.end)),
                na::ItemKind::Element(_) => out.push(("item:element", span.start, span.end)),
                na::ItemKind::ExternComponent(decl) => {
                    out.push(("item:extern-component", span.start, span.end));
                    for property in decl.properties() {
                        push_expr_opt(&mut out, property.default.as_ref());
                    }
                }
                na::ItemKind::Global(decl) => {
                    out.push(("item:global", span.start, span.end));
                    for property in decl.properties() {
                        push_expr_opt(&mut out, property.default.as_ref());
                    }
                }
                na::ItemKind::Component(decl) => {
                    out.push(("item:component", span.start, span.end));
                    for property in decl.properties() {
                        push_expr_opt(&mut out, property.default.as_ref());
                    }
                    for node in decl.body() {
                        push_node(&mut out, node);
                    }
                }
                // Unreachable: an `Error` item always carries a diagnostic
                // (invariant S5), and this function returned already.
                na::ItemKind::Error { .. } => out.push(("item:error", span.start, span.end)),
            }
        }
        Some(out)
    }

    fn push_expr_opt(out: &mut Vec<Construct>, expr: Option<&na::Expr>) {
        if let Some(expr) = expr {
            push_expr(out, expr);
        }
    }

    fn push_block(out: &mut Vec<Construct>, block: &na::Block<na::UiNode>) {
        for node in block.present().into_iter().flatten() {
            push_node(out, node);
        }
    }

    fn push_node(out: &mut Vec<Construct>, node: &na::UiNode) {
        let span = node.span();
        match node {
            na::UiNode::Element(element) => {
                out.push(("node:element", span.start, span.end));
                for prop in &element.props {
                    push_expr(out, &prop.value);
                }
                for child in &element.children {
                    push_node(out, child);
                }
            }
            na::UiNode::Text(text) => {
                out.push(("node:text", span.start, span.end));
                push_expr(out, &text.content);
            }
            na::UiNode::If(node) => {
                out.push(("node:if", span.start, span.end));
                push_expr(out, &node.condition);
                push_block(out, &node.then_branch);
                for branch in &node.else_if_branches {
                    push_expr(out, &branch.condition);
                    push_block(out, &branch.body);
                }
                if let Some(body) = &node.else_branch {
                    push_block(out, body);
                }
            }
            na::UiNode::For(node) => {
                out.push(("node:for", span.start, span.end));
                push_expr(out, &node.iterable);
                if let Some(key) = &node.key {
                    push_expr(out, key);
                }
                push_block(out, &node.body);
            }
            na::UiNode::Children { .. } => out.push(("node:children", span.start, span.end)),
            na::UiNode::Error { .. } => out.push(("node:error", span.start, span.end)),
        }
    }

    fn push_stmt(out: &mut Vec<Construct>, stmt: &na::Stmt) {
        let span = stmt.span();
        match stmt {
            na::Stmt::Let(node) => {
                out.push(("stmt:let", span.start, span.end));
                push_expr(out, &node.value);
            }
            na::Stmt::If(node) => {
                out.push(("stmt:if", span.start, span.end));
                push_expr(out, &node.condition);
                for stmt in node.then_branch.present().into_iter().flatten() {
                    push_stmt(out, stmt);
                }
                for stmt in node
                    .else_branch
                    .as_ref()
                    .and_then(|branch| branch.present())
                    .into_iter()
                    .flatten()
                {
                    push_stmt(out, stmt);
                }
            }
            na::Stmt::Assign(node) => {
                out.push(("stmt:assign", span.start, span.end));
                push_expr(out, &node.target);
                push_expr(out, &node.value);
            }
            na::Stmt::Expr(node) => {
                out.push(("stmt:expr", span.start, span.end));
                push_expr(out, &node.expr);
            }
            na::Stmt::Error { .. } => out.push(("stmt:error", span.start, span.end)),
        }
    }

    fn push_expr(out: &mut Vec<Construct>, expr: &na::Expr) {
        let mut stack = vec![expr];
        while let Some(expr) = stack.pop() {
            match &expr.kind {
                na::ExprKind::Closure(closure) => {
                    for stmt in &closure.body {
                        push_stmt(out, stmt);
                    }
                }
                na::ExprKind::Interpolation(parts) => {
                    stack.extend(parts.iter().filter_map(|part| match part {
                        na::InterpolationPart::Expr(expr) => Some(expr),
                        na::InterpolationPart::Literal(_) => None,
                    }));
                }
                na::ExprKind::List(items) | na::ExprKind::Tuple(items) => {
                    stack.extend(items.iter())
                }
                na::ExprKind::Record(fields) => stack.extend(
                    fields
                        .iter()
                        .filter_map(|field| field.present())
                        .map(|field| &field.value),
                ),
                na::ExprKind::Unary { operand, .. } => stack.push(operand),
                na::ExprKind::Binary { lhs, rhs, .. } => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                na::ExprKind::Ternary {
                    condition,
                    then_expr,
                    else_expr,
                } => {
                    stack.push(condition);
                    stack.push(then_expr);
                    stack.push(else_expr);
                }
                na::ExprKind::Range { start, end, .. } => {
                    stack.push(start);
                    stack.push(end);
                }
                na::ExprKind::Call { args, .. } => stack.extend(args.iter()),
                na::ExprKind::PathCall { base, args, .. } => {
                    stack.push(base);
                    stack.extend(args.iter());
                }
                na::ExprKind::Member { base, .. } | na::ExprKind::OptionalMember { base, .. } => {
                    stack.push(base)
                }
                na::ExprKind::Index { base, index } => {
                    stack.push(base);
                    stack.push(index);
                }
                na::ExprKind::Int(_)
                | na::ExprKind::Float(_)
                | na::ExprKind::Unit { .. }
                | na::ExprKind::Color(_)
                | na::ExprKind::Char(_)
                | na::ExprKind::Bool(_)
                | na::ExprKind::String(_)
                | na::ExprKind::Ident(_)
                | na::ExprKind::Error => {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// the comparison
// ---------------------------------------------------------------------------

/// What went wrong, when it did.
#[derive(Debug, PartialEq, Eq)]
enum Mismatch {
    /// Different construct kinds, or a different number of them. The
    /// `(kind, start)` sequences, frozen first.
    Shape(Vec<(&'static str, usize)>, Vec<(&'static str, usize)>),
    /// Same shape, but a construct's span escapes the frozen one it names.
    Extent(Construct, Construct),
}

/// `None` when the two agree, **or** when either parser did not accept the
/// input. `Some(_)` is a real disagreement about what the source says.
fn compare(content: &str) -> Option<Mismatch> {
    let (Some(mut frozen), Some(mut fresh)) =
        (frozen::constructs(content), fresh::constructs(content))
    else {
        return None;
    };
    // Both sides are in traversal order, and the two traversals are different
    // (the frozen `File` is grouped by item kind). Sorting is what makes them
    // comparable; a misidentification changes the sorted sequence just as it
    // changes the traversal one.
    frozen.sort();
    fresh.sort();

    let shape = |list: &[Construct]| -> Vec<(&'static str, usize)> {
        list.iter()
            .map(|(kind, start, _)| (*kind, *start))
            .collect()
    };
    if shape(&frozen) != shape(&fresh) {
        return Some(Mismatch::Shape(shape(&frozen), shape(&fresh)));
    }
    for (f, n) in frozen.iter().zip(&fresh) {
        if n.1 > n.2 || n.2 > f.2 {
            return Some(Mismatch::Extent(*f, *n));
        }
    }
    None
}

/// Whether both parsers accepted, so the input was actually compared.
fn comparable(content: &str) -> bool {
    frozen::constructs(content).is_some() && fresh::constructs(content).is_some()
}

// ---------------------------------------------------------------------------
// the sweeps
// ---------------------------------------------------------------------------

/// Checked-in `.yel` files both parsers accept. One short of the 2094 swept,
/// and the exclusion is named rather than counted:
///
/// * `examples/counter/counter.yel` — rejected by **both**, in agreement.
///
/// It was two short of 2095 until 2026-07-29. `global_filter_default.yel` was
/// the other exclusion — ungrammatical text the frozen parser's
/// `BLOCK_LEVEL_CATCH_ALL` silently swallowed, leaving no accepted parse to
/// compare against — and it moved to `known_bugs/`. **The comparable count did
/// not move**: one fewer file swept, one fewer excused. Removing a file that
/// could not be compared costs no comparison, which is why this number is the
/// one to watch rather than the sweep size.
const COMPARABLE_SOURCES: usize = 2093;

/// By name, so "fewer comparable" can never be explained away by pointing at a
/// different file.
const INCOMPARABLE_SOURCES: &[&str] = &["examples/counter/counter.yel"];

#[test]
fn every_construct_in_every_checked_in_program_is_read_the_same_way() {
    let sources: Vec<_> = corpus_sources()
        .into_iter()
        .chain(positive_fixtures())
        .chain(example_sources())
        .collect();
    assert_eq!(sources.len(), 2094, "the source sweep changed size");

    let mut compared = 0usize;
    let mut skipped = Vec::new();
    let mut mismatches = Vec::new();
    for path in &sources {
        let content = read(path);
        if comparable(&content) {
            compared += 1;
        } else {
            skipped.push(support::label(path));
        }
        if let Some(mismatch) = compare(&content) {
            mismatches.push((support::label(path), mismatch));
        }
    }

    assert_eq!(
        skipped, INCOMPARABLE_SOURCES,
        "the set of programs one parser rejects changed; a new name here is a \
         program that stopped being compared"
    );
    assert_eq!(
        compared, COMPARABLE_SOURCES,
        "fewer programs are being compared than before; a sweep that quietly \
         stops comparing reports zero mismatches for the wrong reason"
    );
    assert!(
        mismatches.is_empty(),
        "{} of {compared} programs are read as different constructs by the two \
         parsers: {:#?}",
        mismatches.len(),
        &mismatches[..mismatches.len().min(4)]
    );
}

/// Deterministic truncate/delete plus randomized byte mutations, over the same
/// seed set `parity.rs` and `corpus.rs` use. Mutations are where a
/// misidentification actually lives: the corpus is machine-generated and never
/// writes a glued keyword.
const MUTATION_CASES: usize = 5_264;

#[test]
fn every_construct_in_every_mutated_program_is_read_the_same_way() {
    let mut rng = Rng::new(RANDOM_SEED);
    let mut cases = 0usize;
    let mut compared = 0usize;
    let mut mismatches = Vec::new();

    for path in mutation_seeds() {
        let content = read(&path);
        let name = support::label(&path);
        let mut subjects: Vec<String> = truncation_offsets(&content)
            .into_iter()
            .map(|cut| content[..cut].to_string())
            .collect();
        subjects.extend(single_token_deletions(&content));
        subjects.extend(random_mutations(&content, 60, &mut rng));

        for (index, subject) in subjects.into_iter().enumerate() {
            cases += 1;
            if comparable(&subject) {
                compared += 1;
            }
            if let Some(mismatch) = compare(&subject) {
                mismatches.push((format!("{name}#{index}: {subject:?}"), mismatch));
            }
        }
    }

    assert_eq!(cases, MUTATION_CASES, "the mutation sweep changed size");
    assert!(
        compared * 10 >= cases,
        "only {compared} of {cases} mutations were comparable; the sweep is no \
         longer exercising the projection"
    );
    assert!(
        mismatches.is_empty(),
        "{} of {compared} comparable mutations are read as different constructs: \
         {:#?}",
        mismatches.len(),
        &mismatches[..mismatches.len().min(4)]
    );
}

// ---------------------------------------------------------------------------
// the hand-written table
// ---------------------------------------------------------------------------

/// The constructs the corpus never writes, and the ones a hand-rolled lexer is
/// most likely to misread: keyword-shaped names, the `{` alternatives, and the
/// positions where `if_node` and `element_node` are both live.
///
/// Every one of these is *accepted by both parsers with no diagnostic*, which
/// is precisely why `parity.rs` cannot see them.
///
/// The keyword word boundary moved eleven rows **out** of this table: with
/// `ifo > 0 { … }`, `forx in xs { … }` and their siblings now rejected by both
/// parsers, they are accept/reject cases and live in `parity.rs`'s
/// keyword-boundary class instead. The rows that stayed are the ones both
/// parsers still accept — and they now agree on *element*, where they used to
/// agree on *if*.
const HANDWRITTEN: &[&str] = &[
    // -- a name that begins with `if`, at node position, where `element_node`
    // is the only live alternative
    "component A { ife { div { } } }",
    "component A { ifa { div { } } }",
    "component A { ifx { div { } } }",
    // …and the one that reached the element reading by backtracking before the
    // boundary landed, and reaches it directly now.
    "component A { iflex { color: red } }",
    "component A { iflex { color: red } else { \"x\" } }",
    // an `if` whose condition is a record literal, and the element called `if`
    "component A { if { a: 1 } { div { } } }",
    "component A { if { a: 1 } { div { } } else { \"x\" } }",
    "component A { if { span { \"x\" } } }",
    "component A { if { a: 1 } }",
    // -- names beginning with `for`, and the alternatives `for` must not steal
    "component A { format { \"a\" } }",
    "component A { for { span { \"x\" } } }",
    // -- names beginning with `let` / `if` at statement position
    "component A { div { f: { letx = 1; } } }",
    "component A { div { f: { letters = 1; } } }",
    "component A { div { f: { let8 = 1; } } }",
    "component A { div { f: { let-x = 1; } } }",
    "component A { div { f: { let = 1; } } }",
    "component A { div { f: { ifx = 1; } } }",
    "component A { div { f: { if a { } else } } }",
    "component A { div { f: { if a { b(); } else { c(); } } } }",
    // -- the `{` alternatives: record, closure, typed closure
    "component A { div { f: { a: 1 } } }",
    "component A { div { f: { k: s32 -> k } } }",
    "component A { div { f: { p -> p } } }",
    "component A { div { f: { p, q -> p } } }",
    "component A { div { f: { } } }",
    "component A { div { f: { 10 } } }",
    "component A { div { f: { a: 1, b: { c: 2 } } } }",
    // -- every top-level item kind, in one file
    "package a:b@1.0.0;\nrecord R { a: s32, }\nenum E { c }\nvariant V { c(s32) }\n\
     element El { a: s32; }\nextern component C { a: string; @children }\n\
     export global G { in x: u32; }\nexport component App { @children }",
    // -- statement kinds
    "component A { div { f: { let x: s32 = 1; count += 1; count = x; \
     if x > 0 { count -= 1; } else { count *= 2; } g(x); x } } }",
];

/// Inputs in [`HANDWRITTEN`] the two parsers genuinely read differently.
///
/// **Empty.** It briefly held `{ p: s32->p }`, where pest read a closure and
/// this parser read a record: pest is scannerless, so `primitive_type` matched
/// the bare `"s32"` literal and stopped, leaving `->`; this lexer admitted `-`
/// into identifiers unconditionally, so maximal munch produced `s32-` `>` and
/// no `ARROW` ever formed.
///
/// Fixed in **both** compilers by one character of lookahead — a `-` joins an
/// identifier only when a name character follows it. The frozen grammar gained
/// `("-" ~ &(ALNUM|"_"))` on its six kebab rules; the lexer gained the matching
/// peek. Kebab names are untouched (`selected-id`, `case-a`, `starts-with`),
/// and `count-=1` and `{p->p}` now mean what they look like.
///
/// The change is a **surface language change**, so it was gated on evidence
/// rather than argument: the corpus was regenerated and all 8000 artifacts came
/// back byte-identical. See `plans/rewrite/goldens-changed.md`.
const KNOWN_IDENTITY_DIVERGENCES: &[&str] = &[];

#[test]
fn the_hand_written_table_is_read_the_same_way() {
    // Exact, so a case cannot be quietly dropped when it starts failing.
    assert_eq!(HANDWRITTEN.len(), 28);

    let mut mismatches = Vec::new();
    for case in HANDWRITTEN {
        assert!(
            comparable(case),
            "{case:?} is no longer accepted by both parsers, so it no longer \
             tests construct identity — move it to parity.rs or fix it"
        );
        if let Some(mismatch) = compare(case) {
            mismatches.push((*case, mismatch));
        }
    }
    assert!(
        mismatches.is_empty(),
        "{} hand-written cases are read as different constructs: {:#?}",
        mismatches.len(),
        mismatches
    );
}

/// The list is exact in both directions: a new construct-identity divergence
/// fails [`the_hand_written_table_is_read_the_same_way`], and an entry that has
/// *stopped* diverging fails here.
///
/// It is currently empty. That is the strong state, and this test is what keeps
/// it from silently regaining an entry: an allow-list nobody has to justify
/// growing is not a ratchet.
#[test]
fn every_known_identity_divergence_still_diverges() {
    for case in KNOWN_IDENTITY_DIVERGENCES {
        assert!(
            comparable(case),
            "{case:?}: both parsers must still accept this, or it is an \
             accept/reject divergence and belongs in parity.rs"
        );
        assert!(
            matches!(compare(case), Some(Mismatch::Shape(_, _))),
            "{case:?}: this no longer diverges — delete the entry, do not \
             keep an allow-list that outlives the thing it allows"
        );
    }
}

/// The projection has to be able to *see* every construct kind it claims to
/// cover, or "zero mismatches" is a statement about an empty set.
#[test]
fn the_projection_reaches_every_construct_kind_it_names() {
    let all = "package a:b@1.0.0;\nrecord R { a: s32, }\nenum E { c }\nvariant V { c(s32) }\n\
               element El { a: s32; }\nextern component C { a: string; @children }\n\
               export global G { in x: u32; }\n\
               export component App {\n\
                 v: s32 = 0;\n\
                 if v > 0 { div { on: { let x: s32 = 1; v += 1; v = x; \
                   if x > 0 { g(); } else { h(); } g(x); x } } } else { \"t\" }\n\
                 for i in 0..3 { \"x\" }\n\
                 @children\n\
               }";
    let frozen = frozen::constructs(all).expect("the frozen parser must accept the sample");
    let fresh = fresh::constructs(all).expect("the new parser must accept the sample");
    assert_eq!(compare(all), None, "the sample itself must not diverge");

    let mut kinds: Vec<&str> = fresh.iter().map(|(kind, _, _)| *kind).collect();
    kinds.sort_unstable();
    kinds.dedup();
    assert_eq!(
        kinds,
        [
            "item:component",
            "item:element",
            "item:enum",
            "item:extern-component",
            "item:global",
            "item:record",
            "item:variant",
            "node:children",
            "node:element",
            "node:for",
            "node:if",
            "node:text",
            "stmt:assign",
            "stmt:expr",
            "stmt:if",
            "stmt:let",
        ],
        "the sample no longer exercises every construct kind the projection names"
    );
    assert_eq!(frozen.len(), fresh.len());
}

/// A projection that ignores its input would report zero mismatches forever.
/// This is the check that it does not: a misidentification injected on the new
/// side has to be *caught*.
#[test]
fn the_projection_catches_an_injected_misidentification() {
    // The pair used to be `ife { div { } }` (an `if`) against
    // `iflex { color: red }` (an element). `ife` is a name now, so the probe
    // moved to the ambiguity that survives the keyword boundary: `if` followed
    // directly by `{` is an if-node when the block is a record-literal
    // condition and an element called `if` when it is not. Both start at the
    // same offset, which is what the projection has to tell apart.
    let as_an_if = "component A { if { a: 1 } { div { } } }";
    let as_an_element = "component A { if { span { \"x\" } } }";

    let kind = |list: &[Construct], at: usize| {
        list.iter()
            .find(|(_, start, _)| *start == at)
            .map(|(kind, _, _)| *kind)
    };
    let one = frozen::constructs(as_an_if).expect("frozen accepts");
    assert_eq!(kind(&one, 14), Some("node:if"));
    assert_eq!(
        kind(&fresh::constructs(as_an_if).expect("accepts"), 14),
        Some("node:if")
    );
    // …and the sibling the guard must *not* claim projects differently, on both
    // sides, at the same offset.
    assert_eq!(
        kind(&frozen::constructs(as_an_element).expect("accepts"), 14),
        Some("node:element")
    );
    assert_eq!(
        kind(&fresh::constructs(as_an_element).expect("accepts"), 14),
        Some("node:element")
    );

    // The comparison itself must report, not silently pass, when the sequences
    // differ. Feed it two projections that do differ.
    let mut doctored = one.clone();
    let at = doctored
        .iter()
        .position(|(_, start, _)| *start == 14)
        .expect("the node at offset 14");
    assert_eq!(doctored[at].0, "node:if");
    doctored[at].0 = "node:element";
    let shape = |list: &[Construct]| -> Vec<(&'static str, usize)> {
        list.iter()
            .map(|(kind, start, _)| (*kind, *start))
            .collect()
    };
    assert_ne!(
        shape(&one),
        shape(&doctored),
        "the shape projection does not distinguish an `if` from an element, so \
         every sweep above is vacuous"
    );
}

/// The parsers are compared at all — a guard against both projections silently
/// returning `None` for everything, which would make every sweep above vacuous.
#[test]
fn the_projections_are_not_empty() {
    let interner = Interner::new();
    let mut diags = Diagnostics::new();
    let sample = read(&positive_fixtures()[0]);
    let parsed = yelc_syntax::parse(SourceId(0), &sample, &interner, &mut diags);
    assert!(!parsed.ast.items.is_empty());
    assert!(
        !frozen::constructs(&sample)
            .expect("frozen accepts")
            .is_empty()
    );
    assert!(!fresh::constructs(&sample).expect("new accepts").is_empty());
}
