//! Phase 3: bodies — statements, expressions, and the desugarings that need
//! nothing but names.
//!
//! One lowering walk. There is no second walker over the AST anywhere in this
//! phase (A3): the `@children` slot count that the frozen tree recomputed with
//! `collect_children_slots` is collected *by this walk* as it passes each
//! marker.

use yelc_base::{Diagnostic, ErrorCode, IndexVec, Name, Span};
use yelc_sema::{DefId, Known};
use yelc_syntax::{ParsedFile, ast};

use super::{LoweringContext, ui};
use crate::expr::{
    BinaryOp, HirBlock, HirCallee, HirClosure, HirExpr, HirExprKind, HirFieldInit,
    HirInterpolationPart, HirLiteral, HirLocal, HirMatch, HirMatchArm, HirPattern, HirStmt,
    UnaryOp,
};
use crate::ids::{BodyId, HirId, LocalId, SourceNodeId, TypeId};
use crate::module::{HirBody, HirComponent, HirDefault, HirFunction, HirGlobal, HirItem};
use crate::sym::{ModuleSymTable, SymbolKind};

// ---------------------------------------------------------------------------
// Items
// ---------------------------------------------------------------------------

/// A root function — the .yelir subset's `name: func(…) { … }` at item or
/// module level. Same lowering a global's member function gets, with no
/// owner: bare names inside resolve to locals and root definitions only.
pub(super) fn lower_item_function(
    lcx: &mut LoweringContext,
    file: &ParsedFile,
    decl: &ast::FunctionDecl,
) {
    let Some(def) = lcx.def_of(file, decl.id) else {
        // Unregistered: a parse hole or a duplicate; the diagnostic exists (H5).
        return;
    };
    // `member: 0` — a root function has no owner row; the def alongside is
    // the identity (see `HirItem::Function`).
    let Some(function) = lower_function(lcx, file, None, decl, 0) else {
        return;
    };
    lcx.module.items.push(HirItem::Function { def, function });
}

pub(super) fn lower_global(lcx: &mut LoweringContext, file: &ParsedFile, decl: &ast::GlobalDecl) {
    let Some(def) = lcx.def_of(file, decl.id) else {
        // Unregistered: a parse hole where the name should be, or a duplicate.
        // Either way the diagnostic exists (H5); there is no item to build.
        return;
    };
    let hir_id = lcx.primary(lcx.node(file, decl.id));
    let name = lcx.sema.defs.get(def).name;

    let mut defaults = Vec::new();
    let mut functions = Vec::new();
    let mut member = 0u32;
    for item in &decl.members {
        match item {
            ast::GlobalMember::Property(property) => {
                if property.name.present().is_none() {
                    continue;
                }
                if let Some(default) = &property.default {
                    let body = lower_default(lcx, file, def, property.id, default);
                    defaults.push(HirDefault { member, body });
                }
                member += 1;
            }
            ast::GlobalMember::Callback(callback) => {
                if let Some(function) = lower_function(lcx, file, Some(def), callback, member) {
                    functions.push(function);
                }
                if callback.name.present().is_some() {
                    member += 1;
                }
            }
            ast::GlobalMember::Error { .. } => {}
        }
    }

    lcx.module.items.push(HirItem::Global(HirGlobal {
        hir_id,
        def,
        name,
        is_export: decl.is_export,
        defaults,
        functions,
    }));
}

pub(super) fn lower_component(
    lcx: &mut LoweringContext,
    file: &ParsedFile,
    decl: &ast::ComponentDecl,
) {
    let Some(def) = lcx.def_of(file, decl.id) else {
        return;
    };
    let hir_id = lcx.primary(lcx.node(file, decl.id));
    let name = lcx.sema.defs.get(def).name;

    let mut defaults = Vec::new();
    let mut functions = Vec::new();
    let mut member = 0u32;
    for item in &decl.members {
        match item {
            ast::ComponentMember::Property(property) => {
                if property.name.present().is_none() {
                    continue;
                }
                if let Some(default) = &property.default {
                    let body = lower_default(lcx, file, def, property.id, default);
                    defaults.push(HirDefault { member, body });
                }
                member += 1;
            }
            ast::ComponentMember::Function(function) => {
                if let Some(lowered) = lower_function(lcx, file, Some(def), function, member) {
                    functions.push(lowered);
                }
                if function.name.present().is_some() {
                    member += 1;
                }
            }
            // The UI tree lowers below, as one build body.
            ast::ComponentMember::Node(_) => {}
            ast::ComponentMember::Error { .. } => {}
        }
    }

    let build = ui::lower_tree(lcx, file, def, decl);

    lcx.module.items.push(HirItem::Component(HirComponent {
        hir_id,
        def,
        name,
        is_export: decl.is_export,
        defaults,
        functions,
        build,
    }));
}

/// A property default: a parameterless body whose tail is the value.
fn lower_default(
    lcx: &mut LoweringContext,
    file: &ParsedFile,
    owner: DefId,
    property: yelc_syntax::NodeId,
    default: &ast::Expr,
) -> BodyId {
    let origin = lcx.node(file, property);
    let mut body = BodyLowering::new(lcx, file, origin, Some(owner));
    let value = body.lower_expr(default);
    body.finish(
        0,
        HirBlock {
            stmts: Vec::new(),
            tail: Some(value),
        },
    )
}

/// A member function. `None` when the declaration has no usable name — its
/// parse diagnostic exists and there is no member row to point at.
fn lower_function(
    lcx: &mut LoweringContext,
    file: &ParsedFile,
    owner: Option<DefId>,
    decl: &ast::FunctionDecl,
    member: u32,
) -> Option<HirFunction> {
    let ident = decl.name.present()?;
    let hir_id = lcx.primary(lcx.node(file, decl.id));

    let body = decl.body.as_ref().map(|block| {
        let origin = lcx.node(file, decl.id);
        let mut lowering = BodyLowering::new(lcx, file, origin, owner);
        let mut params = 0u32;
        if let Some(signature) = decl.signature.present() {
            for param in signature.present_params() {
                let Some(param_ident) = param.name.present() else {
                    continue;
                };
                let annotation = TypeId::new(lowering.node(param.ty.id));
                lowering.define(param_ident, Some(annotation));
                params += 1;
            }
        }
        let block = lowering.lower_block(block);
        lowering.finish(params, block)
    });

    Some(HirFunction {
        hir_id,
        name: ident.name,
        member,
        is_export: decl.is_export,
        body,
    })
}

// ---------------------------------------------------------------------------
// One body's lowering state
// ---------------------------------------------------------------------------

/// Lowers the contents of one [`HirBody`]: one locals arena, a scope stack
/// over it, and the primary/synthesized allocation mode.
pub(super) struct BodyLowering<'a, 'ctx> {
    pub lcx: &'a mut LoweringContext<'ctx>,
    file: &'a ParsedFile,
    /// The construct this body is generated from — the origin of every id the
    /// body itself needs beyond its nodes'.
    origin: SourceNodeId,
    locals: IndexVec<LocalId, HirLocal>,
    /// Ark's `ModuleSymTable` (`sym.rs`, ported): block levels over the
    /// module's declarations over the prelude. The enclosing item's members
    /// arrive as the outermost pushed level — scope composition, not a
    /// special resolution domain.
    symtable: ModuleSymTable,
    /// Non-zero while lowering a desugaring's *second* reading of syntax that
    /// already has a primary node — `x += 1`'s rebuilt `x`. Every allocation
    /// in that mode is `synthesize`, so H2's injectivity holds.
    synthetic: u32,
    /// `@children` markers passed by this walk, for the single-slot rule.
    pub slots: Vec<Span>,
}

impl<'a, 'ctx> BodyLowering<'a, 'ctx> {
    pub(super) fn new(
        lcx: &'a mut LoweringContext<'ctx>,
        file: &'a ParsedFile,
        origin: SourceNodeId,
        owner: Option<DefId>,
    ) -> Self {
        let mut symtable = ModuleSymTable::new();
        // The enclosing item's members are the outermost level — ark's
        // checker seeds scope the same way, and composition is what lets a
        // `let count` further in shadow the property.
        symtable.push_level();
        if let Some(owner) = owner {
            for member in lcx.sema.defs.members(owner) {
                symtable.insert(
                    member.name,
                    SymbolKind::Prop {
                        owner,
                        member: member.name,
                    },
                );
            }
        }
        // The body's own root level.
        symtable.push_level();
        Self {
            lcx,
            file,
            origin,
            locals: IndexVec::new(),
            symtable,
            synthetic: 0,
            slots: Vec::new(),
        }
    }

    pub(super) fn node(&self, id: yelc_syntax::NodeId) -> SourceNodeId {
        SourceNodeId::new(self.file.source, id)
    }

    /// A primary id — or a synthesized one when this walk is re-reading syntax
    /// whose primary already exists.
    fn alloc(&mut self, id: yelc_syntax::NodeId) -> HirId {
        let node = self.node(id);
        if self.synthetic > 0 {
            self.lcx.synthesize(node)
        } else {
            self.lcx.primary(node)
        }
    }

    /// An id for a node the desugaring invents, whatever the mode.
    fn invent(&mut self, origin: yelc_syntax::NodeId) -> HirId {
        let node = self.node(origin);
        self.lcx.synthesize(node)
    }

    /// The UI walk's entry points for the same two allocators.
    pub(super) fn primary_id(&mut self, id: yelc_syntax::NodeId) -> HirId {
        self.alloc(id)
    }

    pub(super) fn invent_id(&mut self, origin: yelc_syntax::NodeId) -> HirId {
        self.invent(origin)
    }

    /// A binder position whose name is a parse hole: bind a placeholder local
    /// so the body still lowers and its errors surface. The hole's diagnostic
    /// exists.
    pub(super) fn define_missing_binder(&mut self, origin: yelc_syntax::NodeId) -> LocalId {
        let hir_id = self.invent(origin);
        let name = self.lcx.sema.names.intern("<missing>");
        self.locals.push(HirLocal {
            hir_id,
            name,
            ty: None,
        })
    }

    pub(super) fn finish(mut self, params: u32, block: HirBlock) -> BodyId {
        // Ark's balance check (`typecheck/function.rs`): every push has its
        // pop, asserted where the body closes rather than trusted.
        assert_eq!(
            self.symtable.levels(),
            2,
            "a scope level leaked out of a body's lowering"
        );
        let hir_id = self.lcx.module.map.synthesize(self.origin);
        self.lcx.module.bodies.push(HirBody {
            hir_id,
            params,
            locals: self.locals,
            block,
        })
    }

    // -- scopes ------------------------------------------------------------

    pub(super) fn push_scope(&mut self) {
        self.symtable.push_level();
    }

    pub(super) fn pop_scope(&mut self) {
        self.symtable.pop_level();
    }

    pub(super) fn define(&mut self, ident: &ast::Ident, ty: Option<TypeId>) -> LocalId {
        let hir_id = self.alloc(ident.id);
        let local = self.locals.push(HirLocal {
            hir_id,
            name: ident.name,
            ty,
        });
        self.symtable.insert(ident.name, SymbolKind::Var(local));
        local
    }

    /// A module bound by an `include`, when `name` reaches one through the
    /// symtable (a local shadowing the module name wins, as anywhere).
    fn lookup_module(&self, name: Name) -> Option<yelc_sema::ModuleId> {
        self.symtable.get(self.lcx.sema, name)?.to_module()
    }

    fn lookup_local(&self, name: Name) -> Option<LocalId> {
        match self.symtable.get(self.lcx.sema, name) {
            Some(SymbolKind::Var(local)) => Some(local),
            _ => None,
        }
    }

    /// Resolve a bare name through the one walk — ark's `check_expr_ident`
    /// shape: `symtable.get`, then map the symbol to the expression form. An
    /// unmatched name stays a name (H4).
    fn resolve(&self, name: Name) -> HirExprKind {
        match self.symtable.get(self.lcx.sema, name) {
            Some(SymbolKind::Var(local)) => HirExprKind::Local(local),
            Some(SymbolKind::Prop { owner, member }) => HirExprKind::Prop { owner, member },
            Some(
                SymbolKind::Type(def)
                | SymbolKind::Value(def)
                | SymbolKind::Component(def)
                | SymbolKind::Global(def),
            ) => HirExprKind::Def(def),
            Some(SymbolKind::Intrinsic(name)) => HirExprKind::Intrinsic(name),
            // A module is not a value; bare use stays unresolved for stage 4
            // to report.
            Some(SymbolKind::Module(_)) | None => HirExprKind::Unresolved(name),
        }
    }

    /// The same walk, callee-shaped.
    pub(super) fn resolve_callee(&self, name: Name) -> HirCallee {
        match self.symtable.get(self.lcx.sema, name) {
            Some(SymbolKind::Var(local)) => HirCallee::Local(local),
            Some(SymbolKind::Prop { owner, member }) => HirCallee::Member {
                base: owner,
                member,
            },
            Some(
                SymbolKind::Type(def)
                | SymbolKind::Value(def)
                | SymbolKind::Component(def)
                | SymbolKind::Global(def),
            ) => HirCallee::Def(def),
            Some(SymbolKind::Intrinsic(name)) => HirCallee::Intrinsic(name),
            Some(SymbolKind::Module(_)) | None => HirCallee::Unresolved(name),
        }
    }

    // -- blocks and statements ---------------------------------------------

    /// A surface `match` arm's pattern. Only what the grammar produces
    /// today: literal patterns. Anything else is reported and lowered to
    /// [`HirPattern::Error`] (H5) — never guessed at (H4).
    fn lower_match_pattern(&mut self, pattern: &ast::Expr) -> HirPattern {
        match &pattern.kind {
            ast::ExprKind::Bool(value) => HirPattern::Bool(*value),
            ast::ExprKind::Int(value) => HirPattern::Int(*value),
            _ => {
                self.report(
                    pattern.span,
                    ErrorCode::SyntaxError,
                    "only literal patterns are supported in `match` arms for now".to_string(),
                    None,
                );
                HirPattern::Error
            }
        }
    }

    pub(super) fn lower_block(&mut self, block: &ast::Block) -> HirBlock {
        self.push_scope();
        let stmts = block
            .stmts
            .iter()
            .map(|stmt| self.lower_stmt(stmt))
            .collect();
        let tail = block.tail.as_ref().map(|expr| self.lower_expr(expr));
        self.pop_scope();
        HirBlock { stmts, tail }
    }

    /// A block position whose `{` may itself be missing. The hole's diagnostic
    /// exists; an empty block is the lowering (H5).
    fn lower_recovered_block(&mut self, block: &ast::Recovered<ast::Block>) -> HirBlock {
        match block.present() {
            Some(block) => self.lower_block(block),
            None => HirBlock {
                stmts: Vec::new(),
                tail: None,
            },
        }
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> HirStmt {
        match stmt {
            ast::Stmt::Let(stmt) => {
                // Value first: `let x = x + 1` reads the outer `x`.
                let value = self.lower_expr(&stmt.value);
                match stmt.name.present() {
                    Some(ident) => {
                        let ident = ast::Ident {
                            id: ident.id,
                            span: ident.span,
                            name: ident.name,
                        };
                        let annotation = stmt.ty.as_ref().map(|ty| TypeId::new(self.node(ty.id)));
                        let local = self.define(&ident, annotation);
                        HirStmt::Let {
                            hir_id: self.alloc(stmt.id),
                            local,
                            value,
                        }
                    }
                    // The name is a hole (reported). Keep the value — dropping
                    // it would be the silently-discarded subtree S5 forbids.
                    None => HirStmt::Expr(value),
                }
            }
            ast::Stmt::If(stmt) => {
                // `if c { … } else { … }`  →  `match c { true -> …, false -> … }`
                let scrutinee = self.lower_expr(&stmt.condition);
                let then_block = self.lower_recovered_block(&stmt.then_branch);
                let else_block = stmt
                    .else_branch
                    .as_ref()
                    .map(|block| self.lower_recovered_block(block))
                    .unwrap_or(HirBlock {
                        stmts: Vec::new(),
                        tail: None,
                    });
                let arms = vec![
                    HirMatchArm {
                        hir_id: self.invent(stmt.id),
                        pattern: HirPattern::Bool(true),
                        value: HirExpr {
                            hir_id: self.invent(stmt.id),
                            kind: HirExprKind::Block(Box::new(then_block)),
                        },
                    },
                    HirMatchArm {
                        hir_id: self.invent(stmt.id),
                        pattern: HirPattern::Bool(false),
                        value: HirExpr {
                            hir_id: self.invent(stmt.id),
                            kind: HirExprKind::Block(Box::new(else_block)),
                        },
                    },
                ];
                HirStmt::Expr(HirExpr {
                    hir_id: self.alloc(stmt.id),
                    kind: HirExprKind::Match(Box::new(HirMatch { scrutinee, arms })),
                })
            }
            ast::Stmt::For(stmt) => self.lower_for_stmt(stmt),
            ast::Stmt::Return(stmt) => HirStmt::Return {
                hir_id: self.alloc(stmt.id),
                value: stmt.value.as_ref().map(|expr| self.lower_expr(expr)),
            },
            ast::Stmt::Assign(stmt) => self.lower_assign(stmt),
            ast::Stmt::Expr(stmt) => HirStmt::Expr(self.lower_expr(&stmt.expr)),
            ast::Stmt::Error { id, .. } => HirStmt::Error {
                hir_id: self.alloc(*id),
            },
        }
    }

    fn lower_for_stmt(&mut self, stmt: &ast::ForNode) -> HirStmt {
        let iterable = self.lower_expr(&stmt.iterable);
        self.push_scope();
        let binder = match stmt.item.present() {
            Some(ident) => self.define(ident, None),
            None => self.define_missing_binder(stmt.id),
        };
        let body = match &stmt.body {
            ast::ForBody::Statements(block) => self.lower_recovered_block(block),
            // A UI-bodied `for` cannot appear in statement position by
            // construction — the parser picks the body form from the position.
            ast::ForBody::Nodes(_) => unreachable!("statement `for` with a UI body"),
        };
        self.pop_scope();
        HirStmt::For {
            hir_id: self.alloc(stmt.id),
            binder,
            iterable,
            body,
        }
    }

    /// `x += e` → `x = x + e`. The rebuilt left-hand side is a second reading
    /// of syntax that already has its primary node, so it is lowered in
    /// synthetic mode.
    fn lower_assign(&mut self, stmt: &ast::AssignStmt) -> HirStmt {
        let target = self.lower_expr(&stmt.target);
        let value = match assign_op(stmt.op) {
            None => self.lower_expr(&stmt.value),
            Some(op) => {
                self.synthetic += 1;
                let lhs = self.lower_expr(&stmt.target);
                self.synthetic -= 1;
                let rhs = self.lower_expr(&stmt.value);
                HirExpr {
                    hir_id: self.invent(stmt.id),
                    kind: HirExprKind::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                }
            }
        };
        HirStmt::Assign {
            hir_id: self.alloc(stmt.id),
            target,
            value,
        }
    }

    // -- expressions --------------------------------------------------------

    pub(super) fn lower_expr(&mut self, expr: &ast::Expr) -> HirExpr {
        let hir_id = self.alloc(expr.id);
        let kind = match &expr.kind {
            ast::ExprKind::Int(value) => HirExprKind::Literal(HirLiteral::Int(*value)),
            ast::ExprKind::Float(value) => HirExprKind::Literal(HirLiteral::Float(*value)),
            ast::ExprKind::Bool(value) => HirExprKind::Literal(HirLiteral::Bool(*value)),
            ast::ExprKind::Char(value) => HirExprKind::Literal(HirLiteral::Char(*value)),
            ast::ExprKind::String(value) => HirExprKind::Literal(HirLiteral::String(*value)),
            ast::ExprKind::Unit { value, suffix } => {
                HirExprKind::Literal(HirLiteral::Unit(*value, *suffix))
            }
            ast::ExprKind::Color(hex) => self.lower_color(expr.id, *hex),
            ast::ExprKind::Interpolation(parts) => HirExprKind::Interpolation(
                parts
                    .iter()
                    .map(|part| match part {
                        ast::InterpolationPart::Literal(text) => {
                            HirInterpolationPart::Literal(*text)
                        }
                        ast::InterpolationPart::Expr(expr) => {
                            HirInterpolationPart::Expr(self.lower_expr(expr))
                        }
                    })
                    .collect(),
            ),
            ast::ExprKind::List(items) => {
                HirExprKind::List(items.iter().map(|item| self.lower_expr(item)).collect())
            }
            ast::ExprKind::Tuple(items) => {
                HirExprKind::Tuple(items.iter().map(|item| self.lower_expr(item)).collect())
            }
            // The literal's type name (if written) is carried by the AST and
            // resolved by stage 4 — type-directed, like every other type
            // position. The fields lower now.
            ast::ExprKind::Record { name: _, fields } => HirExprKind::Record {
                fields: fields
                    .iter()
                    .filter_map(|field| {
                        // A `Missing` entry is a reported hole (H5's diagnostic
                        // arm); a present field with a hole for a name keeps
                        // its value for the same S5 reason `let` does.
                        let field = field.present()?;
                        let name = field.name.present()?.name;
                        Some(HirFieldInit {
                            hir_id: self.alloc(field.id),
                            name,
                            value: self.lower_expr(&field.value),
                        })
                    })
                    .collect(),
            },
            ast::ExprKind::Closure(closure) => {
                HirExprKind::Closure(Box::new(self.lower_closure(closure)))
            }
            ast::ExprKind::Ident(name) => self.resolve(*name),
            ast::ExprKind::Unary { op, operand } => HirExprKind::Unary {
                op: unary_op(*op),
                operand: Box::new(self.lower_expr(operand)),
            },
            ast::ExprKind::Binary { op, lhs, rhs } => HirExprKind::Binary {
                op: binary_op(*op),
                lhs: Box::new(self.lower_expr(lhs)),
                rhs: Box::new(self.lower_expr(rhs)),
            },
            ast::ExprKind::Ternary {
                condition,
                then_expr,
                else_expr,
            } => {
                // `c ? a : b` → `match c { true -> a, false -> b }` (§9).
                let scrutinee = self.lower_expr(condition);
                let arms = vec![
                    HirMatchArm {
                        hir_id: self.invent(expr.id),
                        pattern: HirPattern::Bool(true),
                        value: self.lower_expr(then_expr),
                    },
                    HirMatchArm {
                        hir_id: self.invent(expr.id),
                        pattern: HirPattern::Bool(false),
                        value: self.lower_expr(else_expr),
                    },
                ];
                HirExprKind::Match(Box::new(HirMatch { scrutinee, arms }))
            }
            ast::ExprKind::Range {
                start,
                end,
                inclusive,
            } => HirExprKind::Range {
                start: Box::new(self.lower_expr(start)),
                end: Box::new(self.lower_expr(end)),
                inclusive: *inclusive,
            },
            ast::ExprKind::Call { callee, args } => {
                let callee = self.resolve_callee(callee.name);
                let args = args.iter().map(|arg| self.lower_expr(arg)).collect();
                HirExprKind::Call { callee, args }
            }
            ast::ExprKind::PathCall { base, member, args } => {
                return self.lower_path_call(hir_id, base, member, args);
            }
            ast::ExprKind::Member { base, member } => match member.present() {
                Some(member) => {
                    // `List.something` where `List` is an included module: the
                    // whole path is one resolution, not a field access.
                    if let ast::ExprKind::Ident(name) = &base.kind
                        && self.lookup_local(*name).is_none()
                        && let Some(module) = self.lookup_module(*name)
                    {
                        let kind = self
                            .lcx
                            .sema
                            .module_member(module, member.name)
                            .map(HirExprKind::Def)
                            .unwrap_or(HirExprKind::Unresolved(member.name));
                        return HirExpr { hir_id, kind };
                    }
                    HirExprKind::Field {
                        base: Box::new(self.lower_expr(base)),
                        field: member.name,
                    }
                }
                // `x.` with the member missing: reported; keep the base.
                None => return self.lower_expr(base),
            },
            ast::ExprKind::OptionalMember { base, member } => match member.present() {
                Some(member) => HirExprKind::OptionalField {
                    base: Box::new(self.lower_expr(base)),
                    field: member.name,
                },
                None => return self.lower_expr(base),
            },
            ast::ExprKind::Index { base, index } => HirExprKind::Index {
                base: Box::new(self.lower_expr(base)),
                index: Box::new(self.lower_expr(index)),
            },
            ast::ExprKind::Match(match_expr) => {
                let scrutinee = self.lower_expr(&match_expr.scrutinee);
                let arms = match_expr
                    .arms
                    .iter()
                    .map(|arm| {
                        let pattern = self.lower_match_pattern(&arm.pattern);
                        let value = match &arm.body {
                            ast::MatchArmBody::Expr(expr) => self.lower_expr(expr),
                            ast::MatchArmBody::Block(block) => {
                                let block = self.lower_block(block);
                                HirExpr {
                                    hir_id: self.invent(arm.id),
                                    kind: HirExprKind::Block(Box::new(block)),
                                }
                            }
                            // `pattern -> place = value`: a one-statement
                            // block, the same shape the statement-`if`
                            // desugar produces for its branches.
                            ast::MatchArmBody::Assign(assign) => {
                                let target = self.lower_expr(&assign.target);
                                let value = self.lower_expr(&assign.value);
                                let stmt = HirStmt::Assign {
                                    hir_id: self.alloc(assign.id),
                                    target,
                                    value,
                                };
                                HirExpr {
                                    hir_id: self.invent(arm.id),
                                    kind: HirExprKind::Block(Box::new(HirBlock {
                                        stmts: vec![stmt],
                                        tail: None,
                                    })),
                                }
                            }
                        };
                        HirMatchArm {
                            hir_id: self.alloc(arm.id),
                            pattern,
                            value,
                        }
                    })
                    .collect();
                HirExprKind::Match(Box::new(HirMatch { scrutinee, arms }))
            }
            ast::ExprKind::Error => HirExprKind::Error,
        };
        HirExpr { hir_id, kind }
    }

    pub(super) fn lower_closure(&mut self, closure: &ast::ClosureExpr) -> HirClosure {
        let hir_id = self.alloc(closure.id);
        self.push_scope();
        let params = closure
            .params
            .iter()
            .filter_map(|param| {
                let param = param.present()?;
                let ident = param.name.present()?;
                let annotation = param.ty.as_ref().map(|ty| TypeId::new(self.node(ty.id)));
                Some(self.define(ident, annotation))
            })
            .collect();
        let block = self.lower_block(&closure.body);
        self.pop_scope();
        HirClosure {
            hir_id,
            params,
            block,
        }
    }

    /// `#ff0000` → `Color.rgba((r, g, b, a))` — the frozen desugaring, against
    /// the `Color` lang item. Everything but the call node is invented, so it
    /// is synthesized from the literal.
    fn lower_color(&mut self, origin: yelc_syntax::NodeId, hex: Name) -> HirExprKind {
        let (r, g, b, a) = parse_color_hex(&self.lcx.sema.names.str(hex));
        let color = self.lcx.sema.known().get(Known::Color);
        let rgba = self.lcx.sema.names.intern("rgba");
        let channels: Vec<HirExpr> = [r, g, b, a]
            .into_iter()
            .map(|channel| HirExpr {
                hir_id: self.invent(origin),
                kind: HirExprKind::Literal(HirLiteral::Int(channel as i64)),
            })
            .collect();
        let tuple = HirExpr {
            hir_id: self.invent(origin),
            kind: HirExprKind::Tuple(channels),
        };
        HirExprKind::Call {
            callee: HirCallee::Member {
                base: color,
                member: rgba,
            },
            args: vec![tuple],
        }
    }

    /// `base.member(args)`: a member call when `base` names a type or global —
    /// a case constructor, a global's callback — and **pure UFCS** otherwise:
    /// `x.f(a)` is `f(x, a)`, the receiver becoming the first argument
    /// (`plans/modules.md` §8; `MethodCall` deliberately does not exist).
    fn lower_path_call(
        &mut self,
        hir_id: HirId,
        base: &ast::Expr,
        member: &ast::MaybeIdent,
        args: &[ast::Expr],
    ) -> HirExpr {
        let Some(member) = member.present() else {
            // `x.(…)` — the member is a hole, reported. Keep the base and the
            // arguments; there is no call to build.
            let mut kept = vec![self.lower_expr(base)];
            kept.extend(args.iter().map(|arg| self.lower_expr(arg)));
            return HirExpr {
                hir_id,
                kind: HirExprKind::List(kept),
            };
        };

        // A namespace-shaped base: a bare name that resolves to a type, global
        // or component definition (locals shadow — `Color` the local beats
        // `Color` the record, same as any scope). A **module** base resolves
        // one level further: `List.empty(…)` looks `empty` up in the module's
        // own scope and the callee is the foreign definition itself.
        // One symtable walk decides the base's meaning (a local shadowing a
        // namespace name wins, because `Var` sits in an inner level).
        if let ast::ExprKind::Ident(name) = &base.kind {
            match self.symtable.get(self.lcx.sema, *name) {
                Some(SymbolKind::Module(module)) => {
                    let member_def = self.lcx.sema.module_member(module, member.name);
                    let args = args.iter().map(|arg| self.lower_expr(arg)).collect();
                    let callee = match member_def {
                        Some(def) => HirCallee::Def(def),
                        // The module resolved, its member did not (H4).
                        None => HirCallee::Unresolved(member.name),
                    };
                    return HirExpr {
                        hir_id,
                        kind: HirExprKind::Call { callee, args },
                    };
                }
                Some(
                    SymbolKind::Type(def) | SymbolKind::Global(def) | SymbolKind::Component(def),
                ) => {
                    // The base ident is consumed by the resolution — its node
                    // maps through the call.
                    let args = args.iter().map(|arg| self.lower_expr(arg)).collect();
                    return HirExpr {
                        hir_id,
                        kind: HirExprKind::Call {
                            callee: HirCallee::Member {
                                base: def,
                                member: member.name,
                            },
                            args,
                        },
                    };
                }
                Some(
                    SymbolKind::Var(_)
                    | SymbolKind::Value(_)
                    | SymbolKind::Prop { .. }
                    | SymbolKind::Intrinsic(_),
                )
                | None => {}
            }
        }

        // UFCS: the receiver is the first argument.
        let mut ufcs_args = vec![self.lower_expr(base)];
        ufcs_args.extend(args.iter().map(|arg| self.lower_expr(arg)));
        HirExpr {
            hir_id,
            kind: HirExprKind::Call {
                callee: self.resolve_callee(member.name),
                args: ufcs_args,
            },
        }
    }

    pub(super) fn report(
        &mut self,
        span: Span,
        code: ErrorCode,
        message: String,
        note: Option<String>,
    ) {
        let mut diagnostic = Diagnostic::error(message).with_span(span).with_code(code);
        if let Some(note) = note {
            diagnostic = diagnostic.with_note(note);
        }
        self.lcx.sema.diagnostics.push(diagnostic);
    }
}

fn assign_op(op: ast::AssignOp) -> Option<BinaryOp> {
    match op {
        ast::AssignOp::Assign => None,
        ast::AssignOp::Add => Some(BinaryOp::Add),
        ast::AssignOp::Sub => Some(BinaryOp::Sub),
        ast::AssignOp::Mul => Some(BinaryOp::Mul),
        ast::AssignOp::Div => Some(BinaryOp::Div),
    }
}

fn binary_op(op: ast::BinaryOp) -> BinaryOp {
    match op {
        ast::BinaryOp::Add => BinaryOp::Add,
        ast::BinaryOp::Sub => BinaryOp::Sub,
        ast::BinaryOp::Mul => BinaryOp::Mul,
        ast::BinaryOp::Div => BinaryOp::Div,
        ast::BinaryOp::Mod => BinaryOp::Mod,
        ast::BinaryOp::Eq => BinaryOp::Eq,
        ast::BinaryOp::Ne => BinaryOp::Ne,
        ast::BinaryOp::Lt => BinaryOp::Lt,
        ast::BinaryOp::Le => BinaryOp::Le,
        ast::BinaryOp::Gt => BinaryOp::Gt,
        ast::BinaryOp::Ge => BinaryOp::Ge,
        ast::BinaryOp::And => BinaryOp::And,
        ast::BinaryOp::Or => BinaryOp::Or,
        ast::BinaryOp::BitAnd => BinaryOp::BitAnd,
        ast::BinaryOp::BitOr => BinaryOp::BitOr,
        ast::BinaryOp::Shl => BinaryOp::Shl,
        ast::BinaryOp::Shr => BinaryOp::Shr,
    }
}

fn unary_op(op: ast::UnaryOp) -> UnaryOp {
    match op {
        ast::UnaryOp::Neg => UnaryOp::Neg,
        ast::UnaryOp::Not => UnaryOp::Not,
    }
}

/// `#rrggbb` / `#rrggbbaa` → channel bytes. The frozen parser's rule,
/// including its answer for malformed input (transparent black) — the token
/// shape was already validated by the lexer, so the fallback arm is for
/// truncated hex only.
fn parse_color_hex(hex: &str) -> (u8, u8, u8, u8) {
    if hex.len() < 7 {
        return (0, 0, 0, 0);
    }
    let r = u8::from_str_radix(&hex[1..3], 16).unwrap_or(0);
    let g = u8::from_str_radix(&hex[3..5], 16).unwrap_or(0);
    let b = u8::from_str_radix(&hex[5..7], 16).unwrap_or(0);
    let a = if hex.len() >= 9 {
        u8::from_str_radix(&hex[7..9], 16).unwrap_or(255)
    } else {
        255
    };
    (r, g, b, a)
}
