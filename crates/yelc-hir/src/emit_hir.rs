//! `--emit-hir`: the desugared, name-resolved IR, yel-flavoured.
//!
//! Like rustc's MIR dump: it *looks like* the language and is deliberately
//! **not** round-trippable — HIR is post-desugaring, and the round-trip need
//! is already met one layer up by `--emit-green-text` (S1). What this dump is
//! for is everything source cannot show: which `DefId` a name bound to, which
//! declared type a member resolved to, what the desugarings did.
//!
//! Notation:
//! - `count#12` — a resolved definition, `DefId` index attached
//! - `prop(count)` / `local(2)` — an enclosing-member / body-local reference
//! - `?name` — a name that resolved to nothing (H4, stage 4 reports)
//! - `builtin(len)` — a builtin overload set, row picked in stage 4
//!
//! The dump is output, so A6 applies: everything printed iterates
//! registration, item or arena order — never a hash map.

use std::fmt::Write;

use crate::{
    HirBlock, HirBody, HirCallee, HirExpr, HirExprKind, HirInterpolationPart, HirItem, HirLiteral,
    HirMatchArm, HirModule, HirPattern, HirStmt,
};
use yelc_sema::definitions::MemberKind;
use yelc_sema::{CompilerContext, DefId, Ty, TyKind};

pub fn hir(module: &HirModule, context: &CompilerContext) -> String {
    let mut out = String::new();
    let printer = Printer { module, context };

    // The definition table first — phase 1 and 2's output, the part the
    // differential pins. Registration order.
    for definition in context.defs.iter() {
        if let Some(doc) = module.docs.get(&definition.id) {
            for line in context.names.str(*doc).lines() {
                let _ = writeln!(out, "/// {line}");
            }
        }
        printer.definition(&mut out, definition);
    }

    // Then the items, in item order (D5: globals before components).
    for (_, item) in module.items.iter_enumerated() {
        out.push('\n');
        match item {
            HirItem::Global(global) => printer.global(&mut out, global),
            HirItem::Component(component) => printer.component(&mut out, component),
        }
    }
    out
}

struct Printer<'a> {
    module: &'a HirModule,
    context: &'a CompilerContext,
}

impl Printer<'_> {
    fn name(&self, name: yelc_base::Name) -> String {
        self.context.names.str(name).to_string()
    }

    fn def(&self, def: DefId) -> String {
        // `definition`, not `defs.get`: an included package's ids render as
        // `name#pkgN.index` so a foreign resolution is visibly foreign.
        match self.context.definition(def) {
            Some(definition) if def.package == self.context.defs.package() => {
                format!("{}#{}", self.name(definition.name), def.index)
            }
            Some(definition) => format!(
                "{}#pkg{}.{}",
                self.name(definition.name),
                def.package.0,
                def.index
            ),
            None => format!("?#pkg{}.{}", def.package.0, def.index),
        }
    }

    fn ty(&self, ty: Ty) -> String {
        render_ty(self.context, ty)
    }
}

/// A type, rendered the dump's way — public because the LSP's hovers want the
/// same spelling.
pub fn render_ty(context: &CompilerContext, ty: Ty) -> String {
    struct Renderer<'a> {
        context: &'a CompilerContext,
    }
    impl Renderer<'_> {
        fn def(&self, def: DefId) -> String {
            match self.context.definition(def) {
                Some(definition) => self.context.names.str(definition.name).to_string(),
                None => format!("?#pkg{}.{}", def.package.0, def.index),
            }
        }
        fn ty(&self, ty: Ty) -> String {
            match self.context.types.kind(ty) {
                TyKind::Bool => "bool".into(),
                TyKind::S8 => "s8".into(),
                TyKind::S16 => "s16".into(),
                TyKind::S32 => "s32".into(),
                TyKind::S64 => "s64".into(),
                TyKind::U8 => "u8".into(),
                TyKind::U16 => "u16".into(),
                TyKind::U32 => "u32".into(),
                TyKind::U64 => "u64".into(),
                TyKind::F32 => "f32".into(),
                TyKind::F64 => "f64".into(),
                TyKind::Char => "char".into(),
                TyKind::String => "string".into(),
                TyKind::List(inner) => format!("list<{}>", self.ty(inner)),
                TyKind::Option(inner) => format!("option<{}>", self.ty(inner)),
                TyKind::Result { ok, err } => match (ok, err) {
                    (None, None) => "result".into(),
                    (Some(ok), None) => format!("result<{}>", self.ty(ok)),
                    (Some(ok), Some(err)) => format!("result<{}, {}>", self.ty(ok), self.ty(err)),
                    (None, Some(err)) => format!("result<_, {}>", self.ty(err)),
                },
                TyKind::Tuple(elements) => {
                    let elements: Vec<String> =
                        elements.iter().map(|element| self.ty(*element)).collect();
                    format!("tuple<{}>", elements.join(", "))
                }
                TyKind::Adt(def) => self.def(def),
                TyKind::Func { params, ret } => {
                    let params: Vec<String> = params.iter().map(|param| self.ty(*param)).collect();
                    match ret {
                        Some(ret) => format!("func({}) -> {}", params.join(", "), self.ty(ret)),
                        None => format!("func({})", params.join(", ")),
                    }
                }
                TyKind::Param(index) => format!("param({index})"),
                TyKind::Infer(index) => format!("infer({index})"),
                TyKind::Error => "{error}".into(),
                TyKind::Unit => "unit".into(),
            }
        }
    }
    Renderer { context }.ty(ty)
}

impl Printer<'_> {
    fn definition(&self, out: &mut String, definition: &yelc_sema::Definition) {
        let kind = match definition.kind {
            yelc_sema::DefKind::Type => "type",
            yelc_sema::DefKind::Value => "value",
            yelc_sema::DefKind::Component => "component",
            yelc_sema::DefKind::Global => "global",
        };
        let ty = definition
            .ty
            .map(|ty| format!(": {}", self.ty(ty)))
            .unwrap_or_default();
        let export = if definition.is_export { " export" } else { "" };
        let _ = writeln!(
            out,
            "def {}{} {}{}",
            kind,
            export,
            self.def(definition.id),
            ty
        );
        for member in self.context.defs.members(definition.id) {
            let kind = match member.kind {
                MemberKind::Field => "field",
                MemberKind::Case => "case",
                MemberKind::Property { .. } => "prop",
                MemberKind::Function => "func",
            };
            let ty = member
                .ty
                .map(|ty| format!(": {}", self.ty(ty)))
                .unwrap_or_else(|| ": ?".into());
            let _ = writeln!(out, "  {} {}{}", kind, self.name(member.name), ty);
        }
    }

    fn global(&self, out: &mut String, global: &crate::HirGlobal) {
        let export = if global.is_export { "export " } else { "" };
        let _ = writeln!(out, "{}global {} {{", export, self.def(global.def));
        self.item_bodies(out, &global.defaults, &global.functions, global.def);
        let _ = writeln!(out, "}}");
    }

    fn component(&self, out: &mut String, component: &crate::HirComponent) {
        let export = if component.is_export { "export " } else { "" };
        let _ = writeln!(out, "{}component {} {{", export, self.def(component.def));
        self.item_bodies(
            out,
            &component.defaults,
            &component.functions,
            component.def,
        );
        let build = &self.module.bodies[component.build];
        let _ = writeln!(out, "  build:");
        self.deps(out, build, 2);
        self.block(out, &build.block, 2);
        let _ = writeln!(out, "}}");
    }

    /// One body's dependency line, when it has one: `deps: reads(a, b) writes(c)`.
    fn deps(&self, out: &mut String, body: &HirBody, depth: usize) {
        let Some(dependencies) = self.module.dependencies.get(body.hir_id) else {
            return;
        };
        let render = |refs: &[crate::StateRef]| {
            let rendered: Vec<String> = refs
                .iter()
                .map(|state| format!("{}.{}", self.def(state.owner), self.name(state.member)))
                .collect();
            rendered.join(", ")
        };
        let pad = "  ".repeat(depth);
        let mut parts = Vec::new();
        if !dependencies.reads.is_empty() {
            parts.push(format!("reads({})", render(&dependencies.reads)));
        }
        if !dependencies.writes.is_empty() {
            parts.push(format!("writes({})", render(&dependencies.writes)));
        }
        let _ = writeln!(out, "{pad}deps: {}", parts.join(" "));
    }

    fn item_bodies(
        &self,
        out: &mut String,
        defaults: &[crate::HirDefault],
        functions: &[crate::HirFunction],
        owner: DefId,
    ) {
        for default in defaults {
            let member = &self.context.defs.members(owner)[default.member as usize];
            let body = &self.module.bodies[default.body];
            let _ = writeln!(out, "  default {}:", self.name(member.name));
            self.block(out, &body.block, 2);
        }
        for function in functions {
            match function.body {
                Some(body) => {
                    let body = &self.module.bodies[body];
                    let _ = writeln!(
                        out,
                        "  func {}({}):",
                        self.name(function.name),
                        self.params(body)
                    );
                    self.deps(out, body, 2);
                    self.block(out, &body.block, 2);
                }
                None => {
                    let _ = writeln!(out, "  extern func {};", self.name(function.name));
                }
            }
        }
    }

    fn params(&self, body: &HirBody) -> String {
        let params: Vec<String> = body
            .locals
            .iter_enumerated()
            .take(body.params as usize)
            .map(|(_, local)| self.name(local.name))
            .collect();
        params.join(", ")
    }

    fn block(&self, out: &mut String, block: &HirBlock, depth: usize) {
        let pad = "  ".repeat(depth);
        for stmt in &block.stmts {
            match stmt {
                HirStmt::Let { local, value, .. } => {
                    let _ = writeln!(out, "{pad}let local({}) = {};", local.0, self.expr(value));
                }
                HirStmt::Assign { target, value, .. } => {
                    let _ = writeln!(out, "{pad}{} = {};", self.expr(target), self.expr(value));
                }
                HirStmt::Expr(expr) => {
                    let _ = writeln!(out, "{pad}{};", self.expr(expr));
                }
                HirStmt::Return { value, .. } => match value {
                    Some(value) => {
                        let _ = writeln!(out, "{pad}return {};", self.expr(value));
                    }
                    None => {
                        let _ = writeln!(out, "{pad}return;");
                    }
                },
                HirStmt::For {
                    binder,
                    iterable,
                    body,
                    ..
                } => {
                    let _ = writeln!(
                        out,
                        "{pad}for local({}) in {} {{",
                        binder.0,
                        self.expr(iterable)
                    );
                    self.block(out, body, depth + 1);
                    let _ = writeln!(out, "{pad}}}");
                }
                HirStmt::Error { .. } => {
                    let _ = writeln!(out, "{pad}{{error}};");
                }
            }
        }
        if let Some(tail) = &block.tail {
            let _ = writeln!(out, "{pad}{}", self.expr(tail));
        }
    }

    fn expr(&self, expr: &HirExpr) -> String {
        match &expr.kind {
            HirExprKind::Local(local) => format!("local({})", local.0),
            HirExprKind::Def(def) => self.def(*def),
            HirExprKind::Prop { member, .. } => format!("prop({})", self.name(*member)),
            HirExprKind::Intrinsic(name) => format!("builtin({})", self.name(*name)),
            HirExprKind::Unresolved(name) => format!("?{}", self.name(*name)),
            HirExprKind::Literal(literal) => self.literal(literal),
            HirExprKind::List(items) => format!("[{}]", self.exprs(items)),
            HirExprKind::Tuple(items) => format!("({})", self.exprs(items)),
            HirExprKind::Record { fields } => {
                let fields: Vec<String> = fields
                    .iter()
                    .map(|field| format!("{}: {}", self.name(field.name), self.expr(&field.value)))
                    .collect();
                format!("{{ {} }}", fields.join(", "))
            }
            HirExprKind::Field { base, field } => {
                format!("{}.{}", self.expr(base), self.name(*field))
            }
            HirExprKind::OptionalField { base, field } => {
                format!("{}?.{}", self.expr(base), self.name(*field))
            }
            HirExprKind::Index { base, index } => {
                format!("{}[{}]", self.expr(base), self.expr(index))
            }
            HirExprKind::Unary { op, operand } => {
                let op = match op {
                    crate::UnaryOp::Neg => "-",
                    crate::UnaryOp::Not => "!",
                };
                format!("{}{}", op, self.expr(operand))
            }
            HirExprKind::Binary { op, lhs, rhs } => {
                format!("({} {} {})", self.expr(lhs), binary(*op), self.expr(rhs))
            }
            HirExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                let dots = if *inclusive { "..=" } else { ".." };
                format!("{}{}{}", self.expr(start), dots, self.expr(end))
            }
            HirExprKind::Call { callee, args } => {
                format!("{}({})", self.callee(callee), self.exprs(args))
            }
            HirExprKind::Closure(closure) => {
                let params: Vec<String> = closure
                    .params
                    .iter()
                    .map(|param| format!("local({})", param.0))
                    .collect();
                let mut body = String::new();
                self.block(&mut body, &closure.block, 0);
                format!(
                    "closure({}) {{ {} }}",
                    params.join(", "),
                    body.trim_end().replace('\n', " ")
                )
            }
            HirExprKind::Interpolation(parts) => {
                let parts: Vec<String> = parts
                    .iter()
                    .map(|part| match part {
                        HirInterpolationPart::Literal(text) => {
                            format!("{:?}", self.name(*text))
                        }
                        HirInterpolationPart::Expr(expr) => format!("{{{}}}", self.expr(expr)),
                    })
                    .collect();
                format!("interp({})", parts.join(" "))
            }
            HirExprKind::Match(node) => {
                let arms: Vec<String> = node.arms.iter().map(|arm| self.arm(arm)).collect();
                format!(
                    "match {} {{ {} }}",
                    self.expr(&node.scrutinee),
                    arms.join(", ")
                )
            }
            HirExprKind::Block(block) => {
                let mut body = String::new();
                self.block(&mut body, block, 0);
                format!("{{ {} }}", body.trim_end().replace('\n', "; "))
            }
            HirExprKind::Instantiate(node) => {
                let props: Vec<String> = node
                    .props
                    .iter()
                    .map(|prop| {
                        let getter = prop
                            .getter
                            .as_ref()
                            .map(|getter| self.expr(getter))
                            .unwrap_or_else(|| "_".into());
                        match &prop.setter {
                            Some(setter)
                                if setter.block.stmts.is_empty() && setter.block.tail.is_none() =>
                            {
                                format!("bind {}: {}", self.name(prop.name), getter)
                            }
                            Some(_) => format!("{}: {} + set", self.name(prop.name), getter),
                            None => format!("{}: {}", self.name(prop.name), getter),
                        }
                    })
                    .collect();
                let children: Vec<String> =
                    node.children.iter().map(|child| self.expr(child)).collect();
                let mut parts = props;
                parts.extend(children);
                format!("{}{{ {} }}", self.callee(&node.target), parts.join(", "))
            }
            HirExprKind::UiText(content) => format!("text({})", self.expr(content)),
            HirExprKind::Repeat(node) => {
                let key = node
                    .key
                    .as_ref()
                    .map(|key| format!(" key({})", self.expr(key)))
                    .unwrap_or_default();
                format!(
                    "repeat local({}) in {}{} {{ {} }}",
                    node.binder.0,
                    self.expr(&node.iterable),
                    key,
                    self.exprs(&node.children)
                )
            }
            HirExprKind::Fragment(children) => format!("fragment({})", self.exprs(children)),
            HirExprKind::ChildrenPlaceholder => "@children".into(),
            HirExprKind::Error => "{error}".into(),
        }
    }

    fn arm(&self, arm: &HirMatchArm) -> String {
        let pattern = match arm.pattern {
            HirPattern::Bool(value) => value.to_string(),
        };
        format!("{pattern} -> {}", self.expr(&arm.value))
    }

    fn callee(&self, callee: &HirCallee) -> String {
        match callee {
            HirCallee::Local(local) => format!("local({})", local.0),
            HirCallee::Def(def) => self.def(*def),
            HirCallee::Intrinsic(name) => format!("builtin({})", self.name(*name)),
            HirCallee::Member { base, member } => {
                format!("{}.{}", self.def(*base), self.name(*member))
            }
            HirCallee::Unresolved(name) => format!("?{}", self.name(*name)),
        }
    }

    fn exprs(&self, exprs: &[HirExpr]) -> String {
        let rendered: Vec<String> = exprs.iter().map(|expr| self.expr(expr)).collect();
        rendered.join(", ")
    }

    fn literal(&self, literal: &HirLiteral) -> String {
        match literal {
            HirLiteral::Int(value) => value.to_string(),
            HirLiteral::Float(value) => format!("{value:?}"),
            HirLiteral::Bool(value) => value.to_string(),
            HirLiteral::Char(value) => format!("{value:?}"),
            HirLiteral::String(value) => format!("{:?}", self.name(*value)),
            HirLiteral::Unit(value, suffix) => format!("{value}{}", self.name(*suffix)),
        }
    }
}

fn binary(op: crate::BinaryOp) -> &'static str {
    use crate::BinaryOp as B;
    match op {
        B::Add => "+",
        B::Sub => "-",
        B::Mul => "*",
        B::Div => "/",
        B::Mod => "%",
        B::Eq => "==",
        B::Ne => "!=",
        B::Lt => "<",
        B::Le => "<=",
        B::Gt => ">",
        B::Ge => ">=",
        B::And => "&&",
        B::Or => "||",
        B::BitAnd => "&",
        B::BitOr => "|",
        B::Shl => "<<",
        B::Shr => ">>",
    }
}
