//! yel-smith: Random Yel source code generator for fuzzing
//!
//! This crate generates random but **valid** Yel source files using the `arbitrary` crate.
//! The key insight from wasm-smith is that generated code must be semantically valid,
//! not just syntactically correct. This allows fuzzers to bypass parsing and exercise
//! deeper parts of the compiler (type checking, code generation, etc.).
//!
//! # Architecture
//!
//! The generator maintains state to ensure validity:
//! - Track defined types (records, enums, variants)
//! - Track defined properties and their types
//! - Track defined callbacks
//! - Generate expressions that type-check against expected types
//!
//! # Usage
//!
//! ```rust,ignore
//! use arbitrary::Unstructured;
//! use yel_smith::YelModule;
//!
//! let data: &[u8] = /* from fuzzer */;
//! let mut u = Unstructured::new(data);
//! let module: YelModule = u.arbitrary()?;
//! let source_code = module.to_source();
//! ```

use arbitrary::{Arbitrary, Result, Unstructured};
use std::collections::BTreeMap;

/// Configuration for the generator.
#[derive(Debug, Clone)]
pub struct Config {
    /// Maximum number of type definitions.
    pub max_types: usize,
    /// Maximum number of components.
    pub max_components: usize,
    /// Maximum number of properties per component.
    pub max_properties: usize,
    /// Maximum number of callbacks per component.
    pub max_callbacks: usize,
    /// Maximum depth of UI node nesting.
    pub max_node_depth: usize,
    /// Maximum number of children per node.
    pub max_children: usize,
    /// Maximum expression depth.
    pub max_expr_depth: usize,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_types: 5,
            max_components: 3,
            max_properties: 10,
            max_callbacks: 5,
            max_node_depth: 4,
            max_children: 5,
            max_expr_depth: 3,
        }
    }
}

/// A generated Yel module (file).
#[derive(Debug)]
pub struct YelModule {
    pub package: Option<PackageId>,
    pub records: Vec<RecordDef>,
    pub enums: Vec<EnumDef>,
    pub variants: Vec<VariantDef>,
    pub components: Vec<ComponentDef>,
}

impl YelModule {
    /// Generate a Yel module with default configuration.
    pub fn arbitrary_with_config(u: &mut Unstructured, config: &Config) -> Result<Self> {
        let mut ctx = GenerationContext::new(config.clone());

        // Generate package declaration (optional)
        let package = if u.arbitrary()? {
            Some(ctx.arbitrary_package_id(u)?)
        } else {
            None
        };

        // Generate type definitions first (so components can use them)
        let num_records: usize = u.int_in_range(0..=config.max_types)?;
        let records: Vec<_> = (0..num_records)
            .map(|_| ctx.arbitrary_record(u))
            .collect::<Result<_>>()?;

        let num_enums: usize = u.int_in_range(0..=config.max_types)?;
        let enums: Vec<_> = (0..num_enums)
            .map(|_| ctx.arbitrary_enum(u))
            .collect::<Result<_>>()?;

        let num_variants: usize = u.int_in_range(0..=config.max_types)?;
        let variants: Vec<_> = (0..num_variants)
            .map(|_| ctx.arbitrary_variant(u))
            .collect::<Result<_>>()?;

        // Generate components (ensure at least one is exported for valid WASM)
        let num_components: usize = u.int_in_range(1..=config.max_components)?;
        let mut components: Vec<_> = (0..num_components)
            .map(|_| ctx.arbitrary_component(u))
            .collect::<Result<_>>()?;

        // Ensure exactly one component is exported (the first one)
        // Note: Multiple exported components with callbacks require additional component model
        // wiring that isn't yet implemented in yel-core
        for (i, c) in components.iter_mut().enumerate() {
            c.is_export = i == 0;
        }

        Ok(YelModule {
            package,
            records,
            enums,
            variants,
            components,
        })
    }

    /// Convert the module to Yel source code.
    pub fn to_source(&self) -> String {
        let mut out = String::new();

        // Package declaration
        if let Some(pkg) = &self.package {
            out.push_str(&format!("package {}:{}@{};\n\n", pkg.namespace, pkg.name, pkg.version));
        }

        // Records
        for record in &self.records {
            out.push_str(&record.to_source());
            out.push_str("\n\n");
        }

        // Enums
        for enum_def in &self.enums {
            out.push_str(&enum_def.to_source());
            out.push_str("\n\n");
        }

        // Variants
        for variant in &self.variants {
            out.push_str(&variant.to_source());
            out.push_str("\n\n");
        }

        // Components
        for component in &self.components {
            out.push_str(&component.to_source());
            out.push_str("\n\n");
        }

        out
    }
}

impl<'a> Arbitrary<'a> for YelModule {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        Self::arbitrary_with_config(u, &Config::default())
    }
}

// ============================================================================
// Type Definitions
// ============================================================================

#[derive(Debug, Clone)]
pub struct PackageId {
    pub namespace: String,
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone)]
pub struct RecordDef {
    pub name: String,
    pub fields: Vec<(String, TypeRef)>,
}

impl RecordDef {
    fn to_source(&self) -> String {
        let fields: Vec<_> = self
            .fields
            .iter()
            .map(|(name, ty)| format!("    {}: {},", name, ty.to_source()))
            .collect();
        format!("record {} {{\n{}\n}}", self.name, fields.join("\n"))
    }
}

#[derive(Debug, Clone)]
pub struct EnumDef {
    pub name: String,
    pub cases: Vec<String>,
}

impl EnumDef {
    fn to_source(&self) -> String {
        let cases: Vec<_> = self.cases.iter().map(|c| format!("    {},", c)).collect();
        format!("enum {} {{\n{}\n}}", self.name, cases.join("\n"))
    }
}

#[derive(Debug, Clone)]
pub struct VariantDef {
    pub name: String,
    pub cases: Vec<VariantCase>,
}

#[derive(Debug, Clone)]
pub struct VariantCase {
    pub name: String,
    pub payload: Option<TypeRef>,
}

impl VariantDef {
    fn to_source(&self) -> String {
        let cases: Vec<_> = self
            .cases
            .iter()
            .map(|c| {
                if let Some(ty) = &c.payload {
                    format!("    {}({}),", c.name, ty.to_source())
                } else {
                    format!("    {},", c.name)
                }
            })
            .collect();
        format!("variant {} {{\n{}\n}}", self.name, cases.join("\n"))
    }
}

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    // Primitives
    Bool,
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    String,
    Char,
    // Compound
    List(Box<TypeRef>),
    Option(Box<TypeRef>),
    Result {
        ok: Option<Box<TypeRef>>,
        err: Option<Box<TypeRef>>,
    },
    Tuple(Vec<TypeRef>),
    // Named (user-defined records, enums, variants)
    Named(String),
    // UI-specific
    Length,
    PhysicalLength,
    Angle,
    Duration,
    Percent,
    Color,
    Brush,
}

impl TypeRef {
    fn to_source(&self) -> String {
        match self {
            TypeRef::Bool => "bool".into(),
            TypeRef::S8 => "s8".into(),
            TypeRef::S16 => "s16".into(),
            TypeRef::S32 => "s32".into(),
            TypeRef::S64 => "s64".into(),
            TypeRef::U8 => "u8".into(),
            TypeRef::U16 => "u16".into(),
            TypeRef::U32 => "u32".into(),
            TypeRef::U64 => "u64".into(),
            TypeRef::F32 => "f32".into(),
            TypeRef::F64 => "f64".into(),
            TypeRef::String => "string".into(),
            TypeRef::Char => "char".into(),
            TypeRef::List(inner) => format!("list<{}>", inner.to_source()),
            TypeRef::Option(inner) => format!("option<{}>", inner.to_source()),
            TypeRef::Result { ok, err } => {
                let ok_str = ok.as_ref().map(|t| t.to_source()).unwrap_or_else(|| "_".into());
                let err_str = err.as_ref().map(|t| t.to_source()).unwrap_or_else(|| "_".into());
                format!("result<{}, {}>", ok_str, err_str)
            }
            TypeRef::Tuple(types) => {
                let inner: Vec<_> = types.iter().map(|t| t.to_source()).collect();
                format!("tuple<{}>", inner.join(", "))
            }
            TypeRef::Named(name) => name.clone(),
            TypeRef::Length => "length".into(),
            TypeRef::PhysicalLength => "physical-length".into(),
            TypeRef::Angle => "angle".into(),
            TypeRef::Duration => "duration".into(),
            TypeRef::Percent => "percent".into(),
            TypeRef::Color => "color".into(),
            TypeRef::Brush => "brush".into(),
        }
    }
}

// ============================================================================
// Components
// ============================================================================

#[derive(Debug, Clone)]
pub struct ComponentDef {
    pub name: String,
    pub is_export: bool,
    pub properties: Vec<PropertyDef>,
    pub callbacks: Vec<CallbackDef>,
    pub body: Vec<Node>,
}

impl ComponentDef {
    fn to_source(&self) -> String {
        let mut out = String::new();

        if self.is_export {
            out.push_str("export ");
        }
        out.push_str(&format!("component {} {{\n", self.name));

        // Properties
        for prop in &self.properties {
            out.push_str(&format!("    {}\n", prop.to_source()));
        }

        // Callbacks
        for cb in &self.callbacks {
            out.push_str(&format!("    {}\n", cb.to_source()));
        }

        // Body
        for node in &self.body {
            out.push_str(&node.to_source(1));
        }

        out.push_str("}\n");
        out
    }
}

#[derive(Debug, Clone)]
pub struct PropertyDef {
    pub name: String,
    pub ty: TypeRef,
    pub default: Option<Expr>,
}

impl PropertyDef {
    fn to_source(&self) -> String {
        let mut s = format!("{}: {}", self.name, self.ty.to_source());
        if let Some(default) = &self.default {
            s.push_str(&format!(" = {}", default.to_source()));
        }
        s.push(';');
        s
    }
}

#[derive(Debug, Clone)]
pub struct CallbackDef {
    pub name: String,
    pub params: Vec<(String, TypeRef)>,
    pub return_ty: Option<TypeRef>,
    pub is_export: bool,
}

impl CallbackDef {
    fn to_source(&self) -> String {
        let params: Vec<_> = self
            .params
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty.to_source()))
            .collect();
        let mut s = String::new();
        if self.is_export {
            s.push_str("export ");
        }
        s.push_str(&format!("{}: func({})", self.name, params.join(", ")));
        if let Some(ret) = &self.return_ty {
            s.push_str(&format!(" -> {}", ret.to_source()));
        }
        s.push(';');
        s
    }
}

// ============================================================================
// UI Nodes
// ============================================================================

#[derive(Debug, Clone)]
pub enum Node {
    Element {
        name: String,
        bindings: Vec<Binding>,
        handlers: Vec<Handler>,
        children: Vec<Node>,
    },
    Text(Expr),
    If {
        condition: Expr,
        then_branch: Vec<Node>,
        else_if_branches: Vec<(Expr, Vec<Node>)>,
        else_branch: Option<Vec<Node>>,
    },
    For {
        item: String,
        iterable: Expr,
        key: Option<Expr>,
        body: Vec<Node>,
    },
}

impl Node {
    fn to_source(&self, indent: usize) -> String {
        let pad = "    ".repeat(indent);
        match self {
            Node::Element {
                name,
                bindings,
                handlers,
                children,
            } => {
                let mut out = format!("{}{} {{\n", pad, name);

                // Collect all element items (bindings, handlers, children)
                let mut items: Vec<String> = Vec::new();

                for binding in bindings {
                    items.push(format!(
                        "{}    {}: {}",
                        pad,
                        binding.name,
                        binding.value.to_source()
                    ));
                }

                for handler in handlers {
                    let stmts: Vec<_> = handler.body.iter().map(|s| s.to_source()).collect();
                    items.push(format!(
                        "{}    {}: {{ {} }}",
                        pad,
                        handler.name,
                        stmts.join(" ")
                    ));
                }

                for child in children {
                    // Get child source without trailing newline
                    let child_src = child.to_source(indent + 1);
                    items.push(child_src.trim_end().to_string());
                }

                // Join items with commas
                out.push_str(&items.join(",\n"));
                if !items.is_empty() {
                    out.push('\n');
                }

                out.push_str(&format!("{}}}\n", pad));
                out
            }
            Node::Text(expr) => {
                // Text nodes are string expressions - wrap in quotes if not already
                match expr {
                    Expr::String(s) => format!("{}\"{}\"\n", pad, s.replace('"', "\\\"")),
                    Expr::Interpolation(parts) => {
                        let mut s = String::new();
                        for part in parts {
                            match part {
                                InterpolationPart::Literal(lit) => s.push_str(lit),
                                InterpolationPart::Expr(e) => s.push_str(&format!("{{{}}}", e.to_source())),
                            }
                        }
                        format!("{}\"{}\"\n", pad, s)
                    }
                    _ => format!("{}\"{{ {} }}\"\n", pad, expr.to_source()),
                }
            }
            Node::If {
                condition,
                then_branch,
                else_if_branches,
                else_branch,
            } => {
                let mut out = format!("{}if {} {{\n", pad, condition.to_source());
                for node in then_branch {
                    out.push_str(&node.to_source(indent + 1));
                }
                out.push_str(&format!("{}}}", pad));

                // else if branches
                for (cond, nodes) in else_if_branches {
                    out.push_str(&format!(" else if {} {{\n", cond.to_source()));
                    for node in nodes {
                        out.push_str(&node.to_source(indent + 1));
                    }
                    out.push_str(&format!("{}}}", pad));
                }

                if let Some(else_nodes) = else_branch {
                    out.push_str(" else {\n");
                    for node in else_nodes {
                        out.push_str(&node.to_source(indent + 1));
                    }
                    out.push_str(&format!("{}}}", pad));
                }
                out.push('\n');
                out
            }
            Node::For {
                item,
                iterable,
                key,
                body,
            } => {
                let mut out = format!("{}for {} in {}", pad, item, iterable.to_source());
                if let Some(k) = key {
                    out.push_str(&format!(" key({})", k.to_source()));
                }
                out.push_str(" {\n");
                for node in body {
                    out.push_str(&node.to_source(indent + 1));
                }
                out.push_str(&format!("{}}}\n", pad));
                out
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Handler {
    pub name: String,
    pub body: Vec<Statement>,
}

// ============================================================================
// Expressions
// ============================================================================

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Color(String),
    Unit(f64, String),
    // Variables
    Var(String),
    // Compound
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Record {
        ty_name: String,
        fields: Vec<(String, Expr)>,
    },
    // Operations
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    // Access
    Field {
        base: Box<Expr>,
        field: String,
    },
    OptionalField {
        base: Box<Expr>,
        field: String,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    // Range expressions
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
    // Calls
    Call {
        func: String,
        args: Vec<Expr>,
    },
    // Variant constructors
    VariantCtor {
        variant: String,
        case: String,
        payload: Option<Box<Expr>>,
    },
    EnumCase {
        enum_name: String,
        case: String,
    },
    // Interpolation (desugars to concat)
    Interpolation(Vec<InterpolationPart>),
    // Typed closure: { param: type -> expr }
    Closure {
        params: Vec<(String, TypeRef)>,
        body: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum InterpolationPart {
    Literal(String),
    Expr(Expr),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl BinaryOp {
    fn to_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    fn to_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        }
    }
}

impl Expr {
    fn to_source(&self) -> String {
        match self {
            Expr::Int(v) => v.to_string(),
            Expr::Float(v) => format!("{:.2}", v),
            Expr::String(s) => format!("\"{}\"", s.replace('"', "\\\"")),
            Expr::Char(c) => format!("'{}'", c),
            Expr::Bool(b) => b.to_string(),
            Expr::Color(c) => c.clone(),
            Expr::Unit(v, u) => format!("{}{}", v, u),
            Expr::Var(name) => name.clone(),
            Expr::List(elements) => {
                let inner: Vec<_> = elements.iter().map(|e| e.to_source()).collect();
                format!("[{}]", inner.join(", "))
            }
            Expr::Tuple(elements) => {
                let inner: Vec<_> = elements.iter().map(|e| e.to_source()).collect();
                format!("({})", inner.join(", "))
            }
            Expr::Record { fields, .. } => {
                let inner: Vec<_> = fields
                    .iter()
                    .map(|(n, e)| format!("{}: {}", n, e.to_source()))
                    .collect();
                format!("{{ {} }}", inner.join(", "))
            }
            Expr::Binary { op, lhs, rhs } => {
                format!("({} {} {})", lhs.to_source(), op.to_str(), rhs.to_source())
            }
            Expr::Unary { op, operand } => {
                format!("({}{})", op.to_str(), operand.to_source())
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                format!(
                    "({} ? {} : {})",
                    cond.to_source(),
                    then_expr.to_source(),
                    else_expr.to_source()
                )
            }
            Expr::Field { base, field } => format!("{}.{}", base.to_source(), field),
            Expr::OptionalField { base, field } => format!("{}?.{}", base.to_source(), field),
            Expr::Index { base, index } => {
                format!("{}[{}]", base.to_source(), index.to_source())
            }
            Expr::Range { start, end, inclusive } => {
                if *inclusive {
                    format!("{}..={}", start.to_source(), end.to_source())
                } else {
                    format!("{}..{}", start.to_source(), end.to_source())
                }
            }
            Expr::Call { func, args } => {
                let args_str: Vec<_> = args.iter().map(|a| a.to_source()).collect();
                format!("{}({})", func, args_str.join(", "))
            }
            Expr::VariantCtor {
                variant,
                case,
                payload,
            } => {
                // Builtin variants (option, result) use just the case name
                // User-defined variants need Variant.case format
                let prefix = if variant == "option" || variant == "result" {
                    String::new()
                } else {
                    format!("{}.", variant)
                };
                if let Some(p) = payload {
                    format!("{}{}({})", prefix, case, p.to_source())
                } else {
                    format!("{}{}", prefix, case)
                }
            }
            Expr::EnumCase { enum_name, case } => format!("{}.{}", enum_name, case),
            Expr::Interpolation(parts) => {
                let mut s = String::from("\"");
                for part in parts {
                    match part {
                        InterpolationPart::Literal(lit) => s.push_str(lit),
                        InterpolationPart::Expr(e) => s.push_str(&format!("{{{}}}", e.to_source())),
                    }
                }
                s.push('"');
                s
            }
            Expr::Closure { params, body } => {
                let params_str: Vec<_> = params
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty.to_source()))
                    .collect();
                format!("{{ {} -> {} }}", params_str.join(", "), body.to_source())
            }
        }
    }
}

// ============================================================================
// Statements
// ============================================================================

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    Assign { target: Expr, value: Expr },
    CompoundAssign { target: Expr, op: CompoundOp, value: Expr },
    If {
        condition: Expr,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum CompoundOp {
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl CompoundOp {
    fn to_str(&self) -> &'static str {
        match self {
            CompoundOp::AddAssign => "+=",
            CompoundOp::SubAssign => "-=",
            CompoundOp::MulAssign => "*=",
            CompoundOp::DivAssign => "/=",
        }
    }
}

impl Statement {
    fn to_source(&self) -> String {
        match self {
            Statement::Expr(e) => format!("{};", e.to_source()),
            Statement::Assign { target, value } => {
                format!("{} = {};", target.to_source(), value.to_source())
            }
            Statement::CompoundAssign { target, op, value } => {
                format!("{} {} {};", target.to_source(), op.to_str(), value.to_source())
            }
            Statement::If { condition, then_body, else_body } => {
                let then_stmts: Vec<_> = then_body.iter().map(|s| s.to_source()).collect();
                let mut out = format!("if {} {{ {} }}", condition.to_source(), then_stmts.join(" "));
                if let Some(else_stmts) = else_body {
                    let else_src: Vec<_> = else_stmts.iter().map(|s| s.to_source()).collect();
                    out.push_str(&format!(" else {{ {} }}", else_src.join(" ")));
                }
                out
            }
        }
    }
}

// ============================================================================
// Generation Context (maintains validity invariants)
// ============================================================================

/// Element schema: valid attributes and their types
struct ElementSchema {
    /// Valid attributes for this element (name -> type)
    attributes: &'static [(&'static str, TypeRef)],
}

/// Built-in element schemas (based on stdlib.rs definitions)
fn get_element_schema(element: &str) -> Option<ElementSchema> {
    // Shared layout props: width, height, padding, visible
    static BOX_ATTRS: &[(&str, TypeRef)] = &[
        ("width", TypeRef::Length),
        ("height", TypeRef::Length),
        ("padding", TypeRef::Length),
        ("background", TypeRef::Brush),
        ("corner-radius", TypeRef::Length),
    ];

    // VStack/HStack have gap in addition to layout props
    static STACK_ATTRS: &[(&str, TypeRef)] = &[
        ("width", TypeRef::Length),
        ("height", TypeRef::Length),
        ("padding", TypeRef::Length),
        ("gap", TypeRef::Length),
        ("background", TypeRef::Brush),
        ("corner-radius", TypeRef::Length),
    ];

    static TEXT_ATTRS: &[(&str, TypeRef)] = &[
        ("color", TypeRef::Color),
        ("font-size", TypeRef::Length),
    ];

    static BUTTON_ATTRS: &[(&str, TypeRef)] = &[
        ("width", TypeRef::Length),
        ("height", TypeRef::Length),
    ];

    // List doesn't have many attributes in stdlib
    static LIST_ATTRS: &[(&str, TypeRef)] = &[
        ("width", TypeRef::Length),
        ("height", TypeRef::Length),
    ];

    // ZStack doesn't have gap (only VStack/HStack have it)
    static ZSTACK_ATTRS: &[(&str, TypeRef)] = &[
        ("width", TypeRef::Length),
        ("height", TypeRef::Length),
        ("padding", TypeRef::Length),
        ("background", TypeRef::Brush),
        ("corner-radius", TypeRef::Length),
    ];

    match element {
        "VStack" | "HStack" => Some(ElementSchema { attributes: STACK_ATTRS }),
        "ZStack" => Some(ElementSchema { attributes: ZSTACK_ATTRS }),
        "Box" => Some(ElementSchema { attributes: BOX_ATTRS }),
        "Text" => Some(ElementSchema { attributes: TEXT_ATTRS }),
        "Button" => Some(ElementSchema { attributes: BUTTON_ATTRS }),
        "List" => Some(ElementSchema { attributes: LIST_ATTRS }),
        _ => None,
    }
}

/// Tracks state during generation to ensure validity.
struct GenerationContext {
    config: Config,
    /// Defined record names and their fields with types.
    records: BTreeMap<String, Vec<(String, TypeRef)>>,
    /// Defined enum names and their cases.
    enums: BTreeMap<String, Vec<String>>,
    /// Defined variant names and their cases.
    variants: BTreeMap<String, Vec<VariantCase>>,
    /// Current component's properties (name -> type).
    properties: BTreeMap<String, TypeRef>,
    /// Current component's callbacks.
    callbacks: Vec<String>,
    /// Counter for unique names.
    name_counter: usize,
}

impl GenerationContext {
    fn new(config: Config) -> Self {
        Self {
            config,
            records: BTreeMap::new(),
            enums: BTreeMap::new(),
            variants: BTreeMap::new(),
            properties: BTreeMap::new(),
            callbacks: Vec::new(),
            name_counter: 0,
        }
    }

    fn fresh_name(&mut self, prefix: &str) -> String {
        self.name_counter += 1;
        format!("{}{}", prefix, self.name_counter)
    }

    fn arbitrary_package_id(&mut self, u: &mut Unstructured) -> Result<PackageId> {
        Ok(PackageId {
            namespace: self.arbitrary_kebab_ident(u, "ns")?,
            name: self.arbitrary_kebab_ident(u, "pkg")?,
            version: format!(
                "{}.{}.{}",
                u.int_in_range(0..=10u32)?,
                u.int_in_range(0..=10u32)?,
                u.int_in_range(0..=10u32)?
            ),
        })
    }

    fn arbitrary_kebab_ident(&mut self, u: &mut Unstructured, prefix: &str) -> Result<String> {
        let suffix: u8 = u.int_in_range(0..=99)?;
        Ok(format!("{}-{}", prefix, suffix))
    }

    fn arbitrary_pascal_ident(&mut self, prefix: &str) -> String {
        self.fresh_name(prefix)
    }

    fn arbitrary_record(&mut self, u: &mut Unstructured) -> Result<RecordDef> {
        let name = self.arbitrary_pascal_ident("Record");
        let num_fields: usize = u.int_in_range(1..=5)?;

        let fields: Vec<_> = (0..num_fields)
            .map(|i| {
                let field_name = format!("field-{}", i);
                // 20% chance to use an existing record as field type (for chained access)
                let ty = if !self.records.is_empty() && u.int_in_range(0..=4)? == 0 {
                    // Pick an existing record
                    let record_names: Vec<_> = self.records.keys().cloned().collect();
                    let idx = u.int_in_range(0..=record_names.len() - 1)?;
                    TypeRef::Named(record_names[idx].clone())
                } else {
                    self.arbitrary_simple_type(u)?
                };
                Ok((field_name, ty))
            })
            .collect::<Result<_>>()?;

        self.records.insert(name.clone(), fields.clone());
        Ok(RecordDef { name, fields })
    }

    fn arbitrary_enum(&mut self, u: &mut Unstructured) -> Result<EnumDef> {
        let name = self.arbitrary_pascal_ident("Status");
        let num_cases: usize = u.int_in_range(2..=5)?;

        let cases: Vec<_> = (0..num_cases)
            .map(|i| format!("case-{}", i))
            .collect();

        self.enums.insert(name.clone(), cases.clone());
        Ok(EnumDef { name, cases })
    }

    fn arbitrary_variant(&mut self, u: &mut Unstructured) -> Result<VariantDef> {
        let name = self.arbitrary_pascal_ident("Message");
        let num_cases: usize = u.int_in_range(2..=4)?;

        let cases: Vec<_> = (0..num_cases)
            .map(|i| {
                let case_name = format!("kind-{}", i);
                let has_payload: bool = u.arbitrary()?;
                let payload = if has_payload {
                    Some(self.arbitrary_simple_type(u)?)
                } else {
                    None
                };
                Ok(VariantCase {
                    name: case_name,
                    payload,
                })
            })
            .collect::<Result<_>>()?;

        self.variants.insert(name.clone(), cases.clone());
        Ok(VariantDef { name, cases })
    }

    fn arbitrary_simple_type(&self, u: &mut Unstructured) -> Result<TypeRef> {
        let choice: u8 = u.int_in_range(0..=18)?;
        Ok(match choice {
            0 => TypeRef::Bool,
            1 => TypeRef::S32,
            2 => TypeRef::U32,
            3 => TypeRef::F32,
            4 => TypeRef::String,
            5 => TypeRef::Char,
            6 => TypeRef::Length,
            7 => TypeRef::Color,
            8 => TypeRef::List(Box::new(TypeRef::String)),
            9 => TypeRef::List(Box::new(TypeRef::S32)),
            10 => TypeRef::Option(Box::new(TypeRef::String)),
            11 => TypeRef::Option(Box::new(TypeRef::S32)),
            // Result types
            12 => TypeRef::Result {
                ok: Some(Box::new(TypeRef::String)),
                err: Some(Box::new(TypeRef::String)),
            },
            13 => TypeRef::Result {
                ok: Some(Box::new(TypeRef::S32)),
                err: Some(Box::new(TypeRef::String)),
            },
            // UI-specific types with unit suffixes
            14 => TypeRef::PhysicalLength,
            15 => TypeRef::Angle,
            16 => TypeRef::Duration,
            17 => TypeRef::Percent,
            _ => TypeRef::Bool,
        })
    }

    fn arbitrary_type(&self, u: &mut Unstructured) -> Result<TypeRef> {
        let choice: u8 = u.int_in_range(0..=24)?;
        Ok(match choice {
            0 => TypeRef::Bool,
            1 => TypeRef::S32,
            2 => TypeRef::U32,
            3 => TypeRef::F32,
            4 => TypeRef::String,
            5 => TypeRef::Char,
            6 => TypeRef::Length,
            7 => TypeRef::Color,
            8 => TypeRef::List(Box::new(self.arbitrary_simple_type(u)?)),
            9 => TypeRef::Option(Box::new(self.arbitrary_simple_type(u)?)),
            10..=12 => {
                // Pick a defined record if any
                if let Some(name) = self.records.keys().next() {
                    TypeRef::Named(name.clone())
                } else {
                    TypeRef::S32
                }
            }
            13..=14 => {
                // Pick a defined enum if any
                if let Some(name) = self.enums.keys().next() {
                    TypeRef::Named(name.clone())
                } else {
                    TypeRef::Bool
                }
            }
            15..=16 => {
                // Generate tuple type with 2-3 primitive elements only
                // (avoid nested types like option<T> which can cause type errors)
                let primitives = [TypeRef::S32, TypeRef::Bool, TypeRef::String, TypeRef::F32, TypeRef::Char];
                let num_elements: usize = u.int_in_range(2..=3)?;
                let types: Vec<_> = (0..num_elements)
                    .filter_map(|_| {
                        let idx = u.int_in_range(0..=primitives.len() - 1).ok()?;
                        Some(primitives[idx].clone())
                    })
                    .collect();
                if types.len() >= 2 {
                    TypeRef::Tuple(types)
                } else {
                    TypeRef::String
                }
            }
            17..=18 => {
                // Result type with simple ok/err types
                let ok_ty = self.arbitrary_simple_type(u)?;
                let err_ty = TypeRef::String; // Keep error type simple
                TypeRef::Result {
                    ok: Some(Box::new(ok_ty)),
                    err: Some(Box::new(err_ty)),
                }
            }
            19..=20 => {
                // Pick a defined variant if any
                if let Some(name) = self.variants.keys().next() {
                    TypeRef::Named(name.clone())
                } else {
                    TypeRef::S32
                }
            }
            // UI-specific types with unit suffixes
            21 => TypeRef::PhysicalLength,
            22 => TypeRef::Angle,
            23 => TypeRef::Duration,
            24 => TypeRef::Percent,
            _ => TypeRef::String,
        })
    }

    fn arbitrary_component(&mut self, u: &mut Unstructured) -> Result<ComponentDef> {
        // Reset per-component state
        self.properties.clear();
        self.callbacks.clear();

        let name = self.arbitrary_pascal_ident("Component");
        let is_export: bool = u.arbitrary()?;

        // Generate properties (use valid kebab-case names)
        let prop_names = ["count", "value", "label", "enabled", "visible", "selected", "items", "data", "name", "title"];
        let num_props: usize = u.int_in_range(0..=prop_names.len().min(self.config.max_properties))?;
        let properties: Vec<_> = (0..num_props)
            .map(|i| {
                let prop_name = prop_names[i].to_string();
                let ty = self.arbitrary_type(u)?;
                let has_default: bool = u.arbitrary()?;
                let default = if has_default {
                    Some(self.arbitrary_expr_of_type(u, &ty, 0)?)
                } else {
                    None
                };
                self.properties.insert(prop_name.clone(), ty.clone());
                Ok(PropertyDef {
                    name: prop_name,
                    ty,
                    default,
                })
            })
            .collect::<Result<_>>()?;

        // Generate callbacks (use valid kebab-case names)
        let cb_names = ["on-click", "on-change", "on-submit", "on-select", "on-load"];
        let num_callbacks: usize = u.int_in_range(0..=cb_names.len().min(self.config.max_callbacks))?;
        let callbacks: Vec<_> = (0..num_callbacks)
            .map(|i| {
                let cb_name = cb_names[i].to_string();
                self.callbacks.push(cb_name.clone());
                let has_return: bool = u.arbitrary()?;
                let return_ty = if has_return {
                    Some(self.arbitrary_simple_type(u)?)
                } else {
                    None
                };
                Ok(CallbackDef {
                    name: cb_name,
                    params: vec![],
                    return_ty,
                    is_export: u.arbitrary()?,
                })
            })
            .collect::<Result<_>>()?;

        // Generate body
        let num_nodes: usize = u.int_in_range(1..=3)?;
        let body: Vec<_> = (0..num_nodes)
            .map(|_| self.arbitrary_node(u, 0))
            .collect::<Result<_>>()?;

        Ok(ComponentDef {
            name,
            is_export,
            properties,
            callbacks,
            body,
        })
    }

    fn arbitrary_node(&mut self, u: &mut Unstructured, depth: usize) -> Result<Node> {
        if depth >= self.config.max_node_depth {
            // At max depth, only generate text nodes
            return Ok(Node::Text(self.arbitrary_expr_of_type(u, &TypeRef::String, 0)?));
        }

        let choice: u8 = u.int_in_range(0..=3)?;
        match choice {
            0 => self.arbitrary_element(u, depth),
            1 => Ok(Node::Text(self.arbitrary_expr_of_type(u, &TypeRef::String, 0)?)),
            2 => self.arbitrary_if_node(u, depth),
            3 => self.arbitrary_for_node(u, depth),
            _ => Ok(Node::Text(Expr::String("text".into()))),
        }
    }

    fn arbitrary_element(&mut self, u: &mut Unstructured, depth: usize) -> Result<Node> {
        let elements = ["VStack", "HStack", "ZStack", "Box", "Text", "Button", "List"];
        let name = elements[u.int_in_range(0..=elements.len() - 1)?].to_string();

        // Generate schema-aware bindings
        let bindings = if let Some(schema) = get_element_schema(&name) {
            let num_bindings: usize = u.int_in_range(0..=2.min(schema.attributes.len()))?;
            let mut used_attrs = std::collections::BTreeSet::new();
            let mut result = Vec::new();

            for _ in 0..num_bindings {
                // Pick an unused attribute from schema
                let attr_idx = u.int_in_range(0..=schema.attributes.len() - 1)?;
                let (attr_name, attr_type) = &schema.attributes[attr_idx];
                if used_attrs.contains(*attr_name) {
                    continue;
                }
                used_attrs.insert(*attr_name);

                // Generate value of correct type
                let value = self.arbitrary_expr_of_type(u, attr_type, 0)?;
                result.push(Binding {
                    name: attr_name.to_string(),
                    value,
                });
            }
            result
        } else {
            Vec::new()
        };

        // Generate handlers (unique names, at least one statement each)
        let handler_names = ["clicked", "pressed", "hovered", "changed"];
        let num_handlers: usize = u.int_in_range(0..=1)?; // Limit to avoid duplicates
        let mut used_handlers = std::collections::BTreeSet::new();
        let handlers: Vec<_> = (0..num_handlers)
            .filter_map(|_| {
                // Pick an unused handler name
                let idx = u.int_in_range(0..=handler_names.len() - 1).ok()?;
                let handler_name = handler_names[idx].to_string();
                if used_handlers.contains(&handler_name) {
                    return None; // Skip duplicates
                }
                used_handlers.insert(handler_name.clone());

                // Generate at least one statement
                let num_stmts: usize = u.int_in_range(1..=2).ok()?;
                let body: Vec<_> = (0..num_stmts)
                    .filter_map(|_| self.arbitrary_statement(u).ok())
                    .collect();
                if body.is_empty() {
                    return None; // Skip if no valid statements
                }
                Some(Handler {
                    name: handler_name,
                    body,
                })
            })
            .collect();

        // Generate children (at least one required for valid Yel)
        let num_children: usize = u.int_in_range(1..=self.config.max_children)?;
        let children: Vec<_> = (0..num_children)
            .map(|_| self.arbitrary_node(u, depth + 1))
            .collect::<Result<_>>()?;

        Ok(Node::Element {
            name,
            bindings,
            handlers,
            children,
        })
    }

    fn arbitrary_if_node(&mut self, u: &mut Unstructured, depth: usize) -> Result<Node> {
        let condition = self.arbitrary_expr_of_type(u, &TypeRef::Bool, 0)?;
        let num_then: usize = u.int_in_range(1..=2)?;
        let then_branch: Vec<_> = (0..num_then)
            .map(|_| self.arbitrary_node(u, depth + 1))
            .collect::<Result<_>>()?;

        // Generate else if branches (0-2)
        let num_else_if: usize = u.int_in_range(0..=2)?;
        let else_if_branches: Vec<_> = (0..num_else_if)
            .filter_map(|_| {
                let cond = self.arbitrary_expr_of_type(u, &TypeRef::Bool, 0).ok()?;
                let num_nodes: usize = u.int_in_range(1..=2).ok()?;
                let nodes: Vec<_> = (0..num_nodes)
                    .filter_map(|_| self.arbitrary_node(u, depth + 1).ok())
                    .collect();
                if nodes.is_empty() {
                    return None;
                }
                Some((cond, nodes))
            })
            .collect();

        let has_else: bool = u.arbitrary()?;
        let else_branch = if has_else {
            let num_else: usize = u.int_in_range(1..=2)?;
            Some(
                (0..num_else)
                    .map(|_| self.arbitrary_node(u, depth + 1))
                    .collect::<Result<_>>()?,
            )
        } else {
            None
        };

        Ok(Node::If {
            condition,
            then_branch,
            else_if_branches,
            else_branch,
        })
    }

    fn arbitrary_for_node(&mut self, u: &mut Unstructured, depth: usize) -> Result<Node> {
        let item = format!("item-{}", self.name_counter);
        self.name_counter += 1;

        // Choose iterable type: list property, list literal, or range expression
        let iterable_choice: usize = u.int_in_range(0..=2)?;
        let iterable = match iterable_choice {
            0 => {
                // Try to find a list property
                if let Some((name, _)) = self.properties.iter().find(|(_, ty)| matches!(ty, TypeRef::List(_))) {
                    Expr::Var(name.clone())
                } else {
                    Expr::List(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)])
                }
            }
            1 => {
                // Generate a range expression with fixed simple values
                let start: i64 = u.int_in_range(0..=5)?;
                let length: i64 = u.int_in_range(2..=10)?;
                let end: i64 = start + length;
                let inclusive: bool = u.arbitrary()?;
                Expr::Range {
                    start: Box::new(Expr::Int(start)),
                    end: Box::new(Expr::Int(end)),
                    inclusive,
                }
            }
            _ => {
                // Generate a list literal
                Expr::List(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)])
            }
        };

        let num_body: usize = u.int_in_range(1..=2)?;
        let body: Vec<_> = (0..num_body)
            .map(|_| self.arbitrary_node(u, depth + 1))
            .collect::<Result<_>>()?;

        // 20% chance to add key(expr) - use item variable reference
        let key = if u.int_in_range(0..=9)? < 2 {
            Some(Expr::Var(item.clone()))
        } else {
            None
        };

        Ok(Node::For {
            item,
            iterable,
            key,
            body,
        })
    }

    fn arbitrary_binding_name(&self, u: &mut Unstructured) -> Result<String> {
        let names = ["padding", "gap", "width", "height", "color", "background", "enabled"];
        Ok(names[u.int_in_range(0..=names.len() - 1)?].to_string())
    }

    fn arbitrary_handler_name(&self, u: &mut Unstructured) -> Result<String> {
        let names = ["clicked", "pressed", "hovered", "changed"];
        Ok(names[u.int_in_range(0..=names.len() - 1)?].to_string())
    }

    fn arbitrary_statement(&mut self, u: &mut Unstructured) -> Result<Statement> {
        self.arbitrary_statement_with_depth(u, 0)
    }

    fn arbitrary_statement_with_depth(&mut self, u: &mut Unstructured, depth: usize) -> Result<Statement> {
        // Generate callback calls, property assignments (regular or compound) and if statements

        // 15% chance to generate callback call if we have callbacks
        if !self.callbacks.is_empty() && u.int_in_range(0..=19)? < 3 {
            let cb_idx = u.int_in_range(0..=self.callbacks.len() - 1)?;
            let cb_name = self.callbacks[cb_idx].clone();
            return Ok(Statement::Expr(Expr::Call {
                func: cb_name,
                args: vec![], // Callbacks are generated with no params
            }));
        }

        // 20% chance to generate if statement (only at depth 0 to avoid deep nesting)
        if depth == 0 && u.int_in_range(0..=9)? < 2 {
            let condition = self.arbitrary_expr_of_type(u, &TypeRef::Bool, 0)?;
            let num_then: usize = u.int_in_range(1..=2)?;
            let then_body: Vec<_> = (0..num_then)
                .filter_map(|_| self.arbitrary_statement_with_depth(u, depth + 1).ok())
                .collect();
            if then_body.is_empty() {
                // Fallback if no statements generated
                return Ok(Statement::Expr(Expr::Int(u.int_in_range(0..=100)?)));
            }
            let has_else: bool = u.arbitrary()?;
            let else_body = if has_else {
                let num_else: usize = u.int_in_range(1..=2)?;
                Some((0..num_else)
                    .filter_map(|_| self.arbitrary_statement_with_depth(u, depth + 1).ok())
                    .collect())
            } else {
                None
            };
            return Ok(Statement::If {
                condition,
                then_body,
                else_body,
            });
        }

        // Try to find a numeric property for compound assignment (30% chance)
        let numeric_prop = self.properties.iter()
            .find(|(_, ty)| matches!(
                ty,
                TypeRef::S8 | TypeRef::S16 | TypeRef::S32 | TypeRef::S64 |
                TypeRef::U8 | TypeRef::U16 | TypeRef::U32 | TypeRef::U64 |
                TypeRef::F32 | TypeRef::F64
            ))
            .map(|(n, t)| (n.clone(), t.clone()));

        // 30% chance to use compound assignment if we have a numeric property
        if let Some((name, ty)) = numeric_prop {
            if u.int_in_range(0..=9)? < 3 {
                let op = match u.int_in_range(0..=3)? {
                    0 => CompoundOp::AddAssign,
                    1 => CompoundOp::SubAssign,
                    2 => CompoundOp::MulAssign,
                    _ => CompoundOp::DivAssign,
                };
                let value = self.arbitrary_expr_of_type(u, &ty, 0)?;
                return Ok(Statement::CompoundAssign {
                    target: Expr::Var(name),
                    op,
                    value,
                });
            }
        }

        // Regular assignment to any property
        let prop = self.properties.iter().next().map(|(n, t)| (n.clone(), t.clone()));
        if let Some((name, ty)) = prop {
            let value = self.arbitrary_expr_of_type(u, &ty, 0)?;
            Ok(Statement::Assign {
                target: Expr::Var(name),
                value,
            })
        } else {
            // Fallback: simple expression
            Ok(Statement::Expr(Expr::Int(u.int_in_range(0..=100)?)))
        }
    }

    fn arbitrary_expr(&mut self, u: &mut Unstructured, depth: usize) -> Result<Expr> {
        self.arbitrary_expr_of_type(u, &TypeRef::S32, depth)
    }

    /// Generate an expression of a specific type (key for semantic validity).
    fn arbitrary_expr_of_type(
        &mut self,
        u: &mut Unstructured,
        ty: &TypeRef,
        depth: usize,
    ) -> Result<Expr> {
        if depth >= self.config.max_expr_depth {
            return self.arbitrary_literal_of_type(u, ty);
        }

        // Prefer variables of matching type
        if let Some((name, _)) = self.properties.iter().find(|(_, t)| *t == ty) {
            if u.arbitrary::<bool>()? {
                return Ok(Expr::Var(name.clone()));
            }
        }

        // 10% chance to generate ternary expression for simple non-string types
        // (String ternaries cause quote conflicts in interpolation contexts)
        if depth < 2 && u.int_in_range(0..=9)? == 0 {
            match ty {
                TypeRef::S32 | TypeRef::U32 | TypeRef::F32 | TypeRef::Bool => {
                    let cond = self.arbitrary_expr_of_type(u, &TypeRef::Bool, depth + 1)?;
                    let then_expr = self.arbitrary_literal_of_type(u, ty)?;
                    let else_expr = self.arbitrary_literal_of_type(u, ty)?;
                    return Ok(Expr::Ternary {
                        cond: Box::new(cond),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    });
                }
                _ => {}
            }
        }

        // 10% chance to generate index expression
        if u.int_in_range(0..=9)? == 0 {
            // First try: use a list property if available
            if let Some((list_name, _)) = self.properties.iter().find(|(_, prop_ty)| {
                matches!(prop_ty, TypeRef::List(inner) if inner.as_ref() == ty)
            }) {
                let list_name = list_name.clone();
                return Ok(Expr::Index {
                    base: Box::new(Expr::Var(list_name)),
                    index: Box::new(Expr::Int(u.int_in_range(0..=2)?)),
                });
            }
            // Second: generate a list literal and index it for simple types
            match ty {
                TypeRef::S32 | TypeRef::U32 | TypeRef::Bool | TypeRef::String | TypeRef::Char => {
                    let num_elements: usize = u.int_in_range(2..=4)?;
                    let elements: Vec<Expr> = (0..num_elements)
                        .map(|_| self.arbitrary_literal_of_type(u, ty))
                        .collect::<Result<_>>()?;
                    let index = u.int_in_range(0..=(num_elements - 1) as i64)?;
                    return Ok(Expr::Index {
                        base: Box::new(Expr::List(elements)),
                        index: Box::new(Expr::Int(index)),
                    });
                }
                _ => {}
            }
        }

        // 10% chance to generate field access (record.field or record.field.subfield)
        if u.int_in_range(0..=9)? == 0 {
            if let Some((prop_name, fields)) = self.find_chained_field_access(ty) {
                // Build the chained field access expression
                let mut expr = Expr::Var(prop_name);
                for field in fields {
                    expr = Expr::Field {
                        base: Box::new(expr),
                        field,
                    };
                }
                return Ok(expr);
            }
        }

        // 5% chance to generate typed closure for simple types
        if depth == 0 && u.int_in_range(0..=19)? == 0 {
            match ty {
                TypeRef::S32 | TypeRef::Bool => {
                    let param_name = format!("x{}", self.name_counter);
                    self.name_counter += 1;
                    let param_ty = TypeRef::S32;
                    let body = self.arbitrary_literal_of_type(u, ty)?;
                    return Ok(Expr::Closure {
                        params: vec![(param_name, param_ty)],
                        body: Box::new(body),
                    });
                }
                _ => {}
            }
        }

        // 10% chance to generate unary expression for numeric/bool types
        if depth < 2 && u.int_in_range(0..=9)? == 0 {
            match ty {
                TypeRef::S32 | TypeRef::S8 | TypeRef::S16 | TypeRef::S64 => {
                    // Use positive literal to avoid --N syntax issue
                    let operand = Expr::Int(u.int_in_range(1..=100)?);
                    return Ok(Expr::Unary {
                        op: UnaryOp::Neg,
                        operand: Box::new(operand),
                    });
                }
                TypeRef::Bool => {
                    // Use simple bool literal to avoid nested unary
                    let operand = Expr::Bool(u.arbitrary()?);
                    return Ok(Expr::Unary {
                        op: UnaryOp::Not,
                        operand: Box::new(operand),
                    });
                }
                _ => {}
            }
        }

        // Generate based on type
        match ty {
            TypeRef::Bool => {
                let choice: u8 = u.int_in_range(0..=2)?;
                match choice {
                    0 => Ok(Expr::Bool(u.arbitrary()?)),
                    1 => {
                        // Comparison
                        let lhs = self.arbitrary_expr_of_type(u, &TypeRef::S32, depth + 1)?;
                        let rhs = self.arbitrary_expr_of_type(u, &TypeRef::S32, depth + 1)?;
                        let ops = [BinaryOp::Eq, BinaryOp::Ne, BinaryOp::Lt, BinaryOp::Le, BinaryOp::Gt, BinaryOp::Ge];
                        let op = ops[u.int_in_range(0..=ops.len() - 1)?];
                        Ok(Expr::Binary {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        })
                    }
                    _ => Ok(Expr::Bool(true)),
                }
            }
            TypeRef::S32 | TypeRef::S8 | TypeRef::S16 | TypeRef::S64 => {
                // 20% chance to generate binary arithmetic including modulo
                // Only for signed types since literals default to s32
                if depth < 2 && u.int_in_range(0..=4)? == 0 {
                    let lhs = Expr::Int(u.int_in_range(1..=100)?);
                    let rhs = Expr::Int(u.int_in_range(1..=10)?); // non-zero for modulo
                    let ops = [BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Mod];
                    let op = ops[u.int_in_range(0..=ops.len() - 1)?];
                    return Ok(Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                }
                // Signed integers can be negative
                Ok(Expr::Int(u.int_in_range(-100..=100)?))
            }
            TypeRef::U32 | TypeRef::U8 | TypeRef::U16 | TypeRef::U64 => {
                // Skip binary ops for unsigned (literals are s32, no coercion to unsigned)
                // TODO: Fix when type checker supports polymorphic integer literals
                Ok(Expr::Int(u.int_in_range(0..=100)?))
            }
            TypeRef::F32 | TypeRef::F64 => {
                Ok(Expr::Float(u.arbitrary::<f64>()?.abs() % 1000.0))
            }
            TypeRef::String => {
                let choice: u8 = u.int_in_range(0..=1)?;
                match choice {
                    0 => Ok(Expr::String(self.arbitrary_string(u)?)),
                    _ => {
                        // Use variable interpolation if we have string properties
                        let string_prop = self.properties.iter()
                            .find(|(_, t)| matches!(t, TypeRef::String))
                            .map(|(n, _)| n.clone());
                        if let Some(prop_name) = string_prop {
                            let parts = vec![
                                InterpolationPart::Literal("Value: ".into()),
                                InterpolationPart::Expr(Expr::Var(prop_name)),
                            ];
                            Ok(Expr::Interpolation(parts))
                        } else {
                            // Fallback to plain string
                            Ok(Expr::String(self.arbitrary_string(u)?))
                        }
                    }
                }
            }
            TypeRef::Char => {
                let chars = ['a', 'b', 'c', 'x', 'y', 'z', '0', '1', '!', '@'];
                Ok(Expr::Char(chars[u.int_in_range(0..=chars.len() - 1)?]))
            }
            TypeRef::Length => {
                // Don't use % as it generates percent type, not length
                let units = ["px", "pt", "rem"];
                let unit = units[u.int_in_range(0..=units.len() - 1)?];
                Ok(Expr::Unit(u.int_in_range(0..=100)? as f64, unit.into()))
            }
            TypeRef::PhysicalLength => {
                // phx unit generates physical-length type
                Ok(Expr::Unit(u.int_in_range(0..=100)? as f64, "phx".into()))
            }
            TypeRef::Angle => {
                // deg, rad, turn units generate angle type
                let units = ["deg", "rad", "turn"];
                let unit = units[u.int_in_range(0..=units.len() - 1)?];
                let value = match unit {
                    "deg" => u.int_in_range(0..=360)? as f64,
                    "rad" => (u.int_in_range(0..=628)? as f64) / 100.0, // 0 to ~2π
                    "turn" => (u.int_in_range(0..=100)? as f64) / 100.0, // 0 to 1
                    _ => 0.0,
                };
                Ok(Expr::Unit(value, unit.into()))
            }
            TypeRef::Duration => {
                // ms, s units generate duration type
                let units = ["ms", "s"];
                let unit = units[u.int_in_range(0..=units.len() - 1)?];
                let value = match unit {
                    "ms" => u.int_in_range(0..=1000)? as f64,
                    "s" => (u.int_in_range(0..=100)? as f64) / 10.0, // 0 to 10 seconds
                    _ => 0.0,
                };
                Ok(Expr::Unit(value, unit.into()))
            }
            TypeRef::Percent => {
                // % unit generates percent type
                Ok(Expr::Unit(u.int_in_range(0..=100)? as f64, "%".into()))
            }
            TypeRef::Color | TypeRef::Brush => {
                let colors = ["#ffffff", "#000000", "#ff0000", "#00ff00", "#0000ff"];
                Ok(Expr::Color(colors[u.int_in_range(0..=colors.len() - 1)?].into()))
            }
            TypeRef::List(inner) => {
                let num_elements: usize = u.int_in_range(0..=5)?;
                let elements: Vec<_> = (0..num_elements)
                    .map(|_| self.arbitrary_expr_of_type(u, inner, depth + 1))
                    .collect::<Result<_>>()?;
                Ok(Expr::List(elements))
            }
            TypeRef::Tuple(types) => {
                let elements: Vec<_> = types
                    .iter()
                    .map(|ty| self.arbitrary_expr_of_type(u, ty, depth + 1))
                    .collect::<Result<_>>()?;
                Ok(Expr::Tuple(elements))
            }
            TypeRef::Option(inner) => {
                let choice: u8 = u.int_in_range(0..=2)?;
                match choice {
                    0 => {
                        // Try to use optional member access if we have option<record> property
                        // where the record has a field matching inner type
                        if let Some((prop_name, record_name)) = self.find_option_record_prop_with_field(inner) {
                            // Generate prop?.field
                            if let Some(field_name) = self.find_record_field_of_type(&record_name, inner) {
                                return Ok(Expr::OptionalField {
                                    base: Box::new(Expr::Var(prop_name)),
                                    field: field_name,
                                });
                            }
                        }
                        // Fallback to some()
                        Ok(Expr::VariantCtor {
                            variant: "option".into(),
                            case: "some".into(),
                            payload: Some(Box::new(self.arbitrary_expr_of_type(u, inner, depth + 1)?)),
                        })
                    }
                    1 => {
                        Ok(Expr::VariantCtor {
                            variant: "option".into(),
                            case: "some".into(),
                            payload: Some(Box::new(self.arbitrary_expr_of_type(u, inner, depth + 1)?)),
                        })
                    }
                    _ => {
                        Ok(Expr::VariantCtor {
                            variant: "option".into(),
                            case: "none".into(),
                            payload: None,
                        })
                    }
                }
            }
            TypeRef::Result { ok, err } => {
                // Generate ok(value) or err(error)
                let use_ok: bool = u.arbitrary()?;
                if use_ok {
                    let payload = if let Some(ok_ty) = ok {
                        Some(Box::new(self.arbitrary_expr_of_type(u, ok_ty, depth + 1)?))
                    } else {
                        None
                    };
                    Ok(Expr::VariantCtor {
                        variant: "result".into(),
                        case: "ok".into(),
                        payload,
                    })
                } else {
                    let payload = if let Some(err_ty) = err {
                        Some(Box::new(self.arbitrary_expr_of_type(u, err_ty, depth + 1)?))
                    } else {
                        None
                    };
                    Ok(Expr::VariantCtor {
                        variant: "result".into(),
                        case: "err".into(),
                        payload,
                    })
                }
            }
            TypeRef::Named(name) => {
                // Check if it's a record
                if let Some(fields) = self.records.get(name).cloned() {
                    let field_exprs: Vec<_> = fields
                        .iter()
                        .map(|(fname, fty)| {
                            let expr = self.arbitrary_expr_of_type(u, fty, depth + 1)?;
                            Ok((fname.clone(), expr))
                        })
                        .collect::<Result<_>>()?;
                    return Ok(Expr::Record {
                        ty_name: name.clone(),
                        fields: field_exprs,
                    });
                }
                // Check if it's an enum
                if let Some(cases) = self.enums.get(name).cloned() {
                    let case = &cases[u.int_in_range(0..=cases.len() - 1)?];
                    return Ok(Expr::EnumCase {
                        enum_name: name.clone(),
                        case: case.clone(),
                    });
                }
                // Check if it's a variant
                if let Some(cases) = self.variants.get(name).cloned() {
                    let case = &cases[u.int_in_range(0..=cases.len() - 1)?];
                    let payload = if let Some(ref payload_ty) = case.payload {
                        Some(Box::new(self.arbitrary_expr_of_type(u, payload_ty, depth + 1)?))
                    } else {
                        None
                    };
                    return Ok(Expr::VariantCtor {
                        variant: name.clone(),
                        case: case.name.clone(),
                        payload,
                    });
                }
                // Fallback
                Ok(Expr::Int(0))
            }
            _ => self.arbitrary_literal_of_type(u, ty),
        }
    }

    fn arbitrary_literal_of_type(&self, u: &mut Unstructured, ty: &TypeRef) -> Result<Expr> {
        Ok(match ty {
            TypeRef::Bool => Expr::Bool(u.arbitrary()?),
            TypeRef::S32 | TypeRef::S8 | TypeRef::S16 | TypeRef::S64 => {
                Expr::Int(u.int_in_range(-100..=100)?)
            }
            TypeRef::U32 | TypeRef::U8 | TypeRef::U16 | TypeRef::U64 => {
                Expr::Int(u.int_in_range(0..=100)?)
            }
            TypeRef::F32 | TypeRef::F64 => Expr::Float(u.arbitrary::<f64>()?.abs() % 100.0),
            TypeRef::String => Expr::String("test".into()),
            TypeRef::Char => Expr::Char('x'),
            TypeRef::Length => Expr::Unit(10.0, "px".into()),
            TypeRef::PhysicalLength => Expr::Unit(10.0, "phx".into()),
            TypeRef::Angle => Expr::Unit(45.0, "deg".into()),
            TypeRef::Duration => Expr::Unit(100.0, "ms".into()),
            TypeRef::Percent => Expr::Unit(50.0, "%".into()),
            TypeRef::Color | TypeRef::Brush => Expr::Color("#ffffff".into()),
            TypeRef::List(_) => Expr::List(vec![]),
            TypeRef::Tuple(types) => {
                // Generate simple literals for each tuple element
                let elements: Vec<_> = types
                    .iter()
                    .map(|ty| self.arbitrary_literal_of_type(u, ty).unwrap_or(Expr::Int(0)))
                    .collect();
                Expr::Tuple(elements)
            }
            TypeRef::Option(_) => Expr::VariantCtor {
                variant: "option".into(),
                case: "none".into(),
                payload: None,
            },
            TypeRef::Result { ok, err: _ } => {
                // Default to ok with literal payload
                let payload = ok.as_ref().map(|ty| {
                    Box::new(self.arbitrary_literal_of_type(u, ty).unwrap_or(Expr::Int(0)))
                });
                Expr::VariantCtor {
                    variant: "result".into(),
                    case: "ok".into(),
                    payload,
                }
            }
            _ => Expr::Int(0),
        })
    }

    fn arbitrary_string(&self, u: &mut Unstructured) -> Result<String> {
        let words = ["hello", "world", "test", "value", "item", "data"];
        let num_words: usize = u.int_in_range(1..=3)?;
        let result: Vec<_> = (0..num_words)
            .map(|_| Ok(words[u.int_in_range(0..=words.len() - 1)?]))
            .collect::<Result<_>>()?;
        Ok(result.join(" "))
    }

    /// Find a property of type option<RecordName> where the record has a field of the given type.
    fn find_option_record_prop_with_field(&self, inner_ty: &TypeRef) -> Option<(String, String)> {
        for (prop_name, prop_ty) in &self.properties {
            if let TypeRef::Option(inner) = prop_ty {
                if let TypeRef::Named(record_name) = inner.as_ref() {
                    // Check if this record has a field of the target type
                    if let Some(fields) = self.records.get(record_name) {
                        if fields.iter().any(|(_, fty)| fty == inner_ty) {
                            return Some((prop_name.clone(), record_name.clone()));
                        }
                    }
                }
            }
        }
        None
    }

    /// Find a field in a record that matches the given type.
    fn find_record_field_of_type(&self, record_name: &str, target_ty: &TypeRef) -> Option<String> {
        if let Some(fields) = self.records.get(record_name) {
            for (field_name, field_ty) in fields {
                if field_ty == target_ty {
                    return Some(field_name.clone());
                }
            }
        }
        None
    }

    /// Find a property that is a record type, and a field in that record matching the target type.
    /// Returns (property_name, field_name) if found.
    fn find_record_prop_with_field(&self, target_ty: &TypeRef) -> Option<(String, String)> {
        for (prop_name, prop_ty) in &self.properties {
            if let TypeRef::Named(record_name) = prop_ty {
                if let Some(field_name) = self.find_record_field_of_type(record_name, target_ty) {
                    return Some((prop_name.clone(), field_name));
                }
            }
        }
        None
    }

    /// Find a chain of field accesses to reach the target type.
    /// Returns (property_name, vec![field1, field2, ...]) for chained access like prop.field1.field2
    /// Searches up to 2 levels deep (a.b.c).
    fn find_chained_field_access(&self, target_ty: &TypeRef) -> Option<(String, Vec<String>)> {
        // First try direct field access (depth 1)
        if let Some((prop_name, field_name)) = self.find_record_prop_with_field(target_ty) {
            return Some((prop_name, vec![field_name]));
        }

        // Try depth 2: property -> field (record) -> subfield
        for (prop_name, prop_ty) in &self.properties {
            if let TypeRef::Named(record_name) = prop_ty {
                if let Some(fields) = self.records.get(record_name).cloned() {
                    for (field_name, field_ty) in &fields {
                        // Check if field_ty is a record with a subfield of target type
                        if let TypeRef::Named(nested_record_name) = field_ty {
                            if let Some(subfield_name) = self.find_record_field_of_type(nested_record_name, target_ty) {
                                return Some((prop_name.clone(), vec![field_name.clone(), subfield_name]));
                            }
                        }
                    }
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_module() {
        let data = [0u8; 1024];
        let mut u = Unstructured::new(&data);
        let module = YelModule::arbitrary(&mut u).unwrap();
        let source = module.to_source();
        assert!(!source.is_empty());
        println!("{}", source);
    }

    #[test]
    fn test_generated_code_parses() {
        // Generate and try to parse
        let data = [42u8; 2048];
        let mut u = Unstructured::new(&data);
        let module = YelModule::arbitrary(&mut u).unwrap();
        let source = module.to_source();

        // Try to parse with yel-core
        let result = yel_core::parse(&source);
        assert!(result.is_ok(), "Generated code failed to parse: {:?}\nSource:\n{}", result.err(), source);
    }

    #[test]
    fn test_generated_code_compiles() {
        // Generate and compile to WASM
        let data = [42u8; 2048];
        let mut u = Unstructured::new(&data);
        let module = YelModule::arbitrary(&mut u).unwrap();
        let source = module.to_source();

        // Full compilation pipeline
        let mut compiler = yel_core::Compiler::new();
        let parsed = compiler.parse(&source).expect("Failed to parse");
        let hir_components = compiler.lower_to_hir(&parsed);

        assert!(!compiler.has_errors(), "HIR errors: {}\nSource:\n{}", compiler.render_diagnostics(), source);

        for hir in &hir_components {
            let thir = compiler.type_check(hir);
            assert!(!compiler.has_errors(), "Type errors: {}\nSource:\n{}", compiler.render_diagnostics(), source);

            let lir = compiler.lower_to_lir(&thir);
            let ctx = compiler.context();

            // Generate WASM
            let wasm_result = yel_core::codegen::generate_wasm(&[lir], ctx);
            assert!(wasm_result.is_ok(), "WASM generation failed: {:?}\nSource:\n{}", wasm_result.err(), source);
        }
    }

    #[test]
    fn test_multiple_seeds() {
        // Test with multiple seeds to catch more edge cases
        for seed in 0..100u64 {
            let data = generate_seed_data(seed);
            let mut u = Unstructured::new(&data);

            if let Ok(module) = YelModule::arbitrary(&mut u) {
                let source = module.to_source();

                // At minimum, it should parse
                let result = yel_core::parse(&source);
                assert!(result.is_ok(), "Seed {} failed to parse: {:?}\nSource:\n{}", seed, result.err(), source);
            }
        }
    }

    fn generate_seed_data(seed: u64) -> Vec<u8> {
        let mut state = seed;
        let mut data = Vec::with_capacity(4096);
        for _ in 0..4096 {
            state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
            data.push((state >> 33) as u8);
        }
        data
    }
}
