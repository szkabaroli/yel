//! Abstract Syntax Tree definitions for the Yel.

use crate::source::Span;
use crate::syntax::NodeId;
use serde::{Deserialize, Serialize};

/// A node with source location information.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn dummy(node: T) -> Self {
        Self {
            node,
            span: Span::default(),
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

/// Package identifier following WIT naming: namespace:name@version
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PackageId {
    /// Namespace (e.g., "yel", "wasi")
    pub namespace: String,
    /// Package name (e.g., "counter", "ui-components")
    pub name: String,
    /// Semantic version (e.g., "1.0.0")
    pub version: Option<String>,
}

impl PackageId {
    pub fn new(namespace: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
            name: name.into(),
            version: None,
        }
    }

    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.namespace, self.name)?;
        if let Some(ref v) = self.version {
            write!(f, "@{}", v)?;
        }
        Ok(())
    }
}

/// A complete file with optional package declaration, records, enums, variants, and components.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct File {
    /// Optional package declaration.
    pub package: Option<PackageId>,
    /// Record type definitions.
    pub records: Vec<Spanned<Record>>,
    /// Enum type definitions.
    pub enums: Vec<Spanned<Enum>>,
    /// Variant type definitions.
    pub variants: Vec<Spanned<Variant>>,
    /// Intrinsic element type definitions.
    pub elements: Vec<Spanned<Element>>,
    /// Imported component declarations.
    pub import_components: Vec<Spanned<ImportComponent>>,
    /// Global singleton declarations.
    pub globals: Vec<Spanned<Global>>,
    /// Component definitions.
    pub components: Vec<Spanned<Component>>,
}

/// A complete component definition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Component {
    /// Unique node ID for this component.
    pub id: NodeId,
    /// Component name (PascalCase).
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Whether this component is exported (marked with `export` keyword).
    pub is_export: bool,
    /// Property declarations.
    pub properties: Vec<Spanned<Property>>,
    /// Function declarations (callbacks).
    pub functions: Vec<Spanned<FunctionDecl>>,
    /// UI tree body.
    pub body: Vec<Spanned<Node>>,
}

/// A property declaration: `name: type = value;`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Property {
    /// Property name.
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Property type.
    pub ty: Ty,
    /// Default value expression.
    pub default: Option<Spanned<Expr>>,
}

/// A type with source location and unique ID.
///
/// This is the primary type representation used throughout the compiler.
/// It wraps a `TyKind` with metadata (span, node ID) for error reporting.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Ty {
    /// Unique identifier for this type node.
    pub id: NodeId,
    /// The kind of type.
    pub kind: TyKind,
    /// Source location.
    pub span: Span,
}

impl Ty {
    /// Create a new type with a fresh ID.
    pub fn new(kind: TyKind, span: Span) -> Self {
        Self {
            id: NodeId::new(),
            kind,
            span,
        }
    }

    /// Create a dummy type (for testing or generated code).
    pub fn dummy(kind: TyKind) -> Self {
        Self {
            id: NodeId::dummy(),
            kind,
            span: Span::default(),
        }
    }
}

/// Type kinds - aligned with WIT specification.
///
/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TyKind {
    // WIT primitive types
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
    Char,
    String,

    // WIT compound types
    List(Box<Ty>),
    Option(Box<Ty>),
    Result {
        ok: Option<Box<Ty>>,
        err: Option<Box<Ty>>,
    },
    Tuple(Vec<Ty>),

    // UI-specific types (extensions)
    Length,
    PhysicalLength,
    Angle,
    Duration,
    Percent,
    RelativeFontSize,
    Color,
    Brush,
    Image,
    Easing,

    // Function type for callbacks/handlers
    Func {
        params: Vec<(String, Ty)>,
        return_type: Option<Box<Ty>>,
    },

    // Custom/named types (resources, records, etc.)
    Named(String),

    // Unknown type (for type inference)
    Unknown,
}

impl Serialize for TyKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        match self {
            // Primitives: { "kind": "bool" }
            TyKind::Bool => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "bool")?;
                map.end()
            }
            TyKind::S8 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "s8")?;
                map.end()
            }
            TyKind::S16 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "s16")?;
                map.end()
            }
            TyKind::S32 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "s32")?;
                map.end()
            }
            TyKind::S64 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "s64")?;
                map.end()
            }
            TyKind::U8 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "u8")?;
                map.end()
            }
            TyKind::U16 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "u16")?;
                map.end()
            }
            TyKind::U32 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "u32")?;
                map.end()
            }
            TyKind::U64 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "u64")?;
                map.end()
            }
            TyKind::F32 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "f32")?;
                map.end()
            }
            TyKind::F64 => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "f64")?;
                map.end()
            }
            TyKind::Char => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "char")?;
                map.end()
            }
            TyKind::String => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "string")?;
                map.end()
            }
            // UI types
            TyKind::Length => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "length")?;
                map.end()
            }
            TyKind::PhysicalLength => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "physical-length")?;
                map.end()
            }
            TyKind::Angle => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "angle")?;
                map.end()
            }
            TyKind::Duration => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "duration")?;
                map.end()
            }
            TyKind::Percent => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "percent")?;
                map.end()
            }
            TyKind::RelativeFontSize => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "relative-font-size")?;
                map.end()
            }
            TyKind::Color => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "color")?;
                map.end()
            }
            TyKind::Brush => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "brush")?;
                map.end()
            }
            TyKind::Image => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "image")?;
                map.end()
            }
            TyKind::Easing => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "easing")?;
                map.end()
            }
            TyKind::Unknown => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("kind", "unknown")?;
                map.end()
            }
            // Compound types: { "kind": "list", "of": {...} }
            TyKind::List(inner) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("kind", "list")?;
                map.serialize_entry("of", &inner.kind)?;
                map.end()
            }
            TyKind::Option(inner) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("kind", "option")?;
                map.serialize_entry("of", &inner.kind)?;
                map.end()
            }
            TyKind::Result { ok, err } => {
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("kind", "result")?;
                map.serialize_entry("ok", &ok.as_ref().map(|t| &t.kind))?;
                map.serialize_entry("err", &err.as_ref().map(|t| &t.kind))?;
                map.end()
            }
            TyKind::Tuple(types) => {
                let kinds: Vec<_> = types.iter().map(|t| &t.kind).collect();
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("kind", "tuple")?;
                map.serialize_entry("types", &kinds)?;
                map.end()
            }
            TyKind::Func { params, return_type } => {
                let params_kinds: Vec<_> = params.iter().map(|(n, t)| (n, &t.kind)).collect();
                let mut map = serializer.serialize_map(Some(3))?;
                map.serialize_entry("kind", "func")?;
                map.serialize_entry("params", &params_kinds)?;
                map.serialize_entry("return_type", &return_type.as_ref().map(|t| &t.kind))?;
                map.end()
            }
            TyKind::Named(name) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("kind", "named")?;
                map.serialize_entry("name", name)?;
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for TyKind {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, MapAccess, Visitor};

        struct TyKindVisitor;

        impl<'de> Visitor<'de> for TyKindVisitor {
            type Value = TyKind;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a type object with 'kind' field")
            }

            fn visit_map<M>(self, mut map: M) -> Result<TyKind, M::Error>
            where
                M: MapAccess<'de>,
            {
                let mut kind: Option<String> = None;
                let mut of: Option<Box<TyKind>> = None;
                let mut ok: Option<Option<Box<TyKind>>> = None;
                let mut err: Option<Option<Box<TyKind>>> = None;
                let mut types: Option<Vec<TyKind>> = None;
                let mut params: Option<Vec<(String, TyKind)>> = None;
                let mut return_type: Option<Option<Box<TyKind>>> = None;
                let mut name: Option<String> = None;

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "kind" => kind = Some(map.next_value()?),
                        "of" => of = Some(map.next_value()?),
                        "ok" => ok = Some(map.next_value()?),
                        "err" => err = Some(map.next_value()?),
                        "types" => types = Some(map.next_value()?),
                        "params" => params = Some(map.next_value()?),
                        "return_type" => return_type = Some(map.next_value()?),
                        "name" => name = Some(map.next_value()?),
                        _ => { let _: serde::de::IgnoredAny = map.next_value()?; }
                    }
                }

                let kind = kind.ok_or_else(|| de::Error::missing_field("kind"))?;

                match kind.as_str() {
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
                    "unknown" => Ok(TyKind::Unknown),
                    "list" => {
                        let inner = of.ok_or_else(|| de::Error::missing_field("of"))?;
                        Ok(TyKind::List(Box::new(Ty::dummy(*inner))))
                    }
                    "option" => {
                        let inner = of.ok_or_else(|| de::Error::missing_field("of"))?;
                        Ok(TyKind::Option(Box::new(Ty::dummy(*inner))))
                    }
                    "result" => {
                        Ok(TyKind::Result {
                            ok: ok.unwrap_or(None).map(|k| Box::new(Ty::dummy(*k))),
                            err: err.unwrap_or(None).map(|k| Box::new(Ty::dummy(*k))),
                        })
                    }
                    "tuple" => {
                        let types = types.ok_or_else(|| de::Error::missing_field("types"))?;
                        Ok(TyKind::Tuple(types.into_iter().map(Ty::dummy).collect()))
                    }
                    "func" => {
                        let params = params.ok_or_else(|| de::Error::missing_field("params"))?;
                        Ok(TyKind::Func {
                            params: params.into_iter().map(|(n, k)| (n, Ty::dummy(k))).collect(),
                            return_type: return_type.unwrap_or(None).map(|k| Box::new(Ty::dummy(*k))),
                        })
                    }
                    "named" => {
                        let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
                        Ok(TyKind::Named(name))
                    }
                    other => Err(de::Error::unknown_variant(other, &[
                        "bool", "s8", "s16", "s32", "s64", "u8", "u16", "u32", "u64",
                        "f32", "f64", "char", "string", "length", "physical-length",
                        "angle", "duration", "percent", "relative-font-size", "color",
                        "brush", "image", "easing", "unknown", "list", "option", "result",
                        "tuple", "func", "named"
                    ])),
                }
            }
        }

        deserializer.deserialize_map(TyKindVisitor)
    }
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Bool => write!(f, "bool"),
            TyKind::S8 => write!(f, "s8"),
            TyKind::S16 => write!(f, "s16"),
            TyKind::S32 => write!(f, "s32"),
            TyKind::S64 => write!(f, "s64"),
            TyKind::U8 => write!(f, "u8"),
            TyKind::U16 => write!(f, "u16"),
            TyKind::U32 => write!(f, "u32"),
            TyKind::U64 => write!(f, "u64"),
            TyKind::F32 => write!(f, "f32"),
            TyKind::F64 => write!(f, "f64"),
            TyKind::Char => write!(f, "char"),
            TyKind::String => write!(f, "string"),
            TyKind::List(inner) => write!(f, "list<{}>", inner.kind),
            TyKind::Option(inner) => write!(f, "option<{}>", inner.kind),
            TyKind::Result { ok, err } => {
                let ok_str = ok
                    .as_ref()
                    .map(|t| t.kind.to_string())
                    .unwrap_or_else(|| "_".to_string());
                let err_str = err
                    .as_ref()
                    .map(|t| t.kind.to_string())
                    .unwrap_or_else(|| "_".to_string());
                write!(f, "result<{}, {}>", ok_str, err_str)
            }
            TyKind::Tuple(types) => {
                let inner: Vec<_> = types.iter().map(|t| t.kind.to_string()).collect();
                write!(f, "tuple<{}>", inner.join(", "))
            }
            TyKind::Length => write!(f, "length"),
            TyKind::PhysicalLength => write!(f, "physical-length"),
            TyKind::Angle => write!(f, "angle"),
            TyKind::Duration => write!(f, "duration"),
            TyKind::Percent => write!(f, "percent"),
            TyKind::RelativeFontSize => write!(f, "relative-font-size"),
            TyKind::Color => write!(f, "color"),
            TyKind::Brush => write!(f, "brush"),
            TyKind::Image => write!(f, "image"),
            TyKind::Easing => write!(f, "easing"),
            TyKind::Func { params, return_type } => {
                let param_strs: Vec<_> = params
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty.kind))
                    .collect();
                match return_type {
                    Some(ret) => write!(f, "func({}) -> {}", param_strs.join(", "), ret.kind),
                    None => write!(f, "func({})", param_strs.join(", ")),
                }
            }
            TyKind::Named(name) => write!(f, "{}", name),
            TyKind::Unknown => write!(f, "unknown"),
        }
    }
}

/// A UI node in the tree.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Node {
    /// An element or component instantiation.
    Element(ElementNode),
    /// Bare text content.
    Text(TextNode),
    /// Conditional rendering. Boxed: `IfNode`/`ForNode` are large (they embed
    /// `Spanned<Expr>` and several node vectors), and conditionals/loops are
    /// far rarer than `Element`/`Text`, so keeping them inline would bloat
    /// every `Node` slot.
    If(Box<IfNode>),
    /// List rendering.
    For(Box<ForNode>),
    /// Slot marker (`@children`) — when the enclosing component is used at
    /// a call site, the caller's child nodes splice in at this position.
    /// Exactly one `@children` per component body; zero means the component
    /// rejects caller-supplied children at type-check time.
    Children,
}

// `Node` is stored in `Vec<Spanned<Node>>` throughout the AST; the large
// `If`/`For` payloads are boxed to keep it small. Guard against regressions.
const _: () = assert!(std::mem::size_of::<Node>() <= 120);

/// An element or component node.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ElementNode {
    /// Element/component name (e.g., "div", "Button", "MyComponent").
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Property bindings.
    pub bindings: Vec<Spanned<Binding>>,
    /// Handler bindings.
    pub handlers: Vec<Spanned<Handler>>,
    /// Child nodes.
    pub children: Vec<Spanned<Node>>,
}

/// A text node (bare string content).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TextNode {
    /// Text content (may include interpolations).
    pub content: Spanned<Expr>,
}

/// Property binding modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PropModifier {
    /// No modifier - regular property binding (getter).
    None,
    /// `set` modifier - handler called when property changes from outside (e.g., user input).
    Set,
    /// `bind` modifier - two-way binding: value is the target signal, and a
    /// synthetic auto-sync input-event handler is generated (no user body).
    Bind,
}

/// A property binding (kept separate in AST, merged in HIR).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Binding {
    /// Optional modifier (e.g., `set`).
    pub modifier: PropModifier,
    /// Property name.
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Bound value expression.
    pub value: Spanned<Expr>,
}

/// An if/else-if/else conditional block.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfNode {
    /// Main condition.
    pub condition: Spanned<Expr>,
    /// Then branch nodes.
    pub then_branch: Vec<Spanned<Node>>,
    /// Else-if branches: (condition, nodes).
    pub else_if_branches: Vec<(Spanned<Expr>, Vec<Spanned<Node>>)>,
    /// Optional else branch.
    pub else_branch: Option<Vec<Spanned<Node>>>,
}

/// A for loop block: `for item in collection { ... }`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForNode {
    /// Loop variable name.
    pub item_name: String,
    /// Span of item name.
    pub item_name_span: Span,
    /// Iterable expression.
    pub iterable: Spanned<Expr>,
    /// Optional key expression for efficient diffing.
    pub key: Option<Spanned<Expr>>,
    /// Loop body nodes.
    pub body: Vec<Spanned<Node>>,
}

/// A handler binding.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Handler {
    /// Handler name (e.g., "onclick", "clicked").
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Handler body statements.
    pub body: Vec<Spanned<Statement>>,
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    /// Identifier reference.
    Ident(String),
    /// Literal value.
    Literal(Literal),
    /// Binary operation.
    Binary(Box<Spanned<Expr>>, String, Box<Spanned<Expr>>),
    /// Unary operation.
    Unary(String, Box<Spanned<Expr>>),
    /// Function call.
    Call(String, Vec<Spanned<Expr>>),
    /// Path call (e.g., `Type.case(args)` for variant constructors).
    PathCall {
        base: Box<Spanned<Expr>>,
        member: String,
        args: Vec<Spanned<Expr>>,
    },
    /// Member access (e.g., `obj.field`).
    Member(Box<Spanned<Expr>>, String),
    /// Optional member access (e.g., `obj?.field`).
    OptionalMember(Box<Spanned<Expr>>, String),
    /// Method call (e.g., `list.filter(closure)`).
    MethodCall {
        receiver: Box<Spanned<Expr>>,
        method: String,
        method_span: Span,
        args: Vec<Spanned<Expr>>,
    },
    /// Index access (e.g., `arr[0]`).
    Index(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    /// String interpolation: "Hello {name}!"
    Interpolation(Vec<InterpolationPart>),
    /// Range expression: `0..5` or `0..=5`
    Range {
        start: Box<Spanned<Expr>>,
        end: Box<Spanned<Expr>>,
        inclusive: bool,
    },
    /// Ternary/conditional expression: `condition ? then : else`
    Ternary {
        condition: Box<Spanned<Expr>>,
        then_expr: Box<Spanned<Expr>>,
        else_expr: Box<Spanned<Expr>>,
    },
    /// Closure expression: `|params| { statements }`
    Closure {
        params: Vec<(String, Ty)>,
        body: Vec<Spanned<Statement>>,
    },
}

/// A literal value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    /// Value with unit: (value, unit) e.g., (8.0, "px")
    Unit(f64, String),
    /// Color in hex: #rrggbb or #rrggbbaa
    Color(String),
    /// List literal: [1, 2, 3]
    List(Vec<Spanned<Expr>>),
    /// Tuple literal: (1, 2, 3)
    Tuple(Vec<Spanned<Expr>>),
    /// Record literal (anonymous): { id: "...", name: "..." }
    /// Type is inferred from context.
    Record {
        fields: Vec<(String, Spanned<Expr>)>,
    },
}

/// Part of a string interpolation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum InterpolationPart {
    /// Literal string segment.
    Literal(String),
    /// Interpolated expression.
    Expr(Spanned<Expr>),
}

/// A statement in an event handler.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    /// Expression statement.
    Expr(Spanned<Expr>),
    /// Assignment: `target = value`.
    Assign(Spanned<Expr>, Spanned<Expr>),
    /// Compound assignment: `target += value`.
    CompoundAssign(Spanned<Expr>, String, Spanned<Expr>),
    /// If statement.
    If {
        condition: Spanned<Expr>,
        then_branch: Vec<Spanned<Statement>>,
        else_branch: Option<Vec<Spanned<Statement>>>,
    },
    /// Let binding: `let name: type = value;`
    Let {
        name: String,
        name_span: Span,
        ty: Option<Ty>,
        value: Spanned<Expr>,
    },
}

// Keep PropertyDirection for backward compatibility with analyzer/codegen
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum PropertyDirection {
    In,
    Out,
    InOut,
    Private,
}

// Legacy Callback for backward compatibility
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Callback {
    pub name: String,
    pub name_span: Span,
    pub params: Vec<(String, Ty)>,
}

/// A function declaration: `[export] name: func(params) -> ret;`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDecl {
    /// Function name.
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Whether this function is exported.
    pub is_export: bool,
    /// Function parameters: (name, type).
    pub params: Vec<(String, Ty)>,
    /// Optional return type.
    pub return_type: Option<Ty>,
}

/// A record type declaration: `record Name { field: type, ... }`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Record {
    /// Record name (PascalCase).
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Record fields: (name, type).
    pub fields: Vec<Spanned<RecordField>>,
}

/// A field in a record type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordField {
    /// Field name.
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Field type.
    pub ty: Ty,
}

/// An enum type declaration (WIT-style, no payloads):
/// `enum color { red, green, blue }`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Enum {
    /// Enum name (PascalCase).
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Enum cases (simple names, no payloads).
    pub cases: Vec<Spanned<String>>,
}

/// A variant type declaration (WIT-style, with optional payloads):
/// `variant filter { all, none, some(list<string>) }`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variant {
    /// Variant name.
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Variant cases (with optional payload types).
    pub cases: Vec<Spanned<VariantCase>>,
}

/// A single case in a variant type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariantCase {
    /// Case name (kebab-case).
    pub name: String,
    /// Optional payload type.
    pub payload: Option<Ty>,
}

/// An intrinsic element type declaration: `element HStack { alignment: option<Alignment>; }`
/// Elements are primitive UI building blocks provided by the runtime.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Element {
    /// Element name (PascalCase).
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Element properties (including function-typed callbacks).
    pub properties: Vec<Spanned<Property>>,
}

/// An imported component declaration: `import component Dialog { name: string; func show(); }`
/// Imported components are external components provided by the host or other modules.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImportComponent {
    /// Component name (PascalCase).
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Component properties (including function-typed callbacks).
    pub properties: Vec<Spanned<Property>>,
    /// Methods that can be called on the component instance.
    pub methods: Vec<Spanned<FunctionDecl>>,
    /// `true` when the declaration body includes `@children;`. Signals to
    /// the module type-checker that callers may pass child nodes inside
    /// `X { ... }` and that the host's `mount` returns a children-root id.
    pub has_children_slot: bool,
}

/// A global property declaration: `in-out name: type = default;`
///
/// Direction semantics:
/// - No direction (with optional default): pure in-tree shared state. The
///   component owns the slot; writes re-render every reader. No WIT emission.
/// - `In`: host pushes via imported setter.
/// - `Out`: component writes; host is notified.
/// - `InOut`: both.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GlobalProperty {
    /// Direction (defaults to `In`).
    pub direction: PropertyDirection,
    /// Property name.
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Property type.
    pub ty: Ty,
    /// Optional default value.
    pub default: Option<Spanned<Expr>>,
}

/// A global singleton declaration.
///
/// ```text
/// // Pure in-tree shared state — no WIT emission.
/// global AppState {
///     count: s32 = 0;
/// }
///
/// // Host-boundary interface — emits a WIT interface.
/// export global Theme {
///     in dark-mode: bool;
///     callback toggle-dark-mode();
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Global {
    /// Global name (PascalCase by convention).
    pub name: String,
    /// Span of the name.
    pub name_span: Span,
    /// Whether this global's WIT interface is published for other packages
    /// to import. Not a gate on WIT emission itself — that depends on
    /// whether any member crosses the host boundary.
    pub is_export: bool,
    /// Properties.
    pub properties: Vec<Spanned<GlobalProperty>>,
    /// Callbacks (host implements).
    pub callbacks: Vec<Spanned<FunctionDecl>>,
}
