// AST types matching yel-dsl structure

export interface Span {
  source: number;
  start: number;
  end: number;
}

export interface Spanned<T> {
  node: T;
  span: Span;
}

export interface AstFile {
  package?: PackageId;
  records: Spanned<AstRecord>[];
  enums: Spanned<Enum>[];
  variants: Spanned<Variant>[];
  components: Spanned<Component>[];
}

export interface PackageId {
  namespace: string;
  name: string;
  version?: string;
}

export interface Component {
  name: string;
  name_span: Span;
  is_export: boolean;
  properties: Spanned<Property>[];
  functions: Spanned<FunctionDecl>[];
  body: Spanned<Node>[];
}

export interface Property {
  name: string;
  name_span: Span;
  ty: Spanned<Type>;
  default?: Spanned<Expr>;
}

export interface FunctionDecl {
  name: string;
  name_span: Span;
  is_export: boolean;
  params: [string, Spanned<Type>][];
  return_type?: Spanned<Type>;
}

// Type follows Rust serde serialization:
// - Primitive types: "Bool", "S8", "S16", "S32", "S64", "U8", "U16", "U32", "U64", "F32", "F64", "Char", "String"
// - Compound: {"List": inner}, {"Option": inner}, {"Result": {ok, err}}, {"Tuple": types}
// - UI: "Length", "PhysicalLength", "Angle", etc.
// - Function: {"Func": {params, return_type}}
// - Custom: {"Named": name}
export type Type =
  | "Bool" | "S8" | "S16" | "S32" | "S64" | "U8" | "U16" | "U32" | "U64" | "F32" | "F64" | "Char" | "String"
  | "Length" | "PhysicalLength" | "Angle" | "Duration" | "Percent" | "RelativeFontSize" | "Color" | "Brush" | "Image" | "Easing"
  | { List: Spanned<Type> }
  | { Option: Spanned<Type> }
  | { Result: { ok?: Spanned<Type>; err?: Spanned<Type> } }
  | { Tuple: Spanned<Type>[] }
  | { Func: { params: [string, Spanned<Type>][]; return_type?: Spanned<Type> } }
  | { Named: string };

// Node follows Rust serde serialization: { Element: ... } | { Text: ... } | { If: ... } | { For: ... }
export type Node =
  | { Element: ElementNode }
  | { Text: TextNode }
  | { If: IfNode }
  | { For: ForNode };

export interface ElementNode {
  name: string;
  name_span: Span;
  bindings: Spanned<Binding>[];
  handlers: Spanned<Handler>[];
  children: Spanned<Node>[];
}

export interface TextNode {
  content: Spanned<Expr>;
}

export interface IfNode {
  condition: Spanned<Expr>;
  then_branch: Spanned<Node>[];
  else_if_branches: [Spanned<Expr>, Spanned<Node>[]][];
  else_branch?: Spanned<Node>[];
}

export interface ForNode {
  item_name: string;
  item_name_span: Span;
  iterable: Spanned<Expr>;
  key?: Spanned<Expr>;
  body: Spanned<Node>[];
}

export interface Binding {
  name: string;
  name_span: Span;
  value: Spanned<Expr>;
}

export interface Handler {
  name: string;
  name_span: Span;
  body: Spanned<Statement>[];
}

// Expr follows Rust serde serialization
export type Expr =
  | { Ident: string }
  | { Literal: Literal }
  | { Binary: [Spanned<Expr>, string, Spanned<Expr>] }
  | { Unary: [string, Spanned<Expr>] }
  | { Call: [string, Spanned<Expr>[]] }
  | { Member: [Spanned<Expr>, string] }
  | { OptionalMember: [Spanned<Expr>, string] }
  | { Index: [Spanned<Expr>, Spanned<Expr>] }
  | { Interpolation: InterpolationPart[] }
  | { Range: { start: Spanned<Expr>; end: Spanned<Expr>; inclusive: boolean } }
  | { Ternary: { condition: Spanned<Expr>; then_expr: Spanned<Expr>; else_expr: Spanned<Expr> } }
  | { Closure: { params: [string, Spanned<Type>][]; body: Spanned<Statement>[] } };

// Literal follows Rust serde serialization
export type Literal =
  | { Int: number }
  | { Float: number }
  | { String: string }
  | { Bool: boolean }
  | { Unit: [number, string] }
  | { Color: string }
  | { List: Spanned<Expr>[] }
  | { Tuple: Spanned<Expr>[] }
  | { Record: { fields: [string, Spanned<Expr>][] } };

// InterpolationPart follows Rust serde serialization
export type InterpolationPart =
  | { Literal: string }
  | { Expr: Spanned<Expr> };

// Statement follows Rust serde serialization
export type Statement =
  | { Expr: Spanned<Expr> }
  | { Assign: [Spanned<Expr>, Spanned<Expr>] }
  | { CompoundAssign: [Spanned<Expr>, string, Spanned<Expr>] }
  | { If: { condition: Spanned<Expr>; then_branch: Spanned<Statement>[]; else_branch?: Spanned<Statement>[] } };

export interface AstRecord {
  name: string;
  name_span: Span;
  fields: Spanned<RecordField>[];
}

export interface RecordField {
  name: string;
  name_span: Span;
  ty: Spanned<Type>;
}

export interface Enum {
  name: string;
  name_span: Span;
  cases: Spanned<string>[];
}

export interface Variant {
  name: string;
  name_span: Span;
  cases: Spanned<VariantCase>[];
}

export interface VariantCase {
  name: string;
  payload?: Spanned<Type>;
}

// Tree node for display
export interface TreeNode {
  id: string;
  label: string;
  kind: string;
  span: Span;
  details?: Record<string, unknown>;
  children: TreeNode[];
}

// =============================================================================
// HIR Types - High-level IR with resolved names
// Matches the Rust serialization from yel-dsl-next/src/hir/
// =============================================================================

/** HIR output from compiler - array of components */
export type HirOutput = HirComponent[];

/** Interned type handle (u32 index) */
export type Ty = number;

/** Definition ID (u32 index) */
export type DefId = number;

/** Local variable ID (u32 index) */
export type LocalId = number;

/** Node ID (u32 index) */
export type NodeId = number;

/** Interned name (usize index) */
export type Name = number;

/** HIR Component - matches Rust HirComponent */
export interface HirComponent {
  def_id: DefId;
  name: Name;
  span: Span;
  is_export: boolean;
  body: HirNode[];
}

/** HIR Node - matches Rust HirNode */
export interface HirNode {
  id: NodeId;
  kind: HirNodeKind;
  span: Span;
}

/** HIR Node Kind - matches Rust HirNodeKind */
export type HirNodeKind =
  | { Element: { name: string; bindings: HirBinding[]; handlers: HirHandler[]; children: HirNode[] } }
  | { Text: HirExpr }
  | { If: { condition: HirExpr; then_branch: HirNode[]; else_if_branches: [HirExpr, HirNode[]][]; else_branch?: HirNode[] } }
  | { For: { item: LocalId; item_name: Name; item_span: Span; item_ty: Ty; iterable: HirExpr; key?: HirExpr; body: HirNode[] } };

/** HIR Binding - matches Rust HirBinding */
export interface HirBinding {
  name: string;
  name_span: Span;
  value: HirExpr;
}

/** HIR Handler - matches Rust HirHandler */
export interface HirHandler {
  name: string;
  name_span: Span;
  body: HirStatement[];
}

/** HIR Expression - matches Rust HirExpr */
export interface HirExpr {
  kind: HirExprKind;
  span: Span;
}

/** HIR Expression Kind - matches Rust HirExprKind */
export type HirExprKind =
  | { Local: LocalId }
  | { Def: DefId }
  | { Literal: HirLiteral }
  | { Binary: { op: BinOp; lhs: HirExpr; rhs: HirExpr } }
  | { Unary: { op: UnaryOp; operand: HirExpr } }
  | { Field: { base: HirExpr; field: string } }
  | { OptionalField: { base: HirExpr; field: string } }
  | { Index: { base: HirExpr; index: HirExpr } }
  | { Call: { func: string; args: HirExpr[] } }
  | { PathCall: { base: string; member: string; args: HirExpr[] } }
  | { Range: { start: HirExpr; end: HirExpr; inclusive: boolean } }
  | { Ternary: { condition: HirExpr; then_expr: HirExpr; else_expr: HirExpr } }
  | { Closure: { params: [string, Ty][]; body: HirStatement[] } }
  | { Interpolation: HirInterpolationPart[] }
  | { Path: { segments: string[] } }
  | "Error";

/** HIR Literal - matches Rust HirLiteral */
export type HirLiteral =
  | { Int: number }
  | { Float: number }
  | { String: string }
  | { Char: string }
  | { Bool: boolean }
  | { Unit: [number, string] }
  | { Color: string }
  | { List: HirExpr[] }
  | { Tuple: HirExpr[] }
  | { Record: { fields: [string, HirExpr][] } };

/** HIR Interpolation Part - matches Rust HirInterpolationPart */
export type HirInterpolationPart =
  | { Literal: string }
  | { Expr: HirExpr };

/** Binary operators - matches Rust BinOp */
export type BinOp =
  | "Add" | "Sub" | "Mul" | "Div" | "Mod"
  | "Eq" | "Ne" | "Lt" | "Le" | "Gt" | "Ge"
  | "And" | "Or"
  | "BitAnd" | "BitOr" | "BitXor";

/** Unary operators - matches Rust UnaryOp */
export type UnaryOp = "Neg" | "Not";

/** HIR Statement - matches Rust HirStatement */
export type HirStatement =
  | { Expr: HirExpr }
  | { Assign: { target: HirExpr; value: HirExpr } }
  | { CompoundAssign: { target: HirExpr; op: BinOp; value: HirExpr } }
  | { If: { condition: HirExpr; then_branch: HirStatement[]; else_branch?: HirStatement[] } };

// Legacy aliases for compatibility
export type HirStmt = HirStatement;
export type SymbolId = number;
export type ResolvedType = string | { [key: string]: unknown };

// =============================================================================
// THIR Types - Typed HIR with full type information
// =============================================================================

/** THIR output from compiler - component + symbol table + types */
export interface ThirOutput {
  component: ThirComponent;
  symbols: HirSymbolTable;
  types: TypeRegistry;
}

/** THIR Component */
export interface ThirComponent {
  name: string;
  is_export: boolean;
  properties: ThirProperty[];
  callbacks: ThirCallback[];
  body: ThirNode[];
  span: Span;
}

/** THIR Property */
export interface ThirProperty {
  symbol: SymbolId;
  ty: ResolvedType;
  default?: ThirExpr;
  span: Span;
}

/** THIR Callback */
export interface ThirCallback {
  symbol: SymbolId;
  params: [SymbolId, ResolvedType][];
  return_ty?: ResolvedType;
  is_export: boolean;
  span: Span;
}

/** THIR Node */
export interface ThirNode {
  kind: ThirNodeKind;
  span: Span;
}

/** THIR Node Kind */
export type ThirNodeKind =
  | { Element: { name: string; is_component: boolean; bindings: ThirBinding[]; handlers: ThirHandler[]; children: ThirNode[] } }
  | { Text: { content: ThirExpr } }
  | { If: { condition: ThirExpr; then_branch: ThirNode[]; else_ifs: [ThirExpr, ThirNode[]][]; else_branch?: ThirNode[] } }
  | { For: { item: SymbolId; item_ty: ResolvedType; iterable: ThirExpr; key?: ThirExpr; body: ThirNode[] } };

/** THIR Binding */
export interface ThirBinding {
  name: string;
  value: ThirExpr;
  expected_ty: ResolvedType;
  span: Span;
}

/** THIR Handler */
export interface ThirHandler {
  name: string;
  body: ThirStmt[];
  span: Span;
}

/** THIR Expression - every expression has a type */
export interface ThirExpr {
  kind: ThirExprKind;
  ty: ResolvedType;
  span: Span;
}

/** THIR Expression Kind */
export type ThirExprKind =
  | { Var: SymbolId }
  | { Literal: HirLiteral }
  | { Binary: { op: BinOp; lhs: ThirExpr; rhs: ThirExpr } }
  | { Unary: { op: UnaryOp; operand: ThirExpr } }
  | { Call: { func: SymbolId; args: ThirExpr[] } }
  | { MethodCall: { receiver: ThirExpr; method: string; args: ThirExpr[] } }
  | { Field: { base: ThirExpr; field: string } }
  | { Index: { base: ThirExpr; index: ThirExpr } }
  | { Match: { scrutinee: ThirExpr; arms: ThirMatchArm[] } }
  | { If: { cond: ThirExpr; then_expr: ThirExpr; else_expr: ThirExpr } }
  | { Range: { start: ThirExpr; end: ThirExpr; inclusive: boolean } }
  | { Closure: { params: [SymbolId, ResolvedType][]; body: ThirStmt[]; return_ty?: ResolvedType } }
  | { RecordLit: { ty_name: string; fields: [string, ThirExpr][] } }
  | { VariantCtor: { ty_name: string; case: string; payload?: ThirExpr } }
  | { EnumCase: { ty_name: string; case: string } }
  | { Tuple: ThirExpr[] }
  | { List: ThirExpr[] }
  | { Coerce: { expr: ThirExpr; from: ResolvedType; to: ResolvedType } }
  | "Error";

/** THIR Match Arm */
export interface ThirMatchArm {
  pattern: ThirPattern;
  guard?: ThirExpr;
  body: ThirExpr;
}

/** THIR Pattern */
export type ThirPattern =
  | "Wildcard"
  | { Var: SymbolId }
  | { Some: ThirPattern }
  | "None"
  | { Ok: ThirPattern }
  | { Err: ThirPattern }
  | { Variant: { ty_name: string; case: string; binding?: SymbolId } }
  | { Literal: HirLiteral };

/** THIR Statement */
export type ThirStmt =
  | { Expr: ThirExpr }
  | { Assign: { target: ThirExpr; value: ThirExpr } }
  | { If: { cond: ThirExpr; then_branch: ThirStmt[]; else_branch?: ThirStmt[] } };
