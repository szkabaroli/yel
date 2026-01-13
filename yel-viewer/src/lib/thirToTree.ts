import type {
  ThirOutput,
  ThirComponent,
  ThirProperty,
  ThirCallback,
  ThirNode,
  ThirNodeKind,
  ThirBinding,
  ThirHandler,
  ThirExpr,
  ThirExprKind,
  ThirStmt,
  ThirMatchArm,
  ThirPattern,
  HirLiteral,
  HirSymbolTable,
  ResolvedType,
  SymbolId,
  TreeNode,
  Span,
} from "./types";

let idCounter = 0;

function nextId(): string {
  return `thir-${idCounter++}`;
}

export function resetThirIdCounter(): void {
  idCounter = 0;
}

// Symbol table for looking up names
let symbolTable: HirSymbolTable | null = null;

function getSymbolName(id: SymbolId): string {
  if (!symbolTable) return `sym#${id}`;
  const sym = symbolTable.symbols.find((s) => s.id === id);
  return sym ? sym.name : `sym#${id}`;
}

function resolvedTypeToString(ty: ResolvedType): string {
  if (typeof ty === "string") return ty.toLowerCase();
  if ("List" in ty) return `list<${resolvedTypeToString(ty.List)}>`;
  if ("Option" in ty) return `option<${resolvedTypeToString(ty.Option)}>`;
  if ("Result" in ty) {
    const ok = ty.Result.ok ? resolvedTypeToString(ty.Result.ok) : "_";
    const err = ty.Result.err ? resolvedTypeToString(ty.Result.err) : "_";
    return `result<${ok}, ${err}>`;
  }
  if ("Tuple" in ty) return `(${ty.Tuple.map(resolvedTypeToString).join(", ")})`;
  if ("Func" in ty) {
    const params = ty.Func.params.map(([n, t]) => `${n}: ${resolvedTypeToString(t)}`).join(", ");
    const ret = ty.Func.return_type ? ` -> ${resolvedTypeToString(ty.Func.return_type)}` : "";
    return `func(${params})${ret}`;
  }
  if ("Named" in ty) return ty.Named;
  return "unknown";
}

function hirLiteralToString(lit: HirLiteral): string {
  if ("Int" in lit) return String(lit.Int);
  if ("Float" in lit) return String(lit.Float);
  if ("String" in lit) return `"${lit.String}"`;
  if ("Bool" in lit) return String(lit.Bool);
  if ("Unit" in lit) return `${lit.Unit.value}${lit.Unit.unit}`;
  if ("Color" in lit) return `rgba(${lit.Color.r}, ${lit.Color.g}, ${lit.Color.b}, ${lit.Color.a})`;
  return "?";
}

function thirExprToString(expr: ThirExpr): string {
  const kind = expr.kind;
  if (kind === "Error") return "<error>";
  if ("Var" in kind) return getSymbolName(kind.Var);
  if ("Literal" in kind) return hirLiteralToString(kind.Literal);
  if ("Binary" in kind) return `${thirExprToString(kind.Binary.lhs)} ${kind.Binary.op} ${thirExprToString(kind.Binary.rhs)}`;
  if ("Unary" in kind) return `${kind.Unary.op}${thirExprToString(kind.Unary.operand)}`;
  if ("Call" in kind) return `${getSymbolName(kind.Call.func)}(...)`;
  if ("MethodCall" in kind) return `${thirExprToString(kind.MethodCall.receiver)}.${kind.MethodCall.method}(...)`;
  if ("Field" in kind) return `${thirExprToString(kind.Field.base)}.${kind.Field.field}`;
  if ("Index" in kind) return `${thirExprToString(kind.Index.base)}[...]`;
  if ("Match" in kind) return "match ...";
  if ("If" in kind) return `${thirExprToString(kind.If.cond)} ? ... : ...`;
  if ("Range" in kind) return `${thirExprToString(kind.Range.start)}..${thirExprToString(kind.Range.end)}`;
  if ("Closure" in kind) return "{ ... }";
  if ("RecordLit" in kind) return `${kind.RecordLit.ty_name} { ... }`;
  if ("VariantCtor" in kind) return `${kind.VariantCtor.ty_name}.${kind.VariantCtor.case}`;
  if ("Tuple" in kind) return `(${kind.Tuple.map(thirExprToString).join(", ")})`;
  if ("List" in kind) return `[${kind.List.map(thirExprToString).join(", ")}]`;
  if ("EnumCase" in kind) return `${kind.EnumCase.ty_name}.${kind.EnumCase.case}`;
  if ("Coerce" in kind) return `${thirExprToString(kind.Coerce.expr)} as ${resolvedTypeToString(kind.Coerce.to)}`;
  return "?";
}

function thirPatternToString(pat: ThirPattern): string {
  if (pat === "Wildcard") return "_";
  if (pat === "None") return "none";
  if (typeof pat === "object") {
    if ("Var" in pat) return getSymbolName(pat.Var);
    if ("Some" in pat) return `some(${thirPatternToString(pat.Some)})`;
    if ("Ok" in pat) return `ok(${thirPatternToString(pat.Ok)})`;
    if ("Err" in pat) return `err(${thirPatternToString(pat.Err)})`;
    if ("Variant" in pat) {
      const binding = pat.Variant.binding ? `(${getSymbolName(pat.Variant.binding)})` : "";
      return `${pat.Variant.ty_name}.${pat.Variant.case}${binding}`;
    }
    if ("Literal" in pat) return hirLiteralToString(pat.Literal);
  }
  return "?";
}

function convertThirExpr(expr: ThirExpr): TreeNode {
  const kind = expr.kind;
  const typeStr = resolvedTypeToString(expr.ty);
  const base = { id: nextId(), span: expr.span, children: [] as TreeNode[] };

  if (kind === "Error") {
    return { ...base, label: `<error> : ${typeStr}`, kind: "Error" };
  }
  if ("Var" in kind) {
    return { ...base, label: `${getSymbolName(kind.Var)} : ${typeStr}`, kind: "Var", details: { symbol: kind.Var, type: typeStr } };
  }
  if ("Literal" in kind) {
    return { ...base, label: `${hirLiteralToString(kind.Literal)} : ${typeStr}`, kind: "Literal", details: { type: typeStr } };
  }
  if ("Binary" in kind) {
    return {
      ...base,
      label: `Binary (${kind.Binary.op}) : ${typeStr}`,
      kind: "Binary",
      details: { op: kind.Binary.op, type: typeStr },
      children: [convertThirExpr(kind.Binary.lhs), convertThirExpr(kind.Binary.rhs)],
    };
  }
  if ("Unary" in kind) {
    return {
      ...base,
      label: `Unary (${kind.Unary.op}) : ${typeStr}`,
      kind: "Unary",
      details: { op: kind.Unary.op, type: typeStr },
      children: [convertThirExpr(kind.Unary.operand)],
    };
  }
  if ("Call" in kind) {
    return {
      ...base,
      label: `Call ${getSymbolName(kind.Call.func)}() : ${typeStr}`,
      kind: "Call",
      details: { func: kind.Call.func, type: typeStr },
      children: kind.Call.args.map(convertThirExpr),
    };
  }
  if ("MethodCall" in kind) {
    return {
      ...base,
      label: `.${kind.MethodCall.method}() : ${typeStr}`,
      kind: "MethodCall",
      details: { method: kind.MethodCall.method, type: typeStr },
      children: [convertThirExpr(kind.MethodCall.receiver), ...kind.MethodCall.args.map(convertThirExpr)],
    };
  }
  if ("Field" in kind) {
    return {
      ...base,
      label: `.${kind.Field.field} : ${typeStr}`,
      kind: "Field",
      details: { field: kind.Field.field, type: typeStr },
      children: [convertThirExpr(kind.Field.base)],
    };
  }
  if ("Index" in kind) {
    return {
      ...base,
      label: `Index : ${typeStr}`,
      kind: "Index",
      details: { type: typeStr },
      children: [convertThirExpr(kind.Index.base), convertThirExpr(kind.Index.index)],
    };
  }
  if ("Match" in kind) {
    return {
      ...base,
      label: `Match : ${typeStr}`,
      kind: "Match",
      details: { type: typeStr },
      children: [
        convertThirExpr(kind.Match.scrutinee),
        ...kind.Match.arms.map((arm) => convertThirMatchArm(arm)),
      ],
    };
  }
  if ("If" in kind) {
    return {
      ...base,
      label: `If : ${typeStr}`,
      kind: "If",
      details: { type: typeStr },
      children: [
        convertThirExpr(kind.If.cond),
        convertThirExpr(kind.If.then_expr),
        convertThirExpr(kind.If.else_expr),
      ],
    };
  }
  if ("Range" in kind) {
    return {
      ...base,
      label: `Range ${kind.Range.inclusive ? "(..=)" : "(..)"} : ${typeStr}`,
      kind: "Range",
      details: { type: typeStr },
      children: [convertThirExpr(kind.Range.start), convertThirExpr(kind.Range.end)],
    };
  }
  if ("Closure" in kind) {
    const params = kind.Closure.params.map(([id]) => getSymbolName(id)).join(", ");
    return {
      ...base,
      label: `Closure (${params}) : ${typeStr}`,
      kind: "Closure",
      details: { type: typeStr },
      children: kind.Closure.body.map(convertThirStmt),
    };
  }
  if ("RecordLit" in kind) {
    return {
      ...base,
      label: `${kind.RecordLit.ty_name} { ... } : ${typeStr}`,
      kind: "RecordLit",
      details: { ty_name: kind.RecordLit.ty_name, type: typeStr },
      children: kind.RecordLit.fields.map(([name, val]) => ({
        id: nextId(),
        label: name,
        kind: "Field",
        span: val.span,
        children: [convertThirExpr(val)],
      })),
    };
  }
  if ("VariantCtor" in kind) {
    const children = kind.VariantCtor.payload ? [convertThirExpr(kind.VariantCtor.payload)] : [];
    return {
      ...base,
      label: `${kind.VariantCtor.ty_name}.${kind.VariantCtor.case} : ${typeStr}`,
      kind: "VariantCtor",
      details: { type: typeStr },
      children,
    };
  }
  if ("Tuple" in kind) {
    return { ...base, label: `Tuple : ${typeStr}`, kind: "Tuple", details: { type: typeStr }, children: kind.Tuple.map(convertThirExpr) };
  }
  if ("List" in kind) {
    return { ...base, label: `List : ${typeStr}`, kind: "List", details: { type: typeStr }, children: kind.List.map(convertThirExpr) };
  }
  if ("EnumCase" in kind) {
    return {
      ...base,
      label: `${kind.EnumCase.ty_name}.${kind.EnumCase.case} : ${typeStr}`,
      kind: "EnumCase",
      details: { type: typeStr },
    };
  }
  if ("Coerce" in kind) {
    return {
      ...base,
      label: `Coerce (${resolvedTypeToString(kind.Coerce.from)} -> ${resolvedTypeToString(kind.Coerce.to)})`,
      kind: "Coerce",
      details: { from: resolvedTypeToString(kind.Coerce.from), to: resolvedTypeToString(kind.Coerce.to) },
      children: [convertThirExpr(kind.Coerce.expr)],
    };
  }
  return { ...base, label: "Unknown", kind: "Unknown" };
}

function convertThirMatchArm(arm: ThirMatchArm): TreeNode {
  const children = [convertThirExpr(arm.body)];
  if (arm.guard) {
    children.unshift({
      id: nextId(),
      label: `guard: ${thirExprToString(arm.guard)}`,
      kind: "Guard",
      span: arm.guard.span,
      children: [convertThirExpr(arm.guard)],
    });
  }
  return {
    id: nextId(),
    label: thirPatternToString(arm.pattern),
    kind: "MatchArm",
    span: { source: 0, start: 0, end: 0 },
    children,
  };
}

function convertThirStmt(stmt: ThirStmt): TreeNode {
  const base = { id: nextId(), span: { source: 0, start: 0, end: 0 } as Span, children: [] as TreeNode[] };

  if ("Expr" in stmt) {
    return {
      ...base,
      label: thirExprToString(stmt.Expr),
      kind: "ExprStmt",
      span: stmt.Expr.span,
      children: [convertThirExpr(stmt.Expr)],
    };
  }
  if ("Assign" in stmt) {
    return {
      ...base,
      label: `${thirExprToString(stmt.Assign.target)} = ${thirExprToString(stmt.Assign.value)}`,
      kind: "Assign",
      children: [convertThirExpr(stmt.Assign.target), convertThirExpr(stmt.Assign.value)],
    };
  }
  if ("If" in stmt) {
    return {
      ...base,
      label: `if ${thirExprToString(stmt.If.cond)}`,
      kind: "IfStmt",
      children: [
        convertThirExpr(stmt.If.cond),
        ...stmt.If.then_branch.map(convertThirStmt),
        ...(stmt.If.else_branch?.map(convertThirStmt) ?? []),
      ],
    };
  }
  return { ...base, label: "Unknown", kind: "Unknown" };
}

function convertThirBinding(binding: ThirBinding): TreeNode {
  return {
    id: nextId(),
    label: `${binding.name} = ${thirExprToString(binding.value)} (expects: ${resolvedTypeToString(binding.expected_ty)})`,
    kind: "Binding",
    span: binding.span,
    details: { name: binding.name, expected_ty: resolvedTypeToString(binding.expected_ty) },
    children: [convertThirExpr(binding.value)],
  };
}

function convertThirHandler(handler: ThirHandler): TreeNode {
  return {
    id: nextId(),
    label: handler.name,
    kind: "Handler",
    span: handler.span,
    details: { name: handler.name },
    children: handler.body.map(convertThirStmt),
  };
}

function convertThirNode(node: ThirNode): TreeNode {
  const kind = node.kind;
  const base = { id: nextId(), span: node.span, children: [] as TreeNode[] };

  if ("Element" in kind) {
    const el = kind.Element;
    return {
      ...base,
      label: `<${el.name}>`,
      kind: el.is_component ? "Component" : "Element",
      details: { name: el.name, is_component: el.is_component },
      children: [
        ...el.bindings.map(convertThirBinding),
        ...el.handlers.map(convertThirHandler),
        ...el.children.map(convertThirNode),
      ],
    };
  }
  if ("Text" in kind) {
    return {
      ...base,
      label: `"${thirExprToString(kind.Text.content)}" : ${resolvedTypeToString(kind.Text.content.ty)}`,
      kind: "Text",
      children: [convertThirExpr(kind.Text.content)],
    };
  }
  if ("If" in kind) {
    const ifKind = kind.If;
    const children: TreeNode[] = [];
    children.push({
      id: nextId(),
      label: `if ${thirExprToString(ifKind.condition)}`,
      kind: "IfBranch",
      span: ifKind.condition.span,
      children: ifKind.then_branch.map(convertThirNode),
    });
    for (const [cond, body] of ifKind.else_ifs) {
      children.push({
        id: nextId(),
        label: `else if ${thirExprToString(cond)}`,
        kind: "ElseIfBranch",
        span: cond.span,
        children: body.map(convertThirNode),
      });
    }
    if (ifKind.else_branch) {
      children.push({
        id: nextId(),
        label: "else",
        kind: "ElseBranch",
        span: { source: 0, start: 0, end: 0 },
        children: ifKind.else_branch.map(convertThirNode),
      });
    }
    return { ...base, label: "if", kind: "If", children };
  }
  if ("For" in kind) {
    const forKind = kind.For;
    return {
      ...base,
      label: `for ${getSymbolName(forKind.item)}: ${resolvedTypeToString(forKind.item_ty)} in ${thirExprToString(forKind.iterable)}`,
      kind: "For",
      details: { item: forKind.item, item_ty: resolvedTypeToString(forKind.item_ty) },
      children: forKind.body.map(convertThirNode),
    };
  }
  return { ...base, label: "Unknown", kind: "Unknown" };
}

function convertThirProperty(prop: ThirProperty): TreeNode {
  const name = getSymbolName(prop.symbol);
  const typeStr = resolvedTypeToString(prop.ty);
  const defaultStr = prop.default ? ` = ${thirExprToString(prop.default)}` : "";
  return {
    id: nextId(),
    label: `${name}: ${typeStr}${defaultStr}`,
    kind: "Property",
    span: prop.span,
    details: { symbol: prop.symbol, type: typeStr },
    children: prop.default ? [convertThirExpr(prop.default)] : [],
  };
}

function convertThirCallback(cb: ThirCallback): TreeNode {
  const name = getSymbolName(cb.symbol);
  const params = cb.params.map(([id, ty]) => `${getSymbolName(id)}: ${resolvedTypeToString(ty)}`).join(", ");
  const ret = cb.return_ty ? ` -> ${resolvedTypeToString(cb.return_ty)}` : "";
  return {
    id: nextId(),
    label: `${cb.is_export ? "export " : ""}${name}: func(${params})${ret}`,
    kind: "Callback",
    span: cb.span,
    details: { symbol: cb.symbol, is_export: cb.is_export },
    children: [],
  };
}

function convertThirComponent(comp: ThirComponent): TreeNode {
  return {
    id: nextId(),
    label: `${comp.is_export ? "export " : ""}component ${comp.name}`,
    kind: "Component",
    span: comp.span,
    details: { name: comp.name, is_export: comp.is_export },
    children: [
      ...comp.properties.map(convertThirProperty),
      ...comp.callbacks.map(convertThirCallback),
      ...comp.body.map(convertThirNode),
    ],
  };
}

function convertSymbolTable(symbols: HirSymbolTable): TreeNode {
  return {
    id: nextId(),
    label: `Symbols [${symbols.symbols.length}]`,
    kind: "SymbolTable",
    span: { source: 0, start: 0, end: 0 },
    children: symbols.symbols.map((sym) => {
      let kindLabel = "unknown";
      if ("Property" in sym.kind) kindLabel = `property: ${resolvedTypeToString(sym.kind.Property.ty)}`;
      else if ("Local" in sym.kind) kindLabel = `local: ${resolvedTypeToString(sym.kind.Local.ty)}`;
      else if ("Callback" in sym.kind) kindLabel = "callback";
      else if ("LoopVar" in sym.kind) kindLabel = `loop var: ${resolvedTypeToString(sym.kind.LoopVar.ty)}`;
      else if ("Component" in sym.kind) kindLabel = `component: ${sym.kind.Component.name}`;
      else if ("BuiltinFunction" in sym.kind) kindLabel = `builtin: ${sym.kind.BuiltinFunction.name}`;

      return {
        id: nextId(),
        label: `#${sym.id} ${sym.name} (${kindLabel})`,
        kind: "Symbol",
        span: sym.span ?? { source: 0, start: 0, end: 0 },
        details: { id: sym.id, name: sym.name, kind: sym.kind },
        children: [],
      };
    }),
  };
}

export function convertThirToTree(output: ThirOutput): TreeNode {
  resetThirIdCounter();
  symbolTable = output.symbols;

  return {
    id: "thir-root",
    label: "THIR",
    kind: "THIR",
    span: { source: 0, start: 0, end: 0 },
    children: [
      convertThirComponent(output.component),
      convertSymbolTable(output.symbols),
    ],
  };
}
