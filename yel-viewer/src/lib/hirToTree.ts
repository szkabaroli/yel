import type {
  HirOutput,
  HirComponent,
  HirNode,
  HirNodeKind,
  HirBinding,
  HirHandler,
  HirExpr,
  HirExprKind,
  HirStatement,
  HirLiteral,
  HirInterpolationPart,
  TreeNode,
  Span,
  BinOp,
  LocalId,
  DefId,
  Name,
  Ty,
} from "./types";

let idCounter = 0;

function nextId(): string {
  return `hir-${idCounter++}`;
}

export function resetHirIdCounter(): void {
  idCounter = 0;
}

function hirLiteralToString(lit: HirLiteral): string {
  if ("Int" in lit) return String(lit.Int);
  if ("Float" in lit) return String(lit.Float);
  if ("String" in lit) return `"${lit.String}"`;
  if ("Char" in lit) return `'${lit.Char}'`;
  if ("Bool" in lit) return String(lit.Bool);
  if ("Unit" in lit) return `${lit.Unit[0]}${lit.Unit[1]}`;
  if ("Color" in lit) return lit.Color;
  if ("List" in lit) return `[${lit.List.length} items]`;
  if ("Tuple" in lit) return `(${lit.Tuple.length} items)`;
  if ("Record" in lit) return `{ ${lit.Record.fields.length} fields }`;
  return "?";
}

function hirExprToString(expr: HirExpr): string {
  const kind = expr.kind;
  if (kind === "Error") return "<error>";
  if ("Local" in kind) return `local#${kind.Local}`;
  if ("Def" in kind) return `def#${kind.Def}`;
  if ("Literal" in kind) return hirLiteralToString(kind.Literal);
  if ("Binary" in kind) return `${hirExprToString(kind.Binary.lhs)} ${kind.Binary.op} ${hirExprToString(kind.Binary.rhs)}`;
  if ("Unary" in kind) return `${kind.Unary.op}${hirExprToString(kind.Unary.operand)}`;
  if ("Call" in kind) return `${kind.Call.func}(${kind.Call.args.map(hirExprToString).join(", ")})`;
  if ("PathCall" in kind) return `${kind.PathCall.base}.${kind.PathCall.member}(...)`;
  if ("Field" in kind) return `${hirExprToString(kind.Field.base)}.${kind.Field.field}`;
  if ("OptionalField" in kind) return `${hirExprToString(kind.OptionalField.base)}?.${kind.OptionalField.field}`;
  if ("Index" in kind) return `${hirExprToString(kind.Index.base)}[${hirExprToString(kind.Index.index)}]`;
  if ("Ternary" in kind) return `${hirExprToString(kind.Ternary.condition)} ? ... : ...`;
  if ("Range" in kind) return `${hirExprToString(kind.Range.start)}..${hirExprToString(kind.Range.end)}`;
  if ("Closure" in kind) return `|...| { ... }`;
  if ("Interpolation" in kind) return `"..."`;
  if ("Path" in kind) return kind.Path.segments.join(".");
  return "?";
}

function convertHirLiteral(lit: HirLiteral, span: Span): TreeNode {
  const base = { id: nextId(), span, children: [] as TreeNode[] };

  if ("Int" in lit) return { ...base, label: String(lit.Int), kind: "Int" };
  if ("Float" in lit) return { ...base, label: String(lit.Float), kind: "Float" };
  if ("String" in lit) return { ...base, label: `"${lit.String}"`, kind: "String" };
  if ("Char" in lit) return { ...base, label: `'${lit.Char}'`, kind: "Char" };
  if ("Bool" in lit) return { ...base, label: String(lit.Bool), kind: "Bool" };
  if ("Unit" in lit) return { ...base, label: `${lit.Unit[0]}${lit.Unit[1]}`, kind: "Unit" };
  if ("Color" in lit) return { ...base, label: lit.Color, kind: "Color" };
  if ("List" in lit) return { ...base, label: `[${lit.List.length}]`, kind: "List", children: lit.List.map(convertHirExpr) };
  if ("Tuple" in lit) return { ...base, label: `(${lit.Tuple.length})`, kind: "Tuple", children: lit.Tuple.map(convertHirExpr) };
  if ("Record" in lit) {
    return {
      ...base,
      label: `{ ${lit.Record.fields.length} fields }`,
      kind: "Record",
      children: lit.Record.fields.map(([name, val]) => ({
        id: nextId(),
        label: name,
        kind: "RecordField",
        span: val.span,
        children: [convertHirExpr(val)],
      })),
    };
  }
  return { ...base, label: "?", kind: "Unknown" };
}

function convertHirExpr(expr: HirExpr): TreeNode {
  const kind = expr.kind;
  const base = { id: nextId(), span: expr.span, children: [] as TreeNode[] };

  if (kind === "Error") {
    return { ...base, label: "<error>", kind: "Error" };
  }
  if ("Local" in kind) {
    return { ...base, label: `local#${kind.Local}`, kind: "Local", details: { local_id: kind.Local } };
  }
  if ("Def" in kind) {
    return { ...base, label: `def#${kind.Def}`, kind: "Def", details: { def_id: kind.Def } };
  }
  if ("Literal" in kind) {
    return convertHirLiteral(kind.Literal, expr.span);
  }
  if ("Binary" in kind) {
    return {
      ...base,
      label: `Binary (${kind.Binary.op})`,
      kind: "Binary",
      details: { op: kind.Binary.op },
      children: [convertHirExpr(kind.Binary.lhs), convertHirExpr(kind.Binary.rhs)],
    };
  }
  if ("Unary" in kind) {
    return {
      ...base,
      label: `Unary (${kind.Unary.op})`,
      kind: "Unary",
      details: { op: kind.Unary.op },
      children: [convertHirExpr(kind.Unary.operand)],
    };
  }
  if ("Call" in kind) {
    return {
      ...base,
      label: `Call ${kind.Call.func}()`,
      kind: "Call",
      details: { func: kind.Call.func },
      children: kind.Call.args.map(convertHirExpr),
    };
  }
  if ("PathCall" in kind) {
    return {
      ...base,
      label: `${kind.PathCall.base}.${kind.PathCall.member}()`,
      kind: "PathCall",
      details: { base: kind.PathCall.base, member: kind.PathCall.member },
      children: kind.PathCall.args.map(convertHirExpr),
    };
  }
  if ("Field" in kind) {
    return {
      ...base,
      label: `.${kind.Field.field}`,
      kind: "Field",
      details: { field: kind.Field.field },
      children: [convertHirExpr(kind.Field.base)],
    };
  }
  if ("OptionalField" in kind) {
    return {
      ...base,
      label: `?.${kind.OptionalField.field}`,
      kind: "OptionalField",
      details: { field: kind.OptionalField.field },
      children: [convertHirExpr(kind.OptionalField.base)],
    };
  }
  if ("Index" in kind) {
    return {
      ...base,
      label: "Index",
      kind: "Index",
      children: [convertHirExpr(kind.Index.base), convertHirExpr(kind.Index.index)],
    };
  }
  if ("Ternary" in kind) {
    return {
      ...base,
      label: "Ternary",
      kind: "Ternary",
      children: [
        convertHirExpr(kind.Ternary.condition),
        convertHirExpr(kind.Ternary.then_expr),
        convertHirExpr(kind.Ternary.else_expr),
      ],
    };
  }
  if ("Range" in kind) {
    return {
      ...base,
      label: kind.Range.inclusive ? "Range (..=)" : "Range (..)",
      kind: "Range",
      children: [convertHirExpr(kind.Range.start), convertHirExpr(kind.Range.end)],
    };
  }
  if ("Closure" in kind) {
    const params = kind.Closure.params.map(([name]) => name).join(", ");
    return {
      ...base,
      label: `Closure |${params}|`,
      kind: "Closure",
      children: kind.Closure.body.map(convertHirStatement),
    };
  }
  if ("Interpolation" in kind) {
    return {
      ...base,
      label: "Interpolation",
      kind: "Interpolation",
      children: kind.Interpolation.map((part, i) => {
        if ("Literal" in part) {
          return { id: nextId(), label: `"${part.Literal}"`, kind: "LiteralPart", span: expr.span, children: [] };
        } else {
          return convertHirExpr(part.Expr);
        }
      }),
    };
  }
  if ("Path" in kind) {
    return {
      ...base,
      label: kind.Path.segments.join("."),
      kind: "Path",
      details: { segments: kind.Path.segments },
    };
  }
  return { ...base, label: "Unknown", kind: "Unknown" };
}

function convertHirStatement(stmt: HirStatement): TreeNode {
  const base = { id: nextId(), span: { source: 0, start: 0, end: 0 } as Span, children: [] as TreeNode[] };

  if ("Expr" in stmt) {
    return {
      ...base,
      label: hirExprToString(stmt.Expr),
      kind: "ExprStmt",
      span: stmt.Expr.span,
      children: [convertHirExpr(stmt.Expr)],
    };
  }
  if ("Assign" in stmt) {
    return {
      ...base,
      label: `${hirExprToString(stmt.Assign.target)} = ${hirExprToString(stmt.Assign.value)}`,
      kind: "Assign",
      children: [convertHirExpr(stmt.Assign.target), convertHirExpr(stmt.Assign.value)],
    };
  }
  if ("CompoundAssign" in stmt) {
    return {
      ...base,
      label: `${hirExprToString(stmt.CompoundAssign.target)} ${stmt.CompoundAssign.op}= ${hirExprToString(stmt.CompoundAssign.value)}`,
      kind: "CompoundAssign",
      details: { op: stmt.CompoundAssign.op },
      children: [convertHirExpr(stmt.CompoundAssign.target), convertHirExpr(stmt.CompoundAssign.value)],
    };
  }
  if ("If" in stmt) {
    return {
      ...base,
      label: `if ${hirExprToString(stmt.If.condition)}`,
      kind: "IfStmt",
      children: [
        convertHirExpr(stmt.If.condition),
        ...stmt.If.then_branch.map(convertHirStatement),
        ...(stmt.If.else_branch?.map(convertHirStatement) ?? []),
      ],
    };
  }
  return { ...base, label: "Unknown", kind: "Unknown" };
}

function convertHirBinding(binding: HirBinding): TreeNode {
  return {
    id: nextId(),
    label: `${binding.name} = ${hirExprToString(binding.value)}`,
    kind: "Binding",
    span: binding.name_span,
    details: { name: binding.name },
    children: [convertHirExpr(binding.value)],
  };
}

function convertHirHandler(handler: HirHandler): TreeNode {
  return {
    id: nextId(),
    label: handler.name,
    kind: "Handler",
    span: handler.name_span,
    details: { name: handler.name },
    children: handler.body.map(convertHirStatement),
  };
}

function convertHirNode(node: HirNode): TreeNode {
  const kind = node.kind;
  const base = { id: nextId(), span: node.span, children: [] as TreeNode[] };

  if ("Element" in kind) {
    const el = kind.Element;
    return {
      ...base,
      label: `<${el.name}>`,
      kind: "Element",
      details: { name: el.name, node_id: node.id },
      children: [
        ...el.bindings.map(convertHirBinding),
        ...el.handlers.map(convertHirHandler),
        ...el.children.map(convertHirNode),
      ],
    };
  }
  if ("Text" in kind) {
    return {
      ...base,
      label: `Text: ${hirExprToString(kind.Text)}`,
      kind: "Text",
      children: [convertHirExpr(kind.Text)],
    };
  }
  if ("If" in kind) {
    const ifKind = kind.If;
    const children: TreeNode[] = [];
    children.push({
      id: nextId(),
      label: `if ${hirExprToString(ifKind.condition)}`,
      kind: "IfBranch",
      span: ifKind.condition.span,
      children: ifKind.then_branch.map(convertHirNode),
    });
    if (ifKind.else_if_branches) {
      for (const [cond, body] of ifKind.else_if_branches) {
        children.push({
          id: nextId(),
          label: `else if ${hirExprToString(cond)}`,
          kind: "ElseIfBranch",
          span: cond.span,
          children: body.map(convertHirNode),
        });
      }
    }
    if (ifKind.else_branch) {
      children.push({
        id: nextId(),
        label: "else",
        kind: "ElseBranch",
        span: { source: 0, start: 0, end: 0 },
        children: ifKind.else_branch.map(convertHirNode),
      });
    }
    return { ...base, label: "if", kind: "If", children };
  }
  if ("For" in kind) {
    const forKind = kind.For;
    return {
      ...base,
      label: `for name#${forKind.item_name} in ${hirExprToString(forKind.iterable)}`,
      kind: "For",
      details: { item: forKind.item, item_name: forKind.item_name },
      children: forKind.body.map(convertHirNode),
    };
  }
  return { ...base, label: "Unknown", kind: "Unknown" };
}

function convertHirComponent(comp: HirComponent): TreeNode {
  return {
    id: nextId(),
    label: `${comp.is_export ? "export " : ""}component name#${comp.name}`,
    kind: "Component",
    span: comp.span,
    details: { def_id: comp.def_id, name: comp.name, is_export: comp.is_export },
    children: comp.body.map(convertHirNode),
  };
}

export function convertHirToTree(output: HirOutput): TreeNode {
  resetHirIdCounter();

  // HirOutput is an array of HirComponent
  const children = output.map(convertHirComponent);

  return {
    id: "hir-root",
    label: `HIR [${output.length} component${output.length === 1 ? "" : "s"}]`,
    kind: "HIR",
    span: { source: 0, start: 0, end: 0 },
    children,
  };
}
