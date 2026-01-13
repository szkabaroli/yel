import type { TreeNode } from "./types";

// HIR (High-level IR) - after name resolution and desugaring
// Key differences from AST:
// - Names are resolved to symbols (unique IDs)
// - Compound assignments desugared (count += 1 -> count = count + 1)
// - String interpolation desugared to Format calls

export interface HirSymbol {
  id: number;
  name: string;
  kind: "property" | "function" | "local" | "component" | "type";
}

export const sampleHirSymbols: HirSymbol[] = [
  { id: 0, name: "count", kind: "property" },
  { id: 1, name: "label", kind: "property" },
  { id: 2, name: "items", kind: "property" },
  { id: 3, name: "incremented", kind: "function" },
  { id: 4, name: "item", kind: "local" },
  { id: 5, name: "Person", kind: "type" },
  { id: 6, name: "Status", kind: "type" },
  { id: 7, name: "Counter", kind: "component" },
];

let idCounter = 0;
function nextId(): string {
  return `hir-${idCounter++}`;
}

export function createHirTree(): TreeNode {
  idCounter = 0;

  return {
    id: nextId(),
    label: "HIR Module",
    kind: "Module",
    span: { start: 0, end: 787 },
    children: [
      // Symbol table
      {
        id: nextId(),
        label: "Symbol Table (8 symbols)",
        kind: "SymbolTable",
        span: { start: 0, end: 0 },
        children: sampleHirSymbols.map((sym) => ({
          id: nextId(),
          label: `${sym.name} [#${sym.id}]`,
          kind: "Symbol",
          span: { start: 0, end: 0 },
          details: { id: sym.id, name: sym.name, kind: sym.kind },
          children: [],
        })),
      },
      // Types
      {
        id: nextId(),
        label: "Types",
        kind: "Types",
        span: { start: 30, end: 136 },
        children: [
          {
            id: nextId(),
            label: "record Person [#5]",
            kind: "RecordDef",
            span: { start: 30, end: 79 },
            details: { symbol: 5 },
            children: [
              { id: nextId(), label: "name: string", kind: "Field", span: { start: 50, end: 62 }, children: [] },
              { id: nextId(), label: "age: u32", kind: "Field", span: { start: 68, end: 76 }, children: [] },
            ],
          },
          {
            id: nextId(),
            label: "enum Status [#6]",
            kind: "EnumDef",
            span: { start: 81, end: 136 },
            details: { symbol: 6 },
            children: [
              { id: nextId(), label: "pending", kind: "Variant", span: { start: 99, end: 106 }, children: [] },
              { id: nextId(), label: "active", kind: "Variant", span: { start: 112, end: 118 }, children: [] },
              { id: nextId(), label: "completed", kind: "Variant", span: { start: 124, end: 133 }, children: [] },
            ],
          },
        ],
      },
      // Component
      {
        id: nextId(),
        label: "component Counter [#7]",
        kind: "Component",
        span: { start: 138, end: 787 },
        details: { symbol: 7 },
        children: [
          // Properties with resolved symbols
          {
            id: nextId(),
            label: "Properties",
            kind: "Properties",
            span: { start: 162, end: 236 },
            children: [
              {
                id: nextId(),
                label: "count [#0]: s32 = 0",
                kind: "Property",
                span: { start: 162, end: 177 },
                details: { symbol: 0, type: "s32" },
                children: [],
              },
              {
                id: nextId(),
                label: "label [#1]: string = \"Count\"",
                kind: "Property",
                span: { start: 182, end: 206 },
                details: { symbol: 1, type: "string" },
                children: [],
              },
              {
                id: nextId(),
                label: "items [#2]: list<Person>",
                kind: "Property",
                span: { start: 211, end: 236 },
                details: { symbol: 2, type: "list<Person>" },
                children: [],
              },
            ],
          },
          // Functions
          {
            id: nextId(),
            label: "Functions",
            kind: "Functions",
            span: { start: 242, end: 276 },
            children: [
              {
                id: nextId(),
                label: "incremented [#3]: func() -> s32",
                kind: "Function",
                span: { start: 242, end: 276 },
                details: { symbol: 3, exported: true },
                children: [],
              },
            ],
          },
          // Body with desugared expressions
          {
            id: nextId(),
            label: "Body",
            kind: "Body",
            span: { start: 282, end: 786 },
            children: [
              {
                id: nextId(),
                label: "<VStack>",
                kind: "Element",
                span: { start: 282, end: 786 },
                children: [
                  {
                    id: nextId(),
                    label: "<Text>",
                    kind: "Element",
                    span: { start: 299, end: 326 },
                    children: [
                      {
                        id: nextId(),
                        label: "Format(\"{}: {}\", label, count)",
                        kind: "FormatCall",
                        span: { start: 306, end: 324 },
                        details: { format: "{}: {}", args: ["#1 (label)", "#0 (count)"] },
                        children: [
                          { id: nextId(), label: "SymbolRef #1 (label)", kind: "SymbolRef", span: { start: 308, end: 313 }, details: { symbol: 1 }, children: [] },
                          { id: nextId(), label: "SymbolRef #0 (count)", kind: "SymbolRef", span: { start: 316, end: 321 }, details: { symbol: 0 }, children: [] },
                        ],
                      },
                    ],
                  },
                  {
                    id: nextId(),
                    label: "<HStack>",
                    kind: "Element",
                    span: { start: 336, end: 560 },
                    children: [
                      {
                        id: nextId(),
                        label: "<Button>",
                        kind: "Element",
                        span: { start: 357, end: 438 },
                        children: [
                          { id: nextId(), label: '"-"', kind: "Text", span: { start: 381, end: 384 }, children: [] },
                          {
                            id: nextId(),
                            label: "clicked handler",
                            kind: "Handler",
                            span: { start: 399, end: 424 },
                            children: [
                              {
                                id: nextId(),
                                label: "count = count - 1 (desugared)",
                                kind: "Assign",
                                span: { start: 410, end: 421 },
                                details: { desugaredFrom: "count -= 1" },
                                children: [
                                  { id: nextId(), label: "SymbolRef #0 (count)", kind: "SymbolRef", span: { start: 410, end: 415 }, children: [] },
                                  {
                                    id: nextId(),
                                    label: "Binary (-)",
                                    kind: "Binary",
                                    span: { start: 410, end: 420 },
                                    children: [
                                      { id: nextId(), label: "SymbolRef #0 (count)", kind: "SymbolRef", span: { start: 410, end: 415 }, children: [] },
                                      { id: nextId(), label: "1", kind: "Literal", span: { start: 419, end: 420 }, children: [] },
                                    ],
                                  },
                                ],
                              },
                            ],
                          },
                        ],
                      },
                      {
                        id: nextId(),
                        label: "<Button>",
                        kind: "Element",
                        span: { start: 453, end: 550 },
                        children: [
                          { id: nextId(), label: '"+"', kind: "Text", span: { start: 477, end: 480 }, children: [] },
                          {
                            id: nextId(),
                            label: "clicked handler",
                            kind: "Handler",
                            span: { start: 495, end: 536 },
                            children: [
                              {
                                id: nextId(),
                                label: "count = count + 1 (desugared)",
                                kind: "Assign",
                                span: { start: 506, end: 517 },
                                details: { desugaredFrom: "count += 1" },
                                children: [],
                              },
                              {
                                id: nextId(),
                                label: "Call incremented [#3]",
                                kind: "Call",
                                span: { start: 519, end: 532 },
                                details: { symbol: 3 },
                                children: [],
                              },
                            ],
                          },
                        ],
                      },
                    ],
                  },
                  {
                    id: nextId(),
                    label: "if",
                    kind: "If",
                    span: { start: 571, end: 695 },
                    children: [
                      {
                        id: nextId(),
                        label: "condition: count > 10",
                        kind: "Condition",
                        span: { start: 574, end: 584 },
                        children: [
                          { id: nextId(), label: "SymbolRef #0 (count)", kind: "SymbolRef", span: { start: 574, end: 579 }, children: [] },
                          { id: nextId(), label: "10", kind: "Literal", span: { start: 582, end: 584 }, children: [] },
                        ],
                      },
                      {
                        id: nextId(),
                        label: "then",
                        kind: "ThenBranch",
                        span: { start: 599, end: 622 },
                        children: [
                          { id: nextId(), label: '<Text> "High count!"', kind: "Element", span: { start: 599, end: 622 }, children: [] },
                        ],
                      },
                      {
                        id: nextId(),
                        label: "else if: count < 0",
                        kind: "ElseIfBranch",
                        span: { start: 632, end: 685 },
                        children: [
                          { id: nextId(), label: '<Text> "Negative!"', kind: "Element", span: { start: 664, end: 685 }, children: [] },
                        ],
                      },
                    ],
                  },
                  {
                    id: nextId(),
                    label: "for item [#4] in items [#2]",
                    kind: "For",
                    span: { start: 704, end: 780 },
                    details: { itemSymbol: 4, iterableSymbol: 2 },
                    children: [
                      {
                        id: nextId(),
                        label: "key: item.name",
                        kind: "Key",
                        span: { start: 726, end: 735 },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: "<Text>",
                        kind: "Element",
                        span: { start: 751, end: 770 },
                        children: [
                          { id: nextId(), label: "SymbolRef #4.name", kind: "MemberAccess", span: { start: 758, end: 767 }, children: [] },
                        ],
                      },
                    ],
                  },
                ],
              },
            ],
          },
        ],
      },
    ],
  };
}
