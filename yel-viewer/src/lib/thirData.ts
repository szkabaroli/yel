import type { TreeNode } from "./types";

// THIR (Typed High-level IR) - after type inference and type checking
// Key differences from HIR:
// - Every expression has a resolved type
// - Type-directed desugaring applied
// - Type errors would be caught here

let idCounter = 0;
function nextId(): string {
  return `thir-${idCounter++}`;
}

function typed(label: string, type: string): string {
  return `${label} : ${type}`;
}

export function createThirTree(): TreeNode {
  idCounter = 0;

  return {
    id: nextId(),
    label: "THIR Module",
    kind: "Module",
    span: { start: 0, end: 787 },
    details: { stage: "THIR", description: "Every expression has a type" },
    children: [
      // Type definitions
      {
        id: nextId(),
        label: "Type Definitions",
        kind: "TypeDefs",
        span: { start: 30, end: 136 },
        children: [
          {
            id: nextId(),
            label: "Person : record",
            kind: "RecordType",
            span: { start: 30, end: 79 },
            details: { fields: { name: "string", age: "u32" } },
            children: [
              { id: nextId(), label: typed("name", "string"), kind: "Field", span: { start: 50, end: 62 }, children: [] },
              { id: nextId(), label: typed("age", "u32"), kind: "Field", span: { start: 68, end: 76 }, children: [] },
            ],
          },
          {
            id: nextId(),
            label: "Status : enum",
            kind: "EnumType",
            span: { start: 81, end: 136 },
            details: { variants: ["pending", "active", "completed"] },
            children: [],
          },
        ],
      },
      // Component with typed properties
      {
        id: nextId(),
        label: "Counter : component",
        kind: "Component",
        span: { start: 138, end: 787 },
        children: [
          // Typed properties
          {
            id: nextId(),
            label: "Signals",
            kind: "Signals",
            span: { start: 162, end: 236 },
            children: [
              {
                id: nextId(),
                label: typed("count", "s32"),
                kind: "Signal",
                span: { start: 162, end: 177 },
                details: { type: "s32", mutable: true },
                children: [
                  { id: nextId(), label: typed("0", "s32"), kind: "Literal", span: { start: 175, end: 176 }, details: { type: "s32" }, children: [] },
                ],
              },
              {
                id: nextId(),
                label: typed("label", "string"),
                kind: "Signal",
                span: { start: 182, end: 206 },
                details: { type: "string", mutable: true },
                children: [
                  { id: nextId(), label: typed('"Count"', "string"), kind: "Literal", span: { start: 198, end: 205 }, details: { type: "string" }, children: [] },
                ],
              },
              {
                id: nextId(),
                label: typed("items", "list<Person>"),
                kind: "Signal",
                span: { start: 211, end: 236 },
                details: { type: "list<Person>", mutable: true },
                children: [
                  { id: nextId(), label: typed("[]", "list<Person>"), kind: "Literal", span: { start: 233, end: 235 }, details: { type: "list<Person>" }, children: [] },
                ],
              },
            ],
          },
          // Typed function
          {
            id: nextId(),
            label: typed("incremented", "func() -> s32"),
            kind: "Function",
            span: { start: 242, end: 276 },
            details: { type: "func() -> s32", exported: true },
            children: [],
          },
          // Body with typed expressions
          {
            id: nextId(),
            label: "Body",
            kind: "Body",
            span: { start: 282, end: 786 },
            children: [
              {
                id: nextId(),
                label: "<VStack> : Element",
                kind: "Element",
                span: { start: 282, end: 786 },
                details: { elementType: "VStack" },
                children: [
                  {
                    id: nextId(),
                    label: "<Text> : Element",
                    kind: "Element",
                    span: { start: 299, end: 326 },
                    children: [
                      {
                        id: nextId(),
                        label: typed('Format("{}: {}", ...)', "string"),
                        kind: "Call",
                        span: { start: 306, end: 324 },
                        details: { type: "string", function: "format" },
                        children: [
                          {
                            id: nextId(),
                            label: typed("label", "string"),
                            kind: "Ref",
                            span: { start: 308, end: 313 },
                            details: { type: "string", symbol: "label" },
                            children: [],
                          },
                          {
                            id: nextId(),
                            label: typed("count", "s32"),
                            kind: "Ref",
                            span: { start: 316, end: 321 },
                            details: { type: "s32", symbol: "count", coercedTo: "string" },
                            children: [],
                          },
                        ],
                      },
                    ],
                  },
                  {
                    id: nextId(),
                    label: "<HStack> : Element",
                    kind: "Element",
                    span: { start: 336, end: 560 },
                    children: [
                      {
                        id: nextId(),
                        label: "<Button> : Element",
                        kind: "Element",
                        span: { start: 357, end: 438 },
                        children: [
                          {
                            id: nextId(),
                            label: typed('"-"', "string"),
                            kind: "Literal",
                            span: { start: 381, end: 384 },
                            details: { type: "string" },
                            children: [],
                          },
                          {
                            id: nextId(),
                            label: "clicked : handler",
                            kind: "Handler",
                            span: { start: 399, end: 424 },
                            children: [
                              {
                                id: nextId(),
                                label: typed("count = count - 1", "()"),
                                kind: "Assign",
                                span: { start: 410, end: 421 },
                                details: { type: "()", lhsType: "s32", rhsType: "s32" },
                                children: [
                                  {
                                    id: nextId(),
                                    label: typed("count - 1", "s32"),
                                    kind: "Binary",
                                    span: { start: 410, end: 420 },
                                    details: { type: "s32", op: "-" },
                                    children: [
                                      { id: nextId(), label: typed("count", "s32"), kind: "Ref", span: { start: 410, end: 415 }, children: [] },
                                      { id: nextId(), label: typed("1", "s32"), kind: "Literal", span: { start: 419, end: 420 }, children: [] },
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
                        label: "<Button> : Element",
                        kind: "Element",
                        span: { start: 453, end: 550 },
                        children: [
                          {
                            id: nextId(),
                            label: typed('"+"', "string"),
                            kind: "Literal",
                            span: { start: 477, end: 480 },
                            children: [],
                          },
                          {
                            id: nextId(),
                            label: "clicked : handler",
                            kind: "Handler",
                            span: { start: 495, end: 536 },
                            children: [
                              {
                                id: nextId(),
                                label: typed("count = count + 1", "()"),
                                kind: "Assign",
                                span: { start: 506, end: 517 },
                                children: [],
                              },
                              {
                                id: nextId(),
                                label: typed("incremented()", "s32"),
                                kind: "Call",
                                span: { start: 519, end: 532 },
                                details: { type: "s32", function: "incremented" },
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
                    label: "if : Element",
                    kind: "If",
                    span: { start: 571, end: 695 },
                    children: [
                      {
                        id: nextId(),
                        label: typed("count > 10", "bool"),
                        kind: "Condition",
                        span: { start: 574, end: 584 },
                        details: { type: "bool" },
                        children: [
                          { id: nextId(), label: typed("count", "s32"), kind: "Ref", span: { start: 574, end: 579 }, children: [] },
                          { id: nextId(), label: typed("10", "s32"), kind: "Literal", span: { start: 582, end: 584 }, children: [] },
                        ],
                      },
                      {
                        id: nextId(),
                        label: "then : list<Element>",
                        kind: "ThenBranch",
                        span: { start: 599, end: 622 },
                        children: [
                          { id: nextId(), label: "<Text> : Element", kind: "Element", span: { start: 599, end: 622 }, children: [] },
                        ],
                      },
                      {
                        id: nextId(),
                        label: typed("count < 0", "bool"),
                        kind: "ElseIfCondition",
                        span: { start: 640, end: 649 },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: "else if : list<Element>",
                        kind: "ElseIfBranch",
                        span: { start: 664, end: 685 },
                        children: [
                          { id: nextId(), label: "<Text> : Element", kind: "Element", span: { start: 664, end: 685 }, children: [] },
                        ],
                      },
                    ],
                  },
                  {
                    id: nextId(),
                    label: typed("for item in items", "list<Element>"),
                    kind: "For",
                    span: { start: 704, end: 780 },
                    details: { itemType: "Person", iterableType: "list<Person>" },
                    children: [
                      {
                        id: nextId(),
                        label: typed("item", "Person"),
                        kind: "LoopVar",
                        span: { start: 708, end: 712 },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: typed("items", "list<Person>"),
                        kind: "Iterable",
                        span: { start: 716, end: 721 },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: typed("item.name", "string"),
                        kind: "Key",
                        span: { start: 726, end: 735 },
                        details: { type: "string" },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: "<Text> : Element",
                        kind: "Element",
                        span: { start: 751, end: 770 },
                        children: [
                          {
                            id: nextId(),
                            label: typed("item.name", "string"),
                            kind: "MemberAccess",
                            span: { start: 758, end: 767 },
                            details: { type: "string", object: "item", field: "name" },
                            children: [],
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
      },
    ],
  };
}
