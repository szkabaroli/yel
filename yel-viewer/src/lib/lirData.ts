import type { TreeNode } from "./types";

// LIR (Low-level IR) - ready for code generation
// Key features:
// - Nodes have unique IDs for DOM tracking
// - Effects extracted with dependency information
// - Handlers collected with IDs
// - Expressions are structured (not strings)

let idCounter = 0;
function nextId(): string {
  return `lir-${idCounter++}`;
}

export function createLirTree(): TreeNode {
  idCounter = 0;

  return {
    id: nextId(),
    label: "LIR Package",
    kind: "Package",
    span: { start: 0, end: 787 },
    children: [
      // Signals
      {
        id: nextId(),
        label: "Signals (3)",
        kind: "Signals",
        span: { start: 0, end: 0 },
        children: [
          {
            id: nextId(),
            label: "count: s32 = 0",
            kind: "Signal",
            span: { start: 162, end: 177 },
            details: { symbol: 0, rustType: "i32", witType: "s32" },
            children: [],
          },
          {
            id: nextId(),
            label: "label: string = \"Count\"",
            kind: "Signal",
            span: { start: 182, end: 206 },
            details: { symbol: 1, rustType: "String", witType: "string" },
            children: [],
          },
          {
            id: nextId(),
            label: "items: list<Person>",
            kind: "Signal",
            span: { start: 211, end: 236 },
            details: { symbol: 2, rustType: "Vec<Person>", witType: "list<person>" },
            children: [],
          },
        ],
      },
      // Effects
      {
        id: nextId(),
        label: "Effects (4)",
        kind: "Effects",
        span: { start: 0, end: 0 },
        children: [
          {
            id: nextId(),
            label: "Effect #0: TextContent",
            kind: "Effect",
            span: { start: 306, end: 324 },
            details: {
              id: 0,
              targetNode: 1,
              updateKind: "TextContent",
              dependencies: ["count", "label"],
            },
            children: [
              {
                id: nextId(),
                label: "deps: [count, label]",
                kind: "Dependencies",
                span: { start: 0, end: 0 },
                children: [],
              },
              {
                id: nextId(),
                label: "expr: format!(\"{}: {}\", label, count)",
                kind: "Expression",
                span: { start: 306, end: 324 },
                children: [],
              },
            ],
          },
          {
            id: nextId(),
            label: "Effect #1: Visibility",
            kind: "Effect",
            span: { start: 571, end: 622 },
            details: {
              id: 1,
              targetNode: 5,
              updateKind: "Visibility",
              dependencies: ["count"],
            },
            children: [
              {
                id: nextId(),
                label: "deps: [count]",
                kind: "Dependencies",
                span: { start: 0, end: 0 },
                children: [],
              },
              {
                id: nextId(),
                label: "condition: count > 10",
                kind: "Expression",
                span: { start: 574, end: 584 },
                children: [],
              },
            ],
          },
          {
            id: nextId(),
            label: "Effect #2: Visibility",
            kind: "Effect",
            span: { start: 632, end: 685 },
            details: {
              id: 2,
              targetNode: 6,
              updateKind: "Visibility",
              dependencies: ["count"],
            },
            children: [
              {
                id: nextId(),
                label: "deps: [count]",
                kind: "Dependencies",
                span: { start: 0, end: 0 },
                children: [],
              },
              {
                id: nextId(),
                label: "condition: count < 0",
                kind: "Expression",
                span: { start: 640, end: 649 },
                children: [],
              },
            ],
          },
          {
            id: nextId(),
            label: "Effect #3: List",
            kind: "Effect",
            span: { start: 704, end: 780 },
            details: {
              id: 3,
              targetNode: 7,
              updateKind: "List",
              dependencies: ["items"],
            },
            children: [
              {
                id: nextId(),
                label: "deps: [items]",
                kind: "Dependencies",
                span: { start: 0, end: 0 },
                children: [],
              },
              {
                id: nextId(),
                label: "iterable: items",
                kind: "Expression",
                span: { start: 717, end: 722 },
                children: [],
              },
            ],
          },
        ],
      },
      // Handlers
      {
        id: nextId(),
        label: "Handlers (2)",
        kind: "Handlers",
        span: { start: 0, end: 0 },
        children: [
          {
            id: nextId(),
            label: "Handler #0: clicked (Button -)",
            kind: "Handler",
            span: { start: 399, end: 424 },
            details: { id: 0, event: "clicked", targetNode: 3 },
            children: [
              {
                id: nextId(),
                label: "Assign: count = count - 1",
                kind: "Stmt",
                span: { start: 410, end: 421 },
                children: [],
              },
            ],
          },
          {
            id: nextId(),
            label: "Handler #1: clicked (Button +)",
            kind: "Handler",
            span: { start: 495, end: 536 },
            details: { id: 1, event: "clicked", targetNode: 4 },
            children: [
              {
                id: nextId(),
                label: "Assign: count = count + 1",
                kind: "Stmt",
                span: { start: 506, end: 517 },
                children: [],
              },
              {
                id: nextId(),
                label: "Call: incremented()",
                kind: "Stmt",
                span: { start: 519, end: 532 },
                children: [],
              },
            ],
          },
        ],
      },
      // Node Tree
      {
        id: nextId(),
        label: "Node Tree",
        kind: "NodeTree",
        span: { start: 282, end: 786 },
        children: [
          {
            id: nextId(),
            label: "Node #0: Element <VStack>",
            kind: "Element",
            span: { start: 282, end: 786 },
            details: { nodeId: 0, tag: "VStack" },
            children: [
              {
                id: nextId(),
                label: "Node #1: Element <Text>",
                kind: "Element",
                span: { start: 299, end: 326 },
                details: { nodeId: 1, tag: "Text", hasEffect: true },
                children: [
                  {
                    id: nextId(),
                    label: "content: Format",
                    kind: "TextContent",
                    span: { start: 306, end: 324 },
                    children: [],
                  },
                ],
              },
              {
                id: nextId(),
                label: "Node #2: Element <HStack>",
                kind: "Element",
                span: { start: 336, end: 560 },
                details: { nodeId: 2, tag: "HStack" },
                children: [
                  {
                    id: nextId(),
                    label: "Node #3: Element <Button>",
                    kind: "Element",
                    span: { start: 357, end: 438 },
                    details: { nodeId: 3, tag: "Button", handler: 0 },
                    children: [
                      {
                        id: nextId(),
                        label: "content: \"-\"",
                        kind: "TextContent",
                        span: { start: 381, end: 384 },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: "event: clicked -> Handler #0",
                        kind: "EventBinding",
                        span: { start: 399, end: 424 },
                        children: [],
                      },
                    ],
                  },
                  {
                    id: nextId(),
                    label: "Node #4: Element <Button>",
                    kind: "Element",
                    span: { start: 453, end: 550 },
                    details: { nodeId: 4, tag: "Button", handler: 1 },
                    children: [
                      {
                        id: nextId(),
                        label: "content: \"+\"",
                        kind: "TextContent",
                        span: { start: 477, end: 480 },
                        children: [],
                      },
                      {
                        id: nextId(),
                        label: "event: clicked -> Handler #1",
                        kind: "EventBinding",
                        span: { start: 495, end: 536 },
                        children: [],
                      },
                    ],
                  },
                ],
              },
              {
                id: nextId(),
                label: "Node #5: Conditional",
                kind: "Conditional",
                span: { start: 571, end: 695 },
                details: { nodeId: 5, hasEffect: true },
                children: [
                  {
                    id: nextId(),
                    label: "condition: count > 10",
                    kind: "Condition",
                    span: { start: 574, end: 584 },
                    children: [],
                  },
                  {
                    id: nextId(),
                    label: "then: <Text> \"High count!\"",
                    kind: "Branch",
                    span: { start: 599, end: 622 },
                    children: [],
                  },
                  {
                    id: nextId(),
                    label: "else_if: count < 0",
                    kind: "Condition",
                    span: { start: 640, end: 649 },
                    children: [],
                  },
                  {
                    id: nextId(),
                    label: "Node #6: <Text> \"Negative!\"",
                    kind: "Branch",
                    span: { start: 664, end: 685 },
                    details: { nodeId: 6 },
                    children: [],
                  },
                ],
              },
              {
                id: nextId(),
                label: "Node #7: Loop",
                kind: "Loop",
                span: { start: 704, end: 780 },
                details: { nodeId: 7, item: "item", iterable: "items", hasEffect: true },
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
                    label: "Node #8: <Text>",
                    kind: "Element",
                    span: { start: 751, end: 770 },
                    details: { nodeId: 8, tag: "Text" },
                    children: [
                      {
                        id: nextId(),
                        label: "content: item.name",
                        kind: "TextContent",
                        span: { start: 758, end: 767 },
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
  };
}
