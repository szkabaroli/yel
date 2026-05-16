# Plan: Compiling a Node-Based Flow Language to WebAssembly

# Goal

Emit WebAssembly (Wasm) from a node-based (flow graph) language with:

* Control flow (execution wires, “X axis”)
* Data flow (value wires, “Y axis”)

⸻

Core Insight

Do not compile nodes directly.

Instead:

Compile the graph into structured control flow + variables (locals), then emit Wasm.

⸻

Concept Mapping

Graph Concept	Wasm Equivalent
Execution wire	if, block, loop
Value wire	stack values or locals
Node output	local variable
Merge (phi node)	assignments in predecessor blocks

⸻

Compilation Pipeline

1. Graph → CFG (Control Flow Graph)

Convert node graph into basic blocks:

Block {
  nodes: [...]
  terminator: branch | jump | return
}

Split blocks at:

* Branch nodes
* Merge points
* Loop boundaries

⸻

2. Convert to SSA (Static Single Assignment)

Assign each node output a unique variable:

v1 = input
v2 = const 1
v3 = add(v1, v2)

Insert phi nodes at merge points:

x = φ(x_from_then, x_from_else)

⸻

3. Eliminate Phi Nodes

Transform:

merge:
  x = φ(a from A, b from B)

Into:

A: x = a
B: x = b

Result:

* No phi nodes remain
* Everything becomes simple assignments

⸻

4. Structure Control Flow

Wasm requires structured constructs:

* if
* block
* loop

Transform CFG into structured regions.

⸻

If / Else

Graph:

   cond
    |
  branch
   / \
  A   B
   \ /
   merge

Becomes:

if (cond) {
  A
} else {
  B
}

⸻

Loop

Graph:

entry → loop_header → body → backedge

Becomes:

loop {
  body
  br_if 0   ;; continue loop
}

⸻

5. Emit Wasm

Use:

* locals for variables
* structured control flow
* stack for temporary ops

⸻

Example

Source (Graph Concept)

if (cond) {
  x = 1
} else {
  x = 2
}
return x

⸻

After Phi Elimination

if cond:
  x = 1
else:
  x = 2
return x

⸻

Wasm Output (WAT)

(func (param $cond i32) (result i32)
  (local $x i32)
  local.get $cond
  if
    i32.const 1
    local.set $x
  else
    i32.const 2
    local.set $x
  end
  local.get $x
)

⸻

Node → Wasm Translation

Arithmetic Node

c = add(a, b)
local.get $a
local.get $b
i32.add
local.set $c

⸻

Branch Node

branch(cond)
local.get $cond
if
  ...
else
  ...
end

⸻

Merge (Phi Node)

Handled by:

* assigning to the same local in each branch

⸻

Stack vs Locals

Recommendation: Use locals

Why:

* Graphs are non-linear
* Values are reused
* Merges require stable storage

Rule:

* Each node output → local
* Stack → temporary ops only

⸻

Handling Flow Axes

X Axis (Execution Flow)

* Drives control structures (if, loop, block)

Y Axis (Data Flow)

* Becomes local variables and stack operations

⸻

General Algorithm

Graph
  ↓
Build CFG (basic blocks)
  ↓
Convert to SSA (insert phi nodes)
  ↓
Eliminate phi nodes (into assignments)
  ↓
Structure control flow (if/loop/block)
  ↓
Emit Wasm

⸻

Constraints

Wasm limitations:

* No arbitrary jumps
* No runtime code generation
* Structured control flow only

⸻

Practical Design Constraints (Recommended)

Restrict graph to structured patterns:

* Explicit if node
* Explicit loop node
* No arbitrary gotos

This avoids complex CFG restructuring.

⸻

Architectural Recommendation

Split system into:

Dataflow Layer

* Pure computation
* Compiles to straight-line Wasm

Control Flow Layer

* Branches, loops
* Maps directly to Wasm constructs

⸻

Key Mental Model

You are not compiling nodes.

You are compiling structured control flow regions that contain nodes.

⸻

Next Steps (Optional Extensions)

* Implement SSA IR as structs
* Add optimization passes:
    * constant folding
    * dead node elimination
* Support foreach via loop lowering

⸻