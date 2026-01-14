<script lang="ts">
  import type { TreeNode } from "../types";
  import TreeNodeSelf from "./TreeNode.svelte";

  interface Props {
    node: TreeNode;
    depth?: number;
    selectedId?: string | null;
    onSelect?: (node: TreeNode) => void;
    onHover?: (node: TreeNode | null) => void;
  }

  let {
    node,
    depth = 0,
    selectedId = null,
    onSelect,
    onHover,
  }: Props = $props();

  let expanded = $state(true);

  // Initialize expansion based on depth (only on mount)
  $effect(() => {
    expanded = depth < 2;
  });

  const hasChildren = $derived(node.children.length > 0);
  const isSelected = $derived(selectedId === node.id);

  function toggle(e: MouseEvent) {
    e.stopPropagation();
    if (node.children.length > 0) {
      expanded = !expanded;
    }
  }

  function handleClick(e: MouseEvent) {
    e.stopPropagation();
    onSelect?.(node);
  }

  function handleMouseEnter() {
    onHover?.(node);
  }

  function handleMouseLeave() {
    onHover?.(null);
  }

  // Color class mapping for syntax highlighting
  // Uses CSS classes that switch colors based on color scheme
  function getKindColorClass(kind: string): string {
    const classMap: Record<string, string> = {
      // Root/Structure (blue)
      File: "kind-blue",
      HIR: "kind-blue",
      THIR: "kind-blue",
      Package: "kind-orange",

      // Type definitions (orange)
      Component: "kind-orange",
      Record: "kind-orange",
      Enum: "kind-orange",
      Variant: "kind-orange",
      RecordLit: "kind-orange",
      VariantCtor: "kind-orange",

      // Properties and fields (blue)
      Property: "kind-blue",
      Field: "kind-blue",
      Binding: "kind-blue",
      EnumCase: "kind-blue",
      VariantCase: "kind-blue",

      // Functions (purple)
      Function: "kind-purple",
      Callback: "kind-purple",
      Call: "kind-purple",
      MethodCall: "kind-purple",
      Handler: "kind-purple",
      Closure: "kind-purple",

      // UI Elements (green)
      Element: "kind-green",

      // Control flow (red)
      If: "kind-red",
      IfBranch: "kind-red",
      ElseIfBranch: "kind-red",
      ElseBranch: "kind-red",
      For: "kind-red",
      Match: "kind-red",
      MatchArm: "kind-red",
      Guard: "kind-red",
      Ternary: "kind-red",
      Range: "kind-red",

      // Text/Strings (cyan)
      Text: "kind-cyan",
      Literal: "kind-cyan",
      Interpolation: "kind-cyan",

      // Variables/Expressions (default)
      Var: "kind-default",
      Ident: "kind-default",
      Binary: "kind-default",
      Unary: "kind-default",
      Index: "kind-default",
      Member: "kind-default",
      OptionalMember: "kind-default",
      OptionalChain: "kind-default",
      Tuple: "kind-default",
      List: "kind-default",

      // Statements (default)
      ExprStmt: "kind-default",
      Assign: "kind-default",
      CompoundAssign: "kind-default",
      IfStmt: "kind-red",

      // Coercion/Type ops (orange)
      Coerce: "kind-orange",

      // Symbols (purple - like functions)
      Symbol: "kind-purple",
      SymbolTable: "kind-purple",

      // Errors (red)
      Error: "kind-red",
    };
    return classMap[kind] || "kind-default";
  }
</script>

<div
  class="tree-node font-mono text-[13px] select-none"
  class:selected={isSelected}
  style:--depth={depth}
  role="treeitem"
  tabindex="0"
  aria-selected={isSelected}
  aria-expanded={hasChildren ? expanded : undefined}
  onclick={handleClick}
  onmouseenter={handleMouseEnter}
  onmouseleave={handleMouseLeave}
  onkeydown={(e) => {
    if (e.key === "Enter" || e.key === " ") {
      handleClick(e as unknown as MouseEvent);
    }
  }}
>
  <div class="node-content flex items-center gap-1.5 py-1 px-2 rounded cursor-pointer transition-colors hover:bg-secondary/50"
       class:selected-content={isSelected}
       style:padding-left="calc(8px + var(--depth, 0) * 16px)">
    {#if hasChildren}
      <button
        class="w-4 h-4 flex items-center justify-center bg-transparent border-none text-muted-foreground cursor-pointer p-0 text-[10px] shrink-0 hover:text-foreground"
        onclick={toggle}
        aria-label={expanded ? "Collapse" : "Expand"}
      >
        {expanded ? "▼" : "▶"}
      </button>
    {:else}
      <span class="w-4 shrink-0"></span>
    {/if}

    <span class="font-medium shrink-0 {getKindColorClass(node.kind)}">
      {node.kind}
    </span>

    <span class="text-foreground whitespace-nowrap overflow-hidden text-ellipsis">{node.label}</span>

    {#if node.span && (node.span.start > 0 || node.span.end > 0)}
      <span class="text-muted-foreground text-[11px] shrink-0 ml-auto">[{node.span.start}:{node.span.end}]</span>
    {/if}
  </div>

  {#if expanded && hasChildren}
    <div role="group">
      {#each node.children as child (child.id)}
        <TreeNodeSelf
          node={child}
          depth={depth + 1}
          {selectedId}
          {onSelect}
          {onHover}
        />
      {/each}
    </div>
  {/if}
</div>

<style>
  .tree-node {
    --indent: calc(var(--depth, 0) * 16px);
  }

  .selected-content {
    background-color: rgba(250, 204, 21, 0.15);
    outline: 1px solid rgba(250, 204, 21, 0.4);
  }

  /* Light mode colors */
  .kind-red { color: #cf222e; }
  .kind-orange { color: #bc4c00; }
  .kind-green { color: #116329; }
  .kind-blue { color: #0969da; }
  .kind-purple { color: #8250df; }
  .kind-cyan { color: #0284c7; }
  .kind-default { color: var(--color-foreground); }

  /* Dark mode colors */
  @media (prefers-color-scheme: dark) {
    .kind-red { color: #ff7b72; }
    .kind-orange { color: #ffa657; }
    .kind-green { color: #7ee787; }
    .kind-blue { color: #79c0ff; }
    .kind-purple { color: #d2a8ff; }
    .kind-cyan { color: #a5d6ff; }
  }
</style>
