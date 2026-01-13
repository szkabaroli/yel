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

  // Color palette matching yelLanguage.ts (GitHub Dark theme)
  // Keywords: #ff7b72 (red) - control flow
  // Types/Records: #ffa657 (orange) - type definitions
  // Elements: #7ee787 (green) - UI elements
  // Properties/Fields: #79c0ff (blue)
  // Functions/Handlers: #d2a8ff (purple)
  // Strings/Text: #a5d6ff (light blue)
  // Variables/Default: #c9d1d9 (gray)
  function getKindColor(kind: string): string {
    const colors: Record<string, string> = {
      // Root/Structure
      File: "#79c0ff",
      HIR: "#79c0ff",
      THIR: "#79c0ff",
      Package: "#ffa657",

      // Type definitions (orange)
      Component: "#ffa657",
      Record: "#ffa657",
      Enum: "#ffa657",
      Variant: "#ffa657",
      RecordLit: "#ffa657",
      VariantCtor: "#ffa657",

      // Properties and fields (blue)
      Property: "#79c0ff",
      Field: "#79c0ff",
      Binding: "#79c0ff",
      EnumCase: "#79c0ff",
      VariantCase: "#79c0ff",

      // Functions (purple)
      Function: "#d2a8ff",
      Callback: "#d2a8ff",
      Call: "#d2a8ff",
      MethodCall: "#d2a8ff",
      Handler: "#d2a8ff",
      Closure: "#d2a8ff",

      // UI Elements (green)
      Element: "#7ee787",

      // Control flow (red)
      If: "#ff7b72",
      IfBranch: "#ff7b72",
      ElseIfBranch: "#ff7b72",
      ElseBranch: "#ff7b72",
      For: "#ff7b72",
      Match: "#ff7b72",
      MatchArm: "#ff7b72",
      Guard: "#ff7b72",
      Ternary: "#ff7b72",
      Range: "#ff7b72",

      // Text/Strings (light blue)
      Text: "#a5d6ff",
      Literal: "#a5d6ff",
      Interpolation: "#a5d6ff",

      // Variables/Expressions (gray)
      Var: "#c9d1d9",
      Ident: "#c9d1d9",
      Binary: "#c9d1d9",
      Unary: "#c9d1d9",
      Index: "#c9d1d9",
      Member: "#c9d1d9",
      OptionalMember: "#c9d1d9",
      OptionalChain: "#c9d1d9",
      Tuple: "#c9d1d9",
      List: "#c9d1d9",

      // Statements (gray)
      ExprStmt: "#c9d1d9",
      Assign: "#c9d1d9",
      CompoundAssign: "#c9d1d9",
      IfStmt: "#ff7b72",

      // Coercion/Type ops (orange)
      Coerce: "#ffa657",

      // Symbols (purple - like functions)
      Symbol: "#d2a8ff",
      SymbolTable: "#d2a8ff",

      // Errors (red)
      Error: "#ff7b72",
    };
    return colors[kind] || "#c9d1d9";
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
  <div class="node-content flex items-center gap-1.5 py-1 px-2 rounded cursor-pointer transition-colors hover:bg-white/5"
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

    <span class="font-medium shrink-0" style:color={getKindColor(node.kind)}>
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
</style>
