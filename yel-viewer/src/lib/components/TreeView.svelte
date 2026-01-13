<script lang="ts">
  import TreeNodeComponent from "./TreeNode.svelte";
  import type { TreeNode } from "../types";
  import { ScrollArea } from "$lib/components/ui/scroll-area";
  import * as Resizable from "$lib/components/ui/resizable";
  import { Input } from "$lib/components/ui/input";

  interface Props {
    tree: TreeNode;
    selectedNode?: TreeNode | null;
    hoveredNode?: TreeNode | null;
    onSelect?: (node: TreeNode) => void;
    onHover?: (node: TreeNode | null) => void;
  }

  let {
    tree,
    selectedNode = null,
    hoveredNode = null,
    onSelect,
    onHover,
  }: Props = $props();

  // Show details for hovered node (priority) or selected node
  const detailNode = $derived(hoveredNode ?? selectedNode);

  let searchQuery = $state("");

  // Helper to find matching nodes
  function findMatches(node: TreeNode, query: string): Set<string> {
    const matches = new Set<string>();
    const q = query.toLowerCase();

    function traverse(n: TreeNode) {
      if (
        n.label.toLowerCase().includes(q) ||
        n.kind.toLowerCase().includes(q)
      ) {
        matches.add(n.id);
      }
      for (const child of n.children) {
        traverse(child);
      }
    }

    if (query) {
      traverse(node);
    }

    return matches;
  }

  const matchingNodes = $derived(findMatches(tree, searchQuery));
  const matchCount = $derived(matchingNodes.size);
</script>

<div class="flex flex-col h-full bg-background text-foreground">
  <!-- Header -->
  <div class="flex items-center justify-between h-12 shrink-0 px-4 border-b border-border bg-card">
    <h3 class="text-xs font-semibold text-muted-foreground">AST</h3>
  </div>

  <Resizable.PaneGroup direction="vertical" class="flex-1">
    <!-- Tree -->
    <Resizable.Pane defaultSize={70} minSize={30}>
      <ScrollArea class="h-full">
        <div class="p-3" role="tree">
          <TreeNodeComponent node={tree} selectedId={selectedNode?.id} {onSelect} {onHover} />
        </div>
      </ScrollArea>
    </Resizable.Pane>

    {#if detailNode}
      <Resizable.Handle />

      <!-- Details Panel -->
      <Resizable.Pane defaultSize={30} minSize={15}>
        <ScrollArea class="h-full">
          <div class="bg-card p-3 text-xs">
            <div class="font-semibold text-cyan-400 mb-1">{detailNode.kind}</div>
            <div class="text-foreground mb-1 font-mono">{detailNode.label}</div>
            {#if detailNode.span && (detailNode.span.start > 0 || detailNode.span.end > 0)}
              <div class="text-muted-foreground mb-2">
                Span: {detailNode.span.start} - {detailNode.span.end}
                ({detailNode.span.end - detailNode.span.start} chars)
              </div>
            {/if}
            {#if detailNode.details}
              <div class="flex flex-col gap-0.5">
                {#each Object.entries(detailNode.details) as [key, value]}
                  <div class="flex gap-2">
                    <span class="text-blue-400">{key}:</span>
                    <span class="text-orange-400 font-mono">{JSON.stringify(value)}</span>
                  </div>
                {/each}
              </div>
            {/if}
          </div>
        </ScrollArea>
      </Resizable.Pane>
    {/if}
  </Resizable.PaneGroup>
</div>
