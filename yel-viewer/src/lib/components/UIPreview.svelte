<script lang="ts">
  import type { Span } from "../types";
  import { Button } from "$lib/components/ui/button";
  import { ScrollArea } from "$lib/components/ui/scroll-area";
  import * as Resizable from "$lib/components/ui/resizable";

  interface Props {
    onSelectSpan?: (span: Span) => void;
    onHoverSpan?: (span: Span | null) => void;
  }

  let { onSelectSpan, onHoverSpan }: Props = $props();

  // Simulated reactive state for the Counter component
  let count = $state(0);
  const label = "Count";
  const items = [
    { name: "Alice", age: 30 },
    { name: "Bob", age: 25 },
  ];

  // Span mappings for each UI element
  const spans = {
    vstack: { start: 282, end: 786 },
    text_label: { start: 299, end: 326 },
    hstack: { start: 336, end: 560 },
    button_minus: { start: 357, end: 438 },
    button_plus: { start: 453, end: 550 },
    if_block: { start: 571, end: 695 },
    text_high: { start: 599, end: 622 },
    text_negative: { start: 664, end: 685 },
    for_block: { start: 704, end: 780 },
  };

  function handleClick(span: Span) {
    onSelectSpan?.(span);
  }

  function handleHover(span: Span | null) {
    onHoverSpan?.(span);
  }

  function handleKeydown(e: KeyboardEvent, span: Span) {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      handleClick(span);
    }
  }
</script>

<div class="flex flex-col h-full bg-background">
  <!-- Header -->
  <div class="flex items-center justify-between h-12 shrink-0 px-4 bg-card border-b border-border">
    <h3 class="text-xs font-semibold text-muted-foreground">UI Preview</h3>
  </div>

  <Resizable.PaneGroup direction="vertical" class="flex-1">
    <!-- Preview -->
    <Resizable.Pane defaultSize={70} minSize={30}>
      <ScrollArea class="h-full">
        <div class="p-4">
    <!-- VStack -->
    <div
      class="element-outline flex flex-col gap-3 p-2 rounded cursor-pointer"
      role="button"
      tabindex="0"
      onmouseenter={() => handleHover(spans.vstack)}
      onmouseleave={() => handleHover(null)}
      onclick={() => handleClick(spans.vstack)}
      onkeydown={(e) => handleKeydown(e, spans.vstack)}
    >
      <!-- Text: {label}: {count} -->
      <div
        class="element-outline py-2 px-3 rounded cursor-pointer text-center text-lg font-semibold text-foreground"
        role="button"
        tabindex="0"
        onmouseenter={() => handleHover(spans.text_label)}
        onmouseleave={() => handleHover(null)}
        onclick={(e) => { e.stopPropagation(); handleClick(spans.text_label); }}
        onkeydown={(e) => { e.stopPropagation(); handleKeydown(e, spans.text_label); }}
      >
        {label}: {count}
      </div>

      <!-- HStack with buttons -->
      <div
        class="element-outline flex flex-row gap-2 justify-center p-2 rounded cursor-pointer"
        role="button"
        tabindex="0"
        onmouseenter={() => handleHover(spans.hstack)}
        onmouseleave={() => handleHover(null)}
        onclick={(e) => { e.stopPropagation(); handleClick(spans.hstack); }}
        onkeydown={(e) => { e.stopPropagation(); handleKeydown(e, spans.hstack); }}
      >
        <Button
          variant="secondary"
          size="lg"
          class="element-outline text-lg font-semibold px-6"
          onmouseenter={() => handleHover(spans.button_minus)}
          onmouseleave={() => handleHover(null)}
          onclick={(e: MouseEvent) => { e.stopPropagation(); count -= 1; handleClick(spans.button_minus); }}
        >
          -
        </Button>
        <Button
          variant="secondary"
          size="lg"
          class="element-outline text-lg font-semibold px-6"
          onmouseenter={() => handleHover(spans.button_plus)}
          onmouseleave={() => handleHover(null)}
          onclick={(e: MouseEvent) => { e.stopPropagation(); count += 1; handleClick(spans.button_plus); }}
        >
          +
        </Button>
      </div>

      <!-- Conditional: if count > 10 -->
      {#if count > 10}
        <div
          class="element-outline py-2 px-3 rounded cursor-pointer text-center text-sm text-yellow-400"
          role="button"
          tabindex="0"
          onmouseenter={() => handleHover(spans.text_high)}
          onmouseleave={() => handleHover(null)}
          onclick={(e) => { e.stopPropagation(); handleClick(spans.text_high); }}
          onkeydown={(e) => { e.stopPropagation(); handleKeydown(e, spans.text_high); }}
        >
          High count!
        </div>
      {:else if count < 0}
        <div
          class="element-outline py-2 px-3 rounded cursor-pointer text-center text-sm text-destructive-foreground bg-destructive/20"
          role="button"
          tabindex="0"
          onmouseenter={() => handleHover(spans.text_negative)}
          onmouseleave={() => handleHover(null)}
          onclick={(e) => { e.stopPropagation(); handleClick(spans.text_negative); }}
          onkeydown={(e) => { e.stopPropagation(); handleKeydown(e, spans.text_negative); }}
        >
          Negative!
        </div>
      {/if}

      <!-- For loop: items -->
      <div
        class="element-outline flex flex-col gap-1 p-2 rounded cursor-pointer"
        role="button"
        tabindex="0"
        onmouseenter={() => handleHover(spans.for_block)}
        onmouseleave={() => handleHover(null)}
        onclick={(e) => { e.stopPropagation(); handleClick(spans.for_block); }}
        onkeydown={(e) => { e.stopPropagation(); handleKeydown(e, spans.for_block); }}
      >
        {#each items as item (item.name)}
          <div class="text-sm text-blue-400 py-1 px-2">{item.name}</div>
        {/each}
      </div>
        </div>
        </div>
      </ScrollArea>
    </Resizable.Pane>

    <Resizable.Handle />

    <!-- State Panel -->
    <Resizable.Pane defaultSize={30} minSize={15}>
      <ScrollArea class="h-full">
        <div class="bg-card p-3 text-xs">
          <div class="font-semibold text-muted-foreground mb-2">Reactive State</div>
          <div class="flex flex-col gap-1 font-mono">
            <div class="flex justify-between">
              <span class="text-blue-400">count</span>
              <span class="text-orange-400">{count}</span>
            </div>
            <div class="flex justify-between">
              <span class="text-blue-400">label</span>
              <span class="text-orange-400">"{label}"</span>
            </div>
            <div class="flex justify-between">
              <span class="text-blue-400">items</span>
              <span class="text-orange-400">[{items.length} items]</span>
            </div>
          </div>
        </div>
      </ScrollArea>
    </Resizable.Pane>
  </Resizable.PaneGroup>
</div>

<style>
  :global(.element-outline) {
    border: 1px solid transparent !important;
    border-radius: 2px !important;
    transition: border-color 0.15s;
  }

  :global(.element-outline:hover) {
    border-color: #7ee787 !important;
  }
</style>
