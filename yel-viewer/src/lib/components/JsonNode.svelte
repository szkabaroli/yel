<script lang="ts">
  import JsonNodeSelf from "./JsonNode.svelte";

  interface Props {
    value: unknown;
    name: string;
    depth?: number;
  }

  let { value, name, depth = 0 }: Props = $props();

  const isObject = $derived(value !== null && typeof value === "object" && !Array.isArray(value));
  const isArray = $derived(Array.isArray(value));
  const isExpandable = $derived(isObject || isArray);
  const shouldExpandByDefault = $derived(depth < 2);

  let expanded = $state(true);

  $effect(() => {
    expanded = shouldExpandByDefault;
  });

  function toggle() {
    expanded = !expanded;
  }

  function getEntries(): [string, unknown][] {
    if (isArray) {
      return (value as unknown[]).map((item, i) => [String(i), item]);
    }
    if (isObject) {
      return Object.entries(value as Record<string, unknown>);
    }
    return [];
  }
</script>

{#if isExpandable}
  <div>
    <button
      class="cursor-pointer hover:bg-secondary/50 py-0.5 px-1 rounded select-none flex items-center gap-1 bg-transparent border-none text-left w-full"
      onclick={toggle}
    >
      <span class="text-muted-foreground w-3">{expanded ? "▼" : "▶"}</span>
      <span class="json-key">{name}</span>
      <span class="text-muted-foreground">
        {#if isArray}
          <span class="json-bracket">[</span>{(value as unknown[]).length}<span class="json-bracket">]</span>
        {:else}
          <span class="json-bracket">{"{"}</span>{Object.keys(value as object).length}<span class="json-bracket">{"}"}</span>
        {/if}
      </span>
    </button>
    {#if expanded}
      <div class="border-l border-border/30 ml-4 pl-3">
        {#each getEntries() as [key, val]}
          <JsonNodeSelf value={val} name={key} depth={depth + 1} />
        {/each}
      </div>
    {/if}
  </div>
{:else}
  <div class="py-0.5 px-1 flex gap-2 ml-4">
    <span class="json-key">{name}:</span>
    {#if typeof value === "string"}
      <span class="json-string">"{value}"</span>
    {:else if typeof value === "number"}
      <span class="json-number">{value}</span>
    {:else if typeof value === "boolean"}
      <span class="json-boolean">{value}</span>
    {:else if value === null}
      <span class="json-null">null</span>
    {:else}
      <span class="text-foreground">{String(value)}</span>
    {/if}
  </div>
{/if}

<style>
  .json-key {
    color: #16a34a;
  }
  .json-string {
    color: #0284c7;
  }
  .json-number {
    color: #7c3aed;
  }
  .json-boolean {
    color: #7c3aed;
  }
  .json-null {
    color: var(--color-muted-foreground);
  }
  .json-bracket {
    color: var(--color-foreground);
  }

  @media (prefers-color-scheme: dark) {
    .json-key {
      color: #7ee787;
    }
    .json-string {
      color: #a5d6ff;
    }
    .json-number {
      color: #79c0ff;
    }
    .json-boolean {
      color: #79c0ff;
    }
  }
</style>
