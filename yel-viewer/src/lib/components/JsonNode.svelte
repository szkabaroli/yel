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
      class="cursor-pointer hover:bg-white/5 py-0.5 px-1 rounded select-none flex items-center gap-1 bg-transparent border-none text-left w-full"
      onclick={toggle}
    >
      <span class="text-muted-foreground w-3">{expanded ? "▼" : "▶"}</span>
      <span style:color="#7ee787">{name}</span>
      <span class="text-muted-foreground">
        {#if isArray}
          <span style:color="#c9d1d9">[</span>{(value as unknown[]).length}<span style:color="#c9d1d9">]</span>
        {:else}
          <span style:color="#c9d1d9">{"{"}</span>{Object.keys(value as object).length}<span style:color="#c9d1d9">{"}"}</span>
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
    <span style:color="#7ee787">{name}:</span>
    {#if typeof value === "string"}
      <span style:color="#a5d6ff">"{value}"</span>
    {:else if typeof value === "number"}
      <span style:color="#79c0ff">{value}</span>
    {:else if typeof value === "boolean"}
      <span style:color="#79c0ff">{value}</span>
    {:else if value === null}
      <span style:color="#8b949e">null</span>
    {:else}
      <span style:color="#c9d1d9">{String(value)}</span>
    {/if}
  </div>
{/if}
