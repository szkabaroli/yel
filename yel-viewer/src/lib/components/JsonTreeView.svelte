<script lang="ts">
  import { ScrollArea } from "$lib/components/ui/scroll-area";
  import JsonNode from "./JsonNode.svelte";
  import CopyButton from "./CopyButton.svelte";

  interface Props {
    data: unknown;
    label?: string;
  }

  let { data, label = "root" }: Props = $props();

  const jsonText = $derived(
    (() => {
      try {
        return JSON.stringify(data, null, 2);
      } catch {
        return String(data);
      }
    })()
  );
</script>

<div class="flex flex-col h-full bg-background text-foreground overflow-hidden">
  <div class="flex items-center justify-between h-12 shrink-0 px-4 border-b border-border bg-card">
    <h3 class="text-xs font-semibold text-muted-foreground">{label} (JSON)</h3>
    <CopyButton text={jsonText} title="Copy JSON" />
  </div>
  <ScrollArea class="flex-1 h-0">
    <div class="p-3 font-mono text-[13px]">
      <JsonNode value={data} name={label} depth={0} />
    </div>
  </ScrollArea>
</div>
