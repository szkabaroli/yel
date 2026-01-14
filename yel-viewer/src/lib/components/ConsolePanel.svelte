<script lang="ts">
  import type { Diagnostic } from "../compiler";
  import { Badge } from "$lib/components/ui/badge";

  interface Props {
    diagnostics: Diagnostic[];
    onClickDiagnostic?: (diagnostic: Diagnostic) => void;
  }

  let { diagnostics, onClickDiagnostic }: Props = $props();

  const errors = $derived(
    diagnostics.filter((d) => d.severity === "error").length,
  );
  const warnings = $derived(
    diagnostics.filter((d) => d.severity === "warning").length,
  );
</script>

<div class="flex flex-col h-full bg-background text-xs">
  <header
    class="flex items-center gap-3 h-9 shrink-0 px-3 bg-card border-b border-border select-none text-muted-foreground"
  >
    <span>Problems</span>
    <div class="flex items-center gap-2">
      <Badge variant={errors > 0 ? "destructive" : "secondary"}>
        {errors} errors
      </Badge>
      <Badge variant={warnings > 0 ? "warning" : "secondary"}>
        {warnings} warnings
      </Badge>
    </div>
  </header>

  <div class="flex-1 min-h-0 overflow-y-auto font-mono py-1">
    {#each diagnostics as diagnostic}
      <button
        type="button"
        class="flex items-center gap-3 w-full text-left px-3 py-1 hover:bg-secondary focus:bg-accent focus:outline-none"
        onclick={() => onClickDiagnostic?.(diagnostic)}
      >
        <Badge
          variant={diagnostic.severity === "error"
            ? "destructive"
            : "secondary"}
        >
          {diagnostic.severity}
        </Badge>
        <span class="shrink-0 text-muted-foreground tabular-nums text-right">
          {diagnostic.line}:{diagnostic.column}
        </span>
        <span class="flex-1 text-foreground truncate"
          >{diagnostic.message}</span
        >
      </button>
    {/each}
  </div>
</div>
