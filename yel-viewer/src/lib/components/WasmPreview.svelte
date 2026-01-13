<script lang="ts">
  import { onDestroy, tick } from "svelte";
  import { ScrollArea } from "$lib/components/ui/scroll-area";
  import * as Resizable from "$lib/components/ui/resizable";
  import { YelPreviewHost } from "$lib/preview";

  interface Props {
    wasmBytes: Uint8Array | null;
    onError?: (error: string) => void;
    onStateChange?: (state: Record<string, any>) => void;
  }

  let { wasmBytes, onError, onStateChange }: Props = $props();

  let previewContainer: HTMLDivElement | null = $state(null);
  let host: YelPreviewHost | null = null;
  let error = $state<string | null>(null);
  let isLoading = $state(false);
  let componentState = $state<Record<string, any>>({});

  onDestroy(() => {
    host?.destroy();
    host = null;
  });

  // Load component when WASM bytes or container changes
  $effect(() => {
    if (wasmBytes && previewContainer) {
      loadComponent(wasmBytes, previewContainer);
    }
  });

  async function loadComponent(bytes: Uint8Array, container: HTMLDivElement) {
    // Tear down old host completely
    if (host) {
      host.destroy();
      host = null;
    }

    // Create fresh host
    host = new YelPreviewHost();
    host.init(container);

    isLoading = true;
    error = null;

    try {
      const success = await host.load(bytes, {
        callbacks: {
          incremented: () => console.log("[WasmPreview] incremented callback"),
          decremented: () => console.log("[WasmPreview] decremented callback"),
        },
        onDispatch: () => updateComponentState(),
      });

      if (success) {
        await tick();
        host.mount();
        updateComponentState();
      } else {
        error = "Failed to load component. Check console for details.";
        onError?.(error);
      }
    } catch (e) {
      console.error("[WasmPreview] Error:", e);
      error = e instanceof Error ? e.message : String(e);
      onError?.(error);
    } finally {
      isLoading = false;
    }
  }

  function updateComponentState() {
    if (!host) return;

    // Try to read common properties
    const count = host.getProperty("count");
    const label = host.getProperty("label");
    const items = host.getProperty("items");

    componentState = {
      ...(count !== undefined && { count }),
      ...(label !== undefined && { label }),
      ...(items !== undefined && { items }),
    };

    onStateChange?.(componentState);
  }
</script>

<div class="flex flex-col h-full bg-background">
  <!-- Header -->
  <div
    class="flex items-center justify-between h-12 shrink-0 px-4 bg-card border-b border-border"
  >
    <h3 class="text-xs font-semibold text-muted-foreground">WASM Preview</h3>
    {#if isLoading}
      <span class="text-[11px] text-muted-foreground">Loading...</span>
    {:else if error}
      <span class="text-[11px] text-destructive">Error</span>
    {:else if host?.isMounted()}
      <span class="text-[11px] text-green-500">Running</span>
    {:else}
      <span class="text-[11px] text-muted-foreground">No component</span>
    {/if}
  </div>

  <Resizable.PaneGroup direction="vertical" class="flex-1">
    <!-- Preview Container -->
    <Resizable.Pane defaultSize={70} minSize={30}>
      <ScrollArea class="h-full">
        <div class="p-4">
          {#if error}
            <div class="text-destructive text-sm p-4 bg-destructive/10 rounded">
              <div class="font-semibold mb-2">Preview Error</div>
              <div class="font-mono text-xs">{error}</div>
              <div class="mt-4 text-xs text-muted-foreground">
                <p>
                  Note: The WASM preview requires jco-transpiled components.
                </p>
                <p class="mt-2">
                  The WebAssembly Component Model format cannot be directly
                  instantiated in the browser.
                </p>
              </div>
            </div>
          {:else if !wasmBytes}
            <div class="text-muted-foreground text-sm p-4 text-center">
              <p>No WASM component loaded.</p>
              <p class="text-xs mt-2">
                Compile your Yel code to see the preview.
              </p>
            </div>
          {:else}
            <!-- This div is where the WASM component renders -->
            <div bind:this={previewContainer}></div>
          {/if}
        </div>
      </ScrollArea>
    </Resizable.Pane>

    <Resizable.Handle />

    <!-- State Panel -->
    <Resizable.Pane defaultSize={30} minSize={15}>
      <ScrollArea class="h-full">
        <div class="bg-card p-3 text-xs">
          <div class="font-semibold text-muted-foreground mb-2">
            Component State
          </div>
          {#if Object.keys(componentState).length > 0}
            <div class="flex flex-col gap-1 font-mono">
              {#each Object.entries(componentState) as [key, value]}
                <div class="flex justify-between">
                  <span class="text-blue-400">{key}</span>
                  <span class="text-orange-400">
                    {typeof value === "object"
                      ? JSON.stringify(value)
                      : String(value)}
                  </span>
                </div>
              {/each}
            </div>
          {:else}
            <div class="text-muted-foreground">No state available</div>
          {/if}
        </div>
      </ScrollArea>
    </Resizable.Pane>
  </Resizable.PaneGroup>
</div>
