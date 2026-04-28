<script lang="ts">
    import { onMount, onDestroy } from 'svelte'
    import { instance } from '@viz-js/viz'
    import CopyButton from './CopyButton.svelte'

    interface Props {
        dot: string
    }

    let { dot }: Props = $props()

    let container = $state<HTMLDivElement | null>(null)
    let viz: Awaited<ReturnType<typeof instance>> | null = null
    let error = $state<string | null>(null)

    onMount(async () => {
        try {
            viz = await instance()
            renderGraph()
        } catch (e) {
            error = String(e)
        }
    })

    function renderGraph() {
        if (!viz || !container) return
        if (!dot.trim()) {
            container.replaceChildren()
            error = null
            return
        }
        try {
            const svg = viz.renderSVGElement(dot)
            svg.setAttribute('width', '100%')
            svg.setAttribute('height', '100%')
            svg.style.maxWidth = '100%'
            container.replaceChildren(svg)
            error = null
        } catch (e) {
            error = String(e)
        }
    }

    $effect(() => {
        void dot
        if (viz) renderGraph()
    })
</script>

<div class="flex flex-col h-full bg-background">
    <div
        class="flex items-center justify-between h-12 shrink-0 px-4 bg-card border-b border-border"
    >
        <h3 class="text-xs font-semibold text-muted-foreground">
            Signal Dependency Graph
        </h3>
        <CopyButton text={dot} title="Copy DOT source" />
    </div>

    <div class="flex-1 overflow-auto p-4">
        {#if error}
            <pre class="text-xs text-red-500 whitespace-pre-wrap">{error}

{dot}</pre>
        {:else if !dot.trim()}
            <div class="text-xs text-muted-foreground">No graph available.</div>
        {:else}
            <div bind:this={container} class="yel-graph w-full h-full"></div>
        {/if}
    </div>
</div>

<style>
    /*
     * Dark-mode theming for the rendered DOT signal graph.
     *
     * Graphviz emits hardcoded `fill="#fff5d6"` etc. on each shape. The
     * Rust DOT generator additionally tags every node/edge with a
     * `yel-*` class (yel-signal, yel-effect, yel-fn, yel-domsink,
     * yel-handler, yel-reads, yel-writes, yel-updates, yel-calls) — viz
     * propagates those classes onto the wrapping `<g>` elements, so we
     * can override fills/strokes/text per role from CSS. `!important`
     * is required because Graphviz sets the visual attributes inline
     * on each shape.
     *
     * We only override under `prefers-color-scheme: dark` so the light
     * theme keeps the existing palette (and raw `dot -Tpng` keeps
     * working unchanged).
     */
    @media (prefers-color-scheme: dark) {
        :global(.yel-graph svg polygon) {
            fill: var(--color-background) !important;
        }
        :global(.yel-graph svg text) {
            fill: hsl(0 0% 90%) !important;
        }
        :global(.yel-graph svg .cluster polygon),
        :global(.yel-graph svg .cluster path) {
            fill: var(--color-card) !important;
            stroke: var(--color-border) !important;
        }
        /* Local signals — soft blue */
        :global(.yel-graph svg .yel-signal ellipse) {
            fill: hsl(212 40% 22%) !important;
            stroke: hsl(212 50% 60%) !important;
        }
        /* Global signals — soft pink */
        :global(.yel-graph svg .yel-signal-global ellipse) {
            fill: hsl(340 35% 22%) !important;
            stroke: hsl(340 50% 65%) !important;
        }
        /* Effect entry boxes — warm yellow */
        :global(.yel-graph svg .yel-effect polygon),
        :global(.yel-graph svg .yel-effect path) {
            fill: hsl(45 35% 22%) !important;
            stroke: hsl(45 60% 65%) !important;
        }
        /* Inner update fns — purple */
        :global(.yel-graph svg .yel-fn polygon),
        :global(.yel-graph svg .yel-fn path) {
            fill: hsl(265 30% 25%) !important;
            stroke: hsl(265 50% 70%) !important;
        }
        /* DOM sinks — green */
        :global(.yel-graph svg .yel-domsink ellipse) {
            fill: hsl(135 25% 20%) !important;
            stroke: hsl(135 45% 60%) !important;
        }
        /* Source elements — cyan */
        :global(.yel-graph svg .yel-element polygon),
        :global(.yel-graph svg .yel-element path) {
            fill: hsl(190 30% 22%) !important;
            stroke: hsl(190 50% 60%) !important;
        }
        /* `fires` — element → handler diamond */
        :global(.yel-graph svg .yel-fires path),
        :global(.yel-graph svg .yel-fires polygon) {
            stroke: hsl(265 50% 75%) !important;
            fill: hsl(265 50% 75%) !important;
        }
        :global(.yel-graph svg .yel-fires path) {
            fill: none !important;
        }
        /* `binds` — effect → element (read path leaf) */
        :global(.yel-graph svg .yel-binds path),
        :global(.yel-graph svg .yel-binds polygon) {
            stroke: hsl(135 60% 60%) !important;
            fill: hsl(135 60% 60%) !important;
        }
        :global(.yel-graph svg .yel-binds path) {
            fill: none !important;
        }
        /* Handler diamonds — lavender */
        :global(.yel-graph svg .yel-handler polygon) {
            fill: hsl(285 25% 25%) !important;
            stroke: hsl(285 45% 70%) !important;
        }
        /* Edges — brighten the strokes and arrowheads */
        :global(.yel-graph svg .yel-reads path),
        :global(.yel-graph svg .yel-reads polygon) {
            stroke: hsl(215 80% 70%) !important;
            fill: hsl(215 80% 70%) !important;
        }
        :global(.yel-graph svg .yel-reads path) {
            fill: none !important;
        }
        :global(.yel-graph svg .yel-writes path),
        :global(.yel-graph svg .yel-writes polygon) {
            stroke: hsl(0 70% 65%) !important;
            fill: hsl(0 70% 65%) !important;
        }
        :global(.yel-graph svg .yel-writes path) {
            fill: none !important;
        }
        :global(.yel-graph svg .yel-updates path),
        :global(.yel-graph svg .yel-updates polygon) {
            stroke: hsl(135 60% 60%) !important;
            fill: hsl(135 60% 60%) !important;
        }
        :global(.yel-graph svg .yel-updates path) {
            fill: none !important;
        }
        :global(.yel-graph svg .yel-calls path),
        :global(.yel-graph svg .yel-calls polygon) {
            stroke: hsl(265 60% 75%) !important;
            fill: hsl(265 60% 75%) !important;
        }
        :global(.yel-graph svg .yel-calls path) {
            fill: none !important;
        }
    }
</style>
