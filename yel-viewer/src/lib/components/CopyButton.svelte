<script lang="ts">
    import { Button } from '$lib/components/ui/button'

    interface Props {
        /** Text content to copy to the clipboard. */
        text: string
        /** Optional accessible label. */
        title?: string
    }

    let { text, title = 'Copy to clipboard' }: Props = $props()

    let copied = $state(false)
    let resetTimer: ReturnType<typeof setTimeout> | null = null

    async function handleCopy() {
        try {
            await navigator.clipboard.writeText(text)
        } catch (err) {
            console.error('[CopyButton] clipboard write failed:', err)
            return
        }
        copied = true
        if (resetTimer) clearTimeout(resetTimer)
        resetTimer = setTimeout(() => {
            copied = false
        }, 1200)
    }
</script>

<Button
    variant="ghost"
    size="icon-sm"
    {title}
    aria-label={title}
    onclick={handleCopy}
>
    {#if copied}
        <!-- Check icon -->
        <svg
            xmlns="http://www.w3.org/2000/svg"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            stroke-linecap="round"
            stroke-linejoin="round"
            class="size-4 text-green-500"
            aria-hidden="true"
        >
            <path d="M20 6L9 17l-5-5" />
        </svg>
    {:else}
        <!-- Copy icon -->
        <svg
            xmlns="http://www.w3.org/2000/svg"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            stroke-linecap="round"
            stroke-linejoin="round"
            class="size-4"
            aria-hidden="true"
        >
            <rect x="9" y="9" width="13" height="13" rx="2" ry="2" />
            <path
                d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"
            />
        </svg>
    {/if}
</Button>
