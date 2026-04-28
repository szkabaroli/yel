<script>
    import { Button } from '../../components/ui/button'
    let {
        variant = 'default',
        size = 'default',
        disabled = false,
        children,
    } = $props()
</script>

<Button class="btn {variant} size-{size}" {disabled}>
    {@render children?.()}
</Button>

<style>
    /* shadcn dark theme button styles */
    .btn {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 0.5rem;
        white-space: nowrap;
        border-radius: 0.375rem;
        font-size: 0.875rem;
        font-weight: 500;
        font-family: inherit;
        cursor: pointer;
        border: 1px solid transparent;
        transition:
            background-color 0.15s,
            color 0.15s,
            border-color 0.15s;
    }
</style>
