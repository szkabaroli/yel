
<script>
  import { Button } from "../../components/ui/button";
  let { variant = "default", size = "default", disabled = false } = $props();
</script>

<Button class="btn {variant} size-{size}" {disabled}>
  <slot></slot>
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

  .btn:focus-visible {
    outline: 2px solid hsl(240 5% 34%);
    outline-offset: 2px;
  }

  .btn:disabled {
    pointer-events: none;
    opacity: 0.5;
  }

  /* Sizes */
  .size-default {
    height: 2.25rem;
    padding: 0.5rem 1rem;
  }
  .size-sm {
    height: 2rem;
    padding: 0.375rem 0.75rem;
    font-size: 0.75rem;
  }
  .size-lg {
    height: 2.5rem;
    padding: 0.5rem 2rem;
  }
  .size-icon {
    height: 2.25rem;
    width: 2.25rem;
    padding: 0;
  }

  /* Variants - shadcn dark theme (zinc) */
  .default {
    background: hsl(0 0% 98%);
    color: hsl(240 10% 4%);
    box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);
  }
  .default:hover:not(:disabled) {
    background: hsl(0 0% 90%);
  }

  .secondary {
    background: hsl(240 4% 16%);
    color: hsl(0 0% 98%);
    box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);
  }
  .secondary:hover:not(:disabled) {
    background: hsl(240 4% 26%);
  }

  .destructive {
    background: hsl(0 63% 31%);
    color: hsl(0 0% 98%);
    box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);
  }
  .destructive:hover:not(:disabled) {
    background: hsl(0 63% 41%);
  }

  .outline {
    background: transparent;
    color: hsl(0 0% 98%);
    border-color: hsl(240 4% 16%);
    box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);
  }
  .outline:hover:not(:disabled) {
    background: hsl(240 4% 16%);
  }

  .ghost {
    background: transparent;
    color: hsl(0 0% 98%);
  }
  .ghost:hover:not(:disabled) {
    background: hsl(240 4% 16%);
  }

  .link {
    background: transparent;
    color: hsl(0 0% 98%);
    text-decoration: underline;
    text-underline-offset: 4px;
  }
  .link:hover:not(:disabled) {
    text-decoration-thickness: 2px;
  }
</style>
