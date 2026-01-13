<svelte:options customElement="yel-text" />

<script>
  let { variant = 'default', size = 'default', weight = 'normal' } = $props();
</script>

<span class="text {variant} size-{size} weight-{weight}">
  <slot></slot>
</span>

<style>
  .text {
    display: inline-block;
    font-family: inherit;
    line-height: 1.5;
  }

  /* Variants - shadcn dark theme */
  .default { color: hsl(0 0% 98%); }
  .muted { color: hsl(240 4% 46%); }
  .destructive { color: hsl(0 63% 31%); }

  /* Sizes */
  .size-xs { font-size: 0.75rem; }
  .size-sm { font-size: 0.875rem; }
  .size-default { font-size: 1rem; }
  .size-lg { font-size: 1.125rem; }
  .size-xl { font-size: 1.25rem; }
  .size-2xl { font-size: 1.5rem; line-height: 1.3; }
  .size-3xl { font-size: 1.875rem; line-height: 1.2; }
  .size-4xl { font-size: 2.25rem; line-height: 1.1; }

  /* Weights */
  .weight-normal { font-weight: 400; }
  .weight-medium { font-weight: 500; }
  .weight-semibold { font-weight: 600; }
  .weight-bold { font-weight: 700; }
</style>
