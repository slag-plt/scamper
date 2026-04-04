<script lang="ts">
  import type { DisplayCallbacks, TraceBlock } from "../lpm/output/svelte"
  import { SvelteDisplay } from "../lpm/output/svelte"
  import HTMLRenderer from "../lpm/renderers/html"
  import { createVirtualizer } from "@tanstack/svelte-virtual"
  import { untrack } from "svelte"

  let blocks: TraceBlock[] = $state([])

  /**
   * virtualizer
   */

  let scrollEl = $state<HTMLDivElement | null>(null)

  const virtualizerBaseOptions = {
    getScrollElement: () => scrollEl,
    estimateSize: () => 50,
    overscan: 40,
  }

  const virtualizer = createVirtualizer<HTMLDivElement, HTMLDivElement>({
    get count() {
      return blocks.length
    },
    ...virtualizerBaseOptions,
  })

  $effect(() => {
    const opts = {
      count: blocks.length,
      ...virtualizerBaseOptions,
    }
    untrack(() => $virtualizer).setOptions(opts)
  })
  function measureItem(el: HTMLDivElement) {
    $virtualizer.measureElement(el)
    return {
      update() {
        $virtualizer.measureElement(el)
      },
    }
  }

  /**
   * display
   */
  // non-reactive buffer to flush every animation frame instead to prioritize user input
  let buffer: TraceBlock[] = []
  let isFlushing = false

  // batching function synchronized with the browser's paint cycle
  function scheduleFlush() {
    if (!isFlushing) {
      isFlushing = true
      requestAnimationFrame(() => {
        if (buffer.length > 0) {
          // empty buffer and push chunks iteratively to avoid max call stack size exceptions
          while (buffer.length > 0) {
            blocks.push(...buffer.splice(0, 10_000))
          }
        }
        isFlushing = false
      })
    }
  }
  const displayInput: DisplayCallbacks = {
    StartSectionCallback: (...attrs) => {
      // push the header of the section
      buffer.push({ attrs })
      scheduleFlush()
    },
    EndSectionCallback: () => {
      // TODO: will push an ending block thing eventually, for now don't do anything
    },
    SendCallback: (value) => {
      buffer.push({ attrs: ["trace-step"], value: HTMLRenderer.render(value) })
      scheduleFlush()
    },
  }

  export const display = new SvelteDisplay(displayInput)

  export function reset(): void {
    // https://medium.com/@lauenroth/how-to-empty-a-const-array-365a81916e10
    blocks = []
  }

  function mountValue(container: HTMLElement, element: HTMLElement) {
    container.appendChild(element)
    return {
      update(newElement: HTMLElement) {
        container.replaceChildren(newElement)
      },
      destroy() {
        container.replaceChildren()
      },
    }
  }
</script>

<div bind:this={scrollEl} style="overflow: auto; height: 100%;">
  <div
    style="position: relative; height: {$virtualizer.getTotalSize()}px; width: 100%;"
  >
    {#each $virtualizer.getVirtualItems() as row (row.index)}
      {@const block = blocks[row.index]}
      <div
        class={block.attrs.join(" ")}
        use:measureItem
        data-index={row.index}
        style="position: absolute; top: 0; left: 0; width: 100%; transform: translateY({row.start}px);"
      >
        {#if block.value}
          <div use:mountValue={block.value}></div>
        {/if}
      </div>
    {/each}
  </div>
</div>

<!-- TODO: should rename these classes when we give them a good name -->
<!-- also get rid of these colors when we actually have fake corners and such -->
<style>
  .trace-start {
    background-color: red;
    min-height: 5vh;
  }
  .trace-steps-start {
    background-color: blue;
    min-height: 5vh;
  }
  .trace-step {
    background-color: green;
  }
</style>
