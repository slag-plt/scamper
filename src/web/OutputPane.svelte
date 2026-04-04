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
  let sectionStack: string[] = []

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
      const section = attrs[0]
      sectionStack.push(section)
      buffer.push({ attrs: [section, `${section}-top`] })
      scheduleFlush()
    },
    EndSectionCallback: () => {
      const section = sectionStack.pop()
      if (section) {
        buffer.push({ attrs: [section, `${section}-bottom`] })
      }
      scheduleFlush()
    },
    SendCallback: (value) => {
      const section = sectionStack.length > 0 ? sectionStack[sectionStack.length - 1] : "base";
      buffer.push({ attrs: [`${section}-item`], value: HTMLRenderer.render(value) })
      scheduleFlush()
    },
  }

  export const display = new SvelteDisplay(displayInput)

  export function reset(): void {
    // https://medium.com/@lauenroth/how-to-empty-a-const-array-365a81916e10
    blocks = []
    sectionStack = []
  }

  // TODO: this should be replaced with a svelte component to render values
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
        style="position: absolute; top: 0; left: 0; width: 100%; box-sizing: border-box; transform: translateY({row.start}px);"
      >
        {#if block.value}
          <div use:mountValue={block.value}></div>
        {/if}
      </div>
    {/each}
  </div>
</div>

<style>
  /* Outer Trace Block */
  .trace-top {
    height: 0.75em;
    position: relative;
  }
  .trace-top::after {
    content: "";
    position: absolute;
    top: 0.25em; /* Separation gap measured perfectly! */
    bottom: 0; left: 0; right: 0;
    background-color: #f6f8fa;
    border: 0.1em solid #d0d7de;
    border-bottom: none;
    border-top-left-radius: 0.5em;
    border-top-right-radius: 0.5em;
    z-index: -1;
  }

  .trace-item {
    position: relative;
    padding: 0.25em 0.5em;
  }
  .trace-item::after {
    content: "";
    position: absolute;
    top: 0; bottom: 0; left: 0; right: 0;
    background-color: #f6f8fa;
    border-left: 0.1em solid #d0d7de;
    border-right: 0.1em solid #d0d7de;
    z-index: -1;
  }

  .trace-bottom {
    height: 0.75em;
    position: relative;
  }
  .trace-bottom::after {
    content: "";
    position: absolute;
    top: 0; bottom: 0.25em; left: 0; right: 0;
    background-color: #f6f8fa;
    border: 0.1em solid #d0d7de;
    border-top: none;
    border-bottom-left-radius: 0.5em;
    border-bottom-right-radius: 0.5em;
    z-index: -1;
  }

  /* Inner Trace Steps Block */
  .trace-steps-top {
    height: 0.5em;
    position: relative;
  }
  .trace-steps-top::before {
    content: "";
    position: absolute;
    top: 0; bottom: 0; left: 0; right: 0;
    background-color: #f6f8fa; /* Maintain outer frame color cleanly! */
    border-left: 0.1em solid #d0d7de;
    border-right: 0.1em solid #d0d7de;
    z-index: -2;
  }
  .trace-steps-top::after {
    content: "";
    position: absolute;
    top: 0.25em; bottom: 0;
    left: 1em; right: 1em; /* Indented frame layout horizontally shortened */
    background-color: #ffffff;
    border: 0.1em dashed #d0d7de;
    border-bottom: none;
    border-top-left-radius: 0.5em;
    border-top-right-radius: 0.5em;
    z-index: -1;
  }

  .trace-steps-item {
    position: relative;
    padding: 0.25em 1.5em; /* Deeply indented physical content */
  }
  .trace-steps-item::before {
    content: "";
    position: absolute;
    top: 0; bottom: 0; left: 0; right: 0;
    background-color: #f6f8fa;
    border-left: 0.1em solid #d0d7de;
    border-right: 0.1em solid #d0d7de;
    z-index: -2;
  }
  .trace-steps-item::after {
    content: "";
    position: absolute;
    top: 0; bottom: 0;
    left: 1em; right: 1em;
    background-color: #ffffff;
    border-left: 0.1em dashed #d0d7de;
    border-right: 0.1em dashed #d0d7de;
    z-index: -1;
  }

  .trace-steps-bottom {
    height: 0.5em;
    position: relative;
  }
  .trace-steps-bottom::before {
    content: "";
    position: absolute;
    top: 0; bottom: 0; left: 0; right: 0;
    background-color: #f6f8fa;
    border-left: 0.1em solid #d0d7de;
    border-right: 0.1em solid #d0d7de;
    z-index: -2;
  }
  .trace-steps-bottom::after {
    content: "";
    position: absolute;
    top: 0; bottom: 0.25em;
    left: 1em; right: 1em;
    background-color: #ffffff;
    border: 0.1em dashed #d0d7de;
    border-top: none;
    border-bottom-left-radius: 0.5em;
    border-bottom-right-radius: 0.5em;
    z-index: -1;
  }

  /* Standard isolated outputs */
  .base-item {
    padding: 0.25em;
  }
</style>
