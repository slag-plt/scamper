import { ref, shallowRef } from "vue"
import { useVirtualizer } from "@tanstack/vue-virtual"
import HTMLRenderer from "../../lpm/renderers/html"
import { DisplayCallbacks, TraceBlock, VueDisplay } from "../../lpm/output/vue"

export function useOutputPane() {
  // prevent Vue from making blocks deep reactive
  const blocks = shallowRef<TraceBlock[]>([])

  /**
   * virtualizer setup
   */
  const scrollEl = ref<HTMLDivElement | null>(null)

  const virtualizer = useVirtualizer({
    get count() {
      return blocks.value.length
    },
    getScrollElement: () => scrollEl.value,
    estimateSize: () => 50,
    overscan: 40,
  })

  /**
   * custom directive to mount native DOM elements into Vue
   */
  const vMountValue = {
    mounted(el: HTMLElement, binding: { value: HTMLElement }) {
      if (binding.value) el.appendChild(binding.value)
    },
    updated(
      el: HTMLElement,
      binding: { value: HTMLElement; oldValue?: HTMLElement },
    ) {
      if (binding.value !== binding.oldValue) {
        if (binding.value) el.replaceChildren(binding.value)
        else el.replaceChildren()
      }
    },
    unmounted(el: HTMLElement) {
      el.replaceChildren()
    },
  }

  /**
   * display / batching logic
   */
  let buffer: TraceBlock[] = []
  let isFlushing = false
  let sectionStack: string[] = []

  function scheduleFlush() {
    if (!isFlushing) {
      isFlushing = true
      requestAnimationFrame(() => {
        if (buffer.length > 0) {
          // Create a new array reference so `shallowRef` triggers reactivity updates
          const nextBlocks = [...blocks.value]

          // Empty buffer and push chunks iteratively to avoid max call stack size exceptions
          while (buffer.length > 0) {
            nextBlocks.push(...buffer.splice(0, 10_000))
          }
          blocks.value = nextBlocks
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
      const section =
        sectionStack.length > 0 ? sectionStack[sectionStack.length - 1] : "base"
      buffer.push({
        attrs: [`${section}-item`],
        value: HTMLRenderer.render(value),
      })
      scheduleFlush()
    },
  }

  function reset(): void {
    blocks.value = []
    sectionStack = []
    buffer = []
  }

  function scrollToBottom(): void {
    if (scrollEl.value) {
      scrollEl.value.scrollTo(0, scrollEl.value.scrollHeight)
    }
  }

  const display = new VueDisplay(displayInput)

  return {
    displayInput,
    reset,
    vMountValue,
    virtualizer,
    blocks,
    scrollEl,
    display,
    scrollToBottom,
  }
}

export type OutputPaneType = ReturnType<typeof useOutputPane>
