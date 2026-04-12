import { markRaw, ref, shallowRef, triggerRef } from "vue"
import { useVirtualizer } from "@tanstack/vue-virtual"
import { DisplayCallbacks, TraceBlock, VueDisplay } from "../../lpm/output/vue"

export function useOutputPane() {
  // prevent Vue from making blocks deeply reactive
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
          // push items sequentially to avoid max call stack size exceptions
          // and expensive spread operator usage
          for (const block of buffer) {
            blocks.value.push(block)
          }
          buffer = []
          // manually trigger reactivity b/c we mutated shallow ref
          triggerRef(blocks)
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
        // mark the object as raw to prevent deep reactivity proxying of complex ASTs
        value:
          typeof value === "object" && value !== null ? markRaw(value) : value,
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
    virtualizer,
    blocks,
    scrollEl,
    display,
    scrollToBottom,
  }
}

export type OutputPaneType = ReturnType<typeof useOutputPane>
