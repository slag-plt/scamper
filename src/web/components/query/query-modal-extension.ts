import { ViewPlugin, type EditorView } from "@codemirror/view"
import type { Extension } from "@codemirror/state"

export interface PopupCoords {
  top: number
  bottom: number
  left: number
  right: number
}

export function toPopupCoords(
  rect: { top: number; bottom: number; left: number; right: number } | null,
): PopupCoords | null {
  return rect
    ? { top: rect.top, bottom: rect.bottom, left: rect.left, right: rect.right }
    : null
}

export function createViewChangeNotifier() {
  const listeners = new Set<() => void>()
  let scrollEl: HTMLElement | null = null
  let onScroll: (() => void) | null = null

  const notify = () => {
    for (const listener of listeners) {
      listener()
    }
  }

  const ensureScrollListener = (view: EditorView) => {
    const el = view.scrollDOM
    if (scrollEl === el) return
    if (scrollEl && onScroll) {
      scrollEl.removeEventListener("scroll", onScroll)
    }
    scrollEl = el
    onScroll = () => {
      notify()
    }
    scrollEl.addEventListener("scroll", onScroll, { passive: true })
  }

  const detachScrollListener = () => {
    if (scrollEl && onScroll) {
      scrollEl.removeEventListener("scroll", onScroll)
      scrollEl = null
      onScroll = null
    }
  }

  const extension: Extension = ViewPlugin.fromClass(
    class {
      constructor(view: EditorView) {
        ensureScrollListener(view)
      }
      update() {
        notify()
      }
      destroy() {
        // Keep the scroll listener while modals are subscribed; CM can destroy
        // the plugin without reconstructing it (e.g. internal state updates).
        if (listeners.size === 0) {
          detachScrollListener()
        }
      }
    },
  )
  return {
    extension,
    dispose() {
      detachScrollListener()
    },
    subscribe(listener: () => void): () => void {
      listeners.add(listener)
      return () => {
        listeners.delete(listener)
      }
    },
  }
}
