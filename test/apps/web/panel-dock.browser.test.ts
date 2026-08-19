import { mount } from '@vue/test-utils'
import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { defineComponent, h, nextTick, ref, type Ref } from 'vue'
import PanelDock from '../../../src/app/web/components/PanelDock.vue'
import PanelFrame from '../../../src/app/web/components/PanelFrame.vue'
import CodeMirrorEditor from '../../../src/app/web/components/CodeMirrorEditor.vue'
import { providePanels, type Panels } from '../../../src/app/web/composables/use-panels'
import { provideEditor } from '../../../src/app/web/composables/editor-context'
import { initialize } from '../../../src/scamper'
import type { PanelId } from '../../../src/app/web/panel-layout'

// CodeMirror's query extension reaches for the Scamper singleton as it builds
// its initial state, so the language has to be up before any editor mounts.
await initialize()

// The half of the dock that needs a real browser.
//
// Everything decidable from plain data lives in panel-layout.test.ts and
// panel-frame.test.ts, which run under jsdom in `npm test`. jsdom reports every
// box as zero-sized, so none of it can answer the questions below: whether a
// drag actually moves anything, whether clamp() keeps a window inside a pane
// that just shrank, or whether CodeMirror still has a height after being hidden
// and shown again.
//
// Nothing load-bearing may live only here: this suite is deliberately excluded
// from `npm test` and `npm run validate` (a missing Playwright binary fails
// browser-mode startup outright), so it asserts pixels and nothing else.

const LABELS: Record<PanelId, string> = {
  editor: 'Source',
  output: 'Output',
  trace: 'Step',
}

const DOCK_W = 900
const DOCK_H = 500

let panels: Panels
let host: HTMLDivElement

interface Harness {
  wrapper: ReturnType<typeof mount>
  size: Ref<{ w: number; h: number }>
}

/**
 * Mounts the dock inside a box of a known size, which is the whole point of
 * running here: the frames measure their parent, so it has to have one.
 */
function mountDock(withEditor = false): Harness {
  const size = ref({ w: DOCK_W, h: DOCK_H })
  const present = ref<readonly PanelId[]>(['editor', 'output', 'trace'])
  const Host = defineComponent({
    setup() {
      panels = providePanels({ isCompact: ref(false), present })
      if (withEditor) provideEditor()
      return () =>
        h(
          'div',
          {
            style: {
              width: `${String(size.value.w)}px`,
              height: `${String(size.value.h)}px`,
              display: 'flex',
              flexDirection: 'column',
            },
          },
          h(
            PanelDock,
            { labels: LABELS, closeable: ['trace'], style: 'flex: 1; min-height: 0' },
            () => [
              h(PanelFrame, { id: 'editor', title: 'Source' }, () =>
                withEditor
                  ? h(CodeMirrorEditor)
                  : h('p', { style: 'margin:0' }, 'the code'),
              ),
              h(PanelFrame, { id: 'output', title: 'Output' }, () =>
                h('p', { style: 'margin:0' }, 'the output'),
              ),
              h(PanelFrame, { id: 'trace', title: 'Step', closeable: true }, () =>
                h('p', { style: 'margin:0' }, 'the trace'),
              ),
            ],
          ),
        )
    },
  })
  return { wrapper: mount(Host, { attachTo: host }), size }
}

const frame = (id: PanelId) =>
  host.querySelector<HTMLElement>(`[data-panel="${id}"]`)
const dock = () => host.querySelector<HTMLElement>('.dock')
const rect = (el: Element | null) => el?.getBoundingClientRect()

/** A pointer drag from `from` to `to`, in client coordinates. */
function drag(
  el: HTMLElement,
  from: { x: number; y: number },
  to: { x: number; y: number },
) {
  const opts = { bubbles: true, pointerId: 1, isPrimary: true }
  el.dispatchEvent(new PointerEvent('pointerdown', { ...opts, clientX: from.x, clientY: from.y }))
  el.dispatchEvent(new PointerEvent('pointermove', { ...opts, clientX: to.x, clientY: to.y }))
  el.dispatchEvent(new PointerEvent('pointerup', { ...opts, clientX: to.x, clientY: to.y }))
}

/** Lets a ResizeObserver fire and Vue re-render afterwards. */
async function settle() {
  for (let i = 0; i < 4; i++) {
    await new Promise((r) => requestAnimationFrame(r))
    await nextTick()
  }
}

beforeEach(() => {
  localStorage.clear()
  host = document.createElement('div')
  document.body.appendChild(host)
})

afterEach(() => {
  host.remove()
  document.body.innerHTML = ''
})

describe('the splitter', () => {
  test('dragging it resizes both slots', async () => {
    const { wrapper } = mountDock()
    try {
      panels.dock('output')
      await settle()

      const before = rect(frame('editor'))?.width ?? 0
      expect(before).toBeGreaterThan(0)

      const sp = host.querySelector<HTMLElement>('[role="separator"]')
      const box = rect(sp)
      expect(box).toBeDefined()

      drag(
        sp as HTMLElement,
        { x: box.x + box.width / 2, y: box.y + box.height / 2 },
        { x: box.x - 200, y: box.y + box.height / 2 },
      )
      await settle()

      const after = rect(frame('editor'))?.width ?? 0
      const other = rect(frame('output'))?.width ?? 0
      expect(after).toBeLessThan(before - 150)
      // The two slots plus the gutter still account for the whole dock.
      expect(after + other).toBeGreaterThan(DOCK_W - 40)
    } finally {
      wrapper.unmount()
    }
  })

  test('a slot cannot be dragged shut', async () => {
    const { wrapper } = mountDock()
    try {
      panels.dock('output')
      await settle()

      const sp = host.querySelector<HTMLElement>('[role="separator"]')
      const box = rect(sp)
      // Well past the left edge of the dock.
      drag(
        sp as HTMLElement,
        { x: box.x, y: box.y + 10 },
        { x: box.x - 2000, y: box.y + 10 },
      )
      await settle()

      // 15% is the floor the model clamps to.
      expect(rect(frame('editor'))?.width ?? 0).toBeGreaterThan(DOCK_W * 0.14)
      expect(panels.layout.value.splitPercent).toBe(15)
    } finally {
      wrapper.unmount()
    }
  })
})

describe('a floating panel', () => {
  test('the title bar drags it', async () => {
    const { wrapper } = mountDock()
    try {
      await settle()
      const win = frame('output')
      const before = rect(win)
      const bar = win?.querySelector<HTMLElement>('.window-bar')

      drag(
        bar as HTMLElement,
        { x: before.x + 40, y: before.y + 6 },
        { x: before.x - 60, y: before.y - 40 },
      )
      await settle()

      const after = rect(frame('output'))
      expect(Math.round(after.x)).toBeLessThan(Math.round(before.x))
      expect(Math.round(after.y)).toBeLessThan(Math.round(before.y))
      // Moved, not resized.
      expect(Math.round(after.width)).toBe(Math.round(before.width))
    } finally {
      wrapper.unmount()
    }
  })

  test('an edge handle resizes it', async () => {
    const { wrapper } = mountDock()
    try {
      await settle()
      const win = frame('output')
      const before = rect(win)
      const handle = win?.querySelector<HTMLElement>('.resize-w')

      drag(
        handle as HTMLElement,
        { x: before.x, y: before.y + before.height / 2 },
        { x: before.x - 80, y: before.y + before.height / 2 },
      )
      await settle()

      const after = rect(frame('output'))
      // Pulling the left edge widens the window and moves its left edge.
      expect(after.width).toBeGreaterThan(before.width + 50)
      expect(after.x).toBeLessThan(before.x)
      // The right edge stays put.
      expect(Math.round(after.x + after.width)).toBe(
        Math.round(before.x + before.width),
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('it is pulled back inside a dock that shrinks under it', async () => {
    const { wrapper, size } = mountDock()
    try {
      await settle()
      const dockBefore = rect(dock())
      expect(rect(frame('output'))?.right).toBeLessThanOrEqual(dockBefore.right + 1)

      size.value = { w: 420, h: 320 }
      await settle()

      const dockAfter = rect(dock())
      const win = rect(frame('output'))
      // Still wholly inside, rather than hanging off the edge.
      expect(win.right).toBeLessThanOrEqual(dockAfter.right + 1)
      expect(win.bottom).toBeLessThanOrEqual(dockAfter.bottom + 1)
      expect(win.left).toBeGreaterThanOrEqual(dockAfter.left - 1)
    } finally {
      wrapper.unmount()
    }
  })

  test('two windows with no remembered position do not land on each other', async () => {
    const { wrapper } = mountDock()
    try {
      await settle()
      const output = rect(frame('output'))
      const trace = rect(frame('trace'))
      // Both default to the bottom-right; the cascade steps the second one back.
      expect(Math.round(output.x)).not.toBe(Math.round(trace.x))
      expect(Math.round(output.y)).not.toBe(Math.round(trace.y))
    } finally {
      wrapper.unmount()
    }
  })

  /*
   * The trap this arrangement is most likely to fall into, and one that is
   * invisible in jsdom: an absolutely positioned grid child that carries a
   * `grid-area` takes that area as its containing block rather than the grid's
   * padding box. A floating frame must therefore carry no grid-area, or every
   * clamp() above would be measuring against the wrong box.
   */
  test('a floating frame is positioned against the dock, not a grid area', async () => {
    const { wrapper } = mountDock()
    try {
      panels.dock('output')
      await settle()
      // With slot B occupied the grid has real areas to get this wrong with.
      panels.float('output')
      await settle()

      const win = frame('output')
      expect(win?.offsetParent).toBe(dock())

      expect(panels.layout.value.placement.output.kind).toBe('floating')
      const g = panels.layout.value.geometry.output
      if (g === null) throw new Error('no geometry')

      const d = rect(dock())
      const w = rect(win)
      // The stored x/y are offsets from the dock's own box.
      expect(Math.round(w.x - d.x)).toBe(Math.round(g.x))
      expect(Math.round(w.y - d.y)).toBe(Math.round(g.y))
    } finally {
      wrapper.unmount()
    }
  })
})

describe('CodeMirror', () => {
  test('still has a height after being tabbed away from and back', async () => {
    const { wrapper } = mountDock(true)
    try {
      await settle()
      const content = () => host.querySelector<HTMLElement>('.cm-content')
      const before = rect(content())?.height ?? 0
      expect(before).toBeGreaterThan(0)

      // Share a slot with the output, then show the other tab, then come back.
      panels.dock('output')
      await settle()
      panels.moveToOtherSlot('output')
      await settle()
      expect(rect(frame('editor'))?.height ?? 1).toBe(0)

      panels.reveal('editor')
      await settle()

      const after = rect(content())?.height ?? 0
      expect(after).toBeGreaterThan(0)
      // And the document survived the round trip rather than being remounted.
      expect(content()?.textContent).toContain('sidebar')
    } finally {
      wrapper.unmount()
    }
  })
})
