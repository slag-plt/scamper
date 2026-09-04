/* eslint-disable vue/one-component-per-file -- the hosts below are scaffolding
   for these specs, not components the app ships; each exists to give a
   PanelFrame something to sit in. */
import { mount } from '@vue/test-utils'
import { getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test } from 'vitest'
import { defineComponent, h, nextTick, ref } from 'vue'
import PanelDock from '../../../src/app/web/components/PanelDock.vue'
import PanelFrame from '../../../src/app/web/components/PanelFrame.vue'
import { providePanels, type Panels } from '../../../src/app/web/composables/use-panels'
import type { PanelId } from '../../../src/app/web/panel-layout'

// The window chrome -- title bar, minimize, close, and whether there is any at
// all -- belongs to PanelFrame rather than to whatever it contains. TraceWindow
// used to own its close button; this is where that moved to.
//
// jsdom lays nothing out, so geometry is not testable here. What is testable is
// which chrome appears, which panel is drawn, and that the contents survive
// being moved between placements -- which is the whole reason a frame changes
// classes instead of changing place in the tree.

const LABELS: Record<PanelId, string> = {
  editor: 'Source',
  output: 'Output',
  trace: 'Step',
  repl: 'REPL',
}

/** Counts mounts, so a remount shows up as a number rather than a guess. */
let mountCount = 0

const Contents = defineComponent({
  props: { text: { type: String, required: true } },
  setup(props) {
    mountCount += 1
    return () => h('p', { class: 'contents' }, props.text)
  },
})

let panels: Panels

function mountDock(isCompactInitially = false) {
  const isCompact = ref(isCompactInitially)
  const present = ref<readonly PanelId[]>(['editor', 'output', 'trace'])
  const Host = defineComponent({
    setup() {
      panels = providePanels({ isCompact, present })
      return () => [
        h(PanelDock, { labels: LABELS, closeable: ['trace'] }, () => [
          h(PanelFrame, { id: 'editor', title: 'Source' }, () =>
            h(Contents, { text: 'the code' }),
          ),
          h(PanelFrame, { id: 'output', title: 'Output' }, () =>
            h(Contents, { text: 'the output' }),
          ),
          h(PanelFrame, { id: 'trace', title: 'Step', closeable: true }, () =>
            h(Contents, { text: 'the trace' }),
          ),
        ]),
        // The taskbar is IdeApp's in the real thing; the frames only know it
        // by the data attribute focusPanel looks for.
        h(
          'div',
          { class: 'window-taskbar' },
          panels.minimized.value.map((id) =>
            h(
              'button',
              { key: id, type: 'button', 'data-panel-taskbar': id },
              LABELS[id],
            ),
          ),
        ),
      ]
    },
  })
  return { wrapper: mount(Host, { attachTo: document.body }), isCompact }
}

function frame(id: PanelId): HTMLElement | null {
  return document.querySelector(`[data-panel="${id}"]`)
}

/**
 * Mounts with the output floating over a dock that holds only the editor --
 * the default arrangement until #371 docked the output. The cases about window
 * chrome, minimizing, and stacking all need a floating panel to act on.
 */
async function mountFloatingOutput() {
  const mounted = mountDock()
  panels.float('output')
  await nextTick()
  return mounted
}

beforeEach(() => {
  localStorage.clear()
  mountCount = 0
})

afterEach(() => {
  document.body.innerHTML = ''
})

describe('chrome', () => {
  test('a floating panel gets a title bar, a docked one does not', async () => {
    const { wrapper } = await mountFloatingOutput()
    try {
      expect(frame('output')?.dataset.placement).toBe('floating')
      expect(frame('editor')?.dataset.placement).toBe('docked')

      expect(
        queryByRole(document.body, 'button', { name: 'Minimize Output' }),
      ).toBeInTheDocument()
      // The editor is docked and alone, so it has no bar and needs no label.
      expect(
        queryByRole(document.body, 'button', { name: 'Minimize Source' }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('only a closeable panel gets a close button', () => {
    const { wrapper } = mountDock()
    try {
      expect(
        queryByRole(document.body, 'button', { name: 'Close Step' }),
      ).toBeInTheDocument()
      expect(
        queryByRole(document.body, 'button', { name: 'Close Output' }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('closing emits, rather than the frame deciding it is gone', async () => {
    // Whether a panel still exists is the host's business: the trace goes away
    // because IdeApp drops it, not because its frame hid itself.
    let closed = 0
    const Host = defineComponent({
      setup() {
        providePanels({
          isCompact: ref(false),
          present: ref<readonly PanelId[]>(['editor', 'output', 'trace']),
        })
        return () =>
          h(PanelDock, { labels: LABELS }, () =>
            h(
              PanelFrame,
              {
                id: 'trace',
                title: 'Step',
                closeable: true,
                onClose: () => { closed += 1 },
              },
              () => h('p', 'x'),
            ),
          )
      },
    })
    const wrapper = mount(Host, { attachTo: document.body })
    try {
      getByRole(document.body, 'button', { name: 'Close Step' }).click()
      await nextTick()
      expect(closed).toBe(1)
    } finally {
      wrapper.unmount()
    }
  })

  test('minimizing hides the frame but keeps it mounted', async () => {
    const { wrapper } = await mountFloatingOutput()
    try {
      const before = mountCount
      getByRole(document.body, 'button', { name: 'Minimize Output' }).click()
      await nextTick()

      expect(frame('output')?.style.display).toBe('none')
      // Hidden, not destroyed -- otherwise a run in flight loses its channel.
      expect(document.body.textContent).toContain('the output')
      expect(mountCount).toBe(before)
    } finally {
      wrapper.unmount()
    }
  })
})

describe('the dock', () => {
  test('one panel alone gets no tab strip', async () => {
    const { wrapper } = await mountFloatingOutput()
    try {
      expect(document.querySelector('[role="tablist"]')).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('panels sharing a slot get one, and only the active one is drawn', async () => {
    const { wrapper } = mountDock()
    try {
      // Float first: the output docks by default (#371), so dock() alone
      // would be a no-op and prove nothing.
      panels.float('output')
      panels.dock('output')
      panels.moveToOtherSlot('output')
      await nextTick()

      const tabs = getByRole(document.body, 'tablist')
      expect(
        [...tabs.querySelectorAll('[role="tab"]')].map((t) => t.textContent.trim()),
      ).toEqual(['Source', 'Output'])

      // moveToOtherSlot fronted the output, so it is the one showing.
      expect(frame('output')?.style.display).not.toBe('none')
      expect(frame('editor')?.style.display).toBe('none')

      getByRole(document.body, 'tab', { name: 'Source' }).click()
      await nextTick()
      expect(frame('editor')?.style.display).not.toBe('none')
      expect(frame('output')?.style.display).toBe('none')
    } finally {
      wrapper.unmount()
    }
  })

  test('a docked panel carries its slot, for the grid to place it by', async () => {
    const { wrapper } = mountDock()
    try {
      // Float first: the output docks by default (#371), so dock() alone
      // would be a no-op and prove nothing.
      panels.float('output')
      panels.dock('output')
      await nextTick()
      expect(frame('editor')?.dataset.slot).toBe('a')
      expect(frame('output')?.dataset.slot).toBe('b')
      // One panel each, so neither draws a strip: a strip exists to choose
      // between tabs, and there is nothing to choose.
      expect(document.querySelectorAll('[role="tablist"]').length).toBe(0)
    } finally {
      wrapper.unmount()
    }
  })
})

describe('stacking', () => {
  test('clicking a floating panel brings it to the front', async () => {
    const { wrapper } = await mountFloatingOutput()
    try {
      // Floating the output fronted it, so put the trace back on top -- the
      // starting order this case is about.
      panels.reveal('trace')
      await nextTick()
      const z = (id: PanelId) => Number(frame(id)?.style.zIndex)
      expect(z('trace')).toBeGreaterThan(z('output'))

      frame('output')?.dispatchEvent(
        new PointerEvent('pointerdown', { bubbles: true }),
      )
      await nextTick()
      expect(z('output')).toBeGreaterThan(z('trace'))
    } finally {
      wrapper.unmount()
    }
  })

  test('a docked panel is not raised by clicking it', async () => {
    const { wrapper } = mountDock()
    try {
      const before = panels.layout.value.recency[0]
      frame('editor')?.dispatchEvent(
        new PointerEvent('pointerdown', { bubbles: true }),
      )
      await nextTick()
      // Nothing stacks in the dock, so there is nothing to raise.
      expect(panels.layout.value.recency[0]).toBe(before)
    } finally {
      wrapper.unmount()
    }
  })
})

/*
 * Every one of these commands destroys the control that invoked it -- the
 * minimize button is hidden with its window, the taskbar button vanishes when
 * the window comes back, a tab's Float button goes with the tab. Without
 * somewhere to send focus it lands on <body>, which strands a keyboard user at
 * the top of the page with no way back to what they just moved.
 */
describe('focus after a panel moves', () => {
  /** Focus something inside the dock, so a fall to <body> is detectable. */
  function parkFocus() {
    getByRole(document.body, 'button', { name: 'Minimize Output' }).focus()
  }

  test('minimizing hands focus to the taskbar button that replaces it', async () => {
    const { wrapper } = await mountFloatingOutput()
    try {
      parkFocus()
      getByRole(document.body, 'button', { name: 'Minimize Output' }).click()
      await nextTick()
      await nextTick()

      expect(document.activeElement).not.toBe(document.body)
      expect(
        (document.activeElement as HTMLElement).dataset.panelTaskbar,
      ).toBe('output')
    } finally {
      wrapper.unmount()
    }
  })

  test('docking hands focus to the panel it becomes', async () => {
    const { wrapper } = await mountFloatingOutput()
    try {
      parkFocus()
      getByRole(document.body, 'button', { name: 'Dock Output' }).click()
      await nextTick()
      await nextTick()

      expect(document.activeElement).not.toBe(document.body)
      // Alone in its slot it has no tab, so focus lands on the frame itself.
      expect(document.activeElement?.id).toBe('panel-output')
    } finally {
      wrapper.unmount()
    }
  })

  test('floating from a tab hands focus to the new title bar', async () => {
    const { wrapper } = mountDock()
    try {
      // Share a slot with the editor: only a slot holding more than one panel
      // draws the strip this case floats from.
      panels.moveToOtherSlot('output')
      await nextTick()
      getByRole(document.body, 'button', { name: 'Float Output' }).click()
      await nextTick()
      await nextTick()

      expect(
        (document.activeElement as HTMLElement).dataset.panelBar,
      ).toBe('output')
    } finally {
      wrapper.unmount()
    }
  })

  test('an automatic reveal does not steal focus', async () => {
    const { wrapper } = mountDock()
    try {
      const parked = getByRole(document.body, 'button', { name: 'Minimize Step' })
      parked.focus()
      // What a run does: reveal, with no focusPanel alongside it. Snatching the
      // caret out of the editor mid-keystroke would be the bug here.
      panels.reveal('output')
      await nextTick()
      await nextTick()

      expect(document.activeElement).toBe(parked)
    } finally {
      wrapper.unmount()
    }
  })
})

// The reason the frame swaps classes rather than moving in the tree. A
// CodeMirror editor holds a document and an undo history, and an output pane is
// bound to a run in flight; neither survives a remount.
describe('contents survive being moved', () => {
  test('floating, docking and tabbing a panel never remounts it', async () => {
    const { wrapper, isCompact } = mountDock()
    try {
      const before = mountCount
      expect(before).toBe(3)

      // Float first: the output docks by default (#371), so dock() alone
      // would be a no-op and prove nothing.
      panels.float('output')
      panels.dock('output')
      await nextTick()
      panels.moveToOtherSlot('output')
      await nextTick()
      panels.float('output')
      await nextTick()
      isCompact.value = true
      await nextTick()
      isCompact.value = false
      await nextTick()

      expect(mountCount).toBe(before)
      expect(document.body.textContent).toContain('the output')
    } finally {
      wrapper.unmount()
    }
  })
})
