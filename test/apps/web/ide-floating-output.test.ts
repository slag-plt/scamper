import { flushPromises, mount } from '@vue/test-utils'
import {
  fireEvent,
  findByRole,
  getByRole,
  queryByRole,
} from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { initialize } from '../../../src/scamper'
import { VERSION } from '../../../src/app/web/composables/use-panels'
import { DEFAULT_IDLE_MS } from '../../../src/app/web/composables/use-live-evaluation'

vi.mock('../../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../../stubs/MockCodeMirrorEditor.vue'),
)

vi.mock(
  '../../../src/app/web/components/ResultsPane.vue',
  () => import('../../stubs/MockResultsPane.vue'),
)

await initialize()

const LAYOUT_KEY = 'scamper.panels'
const LEGACY_GEOMETRY_KEY = 'scamper.window.output'

// How the output behaves once it *is* a floating window. Since #371 it docks
// by default, so these seed a floating arrangement first; the docked default is
// covered in test/regressions/default-output-placement.
//
// jsdom has no layout, so dragging and resizing cannot be meaningfully checked
// here -- every box measures zero. What these cover is the part that is logic
// rather than pixels: where the window is, and how it comes and goes.
describe('IDE floating output window', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    try {
      localStorage.removeItem(LAYOUT_KEY)
      localStorage.removeItem(LEGACY_GEOMETRY_KEY)
    } catch {
      /* no storage here; nothing to clear */
    }
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  async function mountIde() {
    await fs.saveFile('hello.scm', '(display "hi")')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  /** A panel's frame, whether or not it is currently shown. */
  function panel(id: 'editor' | 'output' | 'trace'): HTMLElement | null {
    return document.querySelector(`[data-panel="${id}"]`)
  }

  const outputWindow = () => panel('output')

  /**
   * Stores an arrangement with the output floating, then mounts. The output
   * docks by default since #371, but everything below is about how the window
   * behaves once it *is* a window.
   */
  async function mountWithFloatingOutput() {
    localStorage.setItem(
      LAYOUT_KEY,
      JSON.stringify({
        version: VERSION,
        placement: {
          editor: { kind: 'docked', slot: 'a' },
          output: { kind: 'floating', minimized: false },
          trace: { kind: 'floating', minimized: false },
        },
        geometry: { editor: null, output: null, trace: null },
        recency: ['trace', 'output', 'editor'],
        axis: 'row',
        splitPercent: 62,
      }),
    )
    return mountIde()
  }

  test('floated, the output sits over the code instead of beside it', async () => {
    // The docked default is covered in test/regressions/default-output-placement.
    const wrapper = await mountWithFloatingOutput()
    try {
      expect(panel('editor')?.dataset.placement).toBe('docked')
      expect(outputWindow()?.dataset.placement).toBe('floating')
      // Alone in the dock, the editor gets no tab strip -- no chrome over the
      // code, exactly as before the dock existed.
      expect(document.querySelector('[role="tablist"]')).toBeNull()
      expect(
        getByRole(document.body, 'region', { name: 'Output' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('minimizing tucks it into the taskbar, and the taskbar brings it back', async () => {
    const wrapper = await mountWithFloatingOutput()
    try {
      expect(document.querySelector('.window-taskbar')).toBeNull()

      getByRole(document.body, 'button', { name: 'Minimize Output' }).click()
      await flushPromises()

      // Hidden rather than destroyed, so the output it holds is still there.
      expect(outputWindow()).not.toBeNull()
      expect(outputWindow()?.style.display).toBe('none')
      const restore = getByRole(document.body, 'button', { name: 'Output' })
      expect(restore.closest('.window-taskbar')).not.toBeNull()

      restore.click()
      await flushPromises()
      expect(outputWindow()?.style.display).not.toBe('none')
      expect(document.querySelector('.window-taskbar')).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('running brings a minimized window back', async () => {
    // Otherwise pressing Run with the output put away produces nothing the
    // person can see, which reads as Scamper being broken.
    const wrapper = await mountWithFloatingOutput()
    try {
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()
      getByRole(document.body, 'button', { name: 'Minimize Output' }).click()
      await flushPromises()
      expect(outputWindow()?.style.display).toBe('none')

      getByRole(document.body, 'button', { name: 'Autorun' }).click()
      await flushPromises()

      expect(outputWindow()?.style.display).not.toBe('none')
      expect(
        queryByRole(document.body, 'button', { name: 'Minimize Output' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  // Below the breakpoint the window docks and the two panes take turns behind a
  // pair of tabs. jsdom reports every element as zero-wide, so these stub the
  // measurement the layout switches on.
  describe('on a pane too narrow to float a window in', () => {
    beforeEach(() => {
      vi.spyOn(Element.prototype, 'clientWidth', 'get').mockReturnValue(500)
    })

    test('shows tabs instead of a floating window, starting on the code', async () => {
      const wrapper = await mountIde()
      try {
        const tabs = getByRole(document.body, 'tablist')
        expect(
          getByRole(tabs, 'tab', { name: 'Source' }),
        ).toHaveAttribute('aria-selected', 'true')
        expect(
          getByRole(tabs, 'tab', { name: 'Output' }),
        ).toHaveAttribute('aria-selected', 'false')

        // Docked: no title bar to drag it by, and no taskbar, since the tabs
        // are the way back to it.
        expect(outputWindow()?.classList.contains('docked')).toBe(true)
        expect(document.querySelector('.window-bar')).toBeNull()
        expect(document.querySelector('.window-taskbar')).toBeNull()
      } finally {
        wrapper.unmount()
      }
    })

    test('the tabs swap which pane is showing', async () => {
      const wrapper = await mountIde()
      try {
        const editor = panel('editor')
        expect(editor?.style.display).not.toBe('none')
        expect(outputWindow()?.style.display).toBe('none')

        getByRole(document.body, 'tab', { name: 'Output' }).click()
        await flushPromises()
        expect(editor?.style.display).toBe('none')
        expect(outputWindow()?.style.display).not.toBe('none')

        getByRole(document.body, 'tab', { name: 'Source' }).click()
        await flushPromises()
        expect(editor?.style.display).not.toBe('none')
        expect(outputWindow()?.style.display).toBe('none')
      } finally {
        wrapper.unmount()
      }
    })

    test('the arrow keys move between the tabs', async () => {
      const wrapper = await mountIde()
      try {
        fireEvent.keyDown(getByRole(document.body, 'tablist'), {
          key: 'ArrowRight',
        })
        await flushPromises()
        expect(
          getByRole(document.body, 'tab', { name: 'Output' }),
        ).toHaveAttribute('aria-selected', 'true')
      } finally {
        wrapper.unmount()
      }
    })

    test('a run started by typing leaves the person on the code', async () => {
      const wrapper = await mountIde()
      try {
        getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
        await flushPromises()

        vi.useFakeTimers()
        const area = getByRole(document.body, 'textbox', {
          name: 'Source code',
        }) as HTMLTextAreaElement
        area.value = '(+ 1 2)'
        area.dispatchEvent(new Event('input'))
        await flushPromises()
        await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
        await flushPromises()

        // The output pane fills behind the tab; being taken to it mid-keystroke
        // would put the person somewhere they did not ask to be.
        expect(
          getByRole(document.body, 'tab', { name: 'Source' }),
        ).toHaveAttribute('aria-selected', 'true')
      } finally {
        vi.useRealTimers()
        wrapper.unmount()
      }
    })

    test('running switches to the output tab', async () => {
      const wrapper = await mountIde()
      try {
        getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
        await flushPromises()
        expect(
          getByRole(document.body, 'tab', { name: 'Source' }),
        ).toHaveAttribute('aria-selected', 'true')

        getByRole(document.body, 'button', { name: 'Autorun' }).click()
        await flushPromises()

        expect(
          getByRole(document.body, 'tab', { name: 'Output' }),
        ).toHaveAttribute('aria-selected', 'true')
      } finally {
        wrapper.unmount()
      }
    })
  })

  test('a remembered geometry is read back on the next mount', async () => {
    localStorage.setItem(
      LAYOUT_KEY,
      JSON.stringify({
        version: VERSION,
        placement: {
          editor: { kind: 'docked', slot: 'a' },
          output: {
            kind: 'floating',
            geometry: { x: 12, y: 34, w: 400, h: 300 },
            minimized: false,
          },
          trace: { kind: 'floating', geometry: null, minimized: false },
        },
        recency: ['trace', 'output', 'editor'],
        axis: 'row',
        splitPercent: 62,
      }),
    )
    const wrapper = await mountIde()
    try {
      // jsdom reports a zero-sized parent, so the clamp pulls everything to
      // the origin and the minimum size. That the window is positioned at all
      // is what says the stored value was read rather than ignored.
      const style = outputWindow()?.style
      expect(style?.left).toBe('0px')
      expect(style?.width).toBe('240px')
    } finally {
      wrapper.unmount()
    }
  })

  test('nonsense in storage falls back to the default placement', async () => {
    localStorage.setItem(LAYOUT_KEY, '{ not json')
    const wrapper = await mountIde()
    try {
      expect(outputWindow()).not.toBeNull()
      // The default docks the output (#371), so it is a pane with no box of
      // its own rather than a window at the default corner.
      expect(outputWindow()?.dataset.placement).toBe('docked')
    } finally {
      wrapper.unmount()
    }
  })
})
