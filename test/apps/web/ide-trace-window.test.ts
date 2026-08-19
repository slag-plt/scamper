import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { mockEditorHandle } from '../../stubs/mock-editor-handle'
import { initialize } from '../../../src/scamper'
import '../../../src/app/web/renderers'

vi.mock('../../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../../stubs/MockCodeMirrorEditor.vue'),
)

await initialize()

// The trace window is a second floating window, and everything the output
// window gets it has to get too: somewhere to go when minimized, and a tab of
// its own when the pane is too narrow to float anything.
describe('IDE trace window', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    // Put the cursor inside a statement, so stepping is available at all.
    mockEditorHandle.cursorPath = ['display']
  })

  afterEach(() => {
    mockEditorHandle.cursorPath = []
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** Mounts the IDE over a one-statement file and opens it. */
  async function mountIde() {
    await fs.saveFile('a.scm', '(display (+ 1 2))')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: 'Open a.scm' }).click()
    await flushPromises()
    return wrapper
  }

  /** Presses Step and waits for the trace to be collected. */
  async function step() {
    getByRole(document.body, 'button', { name: 'Step statement' }).click()
    await flushPromises()
    await flushPromises()
  }

  function traceWindow(): HTMLElement | null {
    return document.querySelector<HTMLElement>(
      '[aria-label="Step"].floating-window',
    )
  }

  /** The labels currently sitting in the taskbar. */
  function taskbar(): string[] {
    return [...document.querySelectorAll('.taskbar-button')].map((b) =>
      b.textContent.trim(),
    )
  }

  test('stepping opens the trace window', async () => {
    const wrapper = await mountIde()
    try {
      expect(traceWindow()).toBeNull()
      await step()
      expect(traceWindow()).not.toBeNull()
      expect(traceWindow()?.style.display).not.toBe('none')
    } finally {
      wrapper.unmount()
    }
  })

  test('minimizing it puts it in the taskbar, where it can be got back', async () => {
    // Regression: it used to minimize into nowhere, with no way to recover it.
    const wrapper = await mountIde()
    try {
      await step()
      expect(taskbar()).toEqual([])

      getByRole(document.body, 'button', { name: 'Minimize Step' }).click()
      await flushPromises()
      expect(traceWindow()?.style.display).toBe('none')
      expect(taskbar()).toEqual(['Step'])

      getByRole(document.body, 'button', { name: 'Step' }).click()
      await flushPromises()
      expect(traceWindow()?.style.display).not.toBe('none')
      expect(taskbar()).toEqual([])
    } finally {
      wrapper.unmount()
    }
  })

  test('both windows can wait in the taskbar at once', async () => {
    const wrapper = await mountIde()
    try {
      await step()
      getByRole(document.body, 'button', { name: 'Minimize Output' }).click()
      getByRole(document.body, 'button', { name: 'Minimize Step' }).click()
      await flushPromises()
      expect(taskbar()).toEqual(['Output', 'Step'])
    } finally {
      wrapper.unmount()
    }
  })

  test('closing it takes it out of the taskbar rather than stranding it', async () => {
    const wrapper = await mountIde()
    try {
      await step()
      getByRole(document.body, 'button', { name: 'Minimize Step' }).click()
      await flushPromises()
      expect(taskbar()).toEqual(['Step'])

      // Reopening and closing clears it; a closed window has nothing to restore.
      getByRole(document.body, 'button', { name: 'Step' }).click()
      await flushPromises()
      getByRole(document.body, 'button', { name: 'Close Step' }).click()
      await flushPromises()
      expect(traceWindow()).toBeNull()
      expect(taskbar()).toEqual([])
    } finally {
      wrapper.unmount()
    }
  })

  describe('on a pane too narrow to float windows in', () => {
    beforeEach(() => {
      vi.spyOn(Element.prototype, 'clientWidth', 'get').mockReturnValue(500)
    })

    test('it becomes a tab, like the output does', async () => {
      // Regression: the tabs were Source and Output only, so a trace opened on
      // a narrow pane had no way to be shown.
      const wrapper = await mountIde()
      try {
        const tabs = () =>
          [...document.querySelectorAll('[role="tab"]')].map((t) =>
            t.textContent.trim(),
          )
        expect(tabs()).toEqual(['Source', 'Output'])

        await step()
        expect(tabs()).toEqual(['Source', 'Output', 'Step'])
        // Stepping brings its own pane up, and docks it rather than floating.
        expect(
          getByRole(document.body, 'tab', { name: 'Step' }),
        ).toHaveAttribute('aria-selected', 'true')
        expect(traceWindow()?.classList.contains('docked')).toBe(true)
      } finally {
        wrapper.unmount()
      }
    })

    test('the tabs take turns rather than stacking', async () => {
      const wrapper = await mountIde()
      try {
        await step()
        const editor = document.querySelector<HTMLElement>('.editor-layer')
        expect(editor?.style.display).toBe('none')

        getByRole(document.body, 'tab', { name: 'Source' }).click()
        await flushPromises()
        expect(editor?.style.display).not.toBe('none')
        expect(traceWindow()?.style.display).toBe('none')

        getByRole(document.body, 'tab', { name: 'Output' }).click()
        await flushPromises()
        expect(traceWindow()?.style.display).toBe('none')
        expect(editor?.style.display).toBe('none')
      } finally {
        wrapper.unmount()
      }
    })

    test('closing the trace drops its tab', async () => {
      const wrapper = await mountIde()
      try {
        await step()
        getByRole(document.body, 'button', { name: 'Close Step' }).click()
        await flushPromises()
        expect(
          queryByRole(document.body, 'tab', { name: 'Step' }),
        ).toBeNull()
      } finally {
        wrapper.unmount()
      }
    })

    test('no taskbar when docked; the tabs are the way back', async () => {
      const wrapper = await mountIde()
      try {
        await step()
        expect(document.querySelector('.window-taskbar')).toBeNull()
      } finally {
        wrapper.unmount()
      }
    })
  })
})
