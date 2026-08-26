import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { EditorView } from '@codemirror/view'
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

// The REPL is a panel like the trace: opened on demand, floating over the code,
// and closeable (#399).
describe('IDE REPL window', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** Mounts the IDE over a file with one definition in it, and opens it. */
  async function mountIde(src = '(define sq (lambda (n) (* n n)))') {
    await fs.saveFile('a.scm', src)
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: 'Open a.scm' }).click()
    await flushPromises()
    return wrapper
  }

  async function openRepl() {
    getByRole(document.body, 'button', { name: 'Open a REPL' }).click()
    await flushPromises()
    await flushPromises()
  }

  function replWindow(): HTMLElement | null {
    return document.querySelector<HTMLElement>('[data-panel="repl"]')
  }

  function taskbar(): string[] {
    return [...document.querySelectorAll('.taskbar-button')].map((b) =>
      b.textContent.trim(),
    )
  }

  test('opening one puts up the window, seeded from the open file', async () => {
    const wrapper = await mountIde()
    try {
      expect(replWindow()).toBeNull()
      await openRepl()
      expect(replWindow()).not.toBeNull()
      // The banner says where the definitions came from, and that the file is
      // not at risk -- the whole contract of the window.
      expect(replWindow()?.textContent).toContain('a.scm')
      expect(replWindow()?.textContent).toContain('Nothing you type changes')
    } finally {
      wrapper.unmount()
    }
  })

  test('what the file prints stays out of the transcript', async () => {
    // Seeding is for the definitions; the output pane is where a run is shown.
    const wrapper = await mountIde('(display "hello")')
    try {
      await openRepl()
      expect(replWindow()?.textContent).not.toContain('hello')
    } finally {
      wrapper.unmount()
    }
  })

  // The whole of how an entry is run. Worth a test of the window rather than
  // of the cell alone: the binding is wired here, and a cell that does not ask
  // for it gets an ordinary newline instead (#410).
  test('Enter at the prompt runs what was typed', async () => {
    const wrapper = await mountIde()
    try {
      await openRepl()
      const prompt = EditorView.findFromDOM(
        replWindow()?.querySelector<HTMLElement>(
          '.repl-prompt .cm-editor',
        ) as HTMLElement,
      )
      expect(prompt).not.toBeNull()
      prompt?.dispatch({ changes: { from: 0, insert: '(sq 7)' } })
      await flushPromises()
      prompt?.contentDOM.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }),
      )
      await flushPromises()
      await flushPromises()
      // The entry, and what it produced, are in the transcript.
      expect(replWindow()?.textContent).toContain('(sq 7)')
      expect(replWindow()?.textContent).toContain('49')
      // And the prompt is empty again, rather than holding a new line.
      expect(prompt?.state.doc.toString()).toBe('')
    } finally {
      wrapper.unmount()
    }
  })

  test('typing in the editor warns that the REPL is out of sync', async () => {
    // The same courtesy the output pane pays: nothing is re-seeded, but the
    // window says what it started from has moved on.
    const wrapper = await mountIde()
    try {
      await openRepl()
      expect(replWindow()?.textContent).not.toContain('has changed')

      const editor = getByRole(document.body, 'textbox', { name: 'Source code' })
      ;(editor as HTMLTextAreaElement).value = '(define sq 5)'
      editor.dispatchEvent(new Event('input', { bubbles: true }))
      await flushPromises()
      expect(replWindow()?.textContent).toContain('has changed since this REPL')
    } finally {
      wrapper.unmount()
    }
  })

  test('closing it takes it away, taskbar included', async () => {
    const wrapper = await mountIde()
    try {
      await openRepl()
      getByRole(document.body, 'button', { name: 'Minimize REPL' }).click()
      await flushPromises()
      expect(taskbar()).toEqual(['REPL'])

      getByRole(document.body, 'button', { name: 'REPL' }).click()
      await flushPromises()
      getByRole(document.body, 'button', { name: 'Close REPL' }).click()
      await flushPromises()
      expect(replWindow()).toBeNull()
      expect(taskbar()).toEqual([])
    } finally {
      wrapper.unmount()
    }
  })

  test('asking for a REPL when one is open brings it forward', async () => {
    // Regression: it used to seed a fresh session, throwing away a transcript
    // nobody asked to lose. Restart is how that is done deliberately.
    const wrapper = await mountIde()
    try {
      await openRepl()
      getByRole(document.body, 'button', { name: 'Minimize REPL' }).click()
      await flushPromises()

      await openRepl()
      expect(replWindow()?.style.display).not.toBe('none')
      expect(replWindow()?.textContent).toContain('a.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('it can be closed and opened again', async () => {
    // The prompt is a document the language server holds, and the workspace
    // refuses two views on one URI -- so reopening has to have closed the
    // first one properly.
    const wrapper = await mountIde()
    try {
      await openRepl()
      getByRole(document.body, 'button', { name: 'Close REPL' }).click()
      await flushPromises()
      expect(replWindow()).toBeNull()

      await openRepl()
      expect(replWindow()).not.toBeNull()
      expect(replWindow()?.textContent).toContain('a.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('it opens as a tab where a window has nowhere to float', async () => {
    vi.spyOn(Element.prototype, 'clientWidth', 'get').mockReturnValue(500)
    const wrapper = await mountIde()
    try {
      const tabs = () =>
        [...document.querySelectorAll('[role="tab"]')].map((t) =>
          t.textContent.trim(),
        )
      expect(tabs()).toEqual(['Source', 'Output'])
      await openRepl()
      expect(tabs()).toEqual(['Source', 'Output', 'REPL'])
      expect(getByRole(document.body, 'tab', { name: 'REPL' })).toHaveAttribute(
        'aria-selected',
        'true',
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('the Run menu opens one too', async () => {
    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'menuitem', { name: 'Run' }).click()
      await flushPromises()
      getByRole(getByRole(document.body, 'menu'), 'menuitem', {
        name: 'Open REPL…',
      }).click()
      await flushPromises()
      await flushPromises()
      expect(replWindow()).not.toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})
