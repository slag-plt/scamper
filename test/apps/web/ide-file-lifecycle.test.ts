import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { initialize } from '../../../src/scamper'

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

// Save As, Close File, and the recent-files list: the three File-menu entries
// that change which file is open rather than what is in it.
describe('IDE file lifecycle', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    // The recent list persists in localStorage, so clear it between tests or
    // each one inherits the last one's history.
    try {
      localStorage.removeItem('scamper.config')
    } catch {
      /* no storage in this environment; nothing to clear */
    }
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  function editorText(): string {
    return getByRole<HTMLTextAreaElement>(document.body, 'textbox', {
      name: 'Source code',
    }).value
  }

  async function mountIde(open?: string) {
    await fs.saveFile('one.scm', '(display 1)')
    await fs.saveFile('two.scm', '(display 2)')
    await fs.saveFile('three.scm', '(display 3)')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    if (open !== undefined) await openFile(open)
    return wrapper
  }

  async function openFile(name: string) {
    getByRole(document.body, 'button', { name: `Open ${name}` }).click()
    await flushPromises()
  }

  /** Picks `label` out of the `title` menu. */
  async function pick(title: string, label: string | RegExp) {
    getByRole(document.body, 'menuitem', { name: title }).click()
    await flushPromises()
    const menu = getByRole(document.body, 'menu')
    getByRole(menu, 'menuitem', { name: label }).click()
    await flushPromises()
  }

  test('Save As writes a copy and moves the editor onto it', async () => {
    const wrapper = await mountIde('one.scm')
    try {
      // Type something first, so the copy has to carry the editor's contents
      // rather than what happens to be on disk.
      fireEvent.input(
        getByRole(document.body, 'textbox', { name: 'Source code' }),
        { target: { value: '(display "edited")' } },
      )
      await flushPromises()

      await pick('File', /^Save As/)
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'Save as',
      })
      fireEvent.input(getByRole(prompt, 'textbox'), {
        target: { value: 'copy.scm' },
      })
      getByRole(prompt, 'button', { name: 'OK' }).click()
      await flushPromises()

      expect(await fs.loadFile('copy.scm')).toBe('(display "edited")')
      // The editor is now on the copy, which is what makes it Save As rather
      // than Duplicate.
      expect(editorText()).toBe('(display "edited")')
      expect(
        document.querySelector('.file.selected .file-name')?.textContent.trim(),
      ).toBe('copy.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('Close File empties the editor without deleting anything', async () => {
    const wrapper = await mountIde('one.scm')
    try {
      expect(editorText()).toBe('(display 1)')

      await pick('File', 'Close File')

      expect(await fs.fileExists('one.scm')).toBe(true)
      expect(editorText()).not.toBe('(display 1)')
      // Nothing is open, so no row is selected and the file actions grey out.
      expect(document.querySelector('.file.selected')).toBeNull()
      getByRole(document.body, 'menuitem', { name: 'File' }).click()
      await flushPromises()
      expect(
        getByRole(getByRole(document.body, 'menu'), 'menuitem', {
          name: 'Close File',
        }),
      ).toHaveAttribute('aria-disabled', 'true')
    } finally {
      wrapper.unmount()
    }
  })

  test('Close File saves what was typed before letting go of it', async () => {
    const wrapper = await mountIde('one.scm')
    try {
      fireEvent.input(
        getByRole(document.body, 'textbox', { name: 'Source code' }),
        { target: { value: '(display "unsaved")' } },
      )
      await flushPromises()

      await pick('File', 'Close File')

      expect(await fs.loadFile('one.scm')).toBe('(display "unsaved")')
    } finally {
      wrapper.unmount()
    }
  })

  test('the File menu lists recently opened files, newest first', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('one.scm')
      await openFile('two.scm')
      await openFile('three.scm')

      getByRole(document.body, 'menuitem', { name: 'File' }).click()
      await flushPromises()
      const menu = getByRole(document.body, 'menu')

      // The open file is not offered as somewhere to go back to.
      expect(queryByRole(menu, 'menuitem', { name: 'three.scm' })).toBeNull()
      const recent = [...menu.querySelectorAll('[role="menuitem"]')]
        .map((li) => li.textContent.trim())
        .filter((label) => label === 'one.scm' || label === 'two.scm')
      expect(recent).toEqual(['two.scm', 'one.scm'])
    } finally {
      wrapper.unmount()
    }
  })

  test('a recent entry opens that file', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('one.scm')
      await openFile('two.scm')

      await pick('File', 'one.scm')
      expect(editorText()).toBe('(display 1)')
    } finally {
      wrapper.unmount()
    }
  })

  test('a file that no longer exists drops off the recent list', async () => {
    const wrapper = await mountIde()
    try {
      await openFile('one.scm')
      await openFile('two.scm')

      // Delete the file that is sitting in the recent list.
      getByRole(document.body, 'button', {
        name: 'Actions for one.scm',
      }).click()
      await flushPromises()
      getByRole(document.body, 'menuitem', { name: 'Delete' }).click()
      await flushPromises()
      const confirm = await findByRole(document.body, 'dialog', {
        name: 'Delete file',
      })
      getByRole(confirm, 'button', { name: 'Delete' }).click()
      await flushPromises()

      getByRole(document.body, 'menuitem', { name: 'File' }).click()
      await flushPromises()
      expect(
        queryByRole(getByRole(document.body, 'menu'), 'menuitem', {
          name: 'one.scm',
        }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})
