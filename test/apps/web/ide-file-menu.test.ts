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

// A file's operations live behind the ⋯ on its row in the drawer, and only the
// open file carries one (the way Overleaf does it). So the menu always acts on
// the open file -- but it says which file that is rather than leaving it
// implicit, which is what these tests pin down.
describe('IDE file menu', () => {
  let fs: MockFileSystem
  /** The `download` name and href of every anchor that was clicked. */
  let downloads: { name: string; url: string }[]

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    downloads = []
    vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(
      function (this: HTMLAnchorElement) {
        downloads.push({ name: this.download, url: this.href })
      },
    )
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** The editor's current contents, as the student sees them. */
  function editorText(): string {
    return getByRole<HTMLTextAreaElement>(document.body, 'textbox', {
      name: 'Source code',
    }).value
  }

  /** Mounts the IDE over two files and opens `open`, leaving `other.scm` shut. */
  async function mountIdeWith(open: string) {
    await fs.saveFile('open.scm', '(display "open")')
    await fs.saveFile('other.scm', '(display "other")')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: `Open ${open}` }).click()
    await flushPromises()
    return wrapper
  }

  /** Picks `action` from `filename`'s ⋯ menu in the file drawer. */
  async function fileMenu(filename: string, action: string | RegExp) {
    getByRole(document.body, 'button', {
      name: `Actions for ${filename}`,
    }).click()
    await flushPromises()
    getByRole(document.body, 'menuitem', { name: action }).click()
    await flushPromises()
  }

  // The ⋯ is hidden on every other row by a scoped stylesheet, which jsdom
  // does not apply -- so this checks the structure the rule selects on (the
  // button is inside the row, and the open row is the one marked `selected`)
  // rather than the resulting visibility, which only a browser can show.
  test('the ⋯ belongs to the open file\'s row', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      const selected = document.querySelectorAll('.file.selected')
      expect(selected.length).toBe(1)
      expect(
        selected[0].querySelector('.file-menu-button')?.getAttribute('aria-label'),
      ).toBe('Actions for open.scm')

      // It follows the selection rather than staying put.
      getByRole(document.body, 'button', { name: 'Open other.scm' }).click()
      await flushPromises()
      expect(
        document
          .querySelector('.file.selected .file-menu-button')
          ?.getAttribute('aria-label'),
      ).toBe('Actions for other.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('the ⋯ opens and closes its menu without opening the file', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      const dots = getByRole(document.body, 'button', {
        name: 'Actions for open.scm',
      })

      dots.click()
      await flushPromises()
      expect(getByRole(document.body, 'menu')).toBeInTheDocument()

      // Clicking it again puts the menu away rather than reopening it.
      dots.click()
      await flushPromises()
      expect(queryByRole(document.body, 'menu')).toBeNull()

      // And none of that disturbed what is in the editor.
      expect(editorText()).toBe('(display "open")')
    } finally {
      wrapper.unmount()
    }
  })

  test('renames from the menu, and the file stays open under its new name', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      await fileMenu('open.scm', /^Rename/)
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'Rename file',
      })
      expect(prompt.textContent).toContain('open.scm')
      fireEvent.input(getByRole(prompt, 'textbox'), {
        target: { value: 'renamed.scm' },
      })
      getByRole(prompt, 'button', { name: 'OK' }).click()
      await flushPromises()

      expect(await fs.fileExists('renamed.scm')).toBe(true)
      expect(await fs.fileExists('open.scm')).toBe(false)
      // Still open, still holding what it held, and now the row with the ⋯.
      expect(editorText()).toBe('(display "open")')
      expect(
        document
          .querySelector('.file.selected .file-menu-button')
          ?.getAttribute('aria-label'),
      ).toBe('Actions for renamed.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('deletes from the menu, leaving the other file alone', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      await fileMenu('open.scm', 'Delete')
      const confirm = await findByRole(document.body, 'dialog', {
        name: 'Delete file',
      })
      expect(confirm.textContent).toContain('open.scm')
      getByRole(confirm, 'button', { name: 'Delete' }).click()
      await flushPromises()

      expect(await fs.fileExists('open.scm')).toBe(false)
      expect(await fs.fileExists('other.scm')).toBe(true)
      // Nothing is open afterwards, so no row carries a ⋯.
      expect(document.querySelector('.file.selected')).toBeNull()
      expect(
        getByRole(document.body, 'button', { name: 'Open other.scm' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('downloads from the menu, naming the file it came from', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      await fileMenu('open.scm', 'Download')

      expect(downloads).toHaveLength(1)
      expect(downloads[0].name).toBe('open.scm')
      expect(decodeURIComponent(downloads[0].url.split(',')[1])).toBe(
        '(display "open")',
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('duplicates from the menu, leaving the original and the editor alone', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      await fileMenu('open.scm', /^Duplicate/)
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'Duplicate file',
      })
      // Opens on a name that is free, rather than on the one already taken.
      expect(getByRole<HTMLInputElement>(prompt, 'textbox').value).toBe(
        'open-copy.scm',
      )
      getByRole(prompt, 'button', { name: 'OK' }).click()
      await flushPromises()

      expect(await fs.loadFile('open-copy.scm')).toBe('(display "open")')
      expect(await fs.fileExists('open.scm')).toBe(true)
      // A duplicate is not a switch: the original stays open.
      expect(editorText()).toBe('(display "open")')
      expect(
        document
          .querySelector('.file.selected .file-menu-button')
          ?.getAttribute('aria-label'),
      ).toBe('Actions for open.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('refuses to duplicate onto a name already in use', async () => {
    const wrapper = await mountIdeWith('open.scm')
    try {
      await fileMenu('open.scm', /^Duplicate/)
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'Duplicate file',
      })
      fireEvent.input(getByRole(prompt, 'textbox'), {
        target: { value: 'other.scm' },
      })
      getByRole(prompt, 'button', { name: 'OK' }).click()
      await flushPromises()

      expect(document.body.textContent).toContain('already exists')
      // The file it would have clobbered is untouched.
      expect(await fs.loadFile('other.scm')).toBe('(display "other")')
    } finally {
      wrapper.unmount()
    }
  })

  test('opens history on the file the menu came from', async () => {
    const wrapper = await mountIdeWith('other.scm')
    try {
      await fileMenu('other.scm', /^History/)
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'File history',
      })
      // Named explicitly rather than inferred, so the picker lands on it even
      // though nothing has been recorded for it yet.
      expect(getByRole<HTMLSelectElement>(dialog, 'combobox').value).toBe(
        'other.scm',
      )
    } finally {
      wrapper.unmount()
    }
  })
})
