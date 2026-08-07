import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { historyFilename, loadHistory } from '../../../src/app/web/file-history'
import { initialize } from '../../../src/scamper'

vi.mock('../../../src/app/web/lockfile', () => ({
  acquireLockFile: vi.fn(() => Promise.resolve(true)),
  releaseLockFile: vi.fn(() => Promise.resolve()),
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

// Browsing and restoring a file's saved history (issue #42), driven through a
// mounted IDE: the sidebar button, the timeline, and what a restore does to
// both the editor and storage.
describe('IDE file history', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setFS(fs)
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** Seeds `filename` with a history of `contents`, oldest last. */
  async function seedHistory(filename: string, contents: string[]) {
    await fs.saveFile(
      historyFilename(filename),
      JSON.stringify({
        version: 1,
        snapshots: contents.map((c, i) => ({
          // Spaced an hour apart so each is its own entry, and dated so the
          // labels are stable regardless of when the suite runs.
          time: new Date(Date.UTC(2026, 7, 7, 9 + i)).toISOString(),
          contents: c,
        })),
      }),
    )
  }

  /** Mounts the IDE and opens `filename` from the drawer. */
  async function mountIdeWith(filename: string) {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: `Open ${filename}` }).click()
    await flushPromises()
    return wrapper
  }

  function editorText(): string {
    return getByRole<HTMLTextAreaElement>(document.body, 'textbox', {
      name: 'Source code',
    }).value
  }

  test('has nothing to show until a file is open', async () => {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()

      expect(
        getByRole(document.body, 'button', { name: 'File history' }),
      ).toBeDisabled()
    } finally {
      wrapper.unmount()
    }
  })

  test('lists the saved versions of the open file', async () => {
    await fs.saveFile('hello.scm', '(display 3)')
    await seedHistory('hello.scm', ['(display 2)', '(display 1)'])

    const wrapper = await mountIdeWith('hello.scm')
    try {
      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'File history',
      })

      const versions = getByRole(dialog, 'listbox', { name: 'Saved versions' })
      // The document as it stands now, then each snapshot newest first.
      expect(
        getByRole(versions, 'option', { name: /Current version/ }),
      ).toBeInTheDocument()
      expect(getByRole(versions, 'option', { name: /newest/ })).toBeInTheDocument()
      // Two snapshots plus the current row.
      expect(versions.querySelectorAll('[role="option"]').length).toBe(3)
    } finally {
      wrapper.unmount()
    }
  })

  test('restoring a version puts it in the editor and on disk', async () => {
    await fs.saveFile('hello.scm', '(display 3)')
    await seedHistory('hello.scm', ['(display 2)', '(display 1)'])

    const wrapper = await mountIdeWith('hello.scm')
    try {
      expect(editorText()).toBe('(display 3)')

      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'File history',
      })
      // The oldest of the three rows: current, newest snapshot, then this one.
      const options = [...dialog.querySelectorAll('[role="option"]')]
      ;(options[2] as HTMLElement).click()
      await flushPromises()
      getByRole(dialog, 'button', { name: 'Restore this version' }).click()
      await flushPromises()

      expect(editorText()).toBe('(display 1)')
      expect(await fs.loadFile('hello.scm')).toBe('(display 1)')
      // The dialog closes on restore.
      expect(
        queryByRole(document.body, 'dialog', { name: 'File history' }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('restoring keeps the version being left in the history', async () => {
    // The point of forcing a snapshot on the way out: a student who restores
    // by mistake can still get back to what they had.
    await fs.saveFile('hello.scm', '(display 3)')
    await seedHistory('hello.scm', ['(display 1)'])

    const wrapper = await mountIdeWith('hello.scm')
    try {
      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'File history',
      })
      const options = [...dialog.querySelectorAll('[role="option"]')]
      ;(options[1] as HTMLElement).click()
      await flushPromises()
      getByRole(dialog, 'button', { name: 'Restore this version' }).click()
      await flushPromises()

      const contents = (await loadHistory(fs, 'hello.scm')).snapshots.map(
        (s) => s.contents,
      )
      // Newest first: the restored version, the one that was current, and the
      // original entry.
      expect(contents).toEqual(['(display 1)', '(display 3)', '(display 1)'])
    } finally {
      wrapper.unmount()
    }
  })

  test('says so when a file has no saved versions yet', async () => {
    await fs.saveFile('hello.scm', '(display 1)')

    const wrapper = await mountIdeWith('hello.scm')
    try {
      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'File history',
      })

      expect(dialog.textContent).toContain('no saved versions yet')
      // Nothing to restore, so the action is unavailable rather than absent.
      expect(
        getByRole(dialog, 'button', { name: 'Restore this version' }),
      ).toBeDisabled()
      // Merely looking at the history recorded nothing.
      expect(await fs.fileExists(historyFilename('hello.scm'))).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('brings back a deleted file from its history', async () => {
    // The recovery #42 is actually about. The sidebar button needs an open
    // file, so a deleted one is reached through the modal's picker.
    await fs.saveFile('hello.scm', '(display 1)')
    await fs.saveFile('other.scm', '(display 2)')

    const wrapper = await mountIdeWith('hello.scm')
    try {
      // Record a snapshot, then delete the file.
      getByRole(document.body, 'button', { name: 'Delete file' }).click()
      const confirm = await findByRole(document.body, 'dialog', {
        name: 'Delete file',
      })
      getByRole(confirm, 'button', { name: 'Delete' }).click()
      await flushPromises()
      expect(await fs.fileExists('hello.scm')).toBe(false)

      // Open another file so the history button is available again.
      getByRole(document.body, 'button', { name: 'Open other.scm' }).click()
      await flushPromises()
      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'File history',
      })

      // The deleted file is listed, marked as such.
      const picker = getByRole<HTMLSelectElement>(dialog, 'combobox')
      expect([...picker.options].map((o) => o.textContent.trim())).toContain(
        'hello.scm (deleted)',
      )

      fireEvent.change(picker, { target: { value: 'hello.scm' } })
      await flushPromises()
      getByRole(dialog, 'button', { name: 'Recover this version' }).click()
      await flushPromises()

      // The file is back, holding what it held, and open in the editor.
      expect(await fs.loadFile('hello.scm')).toBe('(display 1)')
      expect(editorText()).toBe('(display 1)')
      expect(
        getByRole(document.body, 'button', { name: 'Open hello.scm' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('typing after a restore still marks the file dirty', async () => {
    await fs.saveFile('hello.scm', '(display 3)')
    await seedHistory('hello.scm', ['(display 1)'])

    const wrapper = await mountIdeWith('hello.scm')
    try {
      fireEvent.input(getByRole(document.body, 'textbox', { name: 'Source code' }), {
        target: { value: '(display 4)' },
      })
      await flushPromises()

      expect(editorText()).toBe('(display 4)')
    } finally {
      wrapper.unmount()
    }
  })
})
