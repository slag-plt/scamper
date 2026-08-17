import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import {
  FlatFileHistory,
  historyFilename,
} from '../../../src/history/flat-file'

/**
 * The whole of `filename`'s flat-file history, snapshots carrying their
 * contents. `History.index` deliberately answers without them -- a
 * server-backed history keeps each snapshot in its own row -- so a test that
 * wants to see what was recorded reads them back through the interface.
 */
async function loadHistory(
  fs: FS.t,
  filename: string,
): Promise<{ snapshots: { time: string; contents: string }[]; deletedAt?: string }> {
  const history = new FlatFileHistory(fs)
  const index = await history.index(filename)
  const snapshots = await Promise.all(
    index.snapshots.map(async (s) => ({
      time: s.time,
      contents: (await history.read(filename, s.id)) ?? '<missing>',
    })),
  )
  return { ...index, snapshots }
}

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

// Browsing and restoring a file's saved history (issue #42), driven through a
// mounted IDE: the sidebar button, the timeline, and what a restore does to
// both the editor and storage.
describe('IDE file history', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** Seeds `filename` with a history of `contents`, oldest last. */
  async function seedHistory(
    filename: string,
    contents: string[],
    deletedAt?: string,
  ) {
    await fs.saveFile(
      historyFilename(filename),
      JSON.stringify({
        version: 1,
        ...(deletedAt === undefined ? {} : { deletedAt }),
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

  /** Picks `action` from `filename`'s ⋯ menu in the file drawer. */
  async function fileMenu(filename: string, action: string | RegExp) {
    getByRole(document.body, 'button', {
      name: `Actions for ${filename}`,
    }).click()
    await flushPromises()
    getByRole(document.body, 'menuitem', { name: action }).click()
    await flushPromises()
  }

  test('offers the history even with no file open', async () => {
    // Deliberately not disabled without a current file: deleting one leaves
    // exactly that state, and it is when recovery matters most.
    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()

      expect(
        getByRole(document.body, 'button', { name: 'File history' }),
      ).toBeEnabled()
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
    // The recovery #42 is actually about. A deleted file has no row in the
    // drawer and so no menu of its own, so it is reached through the toolbar
    // button and the modal's picker.
    await fs.saveFile('hello.scm', '(display 1)')
    await fs.saveFile('other.scm', '(display 2)')

    const wrapper = await mountIdeWith('hello.scm')
    try {
      // Record a snapshot, then delete the file.
      await fileMenu('hello.scm', 'Delete')
      const confirm = await findByRole(document.body, 'dialog', {
        name: 'Delete file',
      })
      getByRole(confirm, 'button', { name: 'Delete' }).click()
      await flushPromises()
      expect(await fs.fileExists('hello.scm')).toBe(false)

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

  test('reaches a deleted file\'s history with nothing left open', async () => {
    // Deleting clears the current file, and the button used to be disabled
    // without one -- so the recovery UI was unreachable exactly when it was
    // needed, and unreachable entirely if that file was the only one.
    await fs.saveFile('hello.scm', '(display 1)')

    const wrapper = await mountIdeWith('hello.scm')
    try {
      await fileMenu('hello.scm', 'Delete')
      const confirm = await findByRole(document.body, 'dialog', { name: 'Delete file' })
      getByRole(confirm, 'button', { name: 'Delete' }).click()
      await flushPromises()

      const button = getByRole(document.body, 'button', { name: 'File history' })
      expect(button).toBeEnabled()
      button.click()
      const dialog = await findByRole(document.body, 'dialog', { name: 'File history' })
      expect(
        [...getByRole<HTMLSelectElement>(dialog, 'combobox').options].map((o) =>
          o.textContent.trim(),
        ),
      ).toEqual(['hello.scm (deleted)'])

      getByRole(dialog, 'button', { name: 'Recover this version' }).click()
      await flushPromises()
      expect(await fs.loadFile('hello.scm')).toBe('(display 1)')
    } finally {
      wrapper.unmount()
    }
  })

  test('says so when no file has a history yet', async () => {
    await fs.saveFile('hello.scm', '(display 1)')

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()
      getByRole(document.body, 'button', { name: 'File history' }).click()

      const dialog = await findByRole(document.body, 'dialog', { name: 'File history' })
      expect(dialog.textContent).toContain('No file has a saved history yet')
      // use-modals keeps one module-level queue, so an alert left active would
      // show up in the next test's ModalHost.
      getByRole(dialog, 'button', { name: 'OK' }).click()
      await flushPromises()
    } finally {
      wrapper.unmount()
    }
  })

  test('a slow history read does not land under a newer selection', async () => {
    // Arrowing through the picker fires one change per key. The newest
    // selection has to win, not whichever read finishes last -- otherwise the
    // dialog shows one file's versions under another file's name, and
    // restoring writes them onto that other file.
    await fs.saveFile('c.scm', '(display 3)')
    await fs.saveFile('a.scm', 'a now')
    await seedHistory('a.scm', ['a old 1', 'a old 2'])
    await fs.saveFile('b.scm', 'b now')
    await seedHistory('b.scm', ['b old'])

    const wrapper = await mountIdeWith('c.scm')
    try {
      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', { name: 'File history' })
      const picker = getByRole<HTMLSelectElement>(dialog, 'combobox')

      // Only now hold a.scm's history read open, so opening the dialog (which
      // reads every history to build the picker) is unaffected.
      const realLoad = fs.loadFile.bind(fs)
      let releaseA: (() => void) | undefined
      vi.spyOn(fs, 'loadFile').mockImplementation((name: string) => {
        if (name !== historyFilename('a.scm')) return realLoad(name)
        return new Promise<string>((resolve, reject) => {
          releaseA = () => {
            realLoad(name).then(resolve, reject)
          }
        })
      })

      fireEvent.change(picker, { target: { value: 'a.scm' } })
      await Promise.resolve()
      fireEvent.change(picker, { target: { value: 'b.scm' } })
      await flushPromises()
      // a.scm's read now finishes, after the selection moved on.
      releaseA?.()
      await flushPromises()

      // b.scm has one saved version and is not the open file, so there is no
      // "current" row: two options would mean a.scm's history landed here.
      expect(picker.value).toBe('b.scm')
      expect(dialog.querySelectorAll('[role="option"]').length).toBe(1)
    } finally {
      wrapper.unmount()
    }
  })

  test('pins another file\'s contents before restoring over them', async () => {
    // A save inside the merge window reaches disk without becoming a snapshot.
    // Restoring over it from the picker must not take those contents off both
    // disk and timeline -- the footer promises the current version is kept.
    await fs.saveFile('a.scm', 'saved but never snapshotted')
    await seedHistory('a.scm', ['old version'])
    await fs.saveFile('b.scm', '(display 2)')

    const wrapper = await mountIdeWith('b.scm')
    try {
      getByRole(document.body, 'button', { name: 'File history' }).click()
      const dialog = await findByRole(document.body, 'dialog', { name: 'File history' })
      fireEvent.change(getByRole<HTMLSelectElement>(dialog, 'combobox'), {
        target: { value: 'a.scm' },
      })
      await flushPromises()
      getByRole(dialog, 'button', { name: 'Restore this version' }).click()
      await flushPromises()

      expect(await fs.loadFile('a.scm')).toBe('old version')
      const contents = (await loadHistory(fs, 'a.scm')).snapshots.map((sn) => sn.contents)
      expect(contents).toContain('saved but never snapshotted')
    } finally {
      wrapper.unmount()
    }
  })

  test('asks before a rename discards a deleted file\'s history', async () => {
    // renameHistory overwrites the destination, so renaming onto the name of a
    // deleted file would silently destroy the only way back to it.
    await fs.saveFile('a.scm', 'contents')
    await seedHistory('gone.scm', ['recoverable'], '2026-08-07T12:00:00.000Z')

    const wrapper = await mountIdeWith('a.scm')
    try {
      await fileMenu('a.scm', /^Rename/)
      const prompt = await findByRole(document.body, 'dialog', { name: 'Rename file' })
      fireEvent.input(getByRole(prompt, 'textbox'), { target: { value: 'gone.scm' } })
      getByRole(prompt, 'button', { name: 'OK' }).click()

      const warning = await findByRole(document.body, 'dialog', {
        name: 'Discard saved history',
      })
      expect(warning.textContent).toContain('gone.scm')
      getByRole(warning, 'button', { name: 'Cancel' }).click()
      await flushPromises()

      // Backing out leaves both the file and the recoverable history alone.
      expect(await fs.fileExists('a.scm')).toBe(true)
      expect(
        (await loadHistory(fs, 'gone.scm')).snapshots.map((sn) => sn.contents),
      ).toEqual(['recoverable'])
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
