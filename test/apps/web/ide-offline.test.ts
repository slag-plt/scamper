import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import * as Connectivity from '../../../src/app/web/connectivity'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { historyFilename } from '../../../src/history/flat-file'
import { initialize } from '../../../src/scamper'

// What the IDE does when the file server cannot be reached (#357).
//
// The behaviour being pinned is the one that was wrong before: a dropped
// connection used to put an error over the editor that could not be dismissed,
// which meant a wifi hiccup ended the session. Now it says so, lets the student
// carry on writing, and refuses only the things that genuinely need the server.

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

// A signed-in user on a server-backed deployment, which is the only
// configuration where being offline costs anything: their files are the
// server's, so losing it puts them out of reach.
vi.mock('../../../src/app/web/server-session', () => ({
  serverSession: () => ({
    client: {},
    methods: { password: true },
    user: { id: 'u1', name: 'Ada', email: 'ada@example.edu' },
  }),
  usesServerFiles: () => true,
  restart: vi.fn(),
}))

await initialize()

describe('IDE offline behaviour', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = new MockFileSystem()
    await fs.saveFile('hello.scm', '(+ 1 2)')
    FS.setBackend(FS.localBackend(fs))
  })

  afterEach(() => {
    Connectivity.stop()
    vi.restoreAllMocks()
    vi.unstubAllGlobals()
    document.body.innerHTML = ''
  })

  /** The editor's current contents, as the student sees them. */
  function editorText(): string {
    return getByRole<HTMLTextAreaElement>(document.body, 'textbox', {
      name: 'Source code',
    }).value
  }

  /** Mounts the IDE and waits for it to settle. */
  async function mountIde() {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  test('the file drawer says whose files these are, and whether they are reachable', async () => {
    const wrapper = await mountIde()
    try {
      // Moved out of the status bar and into the drawer, above the buttons:
      // the server is the difference between files that survive this browser
      // and files that do not.
      expect(document.body.textContent).toContain('ada@example.edu')
      expect(
        getByRole(document.body, 'button', { name: 'Sign out' }),
      ).toBeTruthy()
      expect(document.body.textContent).not.toContain('Offline')

      Connectivity.reportUnreachable()
      await flushPromises()

      expect(document.body.textContent).toContain(
        'Offline — your changes are not being saved.',
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('an operation that needs the server is refused in a modal that closes', async () => {
    const wrapper = await mountIde()
    try {
      Connectivity.reportUnreachable()
      await flushPromises()

      getByRole(document.body, 'button', { name: 'Create file' }).click()
      await flushPromises()

      // Refused, and said so -- not the file-name prompt it would normally be,
      // which is why there is nothing to type into.
      const dialog = getByRole(document.body, 'dialog')
      expect(dialog.textContent).toContain('Scamper is offline')
      expect(queryByRole(dialog, 'textbox')).toBeNull()

      // And it goes away, which is the whole point: the old error screen did
      // not, so a dropped connection ended the session.
      getByRole(dialog, 'button', { name: 'OK' }).click()
      await flushPromises()
      expect(queryByRole(document.body, 'dialog')).toBeNull()

      // The editor is still there and still editable.
      expect(
        getByRole(document.body, 'textbox', { name: 'Source code' }),
      ).toBeTruthy()
    } finally {
      wrapper.unmount()
    }
  })

  // The history modal is only opened while the server is reachable, but it
  // stays open across a drop. Restoring then used to replace the document with
  // an old version while the save meant to pin the current one silently
  // declined -- losing unsaved work in favour of something that could not be
  // written back either.
  test('restoring while offline is refused rather than losing the buffer', async () => {
    // A version to restore, in the flat-file layout the local backend uses.
    await fs.saveFile(
      historyFilename('hello.scm'),
      JSON.stringify({
        version: 1,
        snapshots: [
          { time: new Date(Date.UTC(2026, 7, 7, 9)).toISOString(), contents: '(old)' },
        ],
      }),
    )

    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()

      getByRole(document.body, 'button', { name: 'File history' }).click()
      await flushPromises()
      const dialog = getByRole(document.body, 'dialog')
      const options = [...dialog.querySelectorAll('[role="option"]')]
      ;(options[1] as HTMLElement).click()
      await flushPromises()
      const before = editorText()

      Connectivity.reportUnreachable()
      await flushPromises()

      getByRole(dialog, 'button', { name: 'Restore this version' }).click()
      await flushPromises()

      // Refused with the dismissable notice, and -- the point -- the editor
      // still holds what the student had rather than the old version.
      expect(document.body.textContent).toContain('Scamper is offline')
      expect(editorText()).toBe(before)
      expect(editorText()).not.toBe('(old)')
    } finally {
      wrapper.unmount()
    }
  })

  // Switching forces a save of the outgoing file (#238), and offline that save
  // cannot land -- so a switch that went ahead would drop the edit on its way
  // to a file it then could not load either. Refused instead, and the editor
  // keeps what the student had.
  test('switching files while offline is refused, keeping the buffer', async () => {
    await fs.saveFile('other.scm', '(other)')
    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()
      const before = editorText()

      Connectivity.reportUnreachable()
      await flushPromises()

      getByRole(document.body, 'button', { name: 'Open other.scm' }).click()
      await flushPromises()

      expect(document.body.textContent).toContain('Scamper is offline')
      expect(editorText()).toBe(before)
      expect(editorText()).not.toBe('(other)')
    } finally {
      wrapper.unmount()
    }
  })

  // The one file action that still works offline, and the only way to get work
  // off a machine that cannot save. A stopgap until #364.
  test('downloading the open file works offline, from the editor', async () => {
    const downloads: string[] = []
    vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(
      function (this: HTMLAnchorElement) {
        downloads.push(this.href)
      },
    )

    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()

      Connectivity.reportUnreachable()
      await flushPromises()

      getByRole(document.body, 'button', { name: 'Download file' }).click()
      await flushPromises()

      // Not refused, and carrying what the editor holds rather than what the
      // unreachable server last stored.
      expect(downloads).toHaveLength(1)
      expect(decodeURIComponent(downloads[0].split(',')[1])).toBe(editorText())
      expect(document.body.textContent).not.toContain('Scamper is offline')
    } finally {
      wrapper.unmount()
    }
  })

  test('nothing is written while offline, and the reconnect writes at once', async () => {
    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()

      const saves = vi.spyOn(fs, 'saveFile')
      Connectivity.reportUnreachable()
      await flushPromises()

      // A lifecycle save -- the one that fires when the tab is hidden -- gets
      // the same answer autosave does: there is nowhere to write to.
      hideTab()
      await flushPromises()
      expect(saves).not.toHaveBeenCalled()

      // Recovery is driven through the heartbeat because that is the only way
      // production ever comes back online -- nothing else calls into
      // connectivity with good news. A test that flipped the state directly
      // would pass while the real path stayed broken.
      vi.stubGlobal('fetch', () =>
        Promise.resolve({ ok: true, status: 200 } as Response),
      )
      Connectivity.start('https://files.example/api/v1')
      await Connectivity.checkNow()
      await flushPromises()

      // Coming back is the moment the editor and the server have drifted
      // furthest apart, so it saves then rather than at the next tick.
      expect(saves).toHaveBeenCalledWith('hello.scm', expect.any(String))
    } finally {
      wrapper.unmount()
    }
  })
})

/** Hides the tab, which is what makes the IDE save outside of autosave. */
function hideTab(): void {
  Object.defineProperty(document, 'visibilityState', {
    configurable: true,
    get: () => 'hidden',
  })
  document.dispatchEvent(new Event('visibilitychange'))
}
