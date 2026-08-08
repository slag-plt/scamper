import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole, getByRole, waitFor } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import JSZip from 'jszip'
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

const EXPORT_BUTTON = 'Download all files as a zip archive'

// Exporting the file drawer as a zip (issue #42). These tests drive the whole
// path: the sidebar button, the confirmation, and the download the browser is
// handed.
describe('IDE zip export', () => {
  let fs: MockFileSystem
  /** The blob each object URL was minted from, keyed by URL. */
  let blobs: Map<string, Blob>
  /** The `download` name of every anchor that was clicked. */
  let downloads: { name: string; url: string }[]

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))

    // jsdom implements neither half of the object-URL API.
    blobs = new Map()
    URL.createObjectURL = vi.fn((blob: Blob) => {
      const url = `blob:mock/${blobs.size.toString()}`
      blobs.set(url, blob)
      return url
    })
    URL.revokeObjectURL = vi.fn()

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

  /** Mounts the IDE and waits for the file drawer to be populated. */
  async function mountIde() {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  /** Clicks the export button and confirms the dialog it raises. */
  async function exportFiles() {
    getByRole(document.body, 'button', { name: EXPORT_BUTTON }).click()
    const dialog = await findByRole(document.body, 'dialog', {
      name: 'Export files',
    })
    getByRole(dialog, 'button', { name: 'Download' }).click()
  }

  /** @returns the archive handed to the browser by the one download so far. */
  async function downloadedArchive(): Promise<JSZip> {
    // Zipping spans several tasks, so wait for the download rather than
    // flushing a fixed number of them.
    await waitFor(() => {
      expect(downloads).toHaveLength(1)
    })
    return JSZip.loadAsync(await blobs.get(downloads[0].url)!.arrayBuffer())
  }

  test('downloads the drawer as a date-stamped zip once confirmed', async () => {
    await fs.saveFile('hello.scm', '(display "hello")')
    await fs.saveFile('shapes.scm', '(solid-square 100 "red")')
    // Internal state the student never sees in the drawer.
    await fs.saveFile('.hello.scm.history', '{"version":1}')

    const wrapper = await mountIde()
    try {
      await exportFiles()

      const zip = await downloadedArchive()
      expect(downloads[0].name).toMatch(/^scamper-files-\d{4}-\d{2}-\d{2}\.zip$/)
      expect(Object.keys(zip.files).sort()).toEqual(['hello.scm', 'shapes.scm'])
      expect(await zip.file('hello.scm')!.async('string')).toBe('(display "hello")')
    } finally {
      wrapper.unmount()
    }
  })

  test('archives the edits still sitting in the editor', async () => {
    await fs.saveFile('hello.scm', '(display "hello")')

    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()
      // Typed but never saved: the export has to save the open file first.
      fireEvent.input(getByRole(document.body, 'textbox', { name: 'Source code' }), {
        target: { value: '(display "edited")' },
      })
      await flushPromises()

      await exportFiles()

      const zip = await downloadedArchive()
      expect(await zip.file('hello.scm')!.async('string')).toBe('(display "edited")')
    } finally {
      wrapper.unmount()
    }
  })

  test('downloads nothing when the confirmation is cancelled', async () => {
    await fs.saveFile('hello.scm', '(display "hello")')

    const wrapper = await mountIde()
    try {
      getByRole(document.body, 'button', { name: EXPORT_BUTTON }).click()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'Export files',
      })
      getByRole(dialog, 'button', { name: 'Cancel' }).click()
      await flushPromises()

      expect(downloads).toEqual([])
      // No object URL was minted, so no archive was ever built.
      expect(blobs.size).toBe(0)
    } finally {
      wrapper.unmount()
    }
  })

  test('offers nothing to export until there is a file', async () => {
    const wrapper = await mountIde()
    try {
      const button = getByRole(document.body, 'button', { name: EXPORT_BUTTON })
      expect(button).toBeDisabled()

      // Creating a file makes the export available.
      getByRole(document.body, 'button', { name: 'Create file' }).click()
      const dialog = await findByRole(document.body, 'dialog', { name: 'New file' })
      fireEvent.input(getByRole(dialog, 'textbox'), {
        target: { value: 'hello.scm' },
      })
      getByRole(dialog, 'button', { name: 'OK' }).click()
      await flushPromises()

      await waitFor(() => {
        expect(button).toBeEnabled()
      })
    } finally {
      wrapper.unmount()
    }
  })

  test('reports a failure to read a file instead of downloading a partial zip', async () => {
    await fs.saveFile('hello.scm', '(display "hello")')

    const wrapper = await mountIde()
    try {
      // The file is listed in the drawer, but is gone by the time it is read.
      vi.spyOn(fs, 'loadFile').mockRejectedValue(new Error('NotFoundError'))

      await exportFiles()

      const failure = await findByRole(document.body, 'dialog', {
        name: 'Export failed',
      })
      expect(failure.textContent).toContain('hello.scm')
      expect(downloads).toEqual([])
    } finally {
      wrapper.unmount()
    }
  })
})
