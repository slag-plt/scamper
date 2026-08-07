import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole, waitFor } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import JSZip from 'jszip'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
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

// Exporting the file drawer as a zip (issue #42). These tests drive the whole
// path: the sidebar button, the confirmation, and the download the browser is
// handed.
describe('IDE zip export', () => {
  let fs: MockFileSystem
  /** The blob each object URL was minted from, keyed by URL. */
  let blobs: Map<string, Blob>
  /** The `download` name of every anchor that was clicked. */
  let downloads: { name: string; url: string }[]
  let revoked: string[]

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setFS(fs)

    // jsdom implements neither half of the object-URL API.
    blobs = new Map()
    revoked = []
    let nextUrl = 0
    URL.createObjectURL = vi.fn((blob: Blob) => {
      const url = `blob:mock/${(nextUrl++).toString()}`
      blobs.set(url, blob)
      return url
    })
    URL.revokeObjectURL = vi.fn((url: string) => {
      revoked.push(url)
    })

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

  function clickExport() {
    getByRole(document.body, 'button', { name: 'Download zip archive' }).click()
  }

  test('downloads the drawer as a date-stamped zip once confirmed', async () => {
    await fs.saveFile('hello.scm', '(display "hello")')
    await fs.saveFile('shapes.scm', '(solid-square 100 "red")')
    // Internal state the student never sees in the drawer.
    await fs.saveFile('.scamper.config', '{}')

    const wrapper = await mountIde()
    try {
      clickExport()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'Export files',
      })
      expect(dialog.textContent).toContain('all 2 of your files')
      getByRole(dialog, 'button', { name: 'Download' }).click()
      // Zipping spans several tasks, so wait for the download rather than
      // flushing a fixed number of them.
      await waitFor(() => {
        expect(downloads).toHaveLength(1)
      })
      expect(downloads[0].name).toMatch(/^scamper-files-\d{4}-\d{2}-\d{2}\.zip$/)

      const zip = await JSZip.loadAsync(await blobs.get(downloads[0].url)!.arrayBuffer())
      expect(Object.keys(zip.files).sort()).toEqual(['hello.scm', 'shapes.scm'])
      expect(await zip.file('hello.scm')!.async('string')).toBe('(display "hello")')

      // The object URL is released once the download has started, so the blob
      // isn't pinned in memory for the rest of the session.
      await waitFor(() => {
        expect(revoked).toEqual([downloads[0].url])
      })
    } finally {
      wrapper.unmount()
    }
  })

  test('downloads nothing when the confirmation is cancelled', async () => {
    await fs.saveFile('hello.scm', '(display "hello")')

    const wrapper = await mountIde()
    try {
      clickExport()
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

  test('says so when there is nothing to export', async () => {
    const wrapper = await mountIde()
    try {
      clickExport()
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'Export files',
      })
      expect(dialog.textContent).toContain('no files to export')
      // An alert, not a confirmation: there is nothing to go ahead with.
      expect(queryByRole(dialog, 'button', { name: 'Cancel' })).toBeNull()
      getByRole(dialog, 'button', { name: 'OK' }).click()
      await flushPromises()

      expect(downloads).toEqual([])
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

      clickExport()
      const confirm = await findByRole(document.body, 'dialog', {
        name: 'Export files',
      })
      getByRole(confirm, 'button', { name: 'Download' }).click()

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
