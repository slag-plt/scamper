import { flushPromises, mount } from '@vue/test-utils'
import { findByText, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import FilesApp from '../../../src/app/files/FilesApp.vue'

// The rescue page end to end (issue #130), over a fake OPFS root. The point of
// this page is that it works when the IDE does not, so what is pinned here is
// that the internal files show up at all, that a download reaches the browser,
// that a deletion is asked about first, and that a failure arrives as a
// sentence rather than as a blank page.

interface StoredFile {
  contents: string
  lastModified: number
}

class FakeRoot {
  files = new Map<string, StoredFile>()
  /** Names passed to removeEntry. */
  removed: string[] = []
  /** Set to fail the next listing, as a browser without storage would. */
  failListing = false

  private fileHandle(name: string) {
    return {
      kind: 'file' as const,
      name,
      getFile: () => {
        const stored = this.files.get(name)
        return Promise.resolve(
          new File([stored?.contents ?? ''], name, {
            lastModified: stored?.lastModified ?? 0,
          }),
        )
      },
      move: (to: string) => {
        const stored = this.files.get(name)
        if (stored !== undefined) {
          this.files.delete(name)
          this.files.set(to, stored)
        }
        return Promise.resolve()
      },
    }
  }

  getFileHandle(name: string) {
    if (!this.files.has(name)) {
      return Promise.reject(
        new DOMException(`no such file: ${name}`, 'NotFoundError'),
      )
    }
    return Promise.resolve(this.fileHandle(name))
  }

  removeEntry(name: string) {
    this.removed.push(name)
    this.files.delete(name)
    return Promise.resolve()
  }

  // An async generator standing in for OPFS's, which is async because the real
  // one is; this fake has nothing to await.
  // eslint-disable-next-line @typescript-eslint/require-await
  async *entries() {
    if (this.failListing) {
      throw new Error('storage is unreadable')
    }
    for (const name of this.files.keys()) {
      yield [name, this.fileHandle(name)]
    }
  }
}

describe('the browser-files page', () => {
  let root: FakeRoot
  /** The `download` name and href of every anchor that was clicked. */
  let downloads: { name: string; url: string }[]
  /** Object URLs the page has released. */
  let revoked: string[]

  beforeEach(() => {
    root = new FakeRoot()
    Object.defineProperty(globalThis.navigator, 'storage', {
      value: {
        getDirectory: () => Promise.resolve(root),
        estimate: () => Promise.resolve({ usage: 1024, quota: 4096 }),
      },
      configurable: true,
    })

    // jsdom implements neither half of the object-URL API.
    revoked = []
    URL.createObjectURL = vi.fn(() => 'blob:mock/0')
    URL.revokeObjectURL = vi.fn((url: string) => revoked.push(url))

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

  /** Mounts the page and waits for its first listing. */
  async function mountPage() {
    const wrapper = mount(FilesApp, { attachTo: document.body })
    await flushPromises()
    return wrapper
  }

  test('lists the internal files the IDE hides', async () => {
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 1 })
    root.files.set('.hello.scm.history', { contents: '{}', lastModified: 1 })
    root.files.set('hello.scm.crswap', { contents: 'partial', lastModified: 1 })
    await mountPage()

    const text = document.body.textContent
    expect(text).toContain('hello.scm')
    expect(text).toContain('.hello.scm.history')
    expect(text).toContain('hello.scm.crswap')
    // And says what they are, since the names alone say nothing.
    expect(text).toContain('Save history for hello.scm')
    expect(text).toContain('Left over from an interrupted save of hello.scm')
  })

  test('hands a file to the browser as a download', async () => {
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 1 })
    await mountPage()

    getByRole(document.body, 'button', { name: 'Download hello.scm' }).click()
    await flushPromises()

    expect(downloads).toEqual([{ name: 'hello.scm', url: 'blob:mock/0' }])
    // Revoked later, not now: revoking during the click cancels the download.
    expect(revoked).toEqual([])
  })

  test('asks before deleting, and refreshes when it has', async () => {
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 1 })
    await mountPage()

    const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false)
    getByRole(document.body, 'button', { name: 'Delete hello.scm' }).click()
    await flushPromises()

    expect(confirm).toHaveBeenCalled()
    expect(root.removed).toEqual([])
    expect(document.body.textContent).toContain('hello.scm')

    confirm.mockReturnValue(true)
    getByRole(document.body, 'button', { name: 'Delete hello.scm' }).click()
    await flushPromises()

    expect(root.removed).toEqual(['hello.scm'])
    expect(
      queryByRole(document.body, 'button', { name: 'Delete hello.scm' }),
    ).toBeNull()
    await findByText(document.body, 'There is nothing stored in this browser.')
  })

  test('shows a failed listing as a banner rather than a blank page', async () => {
    root.failListing = true
    await mountPage()

    const banner = getByRole(document.body, 'alert')
    expect(banner.textContent).toContain('Listing your files failed')
    expect(banner.textContent).toContain('storage is unreadable')
    // The explanation of what the page is for is still there to read.
    expect(document.body.textContent).toContain(
      'Everything Scamper keeps in this browser',
    )
  })

  test('clears the IDE settings without touching the files', async () => {
    localStorage.setItem('scamper.config', '{"lastOpenedFilename":"big.scm"}')
    localStorage.setItem('scamper.view', 'notebook')
    localStorage.setItem('scamper-theme', 'dark')
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 1 })
    await mountPage()

    vi.spyOn(window, 'confirm').mockReturnValue(true)
    getByRole(document.body, 'button', {
      name: 'Reset Scamper’s settings',
    }).click()
    await flushPromises()

    expect(localStorage.getItem('scamper.config')).toBeNull()
    expect(localStorage.getItem('scamper.view')).toBeNull()
    // A different prefix, and light/dark cannot wedge anything.
    expect(localStorage.getItem('scamper-theme')).toBe('dark')
    expect(root.removed).toEqual([])
    localStorage.clear()
  })

  test('asks before a rename replaces a file that already exists', async () => {
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 1 })
    root.files.set('backup.scm', { contents: '(display 2)', lastModified: 2 })
    await mountPage()

    vi.spyOn(window, 'prompt').mockReturnValue('hello.scm')
    const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false)
    getByRole(document.body, 'button', { name: 'Rename backup.scm' }).click()
    await flushPromises()

    expect(confirm).toHaveBeenCalled()
    expect(root.files.get('hello.scm')?.contents).toBe('(display 1)')
    expect(root.files.has('backup.scm')).toBe(true)

    confirm.mockReturnValue(true)
    getByRole(document.body, 'button', { name: 'Rename backup.scm' }).click()
    await flushPromises()

    expect(root.files.get('hello.scm')?.contents).toBe('(display 2)')
    expect(root.files.has('backup.scm')).toBe(false)
  })

  test('renames onto an unused name without asking', async () => {
    root.files.set('hello.scm', { contents: '(display 1)', lastModified: 1 })
    await mountPage()

    vi.spyOn(window, 'prompt').mockReturnValue('goodbye.scm')
    const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false)
    getByRole(document.body, 'button', { name: 'Rename hello.scm' }).click()
    await flushPromises()

    expect(confirm).not.toHaveBeenCalled()
    expect(root.files.has('goodbye.scm')).toBe(true)
  })
})
