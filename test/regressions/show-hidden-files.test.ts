import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../src/app/web/components/IdeApp.vue'
import * as FS from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { setShowHiddenFiles } from '../../src/app/web/file-prefs'
import { initialize } from '../../src/scamper'

vi.mock('../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../stubs/MockCodeMirrorEditor.vue'),
)

vi.mock(
  '../../src/app/web/components/ResultsPane.vue',
  () => import('../stubs/MockResultsPane.vue'),
)

await initialize()

// Regression test for #178: dotted files exist in the file system but never
// appeared in the drawer, with no way to see them. They are Scamper's own
// bookkeeping -- a file's saved history, a config an older build left behind --
// so hiding them by default is right; having no way to look was the gap.
//
// Now that there is a menu bar, the View menu carries a "Show Hidden Files"
// toggle, off by default.

describe('#178: showing hidden files', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    setShowHiddenFiles(false)
    await fs.saveFile('hello.scm', '(display "hi")')
    await fs.saveFile('.hidden-notes', 'internal')
  })

  afterEach(() => {
    setShowHiddenFiles(false)
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  async function mountIde() {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  /** Opens the View menu and returns it. */
  async function openViewMenu(): Promise<HTMLElement> {
    getByRole(document.body, 'menuitem', { name: 'View' }).click()
    await flushPromises()
    return getByRole(document.body, 'menu')
  }

  const drawerHas = (name: string) =>
    queryByRole(document.body, 'button', { name: `Open ${name}` }) !== null

  test('the drawer hides dotted files by default', async () => {
    const wrapper = await mountIde()
    try {
      expect(drawerHas('hello.scm')).toBe(true)
      expect(drawerHas('.hidden-notes')).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('the View menu offers the toggle, unchecked to start', async () => {
    const wrapper = await mountIde()
    try {
      const item = getByRole(await openViewMenu(), 'menuitemcheckbox', {
        name: 'Show Hidden Files',
      })
      expect(item).toHaveAttribute('aria-checked', 'false')
    } finally {
      wrapper.unmount()
    }
  })

  test('turning it on reveals them without waiting for anything else', async () => {
    const wrapper = await mountIde()
    try {
      expect(drawerHas('.hidden-notes')).toBe(false)

      getByRole(await openViewMenu(), 'menuitemcheckbox', {
        name: 'Show Hidden Files',
      }).click()
      await flushPromises()

      expect(drawerHas('.hidden-notes')).toBe(true)
      // The student's own files are still there, not replaced by them.
      expect(drawerHas('hello.scm')).toBe(true)
    } finally {
      wrapper.unmount()
    }
  })

  test('turning it back off hides them again', async () => {
    const wrapper = await mountIde()
    try {
      setShowHiddenFiles(true)
      await flushPromises()
      expect(drawerHas('.hidden-notes')).toBe(true)

      setShowHiddenFiles(false)
      await flushPromises()
      expect(drawerHas('.hidden-notes')).toBe(false)
    } finally {
      wrapper.unmount()
    }
  })

  test('the choice is remembered', async () => {
    const wrapper = await mountIde()
    try {
      setShowHiddenFiles(true)
      await flushPromises()
    } finally {
      wrapper.unmount()
    }
    expect(localStorage.getItem('scamper.files.showHidden')).toBe('true')

    const again = await mountIde()
    try {
      expect(drawerHas('.hidden-notes')).toBe(true)
    } finally {
      again.unmount()
    }
  })
})
