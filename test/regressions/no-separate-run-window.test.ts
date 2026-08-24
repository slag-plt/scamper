import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../src/app/web/components/IdeApp.vue'
import * as FS from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
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

// Regression test for #372: the "separate run window" opened the program in a
// standalone `runner.html` tab. It predates both the server-backed file system
// (the runner reloads the file from storage, so it showed the wrong thing, or
// nothing, for a signed-in user) and the Vue render system (it never gained
// parity). Removed rather than brought back to parity.
//
// What this pins is that the affordance is gone from both places that offered
// it, and that nothing opens a browser tab behind the user's back.

describe('#372: the separate run window is gone', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    localStorage.clear()
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
    localStorage.clear()
  })

  async function mountIde() {
    await fs.saveFile('hello.scm', '(display "hi")')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  test('the toolbar no longer offers it', async () => {
    const wrapper = await mountIde()
    try {
      expect(
        queryByRole(document.body, 'button', { name: 'Open Run Window' }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('the Run menu no longer offers it', async () => {
    const wrapper = await mountIde()
    try {
      // The bar's own titles are menuitems; clicking one opens its menu.
      getByRole(document.body, 'menubar')
      getByRole(document.body, 'menuitem', { name: 'Run' }).click()
      await flushPromises()
      const menu = getByRole(document.body, 'menu')
      expect(queryByRole(menu, 'menuitem', { name: 'Open Run Window' })).toBeNull()
      // The menu did open, so this is not passing vacuously.
      expect(
        queryByRole(menu, 'menuitem', { name: 'Query Value at Cursor' }),
      ).not.toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('running opens no browser tab', async () => {
    const open = vi.spyOn(window, 'open').mockReturnValue(null)
    const wrapper = await mountIde()
    try {
      // Open the file first: running an empty editor is a different bug (#366).
      getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
      await flushPromises()
      getByRole(document.body, 'button', { name: 'Run' }).click()
      await flushPromises()
      expect(open).not.toHaveBeenCalled()
    } finally {
      wrapper.unmount()
    }
  })
})
