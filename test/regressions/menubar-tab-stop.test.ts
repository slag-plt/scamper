import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole } from '@testing-library/dom'
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

// #391 dropped three guards in IdeMenuBar that TypeScript could prove dead --
// an empty `menus`, and so a tab stop with nowhere to sit. They were dead, but
// only because `menus` is a fixed literal, which is a fact about the file
// rather than about the type. This pins what they were nominally protecting,
// so a later `menus` that can be empty fails here rather than silently.
//
// N.B. the warnings themselves are what #391 reports; `npx eslint` on the
// component is the reproduction, and this is the behavioural guard beside it.
describe('the menu bar keeps exactly one tab stop (#391)', () => {
  beforeEach(() => {
    FS.setBackend(FS.localBackend(new MockFileSystem()))
    localStorage.clear()
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  const titles = () => [
    ...document.querySelectorAll<HTMLElement>('[data-menu]'),
  ]

  const stops = () => titles().filter((t) => t.tabIndex === 0)

  test('it rests on the first title, and moves with the arrow keys', async () => {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    try {
      const all = titles()
      expect(all.length).toBeGreaterThan(0)
      expect(stops()).toEqual([all[0]])

      fireEvent.keyDown(all[0], { key: 'ArrowRight' })
      await flushPromises()
      expect(stops()).toEqual([all[1]])
    } finally {
      wrapper.unmount()
    }
  })
})
