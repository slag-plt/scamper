import { flushPromises, mount } from '@vue/test-utils'
import { findByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { required } from '../../dom'
import { initialize } from '../../../src/scamper'

vi.mock('../../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

await initialize()

// Where a popup menu lands needs real boxes: it measures itself and clamps
// into the viewport, and jsdom reports every element as zero-sized, so under
// jsdom every menu collapses onto the same margin and the question cannot be
// asked. The behavioural half -- the stale highlight, the dialog guard -- is in
// menu-keyboard.test.ts.

describe('popup menu placement', () => {
  beforeEach(() => {
    FS.setBackend(FS.localBackend(new MockFileSystem()))
    localStorage.clear()
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  // A missing element is a broken spec, not a box that moved, so these throw
  // and name what was not there -- rather than folding it into a NaN whose
  // comparison fails a few lines later with nothing to point at.
  const title = (name: string) =>
    required(
      document.querySelector<HTMLElement>(`[data-menu="${name}"]`),
      `a "${name}" menu title`,
    )
  const menu = () =>
    required(document.querySelector<HTMLElement>('[role="menu"]'), 'an open menu')
  const left = (el: Element) => el.getBoundingClientRect().left

  test('the panel follows the title whose menu it is showing', async () => {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    try {
      title('File').click()
      await flushPromises()
      await new Promise((r) => requestAnimationFrame(r))
      const underFile = left(menu())

      // Sliding along the bar swaps the items on one persistent instance,
      // which is exactly the case that used to leave the panel behind.
      title('Help').click()
      await flushPromises()
      await new Promise((r) => requestAnimationFrame(r))

      expect(menu().textContent).toContain('About Scamper')
      const underHelp = left(menu())
      expect(underHelp).toBeGreaterThan(underFile + 50)
      // And it sits under Help rather than merely somewhere else.
      expect(Math.abs(underHelp - left(title('Help')))).toBeLessThan(30)
    } finally {
      wrapper.unmount()
    }
  })

  test('a menu taller than the window is pulled fully on-screen', async () => {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    try {
      // View is the longest menu; the old clamp assumed 24px rows and could
      // hand it a negative top, losing its first items off the top edge.
      title('View').click()
      await flushPromises()
      await new Promise((r) => requestAnimationFrame(r))

      const box = menu().getBoundingClientRect()
      expect(box.top).toBeGreaterThanOrEqual(0)
      expect(box.left).toBeGreaterThanOrEqual(0)
      expect(box.bottom).toBeLessThanOrEqual(window.innerHeight + 1)
    } finally {
      wrapper.unmount()
    }
  })
})
