import { flushPromises, mount } from '@vue/test-utils'
import { findByRole } from '@testing-library/dom'
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

await initialize()

// #390: the Run control's two halves each paint a hover background, and each
// has to keep the pill's rounding on its outer corners or that background
// spills past it. A cascade question, so it needs a browser: jsdom resolves
// no scoped stylesheet and reports every radius as the empty string.

describe('the Run control keeps its rounded corners (#390)', () => {
  beforeEach(() => {
    FS.setBackend(FS.localBackend(new MockFileSystem()))
    localStorage.clear()
    // The palette lives in public/css/theme.css, which nothing loads here, so
    // --radius-md is stood up by hand: without it every radius below resolves
    // to 0px and the test cannot tell a square corner from a rounded one. The
    // value is arbitrary -- the bug is which rule wins, not how round it is.
    document.documentElement.style.setProperty('--radius-md', '6px')
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.documentElement.style.removeProperty('--radius-md')
    document.body.innerHTML = ''
  })

  const radii = (selector: string) => {
    const el = document.querySelector(selector)
    if (el === null) throw new Error(`no ${selector} in the header`)
    const s = getComputedStyle(el)
    return {
      topLeft: s.borderTopLeftRadius,
      topRight: s.borderTopRightRadius,
      bottomRight: s.borderBottomRightRadius,
      bottomLeft: s.borderBottomLeftRadius,
    }
  }

  test('each half is rounded on the outside and square on the seam', async () => {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    try {
      const pill = radii('.run-group').topLeft
      expect(pill).not.toBe('0px')

      expect(radii('.run-main')).toEqual({
        topLeft: pill,
        topRight: '0px',
        bottomRight: '0px',
        bottomLeft: pill,
      })
      expect(radii('.run-caret')).toEqual({
        topLeft: '0px',
        topRight: pill,
        bottomRight: pill,
        bottomLeft: '0px',
      })

      // Rounding the halves is only worth anything while something still
      // paints on them: the corners were invisible until one did. The caret's
      // open state is the half of that reachable without a synthetic hover.
      const caret = document.querySelector<HTMLElement>('.run-caret')
      if (caret === null) throw new Error('no .run-caret in the header')
      caret.click()
      await flushPromises()
      expect(caret.classList.contains('open')).toBe(true)
      expect(getComputedStyle(caret).backgroundColor).not.toBe('rgba(0, 0, 0, 0)')
    } finally {
      wrapper.unmount()
    }
  })
})
