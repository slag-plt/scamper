import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import Scamper, { initialize, type DisplayRequest } from '../../../src/scamper'
import { DEFAULT_IDLE_MS } from '../../../src/app/web/composables/use-live-evaluation'
import {
  liveEvaluation,
  setLiveEvaluation,
} from '../../../src/app/web/run-prefs'

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

/**
 * The header's Run control (issue #378).
 *
 * Live evaluation replaces the output as the student types, and until this the
 * only sign of it was the output changing. So the control says "Autorun" while
 * it is on and animates while a run is coming or going -- and, because the
 * animation is the whole indication, these check the state it is driven from
 * rather than trusting the CSS.
 */
describe('IDE run control', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    await fs.saveFile('hello.scm', '(display "hi")')
    setLiveEvaluation(true)
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
    setLiveEvaluation(true)
    document.body.innerHTML = ''
  })

  async function mountIde() {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: 'Open hello.scm' }).click()
    await flushPromises()
    return wrapper
  }

  async function type(src: string) {
    const area = getByRole(document.body, 'textbox', { name: 'Source code' })
    ;(area as HTMLTextAreaElement).value = src
    area.dispatchEvent(new Event('input'))
    await flushPromises()
  }

  /** The pill around both halves of the control, which is what animates. */
  function control(): HTMLElement {
    const el = document.querySelector('.run-group')
    if (el === null) throw new Error('no run control in the header')
    return el as HTMLElement
  }

  /** The half holding the caret, which opens the menu. */
  function caret(): HTMLElement {
    const name = liveEvaluation.value ? 'Autorun options' : 'Run options'
    return getByRole(document.body, 'button', { name })
  }

  /**
   * Clicks the caret as a mouse does -- mousedown included, because that is
   * what an open menu closes itself on.
   *
   * The wait is not padding: PopupMenu defers registering that listener by a
   * timeout, so without letting one elapse the menu never closes on an outside
   * click and this would pass whatever the caret did.
   */
  async function clickCaret() {
    caret().dispatchEvent(new MouseEvent('mousedown', { bubbles: true }))
    caret().click()
    await flushPromises()
    if (vi.isFakeTimers()) {
      await vi.advanceTimersByTimeAsync(1)
    } else {
      await new Promise((resolve) => setTimeout(resolve, 0))
    }
    await flushPromises()
  }

  /** Opens the control's own menu, from the half holding the caret. */
  async function openControlMenu() {
    await clickCaret()
    return getByRole(document.body, 'menu')
  }

  /** A run that never finishes, so the control can be caught mid-run. */
  function neverEndingRun(id: string): DisplayRequest {
    return { id, tracing: false, done: new Promise(() => undefined) }
  }

  test('says "Autorun" while live evaluation is on, and "Run" while it is off', async () => {
    const wrapper = await mountIde()
    try {
      expect(control().textContent).toContain('Autorun')

      setLiveEvaluation(false)
      await flushPromises()
      expect(control().textContent).not.toContain('Autorun')
      expect(control().textContent).toContain('Run')
    } finally {
      wrapper.unmount()
    }
  })

  test('its menu turns live evaluation off and on again', async () => {
    const wrapper = await mountIde()
    try {
      let menu = await openControlMenu()
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Live Evaluation' }),
      ).toHaveAttribute('aria-checked', 'true')

      getByRole(menu, 'menuitemcheckbox', { name: 'Live Evaluation' }).click()
      await flushPromises()
      expect(liveEvaluation.value).toBe(false)
      expect(control().textContent).not.toContain('Autorun')

      menu = await openControlMenu()
      getByRole(menu, 'menuitemcheckbox', { name: 'Live Evaluation' }).click()
      await flushPromises()
      expect(liveEvaluation.value).toBe(true)
      expect(control().textContent).toContain('Autorun')
    } finally {
      wrapper.unmount()
    }
  })

  test('animates while a run is coming, and stops once it has been', async () => {
    const wrapper = await mountIde()
    vi.spyOn(Scamper.getInstance(), 'execute').mockResolvedValue(null)
    try {
      expect(control().className).not.toContain('run-group--pending')

      vi.useFakeTimers()
      await type('(+ 1 2)')
      expect(control().className).toContain('run-group--pending')

      // The run happens and finishes, leaving nothing to report.
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
      await flushPromises()
      expect(control().className).not.toContain('run-group--pending')
      expect(control().className).not.toContain('run-group--running')
    } finally {
      wrapper.unmount()
    }
  })

  test('animates a live run that is still going, and leaves Autorun in place', async () => {
    const wrapper = await mountIde()
    vi.spyOn(Scamper.getInstance(), 'execute').mockImplementation(() =>
      Promise.resolve(neverEndingRun('live-run')),
    )
    try {
      vi.useFakeTimers()
      await type('(+ 1 2)')
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
      await flushPromises()

      expect(control().className).toContain('run-group--running')
      // Swapping in Stop on every pause in typing would make the toolbar
      // flicker; the stripe is what says a live run is going.
      expect(queryByRole(document.body, 'button', { name: 'Autorun' })).not.toBe(
        null,
      )
      expect(queryByRole(document.body, 'button', { name: 'Stop' })).toBe(null)
    } finally {
      wrapper.unmount()
    }
  })

  test('leaves a manual run to the Stop button, unanimated', async () => {
    const wrapper = await mountIde()
    vi.spyOn(Scamper.getInstance(), 'execute').mockImplementation(() =>
      Promise.resolve(neverEndingRun('manual-run')),
    )
    try {
      getByRole(document.body, 'button', { name: 'Autorun' }).click()
      await flushPromises()

      expect(getByRole(document.body, 'button', { name: 'Stop' })).toBeTruthy()
      // The user started this one, so nothing has to announce that it is going.
      expect(control().className).not.toContain('run-group--running')
    } finally {
      wrapper.unmount()
    }
  })

  test('keeps Stop while the user types during a manual run', async () => {
    const wrapper = await mountIde()
    vi.spyOn(Scamper.getInstance(), 'execute').mockImplementation(() =>
      Promise.resolve(neverEndingRun('manual-run')),
    )
    try {
      getByRole(document.body, 'button', { name: 'Autorun' }).click()
      await flushPromises()
      expect(getByRole(document.body, 'button', { name: 'Stop' })).toBeTruthy()

      // An edit schedules a live run, which is not a reason to take away the
      // only way to stop the one already going.
      await type('(+ 1 2)')
      expect(getByRole(document.body, 'button', { name: 'Stop' })).toBeTruthy()
    } finally {
      wrapper.unmount()
    }
  })

  test('its menu can stop a live run, which the Autorun half no longer does', async () => {
    const wrapper = await mountIde()
    const scamper = Scamper.getInstance()
    vi.spyOn(scamper, 'execute').mockImplementation(() =>
      Promise.resolve(neverEndingRun('live-run')),
    )
    const cancel = vi.spyOn(scamper, 'cancel').mockImplementation(() => undefined)
    try {
      vi.useFakeTimers()
      await type('(+ 1 2)')
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
      await flushPromises()

      const menu = await openControlMenu()
      const stop = getByRole(menu, 'menuitem', { name: 'Stop' })
      expect(stop).not.toHaveAttribute('aria-disabled', 'true')
      stop.click()
      await flushPromises()
      expect(cancel).toHaveBeenCalledWith('live-run')
    } finally {
      wrapper.unmount()
    }
  })

  /**
   * The menu closes itself on a click anywhere outside, so without care the
   * caret's own click would close it and reopen it in the same gesture.
   */
  test('the caret closes the menu it opened', async () => {
    const wrapper = await mountIde()
    try {
      await openControlMenu()
      expect(caret()).toHaveAttribute('aria-expanded', 'true')

      await clickCaret()
      expect(queryByRole(document.body, 'menu')).toBe(null)
      expect(caret()).toHaveAttribute('aria-expanded', 'false')
    } finally {
      wrapper.unmount()
    }
  })

  test('offers no Stop while nothing is running', async () => {
    const wrapper = await mountIde()
    try {
      const menu = await openControlMenu()
      expect(getByRole(menu, 'menuitem', { name: 'Stop' })).toHaveAttribute(
        'aria-disabled',
        'true',
      )
    } finally {
      wrapper.unmount()
    }
  })
})
