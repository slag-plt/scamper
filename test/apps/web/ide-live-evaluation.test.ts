import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { mockEditorHandle } from '../../stubs/mock-editor-handle'
import Scamper, { initialize, type DisplayRequest } from '../../../src/scamper'
import {
  DEFAULT_IDLE_MS,
  DEFAULT_RUN_LIMIT_MS,
} from '../../../src/app/web/composables/use-live-evaluation'
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
 * Live evaluation as the IDE wires it (issue #378): that typing reaches it at
 * all, that the Run menu's toggle governs it, and that the file being open is
 * what a scheduled run is about. The timing rules themselves are
 * live-evaluation.test.ts's business.
 */
describe('IDE live evaluation', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    await fs.saveFile('hello.scm', '(display "hi")')
    await fs.saveFile('other.scm', '(display "other")')
    // Persisted by design, so start from the default rather than from whatever
    // the last test left.
    setLiveEvaluation(true)
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
    setLiveEvaluation(true)
    document.body.innerHTML = ''
  })

  /** Mounts the IDE with `hello.scm` open, before any timers are faked. */
  async function mountIde(open = 'hello.scm') {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: `Open ${open}` }).click()
    await flushPromises()
    return wrapper
  }

  /** Types `src` into the stub editor, as a person editing the file would. */
  async function type(src: string) {
    const area = getByRole(document.body, 'textbox', { name: 'Source code' })
    ;(area as HTMLTextAreaElement).value = src
    area.dispatchEvent(new Event('input'))
    await flushPromises()
  }

  /** Opens the Run menu, which is where live evaluation is turned on and off. */
  async function openRunMenu() {
    const title = getByRole(document.body, 'menuitem', { name: 'Run' })
    if (title.getAttribute('aria-expanded') !== 'true') {
      title.click()
      await flushPromises()
    }
    return getByRole(document.body, 'menu')
  }

  /** A run that never finishes, for the watchdog to find still going. */
  function neverEndingRun(id: string): DisplayRequest {
    return { id, tracing: false, done: new Promise(() => undefined) }
  }

  test('runs the file a moment after the user stops typing', async () => {
    const wrapper = await mountIde()
    const execute = vi
      .spyOn(Scamper.getInstance(), 'execute')
      .mockResolvedValue(null)
    try {
      vi.useFakeTimers()
      await type('(+ 1 2)')
      expect(execute).not.toHaveBeenCalled()

      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
      expect(execute).toHaveBeenCalledTimes(1)
      expect(execute.mock.calls[0][0].src).toBe('(+ 1 2)')
    } finally {
      wrapper.unmount()
    }
  })

  test('does not run while the Run menu has it turned off', async () => {
    const wrapper = await mountIde()
    const execute = vi
      .spyOn(Scamper.getInstance(), 'execute')
      .mockResolvedValue(null)
    try {
      setLiveEvaluation(false)
      vi.useFakeTimers()
      await type('(+ 1 2)')
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS * 4)
      expect(execute).not.toHaveBeenCalled()
    } finally {
      wrapper.unmount()
    }
  })

  test('Run > Live Evaluation ticks the setting, and flips it', async () => {
    const wrapper = await mountIde()
    try {
      let menu = await openRunMenu()
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Live Evaluation' }),
      ).toHaveAttribute('aria-checked', 'true')

      getByRole(menu, 'menuitemcheckbox', { name: 'Live Evaluation' }).click()
      await flushPromises()
      expect(liveEvaluation.value).toBe(false)

      menu = await openRunMenu()
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Live Evaluation' }),
      ).toHaveAttribute('aria-checked', 'false')
    } finally {
      wrapper.unmount()
    }
  })

  test('switching files drops a run the last keystroke had scheduled', async () => {
    const wrapper = await mountIde()
    const execute = vi
      .spyOn(Scamper.getInstance(), 'execute')
      .mockResolvedValue(null)
    try {
      vi.useFakeTimers()
      await type('(+ 1 2)')
      // Switching away before the run comes due: it was about the old file.
      getByRole(document.body, 'button', { name: 'Open other.scm' }).click()
      await flushPromises()

      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS * 4)
      // The file arrived at runs, since opening one runs it; the edit to the
      // file left behind never does.
      expect(execute.mock.calls.map((c) => c[0].src)).toEqual([
        '(display "other")',
      ])
    } finally {
      wrapper.unmount()
    }
  })

  test('runs a file as it is opened, so its output is there to read', async () => {
    const wrapper = await mountIde()
    const execute = vi
      .spyOn(Scamper.getInstance(), 'execute')
      .mockResolvedValue(null)
    try {
      getByRole(document.body, 'button', { name: 'Open other.scm' }).click()
      await flushPromises()

      // At once: waiting out the idle delay would leave the output pane empty
      // for as long as the person took to read the code.
      expect(execute).toHaveBeenCalledTimes(1)
      expect(execute.mock.calls[0][0].src).toBe('(display "other")')
    } finally {
      wrapper.unmount()
    }
  })

  test('does not run a file it opens while live evaluation is off', async () => {
    const wrapper = await mountIde()
    const execute = vi
      .spyOn(Scamper.getInstance(), 'execute')
      .mockResolvedValue(null)
    try {
      setLiveEvaluation(false)
      await flushPromises()

      getByRole(document.body, 'button', { name: 'Open other.scm' }).click()
      await flushPromises()
      expect(execute).not.toHaveBeenCalled()
    } finally {
      wrapper.unmount()
    }
  })

  test('an edit made while a trace collects gets its run once it is done', async () => {
    // Stepping is only offered with the cursor inside a statement.
    mockEditorHandle.cursorPath = ['display']
    const wrapper = await mountIde()
    const scamper = Scamper.getInstance()
    const execute = vi.spyOn(scamper, 'execute').mockResolvedValue(null)
    // A trace the test holds open, so an edit can land while it collects.
    let finishTrace = () => undefined as void
    vi.spyOn(scamper, 'traceStatement').mockImplementation(
      () =>
        new Promise((resolve) => {
          finishTrace = () => {
            resolve({ source: '(display "hi")', steps: [], truncated: false })
          }
        }),
    )
    try {
      getByRole(document.body, 'button', { name: 'Step statement' }).click()
      await flushPromises()

      vi.useFakeTimers()
      await type('(+ 1 2)')
      // A run of its own would tear down the one the trace is collecting, so
      // the gate is shut and this edit schedules nothing.
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS * 2)
      expect(execute).not.toHaveBeenCalled()

      finishTrace()
      await flushPromises()
      // The gate is open again, and the edit that was refused still deserves
      // its run -- otherwise the output sits stale until the next keystroke.
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
      expect(execute).toHaveBeenCalledTimes(1)
      expect(execute.mock.calls[0][0].src).toBe('(+ 1 2)')
    } finally {
      mockEditorHandle.cursorPath = []
      wrapper.unmount()
    }
  })

  test('stops a live run that will not end, and says how to run it anyway', async () => {
    const wrapper = await mountIde()
    const scamper = Scamper.getInstance()
    vi.spyOn(scamper, 'execute').mockImplementation(() =>
      Promise.resolve(neverEndingRun('runaway')),
    )
    const cancel = vi.spyOn(scamper, 'cancel').mockImplementation(() => undefined)
    try {
      vi.useFakeTimers()
      await type('(define loop (lambda () (loop))) (loop)')
      await vi.advanceTimersByTimeAsync(DEFAULT_IDLE_MS)
      expect(cancel).not.toHaveBeenCalled()

      await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS)
      expect(cancel).toHaveBeenCalledWith('runaway')

      const results = getByRole(document.body, 'status', { name: 'Results' })
      expect(results.textContent).toContain('live evaluation stopped it')
      expect(results.textContent).toContain('to run it without a time limit')
    } finally {
      wrapper.unmount()
    }
  })

  test('leaves a manual run to take as long as it likes', async () => {
    const wrapper = await mountIde()
    const scamper = Scamper.getInstance()
    vi.spyOn(scamper, 'execute').mockImplementation(() =>
      Promise.resolve(neverEndingRun('manual')),
    )
    const cancel = vi.spyOn(scamper, 'cancel').mockImplementation(() => undefined)
    try {
      // Run, from the menu, with nothing typed since -- so no watchdog exists.
      const menu = await openRunMenu()
      vi.useFakeTimers()
      getByRole(menu, 'menuitem', { name: 'Run' }).click()
      await flushPromises()

      await vi.advanceTimersByTimeAsync(DEFAULT_RUN_LIMIT_MS * 4)
      expect(cancel).not.toHaveBeenCalled()
    } finally {
      wrapper.unmount()
    }
  })
})
