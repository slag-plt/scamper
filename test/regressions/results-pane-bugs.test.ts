import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, fireEvent, getByRole, waitFor } from '@testing-library/dom'
import { nextTick } from 'vue'
import { afterEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../src/app/web/components/IdeApp.vue'
import { initialize } from '../../src/scamper'
import {
  patchSchedulerYieldForTests,
  QUANTUM_WAIT_MS,
  sleep,
} from '../util.js'

patchSchedulerYieldForTests()

vi.mock('../../src/fs/opfs', async () => {
  const { MockFileSystem } = await import('../stubs/mock-file-system')
  return { default: MockFileSystem }
})

vi.mock('../../src/app/web/lockfile', () => ({
  acquireLockFile: vi.fn(() => Promise.resolve(true)),
  releaseLockFile: vi.fn(() => Promise.resolve()),
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

// Regressions for unintended bugs in the IDE results pane. Each `describe`
// block documents one bug and asserts the *desired* (post-fix) behavior.

/**
 * Bug: `IdeApp.executeScamper` schedules a new run without clearing the
 * results pane first. Main-branch `startScamper` called
 * `resultsRef.reset()` before executing.
 */
describe('consecutive runs replace results pane', () => {
  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  test('Run replaces results from the previous run', async () => {
    vi.spyOn(window, 'prompt').mockReturnValue('regression.scm')

    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await flushPromises()
      await findByRole(document.body, 'button', { name: 'Create file' })
      fireEvent.click(
        getByRole(document.body, 'button', { name: 'Create file' }),
      )
      await findByRole(document.body, 'button', { name: 'Open regression.scm' })
      const editor = getByRole(document.body, 'textbox', {
        name: 'Source code',
      })
      await waitFor(() => {
        expect(editor).toHaveValue('; regression.scm')
      })
      fireEvent.input(editor, { target: { value: '1' } })
      await waitFor(() => {
        expect(editor).toHaveValue('1')
      })
      await flushPromises()

      const run = getByRole(document.body, 'button', { name: 'Run' })
      fireEvent.click(run)
      await sleep(QUANTUM_WAIT_MS)
      await flushPromises()
      await nextTick()
      fireEvent.click(run)
      await sleep(QUANTUM_WAIT_MS)
      await flushPromises()
      await nextTick()

      await waitFor(() => {
        expect(
          getByRole(document.body, 'status', { name: 'Results' }).textContent,
        ).toBe('1')
      })
    } finally {
      wrapper.unmount()
    }
  })
})
