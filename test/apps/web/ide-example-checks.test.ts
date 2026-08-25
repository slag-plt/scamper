import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { mockEditorHandle } from '../../stubs/mock-editor-handle'
import { initialize } from '../../../src/scamper'
import {
  checkExamples,
  setCheckExamples,
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

const withExample = (expected: string) =>
  [
    ';;; (fact n) -> number?',
    ';;;   n : number?',
    ';;; Returns n factorial.',
    `;;; @example (fact 5) -> ${expected}`,
    '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))',
  ].join('\n')

/**
 * Checking `@example` lines as the IDE wires it (issue #374): that a run
 * reaching its end is what starts the checks, that the marks reach the editor,
 * and that the Run menu's toggle governs them. What a check *decides* is
 * example-checks.test.ts's business.
 */
describe('IDE example checks', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    await fs.saveFile('good.scm', withExample('120'))
    await fs.saveFile('bad.scm', withExample('121'))
    await fs.saveFile('plain.scm', '(display "hi")')
    // Both are persisted by design, so start from the defaults rather than
    // from whatever the last test left.
    setCheckExamples(true)
    setLiveEvaluation(true)
  })

  afterEach(() => {
    vi.restoreAllMocks()
    setCheckExamples(true)
    setLiveEvaluation(true)
    document.body.innerHTML = ''
  })

  /** Mounts the IDE with `open` loaded, which also runs it. */
  async function mountIde(open: string) {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: `Open ${open}` }).click()
    await flushPromises()
    return wrapper
  }

  /** Opens the Run menu, where checking is turned on and off. */
  async function openRunMenu() {
    const title = getByRole(document.body, 'menuitem', { name: 'Run' })
    if (title.getAttribute('aria-expanded') !== 'true') {
      title.click()
      await flushPromises()
    }
    return getByRole(document.body, 'menu')
  }

  /** Waits for the checks the run kicked off to reach the editor. */
  async function settle() {
    for (let i = 0; i < 20; i++) {
      await flushPromises()
      if (mockEditorHandle.exampleMarks.length > 0) break
    }
    return mockEditorHandle.exampleMarks
  }

  test('marks an example the file agrees with once it has run', async () => {
    const wrapper = await mountIde('good.scm')
    try {
      const marks = await settle()
      expect(marks).toHaveLength(1)
      expect(marks[0].status).toBe('pass')
      expect(marks[0].range.begin.line).toBe(4)
    } finally {
      wrapper.unmount()
    }
  })

  test('marks an example the file disagrees with', async () => {
    const wrapper = await mountIde('bad.scm')
    try {
      const marks = await settle()
      expect(marks[0].status).toBe('fail')
      expect(marks[0].actual).toBe(120)
    } finally {
      wrapper.unmount()
    }
  })

  test('marks nothing in a file with no examples', async () => {
    const wrapper = await mountIde('plain.scm')
    try {
      await flushPromises()
      await flushPromises()
      expect(mockEditorHandle.exampleMarks).toStrictEqual([])
    } finally {
      wrapper.unmount()
    }
  })

  test('checks nothing while Run > Check Examples is off', async () => {
    setCheckExamples(false)
    const wrapper = await mountIde('good.scm')
    try {
      await flushPromises()
      await flushPromises()
      expect(mockEditorHandle.exampleMarks).toStrictEqual([])
    } finally {
      wrapper.unmount()
    }
  })

  test('Run > Check Examples ticks the setting, and flips it', async () => {
    const wrapper = await mountIde('good.scm')
    try {
      let menu = await openRunMenu()
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Check Examples' }),
      ).toHaveAttribute('aria-checked', 'true')

      getByRole(menu, 'menuitemcheckbox', { name: 'Check Examples' }).click()
      await flushPromises()
      expect(checkExamples.value).toBe(false)
      // Turning it off takes the marks away rather than leaving the last
      // sweep's on screen.
      expect(mockEditorHandle.exampleMarks).toStrictEqual([])

      menu = await openRunMenu()
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Check Examples' }),
      ).toHaveAttribute('aria-checked', 'false')
    } finally {
      wrapper.unmount()
    }
  })

  test('switching files clears the marks the last one left', async () => {
    const wrapper = await mountIde('good.scm')
    try {
      expect(await settle()).toHaveLength(1)

      getByRole(document.body, 'button', { name: 'Open plain.scm' }).click()
      await flushPromises()
      expect(mockEditorHandle.exampleMarks).toStrictEqual([])
    } finally {
      wrapper.unmount()
    }
  })
})
