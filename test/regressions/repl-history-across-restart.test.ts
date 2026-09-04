import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { EditorView } from '@codemirror/view'
import IdeApp from '../../src/app/web/components/IdeApp.vue'
import * as FS from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { initialize } from '../../src/scamper'
import '../../src/app/web/renderers'
import { query, required } from '../dom'

vi.mock('../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../stubs/MockCodeMirrorEditor.vue'),
)

await initialize()

// Regression test for #458: Restart threw away the up-arrow history along with
// the transcript, so the commands typed before it could not be recalled. The
// transcript is meant to go -- that is what Restart is for -- but the history
// is a record of what was typed, as a shell's is, and outlives the session.

describe('#458: the REPL history survives a restart', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** The REPL's prompt, which is the cell below the transcript. */
  function prompt(): EditorView {
    const el = query(
      required(
        document.querySelector('[data-panel="repl"]'),
        'the REPL window',
      ),
      '.repl-prompt .cm-editor',
    )
    return required(EditorView.findFromDOM(el), 'the prompt editor')
  }

  /** Presses `key` in the prompt; jsdom has no input method to type with. */
  function pressInPrompt(key: string) {
    prompt().contentDOM.dispatchEvent(
      new KeyboardEvent('keydown', { key, bubbles: true }),
    )
  }

  test('Up recalls what was typed before the restart', async () => {
    await fs.saveFile('a.scm', '(define sq (lambda (n) (* n n)))')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    try {
      await findByRole(document.body, 'button', { name: 'Create file' })
      await flushPromises()
      getByRole(document.body, 'button', { name: 'Open a.scm' }).click()
      await flushPromises()
      getByRole(document.body, 'button', { name: 'Open a REPL' }).click()
      await flushPromises()
      await flushPromises()

      prompt().dispatch({ changes: { from: 0, insert: '(sq 7)' } })
      await flushPromises()
      pressInPrompt('Enter')
      await flushPromises()
      await flushPromises()
      expect(
        document.querySelector('[data-panel="repl"]')?.textContent,
      ).toContain('49')

      getByRole(document.body, 'button', { name: /Restart/ }).click()
      await flushPromises()
      await flushPromises()

      pressInPrompt('ArrowUp')
      expect(prompt().state.doc.toString()).toBe('(sq 7)')
    } finally {
      wrapper.unmount()
    }
  })
})
