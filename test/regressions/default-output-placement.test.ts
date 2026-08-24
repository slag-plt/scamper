import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../src/app/web/components/IdeApp.vue'
import * as FS from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { DEFAULT_LAYOUT } from '../../src/app/web/panel-layout'
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

// Regression test for #371: the output window started floating over the code.
// That was deliberate, but it read as something gone wrong to beginners, who
// did not connect the window to the Run they had just pressed. It now starts
// docked beside the code, which is where the IDE put it before the dock
// existed.
//
// This is about the arrangement a *new* user gets. A stored layout still wins,
// so nobody's arrangement is rearranged under them -- pinned below.

describe('#371: the output starts docked beside the code', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    localStorage.clear()
  })

  afterEach(() => {
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

  function panel(id: 'editor' | 'output' | 'trace'): HTMLElement | null {
    return document.querySelector(`[data-panel="${id}"]`)
  }

  test('the default layout docks the output opposite the editor', () => {
    // The unit the IDE reads on a first run, independent of any mounting.
    expect(DEFAULT_LAYOUT.placement.editor).toEqual({
      kind: 'docked',
      slot: 'a',
    })
    expect(DEFAULT_LAYOUT.placement.output).toEqual({
      kind: 'docked',
      slot: 'b',
    })
  })

  test('a first run shows code and output side by side, not overlapping', async () => {
    const wrapper = await mountIde()
    try {
      expect(panel('editor')?.dataset.placement).toBe('docked')
      expect(panel('output')?.dataset.placement).toBe('docked')
      expect(getByRole(document.body, 'region', { name: 'Output' })).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('being docked, the output has no floating title bar to dismiss', async () => {
    const wrapper = await mountIde()
    try {
      // Chrome belongs to a floating frame; a docked one is a pane, so there
      // is no window furniture over the code for a beginner to get lost in.
      const output = panel('output')
      expect(output).not.toBeNull()
      expect(
        queryByRole(output as HTMLElement, 'button', { name: /minimi/i }),
      ).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('they sit in separate slots, so neither is hidden behind the other', async () => {
    const wrapper = await mountIde()
    try {
      // Sharing a slot would stack them as tabs, showing one at a time --
      // which is the confusion this change is meant to remove. Separate slots
      // give each its own single-tab strip, naming the pane.
      const strips = document.querySelectorAll('[role="tablist"]')
      expect(strips).toHaveLength(2)
      strips.forEach((strip) => {
        expect(strip.querySelectorAll('[role="tab"]')).toHaveLength(1)
      })
      expect(
        getByRole(document.body, 'region', { name: 'Source' }),
      ).toBeInTheDocument()
      expect(
        getByRole(document.body, 'region', { name: 'Output' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('a stored arrangement still wins, so nobody is rearranged', async () => {
    // Someone who floated the output before this change keeps it floating.
    localStorage.setItem(
      'scamper.panels',
      JSON.stringify({
        version: 1,
        placement: {
          editor: { kind: 'docked', slot: 'a' },
          output: { kind: 'floating', minimized: false },
          trace: { kind: 'floating', minimized: false },
        },
        geometry: { editor: null, output: null, trace: null },
        recency: ['trace', 'output', 'editor'],
        axis: 'row',
        splitPercent: 62,
      }),
    )
    const wrapper = await mountIde()
    try {
      expect(panel('output')?.dataset.placement).toBe('floating')
    } finally {
      wrapper.unmount()
    }
  })
})
