import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../src/app/web/components/IdeApp.vue'
import * as FS from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { DEFAULT_LAYOUT } from '../../src/app/web/panel-layout'
import { VERSION } from '../../src/app/web/composables/use-panels'
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
      // the confusion this change is meant to remove.
      expect(panel('editor')?.dataset.slot).toBe('a')
      expect(panel('output')?.dataset.slot).toBe('b')
      // Both reachable at once: a panel tabbed behind another is a tabpanel,
      // and a hidden one leaves the accessibility tree entirely.
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

  test('neither pane carries chrome over it', async () => {
    const wrapper = await mountIde()
    try {
      // A strip exists to choose between tabs, and with one panel per slot
      // there is nothing to choose. Docking the output must not put window
      // furniture over the code, which is the thing #371 set out to remove.
      expect(document.querySelectorAll('[role="tablist"]')).toHaveLength(0)
      expect(
        queryByRole(document.body, 'button', { name: 'Float Output' }),
      ).toBeNull()
      // Float and Dock still reach every panel, from the View menu.
      getByRole(document.body, 'menuitem', { name: 'View' }).click()
      await flushPromises()
      expect(
        queryByRole(getByRole(document.body, 'menu'), 'menuitem', {
          name: 'Float Output',
        }),
      ).not.toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  /** A layout blob with the output floating, stamped `version`. */
  function storeFloatingOutput(version: number) {
    localStorage.setItem(
      'scamper.panels',
      JSON.stringify({
        version,
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
  }

  test('an arrangement from this build still wins', async () => {
    // Someone who floats the output from here on keeps it floating; the new
    // default is a starting point, not something reapplied on every load.
    storeFloatingOutput(VERSION)
    const wrapper = await mountIde()
    try {
      expect(panel('output')?.dataset.placement).toBe('floating')
    } finally {
      wrapper.unmount()
    }
  })

  test('an arrangement from the previous build is retired, not honoured', async () => {
    // A blob is written on the first visit, so without the VERSION bump every
    // existing profile would keep the floating output and only a brand-new one
    // would ever see this change. Retiring those blobs is the point.
    //
    // Literal 1, not VERSION - 1: this has to pin that VERSION *moved off* the
    // number the previous build wrote. Anything relative to VERSION still
    // passes if the bump is reverted.
    storeFloatingOutput(1)
    const wrapper = await mountIde()
    try {
      expect(panel('output')?.dataset.placement).toBe('docked')
    } finally {
      wrapper.unmount()
    }
  })
})
