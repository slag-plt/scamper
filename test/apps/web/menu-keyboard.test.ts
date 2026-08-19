import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { initialize } from '../../../src/scamper'

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

// The menu bar is the IDE's whole command surface, and until this it could not
// be driven from a keyboard at all: every title was its own tab stop, nothing
// opened a menu, and the highlight inside one moved silently because the menu
// never held focus for aria-activedescendant to mean anything.
describe('driving the menus from the keyboard', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    localStorage.clear()
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  async function mountIde() {
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    return wrapper
  }

  const bar = () => getByRole(document.body, 'menubar')
  const title = (name: string) =>
    document.querySelector<HTMLElement>(`[data-menu="${name}"]`)
  const menu = () => document.querySelector<HTMLElement>('[role="menu"]')

  /** What the open menu says is highlighted, read the way a reader would. */
  function highlighted(): string | null {
    const id = menu()?.getAttribute('aria-activedescendant')
    return id === null || id === undefined
      ? null
      : (document.getElementById(id)?.textContent.trim() ?? null)
  }

  test('the whole bar is one tab stop, not one per menu', async () => {
    const wrapper = await mountIde()
    try {
      const stops = ['File', 'Edit', 'Go', 'Run', 'View', 'Help']
        .map((t) => title(t)?.tabIndex)
      expect(stops.filter((i) => i === 0)).toHaveLength(1)
      expect(stops.filter((i) => i === -1)).toHaveLength(5)
    } finally {
      wrapper.unmount()
    }
  })

  test('the arrow keys walk the bar without opening anything', async () => {
    const wrapper = await mountIde()
    try {
      title('File')?.focus()
      fireEvent.keyDown(bar(), { key: 'ArrowRight' })
      await flushPromises()
      expect(document.activeElement).toBe(title('Edit'))
      expect(menu()).toBeNull()

      fireEvent.keyDown(bar(), { key: 'ArrowLeft' })
      await flushPromises()
      expect(document.activeElement).toBe(title('File'))

      // Wraps, rather than stopping at the ends.
      fireEvent.keyDown(bar(), { key: 'ArrowLeft' })
      await flushPromises()
      expect(document.activeElement).toBe(title('Help'))

      fireEvent.keyDown(bar(), { key: 'Home' })
      await flushPromises()
      expect(document.activeElement).toBe(title('File'))

      fireEvent.keyDown(bar(), { key: 'End' })
      await flushPromises()
      expect(document.activeElement).toBe(title('Help'))
    } finally {
      wrapper.unmount()
    }
  })

  test('Down opens the focused menu on its first item', async () => {
    const wrapper = await mountIde()
    try {
      title('File')?.focus()
      fireEvent.keyDown(bar(), { key: 'ArrowDown' })
      await flushPromises()

      expect(menu()).not.toBeNull()
      // The menu takes focus, which is what makes aria-activedescendant mean
      // anything to a screen reader.
      expect(document.activeElement).toBe(menu())
      // Exactly one step in, not two: the keypress that opened the menu is
      // stopped rather than left to reach the menu's own handler as well.
      expect(highlighted()).toBe('New File…')
    } finally {
      wrapper.unmount()
    }
  })

  test('a menu opened by pointer highlights nothing yet', async () => {
    const wrapper = await mountIde()
    try {
      title('File')?.click()
      await flushPromises()
      expect(menu()).not.toBeNull()
      expect(highlighted()).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('Escape closes the menu and hands focus back', async () => {
    const wrapper = await mountIde()
    try {
      title('View')?.focus()
      fireEvent.keyDown(bar(), { key: 'ArrowDown' })
      await flushPromises()
      expect(menu()).not.toBeNull()

      fireEvent.keyDown(document, { key: 'Escape' })
      await flushPromises()
      expect(menu()).toBeNull()
      // Not <body>: a keyboard user should be left where they were.
      expect(document.activeElement).toBe(title('View'))
    } finally {
      wrapper.unmount()
    }
  })

  // Found in review. IdeMenuBar keeps one PopupMenu and swaps its props as you
  // slide along the bar, so "the menu changed" is a different event from "the
  // menu mounted" -- and everything seeded at mount was going stale.
  describe('switching menus with one popup instance', () => {
    // Whether the panel actually *moves* needs real boxes -- jsdom measures
    // everything as zero, so both menus clamp to the same margin. That half
    // lives in menu-position.browser.test.ts.

    test('a stale highlight cannot survive into a shorter menu', async () => {
      const wrapper = await mountIde()
      try {
        // View is the longest menu; Go is one of the shortest.
        title('View')?.focus()
        fireEvent.keyDown(bar(), { key: 'ArrowDown' })
        await flushPromises()
        for (let i = 0; i < 8; i++) {
          fireEvent.keyDown(document, { key: 'ArrowDown' })
          await flushPromises()
        }
        expect(highlighted()).not.toBeNull()

        title('Go')?.click()
        await flushPromises()

        // Pointing past the end of the new menu would make Enter read
        // undefined and throw.
        const id = menu()?.getAttribute('aria-activedescendant')
        if (id !== null && id !== undefined) {
          expect(document.getElementById(id)).not.toBeNull()
        }
        // And Enter must not throw.
        expect(() => {
          fireEvent.keyDown(document, { key: 'Enter' })
        }).not.toThrow()
      } finally {
        wrapper.unmount()
      }
    })

    test('the global chords do not fire through an open dialog', async () => {
      const wrapper = await mountIde()
      try {
        getByRole(document.body, 'button', { name: 'Create file' }).click()
        await flushPromises()
        expect(document.querySelector('dialog[open]')).not.toBeNull()

        // Ctrl+Enter here used to run the program behind the prompt.
        const before = document.querySelectorAll('dialog[open]').length
        fireEvent.keyDown(window, { key: 'Enter', ctrlKey: true })
        await flushPromises()
        expect(document.querySelectorAll('dialog[open]').length).toBe(before)
      } finally {
        wrapper.unmount()
      }
    })
  })

  test('there is a skip link, and it is the first thing Tab reaches', async () => {
    const wrapper = await mountIde()
    try {
      const link = document.querySelector<HTMLAnchorElement>('.skip-link')
      expect(link).not.toBeNull()
      expect(link?.getAttribute('href')).toBe('#panel-editor')
      // It has to point at something that exists.
      expect(document.getElementById('panel-editor')).not.toBeNull()

      const stops = [
        ...document.querySelectorAll<HTMLElement>(
          'a[href], button, input, [tabindex]:not([tabindex="-1"])',
        ),
      ]
      expect(stops[0]).toBe(link)
    } finally {
      wrapper.unmount()
    }
  })

  test('the IDE has landmarks to jump between', async () => {
    const wrapper = await mountIde()
    try {
      expect(queryByRole(document.body, 'main')).not.toBeNull()
      expect(
        queryByRole(document.body, 'complementary', { name: 'Files' }),
      ).not.toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})
