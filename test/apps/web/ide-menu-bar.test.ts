import { flushPromises, mount } from '@vue/test-utils'
import { fireEvent, findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { mockEditorHandle } from '../../stubs/mock-editor-handle'
import { currentTheme, setTheme } from '../../../src/theme'
import {
  DEFAULT_FONT_SIZE,
  editorFontSize,
  editorWordWrap,
  resetZoom,
  setEditorWordWrap,
} from '../../../src/app/web/editor-prefs'
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

// The menu bar is the IDE's full inventory of actions, so these tests are about
// the two things that make a menu bar rather than a list of buttons: picking an
// item reaches the thing it names, and items grey out when they cannot apply.
describe('IDE menu bar', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    // The display preferences persist to localStorage by design, so each test
    // starts from the defaults rather than from whatever the last one left.
    setTheme('light')
    resetZoom()
    setEditorWordWrap(false)
  })

  afterEach(() => {
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  /** Mounts the IDE; opens `open` from the drawer when one is named. */
  async function mountIde(open?: string) {
    await fs.saveFile('hello.scm', '(display "hi")')
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    if (open !== undefined) {
      getByRole(document.body, 'button', { name: `Open ${open}` }).click()
      await flushPromises()
    }
    return wrapper
  }

  /**
   * Opens the `title` menu and returns it. Idempotent, since clicking the title
   * of an already-open menu is what closes it.
   */
  async function openMenu(title: string) {
    const button = getByRole(document.body, 'menuitem', { name: title })
    if (button.getAttribute('aria-expanded') !== 'true') {
      button.click()
      await flushPromises()
    }
    return getByRole(document.body, 'menu')
  }

  /** Picks `label` out of the `title` menu, plain item or toggle. */
  async function pick(title: string, label: string | RegExp) {
    const menu = await openMenu(title)
    const item =
      queryByRole(menu, 'menuitem', { name: label }) ??
      getByRole(menu, 'menuitemcheckbox', { name: label })
    item.click()
    await flushPromises()
  }

  test('offers the standard menus', async () => {
    const wrapper = await mountIde()
    try {
      const bar = getByRole(document.body, 'menubar')
      expect(
        [...bar.querySelectorAll('.menu-title')].map((b) => b.textContent.trim()),
      ).toEqual(['File', 'Edit', 'Go', 'Run', 'View', 'Help'])
    } finally {
      wrapper.unmount()
    }
  })

  test('opens one menu at a time, and closes on a second click', async () => {
    const wrapper = await mountIde()
    try {
      await openMenu('File')
      expect(document.body.querySelectorAll('[role="menu"]').length).toBe(1)

      // Opening another replaces it rather than stacking a second.
      await openMenu('Edit')
      expect(document.body.querySelectorAll('[role="menu"]').length).toBe(1)

      getByRole(document.body, 'menuitem', { name: 'Edit' }).click()
      await flushPromises()
      expect(queryByRole(document.body, 'menu')).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('greys out the file actions until a file is open', async () => {
    const wrapper = await mountIde()
    try {
      const menu = await openMenu('File')
      for (const label of ['Save', 'Rename…', 'Download', 'Delete']) {
        expect(
          getByRole(menu, 'menuitem', { name: label }),
        ).toHaveAttribute('aria-disabled', 'true')
      }
      // The ones that don't need an open file stay live.
      expect(
        getByRole(menu, 'menuitem', { name: 'New File…' }),
      ).not.toHaveAttribute('aria-disabled', 'true')
      expect(
        getByRole(menu, 'menuitem', { name: 'File History…' }),
      ).not.toHaveAttribute('aria-disabled', 'true')
    } finally {
      wrapper.unmount()
    }
  })

  test('enables the file actions once a file is open', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      const menu = await openMenu('File')
      for (const label of ['Save', 'Rename…', 'Download', 'Delete']) {
        expect(
          getByRole(menu, 'menuitem', { name: label }),
        ).not.toHaveAttribute('aria-disabled', 'true')
      }
    } finally {
      wrapper.unmount()
    }
  })

  test('File > Rename drives the same rename as the drawer', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      await pick('File', /^Rename/)
      const prompt = await findByRole(document.body, 'dialog', {
        name: 'Rename file',
      })
      expect(prompt.textContent).toContain('hello.scm')
    } finally {
      wrapper.unmount()
    }
  })

  test('Edit and Go items reach the editor', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      await pick('Edit', 'Format File')
      await pick('Edit', 'Select All')
      await pick('View', 'Fold All')
      expect(mockEditorHandle.commands).toContain('format')
      expect(mockEditorHandle.commands).toContain('selectAll')
      expect(mockEditorHandle.commands).toContain('foldAll')
    } finally {
      wrapper.unmount()
    }
  })

  test('Edit items that need the editor are greyed out with no file open', async () => {
    // The no-file editor state is read-only, which is what the menu reads.
    const wrapper = await mountIde()
    try {
      const menu = await openMenu('Edit')
      for (const label of ['Paste', 'Format File', 'Toggle Comment']) {
        expect(
          getByRole(menu, 'menuitem', { name: label }),
        ).toHaveAttribute('aria-disabled', 'true')
      }
    } finally {
      wrapper.unmount()
    }
  })

  test('View ticks the toggles it reflects, and flips them', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      let menu = await openMenu('View')
      // The drawer starts open and the theme starts light.
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'File Drawer' }),
      ).toHaveAttribute('aria-checked', 'true')
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Dark Theme' }),
      ).toHaveAttribute('aria-checked', 'false')

      await pick('View', 'Dark Theme')
      expect(currentTheme.value).toBe('dark')

      await pick('View', 'File Drawer')
      menu = await openMenu('View')
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'File Drawer' }),
      ).toHaveAttribute('aria-checked', 'false')
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Dark Theme' }),
      ).toHaveAttribute('aria-checked', 'true')
    } finally {
      setTheme('light')
      wrapper.unmount()
    }
  })

  test('Run greys out stepping until a trace is running', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      const menu = await openMenu('Run')
      for (const label of ['Step Once', 'Step Statement', 'Step All', 'Stop']) {
        expect(
          getByRole(menu, 'menuitem', { name: label }),
        ).toHaveAttribute('aria-disabled', 'true')
      }
      expect(getByRole(menu, 'menuitem', { name: 'Run' })).not.toHaveAttribute(
        'aria-disabled',
        'true',
      )
    } finally {
      wrapper.unmount()
    }
  })

  test('Help > Keyboard Shortcuts opens the panel the (?) button owns', async () => {
    const wrapper = await mountIde()
    try {
      expect(
        queryByRole(document.body, 'dialog', { name: 'Keyboard shortcuts' }),
      ).toBeNull()
      await pick('Help', 'Keyboard Shortcuts')
      expect(
        getByRole(document.body, 'dialog', { name: 'Keyboard shortcuts' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('Help > About names the running version', async () => {
    const wrapper = await mountIde()
    try {
      await pick('Help', 'About Scamper')
      const dialog = await findByRole(document.body, 'dialog', {
        name: 'About Scamper',
      })
      expect(dialog.textContent).toContain(APP_VERSION)
      getByRole(dialog, 'button', { name: 'OK' }).click()
      await flushPromises()
    } finally {
      wrapper.unmount()
    }
  })

  test("Help > What's New reopens the patch notes on demand", async () => {
    const wrapper = await mountIde()
    try {
      await pick('Help', "What's New")
      // The modal is the same one a version bump shows by itself.
      expect(
        await findByRole(document.body, 'dialog', { name: /What's New|Scamper/ }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('Go and Edit reach the search commands', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      await pick('Go', /^Go to Line/)
      await pick('Edit', /^Replace/)
      expect(mockEditorHandle.commands).toContain('goToLine')
      expect(mockEditorHandle.commands).toContain('replace')
    } finally {
      wrapper.unmount()
    }
  })

  test('View zooms the editor and remembers the size', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      const before = editorFontSize.value
      await pick('View', 'Zoom In')
      expect(editorFontSize.value).toBeGreaterThan(before)

      await pick('View', 'Reset Zoom')
      expect(editorFontSize.value).toBe(DEFAULT_FONT_SIZE)

      // Reset greys itself out once there is nothing to reset.
      const menu = await openMenu('View')
      expect(
        getByRole(menu, 'menuitem', { name: 'Reset Zoom' }),
      ).toHaveAttribute('aria-disabled', 'true')
    } finally {
      resetZoom()
      wrapper.unmount()
    }
  })

  test('View toggles word wrap', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      expect(editorWordWrap.value).toBe(false)
      await pick('View', 'Word Wrap')
      expect(editorWordWrap.value).toBe(true)

      const menu = await openMenu('View')
      expect(
        getByRole(menu, 'menuitemcheckbox', { name: 'Word Wrap' }),
      ).toHaveAttribute('aria-checked', 'true')
    } finally {
      setEditorWordWrap(false)
      wrapper.unmount()
    }
  })

  test('Run offers Restart only while something is running', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      const menu = await openMenu('Run')
      expect(
        getByRole(menu, 'menuitem', { name: 'Restart' }),
      ).toHaveAttribute('aria-disabled', 'true')
    } finally {
      wrapper.unmount()
    }
  })

  test('the arrow keys walk an open menu, skipping separators', async () => {
    const wrapper = await mountIde('hello.scm')
    try {
      const menu = await openMenu('File')
      const active = () =>
        menu.getAttribute('aria-activedescendant') === null
          ? null
          : document.getElementById(
              menu.getAttribute('aria-activedescendant') ?? '',
            )?.textContent.trim()

      expect(active()).toBeNull()
      fireEvent.keyDown(document, { key: 'ArrowDown' })
      await flushPromises()
      expect(active()).toBe('New File…')

      // Up from the first item wraps to the last rather than stopping.
      fireEvent.keyDown(document, { key: 'ArrowUp' })
      await flushPromises()
      const last = active()
      expect(last).not.toBe('New File…')

      // Enter runs whatever is highlighted, and the menu closes behind it.
      fireEvent.keyDown(document, { key: 'Home' })
      await flushPromises()
      expect(active()).toBe('New File…')
      fireEvent.keyDown(document, { key: 'Enter' })
      await flushPromises()
      expect(queryByRole(document.body, 'menu')).toBeNull()
      expect(
        await findByRole(document.body, 'dialog', { name: 'New file' }),
      ).toBeInTheDocument()
    } finally {
      wrapper.unmount()
    }
  })

  test('Escape closes an open menu', async () => {
    const wrapper = await mountIde()
    try {
      await openMenu('Edit')
      fireEvent.keyDown(document, { key: 'Escape' })
      await flushPromises()
      expect(queryByRole(document.body, 'menu')).toBeNull()
    } finally {
      wrapper.unmount()
    }
  })
})
