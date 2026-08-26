import { flushPromises, mount } from '@vue/test-utils'
import { findByRole, getByRole, queryByRole } from '@testing-library/dom'
import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'
import { EditorView } from '@codemirror/view'
import IdeApp from '../../../src/app/web/components/IdeApp.vue'
import * as FS from '../../../src/fs'
import { MockFileSystem } from '../../stubs/mock-file-system'
import { mockEditorHandle } from '../../stubs/mock-editor-handle'
import { setFileView } from '../../../src/app/web/view-prefs'
import { initialize } from '../../../src/scamper'
import '../../../src/app/web/renderers'

vi.mock('../../../src/app/web/single-instance', () => ({
  acquireLock: vi.fn(() => Promise.resolve(true)),
  releaseLock: vi.fn(),
  holdsLock: vi.fn(() => true),
}))

vi.mock(
  '../../../src/app/web/components/CodeMirrorEditor.vue',
  () => import('../../stubs/MockCodeMirrorEditor.vue'),
)

await initialize()

// The notebook view: the same file, shown as its forms with what each one
// printed underneath (#410).
describe('IDE notebook view', () => {
  let fs: MockFileSystem

  beforeEach(() => {
    fs = new MockFileSystem()
    FS.setBackend(FS.localBackend(fs))
    setFileView('source')
  })

  afterEach(() => {
    setFileView('source')
    vi.restoreAllMocks()
    document.body.innerHTML = ''
  })

  async function mountIde(src: string, name = 'a.scm') {
    await fs.saveFile(name, src)
    const wrapper = mount(IdeApp, { attachTo: document.body })
    await findByRole(document.body, 'button', { name: 'Create file' })
    await flushPromises()
    getByRole(document.body, 'button', { name: `Open ${name}` }).click()
    await flushPromises()
    return wrapper
  }

  function toggle(): HTMLElement {
    return getByRole(document.body, 'button', { name: 'Notebook view' })
  }

  async function showNotebook() {
    toggle().click()
    await flushPromises()
    await flushPromises()
  }

  function notebook(): HTMLElement | null {
    return document.querySelector<HTMLElement>('.notebook')
  }

  function cells(): HTMLElement[] {
    return [...document.querySelectorAll<HTMLElement>('.notebook-cell')]
  }

  /** What the file now holds, as the editor the notebook writes through has it. */
  function docText(): string {
    return mockEditorHandle.adapter?.getDoc() ?? ''
  }

  /** Runs the file and waits for its output to arrive. */
  async function run() {
    const button =
      queryByRole(document.body, 'button', { name: 'Run' }) ??
      getByRole(document.body, 'button', { name: 'Autorun' })
    button.click()
    await flushPromises()
    // Output is published a frame at a time, as the output pane's is, so a
    // frame has to pass before it is on screen.
    await nextFrame()
    await flushPromises()
  }

  test('the toggle swaps the source view for the notebook', async () => {
    const wrapper = await mountIde('(define x 5)')
    try {
      expect(notebook()).toBeNull()
      await showNotebook()
      expect(notebook()).not.toBeNull()
      // The notebook is the output, so a pane showing the same run beside it
      // would be the run twice.
      expect(document.querySelector('[data-panel="output"]')).toBeNull()
      await showNotebook()
      expect(notebook()).toBeNull()
      expect(document.querySelector('[data-panel="output"]')).not.toBeNull()
    } finally {
      wrapper.unmount()
    }
  })

  test('the editor is still there behind it', async () => {
    // Unmounting it would take the document, the undo history and the handle
    // the rest of the IDE talks through with it.
    const wrapper = await mountIde('(define x 5)')
    try {
      await showNotebook()
      // Hidden rather than gone, which is why it is not found by role.
      expect(
        queryByRole(document.body, 'textbox', { name: 'Source code' }),
      ).toBeNull()
      expect(
        document.querySelector('textarea[aria-label="Source code"]'),
      ).not.toBeNull()
      expect(docText()).toBe('(define x 5)')
    } finally {
      wrapper.unmount()
    }
  })

  test('each form is a cell', async () => {
    const wrapper = await mountIde('(define x 5)\n\n(define y 6)\n\n(+ x y)')
    try {
      await showNotebook()
      expect(cells()).toHaveLength(3)
      expect(cells()[0].textContent).toContain('(define x 5)')
      expect(cells()[2].textContent).toContain('(+ x y)')
    } finally {
      wrapper.unmount()
    }
  })

  test('comments between forms are shown as Markdown', async () => {
    const wrapper = await mountIde(
      '; # Part One\n; Squaring things.\n\n(define x 5)',
    )
    try {
      await showNotebook()
      const prose = document.querySelector('.notebook-prose')
      expect(prose).not.toBeNull()
      // Rendered, not shown as comment lines.
      expect(prose?.querySelector('h1')?.textContent).toBe('Part One')
      expect(prose?.textContent).not.toContain(';')
    } finally {
      wrapper.unmount()
    }
  })

  test('a docstring stays with the function it documents', async () => {
    const wrapper = await mountIde(
      ';;; (sq x) -> number?\n;;; x: number?\n;;; Squares x.\n(define sq (lambda (x) (* x x)))',
    )
    try {
      await showNotebook()
      expect(cells()).toHaveLength(1)
      expect(document.querySelector('.notebook-prose')).toBeNull()
      expect(cells()[0].textContent).toContain('Squares x.')
    } finally {
      wrapper.unmount()
    }
  })

  test('a run puts each form’s output under it', async () => {
    const wrapper = await mountIde('(display 1)\n\n(display 2)')
    try {
      await showNotebook()
      await run()
      const output = cells().map((cell) =>
        cell.querySelector('.cell-output')?.textContent?.trim() ?? '',
      )
      expect(output).toEqual(['1', '2'])
    } finally {
      wrapper.unmount()
    }
  })

  test('a form that prints nothing has nothing under it', async () => {
    const wrapper = await mountIde('(define x 5)\n\n(display x)')
    try {
      await showNotebook()
      await run()
      expect(cells()[0].querySelector('.cell-output')).toBeNull()
      expect(cells()[1].querySelector('.cell-output')?.textContent).toContain(
        '5',
      )
    } finally {
      wrapper.unmount()
    }
  })

  // The notebook is a view of the document, so editing a cell is editing the
  // file -- which is what makes saving, history and live evaluation work
  // without any of them knowing the notebook exists.
  test('editing a cell writes through to the file', async () => {
    const wrapper = await mountIde('(define x 5)\n\n(define y 6)')
    try {
      await showNotebook()
      typeInCell(0, ' ; five')
      await flushPromises()
      expect(docText()).toBe('(define x 5) ; five\n\n(define y 6)')
    } finally {
      wrapper.unmount()
    }
  })

  test('the file keeps its own spacing', async () => {
    // Nothing is ever reassembled from cells, so whatever spacing the author
    // left is still there afterwards.
    const src = '(define x 5)\n\n\n\n; a note\n(define y 6)'
    const wrapper = await mountIde(src)
    try {
      await showNotebook()
      expect(docText()).toBe(src)
    } finally {
      wrapper.unmount()
    }
  })

  test('a prose cell opens for editing when it is clicked', async () => {
    const wrapper = await mountIde('; # A heading\n\n(define x 5)')
    try {
      await showNotebook()
      const prose = document.querySelector<HTMLElement>('.notebook-prose')
      expect(prose).not.toBeNull()
      prose?.click()
      await flushPromises()
      // The rendering gives way to an editor holding the Markdown itself,
      // without the comment markers the file keeps it in.
      expect(document.querySelector('.notebook-prose')).toBeNull()
      const editor = cellViews()[0]
      expect(editor.state.doc.toString()).toBe('# A heading')
    } finally {
      wrapper.unmount()
    }
  })

  test('a file that is not a program cannot be shown as one', async () => {
    const wrapper = await mountIde('plain text', 'notes.txt')
    try {
      expect(toggle()).toBeDisabled()
    } finally {
      wrapper.unmount()
    }
  })
})

/** Resolves once a frame has been drawn. */
function nextFrame(): Promise<void> {
  return new Promise((resolve) => {
    requestAnimationFrame(() => {
      resolve()
    })
  })
}

/**
 * Types `text` at the end of the cell at `index`, through CodeMirror itself so
 * the edit travels the path a person's would.
 */
function typeInCell(index: number, text: string): void {
  const view = cellViews()[index]
  view.dispatch({ changes: { from: view.state.doc.length, insert: text } })
}

/** The CodeMirror views the notebook's cells are made of. */
function cellViews(): EditorView[] {
  return [...document.querySelectorAll<HTMLElement>('.notebook-cell .cm-editor')].map(
    (dom) => {
      const view = EditorView.findFromDOM(dom)
      if (view === null) throw new Error('cell has no editor')
      return view
    },
  )
}
