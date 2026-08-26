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
    // Turning the view on runs the file, and output is published a frame at a
    // time, as the output pane's is.
    await nextFrame()
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

  // A notebook of empty cells says nothing about the program, and the output
  // the last run produced went to a pane this view does not put up.
  test('turning the view on runs the file', async () => {
    const wrapper = await mountIde('(display "hello")')
    try {
      await showNotebook()
      expect(cells()[0].querySelector('.cell-output')?.textContent).toContain(
        'hello',
      )
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

  // A query is shown inline in the source, which the notebook is not showing.
  // Greyed out rather than left to do nothing when pressed.
  test('querying a value is unavailable in the notebook', async () => {
    const wrapper = await mountIde('(define x 5)')
    try {
      expect(
        getByRole(document.body, 'button', { name: 'Query value' }),
      ).not.toBeDisabled()
      await showNotebook()
      expect(
        getByRole(document.body, 'button', { name: 'Query value' }),
      ).toBeDisabled()
    } finally {
      wrapper.unmount()
    }
  })

  test('the arrows move between cells at their edges', async () => {
    const wrapper = await mountIde('(define x 5)\n\n(define y 6)')
    try {
      await showNotebook()
      const [first, second] = cellViews()
      first.focus()
      first.dispatch({ selection: { anchor: first.state.doc.length } })
      await flushPromises()
      first.contentDOM.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true }),
      )
      await flushPromises()
      expect(second.hasFocus).toBe(true)
    } finally {
      wrapper.unmount()
    }
  })

  // Adding a cell in the middle is where this went wrong: the mounted cells
  // are collected in the order they mount, so the one just added was last in
  // the list however far up the notebook it appeared -- and the caret went to
  // whichever cell happened to hold that position, which is where the next
  // thing typed went too.
  describe('adding a cell in the middle', () => {
    const FILE = '(define a 1)\n\n(define b 2)\n\n(define c 3)'

    /** Presses one of the buttons in the seam below cell `index`. */
    function addBelow(index: number, kind: 'Code' | 'Text') {
      const gap = document.querySelectorAll('.notebook-gap')[index]
      const button = [...gap.querySelectorAll('button')].find((b) =>
        b.textContent?.includes(kind),
      )
      button?.click()
    }

    test('the caret goes to the new cell, and so does what is typed', async () => {
      const wrapper = await mountIde(FILE)
      try {
        await showNotebook()
        addBelow(0, 'Code')
        await flushPromises()
        await flushPromises()

        const views = cellViews()
        expect(views).toHaveLength(4)
        expect(views.findIndex((v) => v.hasFocus)).toBe(1)

        views[1].dispatch({ changes: { from: 0, insert: '(display 9)' } })
        await flushPromises()
        expect(docText()).toBe(
          '(define a 1)\n\n(display 9)\n\n(define b 2)\n\n(define c 3)',
        )
      } finally {
        wrapper.unmount()
      }
    })

    test('a text cell opens ready to be written in', async () => {
      const wrapper = await mountIde(FILE)
      try {
        await showNotebook()
        addBelow(0, 'Text')
        await flushPromises()
        await flushPromises()

        const cell = document.querySelectorAll('.notebook-cell')[1]
        expect(cell.className).toContain('notebook-cell-prose')
        // Its editor, not its rendering: there is nothing to read yet.
        expect(cellViews().findIndex((v) => v.hasFocus)).toBe(1)
      } finally {
        wrapper.unmount()
      }
    })

    test('what is written in a text cell reaches the file as comments', async () => {
      const wrapper = await mountIde(FILE)
      try {
        await showNotebook()
        addBelow(0, 'Text')
        await flushPromises()
        await flushPromises()

        // As it is typed, not when the caret leaves: the file is what gets
        // saved and run, so text that is only in a cell is nowhere yet.
        cellViews()[1].dispatch({ changes: { from: 0, insert: '## A heading' } })
        await flushPromises()
        expect(docText()).toBe(
          '(define a 1)\n\n; ## A heading\n\n(define b 2)\n\n(define c 3)',
        )
      } finally {
        wrapper.unmount()
      }
    })

    test('the arrows still find the cell below', async () => {
      const wrapper = await mountIde(FILE)
      try {
        await showNotebook()
        addBelow(0, 'Code')
        await flushPromises()
        await flushPromises()

        const views = cellViews()
        views[0].focus()
        views[0].dispatch({ selection: { anchor: views[0].state.doc.length } })
        views[0].contentDOM.dispatchEvent(
          new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true }),
        )
        await flushPromises()
        expect(cellViews().findIndex((v) => v.hasFocus)).toBe(1)
      } finally {
        wrapper.unmount()
      }
    })

    test('a text cell left empty puts nothing in the file', async () => {
      const wrapper = await mountIde(FILE)
      try {
        await showNotebook()
        addBelow(0, 'Text')
        await flushPromises()
        await flushPromises()
        // Typed into and emptied again: still no stray comment marker, only
        // the blank line the cell was opened with.
        const prose = cellViews()[1]
        prose.dispatch({ changes: { from: 0, insert: 'oops' } })
        await flushPromises()
        prose.dispatch({ changes: { from: 0, to: 4, insert: '' } })
        await flushPromises()
        expect(docText()).toBe(
          '(define a 1)\n\n\n\n(define b 2)\n\n(define c 3)',
        )
      } finally {
        wrapper.unmount()
      }
    })
  })

  // A cell is never run on its own -- the whole program is -- so Enter adds a
  // line to it, whatever it happens to hold. The REPL's Enter *runs* the
  // entry, and while a cell was being built with that binding it swallowed
  // every Enter pressed in anything that already parsed: a finished form, and
  // any sentence of prose, since English parses as a run of atoms.
  describe('Enter in a cell', () => {
    /** Presses Enter at the end of the cell at `index`. */
    async function pressEnter(index: number) {
      const view = cellViews()[index]
      view.focus()
      view.dispatch({ selection: { anchor: view.state.doc.length } })
      view.contentDOM.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'Enter', bubbles: true }),
      )
      await flushPromises()
      return view
    }

    test('adds a line to a finished form', async () => {
      const wrapper = await mountIde('(define x 5)')
      try {
        await showNotebook()
        const view = await pressEnter(0)
        expect(view.state.doc.toString()).toContain('\n')
        expect(docText()).toContain('(define x 5)\n')
      } finally {
        wrapper.unmount()
      }
    })

    test('adds a line to a form still being written', async () => {
      const wrapper = await mountIde('(define x 5)')
      try {
        await showNotebook()
        const view = cellViews()[0]
        view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: '(+ 1' } })
        await flushPromises()
        await pressEnter(0)
        expect(view.state.doc.toString()).toContain('\n')
      } finally {
        wrapper.unmount()
      }
    })

    test('adds a line to a sentence of prose', async () => {
      const wrapper = await mountIde('; Here we build a square\n\n(define x 5)')
      try {
        await showNotebook()
        document.querySelector<HTMLElement>('.notebook-prose')?.click()
        await flushPromises()
        const view = await pressEnter(0)
        expect(view.state.doc.toString()).toBe('Here we build a square\n')
      } finally {
        wrapper.unmount()
      }
    })

    test('adds a line to a cell with nothing in it', async () => {
      const wrapper = await mountIde('(define x 5)')
      try {
        await showNotebook()
        const view = cellViews()[0]
        view.dispatch({ changes: { from: 0, to: view.state.doc.length } })
        await flushPromises()
        await pressEnter(0)
        expect(view.state.doc.toString()).toBe('\n')
      } finally {
        wrapper.unmount()
      }
    })
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
