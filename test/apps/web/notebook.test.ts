import { describe, expect, test } from 'vitest'
import { useNotebook } from '../../../src/app/web/composables/use-notebook'
import type { EditorAccessor } from '../../../src/app/web/composables/editor-context'
import type { CodeMirrorEditorAdapter } from '../../../src/app/web/composables/codemirror-editor-adapter'

/**
 * A document the notebook can be a view of, with none of the editor around it.
 *
 * The notebook only asks two things of the editor -- what the document says,
 * and to change part of it -- which is the whole of what a test of it needs.
 */
function documentHolding(initial: string) {
  let doc = initial
  let cursor = 0
  const adapter = {
    getDoc: () => doc,
    replaceRange: (from: number, to: number, text: string) => {
      doc = doc.slice(0, from) + text + doc.slice(to)
    },
    setCursor: (idx: number) => {
      cursor = idx
    },
  } as unknown as CodeMirrorEditorAdapter
  return {
    editor: (() => adapter) as EditorAccessor,
    text: () => doc,
    cursor: () => cursor,
  }
}

/** Waits out the notebook's lint delay, and the analysis after it. */
async function settled(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 400))
}

describe('the notebook as a view of a document', () => {
  test('it takes its cells from the document', () => {
    const file = documentHolding('(define x 5)\n\n; a note\n\n(+ x 1)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    expect(notebook.cells.value.map((c) => c.kind)).toEqual([
      'code',
      'prose',
      'code',
    ])
  })

  test('an edit in a cell reaches the document', () => {
    const file = documentHolding('(define x 5)\n\n(define y 6)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.applyChanges(0, [{ from: 12, to: 12, insert: ' ; five' }])
    expect(file.text()).toBe('(define x 5) ; five\n\n(define y 6)')
  })

  // The invariant the whole thing rests on: after an edit the cells below have
  // moved, and the next edit has to land where they now are.
  test('an edit moves the cells below it', () => {
    const file = documentHolding('(define x 5)\n\n(define y 6)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.applyChanges(0, [{ from: 12, to: 12, insert: ' ; five' }])
    notebook.applyChanges(1, [{ from: 12, to: 12, insert: ' ; six' }])
    expect(file.text()).toBe('(define x 5) ; five\n\n(define y 6) ; six')
  })

  test('a cell that shrinks moves them back', () => {
    const file = documentHolding('(define xyz 5)\n\n(define y 6)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    // Deleting "yz" from the first cell.
    notebook.applyChanges(0, [{ from: 9, to: 11, insert: '' }])
    notebook.applyChanges(1, [{ from: 0, to: 0, insert: ';' }])
    expect(file.text()).toBe('(define x 5)\n\n;(define y 6)')
  })

  // So stepping a statement, querying a value and the line and column in the
  // status bar mean the same thing in either view.
  test('the caret in a cell is the caret in the file', () => {
    const file = documentHolding('(define x 5)\n\n(+ x 1)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.noteCursor(1, 3)
    expect(file.cursor()).toBe(17)
  })

  test('the caret in a prose cell reports the cell itself', () => {
    // Markdown and the file's comment lines are not the same text, so a
    // position in one is not a position in the other.
    const file = documentHolding('; a note\n\n(+ 1 2)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.noteCursor(0, 5)
    expect(file.cursor()).toBe(0)
  })

  test('prose is written back as comment lines', () => {
    const file = documentHolding('; old note\n\n(define x 5)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.replaceCell(0, '; # A heading\n;\n; Some text.')
    expect(file.text()).toBe('; # A heading\n;\n; Some text.\n\n(define x 5)')
  })
})

describe('adding and removing cells', () => {
  test('a new cell arrives with a blank line before it', () => {
    const file = documentHolding('(define x 5)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    const at = notebook.insertCell(0, 'code')
    expect(at).toBe(1)
    // Empty until something is typed into it: the file holds nothing for it
    // yet, which is exactly why it has to be remembered separately.
    expect(file.text()).toBe('(define x 5)\n\n')
    notebook.applyChanges(at, [{ from: 0, to: 0, insert: '(+ x 1)' }])
    expect(file.text()).toBe('(define x 5)\n\n(+ x 1)')
  })

  test('a new cell at the top goes above what is there', () => {
    const file = documentHolding('(define x 5)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    const at = notebook.insertCell(-1, 'code')
    expect(at).toBe(0)
    notebook.applyChanges(at, [{ from: 0, to: 0, insert: '(import image)' }])
    expect(file.text()).toBe('(import image)\n\n(define x 5)')
  })

  test('a draft survives a re-split', () => {
    const file = documentHolding('(define x 5)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.insertCell(0, 'code')
    notebook.refresh()
    expect(notebook.cells.value).toHaveLength(2)
    expect(notebook.cells.value[1].isDraft).toBe(true)
  })

  test('removing a cell takes its blank line with it', () => {
    const file = documentHolding('(define x 5)\n\n(define y 6)\n\n(+ x y)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.removeCell(1)
    expect(file.text()).toBe('(define x 5)\n\n(+ x y)')
    expect(notebook.cells.value).toHaveLength(2)
  })

  test('removing the first cell leaves no gap at the top', () => {
    const file = documentHolding('(define x 5)\n\n(+ 1 2)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    notebook.removeCell(0)
    expect(file.text()).toBe('(+ 1 2)')
  })
})

// Red squiggles, as in the source view. The file is analysed whole and the
// results handed out by cell, since a cell read on its own is half-typed and
// leans on the forms above it.
describe('what is wrong in a cell', () => {
  test('a problem is marked in the cell that has it', async () => {
    const file = documentHolding('(define x 5)\n\n(+ x nope)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    await settled()
    expect(notebook.diagnostics.value[0]).toEqual([])
    expect(notebook.diagnostics.value[1]?.length).toBeGreaterThan(0)
    // In the cell's own coordinates, so the cell can underline it.
    const [first] = notebook.diagnostics.value[1]
    expect(first.from).toBeGreaterThanOrEqual(0)
    expect(first.to).toBeLessThanOrEqual('(+ x nope)'.length)
  })

  test('a name defined in another cell is not a problem', async () => {
    const file = documentHolding('(define x 5)\n\n(+ x 1)')
    const notebook = useNotebook(file.editor)
    notebook.refresh()
    await settled()
    expect(notebook.diagnostics.value.flat()).toEqual([])
  })
})
