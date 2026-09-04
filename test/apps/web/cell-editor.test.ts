import { describe, expect, test } from 'vitest'
import { EditorView } from '@codemirror/view'
import {
  isCompleteForm,
  mkCellEditorState,
} from '../../../src/app/web/codemirror/cell-editor'
import { initialize } from '../../../src/scamper'

// Nothing below needs Scamper, but cell-editor.ts reaches scamper.ts (via
// codemirror.ts's query extension), whose module body starts the renderer
// registration. initialize() is what awaits that import; without this call it
// is still in flight when this file's environment is torn down, and the
// rejection that follows fails an otherwise green run (#511).
await initialize()

// What decides whether Enter runs the cell or continues it onto another line
// (#399).
describe('a complete form', () => {
  test('a closed form is complete', () => {
    expect(isCompleteForm('(+ 1 2)')).toBe(true)
    expect(isCompleteForm('(define x 5)')).toBe(true)
    expect(isCompleteForm('42')).toBe(true)
  })

  test('an unclosed form is not', () => {
    expect(isCompleteForm('(+ 1')).toBe(false)
    expect(isCompleteForm('(define f (lambda (x)')).toBe(false)
  })

  test('brackets inside a string or a comment do not count', () => {
    // The reason this is read off the grammar rather than counted by hand.
    expect(isCompleteForm('"(unclosed"')).toBe(true)
    expect(isCompleteForm('(list ")")')).toBe(true)
    expect(isCompleteForm('(+ 1 2) ; (a comment')).toBe(true)
  })

  test('a form spanning several lines is complete once it closes', () => {
    expect(isCompleteForm('(define f\n  (lambda (x)\n    x))')).toBe(true)
    expect(isCompleteForm('(define f\n  (lambda (x)\n    x)')).toBe(false)
  })
})

describe('a cell editor', () => {
  /** Mounts a cell, since a keymap can only be exercised through a view. */
  function mount(doc: string, config = {}) {
    const parent = document.createElement('div')
    document.body.appendChild(parent)
    const view = new EditorView({
      state: mkCellEditorState(doc, config),
      parent,
    })
    return {
      view,
      /** Presses a key as CodeMirror's keymap handler sees it. */
      press: (key: string, shift = false) => {
        view.contentDOM.dispatchEvent(
          new KeyboardEvent('keydown', { key, shiftKey: shift, bubbles: true }),
        )
      },
      done: () => {
        view.destroy()
        parent.remove()
      },
    }
  }

  test('Enter runs a complete entry', () => {
    const submitted: string[] = []
    const cell = mount('(+ 1 2)', {
      onSubmit: (text: string) => submitted.push(text),
    })
    try {
      cell.press('Enter')
      expect(submitted).toEqual(['(+ 1 2)'])
      // The cell is left alone; clearing it is the prompt's business.
      expect(cell.view.state.doc.toString()).toBe('(+ 1 2)')
    } finally {
      cell.done()
    }
  })

  test('Enter continues an unclosed entry instead', () => {
    const submitted: string[] = []
    const cell = mount('(define f (lambda (x)', {
      onSubmit: (text: string) => submitted.push(text),
    })
    try {
      cell.press('Enter')
      expect(submitted).toEqual([])
      expect(cell.view.state.doc.lines).toBe(2)
    } finally {
      cell.done()
    }
  })

  test('Enter on a blank last line runs it anyway', () => {
    // The escape hatch: source the grammar cannot make sense of would
    // otherwise be impossible to submit, and so impossible to get an error
    // message about.
    const submitted: string[] = []
    const cell = mount(')\n', {
      onSubmit: (text: string) => submitted.push(text),
    })
    try {
      cell.press('Enter')
      expect(submitted).toEqual([')\n'])
    } finally {
      cell.done()
    }
  })

  test('Enter in an empty cell does nothing at all', () => {
    const submitted: string[] = []
    const cell = mount('', { onSubmit: (text: string) => submitted.push(text) })
    try {
      cell.press('Enter')
      expect(submitted).toEqual([])
      expect(cell.view.state.doc.toString()).toBe('')
    } finally {
      cell.done()
    }
  })

  test('Shift+Enter always adds a line', () => {
    const submitted: string[] = []
    const cell = mount('(+ 1 2)', {
      onSubmit: (text: string) => submitted.push(text),
    })
    try {
      cell.press('Enter', true)
      expect(submitted).toEqual([])
      expect(cell.view.state.doc.lines).toBe(2)
    } finally {
      cell.done()
    }
  })

  test('a read-only cell cannot be typed into', () => {
    const cell = mount('(+ 1 2)', { isReadOnly: true })
    try {
      expect(cell.view.state.readOnly).toBe(true)
    } finally {
      cell.done()
    }
  })
})
