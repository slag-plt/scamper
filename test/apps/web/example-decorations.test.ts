import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { describe, expect, test } from 'vitest'
import { Range } from '../../../src/lpm'
import {
  ExampleExtension,
  exampleMarkTitle,
  setExampleDecorations,
} from '../../../src/app/web/codemirror/extensions/examples'
import type { ExampleOutcome } from '../../../src/scheme/examples'

const doc = [
  ';;; (fact n) -> number?',
  ';;; @example (fact 5) -> 120',
  '(define fact 1)',
].join('\n')

/** An outcome anchored on `line`, which is all the marks care about. */
function outcome(line: number, rest: Omit<ExampleOutcome, 'range'>): ExampleOutcome {
  return { range: Range.of(line, 1, 0, line, 1, 0), ...rest }
}

function mkView(): EditorView {
  return new EditorView({
    state: EditorState.create({ doc, extensions: [ExampleExtension] }),
    parent: document.createElement('div'),
  })
}

describe('example decorations', () => {
  test('starts with no marks', () => {
    const view = mkView()
    expect(view.state.field(ExampleExtension).size).toBe(0)
    view.destroy()
  })

  test('draws one mark per outcome, and clears on an empty list', () => {
    const view = mkView()
    setExampleDecorations(view, [outcome(2, { status: 'pass' })])
    expect(view.state.field(ExampleExtension).size).toBe(1)

    setExampleDecorations(view, [])
    expect(view.state.field(ExampleExtension).size).toBe(0)
    view.destroy()
  })

  test('drops an outcome whose line is past the end of the document', () => {
    const view = mkView()
    setExampleDecorations(view, [
      outcome(2, { status: 'pass' }),
      outcome(99, { status: 'fail', actual: 1, expected: 2 }),
    ])
    expect(view.state.field(ExampleExtension).size).toBe(1)
    view.destroy()
  })

  test('keeps the marks across an edit rather than dropping them', () => {
    const view = mkView()
    setExampleDecorations(view, [outcome(2, { status: 'pass' })])
    view.dispatch({ changes: { from: 0, insert: '\n' } })
    expect(view.state.field(ExampleExtension).size).toBe(1)
    view.destroy()
  })
})

describe('exampleMarkTitle', () => {
  test('names both values on a failure', () => {
    const title = exampleMarkTitle(
      outcome(2, { status: 'fail', actual: 121, expected: 120 }),
    )
    expect(title).toContain('120')
    expect(title).toContain('121')
  })

  test('gives the error message on an error', () => {
    const title = exampleMarkTitle(
      outcome(2, { status: 'error', message: 'boom' }),
    )
    expect(title).toBe('boom')
  })

  test('says so on a timeout', () => {
    expect(exampleMarkTitle(outcome(2, { status: 'timeout' }))).toMatch(
      /too long/,
    )
  })
})
