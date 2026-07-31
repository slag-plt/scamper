import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import { ensureSyntaxTree } from '@codemirror/language'
import { parser } from '../../../src/scheme/generated/parser.js'
import { ScamperSupport } from '../../../src/app/web/codemirror/extensions/language'
import {
  cursorStatus,
  formPathAt,
} from '../../../src/app/web/codemirror/enclosing-form'

// Cursor position is marked with a `|` placed strictly inside the target token
// (so resolveInner's left-bias lands on that token, not an adjacent one). The
// marker is stripped before parsing and its index is used as the offset.
function pathAtCursor(marked: string): string[] {
  const pos = marked.indexOf('|')
  if (pos < 0) throw new Error('test source needs a | cursor marker')
  const src = marked.slice(0, pos) + marked.slice(pos + 1)
  return formPathAt(parser.parse(src), pos)
}

describe('formPathAt', () => {
  test('breadcrumb from outermost statement down to a leaf atom', () => {
    // Cursor inside the `10` in the cond branch.
    expect(
      pathAtCursor('(define f (lambda (num) (cond [(pos? num) 1|0] [else 20])))'),
    ).toEqual(['define', 'lambda', 'cond', 'number'])
  })

  test('includes application and names the identifier under the cursor', () => {
    // Cursor inside `num` within the application `(pos? num)`.
    expect(
      pathAtCursor('(define f (lambda (num) (cond [(pos? nu|m) 10] [else 20])))'),
    ).toEqual(['define', 'lambda', 'cond', 'application', 'identifier'])
  })

  test('top-level whitespace yields an empty path', () => {
    expect(pathAtCursor('(define x 10)\n|\n(define y 20)')).toEqual([])
  })

  test('names string literals', () => {
    expect(pathAtCursor('(define s "hel|lo")')).toEqual(['define', 'string'])
  })

  test('skips the bare-expression wrapper and names booleans', () => {
    expect(pathAtCursor('(if #|t 10 20)')).toEqual(['if', 'boolean'])
  })

  test('let binding value vs. body', () => {
    expect(pathAtCursor('(let ([count 1|0]) count)')).toEqual(['let', 'number'])
    expect(pathAtCursor('(let ([count 10]) cou|nt)')).toEqual([
      'let',
      'identifier',
    ])
  })

  test('match branch patterns are labeled', () => {
    expect(pathAtCursor('(match lst [(cons h|d tl) hd])')).toEqual([
      'match',
      'pattern',
      'identifier',
    ])
  })

  test('quoted list data', () => {
    expect(pathAtCursor("(define q '(1 2 3|0))")).toEqual([
      'define',
      'quote',
      'application',
      'number',
    ])
  })
})

describe('cursorStatus', () => {
  // Exercises the production path: status read off a real CodeMirror state
  // configured with ScamperSupport, at state.selection.head. In the app the
  // EditorView drives parsing; here ensureSyntaxTree forces it.
  function statusAt(doc: string, anchor: number) {
    const state = EditorState.create({
      doc,
      selection: { anchor },
      extensions: [ScamperSupport()],
    })
    ensureSyntaxTree(state, doc.length, 5000)
    return cursorStatus(state)
  }

  test('reads breadcrumb plus 1-based line/column on a single line', () => {
    const doc = '(define f (lambda (num) (cond [(pos? num) 10] [else 20])))'
    const anchor = doc.indexOf('10') + 1
    const status = statusAt(doc, anchor)
    expect(status.path).toEqual(['define', 'lambda', 'cond', 'number'])
    expect(status.line).toBe(1)
    expect(status.column).toBe(anchor + 1)
  })

  test('computes 1-based line/column across multiple lines', () => {
    const doc = '(define x\n  10)'
    const status = statusAt(doc, doc.indexOf('10') + 1)
    expect(status.line).toBe(2)
    expect(status.column).toBe(4)
    expect(status.path).toEqual(['define', 'number'])
  })
})
