import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { ensureSyntaxTree } from '@codemirror/language'
import { parser } from '../../../src/scheme/generated/parser.js'
import { ScamperSupport } from '../../../src/app/web/codemirror/extensions/language'
import { mkFreshEditorState } from '../../../src/app/web/codemirror/codemirror'
import { scamperMode } from '../../../src/app/web/codemirror/modes'
import { initialize } from '../../../src/scamper'
import {
  cursorStatus,
  dedupeCursorStatus,
  formPathAt,
  type CursorStatus,
} from '../../../src/app/web/codemirror/enclosing-form'

// mkFreshEditorState installs QueryExtension, whose state field reaches the
// Scamper singleton; initialize it before building a real editor state.
await initialize()

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

  test('vector literal', () => {
    expect(pathAtCursor('(define v [1 2 3|0])')).toEqual([
      'define',
      'vector',
      'number',
    ])
  })

  test('map literal', () => {
    expect(pathAtCursor('(define m {"a" 1|0})')).toEqual([
      'define',
      'map literal',
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

describe('dedupeCursorStatus', () => {
  test('notifies only when the status key changes', () => {
    const seen: CursorStatus[] = []
    const notify = dedupeCursorStatus((s) => seen.push(s))
    notify({ line: 1, column: 1, path: [] })
    notify({ line: 1, column: 1, path: [] }) // identical -> skipped
    notify({ line: 1, column: 2, path: [] }) // column changed
    notify({ line: 1, column: 2, path: ['define'] }) // path changed
    notify({ line: 2, column: 2, path: ['define'] }) // line changed
    expect(seen).toEqual([
      { line: 1, column: 1, path: [] },
      { line: 1, column: 2, path: [] },
      { line: 1, column: 2, path: ['define'] },
      { line: 2, column: 2, path: ['define'] },
    ])
  })

  test('a longer path at the same line/column still notifies', () => {
    const seen: CursorStatus[] = []
    const notify = dedupeCursorStatus((s) => seen.push(s))
    notify({ line: 1, column: 2, path: ['a'] })
    notify({ line: 1, column: 2, path: ['a', 'b'] })
    expect(seen).toHaveLength(2)
  })

  test('each deduper keeps independent memory', () => {
    const a: CursorStatus[] = []
    const b: CursorStatus[] = []
    const na = dedupeCursorStatus((s) => a.push(s))
    const nb = dedupeCursorStatus((s) => b.push(s))
    na({ line: 1, column: 1, path: [] })
    nb({ line: 1, column: 1, path: [] })
    expect(a).toHaveLength(1)
    expect(b).toHaveLength(1)
  })
})

describe('onCursorChange wiring (codemirror.ts)', () => {
  // A real EditorView exercises the update-listener + dedup path end-to-end
  // (jsdom is fine for dispatch; see query-decorations.test.ts).
  function mountEditor(doc: string) {
    const calls: CursorStatus[] = []
    const view = new EditorView({
      state: mkFreshEditorState(doc, {
        dirtyAction: () => {
          /* empty */
        },
        onCursorChange: (s) => calls.push(s),
        isReadOnly: false,
        mode: scamperMode,
      }),
      parent: document.createElement('div'),
    })
    return { view, calls }
  }

  test('notifies with 1-based line/column when the cursor moves', () => {
    const doc = '(define x\n  10)'
    const { view, calls } = mountEditor(doc)
    view.dispatch({ selection: { anchor: doc.indexOf('10') + 1 } })
    expect(calls.at(-1)).toMatchObject({ line: 2, column: 4 })
    view.destroy()
  })

  test('does not re-notify when the cursor stays put', () => {
    const doc = '(define x 10)'
    const { view, calls } = mountEditor(doc)
    view.dispatch({ selection: { anchor: 5 } })
    const n = calls.length
    view.dispatch({ selection: { anchor: 5 } }) // same position -> deduped
    expect(calls).toHaveLength(n)
    view.destroy()
  })
})
