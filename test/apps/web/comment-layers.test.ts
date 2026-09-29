import { describe, expect, test } from 'vitest'
import {
  EditorSelection,
  EditorState,
  type StateCommand,
} from '@codemirror/state'
import { lineComment } from '@codemirror/commands'
import { ScamperSupport } from '../../../src/app/web/codemirror/extensions/language'
import {
  addCommentLayer,
  removeCommentLayer,
} from '../../../src/app/web/codemirror/extensions/comment'
import { mkParsedState } from './parsed-state'

// Ctrl-; and Ctrl-Shift-; (issue #662). Scamper gives comment depth meaning --
// `;` in a line of code, `;;` between lines, `;;;` a docstring -- so the two
// directions are separate commands rather than one toggle. These go through the
// commands the keymap binds; a StateCommand needs only a state and a dispatch,
// so no view is involved and the suite runs under jsdom.

/** A Scamper editor state with `[from, to)` selected. */
function mkState(doc: string, from = 0, to = from): EditorState {
  return mkParsedState({
    doc,
    selection: EditorSelection.single(from, to),
    extensions: [ScamperSupport()],
  })
}

interface Applied {
  doc: string
  /** What the command returned -- false means the chord falls through. */
  handled: boolean
}

function apply(cmd: StateCommand, state: EditorState): Applied {
  let next = state
  const handled = cmd({
    state,
    dispatch: (tr) => {
      next = tr.state
    },
  })
  return { doc: next.doc.toString(), handled }
}

/** Ctrl-; with the whole document selected. */
function add(doc: string): string {
  return apply(addCommentLayer, mkState(doc, 0, doc.length)).doc
}

/** Ctrl-Shift-; with the whole document selected. */
function remove(doc: string): string {
  return apply(removeCommentLayer, mkState(doc, 0, doc.length)).doc
}

/**
 * Every case the two commands are specified over, as input and what one added
 * layer makes of it. Shared by the behaviour tests and the round-trip ones, so
 * a case cannot be checked in one direction and forgotten in the other.
 */
const cases: [name: string, input: string, added: string][] = [
  ['a line of code', '(+ 1 2)', '; (+ 1 2)'],
  ['a commented line', '; x', ';; x'],
  ['a two-semicolon comment', ';; note', ';;; note'],
  ['a docstring', ';;; doc', ';;;; doc'],
  ['a comment with no space after its marker', ';;foo', ';;;foo'],
  ['an empty line', '', '; '],
  ['a whitespace-only line', '   ', '   ; '],
  ['an indented line', '  (f x)', '  ; (f x)'],
  ['a line with a trailing comment', '(f x) ; sum', '; (f x) ; sum'],
  ['a block at a uniform indent', '  (a)\n  (b)', '  ; (a)\n  ; (b)'],
  [
    'a block at mixed indents',
    '  (a)\n    (b)\n(c)',
    ';   (a)\n;     (b)\n; (c)',
  ],
  ['a block at mixed depths', ';; a\ncode\n;;; d', ';;; a\n; code\n;;;; d'],
  ['a block with a blank line in it', '(a)\n\n(b)', '; (a)\n\n; (b)'],
]

describe('adding a comment layer', () => {
  test.each(cases)('%s: %j gains one marker', (_name, input, added) => {
    expect(add(input)).toBe(added)
  })

  // The bug #662 reports. A toggle can only ever remove a layer from a line
  // that already has one, so the chord a student presses to comment out a
  // docstring quietly turned it back into an ordinary comment.
  test('a docstring deepens rather than being demoted', () => {
    expect(add(';;; doc')).toBe(';;;; doc')
    expect(add(';;; doc')).not.toBe(';; doc')
  })

  /*
   * Why this command is hand-written instead of reusing @codemirror/commands'
   * `lineComment`, which is exactly "comment, do not uncomment": it is a no-op
   * on a line that is already commented -- it only inserts where some selected
   * line has no marker at all -- so it can never deepen `;;` into `;;;`.
   * `removeCommentLayer` has the opposite story: `lineUncomment` removes
   * exactly one layer, so it is reused verbatim.
   */
  test('lineComment cannot deepen a comment, which is why it is not reused', () => {
    expect(apply(lineComment, mkState(';; note', 0, 7))).toEqual({
      doc: ';; note',
      handled: false,
    })
    expect(apply(addCommentLayer, mkState(';; note', 0, 7))).toEqual({
      doc: ';;; note',
      handled: true,
    })
  })

  test('markers land at the shallowest indent in the selection', () => {
    // Column 2, not each line's own indent: the block keeps its shape.
    expect(add('  (a)\n      (b)')).toBe('  ; (a)\n  ;     (b)')
  })

  test('one marker per line, so a mixed-depth block keeps its differences', () => {
    // Deliberately not levelled to the deepest line: levelling cannot be
    // undone, and these two commands are exact inverses.
    expect(add(';; a\ncode\n;;; d')).toBe(';;; a\n; code\n;;;; d')
  })

  test('a bare cursor comments its own line, and keeps its place in the text', () => {
    const state = mkState('(f x)', 3)
    let next = state
    expect(
      addCommentLayer({
        state,
        dispatch: (tr) => {
          next = tr.state
        },
      }),
    ).toBe(true)
    expect(next.doc.toString()).toBe('; (f x)')
    // Two characters were inserted ahead of the cursor, and it moved with them.
    expect(next.selection.main.from).toBe(5)
  })

  test('a blank line in a block is left as a gap, not commented', () => {
    expect(add('(a)\n\n(b)')).toBe('; (a)\n\n; (b)')
    // ...but a blank line on its own is where someone is starting a comment.
    expect(add('')).toBe('; ')
  })
})

describe('removing a comment layer', () => {
  test.each(cases)('%s: %j gives its marker back', (_name, input, added) => {
    expect(remove(added)).toBe(input)
  })

  test('a line with no marker is left alone, and the chord falls through', () => {
    expect(apply(removeCommentLayer, mkState('(f x)', 0, 5))).toEqual({
      doc: '(f x)',
      handled: false,
    })
  })

  test('a trailing comment is not a commented line', () => {
    expect(apply(removeCommentLayer, mkState('(f x) ; sum', 0, 11))).toEqual({
      doc: '(f x) ; sum',
      handled: false,
    })
  })

  test('only the commented lines of a mixed block lose a layer', () => {
    expect(remove(';; a\ncode\n;;; d')).toBe('; a\ncode\n;; d')
  })
})

describe('the two directions are exact inverses', () => {
  test.each(cases)('%s: adding then removing leaves %j', (_name, input) => {
    expect(remove(add(input))).toBe(input)
  })

  // The other way round, for comments as someone would have written them
  // rather than only the ones `add` produces.
  test.each([
    '; x',
    ';; note',
    ';;; doc',
    ';;;; deep',
    ';;;foo',
    '  ;; an indented note',
    '; ',
  ])('removing then adding leaves %j', (commented) => {
    expect(add(remove(commented))).toBe(commented)
  })
})

describe('a file that is not a program', () => {
  // A .txt or .csv buffer has no language and so no `commentTokens`; both
  // chords decline, leaving the key to anything else that wants it (#385).
  const plain = EditorState.create({ doc: 'hello' })

  test('neither command acts where there is no line comment', () => {
    expect(apply(addCommentLayer, plain)).toEqual({
      doc: 'hello',
      handled: false,
    })
    expect(apply(removeCommentLayer, plain)).toEqual({
      doc: 'hello',
      handled: false,
    })
  })
})

describe('a read-only document', () => {
  test('neither command edits one', () => {
    const state = mkParsedState({
      doc: ';; note',
      extensions: [ScamperSupport(), EditorState.readOnly.of(true)],
    })
    expect(apply(addCommentLayer, state).handled).toBe(false)
    expect(apply(removeCommentLayer, state).handled).toBe(false)
  })
})
