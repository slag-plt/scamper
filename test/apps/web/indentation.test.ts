import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import {
  ensureSyntaxTree,
  getIndentation,
  IndentContext,
  indentRange,
} from '@codemirror/language'
import { ScamperSupport } from '../../../src/app/web/codemirror/extensions/language'

// The DrRacket-style indenter (see FORMATTING.md). These tests go through the
// same path the editor does: `indentRange` is what Ctrl-I dispatches, and
// `getIndentation` with a simulated break is what Enter asks for -- so a rule
// that passes here is the rule the user gets.

function mkState(doc: string): EditorState {
  const state = EditorState.create({ doc, extensions: [ScamperSupport()] })
  // A fresh state parses lazily under a time budget; force it so the tree
  // covers the whole document before anything asks for an indent.
  ensureSyntaxTree(state, state.doc.length, 5000)
  return state
}

/** Ctrl-I: re-indent every line, leaving line breaks alone. */
function reindent(doc: string): string {
  const state = mkState(doc)
  const changes = indentRange(state, 0, state.doc.length)
  return state.update({ changes }).state.doc.toString()
}

/** Enter: the indent given to a new line opened at the end of `doc`. */
function indentAfter(doc: string): number | null {
  const state = mkState(doc)
  const cx = new IndentContext(state, { simulateBreak: doc.length })
  return getIndentation(cx, doc.length)
}

// ---- Ctrl-I: whole-buffer re-indentation -----------------------------------

describe('re-indenting a buffer', () => {
  /** Checks that flush-left input re-indents to `expected`, and is stable. */
  function fixes(flat: string, expected: string): void {
    expect(reindent(flat)).toBe(expected)
    expect(reindent(expected)).toBe(expected)
  }

  test('rule 1: a lambda body is indented two spaces', () => {
    fixes(
      '(define f\n(lambda (x y)\n(+ x y)))',
      '(define f\n  (lambda (x y)\n    (+ x y)))',
    )
  })

  test('rule 2: a define body is indented two spaces', () => {
    fixes('(define x\n(f 1 2))', '(define x\n  (f 1 2))')
  })

  test('rule 3: if branches align under the test, at column 4', () => {
    fixes('(if (> x 0)\nx\n(- 0 x))', '(if (> x 0)\n    x\n    (- 0 x))')
  })

  test('rule 4: let bindings align at column 6, body at two', () => {
    fixes(
      '(let ([a 1]\n[b 2])\n(+ a b))',
      '(let ([a 1]\n      [b 2])\n  (+ a b))',
    )
  })

  test('rule 5: cond clauses at two, consequents at three', () => {
    fixes(
      '(cond\n[(< x 0)\n(neg x)]\n[else\n(pos x)])',
      '(cond\n  [(< x 0)\n   (neg x)]\n  [else\n   (pos x)])',
    )
  })

  test('rule 6: match clauses follow cond, scrutinee stays put', () => {
    fixes(
      '(match lst\n[null 0]\n[(cons x xs)\n(+ x 1)])',
      '(match lst\n  [null 0]\n  [(cons x xs)\n   (+ x 1)])',
    )
  })

  test('rule 7: arguments align under the first argument', () => {
    fixes(
      '(some-fn arg1\narg2\narg3)',
      '(some-fn arg1\n         arg2\n         arg3)',
    )
  })

  test('a lambda parameter list aligns under its first parameter', () => {
    fixes('(lambda (x\ny)\nz)', '(lambda (x\n         y)\n  z)')
  })

  test('begin is a body form, not an aligned one', () => {
    fixes('(begin\n(f 1)\n(g 2))', '(begin\n  (f 1)\n  (g 2))')
  })

  test('and/or take the default rule and align', () => {
    fixes('(and (p x)\n(q x))', '(and (p x)\n     (q x))')
  })

  test('nesting composes: each level sees the level above it', () => {
    fixes(
      '(define go\n(lambda (n)\n(cond\n[(zero? n)\n1]\n[else\n(* n (go (- n 1)))])))',
      '(define go\n' +
        '  (lambda (n)\n' +
        '    (cond\n' +
        '      [(zero? n)\n' +
        '       1]\n' +
        '      [else\n' +
        '       (* n (go (- n 1)))])))',
    )
  })

  test('a vector aligns under its first element', () => {
    fixes('[1\n2\n3]', '[1\n 2\n 3]')
  })

  test('a line holding only a closing bracket lines up with its opener', () => {
    fixes('(some-fn arg1\narg2\n)', '(some-fn arg1\n         arg2\n)')
  })

  test('blank lines are emptied rather than padded', () => {
    expect(reindent('(define x\n   \n  1)')).toBe('(define x\n\n  1)')
  })

  test('a comment line is indented like the code it sits among', () => {
    fixes(
      '(cond\n; the negative case\n[(< x 0)\n(neg x)])',
      '(cond\n  ; the negative case\n  [(< x 0)\n   (neg x)])',
    )
  })
})

// ---- Enter: the next line's indent -----------------------------------------

describe('pressing Enter', () => {
  test.each([
    ['(define x', 2],
    ['(define f\n  (lambda (x y)', 4],
    ['(lambda (x y)', 2],
    ['(begin', 2],
    ['(if (> x 0)', 4],
    ['(if (> x 0)\n    x', 4],
    ['(some-fn arg1', 9],
    ['(let ([a 1]', 6],
    ['(let ([a 1]\n      [b 2])', 2],
    ['(cond', 2],
    ['(cond\n  [(< x 0)', 3],
    ['(match lst', 2],
    ['(match lst\n  [null', 3],
    ['(lambda (x', 9],
  ])('%j opens the next line at column %i', (doc, expected) => {
    expect(indentAfter(doc)).toBe(expected)
  })

  test('with no first argument yet, falls back to one unit', () => {
    expect(indentAfter('(some-fn')).toBe(2)
  })

  test('an unclosed form still indents, mid-typing', () => {
    expect(indentAfter('(define f\n  (lambda (x)\n    (if (> x 0)')).toBe(8)
  })
})
