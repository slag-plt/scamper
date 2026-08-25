import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import { ensureSyntaxTree, indentRange } from '@codemirror/language'
import { ScamperSupport } from '../../../src/app/web/codemirror/extensions/language'
import { tokenizeAndParse } from '../../../src/scheme'
import { layoutToString, stmtToLayout } from '../../../src/scheme/ast'
import { formatSource } from '../../../src/scheme/format'
import { PRINT_WIDTH, type UserFormatMode } from '../../../src/scheme/style'

/**
 * The anti-drift invariant (FORMATTING.md):
 *
 *     indentRange(format(p)) === format(p)
 *
 * The pretty-printer decides where line breaks go; the editor's indenter
 * decides how far in each line starts. They are separate engines reading one
 * rule table, and this is what holds them together: the printer's output must
 * be a fixed point of the indenter. If either drifts -- a rule added to one and
 * not the other, an off-by-one in a column -- the two disagree about some
 * program here and the test fails.
 */

/** What the output and step panes draw, one string per statement. */
function paneStatements(src: string, mode: UserFormatMode): string[] {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) throw new Error(diagnostics.map((d) => d.message).join('; '))
  return program.map((s) => layoutToString(stmtToLayout(s), PRINT_WIDTH, mode))
}

/** Those statements as a document, which is what the indenter is handed. */
function format(src: string, mode: UserFormatMode): string {
  return paneStatements(src, mode).join('\n\n')
}

/**
 * Both modes have to hold the invariant, and strict is the harder of the two:
 * it splits every `cond`/`match` clause, putting the consequent at +3 -- a
 * column the indenter only ever reaches through `CondClause`/`MatchClause`.
 */
const MODES: UserFormatMode[] = ['strict', 'relaxed']

/** What Ctrl-I leaves behind. */
function reindent(doc: string): string {
  const state = EditorState.create({ doc, extensions: [ScamperSupport()] })
  ensureSyntaxTree(state, state.doc.length, 5000)
  const changes = indentRange(state, 0, state.doc.length)
  return state.update({ changes }).state.doc.toString()
}

const long = 'a-fairly-long-name'

const PROGRAMS: [string, string][] = [
  ['a short define', '(define x 42)'],
  [
    'a define that must break',
    `(define x (f ${long} ${long} ${long} ${long}))`,
  ],
  [
    'a lambda with a long body',
    `(define f (lambda (x y) (+ ${long} ${long} ${long} x y)))`,
  ],
  [
    'a lambda with many parameters',
    `(lambda (${long} ${long} ${long} ${long} ${long}) 1)`,
  ],
  [
    'an if that must break',
    `(if (> ${long} 0) (f ${long} ${long}) (g ${long} ${long}))`,
  ],
  [
    'a let with several bindings',
    `(let ([a (f ${long})] [b (g ${long})] [c (h ${long})]) (+ a b c))`,
  ],
  [
    'a cond with wide clauses',
    `(cond [(< x 0) (f ${long} ${long})] [(= x 0) zero] [else (g ${long})])`,
  ],
  [
    'a cond whose clause must split',
    `(cond [(< x 0) (f ${long} ${long} ${long} ${long} ${long})])`,
  ],
  [
    'a match with wide clauses',
    `(match lst [null 0] [(cons x xs) (+ x (f ${long} ${long} ${long}))])`,
  ],
  [
    'an application with many arguments',
    `(some-function ${long} ${long} ${long} ${long} ${long})`,
  ],
  ['a begin', `(begin (f ${long} ${long}) (g ${long} ${long}))`],
  ['an and', `(and (p ${long}) (q ${long}) (r ${long}) (s ${long}))`],
  ['a vector', `[${long} ${long} ${long} ${long} ${long}]`],
  ['a struct', `(struct point (${long} ${long} ${long} ${long}))`],
  [
    'a body form nested on an opening line',
    `(map (lambda (x) (+ ${long} ${long} ${long} x)) ${long})`,
  ],
  [
    'deep nesting',
    '(define go (lambda (n) (cond [(zero? n) 1] ' +
      `[else (* n (go (- n 1)) ${long} ${long})])))`,
  ],
  [
    'several statements',
    `(define a (f ${long} ${long} ${long}))\n` +
      `(define b (g ${long} ${long} ${long}))`,
  ],
  // These fit in eighty columns and so never broke at all until rules 1, 3, 5
  // and 6 became mandatory. They are the cases the invariant had never seen.
  ['a short lambda', '(lambda (x) x)'],
  ['a short if', '(if a b c)'],
  ['a short cond', '(cond [(< x 0) -1] [else 1])'],
  ['a short match', '(match l [null 0] [(cons x xs) x])'],
  ['a short let', '(let ([a 1]) a)'],
  ['a define around a short lambda', '(define f (lambda (x) x))'],
  ['a nested short form', '(g (lambda (x) (if x 1 2)) 3)'],
]

describe.each(MODES)('the printer and the indenter agree (%s)', (mode) => {
  test.each(PROGRAMS)('%s', (_name, src) => {
    const formatted = format(src, mode)
    expect(reindent(formatted)).toBe(formatted)
  })

  test('and formatting is itself stable', () => {
    for (const [, src] of PROGRAMS) {
      const once = format(src, mode)
      expect(format(once, mode)).toBe(once)
    }
  })
})

// ---- the reformat command (Ctrl-Shift-I) ------------------------------------

/**
 * The reformat command runs the same printer, over a program parsed from text
 * rather than one already in hand, so the invariant holds for it unchanged --
 * every program, no exceptions.
 *
 * It did not always. The Prettier-backed printer this replaced tracked
 * indentation as a virtual stack rather than an output column, and the two
 * diverge when a form begins part-way through a line -- as the `(f ...)` does
 * inside `[(< x 0) (f ...`. Two of the programs below were held to the weaker
 * property that reformatting is merely stable. Retiring that printer is what
 * closed the gap (FORMATTING.md, stage 3).
 */
describe.each(MODES)(
  'the reformat command and the indenter agree (%s)',
  (mode) => {
    test.each(PROGRAMS)('%s', (_name, src) => {
      const formatted = formatSource(src, PRINT_WIDTH, mode)
      expect(reindent(formatted)).toBe(formatted)
    })

    test('and the reformat command lays statements out the same way', () => {
      // Ignoring blank lines, not merely containing each statement: the spacing
      // between statements is format.ts's own business (it keeps the author's
      // grouping -- see `packs`, tested in format.test.ts), but everything else
      // must match exactly, order and indentation included. Containment alone
      // would pass a reformatter that duplicated or reordered a statement.
      const withoutBlanks = (text: string): string =>
        text
          .split('\n')
          .filter((line) => line !== '')
          .join('\n')
      for (const [, src] of PROGRAMS) {
        const out = formatSource(src, PRINT_WIDTH, mode)
        expect(withoutBlanks(out)).toBe(
          withoutBlanks(paneStatements(src, mode).join('\n')),
        )
      }
    })
  },
)
