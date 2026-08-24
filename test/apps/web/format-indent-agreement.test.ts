import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import { ensureSyntaxTree, indentRange } from '@codemirror/language'
import { ScamperSupport } from '../../../src/app/web/codemirror/extensions/language'
import { tokenizeAndParse } from '../../../src/scheme'
import { layoutToString, stmtToLayout } from '../../../src/scheme/ast'
import * as prettier from 'prettier'
import ScamperPlugin from '../../../src/prettier/prettier-plugin-scamper'

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

/** What the output and step panes draw. */
function format(src: string): string {
  const { program, diagnostics } = tokenizeAndParse(src)
  if (!program) throw new Error(diagnostics.map((d) => d.message).join('; '))
  return program.map((s) => layoutToString(stmtToLayout(s))).join('\n')
}

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
]

describe('the printer and the indenter agree', () => {
  test.each(PROGRAMS)('%s', (_name, src) => {
    const formatted = format(src)
    expect(reindent(formatted)).toBe(formatted)
  })

  test('and formatting is itself stable', () => {
    for (const [, src] of PROGRAMS) {
      const once = format(src)
      expect(format(once)).toBe(once)
    }
  })
})

// ---- the reformat command (Ctrl-Shift-I) ------------------------------------

/**
 * Shapes the Prettier-backed reformat command cannot reproduce.
 *
 * Prettier tracks indentation as a virtual stack rather than an output column.
 * The two agree while a form begins at the start of its own line, and diverge
 * when one begins part-way through a line -- as the `(f ...)` does inside
 * `[(< x 0) (f ...` -- because the alignment is then measured from the
 * enclosing line's indent rather than from the form's real column.
 *
 * This is the structural limit recorded in FORMATTING.md, and the reason the
 * plan there ends by retiring this printer in favour of src/scheme/pretty.ts,
 * which measures real columns and has no such problem. Doing so needs comments
 * modelled in Layout first (issue #304), so until then these two are held to
 * the weaker property that reformatting is at least stable.
 */
const PRETTIER_MISALIGNS = new Set([
  'a cond whose clause must split',
  'a match with wide clauses',
])

function reformat(src: string): Promise<string> {
  return prettier
    .format(src, { parser: 'scamper-scheme', plugins: [ScamperPlugin] })
    .then((out) => out.trimEnd())
}

describe('the reformat command and the indenter agree', () => {
  test.each(PROGRAMS)('%s', async (name, src) => {
    const formatted = await reformat(src)
    if (PRETTIER_MISALIGNS.has(name)) {
      expect(await reformat(formatted)).toBe(formatted)
      return
    }
    expect(reindent(formatted)).toBe(formatted)
  })
})
