import { describe, expect, test } from 'vitest'
import {
  compile,
  compileExamples,
  fiberRaiser,
  tokenizeAndParse,
} from '../../src/scheme'
import { expToString } from '../../src/scheme/ast.js'
import { Loc } from '../../src/lpm'
import * as LPM from '../../src/lpm/'
import { Frame } from '../../src/lpm/frame.js'
import { makeTestFiber } from '../util.js'

// Finds the Loc of the first occurrence of `needle` within `src`, so tests
// can point at a specific token instead of hand-computing line/col/idx.
function locOf(src: string, needle: string): Loc {
  const idx = src.indexOf(needle)
  const before = src.slice(0, idx)
  const line = before.split('\n').length
  const lineStart = before.lastIndexOf('\n') + 1
  return new Loc(line, idx - lineStart + 1, idx)
}

describe('fiberRaiser', () => {
  test('raise reconstructs a let op directly (no un-sugaring pass)', () => {
    const fiber = makeTestFiber([])
    fiber.pushFrame(
      new Frame('f1', LPM.Env.empty, [
        LPM.mkLet([{ pat: LPM.mkPVar('n'), value: [LPM.mkLit(1)] }], [
          LPM.mkVar('n'),
        ]),
        LPM.mkPopScope(),
      ]),
    )
    const raised = fiberRaiser.raise(fiber)
    expect(expToString(raised)).toBe('(let ([n 1]) n)')
  })
})

describe('tokenizeAndParse with a query location', () => {
  test('reports a diagnostic for a query location outside every statement', () => {
    const { program, diagnostics } = tokenizeAndParse(
      '(define foo 1)',
      new Loc(1, 9999, 9999),
    )
    expect(program).toBeUndefined()
    expect(diagnostics).toHaveLength(1)
    expect(diagnostics[0].phase).toBe('Query')
  })

  test('rejects a query outside a function definition', () => {
    const src = '(display 1)'
    const { program, diagnostics } = tokenizeAndParse(src, locOf(src, '1'))
    expect(program).toBeUndefined()
    expect(diagnostics).toHaveLength(1)
    expect(diagnostics[0].message).toMatch(
      /only allowed within function definitions/,
    )
  })

  test('rejects a query on a definition with a malformed docstring', () => {
    const src = `;;; (foo) -> number?
(define foo 1)`
    const { program, diagnostics } = tokenizeAndParse(src, locOf(src, '1'))
    expect(program).toBeUndefined()
    expect(diagnostics).toHaveLength(1)
    expect(diagnostics[0].phase).toBe('Docstring')
  })

  test('rejects a query on a definition whose comment is not a docstring', () => {
    const src = `; just a regular comment
(define foo 1)`
    const { program, diagnostics } = tokenizeAndParse(src, locOf(src, '1'))
    expect(program).toBeUndefined()
    expect(diagnostics).toHaveLength(1)
    expect(diagnostics[0].message).toMatch(
      /only allowed within function definitions/,
    )
  })

  test('rejects a query on a docstring with no example tag', () => {
    const src = `;;; (foo) -> number?
;;; constant one
(define foo 1)`
    const { program, diagnostics } = tokenizeAndParse(src, locOf(src, '1'))
    expect(program).toBeUndefined()
    expect(diagnostics).toHaveLength(1)
    expect(diagnostics[0].message).toMatch(/requires an example tag/)
  })
})

describe('compile with a query location', () => {
  test('returns no program when the query cannot be resolved', async () => {
    const { prog, diagnostics } = await compile('(define foo 1)', {
      queryLoc: new Loc(1, 9999, 9999),
    })
    expect(prog).toBeUndefined()
    expect(diagnostics).toHaveLength(1)
  })
})

describe('compileExamples', () => {
  const documented = (examples: string) =>
    [
      ';;; (fact n) -> number?',
      ';;;   n : number?',
      ';;; Returns n factorial.',
      examples,
      '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))',
    ].join('\n')

  test('lowers one program per example, tagged with its line', () => {
    const src = documented(
      [';;; @example (fact 0) -> 1', ';;; @example (fact 5) -> 120'].join('\n'),
    )
    const { examples, diagnostics } = compileExamples(src)
    expect(diagnostics).toStrictEqual([])
    expect(examples.map((e) => e.range.begin.line)).toStrictEqual([4, 5])
    // Each program is the whole file *plus* the report that ends it, so it is
    // strictly longer than the file's own single statement.
    expect(examples.every((e) => e.prog.length > 1)).toBe(true)
  })

  test('returns nothing for a file with no examples', () => {
    expect(compileExamples('(define x 1)').examples).toStrictEqual([])
  })

  test('returns nothing but the diagnostics when the file does not parse', () => {
    const { examples, diagnostics } = compileExamples('(define x')
    expect(examples).toStrictEqual([])
    expect(diagnostics.length).toBeGreaterThan(0)
  })
})
