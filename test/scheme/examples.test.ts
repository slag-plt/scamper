import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import { Prog, stmtToString } from '../../src/scheme/ast'
import {
  classifyExampleRun,
  collectExamples,
  mkCheckProgram,
} from '../../src/scheme/examples'
import { Range, ReportError, ScamperError } from '../../src/lpm'

function parse(src: string): Prog {
  const { program, diagnostics } = tokenizeAndParse(src)
  expect(diagnostics).toStrictEqual([])
  if (program === undefined) throw new Error('did not parse')
  return program
}

const factorial = `;;; (fact n) -> number?
;;;   n : number?
;;; Returns n factorial.
;;; @example (fact 5) -> 120
(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))`

describe('collectExamples', () => {
  test('finds an example and the line it is on', () => {
    const checks = collectExamples(parse(factorial))
    expect(checks).toHaveLength(1)
    expect(checks[0].range.begin.line).toBe(4)
  })

  test('finds every example on one definition', () => {
    const src = `;;; (fact n) -> number?
;;;   n : number?
;;; Returns n factorial.
;;; @example (fact 0) -> 1
;;; @example (fact 5) -> 120
(define fact 1)`
    const checks = collectExamples(parse(src))
    expect(checks.map((c) => c.range.begin.line)).toStrictEqual([4, 5])
  })

  test('finds examples across several definitions', () => {
    const src = `${factorial}

;;; (twice n) -> number?
;;;   n : number?
;;; Doubles n.
;;; @example (twice 2) -> 4
(define twice (lambda (n) (* 2 n)))`
    expect(collectExamples(parse(src))).toHaveLength(2)
  })

  test('ignores a definition with no docstring', () => {
    expect(collectExamples(parse('(define x 1)'))).toStrictEqual([])
  })

  test('ignores a malformed docstring rather than reporting it', () => {
    const src = `;;; (fact n) -> number?
;;; @example this is not an example
(define fact 1)`
    expect(collectExamples(parse(src))).toStrictEqual([])
  })

  test('ignores a docstring with no examples', () => {
    const src = `;;; (fact n) -> number?
;;;   n : number?
;;; Returns n factorial.
(define fact 1)`
    expect(collectExamples(parse(src))).toStrictEqual([])
  })
})

describe('mkCheckProgram', () => {
  test('appends one statement reporting the call beside the expected value', () => {
    const prog = parse(factorial)
    const checked = mkCheckProgram(prog, collectExamples(prog)[0])
    expect(checked).toHaveLength(prog.length + 1)
    expect(stmtToString(checked[checked.length - 1])).toBe(
      '(##report## [(fact 5) 120])',
    )
  })

  test('leaves the program it was given alone', () => {
    const prog = parse(factorial)
    mkCheckProgram(prog, collectExamples(prog)[0])
    expect(prog).toHaveLength(1)
  })
})

describe('classifyExampleRun', () => {
  const report = (v: unknown) =>
    new ReportError(v as never, Range.none) as ScamperError

  test('passes when the two sides are structurally equal', () => {
    expect(classifyExampleRun([report([[1, 2], [1, 2]])])).toStrictEqual({
      status: 'pass',
    })
  })

  test('fails with both values when they differ', () => {
    expect(classifyExampleRun([report([121, 120])])).toStrictEqual({
      status: 'fail',
      actual: 121,
      expected: 120,
    })
  })

  test('errors with the message when the program failed instead', () => {
    const outcome = classifyExampleRun([
      new ScamperError('Runtime', 'boom'),
    ])
    expect(outcome.status).toBe('error')
    expect(outcome.message).toMatch(/boom/)
  })

  test('errors when nothing was reported at all', () => {
    expect(classifyExampleRun([]).status).toBe('error')
  })
})
