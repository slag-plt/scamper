import { describe, expect, test } from 'vitest'
import { compile, fiberRaiser, tokenizeAndParse } from '../../src/scheme'
import { expToString } from '../../src/scheme/ast.js'
import { Loc } from '../../src/lpm'
import * as LPM from '../../src/lpm/'
import { Frame } from '../../src/lpm/frame.js'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'
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
  test('raise sugars the raised expression', () => {
    const fiber = makeTestFiber([])
    fiber.pushFrame(
      new Frame('f1', LPM.Env.empty, [
        LPM.mkLit(1),
        LPM.mkMatch([[LPM.mkPVar('n'), [LPM.mkVar('n')]]]),
      ]),
    )
    const raised = fiberRaiser.raise(fiber)
    expect(expToString(raised)).toBe('(let ([n 1]) n)')
  })
})

describe('tokenizeAndParse with a query location', () => {
  test('reports an error for a query location outside every statement', () => {
    const err = new SimpleErrorChannel()
    const result = tokenizeAndParse(
      err,
      '(define foo 1)',
      new Loc(1, 9999, 9999),
    )
    expect(result).toBeUndefined()
    expect(err.errors).toHaveLength(1)
    expect(err.errors[0].phase).toBe('Parser')
  })

  test('rejects a query outside a function definition', () => {
    const err = new SimpleErrorChannel()
    const src = '(display 1)'
    const result = tokenizeAndParse(err, src, locOf(src, '1'))
    expect(result).toBeUndefined()
    expect(err.errors).toHaveLength(1)
    expect(err.errors[0].message).toMatch(
      /only allowed within function definitions/,
    )
  })

  test('rejects a query on a definition with a malformed docstring', () => {
    const err = new SimpleErrorChannel()
    const src = `;;; (foo) -> number?
(define foo 1)`
    const result = tokenizeAndParse(err, src, locOf(src, '1'))
    expect(result).toBeUndefined()
    expect(err.errors).toHaveLength(1)
    expect(err.errors[0].phase).toBe('Docstring')
  })

  test('rejects a query on a definition whose comment is not a docstring', () => {
    const err = new SimpleErrorChannel()
    const src = `; just a regular comment
(define foo 1)`
    const result = tokenizeAndParse(err, src, locOf(src, '1'))
    expect(result).toBeUndefined()
    expect(err.errors).toHaveLength(1)
    expect(err.errors[0].message).toMatch(
      /only allowed within function definitions/,
    )
  })

  test('rejects a query on a docstring with no example tag', () => {
    const err = new SimpleErrorChannel()
    const src = `;;; (foo) -> number?
;;; constant one
(define foo 1)`
    const result = tokenizeAndParse(err, src, locOf(src, '1'))
    expect(result).toBeUndefined()
    expect(err.errors).toHaveLength(1)
    expect(err.errors[0].message).toMatch(/requires an example tag/)
  })
})

describe('compile with a query location', () => {
  test('returns undefined when the query cannot be resolved', async () => {
    const err = new SimpleErrorChannel()
    const result = await compile(err, '(define foo 1)', new Loc(1, 9999, 9999))
    expect(result).toBeUndefined()
    expect(err.errors).toHaveLength(1)
  })
})
