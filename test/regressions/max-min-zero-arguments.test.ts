import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/647
//
// `max` and `min` were documented with a rest parameter alone -- `(max & v)`
// -- so the generated contract admitted a call with no arguments, and
// `Math.max()`/`Math.min()` answer with IEEE's identity elements: `(max)` was
// `-Infinity` and `(min)` was `Infinity`. R7RS requires at least one argument
// of each, and no student writing `(max)` means an infinity; it is a value
// that travels silently through whatever arithmetic follows.
//
// #517 left these deliberately, on the rule that a variadic stays total when
// it has a unit -- and these do compose, `(max (apply max null) 4)` is 4. The
// maintainer has since reversed that: an infinity is not a number a student in
// this course has a use for, so the docstrings narrow to `(max v1 & v)` and
// the contract turns the empty call away at the student's own line, exactly as
// #517 did for `-` and `/`.

describe('zero-argument max and min (#647)', () => {
  test('max and min report an arity error rather than an infinity', async () => {
    expect(await runProgram(`
(max)
(min)
`)).toEqual([
      'Runtime error [1:1-1:5]: Arity mismatch in function call: expected 1 arguments, got 0',
      'Runtime error [2:1-2:5]: Arity mismatch in function call: expected 1 arguments, got 0',
    ])
  })

  test('the narrowed contract does not cost the ordinary calls', async () => {
    expect(await runProgram(`
(max 3)
(min 3)
(max 1 5 3)
(min 1 5 3)
(apply max (list 1 5 3))
`)).toEqual(['3', '3', '5', '1', '5'])
  })

  test('an empty spread is an arity error too, not an infinity', async () => {
    expect(await runProgram('(apply max null)')).toEqual([
      'Runtime error [1:1-1:16]: Arity mismatch in function call: expected 1 arguments, got 0',
    ])
  })
})
