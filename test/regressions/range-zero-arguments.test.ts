import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/542
//
// `range` and `vector-range` were documented with a rest parameter alone --
// `(range & args)`, `(vector-range & args)` -- so the generated contract
// admitted a call with no arguments at all and each native then turned it away
// itself: "(range) 1, 2, or 3 numbers must be passed to function". A clean
// ScamperError rather than #492's leaked TypeError, but still a contract
// promising something the function does not do, and an arity complaint arriving
// from the library rather than from the call.
//
// Narrowed like #517's `-` and `/` rather than widened like #492's `string` and
// `append`: a variadic is total at zero arguments only when it has a unit to
// return, and there is no list `(range)` could sensibly be. So the docstring
// requires the first number and the contract turns the empty call away with an
// ordinary arity error pointing at the student's own code.

describe('zero-argument range and vector-range (#542)', () => {
  test('the contract rejects the empty call, not the native', async () => {
    expect(
      await runProgram(`
(range)
(vector-range)
`),
    ).toEqual([
      'Runtime error [1:1-1:7]: Arity mismatch in function call: expected 1 arguments, got 0',
      'Runtime error [2:1-2:14]: Arity mismatch in function call: expected 1 arguments, got 0',
    ])
  })

  // Requiring the first argument must not cost the three forms, which is
  // exactly what it cost the last time `vector-range` was given fixed
  // parameters: a two-parameter signature made `(vector-range 10)` a spurious
  // arity error (see test/libs/prelude.test.ts's `vector-range`). The first
  // argument is the *end* of a one-argument call and the *beginning* of a
  // longer one, so a signature that names it has to leave that shift to the
  // prose -- and these are what say the shift still happens.
  test('one, two and three arguments still mean what they did', async () => {
    expect(
      await runProgram(`
(range 5)
(range 2 5)
(range 2 11 3)
(range 10 0 -2)
(range 5 2)
(vector-range 5)
(vector-range 2 5)
(vector-range 2 11 3)
`),
    ).toEqual([
      '(list 0 1 2 3 4)',
      '(list 2 3 4)',
      '(list 2 5 8)',
      '(list 10 8 6 4 2)',
      'null',
      '(vector 0 1 2 3 4)',
      '(vector 2 3 4)',
      '(vector 2 5 8)',
    ])
  })
})
