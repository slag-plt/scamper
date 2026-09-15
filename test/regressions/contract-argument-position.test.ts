import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { typeOf } from '../../src/lpm/util'

// https://github.com/slag-plt/scamper/issues/606
//
// `(quotient 5.5 2)` and `(remainder 5 1.5)` both reported "expected an
// integer, received number", which hid the two things a student needed: that
// a *non-whole* number is what was rejected (an integer is a number too, so
// the message read as a contradiction), and *which* of the two arguments was
// at fault.
//
// Both halves are fixed centrally rather than on these two functions.
// `typeOf` (src/lpm/util.ts) names a non-integer "floating point number", so
// every "..., received ..." message in the system gains the distinction, and
// contract insertion (src/scheme/contract.ts) appends the offending
// argument's ordinal -- but only when the signature declares more than one
// parameter, since on a unary function there is nothing to disambiguate.

describe('a contract names which argument was at fault (#606)', () => {
  test("the issue's four examples name the position and the float", async () => {
    expect(
      await runProgram(
        '(quotient 5.5 2)\n(remainder 5.5 4)\n(quotient 5 1.5)\n(remainder 5 1.5)',
      ),
    ).toEqual([
      'Runtime error [1:1-1:16]: (error) expected an integer as the first argument, received floating point number',
      'Runtime error [2:1-2:17]: (error) expected an integer as the first argument, received floating point number',
      'Runtime error [3:1-3:16]: (error) expected an integer as the second argument, received floating point number',
      'Runtime error [4:1-4:17]: (error) expected an integer as the second argument, received floating point number',
    ])
  })

  test('the ordinal counts past the second argument, optionals included', async () => {
    // substring's `end` is an *optional* parameter, so its index is counted
    // from the end of the fixed ones rather than from its own position.
    expect(
      await runProgram('(string-split "a,b" 2)\n(substring "hello" 0 1.5)'),
    ).toEqual([
      'Runtime error [1:1-1:22]: (error) expected a string as the second argument, received number',
      'Runtime error [2:1-2:25]: (error) expected an integer as the third argument, received floating point number',
    ])
  })

  test('a one-parameter function still reports no position', async () => {
    // "as the first argument" would be noise where there is only one.
    expect(await runProgram('(car 5)\n(string-length 5)')).toEqual([
      'Runtime error [1:1-1:7]: (error) expected pair or nonempty-list, received number',
      'Runtime error [2:1-2:17]: (error) expected a string, received number',
    ])
  })
})

describe('typeOf distinguishes a whole number from a float (#606)', () => {
  test('only a non-integer is called a floating point number', () => {
    expect([typeOf(5), typeOf(-3), typeOf(0)]).toEqual([
      'number',
      'number',
      'number',
    ])
    expect([typeOf(5.5), typeOf(-0.25)]).toEqual([
      'floating point number',
      'floating point number',
    ])
  })

  test('a whole number written with a decimal point is still a number', () => {
    // 5.0 *is* an integer as far as Javascript is concerned, and Scamper has
    // no separate float type, so nothing distinguishes it from 5.
    expect(typeOf(5.0)).toBe('number')
  })

  test('the distinction reaches a native check, not just a contract', async () => {
    // Every backend renders a type name through the same `typeOf`, so the
    // wording cannot drift between one message and another.
    expect(await runProgram('(vector-ref (vector 1 2) 0.5)')).toEqual([
      'Runtime error [1:1-1:29]: (error) expected an integer as the second argument, received floating point number',
    ])
  })
})
