import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/684
//
// A struct constructor's arity error interpolated its field count bare, so a
// one-field struct read in the plural:
//
//   (struct one (x))
//   (one)
//   -> Runtime error [2:1-2:5]: (one) Constructor one expects 1 arguments,
//      received 0
//
// the same singular/plural slip #670 fixed for the closure arity message.
// `runtime_mkCtorFn` builds its own message rather than calling
// L.arityMismatchMsg -- the two have different shapes -- so the noun is now
// chosen the same way in both places instead of being open-coded a second
// time.
//
// Ranges are stripped: they are the student's own call, and spelling each one
// out would say nothing these tests are about. Only the "expects N" half of
// the message carries a noun; "received N" has none, so nothing about the
// given count pluralises.

describe('#684: a constructor pluralises its expected-argument count', () => {
  test('a one-field struct says "argument", whether the call is short or long', async () => {
    expect(await runProgram(`
    (struct one (x))
    (one)
    (one 1 2)
    `, { stripRanges: true })).toEqual([
      'Runtime error: (one) Constructor one expects 1 argument, received 0',
      'Runtime error: (one) Constructor one expects 1 argument, received 2',
    ])
  })

  // The plural side, pinned so a fix cannot trade one wrong noun for another.
  test('a many-field struct still says "arguments"', async () => {
    expect(await runProgram(`
    (struct two (x y))
    (two)
    (two 1)
    `, { stripRanges: true })).toEqual([
      'Runtime error: (two) Constructor two expects 2 arguments, received 0',
      'Runtime error: (two) Constructor two expects 2 arguments, received 1',
    ])
  })

  // A field-less struct is legal, and zero takes the plural in English.
  test('a field-less struct says "0 arguments", and accepts no-argument calls', async () => {
    expect(await runProgram(`
    (struct zero ())
    (zero 1)
    (zero)
    `, { stripRanges: true })).toEqual([
      'Runtime error: (zero) Constructor zero expects 0 arguments, received 1',
      '(zero)',
    ])
  })
})
