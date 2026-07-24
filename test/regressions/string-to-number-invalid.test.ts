import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/255
//
// `string->number` threw a bare JS `Error` for non-numeral input instead of a
// `ScamperError`, so the LPM op-handler treated it as an unexpected JS
// exception and double-wrapped it into a confusing internal message
// ("Unexpected error in Javascript function call: ..."). Per R7RS,
// `string->number` returns `#f` when the string does not denote a number.

test('string->number returns #f on non-numeral input', async () => {
  expect(await runProgram(`
  (string->number "abc")
  (string->number "")
  (string->number "12x")
  (string->number "  5  ")
  (string->number "1.2.3")
  `)).toEqual([
    '#f',
    '#f',
    '#f',
    '#f',
    '#f',
  ])
})

test('string->number still parses valid numerals', async () => {
  expect(await runProgram(`
  (string->number "9")
  (string->number "-3.14")
  (string->number "+42")
  (string->number "100")
  (string->number "0.4472135954999579")
  (string->number "1e3")
  `)).toEqual([
    '9',
    '-3.14',
    '42',
    '100',
    '0.4472135954999579',
    '1000',
  ])
})
