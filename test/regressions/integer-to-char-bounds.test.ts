import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/644
//
// `integer->char` called `String.fromCodePoint(n)` with no range check, so an
// argument outside Unicode's code space escaped as a raw Javascript error:
// `Unexpected error in Javascript function call: RangeError: Invalid code
// point -1`. The `integer?` contract admitted it -- -1 is an integer -- and
// nothing below it looked at the value.
//
// It now raises in the style its siblings share, `string-ref`, `vector-ref`
// and `list-ref`: a ScamperError naming the procedure and the offending value.
//
// Ranges are stripped: this test is about the message, not the location.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('integer->char rejects code points outside Unicode (#644)', () => {
  test('a negative code point', async () => {
    expect(stripRange(await runProgram('(integer->char -1)'))).toEqual([
      'Runtime error [..]: (integer->char) integer->char: code point -1 out of bounds of Unicode',
    ])
  })

  test('one past the last code point', async () => {
    expect(stripRange(await runProgram('(integer->char 1114112)'))).toEqual([
      'Runtime error [..]: (integer->char) integer->char: code point 1114112 out of bounds of Unicode',
    ])
  })

  test('the ends of the range are still legal', async () => {
    expect(await runProgram(`
(char->integer (integer->char 0))
(char->integer (integer->char 1114111))
(integer->char 97)
`)).toEqual(['0', '1114111', '#\\a'])
  })

  test('a non-integer is still caught by the contract', async () => {
    expect(stripRange(await runProgram('(integer->char 1.5)'))).toEqual([
      'Runtime error [..]: (error) expected an integer as the first argument, received floating point number',
    ])
  })
})
