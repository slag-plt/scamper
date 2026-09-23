import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/645
//
// `substring` was `String.prototype.substring`, which honours none of what
// R7RS 6.7 requires of `0 <= start <= end <= (string-length string)`. It
// clamps an index past the end -- `(substring "hello" 2 99)` was "llo" and
// `(substring "hello" 99 100)` was "" -- and, worse, it *swaps* its arguments
// when start > end, so `(substring "hello" 3 1)` answered "el": a typo'd or
// miscomputed pair of indices silently produced a plausible string instead of
// saying anything.
//
// All three now raise, in the style the other bounds checks share.
//
// Ranges are stripped: this test is about the messages, not the locations.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('substring follows R7RS bounds (#645)', () => {
  test('an end past the string is no longer clamped', async () => {
    expect(stripRange(await runProgram('(substring "hello" 2 99)'))).toEqual([
      'Runtime error [..]: (substring) substring: end index 99 out of bounds of string',
    ])
  })

  test('a start past the string is no longer clamped', async () => {
    expect(stripRange(await runProgram('(substring "hello" 99 100)'))).toEqual([
      'Runtime error [..]: (substring) substring: start index 99 out of bounds of string',
    ])
  })

  test('start after end no longer swaps the two', async () => {
    expect(stripRange(await runProgram('(substring "hello" 3 1)'))).toEqual([
      'Runtime error [..]: (substring) substring: start index 3 is greater than end index 1',
    ])
  })

  test('a negative index', async () => {
    expect(stripRange(await runProgram('(substring "hello" -1 2)'))).toEqual([
      'Runtime error [..]: (substring) substring: start index -1 out of bounds of string',
    ])
  })

  test('a start past the string with no end given', async () => {
    expect(stripRange(await runProgram('(substring "hello" 6)'))).toEqual([
      'Runtime error [..]: (substring) substring: start index 6 out of bounds of string',
    ])
  })

  test('the legal calls are unchanged, boundaries included', async () => {
    expect(await runProgram(`
(substring "hello" 1 3)
(substring "hello" 2)
(substring "hello" 0 5)
(substring "hello" 5 5)
(substring "hello" 5)
(substring "" 0 0)
`)).toEqual(['"el"', '"llo"', '"hello"', '""', '""', '""'])
  })
})
