import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/252
//
// When a string value is displayed, characters that are special to string
// literal syntax (the backslash and the double-quote) must be escaped so the
// printed form is a valid, re-readable string literal.

test('string-output-escaping', async () => {
  expect(await runProgram(`
    "a\\"b"
    "a\\\\b"
    "hello\\""
  `)).toEqual([
    '"a\\"b"',   // string  a"b  ->  "a\"b"
    '"a\\\\b"',  // string  a\b  ->  "a\\b"
    '"hello\\""' // string  hello"  ->  "hello\""
  ])
})
