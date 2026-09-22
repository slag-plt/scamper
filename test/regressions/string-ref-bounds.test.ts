import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import * as L from '../../src/lpm'

// https://github.com/slag-plt/scamper/issues/613
//
// `string-ref` did `mkChar(s[i])` with no bounds check. An out-of-range index
// made `s[i]` `undefined`, so the char it returned held `undefined` as its
// value and printed as `#\undefined` instead of raising. It now raises the
// same bounds error `vector-ref` does (see vector-bounds.test.ts, #257).
//
// `mkChar` is the only place a char is made, so it is guarded too: a char can
// no longer hold anything but a non-empty string, whichever caller asks.
//
// Ranges are stripped: this test is about the bounds *message*, not the
// location, which contract-error-call-site.test.ts asserts.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('string-ref rejects out-of-bounds indices cleanly (#613)', () => {
  test('index == length', async () => {
    expect(stripRange(await runProgram('(string-ref "hello" 5)'))).toEqual([
      'Runtime error [..]: (string-ref) string-ref: index 5 out of bounds of string',
    ])
  })

  test('index > length', async () => {
    expect(stripRange(await runProgram('(string-ref "hello" 6)'))).toEqual([
      'Runtime error [..]: (string-ref) string-ref: index 6 out of bounds of string',
    ])
  })

  test('negative index', async () => {
    expect(stripRange(await runProgram('(string-ref "hello" -1)'))).toEqual([
      'Runtime error [..]: (string-ref) string-ref: index -1 out of bounds of string',
    ])
  })

  test('empty string', async () => {
    expect(stripRange(await runProgram('(string-ref "" 0)'))).toEqual([
      'Runtime error [..]: (string-ref) string-ref: index 0 out of bounds of string',
    ])
  })

  test('non-integer index still caught by the integer? contract', async () => {
    expect(stripRange(await runProgram('(string-ref "hello" 1.5)'))).toEqual([
      'Runtime error [..]: (error) expected an integer as the second argument, received floating point number',
    ])
  })

  test('valid in-range access still works', async () => {
    expect(await runProgram(`
    (string-ref "hello" 0)
    (string-ref "hello" 4)
    `)).toEqual([
      '#\\h',
      '#\\o',
    ])
  })
})

describe('a char cannot be built from a missing value (#613)', () => {
  // The cast is the point: it is exactly what an out-of-range `s[i]` handed
  // `mkChar` before the guard, and what the types cannot rule out.
  const mkBadChar = (v: unknown): (() => L.Char) => () =>
    L.mkChar(v as string)

  test('undefined is rejected', () => {
    expect(mkBadChar(undefined)).toThrow(/expected a non-empty string, received undefined/)
  })

  test('null is rejected', () => {
    expect(mkBadChar(null)).toThrow(/expected a non-empty string, received null/)
  })

  test('the empty string is rejected', () => {
    expect(mkBadChar('')).toThrow(/expected a non-empty string/)
  })

  test('an ordinary character is still fine', () => {
    expect(L.mkChar('a').value).toBe('a')
  })
})
