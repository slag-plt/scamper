import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import * as L from '../../src/lpm'

// https://github.com/slag-plt/scamper/issues/646
//
// #613's `mkChar` guard asked only that the value be a non-empty string,
// deliberately admitting more than one UTF-16 unit because an astral code
// point is one code point in two of them. But it admitted a value that is
// genuinely two *code points* as well, and Unicode's case mappings are not all
// one-to-one: `(char-upcase #\ß)` built a char holding "SS" and printed it as
// `#\SS`, a character no Scamper program can name, compare or round-trip
// (`char->integer` of it was 83, the first S alone).
//
// A char now holds exactly one code point -- `[...v].length === 1`, so astral
// code points stay legal -- and the three case procedures return the character
// unchanged where its mapping is not one. R7RS 6.6 asks for exactly that.

describe('the case procedures stay within one code point (#646)', () => {
  test('a character whose uppercase is two characters is returned unchanged', async () => {
    expect(await runProgram(`
(char-upcase #\\ß)
(char=? (char-upcase #\\ß) #\\ß)
(char->integer (char-upcase #\\ß))
`)).toEqual(['#\\ß', '#t', '223'])
  })

  test('the ordinary mappings are unchanged', async () => {
    expect(await runProgram(`
(char-upcase #\\a)
(char-downcase #\\A)
(char-foldcase #\\A)
(char-upcase #\\1)
(char-downcase #\\ß)
(char-foldcase #\\ß)
`)).toEqual(['#\\A', '#\\a', '#\\a', '#\\1', '#\\ß', '#\\ß'])
  })
})

describe('mkChar admits one code point and no more (#646)', () => {
  // The cast is the point: these are what a caller hands `mkChar` at runtime,
  // and what the `string` type cannot rule out.
  const mkBadChar = (v: unknown): (() => L.Char) => () => L.mkChar(v as string)

  test('a two-code-point value is rejected', () => {
    expect(mkBadChar('SS')).toThrow(/expected a single code point, received SS/)
  })

  // The values #613's guard already turned away stay turned away, under the
  // narrower wording; they are asserted in string-ref-bounds.test.ts.
  test('an astral code point is still one character, not two', () => {
    // Two UTF-16 units, one code point: the case #613's guard was widened for.
    expect('\u{1f600}'.length).toBe(2)
    expect(L.mkChar('\u{1f600}').value).toBe('\u{1f600}')
  })
})
