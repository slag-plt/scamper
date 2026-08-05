import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/257
//
// `vector-ref` did `return v[i]` and `vector-set!` did `v[i] = x` with no
// bounds check. An out-of-range `vector-ref` silently returned void; an
// out-of-range `vector-set!` either grew the vector with holes (index >=
// length) or silently no-op'd (negative index). Both now raise a clean
// bounds error mirroring `list-ref`'s "index N out of bounds of list".
// Non-integer indices were, and remain, caught by the `integer?` docstring
// contract.
//
// Ranges are stripped from error messages here: this test is about the bounds
// *message*, not the location. The ranges point at the call site, asserted in
// contract-error-call-site.test.ts.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('vector-ref rejects out-of-bounds indices cleanly (#257)', () => {
  test('index == length', async () => {
    expect(stripRange(await runProgram('(vector-ref (vector 1 2 3) 3)'))).toEqual([
      'Runtime error [..]: (vector-ref) vector-ref: index 3 out of bounds of vector',
    ])
  })

  test('index > length', async () => {
    expect(stripRange(await runProgram('(vector-ref (vector 1 2 3) 5)'))).toEqual([
      'Runtime error [..]: (vector-ref) vector-ref: index 5 out of bounds of vector',
    ])
  })

  test('negative index', async () => {
    expect(stripRange(await runProgram('(vector-ref (vector 1 2 3) -1)'))).toEqual([
      'Runtime error [..]: (vector-ref) vector-ref: index -1 out of bounds of vector',
    ])
  })

  test('empty vector', async () => {
    expect(stripRange(await runProgram('(vector-ref (vector) 0)'))).toEqual([
      'Runtime error [..]: (vector-ref) vector-ref: index 0 out of bounds of vector',
    ])
  })

  test('non-integer index still caught by the integer? contract', async () => {
    expect(stripRange(await runProgram('(vector-ref (vector 1 2 3) 1.5)'))).toEqual([
      'Runtime error [..]: (error) expected an integer, received number',
    ])
  })

  test('valid in-range access still works', async () => {
    expect(await runProgram(`
    (vector-ref (vector 1 2 3) 0)
    (vector-ref (vector 1 2 3) 2)
    `)).toEqual([
      '1',
      '3',
    ])
  })
})

describe('vector-set! rejects out-of-bounds indices cleanly (#257)', () => {
  test('index == length does not grow the vector', async () => {
    expect(stripRange(await runProgram(`
    (define v (vector 1 2 3))
    (vector-set! v 3 99)
    v
    (vector-length v)
    `))).toEqual([
      'Runtime error [..]: (vector-set!) vector-set!: index 3 out of bounds of vector',
      '(vector 1 2 3)',
      '3',
    ])
  })

  test('index > length does not create holes', async () => {
    expect(stripRange(await runProgram(`
    (define v (vector 1 2 3))
    (vector-set! v 5 99)
    v
    (vector-length v)
    `))).toEqual([
      'Runtime error [..]: (vector-set!) vector-set!: index 5 out of bounds of vector',
      '(vector 1 2 3)',
      '3',
    ])
  })

  test('negative index is no longer a silent no-op', async () => {
    expect(stripRange(await runProgram(`
    (define v (vector 1 2 3))
    (vector-set! v -1 99)
    v
    `))).toEqual([
      'Runtime error [..]: (vector-set!) vector-set!: index -1 out of bounds of vector',
      '(vector 1 2 3)',
    ])
  })

  test('valid in-range mutation is reflected by a subsequent ref', async () => {
    expect(await runProgram(`
    (define v (vector 1 2 3))
    (vector-set! v 1 99)
    v
    (vector-ref v 1)
    `)).toEqual([
      'void',
      '(vector 1 99 3)',
      '99',
    ])
  })
})
