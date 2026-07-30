import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/258
//
// Division by a zero divisor used to silently produce IEEE Infinity/NaN and
// was inconsistent across operators: `/` and `quotient` gave Infinity, while
// `modulo`/`remainder` gave NaN. All four now raise a clean Scamper runtime
// error ("<op>: division by zero"). Scamper numbers are all IEEE doubles, so
// `0` and `0.0` are the same value: a zero divisor errors regardless of how it
// is written, including 0/0.
//
// `/` is variadic and folds left-to-right (unary `(/ x)` means `1/x`), so any
// zero divisor anywhere in the chain must error, not just the two-argument
// case.
//
// Ranges are stripped from error messages: they point at the operators'
// definitions in prelude.scm and would shift with any edit to that file.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('/ rejects a zero divisor cleanly (#258)', () => {
  test('two-argument division by zero', async () => {
    expect(stripRange(await runProgram('(/ 1 0)'))).toEqual([
      'Runtime error [..]: (/) /: division by zero',
    ])
  })

  test('0 / 0 errors instead of returning NaN', async () => {
    expect(stripRange(await runProgram('(/ 0 0)'))).toEqual([
      'Runtime error [..]: (/) /: division by zero',
    ])
  })

  test('negative dividend by zero errors instead of returning -Infinity', async () => {
    expect(stripRange(await runProgram('(/ -1 0)'))).toEqual([
      'Runtime error [..]: (/) /: division by zero',
    ])
  })

  test('a zero divisor mid-chain errors', async () => {
    expect(stripRange(await runProgram('(/ 5 0 2)'))).toEqual([
      'Runtime error [..]: (/) /: division by zero',
    ])
  })

  test('unary (/ 0) is 1/0 and errors', async () => {
    expect(stripRange(await runProgram('(/ 0)'))).toEqual([
      'Runtime error [..]: (/) /: division by zero',
    ])
  })

  test('a zero divisor written as 0.0 also errors', async () => {
    expect(stripRange(await runProgram('(/ 1 0.0)'))).toEqual([
      'Runtime error [..]: (/) /: division by zero',
    ])
  })
})

describe('quotient/modulo/remainder reject a zero divisor cleanly (#258)', () => {
  test('quotient by zero errors instead of returning Infinity', async () => {
    expect(stripRange(await runProgram('(quotient 1 0)'))).toEqual([
      'Runtime error [..]: (quotient) quotient: division by zero',
    ])
  })

  test('modulo by zero errors instead of returning NaN', async () => {
    expect(stripRange(await runProgram('(modulo 1 0)'))).toEqual([
      'Runtime error [..]: (modulo) modulo: division by zero',
    ])
  })

  test('remainder by zero errors instead of returning NaN', async () => {
    expect(stripRange(await runProgram('(remainder 1 0)'))).toEqual([
      'Runtime error [..]: (remainder) remainder: division by zero',
    ])
  })
})

describe('valid divisions are unaffected (#258)', () => {
  test('/, quotient, modulo, remainder still compute normally', async () => {
    expect(
      await runProgram(`
      (/ 6 2)
      (/ 5 2)
      (/ 12 2 3)
      (/ 4)
      (modulo 7 3)
      (quotient 7 2)
      (remainder 7 3)
      `),
    ).toEqual(['3', '2.5', '2', '0.25', '1', '3', '1'])
  })
})
