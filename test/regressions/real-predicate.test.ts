import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/116
//
// `real?` was defined as `typeof x === 'number' && !Number.isInteger(x)`,
// which is backwards: it returned #t only for non-integer numbers (and, since
// `Number.isInteger` is false for them, also for NaN/Infinity) and #f for
// integers. The correct meaning is "finite number", so it is now
// `typeof x === 'number' && Number.isFinite(x)`: #t for every finite number
// (integer or not), #f for NaN, ±Infinity, and non-numbers.
//
// Scamper numbers are all IEEE doubles, so `5` and `5.0` are the same value.

test('real? is #t for all finite numbers, #f for NaN/Infinity/non-numbers (#116)', async () => {
  expect(await runProgram(`
  (real? 5)
  (real? 5.0)
  (real? 2.5)
  (real? -3)
  (real? 0)
  (real? (sqrt -1))
  (real? (expt 10 1000))
  (real? "x")
  (real? (list))
  (real? #t)
  (real? null)
  `)).toEqual([
    '#t', // integer
    '#t', // whole float (same value as 5)
    '#t', // non-integer finite
    '#t', // negative integer
    '#t', // zero
    '#f', // NaN
    '#f', // Infinity
    '#f', // string
    '#f', // list
    '#f', // boolean
    '#f', // null
  ])
})
