import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/250
//
// `hsv?` was bound to the `hsv` constructor rather than an actual predicate.
// The auto-generated contract layer invokes `hsv?` to validate every
// parameter declared `hsv?`, so it called the constructor with a single
// struct argument and every hsv accessor/derived function errored with
// `(hsv?) hsv: expects 3 or 4 arguments, but got 1`.

test('hsv? is a predicate and hsv-typed contracts pass', async () => {
  expect(await runProgram(`
  (import image)
  (hsv? (hsv 180 50 50))
  (hsv? 5)
  (hsv-hue (hsv 180 50 50))
  (hsv-saturation (hsv 180 50 50))
  (hsv-value (hsv 180 50 50))
  (hsv-alpha (hsv 180 50 50))
  `)).toEqual([
    '#t',
    '#f',
    '180',
    '50',
    '50',
    '255',
  ])
})
