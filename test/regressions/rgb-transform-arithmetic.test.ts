import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/259
//
// Four independent arithmetic bugs in src/js/image/color.ts, each pinned by a
// regression case below with exact hand-computed expected values.

describe('rgb-greyscale uses Rec.601 luma without a spurious /3 (#259)', () => {
  // Rec.601 weights (0.30, 0.59, 0.11) sum to 1.0, so the greyscale value is
  // just 0.30r + 0.59g + 0.11b. The old code divided by 3, so white came out
  // at 85 instead of 255.
  test('white maps to white (255), not ~85', async () => {
    expect(await runProgram(`
(import image)
(rgb-greyscale (rgb 255 255 255))
`)).toEqual(['(rgba 255 255 255 255)'])
  })

  test('pure red maps to 0.30*255 = 76.5', async () => {
    expect(await runProgram(`
(import image)
(rgb-greyscale (rgb 255 0 0))
`)).toEqual(['(rgba 76.5 76.5 76.5 255)'])
  })
})

describe('rgb-saturation passes (min, max) in the right order (#259)', () => {
  // Helper is 100*((max-min)/max). Args were swapped, so the "max === 0"
  // early-out mis-fired and (max-min) could go negative.
  test('fully-saturated red is 100, not 0', async () => {
    expect(await runProgram(`
(import image)
(rgb-saturation (rgb 255 0 0))
`)).toEqual(['100'])
  })

  test('(200 100 50) is 100*((200-50)/200) = 75, not -300', async () => {
    expect(await runProgram(`
(import image)
(rgb-saturation (rgb 200 100 50))
`)).toEqual(['75'])
  })

  test('an achromatic grey is 0', async () => {
    expect(await runProgram(`
(import image)
(rgb-saturation (rgb 100 100 100))
`)).toEqual(['0'])
  })
})

describe('rgb-thin lowers alpha by 32 clamped at 0 (#259)', () => {
  // Old code used Math.min(0, alpha-32), always <= 0. Correct is
  // Math.max(0, alpha-32).
  test('alpha 200 thins to 168, not 0', async () => {
    expect(await runProgram(`
(import image)
(rgb-thin (rgb 1 2 3 200))
`)).toEqual(['(rgba 1 2 3 168)'])
  })

  test('alpha 0 stays clamped at 0 (boundary)', async () => {
    expect(await runProgram(`
(import image)
(rgb-thin (rgb 1 2 3 0))
`)).toEqual(['(rgba 1 2 3 0)'])
  })
})

describe('rgb/color clamps components on both ends of [0, 255] (#259)', () => {
  // Old code only clamped from above (Math.min(x, 255)); negatives passed
  // through into color math and CSS rendering.
  test('a negative channel clamps to 0, not through', async () => {
    expect(await runProgram(`
(import image)
(color -10 0 0 255)
`)).toEqual(['(rgba 0 0 0 255)'])
  })

  test('an above-range channel still clamps to 255 (upper boundary)', async () => {
    expect(await runProgram(`
(import image)
(color 300 0 0 255)
`)).toEqual(['(rgba 255 0 0 255)'])
  })
})
