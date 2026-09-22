import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/609
//
// `rgb-component?` is documented as "an integer between 0 and 255", but the two
// colour procedures that divide -- `rgb-greyscale` (Rec.601 luma) and
// `rgb-average` (the midpoint of two colours) -- handed back fractional
// components. Rounding, not truncation: the reported case is an already-grey
// colour whose luma comes out at 31.999999999999996, which truncates to 31.

describe('rgb-greyscale returns whole components (#609)', () => {
  test('an already-grey colour is unchanged, not 31.999999999999996', async () => {
    expect(await runProgram(`
(import image)
(rgb-greyscale (rgb 32 32 32))
`)).toEqual(['(rgba 32 32 32 255)'])
  })

  test('pure red rounds 0.30*255 = 76.5 up to 77', async () => {
    expect(await runProgram(`
(import image)
(rgb-greyscale (rgb 255 0 0))
`)).toEqual(['(rgba 77 77 77 255)'])
  })

  test('the weights still sum to 1, so white stays white', async () => {
    expect(await runProgram(`
(import image)
(rgb-greyscale (rgb 255 255 255))
`)).toEqual(['(rgba 255 255 255 255)'])
  })
})

describe('rgb-average returns whole components (#609)', () => {
  test('an odd sum rounds rather than landing on .5', async () => {
    expect(await runProgram(`
(import image)
(rgb-average (rgb 1 2 3) (rgb 4 5 6))
`)).toEqual(['(rgba 3 4 5 255)'])
  })

  test('alpha is averaged and rounded too', async () => {
    expect(await runProgram(`
(import image)
(rgb-average (rgb 0 0 0 0) (rgb 0 0 0 1))
`)).toEqual(['(rgba 0 0 0 1)'])
  })

  test('an even sum is exact', async () => {
    expect(await runProgram(`
(import image)
(rgb-average (rgb 10 20 30) (rgb 20 40 60))
`)).toEqual(['(rgba 15 30 45 255)'])
  })
})

describe('drawing-color averages children into whole components (#609)', () => {
  // `drawing-color` folds a composite's children with `color_rgbAverage`, so it
  // inherited the fractional result: red beside blue reported 127.5.
  test('red beside blue is a whole purple', async () => {
    expect(await runProgram(`
(import image)
(drawing-color (beside (rectangle 10 10 "solid" "red") (rectangle 5 5 "solid" "blue")))
`)).toEqual(['(rgba 128 0 128 255)'])
  })
})
