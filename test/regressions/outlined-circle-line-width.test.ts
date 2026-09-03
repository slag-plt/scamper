import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/460
//
// #431 gave every outlined shape a `[line-width]` defaulting to 1 but left
// `outlined-circle`'s required, so it was the one shape in the family that had
// to be told. It is optional now too: `(outlined-circle 20 "red")` draws a
// hairline circle rather than raising an arity error.

test('outlined-circle takes an optional line width, defaulting to 1 (#460)', async () => {
  expect(
    await runProgram(`
  (import image)
  (outlined-circle 20 "red")
  (outlined-circle 20 "red" 1)
  (drawing-width (outlined-circle 20 "red"))
  `),
  ).toEqual([
    '(ellipse 21 21 "outline" (rgba 255 0 0 255) 1)',
    '(ellipse 21 21 "outline" (rgba 255 0 0 255) 1)',
    '21',
  ])
})

test('a line width that is given is still checked (#460)', async () => {
  // Optional is not unchecked: a supplied width still has to satisfy the
  // docstring's contract, and a fourth argument is still one too many.
  expect(
    await runProgram(
      `
  (import image)
  (outlined-circle 20 "red" "wide")
  (outlined-circle 20 "red" 1 2)
  `,
      { stripRanges: true },
    ),
  ).toEqual([
    'Runtime error: (error) expected a number, received string',
    'Runtime error: (outlined-circle) Arity mismatch in function call: expected at most 3 arguments, got 4',
  ])
})

// The pen stays *centred* on the diameter it is given, so an outlined shape's
// box is its size plus one line width -- 20 with a pen 4 wide is 24 across,
// leaving a 16-wide hole. #460 also asked for the pen to sit wholly outside
// the stated diameter (making it 28 across), and that half was declined: the
// reporter withdrew it, and #431's "the outline is drawn centred on the size
// you give" shipped in 4.2.0, so reversing it would be the third size change
// to these functions in consecutive releases. This pins that as a decision
// rather than an accident -- revisiting the geometry means changing this test
// deliberately.
test('the outline stays centred on the given diameter (#431, declined in #460)', async () => {
  expect(
    await runProgram(`
  (import image)
  (drawing-width (outlined-circle 20 "red" 4))
  (drawing-height (outlined-circle 20 "red" 4))
  `),
  ).toEqual(['24', '24'])
})
