import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/446
//
// `font` and `text` wrote "optional" in their docstrings' prose, where nothing
// reads it, so contract insertion gave `font` a fixed arity of four and
// `(font "Arial")` -- the documented default of `text` -- was an arity error.
// Every parameter was declared `any` with its real predicate in the prose too,
// so nothing was checked, and `text`'s one optional font was declared as a
// rest parameter and policed by hand inside `drawing_text`.
//
// Both are now written with #435's bracketed optionals and real predicates.

describe('font', () => {
  test('takes its last three arguments optionally (#446)', async () => {
    expect(
      await runProgram(`
(import image)
(font "Arial")
(font "Arial" "serif")
(font "Arial" "serif" #t)
(font "Arial" "serif" #t #t)
`),
    ).toEqual([
      '(font "Arial" "sans-serif" #f #f)',
      '(font "Arial" "serif" #f #f)',
      '(font "Arial" "serif" #t #f)',
      '(font "Arial" "serif" #t #t)',
    ])
  })

  test('checks each argument against its own predicate (#446)', async () => {
    expect(
      await runProgram(
        `
(import image)
(font 5)
(font "Arial" 5)
(font "Arial" "serif" "yes")
(font "Arial" "serif" #t "no")
(font "Arial" "serif" #t #t #t)
`,
        { stripRanges: true },
      ),
    ).toEqual([
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a string, received number',
      'Runtime error: (error) expected a boolean, received string',
      'Runtime error: (error) expected a boolean, received string',
      'Runtime error: (font) Arity mismatch in function call: expected at most 4 arguments, got 5',
    ])
  })
})

describe('text', () => {
  // vitest-canvas-mock's measureText reports width = text length and zero
  // ascent/descent, so "hi" is a deterministic 2x1 box under jsdom.
  test('takes its font optionally rather than as a rest parameter (#446)', async () => {
    expect(
      await runProgram(`
(import image)
(text "hi" 12 "black")
(text "hi" 12 "black" (font "Georgia" "serif" #t #f))
`),
    ).toEqual([
      '(text 2 1 "hi" 12 (rgba 0 0 0 255) (font "Arial" "sans-serif" #f #f))',
      '(text 2 1 "hi" 12 (rgba 0 0 0 255) (font "Georgia" "serif" #t #f))',
    ])
  })

  test('rejects a non-font fourth argument rather than dropping it (#446)', async () => {
    expect(
      await runProgram(
        `
(import image)
(text "hi" 12 "black" 5)
(text "hi" "big" "black")
(text "hi" 12 "black" (font "Arial") 9)
`,
        { stripRanges: true },
      ),
    ).toEqual([
      'Runtime error: (error) expected a font, received number',
      'Runtime error: (error) expected a number, received string',
      'Runtime error: (text) Arity mismatch in function call: expected at most 4 arguments, got 5',
    ])
  })
})
