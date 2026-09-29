import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/677
//
// The prelude's numeric `square` and the image library's shape constructor
// `square` shared one name, and an import that re-binds a library name raises
// no diagnostic where a `define` of it would (src/scheme/scope.ts), so
// `(import image)` replaced the numeric one without a word. A student who then wrote `(square 5)` got an arity
// error about arguments they had never heard of. The numeric one is `sqr` now,
// following Racket's `racket/math`, so the two names coexist.

test('sqr squares a number even with image imported', async () => {
  expect(
    await runProgram(`
(import image)
(sqr 5)
(square 5 "solid" "red")
`),
  ).toEqual(['25', '(rectangle 5 5 "solid" (rgba 255 0 0 255))'])
})

test('square is the image shape alone', async () => {
  expect(
    await runProgram(`
(sqr 5)
(square 5)
`),
  ).toEqual(['25', 'Runtime error: Variable not found: square'])
})
