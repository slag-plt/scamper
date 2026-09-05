import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/517
//
// `-` and `/` were documented with a rest parameter alone -- `(- & v1)`,
// `(/ & v1)` -- so the generated contract admitted a call with no arguments at
// all, and `prelude_minus`/`prelude_div` reduce without an initial value: `(-)`
// and `(/)` leaked "TypeError: Reduce of empty array with no initial value".
//
// The opposite of #492's fix, and for the opposite reason. A variadic is total
// at zero arguments when it has a unit to return: `(+)` is 0, `(append)` is the
// empty list, and #492 widened those natives to say so. Subtraction and
// division have none -- there is no value `(-)` could sensibly be, which is
// also why R7RS requires one argument of each -- so the docstrings are narrowed
// instead, `(- v1 & v2)`, and the contract turns the call away with an ordinary
// arity error pointing at the student's own code.

describe('zero-argument - and / (#517)', () => {
  test('- and / report an arity error rather than a Javascript TypeError', async () => {
    expect(await runProgram(`
(-)
(/)
`)).toEqual([
      'Runtime error [1:1-1:3]: Arity mismatch in function call: expected 1 arguments, got 0',
      'Runtime error [2:1-2:3]: Arity mismatch in function call: expected 1 arguments, got 0',
    ])
  })

  // The narrowed contract must not cost the one-argument forms, which R7RS
  // defines as negation and reciprocal, nor the ordinary left folds.
  test('one argument still means negation and reciprocal', async () => {
    expect(await runProgram(`
(- 5)
(/ 2)
(- 10 3 2)
(/ 100 5 2)
(+)
(*)
`)).toEqual(['-5', '0.5', '5', '10', '0', '1'])
  })

  // A sibling found in the same sweep. `beside`/`above`/`overlay` take the
  // largest of their children's widths or heights via `Math.max(...)`, which
  // is `-Infinity` over no children -- the identity for max over the reals,
  // but not over a drawing's non-negative dimensions. The empty drawing is
  // 0x0, and it has to be, because the enclosing combinator *sums* the other
  // dimension: `(above (beside) shape)` inherited the -Infinity height and the
  // whole picture rendered as a 0x0 canvas, losing `shape` with no error.
  //
  // Fixed in the natives rather than the docstrings because, unlike `-`, these
  // do have a unit: the empty image. Keeping them total is what lets
  // `(apply beside imgs)` fold over a possibly-empty list, and it fixes the
  // `/align` variants for free, since they share these primitives.
  test('an empty image combinator is the empty image, not a degenerate one', async () => {
    expect(await runProgram(`
(import image)
(list (drawing-width (beside)) (drawing-height (beside)))
(list (drawing-width (above)) (drawing-height (above)))
(list (drawing-width (overlay)) (drawing-height (overlay)))
(list (drawing-width (beside/align "center")) (drawing-height (beside/align "center")))
(drawing-height (above (beside) (circle 20 "solid" "red")))
(drawing-width (beside (above) (circle 20 "solid" "red")))
`)).toEqual([
      '(list 0 0)',
      '(list 0 0)',
      '(list 0 0)',
      '(list 0 0)',
      '20',
      '20',
    ])
  })

  // Deliberately unchanged siblings, pinned so the decision is recorded rather
  // than rediscovered. The same rule keeps them as they are: each has a unit.
  // `-Infinity`/`Infinity` really are max's and min's, and they compose --
  // `(max (apply max null) 4)` is 4 -- so these stay total like `(+)`, `(*)`,
  // `(string)` and `(append)`, R7RS's stricter arity notwithstanding. An empty
  // `par`/`seq` is likewise a well-formed composition, the same silence
  // `(empty)` already names.
  test('max, min, par and seq stay total at zero arguments', async () => {
    expect(await runProgram(`
(import music)
(max)
(min)
(max (apply max null) 4)
(composition? (par))
(composition? (seq))
`)).toEqual(['-Infinity', 'Infinity', '4', '#t', '#t'])
  })
})
