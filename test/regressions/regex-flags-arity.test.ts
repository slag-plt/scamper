import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/467
//
// `regex` documented a `flags` parameter its native never had --
// `rex_rexRegex` (src/js/rex/index.ts) takes the pattern alone. A docstring is
// what the contract layer builds a binding's wrapper from, so the documented
// arity was the real one: `(regex "colou?r")`, the only sensible call, was an
// arity error, while `(regex "colou?r" "g")` succeeded with the flags silently
// dropped. Same failure mode as #455.

test('regex takes a pattern alone (#467)', async () => {
  expect(await runProgram(`
  (import rex)
  (rex-matches? (regex "colou?r") "colour")
  (rex-matches? (regex "colou?r") "colouur")
  (rex->string (regex "colou?r"))
  `)).toEqual([
    '#t',
    '#f',
    '"colou?r"',
  ])
})

test('regex rejects a second argument rather than dropping it (#467)', async () => {
  expect(await runProgram(`
  (import rex)
  (regex "colou?r" "g")
  `, { stripRanges: true })).toEqual([
    'Runtime error: Arity mismatch in function call: expected 1 arguments, got 2',
  ])
})
