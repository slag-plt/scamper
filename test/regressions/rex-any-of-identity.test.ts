import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/533
//
// `rex-any-of` renders as `(?:a|b|c)`, so with no arguments at all it renders
// as `(?:)` -- the empty string, which matches at every position. Alternation's
// unit is the pattern that matches *nothing*: an empty alternation has no
// branch to succeed with, so it must fail on every input, including the empty
// one. `(rex-concat)` by contrast really is `""`, because the empty string is
// concatenation's unit.
//
// The rule the library follows after #492, #512 and #517 is *total iff there is
// a unit*, and alternation has one -- this is simply the wrong one -- so the
// fix is the returned pattern rather than the arity.
//
// Behaviour is asserted rather than the rendered pattern, so the spelling of
// "matches nothing" stays an implementation detail.

describe('the unit of rex-any-of (#533)', () => {
  test('an empty alternation matches nothing, not the empty string', async () => {
    expect(await runProgram(`
(import rex)
(rex-matches? (rex-any-of) "")
(rex-matches? (rex-any-of) "a")
(rex-matches? (rex-any-of) "anything")
(rex-find-matches (rex-any-of) "abc")
(rex-split-string (rex-any-of) "abc")
`)).toEqual([
      '#f',
      '#f',
      '#f',
      'null',
      '(list "abc")',
    ])
  })

  // The reachable path, and the same one #517 named: a fold whose list of
  // alternatives came out empty. Today the unmatchable branch silently
  // disappears and the enclosing pattern matches anyway.
  test('a folded-over empty list of alternatives cannot match', async () => {
    expect(await runProgram(`
(import rex)
(define pets null)
(rex-matches? (apply rex-any-of pets) "")
(rex-matches? (rex-concat (rex-string "a") (apply rex-any-of pets)) "a")
(rex-find-matches (rex-concat (rex-string "a") (apply rex-any-of pets)) "banana")
`)).toEqual([
      '#f',
      '#f',
      'null',
    ])
  })

  // A unit has to be absorbed by the operation it is the unit of, and has to
  // survive the other combinators: one repetition of nothing is impossible,
  // zero repetitions is the empty string, and an optional nothing is the empty
  // string.
  test('the unit composes as a unit', async () => {
    expect(await runProgram(`
(import rex)
(rex-matches? (rex-any-of (rex-any-of) (rex-string "a")) "a")
(rex-matches? (rex-any-of (rex-any-of) (rex-string "a")) "")
(rex-matches? (rex-repeat (rex-any-of)) "")
(rex-matches? (rex-repeat (rex-any-of)) "a")
(rex-matches? (rex-repeat-0 (rex-any-of)) "")
(rex-matches? (rex-repeat-0 (rex-any-of)) "a")
(rex-matches? (rex-optional (rex-any-of)) "")
(rex-matches? (rex-optional (rex-any-of)) "a")
`)).toEqual([
      '#t',
      '#f',
      '#f',
      '#f',
      '#t',
      '#f',
      '#t',
      '#f',
    ])
  })

  // The neighbouring fold, pinned so the decision is recorded rather than
  // rediscovered: `(rex-concat)` is already right, and the empty string it
  // returns is both concatenation's unit and exactly `(rex-empty)`.
  test('rex-concat keeps the empty string as its unit', async () => {
    expect(await runProgram(`
(import rex)
(rex-matches? (rex-concat) "")
(rex-matches? (rex-concat) "a")
(rex-matches? (rex-concat (rex-concat) (rex-string "a")) "a")
(rex-matches? (rex-empty) "")
`)).toEqual([
      '#t',
      '#f',
      '#t',
      '#t',
    ])
  })
})
