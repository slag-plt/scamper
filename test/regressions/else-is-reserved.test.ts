import { describe, expect, test } from 'vitest'
import { runProgram } from '../libs/harness.js'
import { parse } from '../scheme/parsing/test-utils.js'

// https://github.com/slag-plt/scamper/issues/639
//
// `else` used to be an ordinary exported constant bound to `#t`, and `cond` had
// no notion of it: `[else ...]` worked only because `else` evaluated to `#t`.
// So `(define else #f)` was legal, silent, and broke every `cond` that followed
// -- with the error pointing at the `cond` rather than at the `define`, which
// could be pages away.
//
// `else` is a reserved word now, and `cond`'s grammar has a `[else EXPR]`
// clause of its own. The mistake is a parse error where it is made.

describe('else is a reserved word (#639)', () => {
  test('rebinding else is a parse error at the define, not a later cond', async () => {
    expect(
      await runProgram(`
(define else #f)
(display (cond [(= 1 2) "a"] [else "fell through"]))
`),
    ).toEqual([
      'Parser error: Malformed define statement (a name and a value).',
    ])
  })

  test('else is not a value either', async () => {
    // `(display else)` printed `#t` before this change.
    expect(await runProgram('(display else)')).toEqual([
      'Parser error: Malformed display statement (a value to display).',
    ])
  })

  test('a final [else ...] clause still catches everything left', async () => {
    expect(
      await runProgram(`
(display (cond [(= 1 2) "one"] [(= 2 2) "two"] [else "neither"]))
(display (cond [(= 1 2) "one"] [(= 2 3) "two"] [else "neither"]))
(display (cond [else "only"]))
`),
    ).toEqual(['"two"', '"neither"', '"only"'])
  })

  test('a cond with no else still raises when nothing matches', async () => {
    expect(await runProgram('(display (cond [(= 1 2) "a"]))')).toEqual([
      'Runtime error: (error) No matching clause in cond',
    ])
  })

  // An else clause anywhere but last is rejected outright, rather than being
  // accepted and quietly making the clauses after it dead code. That falls out
  // of the grammar -- `CondElseClause` is the optional *last* child of `Cond`,
  // so there is no shape for a second one or an earlier one -- which is why it
  // is a parse error rather than a check written by hand.
  test('an else clause that is not last is rejected', () => {
    for (const src of [
      '(cond [else 1] [#t 2])',
      '(cond [else 1] [else 2])',
      '(cond [#t 1] [else 2] [#f 3])',
    ]) {
      expect(parse(src).errors.length, src).toBeGreaterThan(0)
    }
  })

  test('the else body may itself be a cond', async () => {
    // The two are told apart by provenance when a run is displayed, so a
    // nested cond does not get folded into its parent's clause list.
    expect(
      await runProgram('(display (cond [#f 1] [else (cond [#f 2] [else 3])]))'),
    ).toEqual(['3'])
  })
})
