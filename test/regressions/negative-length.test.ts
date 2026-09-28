import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/675
//
// `make-list`, `make-vector` and `make-string` each take a *size*, and each
// documented it as `integer?` -- a predicate `-5` satisfies. What followed
// depended only on how the native happened to be written: `make-list` and
// `make-vector` loop `for (i = 0; i < n; i++)`, so a negative size skipped the
// loop and answered the empty result with no complaint, while `make-string`
// forwards to `String.prototype.repeat` and leaked the host's own words,
// "Unexpected error in Javascript function call: RangeError: Invalid count
// value: -3".
//
// A size is a non-negative integer, so that is now a predicate a docstring can
// name: `nonnegative-integer?`, user-facing and documented in the style of
// `nonempty-list?`, which exists for the same reason -- `car`/`cdr`'s contract
// needed a notion the numeric predicates did not supply. The three contracts
// name it, and the three natives carry the matching #553 guard so the raw
// `js-var` path says what it means too.

describe('a negative size (#675)', () => {
  test('the three size-taking procedures report a contract violation', async () => {
    expect(await runProgram(`
(make-list -5 "Clod")
(make-vector -3 0)
(make-string -3 #\\a)
`, { stripRanges: true })).toEqual([
      'Runtime error: (make-list) expected a nonnegative-integer as the first argument, received number',
      'Runtime error: (make-vector) expected a nonnegative-integer as the first argument, received number',
      // Previously "RangeError: Invalid count value: -3", in the host's words.
      'Runtime error: (make-string) expected a nonnegative-integer as the first argument, received number',
    ])
  })

  // The narrowed contract must not cost the sizes that were always well-formed.
  // Zero is the interesting one: an empty result is the right answer to a
  // request for nothing, and only a *negative* request is the mistake.
  test('zero and positive sizes are unchanged', async () => {
    expect(await runProgram(`
(make-list 0 "a")
(make-vector 0 0)
(make-string 0 #\\a)
(make-list 2 "a")
(make-vector 2 0)
(make-string 2 #\\a)
`)).toEqual([
      'null',
      '(vector)',
      '""',
      '(list "a" "a")',
      '(vector 0 0)',
      '"aa"',
    ])
  })

  // `js-var` is exported to user programs, so the raw native is reachable with
  // no contract at all -- and it is also how a library-internal call arrives
  // (#553). The guard, not the docstring, is what answers there.
  test('the raw native path is guarded too', async () => {
    expect(await runProgram(`
((js-var "prelude_makeString") -3 #\\a)
((js-var "prelude_makeList") -5 "Clod")
((js-var "prelude_makeVector") -3 0)
`, { stripRanges: true })).toEqual([
      'Runtime error: (prelude_makeString) make-string: expected a nonnegative integer',
      'Runtime error: (prelude_makeList) make-list: expected a nonnegative integer',
      'Runtime error: (prelude_makeVector) make-vector: expected a nonnegative integer',
    ])
  })

  test('nonnegative-integer? is itself a predicate a student can call', async () => {
    expect(await runProgram(`
(nonnegative-integer? 0)
(nonnegative-integer? 5)
(nonnegative-integer? -1)
(nonnegative-integer? 1.5)
(nonnegative-integer? "a")
`)).toEqual(['#t', '#t', '#f', '#f', '#f'])
  })

  // Deliberately unchanged siblings, pinned so each reads as a decision rather
  // than an oversight. Both are tracked as follow-up work; neither belongs to
  // "a size is a non-negative integer".
  //
  // `range`'s negative end is not a size but a *bound*, and an empty range is
  // the correct answer to a bound below the start, as in Python.
  //
  // `list-take`/`list-drop` clamp at both ends -- a negative count and one past
  // the list's length -- and the upper bound is the same decision `substring`
  // settled in #645, so all three bounds want deciding together rather than
  // one of them changing here.
  test('range and the clamping list operations keep their current answers', async () => {
    expect(await runProgram(`
(range -4)
(list-take (list 1 2 3) -1)
(list-drop (list 1 2 3) -1)
`)).toEqual(['null', 'null', '(list 1 2 3)'])
  })
})
