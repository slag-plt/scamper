import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/679
//
// `(random n)` is documented as choosing from `0` to `n - 1`, which has no
// meaning for `n <= 0` -- and the native said nothing about it: `Math.floor
// (Math.random() * n)` answered a *negative* "random number" for a negative
// `n`, and `0`, the only value it could ever give, for `0`. A silently wrong
// answer rather than an error.
//
// A count to choose from is a *positive* integer, so this is the sibling of
// the size predicate #675 added rather than the same one: `make-list` of zero
// is the empty list, but there is no number to draw from zero candidates.
// `positive-integer?` is the predicate the docstring names, and the native
// carries the matching #553 guard so the raw `js-var` path says what it means
// too.

describe('a non-positive count for random (#679)', () => {
  test('a negative or zero count reports a contract violation', async () => {
    expect(await runProgram(`
(random -5)
(random 0)
`, { stripRanges: true })).toEqual([
      // Previously a negative integer, drawn at random from -5..-1.
      'Runtime error: (random) expected a positive-integer as the first argument, received number',
      // Previously 0, a value the empty range does not contain.
      'Runtime error: (random) expected a positive-integer as the first argument, received number',
    ])
  })

  // The narrowed contract must not cost the counts that were always
  // well-formed. One is the boundary: `(random 1)` is a legal call with
  // exactly one possible answer, and it has to keep giving it.
  test('a count of one, and of more than one, are unchanged', async () => {
    const log = await runProgram(`
(random 1)
(random 2)
`)
    expect(log[0]).toEqual('0')
    expect(['0', '1']).toContain(log[1])
  })

  // The non-numeric and non-integer rejections `integer?` already made are
  // kept; only the predicate's *name* in the message changes.
  test('a non-integer count is still turned away', async () => {
    expect(await runProgram(`
(random 2.5)
(random "3")
(random -0.5)
`, { stripRanges: true })).toEqual([
      'Runtime error: (random) expected a positive-integer as the first argument, received floating point number',
      'Runtime error: (random) expected a positive-integer as the first argument, received string',
      'Runtime error: (random) expected a positive-integer as the first argument, received floating point number',
    ])
  })

  // `js-var` is exported to user programs, so the raw native is reachable with
  // no contract at all -- and it is also how a library-internal call would
  // arrive (#553). The guard, not the docstring, is what answers there.
  test('the raw native path is guarded too', async () => {
    expect(await runProgram(`
((js-var "prelude_random") -5)
((js-var "prelude_random") 0)
`, { stripRanges: true })).toEqual([
      'Runtime error: (prelude_random) random: expected a positive integer',
      'Runtime error: (prelude_random) random: expected a positive integer',
    ])
  })

  test('positive-integer? is itself a predicate a student can call', async () => {
    expect(await runProgram(`
(positive-integer? 1)
(positive-integer? 5)
(positive-integer? 0)
(positive-integer? -1)
(positive-integer? 1.5)
(positive-integer? "a")
`)).toEqual(['#t', '#t', '#f', '#f', '#f', '#f'])
  })

  // The two predicates differ in exactly one place, and that place is why
  // `random` needed a second one rather than reusing #675's.
  test('positive-integer? and nonnegative-integer? differ only at zero', async () => {
    expect(await runProgram(`
(nonnegative-integer? 0)
(positive-integer? 0)
`)).toEqual(['#t', '#f'])
  })
})
