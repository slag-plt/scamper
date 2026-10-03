import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/680
//
// `(repeat n comp)` is written as a recursion on `n` that stops at `n === 0`,
// so a negative `n` never reached its base case: the student saw "Unexpected
// error in Javascript function call: RangeError: Maximum call stack size
// exceeded", which says nothing about their program. Its contract documented
// `n` as `integer?`, a predicate `-3` satisfies.
//
// A repetition count is a non-negative integer, which is exactly what #675
// named `nonnegative-integer?` for. Zero stays legal and keeps its meaning --
// repeating something no times is the empty composition -- and only a negative
// count is the mistake.
//
// The predicate lives in prelude and is *not* re-exported by music: a library
// contract's check resolves against the importing program's environment, where
// prelude is always present (see mkInitialEnv), so naming it here is enough.
// That is unlike `fill-mode?`/`font?`, which canvas does re-export, because
// their home module is one a student need not have imported.

describe('a negative count for music\'s repeat (#680)', () => {
  test('a negative count reports a contract violation', async () => {
    expect(await runProgram(`
(import music)
(repeat -3 (note 60 qn))
`, { stripRanges: true })).toEqual([
      // Previously "Unexpected error in Javascript function call: RangeError:
      // Maximum call stack size exceeded", in the host's own words.
      'Runtime error: (repeat) expected a nonnegative-integer as the first argument, received number',
    ])
  })

  // Zero is the boundary that must keep working: repeating a composition no
  // times is the empty composition, a well-formed answer rather than a
  // mistake. One and two are pinned beside it so the shape of the result is
  // not quietly changed either.
  test('zero and positive counts are unchanged', async () => {
    expect(await runProgram(`
(import music)
(repeat 0 (note 60 qn))
(repeat 1 (note 60 qn))
(repeat 2 (note 60 qn))
`)).toEqual([
      '(empty)',
      '(seq (vector (note 60 (dur 1 4)) (empty)))',
      '(seq (vector (note 60 (dur 1 4)) (seq (vector (note 60 (dur 1 4)) (empty)))))',
    ])
  })

  // The non-integer rejections `integer?` already made are kept; only the
  // predicate's *name* in the message changes.
  test('a non-integer count is still turned away', async () => {
    expect(await runProgram(`
(import music)
(repeat 2.5 (note 60 qn))
(repeat "2" (note 60 qn))
`, { stripRanges: true })).toEqual([
      'Runtime error: (repeat) expected a nonnegative-integer as the first argument, received floating point number',
      'Runtime error: (repeat) expected a nonnegative-integer as the first argument, received string',
    ])
  })

  // `js-var` is exported to user programs, so the raw native is reachable with
  // no contract at all -- and it is also how a library-internal call would
  // arrive (#553). The guard, not the docstring, is what answers there, and it
  // is the only thing standing between a negative count and the host's stack.
  test('the raw native path is guarded too', async () => {
    expect(await runProgram(`
(import music)
((js-var "music_repeat") -3 (note 60 qn))
`, { stripRanges: true })).toEqual([
      'Runtime error: (music_repeat) repeat: expected a nonnegative integer',
    ])
  })

  // The predicate music's contract names belongs to prelude, and music does
  // not re-export it: a program that imports music sees `repeat` but no new
  // predicate, and the contract still fires. The error above naming
  // `nonnegative-integer` is that resolution working; this pins the other half,
  // that nothing was added to music's own exports to make it work.
  test('the prelude predicate resolves with no re-export from music', async () => {
    expect(await runProgram(`
(import music)
(nonnegative-integer? 0)
(nonnegative-integer? -1)
`)).toEqual(['#t', '#f'])
  })
})
