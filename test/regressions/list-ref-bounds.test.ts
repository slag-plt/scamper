import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/678
//
// `list-ref` only ever checked the upper end. Its walk is
// `while (l !== null && i > 0)`, which a negative `i` never enters, so
// `(list-ref (list 1 2 3) -1)` returned the *head*: an off-by-one that reached
// -1 read element 0 and the program carried on being wrong, with a plausible
// value and no signal. The docstring already promises `0 <= n < (length l)`,
// and the siblings `string-ref`, `vector-ref` and `vector-set!` all reject a
// negative index with the bounds message below.
//
// Reported in the one phrasing this family shares,
// `<name>: index <n> out of bounds of <collection>`, and from inside the native
// rather than from a contract: a contract predicate cannot see the list's
// length, so it could only ever catch the negative end and would give one
// mistake two different message shapes.
//
// The legal range is pinned below. Unlike the take/drop/tail family,
// `list-ref` does *not* accept `n == (length l)` -- there is no element there.
//
// Ranges are stripped: this test is about the bounds *message*, not the
// location, which contract-error-call-site.test.ts asserts.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('list-ref rejects a negative index (#678)', () => {
  test('a negative index no longer answers the head', async () => {
    expect(stripRange(await runProgram(`
(list-ref (list 1 2 3) -1)
(list-ref (list 1 2 3) -5)
`))).toEqual([
      'Runtime error [..]: (list-ref) list-ref: index -1 out of bounds of list',
      'Runtime error [..]: (list-ref) list-ref: index -5 out of bounds of list',
    ])
  })

  // Unchanged, and pinned here so the two ends read as one rule.
  test('the upper end still reports, as it always did', async () => {
    expect(stripRange(await runProgram(`
(list-ref (list 1 2 3) 3)
(list-ref (list 1 2 3) 10)
(list-ref null 0)
`))).toEqual([
      'Runtime error [..]: (list-ref) list-ref: index 3 out of bounds of list',
      'Runtime error [..]: (list-ref) list-ref: index 10 out of bounds of list',
      'Runtime error [..]: (list-ref) list-ref: index 0 out of bounds of list',
    ])
  })

  test('the legal indices are unchanged, boundaries included', async () => {
    expect(await runProgram(`
(list-ref (list 1 2 3) 0)
(list-ref (list 1 2 3) 2)
(list-ref (list 1) 0)
`)).toEqual(['1', '3', '1'])
  })

  test('a non-integer index is still the integer? contract', async () => {
    expect(stripRange(await runProgram('(list-ref (list 1 2 3) 1.5)'))).toEqual([
      'Runtime error [..]: (list-ref) expected an integer as the second argument, received floating point number',
    ])
  })

  // `js-var` is exported to user programs, so the native is reachable with no
  // contract at all. The check lives in the native, so it answers there too.
  test('js-var reaches the same bounds check', async () => {
    expect(stripRange(await runProgram(
      '((js-var "prelude_listRef") (list 1 2 3) -1)',
    ))).toEqual([
      'Runtime error [..]: (prelude_listRef) list-ref: index -1 out of bounds of list',
    ])
  })
})
