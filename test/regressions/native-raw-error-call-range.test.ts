import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/513
//
// When a library native threw a *raw Javascript* error -- not a ScamperError --
// applyFn wrapped it with the Ap op's own range. Inside a contract wrapper that
// range is the *wrapped definition's*, so the IDE underlined a line of the
// standard library:
//
//   Runtime error [90:1-90:54]: Unexpected error in Javascript function call:
//     SyntaxError: Invalid regular expression: /^[$/: ...
//
// for a three-line program -- 90 is `rex-matches?`'s `define-export` in
// src/lib/rex.scm. The ScamperError branch beside it already recovered the
// student's call site from the enclosing frame's `callRange` (#254/#239, see
// contract-error-call-site.test.ts); the raw-error branch was never updated to
// match.
//
// `rex` is the repro because it reaches this branch in any environment: its
// combinators don't validate their contents, so a bad pattern only surfaces
// when JS's RegExp constructor rejects it.

describe("#513: a native's raw Javascript error points at the call, not the library", () => {
  test('the range covers the offending call', async () => {
    expect(
      await runProgram('(import rex)\n(rex-matches? (regex "[") "a")'),
    ).toEqual([
      'Runtime error [2:1-2:30]: Unexpected error in Javascript function call: SyntaxError: Invalid regular expression: /^[$/: Unterminated character class',
    ])
  })

  test('the range tracks the statement it occurs in', async () => {
    expect(
      await runProgram(
        '(import rex)\n(rex-matches? (regex "a") "a")\n(rex-matches? (regex "[") "a")',
      ),
    ).toEqual([
      '#t',
      'Runtime error [3:1-3:30]: Unexpected error in Javascript function call: SyntaxError: Invalid regular expression: /^[$/: Unterminated character class',
    ])
  })

  test("a call inside a user function reports that call, not the function's", async () => {
    // The failing call sits at 2:23-2:42 inside f's body; f itself is invoked
    // from 3:1. Reporting f's own call site would be the coarser answer -- the
    // same blind spot the contract-error and blocking-primitive fixes had to
    // cover.
    expect(
      await runProgram(
        '(import rex)\n(define f (lambda (r) (rex-matches? r "a")))\n(f (regex "["))',
      ),
    ).toEqual([
      'Runtime error [2:23-2:42]: Unexpected error in Javascript function call: SyntaxError: Invalid regular expression: /^[$/: Unterminated character class',
    ])
  })

  test('a call inside a lambda passed to a higher-order function', async () => {
    expect(
      await runProgram(
        '(import rex)\n(map (lambda (s) (rex-matches? (regex "[") s)) (list "a"))',
      ),
    ).toEqual([
      'Runtime error [2:18-2:45]: Unexpected error in Javascript function call: SyntaxError: Invalid regular expression: /^[$/: Unterminated character class',
    ])
  })

  test('another native on the same path', async () => {
    expect(
      await runProgram('(import rex)\n(rex-split-string (regex "[") "abc")'),
    ).toEqual([
      'Runtime error [2:1-2:36]: Unexpected error in Javascript function call: SyntaxError: Invalid regular expression: /[/g: Unterminated character class',
    ])
  })
})
