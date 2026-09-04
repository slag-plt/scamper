import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/493
//
// `MatchHandler` threw its inexhaustive-match error with no range at all, so
// the IDE underlined nothing and a student with several `match` expressions in
// a file was not told which one ran out of cases:
//
//   Runtime error: Inexhaustive pattern match failure
//
// The op's own range (the whole `match` form) is the answer for a match the
// student wrote -- but a library's own `match` carries a range into
// prelude.scm, which is worse than none, so a builtin frame reports the call
// site instead, exactly as applyFn already does for a JS primitive's error.

describe('an inexhaustive match reports where it is (#493)', () => {
  test('the range covers the match form, not the statement around it', async () => {
    expect(await runProgram('(+ 1 (match 5 [0 0]))')).toEqual([
      'Runtime error [1:6-1:20]: Inexhaustive pattern match failure',
    ])
  })

  test("a library's own match reports the student's call, not prelude.scm", async () => {
    expect(await runProgram('(reduce-right + null)')).toEqual([
      'Runtime error [1:1-1:21]: Inexhaustive pattern match failure',
    ])
  })

  // The frame's `origin`, not its `hidden` flag, is what decides this: a
  // library function applying the student's lambda hides that frame from the
  // trace but leaves its origin 'user' (see Frame.hidden). Reading `hidden`
  // here would blame the whole `(map ...)` call instead of the match inside it.
  test("a student's match inside a library call reports their own match", async () => {
    expect(
      await runProgram('(map (lambda (x) (match x [0 "zero"])) (list 1))'),
    ).toEqual(['Runtime error [1:18-1:37]: Inexhaustive pattern match failure'])
  })
})
