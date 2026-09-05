import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/521
//
// `LetHandler` threw its pattern-mismatch error with no range at all, so the
// IDE underlined nothing and the message arrived unlocated:
//
//   Runtime error: let: value did not match pattern (pair x y)
//
// Same defect as #493's inexhaustive `match`, but `let` can do better than
// that fix did: `idx` names the binding being filled, so the range is the
// failing *binder's*, not the whole `let` form -- a `let` with several
// bindings otherwise underlines all of them and says which one failed only in
// prose. A `let` in library source still has no site the student can open, so
// a builtin frame reports the call site, as `applyFn` and `MatchHandler` do.

describe('a let binding that does not match reports where it is (#521)', () => {
  test('the range covers the failing binder, not the let form around it', async () => {
    expect(await runProgram('(+ 1 (let ([(pair x y) 5]) x))')).toEqual([
      'Runtime error [1:13-1:22]: let: value did not match pattern (pair x y)',
    ])
  })

  // The whole-form range #493 used would be [1:1-4:10] here, covering all
  // three binders; only the binder's own range says which one failed.
  test('a multi-binding let picks out the binding that failed', async () => {
    expect(
      await runProgram(`(let ([a 1]
      [(pair x y) 5]
      [b 2])
  (+ a b))`),
    ).toEqual([
      'Runtime error [2:8-2:17]: let: value did not match pattern (pair x y)',
    ])
  })

  // The frame's `origin`, not its `hidden` flag, is what decides this: a
  // library function applying the student's lambda hides that frame from the
  // trace but leaves its origin 'user' (see Frame.hidden). Reading `hidden`
  // here would blame the whole `(map ...)` call, [1:1-1:54], instead of the
  // binder inside it.
  test("a let inside a library call reports the student's own binder", async () => {
    expect(
      await runProgram('(map (lambda (p) (let ([(pair x y) p]) x)) (list 1 2))'),
    ).toEqual([
      'Runtime error [1:25-1:34]: let: value did not match pattern (pair x y)',
    ])
  })
})

// N.B., no case for the builtin arm: no `.scm` in src/lib binds a refutable
// pattern in a `let`, so nothing in today's library can reach it. It is there
// for the same reason `MatchHandler`'s is -- the day one does, the range must
// not point into prelude.scm.
