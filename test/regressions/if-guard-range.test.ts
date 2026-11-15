import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/753
//
// `IfHandler` threw its boolean-guard error with no range at all, so the IDE
// underlined nothing and a student with several branches in a file was not
// told which guard was not a boolean:
//
//   Runtime error: if: expected a boolean guard, received number
//
// Same defect as #493's inexhaustive `match` and #521's `let` pattern, and the
// same answer: the op's own range for an `if` the student wrote, the call site
// for one in library source, since a range into prelude.scm is worse than
// none.

describe('a non-boolean if guard reports where it is (#753)', () => {
  test('the range covers the if form, not the statement around it', async () => {
    expect(await runProgram('(+ 1 (if 5 1 2))')).toEqual([
      'Runtime error [1:6-1:15]: if: expected a boolean guard, received number',
    ])
  })

  // `and` and `or` expand to nested `if` carrying the whole form's range
  // (expansion.ts, the 'and' and 'or' cases), so this raise site is how a
  // student meets `(and 1 2)` -- far more often than a literal `(if 5 ...)`.
  test('an and-form expanded to if reports the and-form', async () => {
    expect(await runProgram('(and #t 2)')).toEqual([
      'Runtime error [1:1-1:10]: and: expected a boolean guard, received number',
    ])
  })

  test('an or-form expanded to if reports the or-form', async () => {
    expect(await runProgram('(or #f 5)')).toEqual([
      'Runtime error [1:1-1:9]: or: expected a boolean guard, received number',
    ])
  })

  // KNOWN LIMITATION, pinned so a change to it is deliberate rather than a
  // surprise. Every clause of a `cond` expands to an `if` carrying the whole
  // `cond`'s range (expansion.ts, the 'cond' case), so a multi-clause `cond` --
  // the shape students actually write -- underlines all of its clauses and says
  // which guard failed only in prose. This answers #753 between forms but not
  // within one `cond`, and it is the opposite of the choice `LetHandler` makes
  // (`binding.pat.range`, the failing binder rather than the whole `let`).
  // Narrowing it needs an expansion or ops change and is a follow-up, not this
  // test's expectation changing quietly.
  test('a cond underlines the whole form, not the clause that failed', async () => {
    expect(
      await runProgram(`(cond
  [(= 1 2) 1]
  [(= 1 3) 2]
  [5 3]
  [else 4])`),
    ).toEqual([
      'Runtime error [1:1-5:11]: cond: expected a boolean guard, received number',
    ])
  })

  // `all-satisfy?` guards on `(pred? (car lst))` in prelude.scm, so a
  // predicate that returns a non-boolean reaches a library `if`. The builtin
  // arm must blame the student's call rather than the library line.
  test("a library's own if reports the student's call, not prelude.scm", async () => {
    expect(await runProgram('(all-satisfy? (lambda (x) 5) (list 1 2))')).toEqual([
      'Runtime error [1:1-1:40]: if: expected a boolean guard, received number',
    ])
  })

  // The frame's `origin`, not its `hidden` flag, is what decides this: a
  // library function applying the student's lambda hides that frame from the
  // trace but leaves its origin 'user' (see Frame.hidden). Reading `hidden`
  // here would blame the whole `(map ...)` call instead of the if inside it.
  test("a student's if inside a library call reports their own if", async () => {
    expect(
      await runProgram('(map (lambda (x) (if x 1 2)) (list 1))'),
    ).toEqual([
      'Runtime error [1:18-1:27]: if: expected a boolean guard, received number',
    ])
  })
})
