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
//
// https://github.com/slag-plt/scamper/issues/762 then narrowed `cond`: a range
// that was the whole form answered "which `if`" but not "which clause", the
// question a multi-clause `cond` actually raises. Each clause's expanded `if`
// carries that clause's range, so the two sibling handlers agree -- `let`
// blames the binder, `cond` blames the clause.

describe('a non-boolean if guard reports where it is (#753)', () => {
  test('the range covers the if form, not the statement around it', async () => {
    expect(await runProgram('(+ 1 (if 5 1 2))')).toEqual([
      'Runtime error [1:6-1:15]: if: expected a boolean guard, received number',
    ])
  })

  // `and` and `or` expand to nested `if` (expansion.ts, the 'and' and 'or'
  // cases), so this raise site is how a student meets `(and 1 2)` -- far more
  // often than a literal `(if 5 ...)`. Each operand's `if` carries that
  // operand's range, the same narrowing #762 made to `cond` clauses, so the
  // underline is the operand that was not a boolean rather than the form.
  test('an and-form expanded to if reports the failing operand', async () => {
    expect(await runProgram('(and #t 2)')).toEqual([
      'Runtime error [1:9-1:9]: and: expected a boolean guard, received number',
    ])
  })

  test('an or-form expanded to if reports the failing operand', async () => {
    expect(await runProgram('(or #f 5)')).toEqual([
      'Runtime error [1:8-1:8]: or: expected a boolean guard, received number',
    ])
  })

  // #762. Every clause of a `cond` used to expand to an `if` carrying the whole
  // `cond`'s range (expansion.ts, the 'cond' case), so a multi-clause `cond` --
  // the shape students actually write -- underlined all of its clauses and said
  // which guard failed only in prose. Each clause's `if` now carries that
  // clause's own range instead, so the underline lands on the one clause at
  // fault, matching the choice `LetHandler` makes (`binding.pat.range`, the
  // failing binder rather than the whole `let`). The fall-through error still
  // carries the whole form's range. A *sugared* `cond` (sugar.ts) no longer
  // does -- it rebuilds from the head `if`, so it reports its first clause's
  // range -- which is harmless because `sugarExpr`'s only caller is the
  // reduction tracer, and nothing it feeds reads an expression's range.
  test('a cond underlines the clause that failed, not the whole form', async () => {
    expect(
      await runProgram(`(cond
  [(= 1 2) 1]
  [(= 1 3) 2]
  [5 3]
  [else 4])`),
    ).toEqual([
      'Runtime error [4:4-4:6]: cond: expected a boolean guard, received number',
    ])
  })

  // The same narrowing on one line, where the clause and the form differ only
  // by a few columns: the point is that the range is the clause's, not that it
  // happens to be on its own line.
  test('a one-line cond narrows to the clause too', async () => {
    expect(await runProgram('(cond [(= 1 2) 1] [5 2])')).toEqual([
      'Runtime error [1:20-1:22]: cond: expected a boolean guard, received number',
    ])
  })

  // The ranges narrowed to are the sub-expressions *as written*, not as
  // expanded: expanding a derived form narrows its own range, so blaming the
  // expanded part would stop the underline part-way through the form the
  // student sees -- here at `(and 1`, mid-operand, rather than at the clause's
  // closing paren.
  test('a clause whose body is a derived form is underlined whole', async () => {
    expect(await runProgram('(cond [9 (and 1 2)] [else 3])')).toEqual([
      'Runtime error [1:8-1:18]: cond: expected a boolean guard, received number',
    ])
  })

  test('an and whose operand is a derived form blames the operand', async () => {
    expect(await runProgram('(and (cond [#t 5]) #t)')).toEqual([
      'Runtime error [1:6-1:18]: and: expected a boolean guard, received number',
    ])
  })

  // The fall-through error is raised by the `##error##` application the
  // expansion appends, which keeps the whole form's range: #762 moves the
  // guard error only.
  test('a cond that falls through still reports the whole form', async () => {
    expect(await runProgram('(cond [#f 1])')).toEqual([
      'Runtime error [1:1-1:13]: (error) No matching clause in cond',
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
