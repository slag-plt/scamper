import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/633
//
// Every contract failure was reported under the name `error`:
//
//   (not 1)
//   -> Runtime error [1:1-1:7]: (error) expected a boolean as the first ...
//
// The range was already the student's own call, so the name was the only thing
// wrong -- and `(error)` names the mechanism rather than the procedure whose
// promise was broken, which is the one thing the student can go and read about.
// The checks are built by contract insertion, which knows the name of the
// definition it is wrapping, so each check now hands it to `##error##` as the
// party to blame.
//
// `##error##` still defaults to `error` when given no one to blame, which is
// what a `cond` fall-through and the prelude's own `error` rely on -- see
// internal-name-hygiene.test.ts, which pins that a user's `(define error 5)`
// cannot make a contract failure leak the internal `(##error##)` spelling.
//
// Ranges are stripped: a library contract error points at the definition in the
// .scm source, so any library edit would otherwise shift these.

describe('#633: a contract failure names the procedure it belongs to', () => {
  test('a required parameter names its own procedure', async () => {
    expect(await runProgram('(not 1)', { stripRanges: true })).toEqual([
      'Runtime error: (not) expected a boolean as the first argument, received number',
    ])
  })

  test('so does a deeper one, called through another library function', async () => {
    expect(await runProgram('(map car (list 1 2))', { stripRanges: true })).toEqual([
      'Runtime error: (car) expected pair or nonempty-list as the first argument, received number',
    ])
  })

  test('an optional parameter names its procedure too', async () => {
    expect(
      await runProgram('(substring "alphabetical" 5 "x")', { stripRanges: true }),
    ).toEqual([
      'Runtime error: (substring) expected an integer as the third argument, received string',
    ])
  })

  test('and so does a rest parameter', async () => {
    expect(await runProgram('(+ 1 "a")', { stripRanges: true })).toEqual([
      'Runtime error: (+) expected every value of v1 to be a number, but at least one was not',
    ])
  })

  test("a user's own docstring blames the definition it documents", async () => {
    expect(
      await runProgram(
        `
        ;;; (double x) -> number?
        ;;;  x : number?
        ;;; Doubles a number.
        (define double (lambda (x) (* 2 x)))
        (double "a")
        `,
        { insertContracts: true, stripRanges: true },
      ),
    ).toEqual([
      'Runtime error: (double) expected a number as the first argument, received string',
    ])
  })

  test('a cond fall-through still reports `error` -- it has no owner to name', async () => {
    expect(await runProgram('(cond [#f 1])', { stripRanges: true })).toEqual([
      'Runtime error: (error) No matching clause in cond',
    ])
  })

  test("a user's own `error` call still reports `error`", async () => {
    expect(await runProgram('(error "boom")', { stripRanges: true })).toEqual([
      'Runtime error: (error) boom',
    ])
  })
})
