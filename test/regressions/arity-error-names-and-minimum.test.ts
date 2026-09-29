import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/670
// https://github.com/slag-plt/scamper/issues/669
//
// An arity error said neither which procedure it was about nor, when that
// procedure takes more arguments than it requires, that the count it named was
// a floor:
//
//   (max)
//   -> Runtime error [1:1-1:5]: Arity mismatch in function call: expected 1
//      arguments, got 0
//
// which reads as though `max` takes exactly one argument -- the opposite of
// what a student calling it is trying to learn -- and as though the bare "1
// arguments" were nobody's in particular. Both halves are now fixed: a rest
// parameter reports "at least", and the closure being called reports its own
// name, the way every other runtime error has since #633.
//
// The name is the *callee's*, which is why the fix is not the one #669
// suggests: at this check the callee's frame does not exist yet, so the running
// frame is the caller -- `##stmt-0##` for a direct call, or `map` for a lambda
// passed to it. Reading the frame would have named the wrong procedure, or
// nothing at all.
//
// Optional parameters ride the same check (a wrapper collects them in a rest
// parameter), so they say "at least" too; their ceiling is a separate "at most"
// message from `##checkArity##`, unchanged here.

describe('#670/#669: an arity error names its procedure and says "at least"', () => {
  test('a fixed-arity procedure names itself, with one argument in the singular', async () => {
    expect(await runProgram('(abs 1 2)', { stripRanges: true })).toEqual([
      'Runtime error: (abs) Arity mismatch in function call: expected 1 argument, got 2',
    ])
  })

  // The two calls #670 is about. `max` requires one argument and takes any
  // number; the comparisons require two (#648).
  test('a rest parameter makes the count a floor, not an exact arity', async () => {
    expect(await runProgram('(max)', { stripRanges: true })).toEqual([
      'Runtime error: (max) Arity mismatch in function call: expected at least 1 argument, got 0',
    ])
    expect(await runProgram('(< 1)', { stripRanges: true })).toEqual([
      'Runtime error: (<) Arity mismatch in function call: expected at least 2 arguments, got 1',
    ])
  })

  // #669's own example, with ranges kept: the range already pointed at the
  // inner form, so the name was the only thing missing. `string-length` is
  // never reached -- arguments evaluate left to right -- which is precisely why
  // naming the procedure matters: two calls in the form are wrong and the
  // message has to say which one it means.
  test('a nested call is both located at and named for the inner form', async () => {
    expect(await runProgram('(+ (abs 1 2) (string-length "a" "b"))')).toEqual([
      'Runtime error [1:4-1:12]: (abs) Arity mismatch in function call: expected 1 argument, got 2',
    ])
  })

  test('a procedure with optional parameters reports its required count as a floor', async () => {
    expect(await runProgram('(substring "hello")', { stripRanges: true })).toEqual([
      'Runtime error: (substring) Arity mismatch in function call: expected at least 2 arguments, got 1',
    ])
  })

  test("a user's own procedure is named the same way", async () => {
    expect(
      await runProgram('(define f (lambda (x y & z) x))\n(f 1)', {
        stripRanges: true,
      }),
    ).toEqual([
      'Runtime error: (f) Arity mismatch in function call: expected at least 2 arguments, got 1',
    ])
  })

  // A lambda never bound to a name has none to report. Every lambda is compiled
  // carrying the placeholder `##anonymous##` (src/scheme/codegen.ts) until a
  // define renames it, so the guard on the name is what keeps that spelling --
  // and any other internal `##...##` name -- out of a student's error.
  test('an anonymous lambda is reported with no name, and no internal spelling', async () => {
    const out = await runProgram('((lambda (x) x) 1 2)', { stripRanges: true })
    expect(out).toEqual([
      'Runtime error: Arity mismatch in function call: expected 1 argument, got 2',
    ])
    expect(out[0]).not.toContain('##')
  })
})
