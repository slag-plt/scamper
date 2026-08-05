import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/254
// https://github.com/slag-plt/scamper/issues/239
//
// A docstring-derived contract check that failed at runtime reported the
// *wrapped definition's* range -- the `(define ...)` in prelude.scm -- instead
// of the call site the student actually wrote. `contract.ts` builds its
// wrapper (including the `(error ...)` call) from `s.range`, since the real
// call site isn't known until runtime, and the `error` special form's handler
// threw with `op.range` unchanged, bypassing the call-site recovery `applyFn`
// already performed for JS-thrown errors.
//
// Demoting `error` from a special form (#317) routed those generated calls
// through `applyFn`, which recovers the range from the enclosing frame's
// `callRange`. Nothing asserted the resulting ranges, so this pins them down:
// the library suites all run with ranges stripped (see test/libs/harness.ts),
// which is what let the original bug sit unnoticed.

describe('a contract violation reports the call site, not the definition (#254)', () => {
  test('the range covers the offending call', async () => {
    expect(await runProgram('(not 1)')).toEqual([
      'Runtime error [1:1-1:7]: (error) expected a boolean, received number',
    ])
  })

  test('the range tracks the statement it occurs in', async () => {
    expect(
      await runProgram('(define f (lambda (x) x))\n(f 1)\n(not 1)'),
    ).toEqual([
      '1',
      'Runtime error [3:1-3:7]: (error) expected a boolean, received number',
    ])
  })

  test('a nested call reports the inner call, not the enclosing one', async () => {
    expect(await runProgram('(+ 1\n   (not 1))')).toEqual([
      'Runtime error [2:4-2:10]: (error) expected a boolean, received number',
    ])
  })

  test("a call inside a user function reports that call, not the function's", async () => {
    // The failing call sits at 1:23-1:39 inside g's body; g itself is invoked
    // from 2:1. Reporting g's own call site would be the coarser answer -- the
    // blind spot the first attempt at this fix had (see #239's history).
    expect(
      await runProgram('(define g (lambda (n) (string-length n)))\n(g 5)'),
    ).toEqual([
      'Runtime error [1:23-1:39]: (error) expected a string, received number',
    ])
  })

  test('a call inside a lambda passed to a higher-order function', async () => {
    expect(await runProgram('(map (lambda (x) (not x)) (list 1 2))')).toEqual([
      'Runtime error [1:18-1:24]: (error) expected a boolean, received number',
    ])
  })

  test('the other repros from the issue', async () => {
    expect(await runProgram('(string-length (list 1 2 3))')).toEqual([
      'Runtime error [1:1-1:28]: (error) expected a string, received list',
    ])
    expect(await runProgram('(+ 1 2 3 "bye")')).toEqual([
      'Runtime error [1:1-1:15]: (error) expected every value of v1 to be a number, but at least one was not',
    ])
  })
})

describe('errors raised from a library body keep reporting the call site', () => {
  // These never went through the contract wrapper -- they are the JS-thrown
  // path `applyFn` already handled correctly -- so they guard against a fix
  // that trades one misattribution for another.
  test('a raw JsFunction argument check', async () => {
    expect(await runProgram('(cons 1 2)')).toEqual([
      'Runtime error [1:1-1:10]: (cons) The second argument to cons should be a list',
    ])
  })

  test('a bounds error thrown from the function body', async () => {
    expect(await runProgram('(vector-ref (vector 1 2 3) 5)')).toEqual([
      'Runtime error [1:1-1:29]: (vector-ref) vector-ref: index 5 out of bounds of vector',
    ])
  })
})
