import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/591
//
// The last of #513/#554's family. `applyFn` (src/lpm/handlers/op-handlers.ts)
// decided whether a frame had a call site worth reporting by testing its
// *name*, and a synthetic name ("##anonymous##") was taken to mean "called
// directly, so this op's own range is already the student's". Inside the
// standard library that is false: a lambda written in prelude.scm is library
// code whose op ranges are lines of prelude.scm, named or not.
//
//   (vector-for-each error (vector "boom"))
//   Runtime error [1059:28-1059:47]: (error) boom
//
// for a one-line program -- 1059 is `vector-for-each`'s own
// `(lambda (i) (f (vector-ref v i)))`. `map` got this right and `vector-map`
// did not, purely because `map` recurses through a *named* helper while
// `vector-map` drives an inner lambda, which is not a distinction a student
// can see.
//
// The site question is now asked of the frame's *origin*, as PR #588 already
// did for the closure arm. The `source` name stays a question about the name:
// `##anonymous##` is worse than the callee's own name, so the two are decided
// separately.
//
// `error` is what makes these repros reachable at all: it carries no docstring
// parameters, so contract.ts leaves it unwrapped (see isContracted) and a
// student can pass the primitive itself as a value.

describe('#591: an anonymous library lambda reports the student call', () => {
  test('vector-for-each blames the one line the student wrote', async () => {
    expect(await runProgram('(vector-for-each error (vector "boom"))')).toEqual([
      'Runtime error [1:1-1:39]: (error) boom',
    ])
  })

  test('so do compose and |>, which apply their argument from a lambda', async () => {
    expect(await runProgram('((compose error) "boom")')).toEqual([
      'Runtime error [1:1-1:24]: (error) boom',
    ])
    expect(await runProgram('(|> "boom" error)')).toEqual([
      'Runtime error [1:1-1:17]: (error) boom',
    ])
  })

  test('vector-map, whose inner lambda fills the result vector', async () => {
    expect(await runProgram('(vector-map error (vector "boom"))')).toEqual([
      'Runtime error [1:1-1:34]: (error) boom',
    ])
  })

  test('the range tracks the statement it occurs in', async () => {
    expect(
      await runProgram(
        '(+ 1 2)\n(vector-for-each error (vector "boom"))',
      ),
    ).toEqual(['3', 'Runtime error [2:1-2:39]: (error) boom'])
  })

  // The half that already worked, kept as the control: a *named* library frame
  // reported its caller all along, and the fix must not disturb it.
  test('map, which applies through a named library helper, is unchanged', async () => {
    expect(await runProgram('(map error (list "boom"))')).toEqual([
      'Runtime error [1:1-1:25]: (error) boom',
    ])
    expect(await runProgram('(apply error (list "boom"))')).toEqual([
      'Runtime error [1:1-1:27]: (error) boom',
    ])
  })

  // The other half of the split, and why this is not a swap of one test for
  // another: a *user* frame's ops are the student's own code, so the failing
  // call in its body is the place to point -- not the call to the function
  // that contains it. The closure arm settled this the same way in #588
  // ("a user's own higher-order function still reports its own call"); the
  // js arm used to disagree, reporting `(f 1)` on line 2.
  test("a call inside the student's own function reports its own body", async () => {
    expect(
      await runProgram('(define f (lambda (x) (error "boom")))\n(f 1)'),
    ).toEqual(['Runtime error [1:23-1:36]: (error) boom'])
  })

  // A contract wrapper is a library frame, so it keeps reporting the call it
  // was made from -- #513's rule, now keyed on origin rather than on the
  // wrapper happening to have a name.
  test('a contract-wrapped primitive still blames the call in the caller', async () => {
    expect(
      await runProgram(
        '(define g (lambda (x) (vector-ref x 99)))\n(g (vector 1 2))',
      ),
    ).toEqual([
      'Runtime error [1:23-1:39]: (vector-ref) vector-ref: index 99 out of bounds of vector',
    ])
  })
})
