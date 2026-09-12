import { describe, expect, test } from 'vitest'
import { ScamperError } from '../../src/lpm'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/554
//
// #513's defect, in the neighbouring arm of the same function. `applyFn`
// (src/lpm/handlers/op-handlers.ts) recovers the student's call site for its
// *js-function* arm, and the Frame it builds for a closure call already does
// the same (`currFrame.origin === 'builtin' ? currFrame.callRange : range`).
// The two `throw`s in the *closure* arm did not: both passed the Ap op's own
// range, which inside library source is a line of prelude.scm.
//
//   (fold (lambda (x) x) 0 (list 1 2))
//   Runtime error [944:17-944:29]: Arity mismatch in function call: ...
//
// for a one-line program -- 944 is `fold`'s own `(f v (car l))`. Passing a
// wrong-arity lambda to `map`, `fold`, `filter` or `reduce` is one of the most
// common beginner mistakes there is, and it pointed at a file the student did
// not write and cannot open.
//
// `apply`'s path had no range at all: its native is the bytecode closure
// `(lambda (f args) «ap-spread»)` (src/js/prelude/index.ts), whose `mkApSpread()`
// carries no range, so the same throw reported unlocated. `map` reaches it
// through its own rest parameter, hence the two cases below with no range today.

/**
 * Runs `src`, which is expected to *throw* rather than report: an error raised
 * while an error is being handled escapes the fiber instead of reaching the
 * output channel.
 *
 * @returns the thrown ScamperError, rendered (range included).
 */
async function runExpectingThrow(src: string): Promise<string> {
  try {
    const log = await runProgram(src)
    throw new Error(`expected a thrown error, got ${log.join('\n')}`)
  } catch (e) {
    if (e instanceof ScamperError) { return e.toString() }
    throw e
  }
}

describe("#554: a closure's arity error points at the call, not the library", () => {
  test('fold blames the call the student wrote', async () => {
    expect(await runProgram('(fold (lambda (x) x) 0 (list 1 2))')).toEqual([
      'Runtime error [1:1-1:34]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('so do filter and reduce', async () => {
    expect(await runProgram('(filter (lambda (x y) x) (list 1 2))')).toEqual([
      'Runtime error [1:1-1:36]: Arity mismatch in function call: expected 2 arguments, got 1',
    ])
    expect(await runProgram('(reduce (lambda (x) x) (list 1 2))')).toEqual([
      'Runtime error [1:1-1:34]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('map and apply report a range at all', async () => {
    expect(await runProgram('(map (lambda (x y) x) (list 1 2))')).toEqual([
      'Runtime error [1:1-1:33]: Arity mismatch in function call: expected 2 arguments, got 1',
    ])
    expect(await runProgram('(apply (lambda (x) x) (list 1 2))')).toEqual([
      'Runtime error [1:1-1:33]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('"not a function or closure" raised in library code blames the call too', async () => {
    expect(await runProgram('(filter-onto 5 (list 1) null)')).toEqual([
      'Runtime error [1:1-1:29]: Not a function or closure: 5',
    ])
  })

  test('the range tracks the statement it occurs in', async () => {
    expect(
      await runProgram(
        '(fold + 0 (list 1 2))\n(fold (lambda (x) x) 0 (list 1 2))',
      ),
    ).toEqual([
      '3',
      'Runtime error [2:1-2:34]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  // The other half of the rule, and the reason the fix keys on the frame's
  // *origin* rather than its name: a call written in the student's own source
  // does have a site, so it keeps reporting its own, not its caller's.
  test("a user's own higher-order function still reports its own call", async () => {
    expect(
      await runProgram(
        '(define apply-twice (lambda (f x) (f x x)))\n(apply-twice (lambda (y) y) 1)',
      ),
    ).toEqual([
      'Runtime error [1:35-1:41]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('a direct wrong-arity call is unaffected', async () => {
    expect(await runProgram('(define f (lambda (x) x))\n(f 1 2)')).toEqual([
      'Runtime error [2:1-2:7]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  // Fiber.handleError is applyFn's third caller: it applies a with-handler's
  // handler from the `with-handler` builtin's own frame. So the rule above moves
  // a wrong-arity *handler*'s error off the thunk's failing form -- it used to
  // report `(error "boom")` at [1:40-1:53] -- and onto the with-handler call
  // whose handler is wrong, which is the form the student has to fix.
  test('a wrong-arity with-handler handler blames the with-handler form', async () => {
    expect(await runExpectingThrow(
      '(with-handler (lambda () 1) (lambda () (error "boom")))',
    )).toEqual(
      'Runtime error [1:1-1:55]: Arity mismatch in function call: expected 0 arguments, got 1',
    )
  })
})
