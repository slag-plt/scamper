import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import * as Scheme from '../../src/scheme'
import * as LPM from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../src/lpm/run'

// https://github.com/slag-plt/scamper/issues/480
//
// `match` kept its branch cursor on the bytecode op itself and reset it only
// on the path where a branch matched. A match that ran out of branches threw
// with the cursor parked past the end -- and a library's Prog is compiled once
// and shared by every run in the session, so that op is the same object next
// time. Every later use of that same match reported "Inexhaustive pattern
// match failure" for a scrutinee that matches perfectly well.
//
// Order matters: the failing call has to come first, or nothing is poisoned.
//
// The subject here is the poisoning; the range on that error is #493's doing.
test('a failed match does not poison later uses of it (#480)', async () => {
  expect(
    await runProgram(`
    (reduce-right + null)
    (reduce-right + (list 42))
    (reduce-right - (list 1 2 3))
    `),
  ).toEqual([
    'Runtime error [1:1-1:21]: Inexhaustive pattern match failure',
    '42',
    '2',
  ])
})

/** Freezes `x` and everything reachable from it. The Prog is acyclic; the seen
 * set is only insurance against a future op shape that is not. */
function deepFreeze(x: unknown, seen = new WeakSet<object>()): void {
  if (typeof x !== 'object' || x === null || seen.has(x)) { return }
  seen.add(x)
  Object.freeze(x)
  Object.values(x).forEach((v) => { deepFreeze(v, seen) })
}

// The behavioural test above also passes if the cursor is merely reset on the
// throw path. This one pins the class instead: a compiled `Prog` is shared by
// every run, every call and every fiber in the session, so executing it must
// not write to it *at all* -- not even transiently, which is what makes the
// cursor visible to a second fiber. Freezing the program is what says that:
// a stringified before/after comparison would let a write through as long as
// it was tidied up again, which is exactly what the cheap fix does.
test('running a program does not mutate its compiled bytecode (#480)', async () => {
  const { prog, diagnostics } = await Scheme.compile(`
    (define classify
      (lambda (x)
        (match x
          [0 "zero"]
          [1 "one"])))
    (classify 1)
    (classify 2)
    (classify 0)
  `.trim())
  expect(diagnostics).toEqual([])
  if (prog === undefined) throw new Error('no program compiled')
  // Ops are acyclic, so a stringification is a faithful snapshot.
  const before = JSON.stringify(prog)
  deepFreeze(prog)
  const out = new LPM.LoggingChannel(true)
  await runFiberOnScheduler(new Fiber(prog, Scheme.mkInitialEnv()), {
    out,
    err: out,
  })
  // The run must have reached both the matching and the inexhaustive paths --
  // a write to a frozen op throws a TypeError, which surfaces here as an
  // unexpected error rather than these three lines.
  expect(out.log).toEqual([
    '"one"',
    'Runtime error [3:9-5:20]: Inexhaustive pattern match failure',
    '"zero"',
  ])
  expect(JSON.stringify(prog)).toEqual(before)
})
