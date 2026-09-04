import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

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
test('a failed match does not poison later uses of it (#480)', async () => {
  expect(
    await runProgram(`
    (reduce-right + null)
    (reduce-right + (list 42))
    (reduce-right - (list 1 2 3))
    `),
  ).toEqual([
    'Runtime error: Inexhaustive pattern match failure',
    '42',
    '2',
  ])
})
