import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/141

test('arity-check', () => {
  expect(runProgram(`
  (define f
    (lambda (x y z)
      (+ x y z)))
  
  (f 1 2 3)
  (f 1 2)
  (f)
  (f 1 2 3 4)
  `)).toEqual([
    '6',
    "Runtime error: Arity mismatch in function call: expected 3 arguments but got 2",
    "Runtime error: Arity mismatch in function call: expected 3 arguments but got 0",
    "Runtime error: Arity mismatch in function call: expected 3 arguments but got 4",
  ])
})
