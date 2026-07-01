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
    'Runtime error [6:3-6:9]: Arity mismatch in function call: expected 3 arguments, got 2',
    'Runtime error [7:3-7:5]: Arity mismatch in function call: expected 3 arguments, got 0',
    'Runtime error [8:3-8:13]: Arity mismatch in function call: expected 3 arguments, got 4',
  ])
})
