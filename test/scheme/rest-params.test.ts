import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// Coverage for rest parameters: (lambda (x y . z) ...) binds the trailing
// arguments beyond the fixed parameters as a proper list bound to z.

test('rest param collects extra arguments into a list', async () => {
  expect(await runProgram(`
  (display ((lambda (x . y) y) 1 2 3))
  `)).toEqual(['(list 2 3)'])
})

test('rest param is an empty list when no extra arguments are given', async () => {
  expect(await runProgram(`
  (display ((lambda (x . y) y) 1))
  `)).toEqual(['null'])
})

test('fixed parameter still binds correctly alongside a rest parameter', async () => {
  expect(await runProgram(`
  (display ((lambda (x . y) x) 1 2 3))
  `)).toEqual(['1'])
})

test('multiple fixed parameters combine with one rest parameter', async () => {
  expect(await runProgram(`
  (display ((lambda (a b . rest) (list a b rest)) 1 2 3 4 5))
  `)).toEqual(['(list 1 2 (list 3 4 5))'])
})

test('same rest-param lambda works across calls with varying argument counts', async () => {
  expect(await runProgram(`
  (define f (lambda (x . y) y))
  (display (f 1))
  (display (f 1 2))
  (display (f 1 2 3))
  `)).toEqual(['null', '(list 2)', '(list 2 3)'])
})

test('rest parameter is a real list usable with car/length', async () => {
  expect(await runProgram(`
  (define f (lambda (x . rest) (list (car rest) (length rest))))
  (display (f 1 2 3 4))
  `)).toEqual(['(list 2 3)'])
})

test('empty rest parameter is recognized by null?/list?', async () => {
  expect(await runProgram(`
  (define f (lambda (x . rest) (list (null? rest) (list? rest))))
  (display (f 1))
  `)).toEqual(['(list #t #t)'])
})

test('rest parameter works with recursion over the resulting list', async () => {
  expect(await runProgram(`
  (define sum-list
    (lambda (lst)
      (if (null? lst)
          0
          (+ (car lst) (sum-list (cdr lst))))))
  (define my-sum
    (lambda (x . rest)
      (+ x (sum-list rest))))
  (display (my-sum 1 2 3 4 5))
  `)).toEqual(['15'])
})

test('rest parameter is captured correctly by a closure returned from a lambda', async () => {
  expect(await runProgram(`
  (define make-adder
    (lambda (x . rest)
      (lambda (y) (+ x y (length rest)))))
  (display ((make-adder 1 2 3) 10))
  `)).toEqual(['13'])
})

test('regression: a fixed-arity lambda (no rest param) still works with exact args', async () => {
  expect(await runProgram(`
  (display ((lambda (x y) (+ x y)) 3 4))
  `)).toEqual(['7'])
})

test('arity mismatch: fewer than the required fixed args, even with a rest param present', async () => {
  expect(await runProgram(`
  ((lambda (x y . z) z) 1)
  `)).toEqual([
    'Runtime error [1:1-1:24]: Arity mismatch in function call: expected 2 arguments, got 1',
  ])
})

test('arity mismatch: zero args against a lambda requiring one fixed arg plus a rest param', async () => {
  expect(await runProgram(`
  ((lambda (x . y) y))
  `)).toEqual([
    'Runtime error [1:1-1:20]: Arity mismatch in function call: expected 1 arguments, got 0',
  ])
})

test('regression: arity mismatch on extra args still fires when there is no rest param', async () => {
  expect(await runProgram(`
  ((lambda (x y) (+ x y)) 1 2 3)
  `)).toEqual([
    'Runtime error [1:1-1:30]: Arity mismatch in function call: expected 2 arguments, got 3',
  ])
})

test('malformed: rest parameter missing its trailing identifier', async () => {
  expect(await runProgram(`
  (lambda (x .) x)
  `)).toEqual([
    'Parser error [1:1-1:16]: Malformed lambda expression (a list of parameters and a body).',
  ])
})

test('malformed: more than one identifier following the rest dot', async () => {
  expect(await runProgram(`
  (lambda (x . y z) x)
  `)).toEqual([
    'Parser error [1:1-1:20]: Malformed lambda expression (a list of parameters and a body).',
  ])
})

test('malformed: rest dot with no fixed parameters before it', async () => {
  expect(await runProgram(`
  (lambda (. y) y)
  `)).toEqual([
    'Parser error [1:1-1:16]: Malformed lambda expression (a list of parameters and a body).',
  ])
})
