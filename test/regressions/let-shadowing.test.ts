import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/127

test('normal-let', async () => {
  expect(await runProgram(`
  (define f
    (lambda ()
      (let ([x (+ 1 1)]
            [y (+ 10 1)]
            [z (+ 100 1)])
           (+ x (* y z)))))
  (f)
  `)).toEqual([
    '1113'
  ])
})

test('let-shadowing', async () => {
  expect(await runProgram(`
  (define sample3
    (lambda (x)
      (list x
            (let ([x (+ x 1)]
                  [y (+ x 1)])
            (list x y)))))

  (sample3 10)
  `)).toEqual([
    '(list 10 (list 11 11))'
  ])
})

test('let-telescoping', async () => {
  expect(await runProgram(`
  (define sample3
    (lambda (x)
      (list x
            (let* ([x (+ x 1)]
                   [y (+ x 1)])
            (list x y)))))

  (sample3 10)
  `)).toEqual([
    '(list 10 (list 11 12))'
  ])
})
