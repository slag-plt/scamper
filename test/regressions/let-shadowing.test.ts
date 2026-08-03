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

// Under letrec, a binding value referencing the same name it binds hits the
// still-unfilled inner slot (shadowing any outer binding), which is a runtime
// error -- unlike the old non-telescoping let that saw the outer value.
test('self-shadowing an outer binding is a referenced-before-defined error', async () => {
  const out = await runProgram(`
  (define sample3
    (lambda (x)
      (let ([x (+ x 1)]) x)))

  (sample3 10)
  `)
  expect(out.length).toBe(1)
  expect(out[0]).toContain('referenced before it is defined')
})

// letrec subsumes let*: a later binding sees an earlier one (distinct names).
test('a later binding sees an earlier one (let* behavior)', async () => {
  expect(await runProgram(`
  (define sample3
    (lambda (x)
      (list x
            (let ([a (+ x 1)]
                  [b (+ a 1)])
            (list a b)))))

  (sample3 10)
  `)).toEqual([
    '(list 10 (list 11 12))'
  ])
})
