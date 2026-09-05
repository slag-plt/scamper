import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/512
//
// `any-of`, `all-of`, `compose` and `o` are each documented with a rest
// parameter alone -- `(any-of & f1)`, `(compose & f1)` -- so the generated
// contract accepts a call with no arguments, but each lambda required a first
// one and the call died with an arity error instead.
//
// Each of the four folds a list of functions, and each fold has a unit, so the
// lambdas are the half that was wrong: `(any-of)` is the constantly-`#f`
// predicate and `(all-of)` the constantly-`#t` one -- the same units as
// Scamper's own `(or)` and `(and)` -- while `(compose)` and `(o)` are the
// identity, as Racket's `(compose)` is `values`.

test('any-of and all-of accept no predicates (#512)', async () => {
  expect(await runProgram(`
  ((any-of) 5)
  ((all-of) 5)
  ((apply any-of null) 5)
  ((apply all-of null) 5)
  ((any-of even?) 4)
  ((any-of even?) 5)
  ((all-of even?) 4)
  ((all-of even? positive?) 5)
  `)).toEqual([
    '#f',
    '#t',
    '#f',
    '#t',
    '#t',
    '#f',
    '#t',
    '#f',
  ])
})

test('compose and o accept no functions (#512)', async () => {
  expect(await runProgram(`
  ((compose) 5)
  ((o) 5)
  ((apply compose null) 5)
  ((apply o null) 5)
  ((compose (lambda (x) (* 2 x)) (lambda (x) (+ x 1))) 5)
  ((o (lambda (x) (* 2 x)) (lambda (x) (+ x 1))) 5)
  `)).toEqual([
    '5',
    '5',
    '5',
    '5',
    '12',
    '12',
  ])
})
