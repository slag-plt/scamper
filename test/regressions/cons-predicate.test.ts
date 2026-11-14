import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { libraryBindings } from '../libs/library-bindings'

// https://github.com/slag-plt/scamper/issues/748
//
// Filed as a bug: `(pair? (cons 1 null))` is `#f`, where standard Scheme says
// `#t`, and the reporter asks for "some predicate that checks whether it's
// safe to call `car` and `cdr`".
//
// The maintainer's call is that `pair?` stays as it is. The pair/cons split is
// deliberate and follows Clojure -- a cons cell's tail must be a list, a pair
// holds any two values, and keeping them apart is what stops a student reading
// a list as a pair of two things (docs/DIFFERENCES.md, "Pairs, lists and
// mutation"; src/lpm/lang.ts). cons-list-contract.test.ts (#541) already pins
// that half: `(pair? (cons 1 (list 2)))` is `#f` because what `cons` builds is
// its own struct kind.
//
// What the issue *does* win is a name. `cons?` is `nonempty-list?`, which the
// prelude has had since #256 but only as the long spelling, so this adds the
// short one -- the same arrangement `function?` and `procedure?` have. The two
// predicates then partition what `car` and `cdr` accept rather than
// overlapping, which is the last test here: `(any-of pair? cons?)` is the
// `pair?` the issue was reaching for.

describe('pair? on a cons cell, which is by design (#748)', () => {
  test('a cons cell is not a pair, however the list was built', async () => {
    expect(
      await runProgram(`
(pair? (cons 1 null))
(pair? (list 1 2 3))
(pair? null)
(pair? (pair 1 2))
`),
    ).toEqual(['#f', '#f', '#f', '#t'])
  })

  test('because a pair and a cons cell are separate types', async () => {
    // Neither predicate is true of what the other describes, and `list?` is
    // true of a cons cell and of null but of neither pair.
    expect(
      await runProgram(`
(cons? (pair 1 2))
(pair? (cons 1 null))
(list? (cons 1 null))
(list? null)
(list? (pair 1 2))
`),
    ).toEqual(['#f', '#f', '#t', '#t', '#f'])
  })
})

describe('cons?, the predicate the issue asked for (#748)', () => {
  test('it is true of exactly the non-empty lists', async () => {
    expect(
      await runProgram(`
(cons? (cons 1 null))
(cons? (list 1))
(cons? null)
(cons? 5)
`),
    ).toEqual(['#t', '#t', '#f', '#f'])
  })

  test('it is the same test as nonempty-list?, the name it is a synonym of', async () => {
    // Written as a sweep rather than a pair of answers: the two names are the
    // same Javascript native, and this is what notices if one is ever rebound
    // to something that merely agrees on the obvious cases.
    expect(
      await runProgram(`
(define vals (list (cons 1 null) (list 1 2) null 5 "abc" (vector 1) (pair 1 2)))
(map cons? vals)
(map nonempty-list? vals)
(andmap (lambda (v) (equal? (cons? v) (nonempty-list? v))) vals)
`),
    ).toEqual([
      '(list #t #t #f #f #f #f #f)',
      '(list #t #t #f #f #f #f #f)',
      '#t',
    ])
  })

  test('it is documented, so it reaches the docs page and gets a contract', () => {
    const binding = libraryBindings().find(
      (b) => b.module === 'prelude' && b.name === 'cons?',
    )
    expect(binding?.doc, 'cons? should have a parsed docstring').toBeDefined()
    expect(binding?.diagnostics.map((d) => d.message)).toEqual([])
  })
})

// The issue's actual request: a predicate that says whether `car` and `cdr`
// will accept a value. Scamper spells it `(any-of pair? cons?)`, which is also
// the contract `car`/`cdr` carry -- `(or/p pair? nonempty-list?)` (#256) -- so
// this asserts the two agree on every shape rather than taking it on trust.
test('(any-of pair? cons?) is true of exactly what car accepts (#748)', async () => {
  expect(
    await runProgram(`
(define takes-car?
  (lambda (v)
    (with-handler (lambda (msg) #f) (lambda () (begin (car v) #t)))))
(define vals (list (pair 1 2) (cons 1 null) (list 1 2) null 5 "abc" (vector 1)))
(map takes-car? vals)
(map (any-of pair? cons?) vals)
(andmap (lambda (v) (equal? (takes-car? v) ((any-of pair? cons?) v))) vals)
`),
  ).toEqual([
    '(list #t #t #t #f #f #f #f)',
    '(list #t #t #t #f #f #f #f)',
    '#t',
  ])
})
