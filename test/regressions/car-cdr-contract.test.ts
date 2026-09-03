import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/256
//
// `car`/`cdr` (and the whole `c[ad]+r` family, `caar`..`cddddr`) had a `v :
// any` contract, so calling them on the empty list leaked a raw JS
// `TypeError` and calling them on a non-pair (e.g. a number) silently
// returned void. The fix gives `car`/`cdr` an `(or/p pair? nonempty-list?)`
// contract and rebuilds the family as Scheme compositions over them, so every
// bad argument now surfaces a clean, uniform contract error.
//
// Ranges are stripped from error messages here: they are the student's own
// call, and spelling each one out would say nothing these tests are about.
//
// The family reports the *primitive that rejected* (`car`/`cdr`), attributed
// to the composition the student called (`cadr`) -- see #476, which stopped
// re-running a contract check on a call the library made to itself. `cadr`
// documents `v : any`, so all of its checking was always the inner
// `(car (cdr v))`; that check is now the primitive's own.

const stripRange = (msgs: string[]): string[] =>
  msgs.map((m) => m.replace(/\[\d+:\d+-\d+:\d+\]/, '[..]'))

describe('car/cdr reject non-pairs cleanly (#256)', () => {
  test('empty list', async () => {
    expect(stripRange(await runProgram(`
    (car (list))
    (cdr (list))
    `))).toEqual([
      'Runtime error [..]: (error) expected pair or nonempty-list, received null',
      'Runtime error [..]: (error) expected pair or nonempty-list, received null',
    ])
  })

  test('non-list value', async () => {
    expect(stripRange(await runProgram(`
    (car 5)
    (cdr "hi")
    (car #t)
    `))).toEqual([
      'Runtime error [..]: (error) expected pair or nonempty-list, received number',
      'Runtime error [..]: (error) expected pair or nonempty-list, received string',
      'Runtime error [..]: (error) expected pair or nonempty-list, received boolean',
    ])
  })
})

describe('c[ad]+r family reject bad arguments cleanly (#256)', () => {
  test('empty list', async () => {
    expect(stripRange(await runProgram(`
    (cadr (list))
    (caddr (list))
    (cddr (list))
    `))).toEqual([
      'Runtime error [..]: (cadr) cdr: expected a pair or a non-empty list',
      'Runtime error [..]: (caddr) cdr: expected a pair or a non-empty list',
      'Runtime error [..]: (cddr) cdr: expected a pair or a non-empty list',
    ])
  })

  test('list too short', async () => {
    expect(stripRange(await runProgram(`
    (cadr (list 1))
    (caddr (list 1 2))
    `))).toEqual([
      'Runtime error [..]: (cadr) car: expected a pair or a non-empty list',
      'Runtime error [..]: (caddr) car: expected a pair or a non-empty list',
    ])
  })

  test('non-list value', async () => {
    expect(stripRange(await runProgram('(cadr 5)'))).toEqual([
      'Runtime error [..]: (cadr) cdr: expected a pair or a non-empty list',
    ])
  })
})

describe('car/cdr/family still return correct results (#256)', () => {
  test('valid car/cdr on lists and pairs', async () => {
    expect(await runProgram(`
    (car (list 1 2 3))
    (cdr (list 1 2 3))
    (car (pair 1 2))
    (cdr (pair 1 2))
    `)).toEqual([
      '1',
      '(list 2 3)',
      '1',
      '2',
    ])
  })

  test('valid family accessors', async () => {
    expect(await runProgram(`
    (cadr (list 1 2 3))
    (caddr (list 1 2 3))
    (cddr (list 1 2 3))
    (caar (list (list 1 2) 3))
    `)).toEqual([
      '2',
      '3',
      '(list 3)',
      '1',
    ])
  })
})

describe('nonempty-list? and or/p (#256)', () => {
  test('nonempty-list?', async () => {
    expect(await runProgram(`
    (nonempty-list? (list 1 2))
    (nonempty-list? (list))
    (nonempty-list? 5)
    (nonempty-list? (pair 1 2))
    `)).toEqual([
      '#t',
      '#f',
      '#f',
      '#f',
    ])
  })

  test('or/p combines predicates disjunctively', async () => {
    expect(await runProgram(`
    ((or/p pair? nonempty-list?) (list 1 2))
    ((or/p pair? nonempty-list?) (pair 1 2))
    ((or/p pair? nonempty-list?) (list))
    ((or/p pair? nonempty-list?) 5)
    `)).toEqual([
      '#t',
      '#t',
      '#f',
      '#f',
    ])
  })

  test('or/p renders in contract messages without an article', async () => {
    // Only library exports are contracted, so car/cdr's own `(or/p pair?
    // nonempty-list?)` contract is what exercises describePred's or/p case:
    // the message reads "pair or nonempty-list", not "a value matching ...".
    expect(stripRange(await runProgram('(car 5)'))).toEqual([
      'Runtime error [..]: (error) expected pair or nonempty-list, received number',
    ])
  })
})
