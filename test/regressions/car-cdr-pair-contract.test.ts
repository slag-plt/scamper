import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/256
//
// `car`/`cdr` (and so the whole composed c[ad]+r family) performed no contract
// check: on the empty list they read `.head`/`.tail` of `null` and leaked a
// raw JS TypeError, and on any other non-pair they read `undefined` and
// silently returned void. They now reject anything that is neither a pair nor a
// non-empty list with a proper Scamper contract error; the family inherits the
// check since each accessor is composed from car/cdr, so each reports its own
// name at the actual call site.

test('car/cdr error on the empty list instead of leaking a JS TypeError', async () => {
  expect(await runProgram(`
  (car (list))
  (cdr (list))
  `)).toEqual([
    'Runtime error [1:1-1:12]: (car) expected a pair or nonempty list, received null',
    'Runtime error [2:3-2:14]: (cdr) expected a pair or nonempty list, received null',
  ])
})

test('car/cdr error on a non-pair instead of returning void', async () => {
  expect(await runProgram(`
  (car 5)
  (cdr 5)
  `)).toEqual([
    'Runtime error [1:1-1:7]: (car) expected a pair or nonempty list, received number',
    'Runtime error [2:3-2:9]: (cdr) expected a pair or nonempty list, received number',
  ])
})

test('the c[ad]+r family inherits the contract on the empty list', async () => {
  const out = await runProgram(`
  (cadr (list))
  (cddr (list))
  (caddr (list))
  `)
  expect(out).toHaveLength(3)
  expect(out[0]).toContain('(cadr) expected a pair or nonempty list, received null')
  expect(out[1]).toContain('(cddr) expected a pair or nonempty list, received null')
  expect(out[2]).toContain('(caddr) expected a pair or nonempty list, received null')
})

test('the c[ad]+r family inherits the contract on a non-pair', async () => {
  const out = await runProgram(`
  (cadr 5)
  (cddr 5)
  (caddr 5)
  `)
  expect(out).toHaveLength(3)
  expect(out[0]).toContain('(cadr) expected a pair or nonempty list, received number')
  expect(out[1]).toContain('(cddr) expected a pair or nonempty list, received number')
  expect(out[2]).toContain('(caddr) expected a pair or nonempty list, received number')
})

test('valid car/cdr and family uses still work on pairs and non-empty lists', async () => {
  expect(await runProgram(`
  (car (list 1 2))
  (cdr (list 1 2))
  (cadr (list 1 2))
  (caddr (list 1 2 3))
  (car (pair 1 2))
  (cdr (pair 1 2))
  `)).toEqual([
    '1',
    '(list 2)',
    '2',
    '3',
    '1',
    '2',
  ])
})
