import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// Tests for src/js/runtime/index.ts's helpers (runtime_mkCtorFn,
// runtime_mkPredFn, runtime_mkGetFn, runtime_typeOf, runtime_any), exercised
// indirectly through the struct form they power at compile time.

test('constructing a struct and accessing its fields', async () => {
  expect(
    await runProgram(`
(struct point (x y))

(define p (point 3 4))

(point-x p)
(point-y p)
`),
  ).toEqual(['3', '4'])
})

test('struct predicate is true for a matching struct and false otherwise', async () => {
  expect(
    await runProgram(`
(struct point (x y))

(point? (point 3 4))
(point? 5)
(point? "not a point")
`),
  ).toEqual(['#t', '#f', '#f'])
})

test('two different struct types have independent predicates', async () => {
  expect(
    await runProgram(`
(struct point (x y))
(struct circle (center radius))

(point? (circle (point 0 0) 5))
`),
  ).toEqual(['#f'])
})

test('constructor called with too few arguments fails', async () => {
  expect(
    await runProgram(`
(struct point (x y))

(point 3)
`),
  ).toEqual([
    'Runtime error [3:1-3:9]: Constructor point expects 2 arguments, received 1',
  ])
})

test('constructor called with too many arguments fails', async () => {
  expect(
    await runProgram(`
(struct point (x y))

(point 3 4 5)
`),
  ).toEqual([
    'Runtime error [3:1-3:13]: Constructor point expects 2 arguments, received 3',
  ])
})

test('accessor called on a value of the wrong struct type fails', async () => {
  expect(
    await runProgram(`
(struct point (x y))
(struct circle (center radius))

(point-x (circle (point 0 0) 5))
`),
  ).toEqual([
    'Runtime error [4:1-4:32]: Accessor function expects a point, received [Struct: circle]',
  ])
})

test('accessor called on a non-struct value fails', async () => {
  expect(
    await runProgram(`
(struct point (x y))

(point-x 5)
`),
  ).toEqual([
    'Runtime error [3:1-3:11]: Accessor function expects a point, received number',
  ])
})

test('accessing a field name absent from the struct fails', async () => {
  // capture foo-a before foo is redefined with a different field list, so
  // it's an accessor stamped with a field name the new struct doesn't have
  expect(
    await runProgram(`
(struct foo (a b))
(define get-a foo-a)

(struct foo (c))

(get-a (foo 10))
`),
  ).toEqual([
    'Runtime error [6:1-6:16]: Accessor expects field a but it is not present in the given struct value',
  ])
})
