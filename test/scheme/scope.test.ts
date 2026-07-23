import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// These specs describe scope-checking behavior (undefined variables,
// duplicate binders, repeated pattern bindings) that src/scheme/scope.ts
// does not yet implement -- scopeCheckProgram exists but isn't wired into
// the compile pipeline. They are intentionally skipped/expected-to-fail
// until that feature lands.

test.fails('duplicate-binders', async () => {
  expect(
    await runProgram(`
(lambda (x x y) (+ x x))

(struct foo (z y z))
`),
  ).toEqual([
    ':8:0: Parser error:',
    'Duplicate name x given in definition.',
    'In program: (x x y)',
  ])
})

test.skip('let-binding-errors', async () => {
  expect(
    await runProgram(`
; let bindings telescope
(let
  ([x1 1]
   [y1 (+ x1 6)])
  (+ x1 y1))

; let bindings refer to future bindings

(let
  ([x2 y2]
   [y2 5])
  (+ x2 y2))

(let*
  ([x3 y3]
   [y3 5])
  (+ x3 y3))
`),
  ).toEqual([
    "Parser error [4:11-4:12]: Undefined variable 'x1'",
    "Parser error [10:8-10:9]: Undefined variable 'y2'",
    "Parser error [15:8-15:9]: Undefined variable 'y3'",
  ])
})

test.fails('match-repeated-bindings', async () => {
  expect(
    await runProgram(`
(match (list 1 2 3)
  [null "fail"]
  [(cons x x) "fail"])
`),
  ).toEqual([
    ':3:2: Scope error:',
    'Variable x is repeated in the pattern',
    'In program: (match (list 1 2 3)',
    '[null "fail"]',
    '[(cons x x) "fail"])',
  ])
})

test.skip.fails('shadowing', async () => {
  expect(
    await runProgram(`
(define x 3)

(define y (+ x 2))

(define x -5)

(+ x y)

(define f
  (lambda (x)
    (* x 2)))

(f 3)

(let*
  ([z 10]
   [x (+ z x)]
   [z 100])
  (+ x z))

x
`),
  ).toEqual(['0', '6', '105', '-5'])
})

test.skip('undefined-variable', async () => {
  expect(
    await runProgram(`
(+ x 1)
`),
  ).toEqual(["Parser error [1:4-1:4]: Undefined variable 'x'"])
})
