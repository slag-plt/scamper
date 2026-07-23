import { describe, expect, test } from 'vitest'
import * as prettier from 'prettier'
import * as A from '../../src/scheme/ast'
import { tokenizeAndParse } from '../../src/scheme'
import ScamperPlugin from '../../src/prettier/prettier-plugin-scamper'
import { SimpleErrorChannel } from '../../src/lpm/output/simple-error'

// ---- helpers ----------------------------------------------------------------

function parse(src: string): A.Prog {
  const err = new SimpleErrorChannel()
  const prog = tokenizeAndParse(err, src)
  if (!prog) {
    throw new Error(
      `Parse failed:\n${err.errors.map((e) => e.message).join('\n')}`,
    )
  }
  return prog
}

function format(src: string): Promise<string> {
  return prettier.format(src, {
    parser: 'scamper-scheme',
    plugins: [ScamperPlugin],
  })
}

function progEquals(p1: A.Prog, p2: A.Prog): boolean {
  return (
    p1.length === p2.length && p1.every((stmt, i) => A.stmtEquals(stmt, p2[i]))
  )
}

// ---- roundtrip: parse(src) ≡ parse(format(src)) ----------------------------

describe('roundtrip', () => {
  async function roundtrip(src: string): Promise<void> {
    const original = parse(src)
    const reformatted = parse(await format(src))
    expect(progEquals(original, reformatted)).toBe(true)
  }

  test('literal define', () => roundtrip('(define x 42)'))

  test('boolean define', () => roundtrip('(define flag #t)'))

  test('lambda with multiple params', () =>
    roundtrip('(define f (lambda (x y) (+ x y)))'))

  test('if expression', () =>
    roundtrip('(define abs (lambda (n) (if (>= n 0) n (- 0 n))))'))

  test('let binding', () =>
    roundtrip('(define sum (lambda (x y) (let ([a x] [b y]) (+ a b))))'))

  test('let* binding', () =>
    roundtrip('(define chain (lambda (x) (let* ([a x] [b (+ a 1)]) (* a b))))'))

  test('match expression', () =>
    roundtrip('(define desc (lambda (n) (match n [0 "zero"] [_ "nonzero"])))'))

  test('match with constructor patterns', () =>
    roundtrip('(define head (lambda (l) (match l [(cons h _) h])))'))

  test('and / or', () =>
    roundtrip('(define both (lambda (a b) (and (> a 0) (< b 10))))'))

  test('cond expression', () =>
    roundtrip(
      '(define sign (lambda (n) (cond [(> n 0) 1] [(< n 0) -1] [#t 0])))',
    ))

  test('nullary application', () => roundtrip('(define zero (lambda () 0))'))

  test('import statement', () => roundtrip('(import image)'))

  test('struct definition', () => roundtrip('(struct point (x y))'))

  test('multi-statement program', () =>
    roundtrip('(define x 1)\n(define y 2)\n(display (+ x y))'))

  test('normalizes extra whitespace', () => roundtrip('(define   x      42)'))

  test('normalizes nested whitespace', () =>
    roundtrip('(define f   (lambda   (x   y)   (+   x   y)))'))
})

// ---- idempotence: format(src) === format(format(src)) -----------------------

describe('idempotence', () => {
  async function idempotent(src: string): Promise<void> {
    const once = await format(src)
    const twice = await format(once)
    expect(once).toBe(twice)
  }

  test('literal define', () => idempotent('(define x 42)'))

  test('lambda', () => idempotent('(define f (lambda (x y) (+ x y)))'))

  test('if expression', () =>
    idempotent('(define abs (lambda (n) (if (>= n 0) n (- 0 n))))'))

  test('let binding', () =>
    idempotent('(define sum (lambda (x y) (let ([a x] [b y]) (+ a b))))'))

  test('match expression', () =>
    idempotent('(define desc (lambda (n) (match n [0 "zero"] [_ "nonzero"])))'))

  test('multi-statement program', () =>
    idempotent('(define x 1)\n(define y 2)\n(display (+ x y))'))
})
