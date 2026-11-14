import { describe, expect, test } from 'vitest'
import { tokenizeAndParse } from '../../src/scheme'
import { expandProgram } from '../../src/scheme/expansion'
import * as A from '../../src/scheme/ast'
import {
  Shape,
  documentedLambda,
  reachableLambdas,
  shapeEnvFor,
  shapeOf,
} from '../../src/scheme/function-shape'

// Unit tests for the judgement behind #745's warning. scope.ts drives it on
// the *expanded* program, so every case here goes through expandProgram --
// which is the whole reason the rule set is small: `and`, `or`, `cond`,
// `anonfn`, `vec`, `obj` and `begin` are gone before it runs.

/** The expanded program `src` parses to. */
function expanded(src: string): A.Prog {
  const { program, diagnostics } = tokenizeAndParse(src)
  expect(diagnostics.map((d) => d.message), `${src} should parse`).toEqual([])
  expect(program).toBeDefined()
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  return expandProgram(program!)
}

/**
 * The shape of the value `(define x ...)` binds, in a program that may define
 * other things first -- so a test can set up the environment it needs.
 */
function shapeOfLast(src: string): Shape {
  const prog = expanded(src)
  const defines = prog.filter((s): s is A.Define => s.tag === 'define')
  const last = defines.at(-1)
  if (last === undefined) {
    throw new Error(`no define in: ${src}`)
  }
  return shapeOf(last.value, shapeEnvFor(prog))
}

describe('a function is recognised however it is built (#745)', () => {
  test.each([
    ['a bare lambda', '(define f (lambda (x) x))'],
    ['a let around one', '(define f (let ([g (lambda (x) x)]) (lambda (y) y)))'],
    ['a let whose body is a let', '(define f (let ([a 1]) (let ([b 2]) (lambda (x) x))))'],
    // begin expands to nested lets, so the let rule covers it unasked.
    ['a begin ending in one', '(define f (begin 1 2 (lambda (x) x)))'],
    ['both branches of an if', '(define f (if #t (lambda (x) x) (lambda (y) y)))'],
    // cond expands to nested ifs, likewise.
    ['every branch of a cond', '(define f (cond [#t (lambda (x) x)] [else (lambda (y) y)]))'],
    ['every branch of a match', '(define f (match 1 [0 (lambda (x) x)] [_ (lambda (y) y)]))'],
    ['a library function by name', '(define f add1)'],
    ['a user function by name', '(define g (lambda (x) x))\n(define f g)'],
    // The combinators the issue names, none of them hardcoded: each declares
    // `-> procedure?` in its own docstring, which is what the rule reads.
    ['compose', '(define f (compose add1 add1))'],
    ['o', '(define f (o add1 add1))'],
    ['l-s', '(define f (l-s + 5))'],
    ['r-s', '(define f (r-s + 5))'],
    // A forward reference: top-level defines are mutually recursive, which is
    // why the environment is built to a fixpoint rather than in source order.
    ['a function defined further down', '(define f g)\n(define g (lambda (x) x))'],
  ])('%s', (_label, src) => {
    expect(shapeOfLast(src)).toBe('function')
  })
})

describe('a non-function is still recognised as one (#745)', () => {
  test.each([
    ['a number', '(define f 3.14159)'],
    ['a string', '(define f "hi")'],
    ['a boolean', '(define f #t)'],
    ['arithmetic, off +’s own -> number?', '(define f (+ 1 2))'],
    ['a list, off list’s -> list?', '(define f (list 1 2))'],
    ['a vector', '(define f (vector 1 2))'],
    ['a constant by name', '(define c 1)\n(define f c)'],
    ['a let around a literal', '(define f (let ([g (lambda (x) x)]) 5))'],
    ['both branches of an if', '(define f (if #t 1 2))'],
    // A definition owns its name even when the library exports it too.
    ['a shadowed library name', '(define add1 5)\n(define f add1)'],
  ])('%s', (_label, src) => {
    expect(shapeOfLast(src)).toBe('not-function')
  })
})

describe('anything else is unknown, and so never warned about (#745)', () => {
  test.each([
    // The point of the three-valued judgement: a gap in the rules goes quiet
    // rather than becoming the next false positive.
    ['a hole, i.e. code not yet written', '(define f ??)'],
    ['a value pulled out of a vector', '(define f (vector-ref (vector add1) 0))'],
    ['a call to a user function', '(define g (lambda (x) (lambda (y) y)))\n(define f (g 1))'],
    ['a library function returning any', '(define f (id add1))'],
    ['branches that disagree', '(define f (if #t (lambda (x) x) 5))'],
    ['an unbound name', '(define f nope)'],
    ['a call to a let-bound function', '(define f (let ([g (lambda (x) 5)]) (g 1)))'],
    ['a computed head', '(define f ((o add1 add1) 1))'],
  ])('%s', (_label, src) => {
    expect(shapeOfLast(src)).toBe('unknown')
  })
})

/** The value `(define f ...)` binds, for the lambda-recovery tests. */
function valueOf(src: string): A.Exp {
  const defines = expanded(src).filter(
    (s): s is A.Define => s.tag === 'define',
  )
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  return defines.at(-1)!.value
}

// A pattern binds names too, and they are not the enclosing environment's.
// Left unbound, a pattern variable spelled like a library name resolved to the
// library's value -- which is a false positive of exactly the kind #745 is
// about, since the warning is the only thing a `not-function` answer produces.
describe('a pattern variable shadows the environment (#745)', () => {
  test.each([
    ['a match branch returning one', '(define f (match (pair add1 1) [(pair pi n) pi]))'],
    ['a match branch applying one', '(define f (match (pair add1 1) [(pair sqrt n) (sqrt 1)]))'],
    ['a destructuring let applying one', '(define f (let ([(pair sqrt n) (pair add1 1)]) (sqrt 1)))'],
    ['a nested pattern', '(define f (match (pair (pair add1 1) 2) [(pair (pair pi n) m) pi]))'],
  ])('%s is not read as the library name', (_label, src) => {
    expect(shapeOfLast(src)).toBe('unknown')
  })

  test('and a wildcard or literal pattern binds nothing', () => {
    expect(shapeOfLast('(define f (match 1 [_ add1]))')).toBe('function')
    expect(shapeOfLast('(define f (match 1 [1 add1]))')).toBe('function')
  })

  test('a match with no branches answers nothing rather than throwing', () => {
    // `(match 1)` parses, so the guard for it is reachable -- and this pass
    // runs on every keystroke in the IDE, where a throw is not an option.
    expect(shapeOfLast('(define f (match 1))')).toBe('unknown')
  })
})

describe('the environment covers the modules actually in scope (#745)', () => {
  test('an unqualified import contributes its names', () => {
    // circle is documented `-> drawing?`, so this is known not to be a
    // function -- which is only true if the import was picked up at all.
    expect(
      shapeOfLast('(import image)\n(define f (circle 10 "solid" "red"))'),
    ).toBe('not-function')
  })

  test('a qualified import deliberately contributes none', () => {
    // `img.circle` is reachable only as a qualified name, which is not a plain
    // identifier, so nothing is claimed about it either way.
    expect(
      shapeOfLast('(import image img)\n(define f (img.circle 10 "solid" "red"))'),
    ).toBe('unknown')
  })

  test("a definition shadows a library name's return type too, not just its value", () => {
    // Both halves of the environment have to be cleared for a shadowing
    // define: `sqr` here returns a procedure, where no library `sqr` does.
    expect(
      shapeOfLast('(define sqr (lambda (f) (o f f)))\n(define f (sqr add1))'),
    ).toBe('unknown')
  })
})

describe('the documented lambda is recovered where there is one (#745)', () => {
  test('through a let, so the parameter checks can still run', () => {
    const lam = documentedLambda(
      valueOf('(define f (let ([g (lambda (a b) a)]) (lambda (lst) lst)))'),
    )
    expect(lam?.params.map((p) => p.name)).toEqual(['lst'])
  })

  test('a bare lambda is itself', () => {
    expect(
      documentedLambda(valueOf('(define f (lambda (x y) x))'))?.params.map(
        (p) => p.name,
      ),
    ).toEqual(['x', 'y'])
  })

  test("an if's branches that agree on their parameters", () => {
    expect(
      documentedLambda(
        valueOf('(define f (if #t (lambda (x) x) (lambda (x) 1)))'),
      )?.params.map((p) => p.name),
    ).toEqual(['x'])
  })

  test('but not when they disagree, since neither is the one to blame', () => {
    expect(
      documentedLambda(valueOf('(define f (if #t (lambda (x) x) (lambda (y) y)))')),
    ).toBeUndefined()
  })

  test('nor when no lambda is written at all', () => {
    expect(documentedLambda(valueOf('(define f (o add1 add1))'))).toBeUndefined()
  })

  test('a rest parameter has to agree too', () => {
    expect(
      documentedLambda(
        valueOf('(define f (if #t (lambda (x & ys) x) (lambda (x) x)))'),
      ),
    ).toBeUndefined()
  })

  test('reachableLambdas finds every candidate, not just the first', () => {
    expect(
      reachableLambdas(
        valueOf('(define f (if #t (lambda (x) x) (lambda (y) y)))'),
      ).map((l) => l.params.map((p) => p.name)),
    ).toEqual([['x'], ['y']])
  })

  test('nor when one branch is a function by some other route', () => {
    // `(if c (lambda (x) x) add1)` is a function either way, and add1's
    // parameter is as much the definition's as the lambda's -- so checking the
    // docstring against the lambda alone would be a guess.
    expect(
      documentedLambda(valueOf('(define f (if #t (lambda (x) x) add1))')),
    ).toBeUndefined()
    expect(
      documentedLambda(
        valueOf('(define f (match 1 [0 (lambda (x) x)] [_ add1]))'),
      ),
    ).toBeUndefined()
  })

  test('but a let around it is still transparent', () => {
    expect(
      documentedLambda(
        valueOf('(define f (let ([a 1]) (if #t (lambda (x) x) (lambda (x) 2))))'),
      )?.params.map((p) => p.name),
    ).toEqual(['x'])
  })
})
