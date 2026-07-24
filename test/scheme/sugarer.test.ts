import { expect, test } from 'vitest'
import { sugarExpr } from '../../src/scheme/sugarer.js'
import * as A from '../../src/scheme/ast.js'

// A match with a single pvar branch is itself sugar for a let, so nesting
// one of these inside another AST node proves sugarExpr recurses into it.
const sugarableMatch = (scrutineeName: string, bodyName: string) =>
  A.mkMatch(A.mkVar(scrutineeName), [
    { pat: A.mkPVar(bodyName), body: A.mkVar(bodyName) },
  ])
const sugaredMatch = (scrutineeName: string, bodyName: string) =>
  A.mkLet([{ name: bodyName, value: A.mkVar(scrutineeName) }], A.mkVar(bodyName))

test('lit, var, quote, and jsvar pass through unchanged', () => {
  expect(sugarExpr(A.mkLit(42))).toEqual(A.mkLit(42))
  expect(sugarExpr(A.mkVar('x'))).toEqual(A.mkVar('x'))
  expect(sugarExpr(A.mkQuote('sym'))).toEqual(A.mkQuote('sym'))
  expect(sugarExpr(A.mkJsVar('Math.sin'))).toEqual(A.mkJsVar('Math.sin'))
})

test('match with a single pvar branch desugars to let, recursively sugaring scrutinee and body', () => {
  const actual = sugarExpr(
    A.mkMatch(sugarableMatch('n', 'm'), [
      { pat: A.mkPVar('x'), body: sugarableMatch('b', 'c') },
    ]),
  )
  const expected = A.mkLet(
    [{ name: 'x', value: sugaredMatch('n', 'm') }],
    sugaredMatch('b', 'c'),
  )
  expect(actual).toEqual(expected)
})

test('match with true/false literal branches desugars to if, recursively sugaring scrutinee and branches', () => {
  const actual = sugarExpr(
    A.mkMatch(sugarableMatch('n', 'm'), [
      { pat: A.mkPLit(true), body: sugarableMatch('t', 'a') },
      { pat: A.mkPLit(false), body: sugarableMatch('e', 'b') },
    ]),
  )
  const expected = A.mkIf(
    sugaredMatch('n', 'm'),
    sugaredMatch('t', 'a'),
    sugaredMatch('e', 'b'),
  )
  expect(actual).toEqual(expected)
})

test('match falls through unchanged when it does not fit either sugar shape', () => {
  // Unlike every other case, the default match case returns `e` itself
  // rather than a rebuilt node, so nested sugar opportunities are not
  // reached here.
  const threeBranches = A.mkMatch(A.mkVar('n'), [
    { pat: A.mkPLit(0), body: A.mkLit('zero') },
    { pat: A.mkPLit(1), body: A.mkLit('one') },
    { pat: A.mkPVar('x'), body: sugarableMatch('b', 'c') },
  ])
  expect(sugarExpr(threeBranches)).toBe(threeBranches)

  // Two branches, but not the true/false literal shape.
  const twoNonLiteralBranches = A.mkMatch(A.mkVar('n'), [
    { pat: A.mkPVar('x'), body: A.mkVar('x') },
    { pat: A.mkPVar('y'), body: A.mkVar('y') },
  ])
  expect(sugarExpr(twoNonLiteralBranches)).toBe(twoNonLiteralBranches)
})

test('app recursively sugars head and each argument', () => {
  const actual = sugarExpr(
    A.mkApp(sugarableMatch('f', 'g'), [A.mkLit(1), sugarableMatch('n', 'm')]),
  )
  const expected = A.mkApp(sugaredMatch('f', 'g'), [
    A.mkLit(1),
    sugaredMatch('n', 'm'),
  ])
  expect(actual).toEqual(expected)
})

test('apply recursively sugars the function and argument-list expressions', () => {
  const actual = sugarExpr(A.mkApply(sugarableMatch('f', 'g'), sugarableMatch('xs', 'ys')))
  const expected = A.mkApply(sugaredMatch('f', 'g'), sugaredMatch('xs', 'ys'))
  expect(actual).toEqual(expected)
})

test('lam recursively sugars the body and preserves params/restParam', () => {
  const actual = sugarExpr(A.mkLam(['a', 'b'], sugarableMatch('x', 'y'), undefined, 'rest'))
  const expected = A.mkLam(['a', 'b'], sugaredMatch('x', 'y'), undefined, 'rest')
  expect(actual).toEqual(expected)
})

test('let and let* recursively sugar binding values and body', () => {
  const bindings = [{ name: 'v', value: sugarableMatch('n', 'm') }]
  const expectedBindings = [{ name: 'v', value: sugaredMatch('n', 'm') }]

  expect(sugarExpr(A.mkLet(bindings, sugarableMatch('x', 'y')))).toEqual(
    A.mkLet(expectedBindings, sugaredMatch('x', 'y')),
  )
  expect(sugarExpr(A.mkLetS(bindings, sugarableMatch('x', 'y')))).toEqual(
    A.mkLetS(expectedBindings, sugaredMatch('x', 'y')),
  )
})

test('begin and section recursively sugar each expression', () => {
  expect(sugarExpr(A.mkBegin([A.mkLit(1), sugarableMatch('x', 'y')]))).toEqual(
    A.mkBegin([A.mkLit(1), sugaredMatch('x', 'y')]),
  )
  expect(
    sugarExpr(A.mkSection([A.mkVar('+'), sugarableMatch('x', 'y')])),
  ).toEqual(A.mkSection([A.mkVar('+'), sugaredMatch('x', 'y')]))
})

test('if recursively sugars guard, then-branch, and else-branch', () => {
  const actual = sugarExpr(
    A.mkIf(sugarableMatch('g', 'a'), sugarableMatch('t', 'b'), sugarableMatch('e', 'c')),
  )
  const expected = A.mkIf(
    sugaredMatch('g', 'a'),
    sugaredMatch('t', 'b'),
    sugaredMatch('e', 'c'),
  )
  expect(actual).toEqual(expected)
})

test('and, or, and cond recursively sugar their sub-expressions', () => {
  expect(sugarExpr(A.mkAnd([A.mkLit(true), sugarableMatch('x', 'y')]))).toEqual(
    A.mkAnd([A.mkLit(true), sugaredMatch('x', 'y')]),
  )
  expect(sugarExpr(A.mkOr([A.mkLit(false), sugarableMatch('x', 'y')]))).toEqual(
    A.mkOr([A.mkLit(false), sugaredMatch('x', 'y')]),
  )
  expect(
    sugarExpr(
      A.mkCond([{ test: sugarableMatch('p', 'q'), body: sugarableMatch('x', 'y') }]),
    ),
  ).toEqual(
    A.mkCond([{ test: sugaredMatch('p', 'q'), body: sugaredMatch('x', 'y') }]),
  )
})

test('error and report recursively sugar their wrapped expression', () => {
  expect(sugarExpr(A.mkError(sugarableMatch('x', 'y')))).toEqual(
    A.mkError(sugaredMatch('x', 'y')),
  )
  expect(sugarExpr(A.mkReport(sugarableMatch('x', 'y')))).toEqual(
    A.mkReport(sugaredMatch('x', 'y')),
  )
})
