import { expect, test, describe } from 'vitest'
import { expandExpr } from '../../src/scheme/expansion.js'
import * as A from '../../src/scheme/ast.js'

describe('Expanded expressions', () => {
  test('and', () => {
    const actual = expandExpr(A.mkAnd([A.mkVar('X'), A.mkVar('Y'), A.mkVar('Z')]))
    const expected = 
      A.mkIf(A.mkVar('X'),
        A.mkIf(A.mkVar('Y'),
          A.mkIf(A.mkVar('Z'), A.mkLit(true), A.mkLit(false)),
          A.mkLit(false)),
        A.mkLit(false))
    expect(actual).toEqual(expected)
  })

  test('or', () => {
    const actual = expandExpr(A.mkOr([A.mkVar('X'), A.mkVar('Y'), A.mkVar('Z')]))
    const expected = 
      A.mkIf(A.mkVar('X'), A.mkLit(true),
        A.mkIf(A.mkVar('Y'), A.mkLit(true),
          A.mkIf(A.mkVar('Z'), A.mkLit(true), A.mkLit(false))))
    expect(actual).toEqual(expected)
  })

  test('let*', () => {
    const actual = expandExpr(A.mkLetS([
      { name: 'x', value: A.mkLit(1) },
      { name: 'y', value: A.mkVar('x') },
      { name: 'z', value: A.mkVar('y') }
    ], A.mkVar('z')))
    const expected = 
      A.mkLet([{ name: 'x', value: A.mkLit(1) }],
        A.mkLet([{ name: 'y', value: A.mkVar('x') }],
          A.mkLet([{ name: 'z', value: A.mkVar('y') }],
            A.mkVar('z'))))
    expect(actual).toEqual(expected)
  })

  test('cond', () => {
    const actual = expandExpr(A.mkCond([
      { test: A.mkVar('X'), body: A.mkVar('A') },
      { test: A.mkVar('Y'), body: A.mkVar('B') },
      { test: A.mkVar('Z'), body: A.mkVar('C') }
    ]))
    const expected = 
      A.mkIf(A.mkVar('X'), A.mkVar('A'),
        A.mkIf(A.mkVar('Y'), A.mkVar('B'),
          A.mkIf(A.mkVar('Z'), A.mkVar('C'),
            A.mkError(A.mkLit('No matching clause in cond')))))
    expect(actual).toEqual(expected)
  })

  test('section', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('+'),
      A.mkVar('_'),
      A.mkLit(1)
    ]))
    const expected =
      A.mkLam(['_0'],
        A.mkApp(A.mkVar('+'), [A.mkVar('_0'), A.mkLit(1)]))
    expect(actual).toEqual(expected)
  })
})

// N.B., genHoleSym's counter is module-level and carries over between
// tests, so these tests read the generated names back off the actual
// result rather than hardcoding them.
describe('Section hole collection', () => {
  test('app', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('+'),
      A.mkApp(A.mkVar('double'), [A.mkVar('_')]),
      A.mkLit(1)
    ])) as A.Lam
    expect(actual.params.length).toBe(1)
    const [h] = actual.params
    const expected =
      A.mkLam([h],
        A.mkApp(A.mkVar('+'), [
          A.mkApp(A.mkVar('double'), [A.mkVar(h)]),
          A.mkLit(1)
        ]))
    expect(actual).toEqual(expected)
  })

  test('lam', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('map'),
      A.mkLam(['x'], A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkVar('_')])),
      A.mkVar('xs')
    ])) as A.Lam
    expect(actual.params.length).toBe(1)
    const [h] = actual.params
    const expected =
      A.mkLam([h],
        A.mkApp(A.mkVar('map'), [
          A.mkLam(['x'], A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkVar(h)])),
          A.mkVar('xs')
        ]))
    expect(actual).toEqual(expected)
  })

  test('let', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkLet(
        [{ name: 'x', value: A.mkVar('_') }],
        A.mkApp(A.mkVar('g'), [A.mkVar('x'), A.mkVar('_')]))
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkLet(
            [{ name: 'x', value: A.mkVar(h1) }],
            A.mkApp(A.mkVar('g'), [A.mkVar('x'), A.mkVar(h2)]))
        ]))
    expect(actual).toEqual(expected)
  })

  test('let*', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkLetS(
        [{ name: 'x', value: A.mkVar('_') }],
        A.mkApp(A.mkVar('g'), [A.mkVar('x'), A.mkVar('_')]))
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkLet(
            [{ name: 'x', value: A.mkVar(h1) }],
            A.mkApp(A.mkVar('g'), [A.mkVar('x'), A.mkVar(h2)]))
        ]))
    expect(actual).toEqual(expected)
  })

  test('begin', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkBegin([
        A.mkApp(A.mkVar('display'), [A.mkVar('_')]),
        A.mkVar('_')
      ])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkBegin([
            A.mkApp(A.mkVar('display'), [A.mkVar(h1)]),
            A.mkVar(h2)
          ])
        ]))
    expect(actual).toEqual(expected)
  })

  test('if', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkIf(A.mkVar('_'), A.mkVar('_'), A.mkVar('_'))
    ])) as A.Lam
    expect(actual.params.length).toBe(3)
    expect(new Set(actual.params).size).toBe(3)
    const [h1, h2, h3] = actual.params
    const expected =
      A.mkLam([h1, h2, h3],
        A.mkApp(A.mkVar('f'), [A.mkIf(A.mkVar(h1), A.mkVar(h2), A.mkVar(h3))]))
    expect(actual).toEqual(expected)
  })

  test('match', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkMatch(A.mkVar('_'), [
        { pat: A.mkPVar('x'), body: A.mkApp(A.mkVar('g'), [A.mkVar('x'), A.mkVar('_')]) }
      ])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkMatch(A.mkVar(h1), [
            { pat: A.mkPVar('x'), body: A.mkApp(A.mkVar('g'), [A.mkVar('x'), A.mkVar(h2)]) }
          ])
        ]))
    expect(actual).toEqual(expected)
  })

  test('error', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkError(A.mkVar('_'))
    ])) as A.Lam
    expect(actual.params.length).toBe(1)
    const [h] = actual.params
    const expected =
      A.mkLam([h], A.mkApp(A.mkVar('f'), [A.mkError(A.mkVar(h))]))
    expect(actual).toEqual(expected)
  })

  test('apply', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkApply(A.mkVar('_'), A.mkVar('_'))
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [A.mkApply(A.mkVar(h1), A.mkVar(h2))]))
    expect(actual).toEqual(expected)
  })

  test('and', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkAnd([A.mkVar('_'), A.mkVar('_')])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkIf(A.mkVar(h1),
            A.mkIf(A.mkVar(h2), A.mkLit(true), A.mkLit(false)),
            A.mkLit(false))
        ]))
    expect(actual).toEqual(expected)
  })

  test('or', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkOr([A.mkVar('_'), A.mkVar('_')])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkIf(A.mkVar(h1), A.mkLit(true),
            A.mkIf(A.mkVar(h2), A.mkLit(true), A.mkLit(false)))
        ]))
    expect(actual).toEqual(expected)
  })

  test('cond', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('f'),
      A.mkCond([
        { test: A.mkVar('_'), body: A.mkVar('_') }
      ])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkVar('f'), [
          A.mkIf(A.mkVar(h1), A.mkVar(h2),
            A.mkError(A.mkLit('No matching clause in cond')))
        ]))
    expect(actual).toEqual(expected)
  })

  test('nested section holes are not collected by the outer section', () => {
    const inner = A.mkSection([A.mkVar('+'), A.mkVar('_'), A.mkLit(1)])
    const actual = expandExpr(A.mkSection([A.mkVar('call'), inner])) as A.Lam
    expect(actual.params).toEqual([])
    const innerLam = (actual.body as A.App).args[0] as A.Lam
    expect(innerLam.params.length).toBe(1)
    const [h] = innerLam.params
    const expected =
      A.mkApp(A.mkVar('call'), [
        A.mkLam([h], A.mkApp(A.mkVar('+'), [A.mkVar(h), A.mkLit(1)]))
      ])
    expect(actual.body).toEqual(expected)
  })

  test('multiple underscores get distinct fresh names', () => {
    const actual = expandExpr(A.mkSection([
      A.mkVar('list'),
      A.mkVar('_'),
      A.mkVar('_'),
      A.mkVar('_')
    ])) as A.Lam
    expect(actual.params.length).toBe(3)
    expect(new Set(actual.params).size).toBe(3)
    const [h1, h2, h3] = actual.params
    const expected =
      A.mkLam([h1, h2, h3],
        A.mkApp(A.mkVar('list'), [A.mkVar(h1), A.mkVar(h2), A.mkVar(h3)]))
    expect(actual).toEqual(expected)
  })
})