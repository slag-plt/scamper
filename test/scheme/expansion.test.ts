import { expect, test, describe } from 'vitest'
import { expandExpr } from '../../src/scheme/expansion.js'
import * as A from '../../src/scheme/ast.js'

// Expansion tags every node it inserts with the derived form's name (its
// provenance) so sugaring can recover the form exactly. These helpers build the
// expected tagged nodes for the structural comparisons below.
const andIf = (g: A.Exp, t: A.Exp, e: A.Exp) => A.mkIf(g, t, e, undefined, 'and')
const andBool = (v: boolean) => A.mkLit(v, undefined, 'and')
const orIf = (g: A.Exp, t: A.Exp, e: A.Exp) => A.mkIf(g, t, e, undefined, 'or')
const orBool = (v: boolean) => A.mkLit(v, undefined, 'or')
const condIf = (g: A.Exp, t: A.Exp, e: A.Exp) => A.mkIf(g, t, e, undefined, 'cond')
const condSentinel = () =>
  A.mkApp(A.mkId('error'), [A.mkLit('No matching clause in cond')], undefined, 'cond')

describe('Expanded expressions', () => {
  test('and', () => {
    const actual = expandExpr(A.mkAnd([A.mkId('X'), A.mkId('Y'), A.mkId('Z')]))
    const expected =
      andIf(A.mkId('X'),
        andIf(A.mkId('Y'),
          andIf(A.mkId('Z'), andBool(true), andBool(false)),
          andBool(false)),
        andBool(false))
    expect(actual).toEqual(expected)
  })

  test('or', () => {
    const actual = expandExpr(A.mkOr([A.mkId('X'), A.mkId('Y'), A.mkId('Z')]))
    const expected =
      orIf(A.mkId('X'), orBool(true),
        orIf(A.mkId('Y'), orBool(true),
          orIf(A.mkId('Z'), orBool(true), orBool(false))))
    expect(actual).toEqual(expected)
  })

  test('cond', () => {
    const actual = expandExpr(A.mkCond([
      { test: A.mkId('X'), body: A.mkId('A') },
      { test: A.mkId('Y'), body: A.mkId('B') },
      { test: A.mkId('Z'), body: A.mkId('C') }
    ]))
    const expected =
      condIf(A.mkId('X'), A.mkId('A'),
        condIf(A.mkId('Y'), A.mkId('B'),
          condIf(A.mkId('Z'), A.mkId('C'), condSentinel())))
    expect(actual).toEqual(expected)
  })

  test('section', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('+'),
      A.mkId('_'),
      A.mkLit(1)
    ]))
    const expected =
      A.mkLam([A.mkId('_0')],
        A.mkApp(A.mkId('+'), [A.mkId('_0'), A.mkLit(1)]))
    expect(actual).toEqual(expected)
  })
})

// N.B., genHoleSym's counter is module-level and carries over between
// tests, so these tests read the generated names back off the actual
// result rather than hardcoding them.
describe('Section hole collection', () => {
  test('app', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('+'),
      A.mkApp(A.mkId('double'), [A.mkId('_')]),
      A.mkLit(1)
    ])) as A.Lam
    expect(actual.params.length).toBe(1)
    const [h] = actual.params
    const expected =
      A.mkLam([h],
        A.mkApp(A.mkId('+'), [
          A.mkApp(A.mkId('double'), [h]),
          A.mkLit(1)
        ]))
    expect(actual).toEqual(expected)
  })

  test('lam', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('map'),
      A.mkLam([A.mkId('x')], A.mkApp(A.mkId('+'), [A.mkId('x'), A.mkId('_')])),
      A.mkId('xs')
    ])) as A.Lam
    expect(actual.params.length).toBe(1)
    const [h] = actual.params
    const expected =
      A.mkLam([h],
        A.mkApp(A.mkId('map'), [
          A.mkLam([A.mkId('x')], A.mkApp(A.mkId('+'), [A.mkId('x'), h])),
          A.mkId('xs')
        ]))
    expect(actual).toEqual(expected)
  })

  test('let', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkLet(
        [{ pat: A.mkId('x'), value: A.mkId('_') }],
        A.mkApp(A.mkId('g'), [A.mkId('x'), A.mkId('_')]))
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkId('f'), [
          A.mkLet(
            [{ pat: A.mkId('x'), value: h1 }],
            A.mkApp(A.mkId('g'), [A.mkId('x'), h2]))
        ]))
    expect(actual).toEqual(expected)
  })

  test('begin', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkBegin([
        A.mkApp(A.mkId('display'), [A.mkId('_')]),
        A.mkId('_')
      ])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkId('f'), [
          A.mkLet(
            [{ pat: A.mkPWild(), value: A.mkApp(A.mkId('display'), [h1]) }],
            h2,
            undefined,
            'begin',
          )
        ]))
    expect(actual).toEqual(expected)
  })

  test('if', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkIf(A.mkId('_'), A.mkId('_'), A.mkId('_'))
    ])) as A.Lam
    expect(actual.params.length).toBe(3)
    expect(new Set(actual.params).size).toBe(3)
    const [h1, h2, h3] = actual.params
    const expected =
      A.mkLam([h1, h2, h3],
        A.mkApp(A.mkId('f'), [A.mkIf(h1, h2, h3)]))
    expect(actual).toEqual(expected)
  })

  test('match', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkMatch(A.mkId('_'), [
        { pat: A.mkId('x'), body: A.mkApp(A.mkId('g'), [A.mkId('x'), A.mkId('_')]) }
      ])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkId('f'), [
          A.mkMatch(h1, [
            { pat: A.mkId('x'), body: A.mkApp(A.mkId('g'), [A.mkId('x'), h2]) }
          ])
        ]))
    expect(actual).toEqual(expected)
  })

  test('and', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkAnd([A.mkId('_'), A.mkId('_')])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkId('f'), [
          andIf(h1,
            andIf(h2, andBool(true), andBool(false)),
            andBool(false))
        ]))
    expect(actual).toEqual(expected)
  })

  test('or', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkOr([A.mkId('_'), A.mkId('_')])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkId('f'), [
          orIf(h1, orBool(true),
            orIf(h2, orBool(true), orBool(false)))
        ]))
    expect(actual).toEqual(expected)
  })

  test('cond', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('f'),
      A.mkCond([
        { test: A.mkId('_'), body: A.mkId('_') }
      ])
    ])) as A.Lam
    expect(actual.params.length).toBe(2)
    const [h1, h2] = actual.params
    const expected =
      A.mkLam([h1, h2],
        A.mkApp(A.mkId('f'), [
          condIf(h1, h2, condSentinel())
        ]))
    expect(actual).toEqual(expected)
  })

  test('nested section holes are not collected by the outer section', () => {
    const inner = A.mkSection([A.mkId('+'), A.mkId('_'), A.mkLit(1)])
    const actual = expandExpr(A.mkSection([A.mkId('call'), inner])) as A.Lam
    expect(actual.params).toEqual([])
    const innerLam = (actual.body as A.App).args[0] as A.Lam
    expect(innerLam.params.length).toBe(1)
    const [h] = innerLam.params
    const expected =
      A.mkApp(A.mkId('call'), [
        A.mkLam([h], A.mkApp(A.mkId('+'), [h, A.mkLit(1)]))
      ])
    expect(actual.body).toEqual(expected)
  })

  test('multiple underscores get distinct fresh names', () => {
    const actual = expandExpr(A.mkSection([
      A.mkId('list'),
      A.mkId('_'),
      A.mkId('_'),
      A.mkId('_')
    ])) as A.Lam
    expect(actual.params.length).toBe(3)
    expect(new Set(actual.params).size).toBe(3)
    const [h1, h2, h3] = actual.params
    const expected =
      A.mkLam([h1, h2, h3],
        A.mkApp(A.mkId('list'), [h1, h2, h3]))
    expect(actual).toEqual(expected)
  })
})