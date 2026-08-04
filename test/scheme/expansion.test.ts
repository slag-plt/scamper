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
})
