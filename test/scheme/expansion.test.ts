import { expect, test, describe } from 'vitest'
import { expandExpr } from '../../src/scheme/expansion.js'
import * as A from '../../src/scheme/ast.js'

describe('Expanded expressions', () => {
  test('and', () => {
    const actual = expandExpr(A.mkAnd([A.mkId('X'), A.mkId('Y'), A.mkId('Z')]))
    const expected = 
      A.mkIf(A.mkId('X'),
        A.mkIf(A.mkId('Y'),
          A.mkIf(A.mkId('Z'), A.mkLit(true), A.mkLit(false)),
          A.mkLit(false)),
        A.mkLit(false))
    expect(actual).toEqual(expected)
  })

  test('or', () => {
    const actual = expandExpr(A.mkOr([A.mkId('X'), A.mkId('Y'), A.mkId('Z')]))
    const expected = 
      A.mkIf(A.mkId('X'), A.mkLit(true),
        A.mkIf(A.mkId('Y'), A.mkLit(true),
          A.mkIf(A.mkId('Z'), A.mkLit(true), A.mkLit(false))))
    expect(actual).toEqual(expected)
  })

  test('let*', () => {
    const actual = expandExpr(A.mkLetS([
      { id: A.mkId('x'), value: A.mkLit(1) },
      { id: A.mkId('y'), value: A.mkId('x') },
      { id: A.mkId('z'), value: A.mkId('y') }
    ], A.mkId('z')))
    const expected = 
      A.mkLet([{ id: A.mkId('x'), value: A.mkLit(1) }],
        A.mkLet([{ id: A.mkId('y'), value: A.mkId('x') }],
          A.mkLet([{ id: A.mkId('z'), value: A.mkId('y') }],
            A.mkId('z'))))
    expect(actual).toEqual(expected)
  })

  test('cond', () => {
    const actual = expandExpr(A.mkCond([
      { test: A.mkId('X'), body: A.mkId('A') },
      { test: A.mkId('Y'), body: A.mkId('B') },
      { test: A.mkId('Z'), body: A.mkId('C') }
    ]))
    const expected = 
      A.mkIf(A.mkId('X'), A.mkId('A'),
        A.mkIf(A.mkId('Y'), A.mkId('B'),
          A.mkIf(A.mkId('Z'), A.mkId('C'),
            A.mkError(A.mkLit('No matching clause in cond')))))
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