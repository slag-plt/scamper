import { expect, test, describe } from "@jest/globals"
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
            A.mkApp(A.mkVar('error'), [A.mkLit('No matching clause in cond')]))))
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

  test('let parallel bindings', () => {
    // This test verifies that let bindings should be evaluated in parallel
    // The second binding (y) should use the outer x, not the let-bound x
    const actual = expandExpr(A.mkLet([
      { name: 'x', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) },
      { name: 'y', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) }
    ], A.mkApp(A.mkVar('list'), [A.mkVar('x'), A.mkVar('y')])))
    
    // The expansion should remain the same - let bindings are core forms
    const expected = A.mkLet([
      { name: 'x', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) },
      { name: 'y', value: A.mkApp(A.mkVar('+'), [A.mkVar('x'), A.mkLit(1)]) }
    ], A.mkApp(A.mkVar('list'), [A.mkVar('x'), A.mkVar('y')]))
    
    expect(actual).toEqual(expected)
  })
})