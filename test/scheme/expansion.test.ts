import { expect, test, describe } from "@jest/globals"
import { expandExpr } from '../../src/scheme/expansion.js'
import * as A from '../../src/scheme/ast.js'
import * as L from '../../src/lpm'

describe('Expanded expressions', () => {
  test('and', () => {
    const actual = 
      A.stripAllSyntax(expandExpr(L.mkList(
        L.mkSym('and'),
        L.mkSym('X'),
        L.mkSym('Y'),
        L.mkSym('Z'))))
    expect(actual).toEqual(
      L.mkList(L.mkSym('if'), L.mkSym('X'),
        L.mkList(L.mkSym('if'), L.mkSym('Y'),
          L.mkList(L.mkSym('if'), L.mkSym('Z'), true, false),
          false),
        false))
  })

  test('or', () => {
    expect(
      A.stripAllSyntax(expandExpr(L.mkList(
        L.mkSym('or'),
        L.mkSym('X'),
        L.mkSym('Y'),
        L.mkSym('Z'))))
    ).toEqual(
      L.mkList(L.mkSym('if'), L.mkSym('X'), true,
        L.mkList(L.mkSym('if'), L.mkSym('Y'), true,
          L.mkList(L.mkSym('if'), L.mkSym('Z'), true, false))))
  })

  test('let*', () => {
    expect(
      A.stripAllSyntax(expandExpr(L.mkList(
        L.mkSym('let*'),
        L.mkList(
          L.mkList(L.mkSym('x'), 1),
          L.mkList(L.mkSym('y'), L.mkSym('x')),
          L.mkList(L.mkSym('z'), L.mkSym('y'))),
        L.mkSym('z'))))
    ).toEqual(
      L.mkList(L.mkSym('let'), L.mkSym('x'), 1,
        L.mkList(L.mkSym('let'), L.mkSym('y'), L.mkSym('x'),
          L.mkList(L.mkSym('let'), L.mkSym('z'), L.mkSym('y'),
            L.mkSym('z')))))
  })

  test('cond', () => {
    expect(
      A.stripAllSyntax(expandExpr(L.mkList(
        L.mkSym('cond'),
        L.mkList(L.mkSym('X'), L.mkSym('A')),
        L.mkList(L.mkSym('Y'), L.mkSym('B')),
        L.mkList(L.mkSym('Z'), L.mkSym('C')))))
    ).toEqual(
      L.mkList(L.mkSym('if'), L.mkSym('X'), L.mkSym('A'),
        L.mkList(L.mkSym('if'), L.mkSym('Y'), L.mkSym('B'),
          L.mkList(L.mkSym('if'), L.mkSym('Z'), L.mkSym('C'), undefined))))
  })

  test('section', () => {
    expect(
      A.stripAllSyntax(expandExpr(L.mkList(
        L.mkSym('section'),
        L.mkSym('+'),
        L.mkSym('_'),
        1)))
    ).toEqual(
      L.mkList(L.mkSym('lambda'), L.mkList(L.mkSym('_0')),
        L.mkList(L.mkSym('+'), L.mkSym('_0'), 1)))
  })
})