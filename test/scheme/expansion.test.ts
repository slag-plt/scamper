import { expect, test, describe } from "@jest/globals"
import { expandExpr } from '../../src/scheme/expansion.js'
import * as A from '../../src/scheme/ast.js'
import * as R from '../../src/lpm/runtime.js'

describe('Expanded expressions', () => {
  test('and', () => {
    expect(
      A.stripAllSyntax(expandExpr(R.mkList(
        R.mkSym('and'),
        R.mkSym('X'),
        R.mkSym('Y'),
        R.mkSym('Z'))))
    ).toEqual(
      R.mkList(R.mkSym('if'), R.mkSym('X'),
        R.mkList(R.mkSym('if'), R.mkSym('Y'),
          R.mkList(R.mkSym('if'), R.mkSym('Z'), true, false),
          false),
        false))
  })

  test('or', () => {
    expect(
      A.stripAllSyntax(expandExpr(R.mkList(
        R.mkSym('or'),
        R.mkSym('X'),
        R.mkSym('Y'),
        R.mkSym('Z'))))
    ).toEqual(
      R.mkList(R.mkSym('if'), R.mkSym('X'), true,
        R.mkList(R.mkSym('if'), R.mkSym('Y'), true,
          R.mkList(R.mkSym('if'), R.mkSym('Z'), true, false))))
  })

  test('let*', () => {
    expect(
      A.stripAllSyntax(expandExpr(R.mkList(
        R.mkSym('let*'),
        R.mkList(
          R.mkList(R.mkSym('x'), 1),
          R.mkList(R.mkSym('y'), R.mkSym('x')),
          R.mkList(R.mkSym('z'), R.mkSym('y'))),
        R.mkSym('z'))))
    ).toEqual(
      R.mkList(R.mkSym('let'), R.mkSym('x'), 1,
        R.mkList(R.mkSym('let'), R.mkSym('y'), R.mkSym('x'),
          R.mkList(R.mkSym('let'), R.mkSym('z'), R.mkSym('y'),
            R.mkSym('z')))))
  })

  test('cond', () => {
    expect(
      A.stripAllSyntax(expandExpr(R.mkList(
        R.mkSym('cond'),
        R.mkList(R.mkSym('X'), R.mkSym('A')),
        R.mkList(R.mkSym('Y'), R.mkSym('B')),
        R.mkList(R.mkSym('Z'), R.mkSym('C')))))
    ).toEqual(
      R.mkList(R.mkSym('if'), R.mkSym('X'), R.mkSym('A'),
        R.mkList(R.mkSym('if'), R.mkSym('Y'), R.mkSym('B'),
          R.mkList(R.mkSym('if'), R.mkSym('Z'), R.mkSym('C'), undefined))))
  })

  test('section', () => {
    expect(
      A.stripAllSyntax(expandExpr(R.mkList(
        R.mkSym('section'),
        R.mkSym('+'),
        R.mkSym('_'),
        1)))
    ).toEqual(
      R.mkList(R.mkSym('lambda'), R.mkList(R.mkSym('_0')),
        R.mkList(R.mkSym('+'), R.mkSym('_0'), 1)))
  })
})