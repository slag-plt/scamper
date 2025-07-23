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
})