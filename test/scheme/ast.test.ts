import { describe, expect, test } from 'vitest'
import * as A from '../../src/scheme/ast.js'
import { anyRange } from './util.js'

describe('expToString', () => {
  test('lit', () => {
    expect(A.expToString(A.mkLit(42, anyRange))).toBe('42')
  })

  test('var', () => {
    expect(A.expToString(A.mkId('x', anyRange))).toBe('x')
  })

  test('app', () => {
    expect(A.expToString(A.mkApp(A.mkId('f', anyRange), [], anyRange))).toBe('(f)')
    expect(
      A.expToString(
        A.mkApp(
          A.mkId('f', anyRange),
          [A.mkId('x', anyRange), A.mkId('y', anyRange)],
          anyRange,
        ),
      ),
    ).toBe('(f x y)')
  })

  test('lam', () => {
    expect(
      A.expToString(A.mkLam([A.mkId('x'), A.mkId('y')], A.mkId('x', anyRange), anyRange)),
    ).toBe('(lambda (x y) x)')
    expect(
      A.expToString(A.mkLam([A.mkId('x')], A.mkId('x', anyRange), anyRange, A.mkId('rest'))),
    ).toBe('(lambda (x & rest) x)')
    // Rest-only (no fixed parameters): (lambda (& rest) rest).
    expect(
      A.expToString(A.mkLam([], A.mkId('rest', anyRange), anyRange, A.mkId('rest'))),
    ).toBe('(lambda (& rest) rest)')
  })

  test('let', () => {
    expect(
      A.expToString(
        A.mkLet(
          [{ pat: A.mkId('x'), value: A.mkLit(1, anyRange) }],
          A.mkId('x', anyRange),
          anyRange,
        ),
      ),
    ).toBe('(let ([x 1]) x)')
  })

  test('begin', () => {
    expect(
      A.expToString(
        A.mkBegin([A.mkId('x', anyRange), A.mkId('y', anyRange)], anyRange),
      ),
    ).toBe('(begin x y)')
  })

  test('if', () => {
    expect(
      A.expToString(
        A.mkIf(
          A.mkId('c', anyRange),
          A.mkId('t', anyRange),
          A.mkId('e', anyRange),
          anyRange,
        ),
      ),
    ).toBe('(if c t e)')
  })

  test('match', () => {
    expect(
      A.expToString(
        A.mkMatch(
          A.mkId('x', anyRange),
          [{ pat: A.mkId('y', anyRange), body: A.mkId('y', anyRange) }],
          anyRange,
        ),
      ),
    ).toBe('(match x [y y])')
  })

  test('vector literal', () => {
    expect(A.expToString(A.mkVec([], anyRange))).toBe('[]')
    expect(
      A.expToString(A.mkVec([A.mkLit(1, anyRange), A.mkId('x', anyRange)], anyRange)),
    ).toBe('[1 x]')
  })

  test('map literal', () => {
    expect(A.expToString(A.mkObj([], anyRange))).toBe('{}')
    expect(
      A.expToString(
        A.mkObj(
          [{ key: A.mkLit('a', anyRange), value: A.mkId('x', anyRange) }],
          anyRange,
        ),
      ),
    ).toBe('{"a" x}')
  })


  test('and', () => {
    expect(
      A.expToString(A.mkAnd([A.mkId('a', anyRange), A.mkId('b', anyRange)], anyRange)),
    ).toBe('(and a b)')
  })

  test('or', () => {
    expect(
      A.expToString(A.mkOr([A.mkId('a', anyRange), A.mkId('b', anyRange)], anyRange)),
    ).toBe('(or a b)')
  })

  test('cond', () => {
    expect(
      A.expToString(
        A.mkCond(
          [{ test: A.mkId('a', anyRange), body: A.mkId('b', anyRange) }],
          anyRange,
        ),
      ),
    ).toBe('(cond [a b])')
  })
})

describe('stmtToString', () => {
  test('import', () => {
    expect(A.stmtToString(A.mkImport('foo.scm', 'file', anyRange))).toBe(
      '(import "foo.scm")',
    )
  })

  test('define', () => {
    expect(A.stmtToString(A.mkDefine(A.mkId('x'), A.mkLit(1, anyRange), anyRange))).toBe(
      '(define x 1)',
    )
  })

  test('display', () => {
    expect(A.stmtToString(A.mkDisp(A.mkLit(1, anyRange), anyRange))).toBe(
      '(display 1)',
    )
  })

  test('stmtexp', () => {
    expect(A.stmtToString(A.mkStmtExp(A.mkId('x', anyRange), anyRange))).toBe('x')
  })

  test('struct', () => {
    expect(A.stmtToString(A.mkStruct(A.mkId('point'), [A.mkId('x'), A.mkId('y')], anyRange))).toBe(
      '(struct point (x y))',
    )
  })
})

describe('patToString', () => {
  test('pvar', () => {
    expect(A.patToString(A.mkId('x', anyRange))).toBe('x')
  })

  test('plit', () => {
    expect(A.patToString(A.mkPLit(7, anyRange))).toBe('7')
  })

  test('pctor', () => {
    expect(A.patToString(A.mkPCtor(A.mkId('nil'), [], anyRange))).toBe('(nil)')
    expect(
      A.patToString(
        A.mkPCtor(A.mkId('cons'), [A.mkId('x', anyRange), A.mkId('y', anyRange)], anyRange),
      ),
    ).toBe('(cons x y)')
  })
})
