import { describe, expect, test } from 'vitest'
import * as A from '../../src/scheme/ast.js'
import { anyRange } from './util.js'

describe('expToString', () => {
  test('lit', () => {
    expect(A.expToString(A.mkLit(42, anyRange))).toBe('42')
  })

  test('var', () => {
    expect(A.expToString(A.mkVar('x', anyRange))).toBe('x')
  })

  test('app', () => {
    expect(A.expToString(A.mkApp(A.mkVar('f', anyRange), [], anyRange))).toBe('(f)')
    expect(
      A.expToString(
        A.mkApp(
          A.mkVar('f', anyRange),
          [A.mkVar('x', anyRange), A.mkVar('y', anyRange)],
          anyRange,
        ),
      ),
    ).toBe('(f x y)')
  })

  test('lam', () => {
    expect(
      A.expToString(A.mkLam(['x', 'y'], A.mkVar('x', anyRange), anyRange)),
    ).toBe('(lambda (x y) x)')
    expect(
      A.expToString(A.mkLam(['x'], A.mkVar('x', anyRange), anyRange, 'rest')),
    ).toBe('(lambda (x . rest) x)')
  })

  test('let', () => {
    expect(
      A.expToString(
        A.mkLet(
          [{ name: 'x', value: A.mkLit(1, anyRange) }],
          A.mkVar('x', anyRange),
          anyRange,
        ),
      ),
    ).toBe('(let ([x 1]) x)')
  })

  test('begin', () => {
    expect(
      A.expToString(
        A.mkBegin([A.mkVar('x', anyRange), A.mkVar('y', anyRange)], anyRange),
      ),
    ).toBe('(begin x y)')
  })

  test('if', () => {
    expect(
      A.expToString(
        A.mkIf(
          A.mkVar('c', anyRange),
          A.mkVar('t', anyRange),
          A.mkVar('e', anyRange),
          anyRange,
        ),
      ),
    ).toBe('(if c t e)')
  })

  test('match', () => {
    expect(
      A.expToString(
        A.mkMatch(
          A.mkVar('x', anyRange),
          [{ pat: A.mkPVar('y', anyRange), body: A.mkVar('y', anyRange) }],
          anyRange,
        ),
      ),
    ).toBe('(match x [y y])')
  })

  test('quote', () => {
    expect(A.expToString(A.mkQuote(1, anyRange))).toBe('(quote 1)')
  })

  test('jsvar', () => {
    expect(A.expToString(A.mkJsVar('Math.sqrt', anyRange))).toBe(
      '(js-var "Math.sqrt")',
    )
  })

  test('error', () => {
    expect(A.expToString(A.mkError(A.mkLit('boom', anyRange), anyRange))).toBe(
      '(error "boom")',
    )
  })

  test('apply', () => {
    expect(
      A.expToString(
        A.mkApply(A.mkVar('f', anyRange), A.mkVar('args', anyRange), anyRange),
      ),
    ).toBe('(apply f args)')
  })

  test('let*', () => {
    expect(
      A.expToString(
        A.mkLetS(
          [{ name: 'x', value: A.mkLit(1, anyRange) }],
          A.mkVar('x', anyRange),
          anyRange,
        ),
      ),
    ).toBe('(let* ([x 1]) x)')
  })

  test('and', () => {
    expect(
      A.expToString(A.mkAnd([A.mkVar('a', anyRange), A.mkVar('b', anyRange)], anyRange)),
    ).toBe('(and a b)')
  })

  test('or', () => {
    expect(
      A.expToString(A.mkOr([A.mkVar('a', anyRange), A.mkVar('b', anyRange)], anyRange)),
    ).toBe('(or a b)')
  })

  test('cond', () => {
    expect(
      A.expToString(
        A.mkCond(
          [{ test: A.mkVar('a', anyRange), body: A.mkVar('b', anyRange) }],
          anyRange,
        ),
      ),
    ).toBe('(cond [a b])')
  })

  test('section', () => {
    expect(
      A.expToString(
        A.mkSection([A.mkVar('_', anyRange), A.mkLit(1, anyRange)], anyRange),
      ),
    ).toBe('(section _ 1)')
  })

  test('report', () => {
    expect(A.expToString(A.mkReport(A.mkVar('x', anyRange), anyRange))).toBe(
      '(report x)',
    )
  })
})

describe('stmtToString', () => {
  test('import', () => {
    expect(A.stmtToString(A.mkImport('foo.scm', 'file', anyRange))).toBe(
      '(import "foo.scm")',
    )
  })

  test('define', () => {
    expect(A.stmtToString(A.mkDefine('x', A.mkLit(1, anyRange), anyRange))).toBe(
      '(define x 1)',
    )
  })

  test('display', () => {
    expect(A.stmtToString(A.mkDisp(A.mkLit(1, anyRange), anyRange))).toBe(
      '(display 1)',
    )
  })

  test('stmtexp', () => {
    expect(A.stmtToString(A.mkStmtExp(A.mkVar('x', anyRange), anyRange))).toBe('x')
  })

  test('struct', () => {
    expect(A.stmtToString(A.mkStruct('point', ['x', 'y'], anyRange))).toBe(
      '(struct point (x y))',
    )
  })
})

describe('patToString', () => {
  test('pvar', () => {
    expect(A.patToString(A.mkPVar('x', anyRange))).toBe('x')
  })

  test('plit', () => {
    expect(A.patToString(A.mkPLit(7, anyRange))).toBe('7')
  })

  test('pctor', () => {
    expect(A.patToString(A.mkPCtor('nil', [], anyRange))).toBe('(nil)')
    expect(
      A.patToString(
        A.mkPCtor('cons', [A.mkPVar('x', anyRange), A.mkPVar('y', anyRange)], anyRange),
      ),
    ).toBe('(cons x y)')
  })
})
