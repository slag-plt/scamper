import * as L from '../../lpm'
import './renderers/text.js'

export type Result = Ok | ErrExp | ErrExn | ErrGen
export interface Ok extends L.Struct { [L.structKind]: 'ok', desc: string }
export interface ErrExp extends L.Struct { [L.structKind]: 'exp', desc: string, expected: L.Value, actual: L.Value }
export interface ErrExn extends L.Struct { [L.structKind]: 'exn', desc: string, exn: L.Value }
export interface ErrGen extends L.Struct { [L.structKind]: 'gen', desc: string, reason: string }

export function test_testResultOk(desc: string): Ok {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'ok', desc }
}

export function test_testResultErrorExpected(desc: string, expected: L.Value, actual: L.Value): ErrExp {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exp', desc, expected, actual }
}

export function test_testResultErrorExn(desc: string, exn: L.Value): ErrExn {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exn', desc, exn }
}

export function test_testResultErrorGeneric(desc: string, reason: string): ErrGen {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'gen', desc, reason }
}

export function test_testCase(desc: string, eqFn: L.ScamperFn, expected: L.Value, testFn: L.ScamperFn): Result {
  try {
    const actual = L.callScamperFn(testFn)
    const isEqual = L.callScamperFn(eqFn, expected, actual) 
    if (isEqual === true) {
      return test_testResultOk(desc)
    } else if (isEqual === false) {
      return test_testResultErrorExpected(desc, expected, actual)
    } else {
      throw new L.ScamperError('Runtime', `Test case function should have produced a boolean, produced ${L.typeOf(actual)} instead`)
    }
  } catch (e) {
    return test_testResultErrorExn(desc, e as L.Value)
  }
}

export function test_testExn(desc: string, testFn: L.ScamperFn): Result {
  try {
    L.callScamperFn(testFn, [])
    return test_testResultErrorGeneric(desc, 'Test case did not throw an exception')
  } catch (e) {
    return test_testResultOk(desc)
  }
}

export function test_isResult (v: any): boolean {
  return L.isStructKind(v, 'ok') || L.isStructKind(v, 'exp')
      || L.isStructKind(v, 'exn') || L.isStructKind(v, 'gen')
}
