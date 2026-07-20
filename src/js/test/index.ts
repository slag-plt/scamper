import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import './renderers/text.js'

export type Result = Ok | ErrExp | ErrExn | ErrGen
export interface Ok extends L.Struct { [L.structKind]: 'ok', desc: string }
export interface ErrExp extends L.Struct { [L.structKind]: 'exp', desc: string, expected: L.Value, actual: L.Value }
export interface ErrExn extends L.Struct { [L.structKind]: 'exn', desc: string, exn: L.Value }
export interface ErrGen extends L.Struct { [L.structKind]: 'gen', desc: string, reason: string }

export function testResultOk(desc: string): Ok {
  checkContract(arguments, contract('test-result-ok', [C.string]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'ok', desc }
}

export function testResultErrorExpected(desc: string, expected: L.Value, actual: L.Value): ErrExp {
  checkContract(arguments, contract('test-result-error-expected', [C.string, C.any, C.any]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exp', desc, expected, actual }
}

export function testResultErrorExn(desc: string, exn: L.Value): ErrExn {
  checkContract(arguments, contract('test-result-error-exn', [C.string, C.any]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exn', desc, exn }
}

export function testResultErrorGeneric(desc: string, reason: string): ErrGen {
  checkContract(arguments, contract('test-result-error-generic', [C.string, C.html]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'gen', desc, reason }
}

export function testCase(desc: string, eqFn: L.ScamperFn, expected: L.Value, testFn: L.ScamperFn): Result {
  checkContract(arguments, contract('test-case', [C.string, C.func, C.any, C.func]))
  try {
    const actual = L.callScamperFn(testFn)
    const isEqual = L.callScamperFn(eqFn, expected, actual) 
    if (isEqual === true) {
      return testResultOk(desc)
    } else if (isEqual === false) {
      return testResultErrorExpected(desc, expected, actual)
    } else {
      throw new L.ScamperError('Runtime', `Test case function should have produced a boolean, produced ${L.typeOf(actual)} instead`)
    }
  } catch (e) {
    return testResultErrorExn(desc, e as L.Value)
  }
}

export function testExn(desc: string, testFn: L.ScamperFn): Result {
  try {
    L.callScamperFn(testFn, [])
    return testResultErrorGeneric(desc, 'Test case did not throw an exception')
  } catch (e) {
    return testResultOk(desc)
  }
}

export function isResult (v: any): boolean {
  return L.isStructKind(v, 'ok') || L.isStructKind(v, 'exp')
      || L.isStructKind(v, 'exn') || L.isStructKind(v, 'gen')
}
