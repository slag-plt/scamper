import { ScamperError, Value } from '../lang.js'
import { callFunction } from '../sem.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as Display from '../display.js'

function registerFn (name: string, fn: Function, map: [string, Value.T][]) {
  Value.nameFn(name, fn)
  map.push([name, fn])
}

const Test: [string, Value.T][] = []

type Result = Ok | Error
type Ok = { _scamperTag: 'struct', kind: 'ok', desc: string }
type Error = { _scamperTag: 'struct', kind: 'error', desc: string, reason: string }

function testOk (desc: string): Ok {
  checkContract(arguments, contract('test-ok', [C.string]))
  return { _scamperTag: 'struct', kind: 'ok', desc }
}
registerFn('test-ok', testOk, Test)

function testError (desc: string, reason: string): Error {
  checkContract(arguments, contract('test-error', [C.string, C.string]))
  return { _scamperTag: 'struct', kind: 'error', desc, reason }
}
registerFn('test-error', testError, Test)

function testCase (desc: string, eqFn: Value.ScamperFn, expected: Value.T, testFn: Value.ScamperFn): Result {
  try {
    const actual = callFunction(testFn)
    const isEqual = callFunction(eqFn, expected, actual) 
    if (isEqual === true) {
      return testOk(desc)
    } else if (isEqual === false) {
      return testError(desc, `Expected ${Value.toString(expected)}, received ${Value.toString(actual)}`)
    } else {
      throw new ScamperError('Runtime', `test case function should have produced a boolean, produced ${Value.typeOf(actual)} instead`)
    }
  } catch (e) {
    return testError(desc, `Test case threw an exception: ${e}`)
  }
}
registerFn('test-case', testCase, Test)

function testExn (desc: string, testFn: Value.ScamperFn): Result {
  try {
    callFunction(testFn, [])
    return testError(desc, `Test case did not throw an exception`)
  } catch (e) {
    return testOk(desc)
  }
}
registerFn('test-exn', testExn, Test)

function isResult (v: any): boolean {
  return Value.isStructKind(v, 'ok') || Value.isStructKind(v, 'error')
}

function render (v: any): HTMLElement {
  const result = v as Result
  const ret = document.createElement('div')
  ret.classList.add('test-result')

  if (result.kind === 'ok') {
    ret.classList.add('ok')
    ret.innerText = `Test "${result.desc}": Passed! ✅`
  } else {
    ret.classList.add('error')
    ret.innerText = `Test "${result.desc}": Failed! ❌`
    ret.appendChild(document.createElement('hr'))
    ret.appendChild(document.createTextNode(`${result.reason}`))
  }
  return ret
}

Display.addCustomWebRenderer(isResult, render)

export default Test