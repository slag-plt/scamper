import { emptyLibrary, Library, registerValue, ScamperError, Value } from '../lang.js'
import { callFunction } from '../sem.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as Display from '../display.js'

const Test: Library = emptyLibrary()

type Result = Ok | Error
interface Ok extends Value.Struct { [Value.structKind]: 'ok', desc: string }
interface Error extends Value.Struct { [Value.structKind]: 'error', desc: string, reason: HTMLElement }

function testOk (desc: string): Ok {
  checkContract(arguments, contract('test-ok', [C.string]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'ok', desc }
}
registerValue('test-ok', testOk, Test)

function testError (desc: string, reason: HTMLElement): Error {
  checkContract(arguments, contract('test-error', [C.string, C.html]))
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'error', desc, reason }
}
registerValue('test-error', testError, Test)

function testCase (desc: string, eqFn: Value.ScamperFn, expected: Value.T, testFn: Value.ScamperFn): Result {
  checkContract(arguments, contract('test-case', [C.string, C.func, C.any, C.func]))
  try {
    const actual = callFunction(testFn)
    const isEqual = callFunction(eqFn, expected, actual) 
    if (isEqual === true) {
      return testOk(desc)
    } else if (isEqual === false) {
      const reason = document.createElement('span')
      reason.appendChild(Display.mkCodeElement('Expected '))
      reason.appendChild(Display.renderToHTML(expected))
      reason.appendChild(Display.mkCodeElement(', received '))
      reason.appendChild(Display.renderToHTML(actual))
      return testError(desc, reason)
    } else {
      throw new ScamperError('Runtime', `Test case function should have produced a boolean, produced ${Value.typeOf(actual)} instead`)
    }
  } catch (e) {
    const reason = document.createElement('span')
    reason.appendChild(Display.mkCodeElement('Test case threw an exception: '))
    reason.appendChild(Display.renderToHTML(e as Value.T))
    return testError(desc, reason)
  }
}
registerValue('test-case', testCase, Test)

function testExn (desc: string, testFn: Value.ScamperFn): Result {
  try {
    callFunction(testFn, [])
    return testError(desc, Display.mkCodeElement(`Test case did not throw an exception`))
  } catch (e) {
    return testOk(desc)
  }
}
registerValue('test-exn', testExn, Test)

function isResult (v: any): boolean {
  return Value.isStructKind(v, 'ok') || Value.isStructKind(v, 'error')
}

function render (v: any): HTMLElement {
  const result = v as Result
  const ret = document.createElement('div')
  ret.classList.add('test-result')

  if (result[Value.structKind] === 'ok') {
    ret.classList.add('ok')
    ret.innerText = `Test "${result.desc}": Passed! ✅`
  } else {
    ret.classList.add('error')
    ret.innerText = `Test "${result.desc}": Failed! ❌`
    ret.appendChild(document.createElement('hr'))
    ret.appendChild(result.reason)
  }
  return ret
}

Display.addCustomWebRenderer(isResult, render)

export default Test