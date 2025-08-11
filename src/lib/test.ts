import * as R from '../lpm/runtime.js'
import { callFunction } from '../sem.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as Display from '../display.js'

const Test: R.Library = new R.Library()

type Result = Ok | Error
interface Ok extends R.Struct { [R.structKind]: 'ok', desc: string }
interface Error extends R.Struct { [R.structKind]: 'error', desc: string, reason: HTMLElement }

function testOk (desc: string): Ok {
  checkContract(arguments, contract('test-ok', [C.string]))
  return { [R.scamperTag]: 'struct', [R.structKind]: 'ok', desc }
}
Test.registerValue('test-ok', testOk)

function testError (desc: string, reason: HTMLElement): Error {
  checkContract(arguments, contract('test-error', [C.string, C.html]))
  return { [R.scamperTag]: 'struct', [R.structKind]: 'error', desc, reason }
}
Test.registerValue('test-error', testError)

function testCase (desc: string, eqFn: R.ScamperFn, expected: R.Value, testFn: R.ScamperFn): Result {
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
      throw new R.ScamperError('Runtime', `Test case function should have produced a boolean, produced ${R.typeOf(actual)} instead`)
    }
  } catch (e) {
    const reason = document.createElement('span')
    reason.appendChild(Display.mkCodeElement('Test case threw an exception: '))
    reason.appendChild(Display.renderToHTML(e as R.T))
    return testError(desc, reason)
  }
}
Test.registerValue('test-case', testCase)

function testExn (desc: string, testFn: R.ScamperFn): Result {
  try {
    callFunction(testFn, [])
    return testError(desc, Display.mkCodeElement(`Test case did not throw an exception`))
  } catch (e) {
    return testOk(desc)
  }
}
Test.registerValue('test-exn', testExn)

function isResult (v: any): boolean {
  return R.isStructKind(v, 'ok') || R.isStructKind(v, 'error')
}

function render (v: any): HTMLElement {
  const result = v as Result
  const ret = document.createElement('div')
  ret.classList.add('test-result')

  if (result[R.structKind] === 'ok') {
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