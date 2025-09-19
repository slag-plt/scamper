import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'
import HtmlRenderer from '../lpm/renderers/html-renderer.js'

const Test: L.Library = new L.Library()

type Result = Ok | Error
interface Ok extends L.Struct { [L.structKind]: 'ok', desc: string }
interface Error extends L.Struct { [L.structKind]: 'error', desc: string, reason: HTMLElement }

function testOk (desc: string): Ok {
  checkContract(arguments, contract('test-ok', [C.string]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'ok', desc }
}
Test.registerValue('test-ok', testOk)

function testError (desc: string, reason: HTMLElement): Error {
  checkContract(arguments, contract('test-error', [C.string, C.html]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'error', desc, reason }
}
Test.registerValue('test-error', testError)

function testCase (desc: string, eqFn: L.ScamperFn, expected: L.Value, testFn: L.ScamperFn): Result {
  checkContract(arguments, contract('test-case', [C.string, C.func, C.any, C.func]))
  try {
    const actual = L.callScamperFn(testFn)
    const isEqual = L.callScamperFn(eqFn, expected, actual) 
    if (isEqual === true) {
      return testOk(desc)
    } else if (isEqual === false) {
      const reason = document.createElement('span')
      reason.appendChild(document.createTextNode('Expected '))
      reason.appendChild(HtmlRenderer.render(expected))
      reason.appendChild(document.createTextNode(', received '))
      reason.appendChild(HtmlRenderer.render(actual))
      return testError(desc, reason)
    } else {
      throw new L.ScamperError('Runtime', `Test case function should have produced a boolean, produced ${L.typeOf(actual)} instead`)
    }
  } catch (e) {
    const reason = document.createElement('span')
    reason.appendChild(document.createTextNode('Test case threw an exception: '))
    reason.appendChild(HtmlRenderer.render(e as L.Value))
    return testError(desc, reason)
  }
}
Test.registerValue('test-case', testCase)

function testExn (desc: string, testFn: L.ScamperFn): Result {
  try {
    L.callScamperFn(testFn, [])
    return testError(desc, HtmlRenderer.render(`Test case did not throw an exception`))
  } catch (e) {
    return testOk(desc)
  }
}
Test.registerValue('test-exn', testExn)

function isResult (v: any): boolean {
  return L.isStructKind(v, 'ok') || L.isStructKind(v, 'error')
}

function render (v: any): HTMLElement {
  const result = v as Result
  const ret = document.createElement('div')
  ret.classList.add('test-result')

  if (result[L.structKind] === 'ok') {
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

HtmlRenderer.registerCustomRenderer(isResult, render)

export default Test