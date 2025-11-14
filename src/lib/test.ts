import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'
import TextRenderer from '../lpm/renderers/text-renderer.js'
import HtmlRenderer from '../lpm/renderers/html-renderer.js'

const Test: L.Library = new L.Library()

type Result = Ok | ErrExp | ErrExn | ErrGen
interface Ok extends L.Struct { [L.structKind]: 'ok', desc: string }
interface ErrExp extends L.Struct { [L.structKind]: 'exp', desc: string, expected: L.Value, actual: L.Value }
interface ErrExn extends L.Struct { [L.structKind]: 'exn', desc: string, exn: L.Value }
interface ErrGen extends L.Struct { [L.structKind]: 'gen', desc: string, reason: string }

function testResultOk (desc: string): Ok {
  checkContract(arguments, contract('test-result-ok', [C.string]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'ok', desc }
}
Test.registerValue('test-result-ok', testResultOk)

function testResultErrorExpected (desc: string, expected: L.Value, actual: L.Value): ErrExp {
  checkContract(arguments, contract('test-result-error-expected', [C.string, C.any, C.any]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exp', desc, expected, actual }
}
Test.registerValue('test-result-error-expected', testResultErrorExpected)

function testResultErrorExn (desc: string, exn: L.Value): ErrExn {
  checkContract(arguments, contract('test-result-error-exn', [C.string, C.any]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exn', desc, exn }
}
Test.registerValue('test-result-error-exn', testResultErrorExn)

function testResultErrorGeneric(desc: string, reason: string): ErrGen {
  checkContract(arguments, contract('test-result-error-generic', [C.string, C.html]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'gen', desc, reason }
}
Test.registerValue('test-result-error-gen', testResultErrorGeneric)

function testCase (desc: string, eqFn: L.ScamperFn, expected: L.Value, testFn: L.ScamperFn): Result {
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
Test.registerValue('test-case', testCase)

function testExn (desc: string, testFn: L.ScamperFn): Result {
  try {
    L.callScamperFn(testFn, [])
    return testResultErrorGeneric(desc, 'Test case did not throw an exception')
  } catch (e) {
    return testResultOk(desc)
  }
}
Test.registerValue('test-exn', testExn)

function isResult (v: any): boolean {
  return L.isStructKind(v, 'ok') || L.isStructKind(v, 'exp')
      || L.isStructKind(v, 'exn') || L.isStructKind(v, 'gen')
}

TextRenderer.registerCustomRenderer(isResult, (v: any) => {
  const result = v as Result
  switch (result[L.structKind]) {
    case 'ok':
      return `Test "${result.desc}"\n✅ Passed!`
    case 'exp':
      return `Test "${result.desc}"\n❌ Failed! Expected ${TextRenderer.render(result.expected)}, received ${TextRenderer.render(result.actual)}`
    case 'exn':
      return `Test "${result.desc}"\n❌ Failed! Exception thrown: ${TextRenderer.render(result.exn as L.Value)}`
    case 'gen':
      return `Test "${result.desc}"\n❌ Failed! ${result.reason}`
  }
})

HtmlRenderer.registerCustomRenderer(isResult, (v: any) => {
  const result = v as Result
  const ret = document.createElement('div')
  ret.classList.add('test-result')
  switch (result[L.structKind]) {
    case 'ok': {
      ret.classList.add('ok')
      ret.innerText = `Test "${result.desc}": Passed! ✅`
      break
    }
    case 'exp': {
      ret.classList.add('error')
      ret.innerText = `Test "${result.desc}": Failed! ❌`
      ret.appendChild(document.createElement('hr'))
      const reason = document.createElement('span')
      reason.appendChild(document.createTextNode('Expected '))
      reason.appendChild(HtmlRenderer.render(result.expected))
      reason.appendChild(document.createTextNode(', received '))
      reason.appendChild(HtmlRenderer.render(result.actual))
      ret.appendChild(reason)
      break
    }
    case 'exn': {
      ret.classList.add('error')
      ret.innerText = `Test "${result.desc}": Failed! ❌`
      ret.appendChild(document.createElement('hr'))
      const reason = document.createElement('span')
      reason.appendChild(document.createTextNode('Test case threw an exception: '))
      reason.appendChild(HtmlRenderer.render(result.exn as L.Value))
      ret.appendChild(reason)
      break
    }
    case 'gen': {
      ret.classList.add('error')
      ret.innerText = `Test "${result.desc}": Failed! ❌`
      ret.appendChild(document.createElement('hr'))
      ret.appendChild(document.createTextNode(result.reason))
      break
    }
  }
  return ret
})

export default Test