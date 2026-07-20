import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { Result, isResult } from '../index.js'

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
      reason.appendChild(HtmlRenderer.render(result.exn))
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
