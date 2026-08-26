import HtmlRenderer from '../../../lpm/renderers/html.js'
import { isSuiteOutput, isTestResult, toJsonText } from './json.js'

function render(v: unknown): HTMLElement {
  const ret = document.createElement('pre')
  ret.classList.add('gradescope-results')
  ret.innerText = toJsonText(v as never)
  return ret
}

HtmlRenderer.registerCustomRenderer(isTestResult, render)
HtmlRenderer.registerCustomRenderer(isSuiteOutput, render)
