import * as LPM from '.'
import HtmlRenderer from './renderers/html-renderer.js'
// import hljs from 'highlight.js'

// export function mkCodeElement (text: string): HTMLElement {
//   const elt = hljs.highlight(text, {language: 'scheme', ignoreIllegals: true})
//   const ret = document.createElement('code')
//   ret.classList.add('hljs')
//   ret.innerHTML = elt.value
//   ret.tabIndex = 0;
//   return ret
// }

// export function mkSourceBlock (text: string): HTMLElement {
//   const elt = hljs.highlight(text, {language: 'scheme', ignoreIllegals: true})
//   const ret = document.createElement('pre')
//   ret.classList.add('hljs')
//   ret.innerHTML = elt.value
//   ret.tabIndex = 0;
//   return ret
// }

export function renderToOutput(output: HTMLElement, v: any) {
  const div = document.createElement('div')
  div.classList.add('scamper-output')
  div.appendChild(HtmlRenderer.render(v))
  output!.appendChild(div)
}

export class HTMLDisplay implements LPM.OutputChannel, LPM.ErrorChannel {
  display: HTMLElement

  constructor (display: HTMLElement) {
    this.display = display
  }

  send (v: LPM.Value): void {
    renderToOutput(this.display, v)
  }

  report (err: LPM.ScamperError): void {
    renderToOutput(this.display, err)
  }
}

export default HTMLDisplay