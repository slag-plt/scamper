import * as LPM from '..'
import HtmlRenderer from '../renderers/html.js'
import { OutputChannel, ErrorChannel } from './channel.js'

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
  output.appendChild(div)
}

export class HTMLDisplay implements OutputChannel, ErrorChannel {
  levels: HTMLElement[]

  constructor (root: HTMLElement) {
    this.levels = [root]
  }

  send (v: LPM.Value): void {
    renderToOutput(this.levels[this.levels.length - 1], v)
  }

  report (err: LPM.ScamperError): void {
    renderToOutput(this.levels[this.levels.length - 1], err)
  }

  pushLevel (...attrs: string[]) {
    const div = document.createElement('div')
    div.classList.add(...attrs)
    this.levels[this.levels.length - 1].appendChild(div)
    this.levels.push(div)
  }

  popLevel () {
    if (this.levels.length === 1) {
      throw new Error('Cannot pop the root level')
    }
    this.levels.pop()
  }
}

export default HTMLDisplay