import { Scamper } from '../scamper.js'
import { renderToOutput } from '../lpm/html-display.js'

import hljs from 'highlight.js'

export function mkSourceBlock (text: string): HTMLElement {
  const elt = hljs.highlight(text, {language: 'scheme', ignoreIllegals: true})
  const ret = document.createElement('pre')
  ret.classList.add('hljs')
  ret.innerHTML = elt.value
  ret.tabIndex = 0;
  return ret
}

export function replaceCodeWidget(base: HTMLElement): void {
  const src = base.innerText
  base.innerHTML = ''

  if (base.classList.contains('source-only')) {
    base.appendChild(mkSourceBlock(src))
  } else {
    // TODO: make sure to respect these options in the future!
    // opts.isPrintingCode = base.classList.contains('source')
    // opts.isTracing = base.classList.contains('trace')
    try {
      new Scamper(base, src).runProgram()
    } catch (e) {
      // if (opts.isPrintingCode) {
      //   base.appendChild(mkSourceBlock(src))
      // }
      renderToOutput(base, e)
    }
  }
}

export function replaceCodeWidgets() {
  const widgets = document.getElementsByClassName('scamper')
  for (var i = 0; i < widgets.length; i++) {
    replaceCodeWidget(widgets[i] as HTMLElement)
  }
}

replaceCodeWidgets()