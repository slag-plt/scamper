import * as U from '../../util'
import HTMLRenderer from '../../renderers/html.js'
import { TraceStart, TraceOutput } from '../index.js'

HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  (v) => {
    const container = document.createElement('div')
    container.classList.add('scamper-trace-start')
    const t = v as TraceStart
    if (t.preamble !== '') {
      container.appendChild(document.createTextNode(`${t.preamble} `))
    }
    if (t.output) {
      container.appendChild(HTMLRenderer.render(t.output))
    }
    return container
  },
)

HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-output'),
  (v) => {
    const trace = v as TraceOutput
    const container = document.createElement('div')
    container.classList.add('scamper-trace')
    // No "--> " marker here either -- see TraceOutputRenderer.vue.
    container.appendChild(HTMLRenderer.render(trace.output))
    return container
  },
)
