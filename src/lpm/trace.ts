import * as L from './lang'
import * as U from './util'
import HTMLRenderer from './renderers/html.js'
import TextRenderer from './renderers/text'

export interface TraceStart extends L.Struct {
  [L.structKind]: 'trace-start'
  preamble: string
  output?: L.Value
}

export function mkTraceStart (preamble: string, output?: L.Value): TraceStart {
  return U.mkStruct('trace-start', ['preamble', 'output'], [preamble, output]) as TraceStart
}

TextRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  (v) => {
    const t = v as TraceStart
    const output = t.output ? ` ${TextRenderer.render(t.output)}` : ''
    return `${t.preamble}${output}`
  })

HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  (v) => {
    const container = document.createElement('div')
    container.classList.add('scamper-trace-start')
    const t = v as TraceStart
    container.appendChild(document.createTextNode(`${t.preamble} `))
    if (t.output) {
      container.appendChild(HTMLRenderer.render(t.output))
    }
    return container
  })

export interface TraceOutput extends L.Struct {
  [L.structKind]: 'trace-output'
  output: L.Value
}

export function mkTraceOutput (output: L.Value): TraceOutput {
  return U.mkStruct('trace-output', ['output'], [output]) as TraceOutput
}

TextRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-output'),
  (v) => {
    return `--> ${TextRenderer.render((v as TraceOutput).output)}`
  })
    
HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-output'),
  (v) => {
    const trace = v as TraceOutput
    const container = document.createElement('div')
    container.classList.add('scamper-trace')

    const prompt = document.createElement('code')
    prompt.textContent = '--> '
    container.appendChild(prompt)

    container.appendChild(HTMLRenderer.render(trace.output))
    return container
  }
)
