import * as L from './lang'
import * as U from './util'
import HTMLRenderer from './renderers/html.js'
import TextRenderer from './renderers/text'

export interface TraceStart extends L.Struct {
  [L.structKind]: 'trace-start'
  output: L.Value
}

export function mkTraceStart (output: L.Value): TraceStart {
  return U.mkStruct('trace-start', ['output'], [output]) as TraceStart
}

TextRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  (v) => {
    return `${TextRenderer.render((v as TraceStart).output)}`
  })

HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  (_v) => {
    // TODO: fix this up!
    const container = document.createElement('div')
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
    // TODO: fix this up!
    const trace = v as TraceOutput
    const container = document.createElement('div')
    container.classList.add('scamper-trace')

    const prompt = document.createElement('code')
    prompt.textContent = '> '
    container.appendChild(prompt)

    container.appendChild(HTMLRenderer.render(trace.output))
    return container
  }
)
