import * as L from '../lang'
import * as U from '../util'
import TextRenderer from '../renderers/text'

export interface TraceStart extends L.Struct {
  [L.structKind]: 'trace-start'
  preamble: string
  output?: L.Value
}

export function mkTraceStart(preamble: string, output?: L.Value): TraceStart {
  return U.mkStruct(
    'trace-start',
    ['preamble', 'output'],
    [preamble, output],
  ) as TraceStart
}

TextRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-start'),
  (v) => {
    const t = v as TraceStart
    const output = t.output ? TextRenderer.render(t.output) : ''
    // Join only the parts that are there: a trace's opening line carries the
    // program's initial state under an empty preamble, and must render bare.
    return [t.preamble, output].filter((s) => s !== '').join(' ')
  },
)

export interface TraceOutput extends L.Struct {
  [L.structKind]: 'trace-output'
  output: L.Value
}

export function mkTraceOutput(output: L.Value): TraceOutput {
  return U.mkStruct('trace-output', ['output'], [output]) as TraceOutput
}

TextRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-output'),
  (v) => {
    return `--> ${TextRenderer.render((v as TraceOutput).output)}`
  },
)

