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
    // The preamble and a space precede the state, so that is the column it
    // starts at -- a state laid out over several lines indents to match.
    const col = t.preamble === '' ? 0 : t.preamble.length + 1
    const output = t.output ? TextRenderer.render(t.output, col) : ''
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

/**
 * The marker a console trace puts before each reduction. The web trace has none:
 * there every step gets a container of its own (see TraceOutputRenderer.vue).
 */
export const TRACE_MARKER = '--> '

TextRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, 'trace-output'),
  (v) => {
    // The marker occupies the first columns of the line, so the step is laid
    // out as beginning after it: continuation lines then sit under the form
    // rather than under the marker, and the width still means the whole line.
    const output = TextRenderer.render(
      (v as TraceOutput).output,
      TRACE_MARKER.length,
    )
    return `${TRACE_MARKER}${output}`
  },
)

