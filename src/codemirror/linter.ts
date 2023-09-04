import { linter, Diagnostic } from '@codemirror/lint'
import Scamper from '../scamper.js'
import { ScamperError } from '../lang.js'

function makeScamperLinter (outputId: HTMLElement) {
  return linter((view) => {
    const scamper = new Scamper(outputId)
    const diagnostics: Diagnostic[] = []
    const doc = view.state.doc.toString()
    try {
      scamper.parseProgram(doc)
    } catch (e) {
      if (e instanceof ScamperError) {
        let to, from
        if (e.range === undefined) {
          to = 0
          from = 0
        } else {
          to = e.range.end.idx
          from = e.range.begin.idx
        }
        diagnostics.push({
          from, to,
          severity: 'error',
          message: e.message
        })
      }
    }
    return diagnostics
  })
}

export default makeScamperLinter