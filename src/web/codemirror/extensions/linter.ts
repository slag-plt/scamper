import { Diagnostic, linter } from '@codemirror/lint'
import * as LPM from '../../../lpm'
import { expandProgram } from '../../../scheme/expansion'
import { scopeCheckProgram } from '../../../scheme/scope'
import { tokenizeAndParse } from '../../../scheme'
import builtinLibs from '../../../lib'

function addError(err: LPM.ScamperError, diagnostics: Diagnostic[]) {
  console.log(err.toString())
  let to, from
  if (err.range === undefined) {
    to = 0
    from = 0
  } else {
    // N.B., needs to be puffed up by 1 to cover the
    // right-hand side of the token, for some reason...
    to = err.range.end.idx + 1
    from = err.range.begin.idx
  }
  diagnostics.push({
    from,
    to,
    severity: err.isFatal ? 'error' : 'warning',
    message: err.message,
  })
}

function makeScamperLinter(_outputId?: HTMLElement) {
  return linter(async (view) => {
    const errors: LPM.ScamperError[] = []
    const diagnostics: Diagnostic[] = []
    const doc = view.state.doc.toString()
    try {
      const errChannel: LPM.ErrorChannel = { report: (e) => errors.push(e) }
      const program = tokenizeAndParse(errChannel, doc)
      if (program !== undefined) {
        await scopeCheckProgram(builtinLibs, errors, expandProgram(program))
      }
    } catch (e) {
      if (e instanceof LPM.ScamperError) {
        addError(e, diagnostics)
      }
    }
    errors.forEach((e) => {
      addError(e, diagnostics)
    })
    // TODO: should also fix diagnostics going to some window in the UI
    return diagnostics
  })
}

export default makeScamperLinter
