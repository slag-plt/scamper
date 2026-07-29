import { Diagnostic, linter } from '@codemirror/lint'
import * as LPM from '../../../../lpm'
import { expandProgram } from '../../../../scheme/expansion'
import { scopeCheckProgram } from '../../../../scheme/scope'
import { tokenizeAndParse } from '../../../../scheme'

/** Translates a Scamper diagnostic into a CodeMirror diagnostic. */
function toCmDiagnostic(d: LPM.ScamperDiagnostic): Diagnostic {
  let from, to
  if (d.range === undefined) {
    from = 0
    to = 0
  } else {
    from = d.range.begin.idx
    // N.B., needs to be puffed up by 1 to cover the right-hand side of the
    // token, for some reason...
    to = d.range.end.idx + 1
  }
  // Severity is carried by the diagnostic itself: parse errors surface as
  // errors, scope/docstring issues as warnings.
  return { from, to, severity: d.severity, message: d.message }
}

function makeScamperLinter(_outputId?: HTMLElement) {
  return linter(async (view) => {
    const diagnostics: LPM.ScamperDiagnostic[] = []
    const doc = view.state.doc.toString()
    try {
      const { program, diagnostics: parseDiagnostics } = tokenizeAndParse(doc)
      diagnostics.push(...parseDiagnostics)
      if (program !== undefined) {
        await scopeCheckProgram(diagnostics, expandProgram(program))
      }
    } catch (e) {
      if (e instanceof LPM.ScamperError) {
        diagnostics.push(LPM.mkDiagnostic('Parse', 'error', e.message, e.range))
      }
    }
    // TODO: should also surface diagnostics to some window in the UI
    return diagnostics.map(toCmDiagnostic)
  })
}

export default makeScamperLinter
