import type { Diagnostic } from 'vscode-languageserver-protocol'
import * as LPM from '../../../../lpm'
import { ScamperDiagnostic, mkDiagnostic } from '../../../../scheme/diagnostic'
import { expandProgram } from '../../../../scheme/expansion'
import { scopeCheckProgram } from '../../../../scheme/scope'
import { tokenizeAndParse } from '../../../../scheme'
import { rangeFromOffsets } from './positions'

// DiagnosticSeverity values (vscode-languageserver-protocol).
const SEVERITY_ERROR = 1
const SEVERITY_WARNING = 2

/**
 * Runs the standard analysis (parse -> expand -> scope-check) over `src`.
 *
 * The ranges are source offsets, which is what a caller placing a diagnostic
 * back into the document needs -- the notebook, whose cells are stretches of
 * the file, uses this directly (#410). {@link computeDiagnostics} is the same
 * thing in the protocol's coordinates.
 */
export async function analyzeSource(
  src: string,
): Promise<ScamperDiagnostic[]> {
  const diagnostics: ScamperDiagnostic[] = []
  try {
    const { program, diagnostics: parseDiagnostics } = tokenizeAndParse(src)
    diagnostics.push(...parseDiagnostics)
    if (program !== undefined) {
      await scopeCheckProgram(diagnostics, expandProgram(program))
    }
  } catch (e) {
    if (e instanceof LPM.ScamperError) {
      diagnostics.push(mkDiagnostic('Parse', 'error', e.message, e.range))
    } else {
      console.error(e)
      diagnostics.push(
        mkDiagnostic('Parse', 'error', e instanceof Error ? e.message : String(e)),
      )
    }
  }
  return diagnostics
}

/**
 * The analysis as LSP diagnostics, pushed to the editor via
 * publishDiagnostics. This is the sole diagnostics source (it replaced the
 * CodeMirror linter).
 */
export async function computeDiagnostics(
  src: string,
  lineStarts: number[],
): Promise<Diagnostic[]> {
  const diagnostics = await analyzeSource(src)
  return diagnostics.map((d) => toLspDiagnostic(d, lineStarts))
}

function toLspDiagnostic(d: ScamperDiagnostic, lineStarts: number[]): Diagnostic {
  const from = d.range === undefined ? 0 : d.range.begin.idx
  // +1 to cover the token's right edge (Scamper ranges are end-inclusive).
  const to = d.range === undefined ? 0 : d.range.end.idx + 1
  return {
    range: rangeFromOffsets(from, to, lineStarts),
    severity: d.severity === 'error' ? SEVERITY_ERROR : SEVERITY_WARNING,
    source: 'scamper',
    message: d.message,
  }
}
