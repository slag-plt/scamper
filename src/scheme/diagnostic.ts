import { Range, ScamperError } from '../lpm'

/** The front-end (static-analysis) phase that produced a diagnostic. */
export type DiagnosticPhase = 'Parse' | 'Scope' | 'Docstring' | 'Query'

/** How a diagnostic should be treated: an error blocks, a warning is advisory. */
export type Severity = 'error' | 'warning'

/**
 * A static diagnostic produced by the Scamper front-end (parsing, scope
 * checking, docstring validation). Pure data with collection semantics: never
 * thrown, never sent through an ErrorChannel. To surface one on an error
 * channel, convert it with diagnosticToError.
 */
export interface ScamperDiagnostic {
  phase: DiagnosticPhase
  severity: Severity
  message: string
  range?: Range
  modName?: string
  source?: string
}

/**
 * Constructs a diagnostic.
 * @param range the source range the diagnostic refers to, if any
 */
export function mkDiagnostic(
  phase: DiagnosticPhase,
  severity: Severity,
  message: string,
  range?: Range,
): ScamperDiagnostic {
  return { phase, severity, message, range }
}

/**
 * Promotes a static diagnostic into a runtime error, for the boundary where a
 * front-end diagnostic must be surfaced on an ErrorChannel (which carries
 * ScamperErrors only). The diagnostic's phase is mapped to a display phase so,
 * e.g., a parse diagnostic still reads as a "Parser error".
 *
 * N.B., the result is always an error: `severity` is deliberately dropped,
 * since a ScamperError has no warning form. Only convert diagnostics that are
 * meant to block (severity 'error'); warnings should be presented as
 * diagnostics directly (see linter.ts, the CLI --check presenter).
 */
export function diagnosticToError(d: ScamperDiagnostic): ScamperError {
  const phase = d.phase === 'Docstring' ? 'Docstring' : 'Parser'
  return new ScamperError(phase, d.message, d.modName, d.range, d.source)
}
