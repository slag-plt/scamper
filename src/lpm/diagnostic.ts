import { Range } from './range.js'

/** The front-end (static-analysis) phase that produced a diagnostic. */
export type DiagnosticPhase = 'Parse' | 'Scope' | 'Docstring' | 'Query'

/** How a diagnostic should be treated: an error blocks, a warning is advisory. */
export type Severity = 'error' | 'warning'

/**
 * A static diagnostic produced by the Scamper front-end (parsing, scope
 * checking, docstring validation). Pure data with collection semantics: never
 * thrown, never sent through an ErrorChannel. To surface one on an error
 * channel, convert it with ScamperError.fromDiagnostic.
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
