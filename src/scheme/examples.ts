import { equals, isArray, Range, ReportError, ScamperError, Value } from '../lpm'
import * as A from './ast.js'
import { parseFunctionDocFromComments } from './docstring/docstring.js'
import { isExampleTag } from './docstring/tags/example-tag.js'

/**
 * Checking `@example` lines (issue #374): an example says what a call should
 * produce, so Scamper can run it and say whether the code agrees.
 *
 * Everything here is about *one* example. A check is a description taken from
 * the AST ({@link ExampleCheck}), a program built from it
 * ({@link mkCheckProgram}), and a reading of what that program reported
 * ({@link classifyExampleRun}) -- none of which needs a scheduler or a UI.
 */

/** One `;;; @example <call> -> <expected>` line, taken from the AST. */
export interface ExampleCheck {
  /** The comment line the example is on -- where its mark is drawn. */
  range: Range
  /** The call to run. */
  call: A.App
  /** What the call should produce, as an expression to evaluate beside it. */
  expected: A.Exp
}

/** How an example turned out. */
export type ExampleStatus = 'pass' | 'fail' | 'error' | 'timeout'

/** What a check found, ready to be drawn beside its example line. */
export interface ExampleOutcome {
  range: Range
  status: ExampleStatus
  /** What the call produced, when it disagreed with the example. */
  actual?: Value
  /** What the example said to expect, when the call disagreed with it. */
  expected?: Value
  /** Why the example could not be checked, when it could not. */
  message?: string
}

/**
 * Collects every `@example` line in `prog`.
 *
 * Docstring diagnostics are dropped: a malformed docstring is already reported
 * as a warning by the editor's diagnostics pass, and repeating it here would
 * show it twice.
 */
export function collectExamples(prog: A.Prog): ExampleCheck[] {
  const checks: ExampleCheck[] = []
  for (const stmt of prog) {
    if (stmt.tag !== 'define' && stmt.tag !== 'defexport') continue
    if (stmt.docComments === undefined) continue
    const { doc } = parseFunctionDocFromComments(stmt.docComments)
    if (doc === undefined) continue
    for (const tag of doc.tags) {
      if (!isExampleTag(tag)) continue
      checks.push({
        range: tag.range,
        call: tag.contents.functionCall,
        expected: tag.contents.result,
      })
    }
  }
  return checks
}

/**
 * Builds the program that checks one example: the whole file, then
 * `(##report## [<call> <expected>])`.
 *
 * Both sides are reported together because the expected side is an expression
 * -- `-> (list 1 2 3)` needs the file's definitions in scope just as the call
 * does, and one report ends the run. The vector literal lowers to the internal
 * `##mkVec##`, so a user binding named `vector` cannot interfere.
 */
export function mkCheckProgram(prog: A.Prog, check: ExampleCheck): A.Prog {
  const pair = A.mkVec([check.call, check.expected])
  return [...prog, A.mkStmtExp(A.mkApp(A.mkId('##report##'), [pair]))]
}

/**
 * Reads what a check run reported into an outcome.
 *
 * A check runs as a report task, which reports exactly one error and stops:
 * either the `ReportError` carrying the pair, or whatever the program failed
 * with.
 * @returns the outcome, less the range its caller attaches
 */
export function classifyExampleRun(
  errors: readonly ScamperError[],
): Omit<ExampleOutcome, 'range'> {
  const first = errors.at(0)
  if (first === undefined || !(first instanceof ReportError)) {
    return {
      status: 'error',
      message: first?.message ?? 'The example never produced a value.',
    }
  }
  const reported = first.value
  if (!isArray(reported) || reported.length !== 2) {
    return { status: 'error', message: 'The example never produced a value.' }
  }
  const [actual, expected] = reported
  // `equals` is what the prelude's `equal?` uses, so a mark can never disagree
  // with what the student's own `(equal? ...)` says.
  return equals(actual, expected)
    ? { status: 'pass' }
    : { status: 'fail', actual, expected }
}
