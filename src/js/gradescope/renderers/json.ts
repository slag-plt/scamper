import * as L from '../../../lpm'
import TextRenderer from '../../../lpm/renderers/text.js'

/**
 * The JSON behind the `gradescope` library's structs (issue #404), shared by
 * every renderer: the text one *is* Gradescope's results file (the CLI's stdout
 * is piped straight to it), and the browser ones show the same thing so an
 * instructor writing a harness in the IDE sees what will be submitted.
 *
 * This is the library's only hand-written JavaScript: src/lib/gradescope.scm
 * does the rest in Scamper, and rendering is what it could not, since turning a
 * Scamper value into text is a renderer's job.
 */

/** One case of Gradescope's test results API. See src/lib/gradescope.scm. */
interface TestResult extends L.Struct {
  [L.structKind]: 'gradescope-test-result'
  name: string
  status: string
  score: number
  'max-score': number
  output: L.Value
}

/** A whole suite: the `tests` array Gradescope reads. */
interface SuiteOutput extends L.Struct {
  [L.structKind]: 'gradescope-test-suite-output'
  tests: L.List
}

/** @returns true iff `v` is a `gradescope-test-result`. */
export const isTestResult = (v: L.Value) =>
  L.isStructKind(v, 'gradescope-test-result')

/** @returns true iff `v` is a `gradescope-test-suite-output`. */
export const isSuiteOutput = (v: L.Value) =>
  L.isStructKind(v, 'gradescope-test-suite-output')

/**
 * @returns `v` as the text Gradescope should show: a string verbatim (so an
 *   instructor can supply their own message), anything else -- a test result,
 *   most often -- rendered the way Scamper prints it.
 */
function asText(v: L.Value): string {
  return typeof v === 'string' ? v : TextRenderer.render(v)
}

/**
 * @returns the case `v` as a plain object under the field names Gradescope's
 *   API uses (`max_score`, not the struct's `max-score`).
 */
function testResultToJson(v: TestResult): object {
  return {
    name: v.name,
    status: v.status,
    score: v.score,
    max_score: v['max-score'],
    output: asText(v.output),
  }
}

/**
 * @param v a `gradescope-test-result` or a `gradescope-test-suite-output`
 * @returns it as the JSON text Gradescope reads, indented two spaces
 *
 * Both are only ever built by the library itself (gradescope-test-suite is the
 * only exported way to make a suite), so the fields are read without
 * re-checking their types.
 */
export function toJsonText(v: L.Value): string {
  const json = isSuiteOutput(v)
    ? {
        tests: L.listToVector((v as SuiteOutput).tests).map((t) =>
          testResultToJson(t as TestResult),
        ),
      }
    : testResultToJson(v as TestResult)
  return JSON.stringify(json, null, 2)
}
