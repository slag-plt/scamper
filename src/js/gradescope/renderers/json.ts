import * as L from '../../../lpm'
import TextRenderer from '../../../lpm/renderers/text.js'

/**
 * The JSON behind the `gradescope` library's structs (issue #404), shared by
 * every renderer: the text one *is* Gradescope's results file (the CLI's stdout
 * is piped straight to it), and the browser ones show the same thing so an
 * instructor writing a harness in the IDE sees what will be submitted.
 *
 * The library itself is plain Scamper (src/lib/gradescope.scm) and binds no JS
 * values -- rendering is the only part of it that could not be, since turning a
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

export const isTestResult = (v: L.Value) =>
  L.isStructKind(v, 'gradescope-test-result')

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

/** One test case, under the field names Gradescope's API uses. */
function testResultToJson(v: TestResult): object {
  return {
    name: v.name,
    status: v.status,
    score: v.score,
    max_score: v['max-score'],
    output: asText(v.output),
  }
}

/** A `gradescope-test-result` or `gradescope-test-suite-output` as JSON text. */
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
