import { readFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { describe, expect, test } from 'vitest'

// Regression test for #599: the Gradescope harness suite stacks three budgets.
// A submission gets SCAMPER_TIMEOUT, which run_autograder spends on the CLI;
// run_autograder itself gets spawnSync's; and the test waiting on it gets
// vitest's. Each has to strictly outlive the one inside it, or the inner budget
// is never what fires and the diagnostic it would have given is lost. #536 made
// them reachable but left the outer two both at 60s, so a hung submission
// killed the test at the same instant as the script it was waiting on.
//
// The numbers are read out of the suite's source because that is where they
// have to stay: naming them in a module elsewhere would hide them from the
// literal-hunting check in contended-test-timeouts.test.ts, which would then
// pass vacuously.

const source = readFileSync(
  resolve(import.meta.dirname, 'gradescope-harness.test.ts'),
  'utf-8',
)

/**
 * The one number `pattern` captures in the suite, in milliseconds.
 *
 * @param scale milliseconds per unit of the captured number.
 */
function budgetMs(what: string, pattern: RegExp, scale: number): number {
  const match = pattern.exec(source)
  if (match === null) {
    throw new Error(`gradescope-harness.test.ts no longer states ${what}`)
  }
  return Number(match[1].replaceAll('_', '')) * scale
}

/** What a submission gets; run_autograder reads it in seconds. */
const submissionMs = budgetMs(
  'a submission budget',
  /SCAMPER_TIMEOUT:\s*'(\d[\d_]*)'/,
  1000,
)

/** What run_autograder gets: past this spawnSync kills the script itself. */
const scriptMs = budgetMs(
  'a script budget',
  /spawnSync\([\s\S]*?\btimeout:\s*(\d[\d_]*)/,
  1,
)

/** What the test gets: past this vitest fails it. */
const testMs = budgetMs(
  'a test budget',
  /describe\(\s*'run_autograder'\s*,\s*\{[^{}]*\btimeout:\s*(\d[\d_]*)/,
  1,
)

describe('#599: the Gradescope harness budgets nest strictly', () => {
  test('run_autograder outlives the submission it is timing', () => {
    expect(
      scriptMs,
      `a submission is given ${submissionMs.toString()}ms but spawnSync kills ` +
        `run_autograder at ${scriptMs.toString()}ms, so the script has no room ` +
        'to turn a timed-out submission into a zero with a reason in it',
    ).toBeGreaterThan(submissionMs)
  })

  test('the test outlives the run_autograder it is waiting on', () => {
    expect(
      testMs,
      `run_autograder is given ${scriptMs.toString()}ms but the test around it ` +
        `only has ${testMs.toString()}ms, so the test dies at the same instant ` +
        'as the child and reports a vitest timeout instead of the child’s',
    ).toBeGreaterThan(scriptMs)
  })

  // The nesting above is also satisfied by shrinking the innermost budget to
  // nothing, which would turn a hang detector into an assertion that the
  // machine is fast. One harness run takes under a second here.
  test('a submission still gets a hang detector rather than a stopwatch', () => {
    expect(submissionMs).toBeGreaterThanOrEqual(10_000)
  })
})
