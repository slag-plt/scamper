// Library tests assert on error *messages*, not source locations. This wrapper
// runs programs with ranges stripped from error output so a test's expectations
// don't shift when its own source string is reflowed. Range *reporting* is
// covered directly by test/lpm/range.test.ts, and the call-site attribution of
// contract errors by test/regressions/contract-error-call-site.test.ts.
import {
  runProgram as runProgramWithRanges,
  runProgramAsync as runProgramAsyncWithRanges,
} from '../harness.js'

/** Like {@link runProgramWithRanges} but drops `[line:col]` ranges from error output. */
export function runProgram (src: string): Promise<string[]> {
  return runProgramWithRanges(src, { stripRanges: true })
}

/**
 * Like {@link runProgram}, but drives a real Scheduler so *blocking* primitives
 * (`with-file`, the `file` library, `with-image-from-url`) can run: those
 * suspend the fiber mid-expression and are resumed by the scheduler, which the
 * synchronous runner cannot service. Prefer runProgram otherwise.
 */
export function runProgramAsync (src: string): Promise<string[]> {
  return runProgramAsyncWithRanges(src, { stripRanges: true })
}
