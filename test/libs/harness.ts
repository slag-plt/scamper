// Library tests assert on error *messages*, not source locations. Contract
// errors report the offending definition's range in the `.scm` library source,
// so editing any library file would otherwise shift those ranges and break
// unrelated tests. This wrapper runs programs with ranges stripped from error
// output, keeping library tests decoupled from library line numbers. Range
// *reporting* itself is covered directly by test/lpm/range.test.ts.
import { runProgram as runProgramWithRanges } from '../harness.js'

/** Like {@link runProgramWithRanges} but drops `[line:col]` ranges from error output. */
export function runProgram (src: string): Promise<string[]> {
  return runProgramWithRanges(src, { stripRanges: true })
}
