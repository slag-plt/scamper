import { readFileSync } from 'node:fs'
import path from 'node:path'
import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness'

// The samples in samples/ are written to be read, but they are only worth
// reading if they still run. Nothing else in the suite would notice one going
// stale -- they are not compiled, imported, or linted -- so these run each one
// and insist it reported no errors. A rename in the standard library or a change
// to the surface syntax fails here rather than in front of a student.
//
// The bar is deliberately "it runs", not "it produces exactly this": a sample
// pinned to its own output would have to be re-blessed on every rendering
// change, a cost with no matching benefit.

const SAMPLES = path.resolve(import.meta.dirname, '../../samples')

/** How a reported error renders, whether from the compiler or the runtime. */
const ERROR = /^(Parser|Runtime|Docstring) error/m

function sample(name: string): string {
  return readFileSync(path.join(SAMPLES, name), 'utf-8')
}

describe('the .scm samples', () => {
  // showcase.scm is pure and runs anywhere, including under `npm run cli`;
  // libs.scm reaches for `document` through the canvas, html and reactive
  // libraries, so jsdom is the tier that can run it (see samples/README.md).
  test.each(['showcase.scm', 'libs.scm'])(
    '%s runs without error',
    async (name) => {
      const log = await runProgram(sample(name))

      expect(log.filter((line) => ERROR.test(line))).toEqual([])
      expect(log.length).toBeGreaterThan(0)
    },
    30000,
  )
})
