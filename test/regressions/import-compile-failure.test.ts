import { afterEach, expect, test, vi } from 'vitest'
import * as fs from '../../src/fs'
import { runProgram } from '../harness.js'

// Regression test for the hang found while adding the Gradescope harness
// (#404): importing a file that does not *compile* reported its diagnostics
// and then simply returned, leaving the importer dequeued from the scheduler
// and never resumed. The run never finished -- on the CLI that surfaced as
// Node's "unsettled top-level await" warning appended to the program's error
// output, which is exactly what a student's failed submission produced.
//
// A missing file and a module that fails at run time were both already handled;
// this branch now does what they do, carrying on at the next statement.

/** Backs {@link runProgram}'s file imports with `files`, by name. */
function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) => Promise.resolve(files[f]),
  } as unknown as ReturnType<typeof fs.getFS>)
}

afterEach(() => {
  vi.restoreAllMocks()
})

test('importing a file that does not compile reports it and finishes', async () => {
  mockFS({ 'broken.scm': '(define x (lambda (n) (* 2 n)' })
  const log = await runProgram(`
  (import "broken.scm")
  "after"
  `)
  expect(log[0]).toContain('Malformed define statement')
  // The run continues rather than hanging -- the statement after the failed
  // import still executes.
  expect(log[log.length - 1]).toBe('"after"')
})
