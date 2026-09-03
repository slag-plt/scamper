import { beforeEach, describe, expect, test } from 'vitest'
import { localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import * as Scheme from '../../src/scheme'
import { LoggingChannel } from '../../src/lpm'
import { Fiber } from '../../src/lpm/fiber'
import { runFiberOnScheduler } from '../../src/lpm/run'
import { runProgram } from '../harness.js'

// Regression test for #476: the contract wrappers the standard library is
// loaded with dominated the cost of running anything -- about 240 opcodes per
// contracted builtin call, most of them spent re-checking arguments the
// library had already handed to itself. `(for-range f 0 1000)` cost ~443,000
// steps where the same loop with contracts off cost ~19,000.
//
// A contract check exists to describe a *student's* mistake at their own call
// site, so library code naming another library function by its top-level name
// now reaches the value behind the wrapper (see VarHandler). The checks a
// student can actually reach are unchanged -- that is what the second and
// third groups here pin down.

/** Steps `src` to completion on a scheduler, counting the fiber's steps. */
async function countSteps(src: string): Promise<number> {
  const { prog, diagnostics } = await Scheme.compile(src.trim())
  expect(diagnostics).toEqual([])
  if (prog === undefined) {
    throw new Error('compile produced no program')
  }
  const fiber = new Fiber(prog, Scheme.mkInitialEnv())
  let steps = 0
  const step = fiber.step.bind(fiber)
  fiber.step = () => {
    steps += 1
    return step()
  }
  const out = new LoggingChannel(false)
  await runFiberOnScheduler(fiber, { out, err: out })
  expect(out.errLog).toEqual([])
  return steps
}

describe('#476: library-internal calls skip their contract checks', () => {
  // Budgets, not measurements: each is a little above what the fix costs and
  // far below what it cost before, so the test states the fix is in place
  // without breaking on an unrelated few-opcode change to a library function.
  // Steps, rather than milliseconds, so a loaded machine cannot fail the run.
  test("a library's own loop does not re-check its own arguments", async () => {
    // Was 443,027; the same loop with contracts off costs 19,037.
    expect(await countSteps('(for-range (lambda (i) i) 0 1000)')).toBeLessThan(
      50_000,
    )
  })

  test('map over 1,000 elements stays within a step budget', async () => {
    // Was 587,173; with contracts off, 92,055.
    expect(
      await countSteps('(length (map (lambda (x) x) (range 1000)))'),
    ).toBeLessThan(150_000)
  })
})

describe('#476: contract checks a student can reach are kept', () => {
  test('a builtin handed to a higher-order builtin still checks its argument', async () => {
    // The student wrote `char-upcase`, so its check is theirs, even though the
    // call that reaches it is `map`'s. Skipping it here raised a raw
    // "Cannot read properties of undefined" from the Javascript primitive.
    expect(await runProgram('(map char-upcase (list "h" "e"))')).toEqual([
      'Runtime error [1:1-1:32]: (error) expected a char, received string',
    ])
  })

  test('a direct call to a builtin still reports its own contract', async () => {
    expect(await runProgram('(car 5)')).toEqual([
      'Runtime error [1:1-1:7]: (error) expected pair or nonempty-list, received number',
    ])
  })

  test('a failure inside a library function is blamed on the call the student wrote', async () => {
    // `cadr` documents `v : any`, so its own contract passes and the failure
    // happens in the `(car (cdr v))` it is defined as. That error used to
    // underline a line of prelude.scm; it now points at the student's call.
    expect(await runProgram('(cadr 5)')).toEqual([
      'Runtime error [1:1-1:8]: (cadr) cdr: expected a pair or a non-empty list',
    ])
  })
})

describe('#476: an imported file is the student\'s own code', () => {
  let fs: MockFileSystem

  beforeEach(async () => {
    fs = await MockFileSystem.create()
    setBackend(localBackend(fs))
  })

  test('a builtin called from an imported file still checks its argument', async () => {
    // A file import is stepped over by a trace exactly as a library is, but it
    // is the student's own code: the checks stay on, or a mistake in their
    // helper file surfaces as a Javascript error.
    await fs.saveFile(
      'helpers.scm',
      '(define-export shout (lambda (c) (char-upcase c)))\n',
    )
    expect(
      await runProgram('(import "helpers.scm")\n(shout "h")'),
    ).toEqual([
      // The range is the `(char-upcase c)` in helpers.scm -- their file, and
      // their mistake.
      'Runtime error [1:34-1:48]: (error) expected a char, received string',
    ])
  })
})
