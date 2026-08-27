import { beforeAll, beforeEach, describe, expect, test } from 'vitest'
import TextRenderer from '../../src/lpm/renderers/text'
import Scamper, { initialize } from '../../src/scamper'
import * as LPM from '../../src/lpm'
import { Loc } from '../../src/lpm/range'
import { getFS, localBackend, setBackend } from '../../src/fs'
import { MockFileSystem } from '../stubs/mock-file-system'
import { runProgram } from '../harness.js'

beforeAll(async () => {
  await initialize()
  // Importing scamper.ts kicks off its renderer registration as a
  // fire-and-forget module-load side effect. These tests finish faster than it
  // resolves, so settle it here; landing after teardown makes vitest report an
  // unhandled rejection and can fail an unrelated file.
  await import('../../src/app/web/renderers.js')
})

// Regression test for #366: running a program with *no statements* aborted with
//
//   ICE: Scheduling invariant violated: scheduling completed fibers is
//   disallowed! (Scheduler.schedule)
//
// `Fiber.isDone()` is `currStmtIdx >= prog.length`, so a fiber built from a
// zero-statement program is already done at construction. `execute()` handed it
// straight to the scheduler, which rejects a finished fiber outright. The throw
// escaped `execute()` synchronously -- it never returned, so there was no run to
// settle -- and pressing Run on an empty file silently did nothing.
//
// Same bug *class* as #341, but a different path: #341 re-scheduled a fiber
// that finished mid-run, while this one never starts.

/** Runs `src` through the IDE's entry point, returning its output log. */
async function run(src: string): Promise<string[]> {
  const ch = new LPM.LoggingChannel()
  const req = await Scamper.getInstance().execute({ src, out: ch, err: ch })
  if (req === null) throw new Error('program did not compile')
  await req.done
  return ch.log as string[]
}

describe('#366: a program with no statements', () => {
  test('runs as a no-op instead of raising a scheduling ICE', async () => {
    expect(await run('')).toEqual([])
  })

  test('is still a no-op for whitespace only', async () => {
    expect(await run('   \n\n\t  \n')).toEqual([])
  })

  test('is still a no-op for comments only', async () => {
    expect(await run('; just a comment\n; and another\n')).toEqual([])
  })

  test('hands back a settled run, so the IDE leaves its running state', async () => {
    const ch = new LPM.LoggingChannel()
    const req = await Scamper.getInstance().execute({
      src: '',
      out: ch,
      err: ch,
    })
    if (req === null) throw new Error('program did not compile')
    await expect(req.done).resolves.toBeUndefined()
  })

  // Tracing and stepping are where the returned shape carries the most: the
  // `tracing` flag, and a step-mode run whose gate the scheduler never opened.
  test('is a no-op when traced, and reports itself as traced', async () => {
    const ch = new LPM.LoggingChannel()
    const req = await Scamper.getInstance().execute({
      src: '',
      out: ch,
      err: ch,
      isTracing: true,
    })
    if (req === null) throw new Error('program did not compile')
    expect(req.tracing).toBe(true)
    await req.done
    expect(ch.log).toEqual([])
  })

  test('is a no-op when stepping, and its step controls stay harmless', async () => {
    const ch = new LPM.LoggingChannel()
    const req = await Scamper.getInstance().execute({
      src: '',
      out: ch,
      err: ch,
      stepping: true,
    })
    if (req === null) throw new Error('program did not compile')
    expect(req.tracing).toBe(true)
    await req.done
    // The scheduler never learned this id, so the IDE's controls must no-op
    // rather than throw on it.
    const scamper = Scamper.getInstance()
    expect(() => {
      scamper.step(req.id)
    }).not.toThrow()
    expect(() => {
      scamper.cancel(req.id)
    }).not.toThrow()
    expect(ch.log).toEqual([])
  })

  test('a single statement still runs normally', async () => {
    expect(await run('(display 42)')).toEqual(['42'])
  })
})

// The other three `scheduler.schedule` call sites *in src/scamper.ts* take the
// same shape, but each is guarded by an earlier early-return: `traceStatement`
// finds no statement under the cursor, `query` compiles no queried range, and
// `spawnClosure` always builds a one-statement program. These pin that, so a
// future change that drops one of those guards fails here rather than at a
// user's keyboard. The fifth site, in the scheduler itself, was genuinely
// exposed -- see the import tests below.
describe('#366: the sibling entry points on an empty program', () => {
  test('traceStatement returns null rather than scheduling', async () => {
    const ch = new LPM.LoggingChannel()
    const result = await Scamper.getInstance().traceStatement({
      src: '',
      cursorLoc: new Loc(1, 1, 0),
      err: ch,
      maxSteps: 100,
    })
    expect(result).toBeNull()
  })

  test('query reports a diagnostic rather than scheduling', async () => {
    const ch = new LPM.LoggingChannel()
    await Scamper.getInstance().query({
      src: '',
      err: ch,
      queryLoc: new Loc(1, 1, 0),
    })
    // No queried range compiles out of an empty program, so query returns on a
    // diagnostic. The point is only that it never reaches the scheduler, so
    // assert that rather than pinning an unrelated parser message.
    expect(ch.log).toHaveLength(1)
    expect(ch.log.map((v) => TextRenderer.render(v)).join()).not.toContain(
      'Scheduling invariant',
    )
  })
})

// The scheduler has a fifth `schedule` call site that #366's fix in
// `execute()` does not reach: the `import-file` branch, which builds a fiber
// for the imported module's own program. A module file that is empty,
// whitespace-only, or comment-only compiles to zero statements, so that fiber
// is born done and `schedule` rejects it -- this time from inside a detached
// promise, with the importing task already pulled out of the run queue. The
// importer is never resumed, so the run hangs rather than merely doing
// nothing: a worse symptom than the one #366 reported.
describe('#366: importing a file with no statements', () => {
  beforeEach(async () => {
    setBackend(localBackend(await MockFileSystem.create()))
  })

  test('binds nothing and lets the importer continue', async () => {
    await getFS().saveFile('empty.scm', '')
    expect(await runProgram('(import "empty.scm")\n(display 1)')).toEqual(['1'])
  })

  test('is still fine as the last statement', async () => {
    await getFS().saveFile('empty.scm', '')
    expect(await runProgram('(display 1)\n(import "empty.scm")')).toEqual(['1'])
  })

  test('is still fine for a comment-only module', async () => {
    await getFS().saveFile('comments.scm', '; nothing here\n; nor here\n')
    expect(await runProgram('(import "comments.scm")\n(display 2)')).toEqual([
      '2',
    ])
  })

  test('a module with exports still binds them', async () => {
    // The behavior that must survive the fix.
    await getFS().saveFile('mod.scm', '(define-export helper 42)\n')
    expect(await runProgram('(import "mod.scm")\n(display helper)')).toEqual([
      '42',
    ])
  })
})
