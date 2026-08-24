import { beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import * as LPM from '../../src/lpm'
import { Loc } from '../../src/lpm/range'

beforeAll(async () => {
  await initialize()
})

// Regression test for #366: running a program with *no statements* aborted with
//
//   ICE: Scheduling invariant violated: scheduling completed fibers is
//   disallowed! (Scheduler.schedule)
//
// `Fiber.isDone()` is `currStmtIdx >= prog.length`, so a fiber built from a
// zero-statement program is already done at construction. `execute()` handed it
// straight to the scheduler, which rejects a finished fiber outright. The throw
// escaped before `onComplete` could run, so the returned `done` never settled
// either -- pressing Run on an empty file silently did nothing.
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

  test('settles the run it hands back, so the IDE leaves its running state', async () => {
    const ch = new LPM.LoggingChannel()
    const req = await Scamper.getInstance().execute({
      src: '',
      out: ch,
      err: ch,
    })
    expect(req).not.toBeNull()
    // The bug left this pending forever; a timeout here is the failure.
    await expect(req!.done).resolves.toBeUndefined()
  })

  test('a single statement still runs normally', async () => {
    expect(await run('(display 42)')).toEqual(['42'])
  })
})

// The other three `scheduler.schedule` call sites in src/scamper.ts take the
// same shape, but each is guarded by an earlier early-return: `traceStatement`
// finds no statement under the cursor, `query` compiles no queried range, and
// `spawnClosure` always builds a one-statement program. These pin that, so a
// future change that drops one of those guards fails here rather than at a
// user's keyboard.
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
    // No queried range compiles out of an empty program, so query returns on
    // the diagnostic. The point is that it never reaches the scheduler.
    expect(ch.log).toEqual([
      'Parser error: Received invalid query location: 1:1',
    ])
  })
})
