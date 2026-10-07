import { describe, expect, test, vi } from 'vitest'
import { Scheduler, SchedulerTask } from '../../src/lpm/scheduler'
import { minorStep, StepResult, traceStep } from '../../src/lpm/fiber'
import {
  makeTask,
  MockFiber,
  patchSchedulerYieldForTests,
  waitForSteps,
} from '../util'

patchSchedulerYieldForTests()

// https://github.com/slag-plt/scamper/issues/730
//
// `execute` awaited `processStepResult` after *every* step, and a step is a
// single bytecode op. On a minor step -- a `var`, `lit`, `cls`, or scope/handler
// bookkeeping op, which is most of what a program runs -- that call walks its
// branches and returns false without doing anything: it cannot be an
// import-file or block-on, `endsStatement` is false, it never reaches `send`,
// and with no gate it cannot park. The await alone cost ~75ns of a ~300ns step.
//
// The await is NOT removable in general. cancel-task-stale-index (#515) relies
// on it as a *guaranteed* microtask boundary for a cancel deferred out of a
// `send`, and scheduler-task-starvation (#415) pins per-step round-robin
// fairness. Both still hold because every step that can `send` still awaits;
// those two suites are what protects that, and this one pins the other half --
// that a minor step skips the round-trip, and that a task being single-stepped
// is excluded from the skip so its gate still sees every step.

/**
 * Runs one never-ending task whose every step returns `result`, and reports how
 * many times the scheduler took the full round-trip through
 * `processStepResult`.
 */
async function roundTrips(
  result: StepResult,
  mkTask: (fiber: MockFiber) => SchedulerTask,
): Promise<{ steps: number; roundTrips: number }> {
  const sched = new Scheduler()
  const spy = vi.spyOn(sched, 'processStepResult')
  try {
    const fiber = new MockFiber()
    fiber.stepImpl = () => result
    sched.schedule(mkTask(fiber))
    await waitForSteps(fiber, 50)
    sched.pauseExecution()
    return { steps: fiber.stepCallCount, roundTrips: spy.mock.calls.length }
  } finally {
    spy.mockRestore()
  }
}

describe('a minor step skips the scheduler round-trip (#730)', () => {
  test('a minor step does not reach processStepResult', async () => {
    const { steps, roundTrips: trips } = await roundTrips(minorStep, (f) =>
      makeTask(f),
    )
    expect(steps).toBeGreaterThanOrEqual(50)
    expect(trips).toBe(0)
  })

  test('a step that can emit output still does', async () => {
    // The guarantee #515 rests on: anything that might `send` keeps its await.
    const { steps, roundTrips: trips } = await roundTrips(traceStep, (f) =>
      makeTask(f),
    )
    expect(steps).toBeGreaterThanOrEqual(50)
    expect(trips).toBe(steps)
  })

  test('a task being single-stepped is not skipped', async () => {
    // Its gate records the statement index on every step, so the skip must not
    // apply even though the steps are minor.
    const { steps, roundTrips: trips } = await roundTrips(minorStep, (f) => ({
      ...makeTask(f),
      stepping: true,
    }))
    expect(steps).toBeGreaterThanOrEqual(50)
    expect(trips).toBe(steps)
  })
})
