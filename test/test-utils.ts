import {
  DisplayStep,
  Fiber,
  StepResult,
  TraceStep,
} from "../src/lpm/fiber"
import { LoggingChannel, Prog, Value } from "../src/lpm"
import { SchedulerTask } from "../src/scheduler"

export type { SchedulerTask }

export function makeTestFiber(prog: Prog): Fiber {
  const fiber = new Fiber(prog)
  fiber.topLevelEnv.set("+", (a: number, b: number) => a + b)
  fiber.topLevelEnv.set("-", (a: number, b: number) => a - b)
  fiber.topLevelEnv.set("*", (a: number, b: number) => a * b)
  return fiber
}

// The scheduler runs its inner loop for one time quantum (default ~17ms at
// 60fps) before yielding. We sleep long enough that the scheduler will have
// burned through several quanta and (importantly) so that pauseExecution has
// time to take effect after the current quantum drains.
export const QUANTUM_WAIT_MS = 100

let schedulerYieldPatched = false

/**
 * In vitest/jsdom the scheduler-polyfill's `yield()` resolves without handing
 * off to timer macrotasks, so a running `#execute()` loop can starve
 * `setTimeout`-based sleeps. Patch yield once per test file so quanta still
 * use the real API but timers can interleave.
 */
export function patchSchedulerYieldForTests(): void {
  if (schedulerYieldPatched) {
    return
  }
  schedulerYieldPatched = true
  const origYield = scheduler.yield.bind(scheduler)
  scheduler.yield = () =>
    origYield().then(
      () =>
        new Promise<void>((resolve) => {
          setTimeout(resolve, 0)
        }),
    )
}

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

export class MockFiber {
  isProcessingBlk = false
  isDone: () => boolean = () => false
  lastResult: Value = null
  lastStatement: { tag: string } = { tag: "stmtexp" }
  stepCallCount = 0
  stepImpl: () => StepResult = () => TraceStep

  step(): StepResult {
    this.stepCallCount++
    const result = this.stepImpl()
    if (
      result === TraceStep &&
      this.lastStatement.tag === "disp" &&
      !this.isProcessingBlk
    ) {
      return DisplayStep
    }
    return result
  }
}

export interface TestTask extends SchedulerTask {
  ch: LoggingChannel
}

export function makeTask(
  fiber: MockFiber | Fiber,
  isTracing = false,
): TestTask {
  const ch = new LoggingChannel(false, false)
  return {
    fiber: fiber as unknown as Fiber,
    out: ch,
    err: ch,
    isTracing,
    ch,
  }
}

/**
 * Installs a process-level `unhandledRejection` handler that swallows
 * rejections for the duration of `fn`, so tests that intentionally trigger
 * unhandled rejections (e.g., the scheduler re-throwing an ICE) don't fail
 * the test runner.
 */
export async function withSuppressedRejections<T>(
  fn: () => Promise<T>,
): Promise<T> {
  const swallow = () => {
    /* intentionally a no-op: we only need a handler attached so the
       unhandled rejection doesn't fail the test runner. */
  }
  process.on("unhandledRejection", swallow)
  try {
    return await fn()
  } finally {
    // give the microtask queue a chance to flush so we observe the rejection
    // before pulling our handler off
    await sleep(QUANTUM_WAIT_MS)
    process.off("unhandledRejection", swallow)
  }
}
