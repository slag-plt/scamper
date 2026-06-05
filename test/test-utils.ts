import { Fiber } from "../src/lpm/fiber"
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
// time to take effect after the current quantum drains. With our setTimeout(0)
// inside MockFiber.step (~1-4ms per step), 100ms gives us comfortably more
// than a single quantum's worth of steps.
export const QUANTUM_WAIT_MS = 100

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

/**
 * A minimal stand-in for {@link Fiber} that implements just what `Scheduler`
 * reads off of it. Each test wires up the behavior it cares about.
 *
 * NOTE: `step()` yields to the macrotask queue via `setTimeout(0)`. Without
 * this, the scheduler's tight `await` inner loop will monopolize the microtask
 * queue and starve the test's own setTimeout-based sleeps -- causing hangs.
 * Real fibers do non-trivial work and are also throttled by `scheduler.yield`,
 * so this matches realistic execution closely enough.
 *
 * NOTE: `isDone` is a function field (not a plain boolean) to match the
 * real `Fiber.isDone()` method signature. Tests can swap it for stateful
 * behavior, e.g. `mock.isDone = () => someFlag`.
 */
export class MockFiber {
  isProcessingBlk = false
  isDone: () => boolean = () => false
  lastResult: Value = null
  lastStatement: { tag: string } = { tag: "stmtexp" }
  stepCallCount = 0
  stepImpl: () => Promise<boolean> = () => Promise.resolve(true)

  async step(): Promise<boolean> {
    this.stepCallCount++
    await new Promise<void>((resolve) => setTimeout(resolve, 0))
    return this.stepImpl()
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
