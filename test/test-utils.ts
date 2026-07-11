import { vi } from "vitest"
import { displayStep, Fiber, StepResult, traceStep } from "../src/lpm/fiber"
import {
  LoggingChannel,
  Prog,
  Range,
  ReportError,
  Stmt,
  Value,
} from "../src/lpm"
import { DisplayTask, QueryTask, SchedulerTask } from "../src/lpm/scheduler"
import { SimpleErrorChannel } from "../src/lpm/output/simple-error"
import * as U from "../src/lpm/util"
import * as process from "node:process"
import * as SchedulerYield from "../src/lpm/scheduler-yield"

export type { QueryTask, SchedulerTask }

const MOCK_FIBER_PROG: Prog = [U.mkStmtExp([U.mkLit(null)])]

export function makeTestFiber(prog: Prog): Fiber {
  const fiber = new Fiber(prog)
  fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel(
    ["+", (a: number, b: number) => a + b],
    ["-", (a: number, b: number) => a - b],
    ["*", (a: number, b: number) => a * b],
  )
  return fiber
}

/** Run a real fiber to completion (for schedule-invariant tests). */
export function runFiberToCompletion(fiber: Fiber): void {
  while (!fiber.isDone()) {
    fiber.step()
  }
}

/**
 * A fiber that always reports a trace step and never advances past its first
 * statement, so the scheduler can keep stepping it without it ever
 * completing. Deliberately avoids real import statements: those route
 * through the scheduler's `import-file` handling, which calls the global
 * file system singleton that isn't initialized in tests.
 */
class NeverCompletingFiber extends Fiber {
  constructor() {
    super(MOCK_FIBER_PROG)
  }
  override step(): StepResult {
    return traceStep
  }
}

export function makeNeverCompletingFiber(): Fiber {
  return new NeverCompletingFiber()
}

export interface StepTrackedFiber extends Fiber {
  readonly stepCallCount: number
}

/**
 * Wrap a real fiber's step() with a vitest spy that delegates to the
 * original implementation while counting calls.
 */
export function trackFiberSteps(fiber: Fiber): StepTrackedFiber {
  let stepCallCount = 0
  const realStep = fiber.step.bind(fiber)
  vi.spyOn(fiber, "step").mockImplementation(() => {
    stepCallCount++
    return realStep()
  })
  Object.defineProperty(fiber, "stepCallCount", {
    get: () => stepCallCount,
    configurable: true,
  })
  return fiber as StepTrackedFiber
}

// The scheduler runs its inner loop for one time quantum (default ~17ms at
// 60fps) before yielding. We sleep long enough that the scheduler will have
// burned through several quanta and (importantly) so that pauseExecution has
// time to take effect after the current quantum drains.
export const QUANTUM_WAIT_MS = 100

let schedulerYieldPatched = false

/**
 * In vitest/jsdom, `schedulerYield()`'s MessageChannel-based fallback can
 * still resolve ahead of pending `setTimeout`-based sleeps, so a running
 * `#execute()` loop can starve them. Patch yield once per test file so
 * quanta still use the real implementation but timers can interleave.
 */
export function patchSchedulerYieldForTests(): void {
  if (schedulerYieldPatched) {
    return
  }
  schedulerYieldPatched = true
  const origYield = SchedulerYield.schedulerYield
  vi.spyOn(SchedulerYield, "schedulerYield").mockImplementation(() =>
    origYield().then(
      () =>
        new Promise<void>((resolve) => {
          setTimeout(resolve, 0)
        }),
    ),
  )
}

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

/**
 * Scheduler test double that extends the real Fiber class so interface drift
 * in `src/lpm/fiber.ts` surfaces as type errors. step() is overridden to
 * simulate controlled stepping behavior; everything else stays tied to Fiber.
 */
export class MockFiber extends Fiber {
  stepCallCount = 0
  stepImpl: () => StepResult = () => traceStep

  #mockIsProcessingBlk?: boolean
  #mockLastStatement?: { tag: string }

  constructor() {
    super(MOCK_FIBER_PROG)
  }

  override get isProcessingBlk(): boolean {
    return this.#mockIsProcessingBlk ?? super.isProcessingBlk
  }

  set isProcessingBlk(value: boolean) {
    this.#mockIsProcessingBlk = value
  }

  override get lastStatement(): Stmt {
    return (this.#mockLastStatement ?? super.lastStatement) as Stmt
  }

  set lastStatement(value: { tag: string }) {
    this.#mockLastStatement = value
  }

  override step(): StepResult {
    this.stepCallCount++
    const result = this.stepImpl()
    if (
      result === traceStep &&
      this.lastStatement.tag === "disp" &&
      !this.isProcessingBlk
    ) {
      return displayStep
    }
    return result
  }
}

export interface TestTask extends DisplayTask {
  ch: LoggingChannel
}

export function makeTask(
  fiber: MockFiber | Fiber,
  isTracing = false,
): TestTask {
  const ch = new LoggingChannel(false, false)
  return {
    id: crypto.randomUUID(),
    fiber,
    out: ch,
    err: ch,
    isTracing,
    ch,
  }
}

export interface TestQueryTask extends QueryTask {
  err: SimpleErrorChannel
}

export function makeQueryTask(fiber: MockFiber | Fiber): TestQueryTask {
  const rep = new SimpleErrorChannel()
  return { id: crypto.randomUUID(), fiber, err: rep }
}

/** MockFiber that throws ReportError on its first step. */
export function makeReportThrowingFiber(
  value: Value,
  range: Range = Range.none,
): MockFiber {
  const fiber = new MockFiber()
  fiber.stepImpl = () => {
    throw new ReportError({ tag: "value", value }, range)
  }
  return fiber
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
