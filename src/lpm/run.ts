import { Fiber } from './fiber.js'
import { ErrorChannel, OutputChannel } from './output/channel.js'
import { FiberTraceStepper } from './raiser.js'
import { Scheduler } from './scheduler.js'

/** How a program's output, errors, and (optionally) reduction trace are wired. */
export interface RunFiberOptions {
  out: OutputChannel
  err: ErrorChannel
  /** Emit each user-visible reduction as a trace step. Requires `stepper`. */
  isTracing?: boolean
  stepper?: FiberTraceStepper
}

/**
 * Runs `fiber` to completion on its own scheduler.
 *
 * This is the way to run a program. Only the scheduler services blocking
 * primitives (`with-file`, the `file` library, `with-image-from-url`, which
 * suspend the fiber mid-expression) and file imports; a hand-stepped fiber sees
 * a SuspendSignal escape as an uncaught throw and ignores import steps.
 *
 * @returns a promise that resolves when the program finishes, and rejects if it
 * dies of an ICE or other non-Scamper error. Ordinary runtime errors are
 * reported to `err` and do not reject -- the run continues at the next
 * statement, as it does in the IDE.
 */
let nextRunId = 0

export function runFiberOnScheduler(
  fiber: Fiber,
  opts: RunFiberOptions,
): Promise<void> {
  // A program with no statements is born done, and `schedule` rejects a
  // completed fiber. There is nothing to run.
  if (fiber.isDone()) {
    return Promise.resolve()
  }
  const sched = new Scheduler()
  return new Promise<void>((resolve, reject) => {
    sched.schedule({
      // A counter, not crypto.randomUUID: this runs during initialize() to load
      // the builtin libraries, and randomUUID needs a secure context -- using it
      // here would turn an insecure-origin deployment from "Run fails" into "the
      // app never boots". Uniqueness within this scheduler is all that is needed
      // (it holds exactly one task).
      id: `run-${(nextRunId++).toString()}`,
      fiber,
      out: opts.out,
      err: opts.err,
      isTracing: opts.isTracing ?? false,
      stepper: opts.stepper,
      onComplete: resolve,
      onFatal: reject,
    })
  }).finally(() => {
    // The loop has no more tasks and has already idled out; stop it explicitly
    // so a stray quantum can't outlive the run.
    sched.pauseExecution()
  })
}
