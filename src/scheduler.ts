import { ErrorChannel, ICE, OutputChannel, ReportError, ScamperError } from "./lpm"
import { DisplayStep, Fiber, MinorStep, StepResult, YieldStep } from "./lpm/fiber"
import "scheduler-polyfill"
import { mkTraceOutput } from "./lpm/trace"

const DEFAULT_REFRESH_RATE = 60

export type SchedulerId = string
interface BaseSchedulerTask {
  id: SchedulerId
  fiber: Fiber
  err: ErrorChannel
  onComplete?: () => void
}
export interface DisplayTask extends BaseSchedulerTask {
  out: OutputChannel
  isTracing: boolean
}
export type QueryTask = BaseSchedulerTask
export type SchedulerTask = DisplayTask | QueryTask

export class Scheduler {
  // invariant: tasks should ONLY contain non-completed fibers.
  // initialization: tasks is initially empty, so vacuously true.
  // maintenance:
  // - schedule disallows pushing completed fibers
  // - #execute should remove tasks that complete during the loop.
  #tasks: SchedulerTask[] = []
  #isRunning = false
  // allows for resuming execution
  #currTaskIdx = 0
  #timeQuantum: number = 1000 / DEFAULT_REFRESH_RATE
  #controller = new AbortController()

  schedule(task: SchedulerTask): void {
    if (task.fiber.isDone()) {
      throw new ICE(
        "Scheduler.schedule",
        "Scheduling invariant violated: scheduling completed fibers is disallowed!",
      )
    }
    this.#tasks.push(task)
    this.resumeExecution()
  }
  cancelTask(id: SchedulerId): void {
    const wasPaused = this.#wasPaused()
    this.pauseExecution()
    const taskI = this.#tasks.findIndex((t) => t.id === id)
    if (taskI === -1) {
      if (!wasPaused) {
        this.resumeExecution()
      }
      return
    }
    this.#tasks[taskI].err.report(
      new ScamperError("Runtime", "Evaluation cancelled"),
    )
    this.#tasks.splice(taskI, 1)
    if (!wasPaused) {
      this.resumeExecution()
    }
  }
  pauseExecution() {
    this.#controller.abort()
    this.#isRunning = false
  }
  resumeExecution() {
    if (this.#isRunning) {
      return
    }
    this.#controller = new AbortController()
    this.#isRunning = true
    void this.#execute()
  }

  async #execute(): Promise<void> {
    while (!this.#wasPaused()) {
      if (this.#tasks.length === 0) {
        this.#isRunning = false
        return
      }
      // Yield before stepping so callers can observe scheduled tasks (e.g. UI
      // run-in-progress) before fibers run in this frame.
      await scheduler.yield()
      const startTime = performance.now()
      while (performance.now() - startTime < this.#timeQuantum) {
        if (this.#wasPaused()) {
          break
        }
        if (this.#currTaskIdx >= this.#tasks.length) {
          // check if there are any left
          if (this.#tasks.length === 0) {
            // wait for more tasks
            break
          }
          // otherwise go back to the beginning
          this.#currTaskIdx = 0
        }

        const task = this.#tasks.at(this.#currTaskIdx)
        if (!task) {
          throw new ICE(
            "Scheduler.#execute",
            `Scheduler attempted to execute task #${this.#currTaskIdx.toString()} when there are only ${this.#tasks.length.toString()} tasks!`,
          )
        }
        const { fiber } = task
        if (fiber.isDone()) {
          throw new ICE(
            "Scheduler.#execute",
            "Scheduling invariant violated: a completed fiber remains in the task queue!",
          )
        }

        let stepResult: StepResult
        try {
          stepResult = fiber.step()
        } catch (e) {
          if (e instanceof ScamperError) {
            if (isReportTask(task)) {
              if (e instanceof ReportError) {
                console.debug(e.value)
              }
              task.err.report(e)
              this.#removeCurrFiber()
              continue
            }
            this.#reportAndUnwind(e, task)
            continue
          }
          // either the runtime broke and threw an ICE (which is bad)
          // or we have an unexpected error somewhere (which is really bad)
          // either way, we should probably just rethrow this...
          throw e
        }

        if (isDisplayTask(task)) {
          const { out, isTracing } = task
          // we don't output minor steps (for now)
          // TODO: maybe consider fine-grained tracing?
          if (stepResult === MinorStep || stepResult === YieldStep) {
            this.#currTaskIdx++
            continue
          }
          // we always output if we just completed a display statement
          if (stepResult === DisplayStep) {
            out.send(fiber.lastResult)
          }
          // implied that stepResult === TraceStep
          else if (isTracing) {
            // package it up in a nice little trace output and send it
            out.send(mkTraceOutput(fiber.lastResult))
          }
        }

        this.#moveNextTask(fiber)
      }
    }
  }

  #removeCurrFiber() {
    const task = this.#tasks.at(this.#currTaskIdx)
    // clean up fiber if completed
    // we swap the last one here then pop the duplicate off to achieve O(1)
    const lastFiber = this.#tasks.at(this.#tasks.length - 1)
    if (!lastFiber) {
      throw new ICE(
        "Scheduler.#execute",
        "Loop iteration atomicity error: somehow scheduler's tasks changed mid-iteration!",
      )
    }
    this.#tasks[this.#currTaskIdx] = lastFiber
    this.#tasks.pop()
    if (task) {
      task.onComplete?.()
    }
  }
  #moveNextTask(currFiber: Fiber) {
    if (!currFiber.isDone()) {
      this.#currTaskIdx++
      return
    }
    this.#removeCurrFiber()
  }
  #reportAndUnwind(e: ScamperError, { err, fiber }: DisplayTask) {
    err.report(e)
    fiber.advanceStmt()
    this.#moveNextTask(fiber)
  }

  #wasPaused(): boolean {
    return !this.#isRunning || this.#controller.signal.aborted
  }
  async setTimeQuantumFromFPS(): Promise<void> {
    const timeQuantum = await new Promise<number>((resolve) => {
      let numFrames = 0
      const startTime = performance.now()

      function checkRate() {
        numFrames++
        const duration = performance.now() - startTime

        if (duration >= 1000) {
          const fps = Math.floor((numFrames * 1000) / duration)
          resolve(1000 / fps)
          return
        }

        requestAnimationFrame(checkRate)
      }

      requestAnimationFrame(checkRate)
    })
    this.#timeQuantum = timeQuantum
  }
}

function isDisplayTask(t: SchedulerTask): t is DisplayTask {
  return typeof t === "object" && "out" in t && "isTracing" in t
}
function isReportTask(t: SchedulerTask): t is QueryTask {
  return !isDisplayTask(t)
}
