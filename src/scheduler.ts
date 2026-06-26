import {
  ErrorChannel,
  ICE,
  OutputChannel,
  ReportError,
  ScamperError,
} from "./lpm"
import {
  Fiber,
  StepResult,
} from "./lpm/fiber"
import "scheduler-polyfill"
import { mkTraceOutput } from "./lpm/trace"
import { getFS } from './fs'

const DEFAULT_REFRESH_RATE = 60

export type SchedulerId = string
interface BaseSchedulerTask {
  id: SchedulerId
  fiber: Fiber
}
export interface DisplayTask extends BaseSchedulerTask {
  out: OutputChannel
  err: ErrorChannel
  isTracing: boolean
}
export interface QueryTask extends BaseSchedulerTask {
  err: ErrorChannel
}
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

        if (fiber.isRunning) {
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
          if (stepResult.tag === 'import-file') {
            if (!await getFS().fileExists(stepResult.filename)) {
              throw new ScamperError(
                "Runtime",
                `Attempted to import file "${stepResult.filename}" but it does not exist!`,
              )
            } else {
              fiber.pause()
              getFS().loadFile(stepResult.filename).then((_src) => {
                // TODO: run src in a new fiber and then inject the fiber's globals into
                // the current fiber's top-level scope
                fiber.resume()
              },
              (_err) => {
                throw new ScamperError(
                  "Runtime",
                  `Attempted to import file "${stepResult.filename}" but it failed to load!`,
                )
              })
            }
          }

          if (isDisplayTask(task)) {
            const { out, isTracing } = task
            // we don't output minor steps (for now)
            // TODO: maybe consider fine-grained tracing?
            if (stepResult.tag === "minor" || stepResult.tag === "yield") {
              this.#currTaskIdx++
              continue
            }
            // we always output if we just completed a display statement
            if (stepResult.tag === "display") {
              out.send(fiber.lastResult)
            }
            // implied that stepResult === TraceStep
            else if (isTracing) {
              // package it up in a nice little trace output and send it
              out.send(mkTraceOutput(fiber.lastResult))
            }
          }
        }

        this.#moveNextTask(fiber)
      }

      // let the event loop breathe before we do another batch of tasks
      await scheduler.yield()
    }
  }

  #removeCurrFiber() {
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
  return typeof t === "object" && "out" in t && "err" in t && "isTracing" in t
}
function isReportTask(t: SchedulerTask): t is QueryTask {
  return !isDisplayTask(t)
}
