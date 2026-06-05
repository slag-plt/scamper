import { ErrorChannel, ICE, OutputChannel, ScamperError } from "./lpm"
import { Fiber } from "./lpm/fiber"
import "scheduler-polyfill"
import { mkTraceOutput } from "./lpm/trace"

const DEFAULT_REFRESH_RATE = 60

export interface SchedulerTask {
  fiber: Fiber,
  out: OutputChannel,
  err: ErrorChannel,
  isTracing: boolean,
}

export class Scheduler {
  #tasks: SchedulerTask[] = []
  #isRunning = false
  // allows for resuming execution
  #currTaskIdx = 0
  #timeQuantum = 1000 / DEFAULT_REFRESH_RATE

  /**
   * @returns this scheduler with the time quantum initialized and the execution queue running.
   */
  async init(): Promise<this> {
    this.#timeQuantum = 1000 / (await this.#getRefreshRate())
    // we just start the execution and don't wait for it so we don't block the event loop
    this.resumeExecution()
    return this
  }

  schedule(task: SchedulerTask): void {
    this.#tasks.push(task)
  }
  pauseExecution() {
    this.#isRunning = false
  }
  resumeExecution() {
    if (this.#isRunning) {
      return
    }
    this.#isRunning = true
    void this.#execute()
  }

  async #execute(): Promise<void> {
    while (this.#isRunning) {
      const startTime = performance.now()
      while (performance.now() - startTime < this.#timeQuantum) {
        if (this.#currTaskIdx >= this.#tasks.length) {
          // check if there are any left
          if (this.#tasks.length === 0) {
            return
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
        
        const { fiber, out, err, isTracing } = task
        
        let majorStep
        try {
          majorStep = await fiber.step()
        } catch (e) {
          if (e instanceof ScamperError) {
            err.report(e)
            this.#currTaskIdx++
            continue
          }
          // either the runtime broke and threw an ICE (which is bad)
          // or we have an unexpected error somewhere (which is really bad)
          // either way, we should probably just rethrow this...
          throw e
        }
        this.#currTaskIdx++
        
        // we don't output minor steps (for now)
        // TODO: maybe consider fine-grained tracing?
        if (!majorStep) {
          continue
        }
        // we always output if we just completed a display statement
        if (!fiber.isProcessingBlk && fiber.lastStatement.tag === "disp") {
          out.send(fiber.lastResult)
        } else if (isTracing) {
          // package it up in a nice little trace output and send it
          out.send(mkTraceOutput(fiber.lastResult))
        }
      }

      // let the event loop breathe before we do another batch of tasks
      await scheduler.yield()
    }
  }
  async #getRefreshRate(): Promise<number> {
    return new Promise((resolve) => {
      let numFrames = 0
      const startTime = performance.now()

      function checkRate() {
        numFrames++
        const duration = performance.now() - startTime

        if (duration >= 1000) {
          const fps = Math.floor((numFrames * 1000) / duration)
          resolve(fps)
          return
        }

        requestAnimationFrame(checkRate)
      }

      requestAnimationFrame(checkRate)
    })
  }
}
