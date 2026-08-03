import { ErrorChannel, ICE, OutputChannel, ReportError, ScamperError, SuspendSignal } from '.'
import { blockOnStep, Fiber, StepResult } from './fiber'
import { FiberTraceStepper } from './raiser.js'
import { schedulerYield } from './scheduler-yield.js'
import { mkTraceOutput } from './trace/index.js'
import { getFS } from '../fs'
import * as S from '../scheme'
import { diagnosticToError } from '../scheme/diagnostic'

const DEFAULT_REFRESH_RATE = 60

///// Scheduler Tasks //////////////////////////////////////////////////////////

export type SchedulerId = string

// How far a paused step-mode run advances when resumed: one user-visible
// reduction, to the next statement boundary, or to completion.
export type StepMode = 'step' | 'statement' | 'all'

interface BaseSchedulerTask {
  id: SchedulerId
  fiber: Fiber
  err: ErrorChannel
  onComplete?: () => void
}

export interface DisplayTask extends BaseSchedulerTask {
  out: OutputChannel
  isTracing: boolean
  // Step mode: the run pauses ("parks") after each user-visible reduction,
  // awaiting a step()/resume() call. `stepper` renders each step (set by Scamper
  // whenever tracing) -- see FiberTraceStepper.
  stepping?: boolean
  stepper?: FiberTraceStepper
}

export type QueryTask = BaseSchedulerTask

export type SchedulerTask = DisplayTask | QueryTask

// A parked step-mode task, keyed by SchedulerId in `steppingGates`: it is NOT in
// the run queue while parked (mirrors how block-on pulls a task out). `resolve`
// wakes any pending resume() awaiter when the task next parks or completes.
interface SteppingGate {
  task: DisplayTask
  mode: StepMode
  resolve: () => void
  // The fiber's statement index as of the last step, so 'statement' mode can
  // detect when a statement completes (the index advances).
  lastStmtIdx: number
}

////////////////////////////////////////////////////////////////////////////////

export class Scheduler {
  // invariant: tasks should ONLY contain non-completed fibers.
  // initialization: tasks is initially empty, so vacuously true.
  // maintenance:
  // - schedule disallows pushing completed fibers
  // - execute should remove tasks that complete during the loop.
  private tasks: SchedulerTask[] = []
  // Parked step-mode tasks, keyed by id. A parked task lives here, not in `tasks`.
  private steppingGates: Map<SchedulerId, SteppingGate> = new Map()
  private isRunning = false
  // allows for resuming execution
  private currTaskIdx = 0
  private timeQuantum: number = 1000 / DEFAULT_REFRESH_RATE
  private controller = new AbortController()

  schedule(task: SchedulerTask): void {
    if (task.fiber.isDone()) {
      throw new ICE(
        'Scheduler.schedule',
        'Scheduling invariant violated: scheduling completed fibers is disallowed!',
      )
    }
    // Seed the step gate on the initial schedule of a step-mode run (not on the
    // re-schedules that wakeGate performs, which must preserve mode/dedup state).
    if (isDisplayTask(task) && task.stepping && !this.steppingGates.has(task.id)) {
      this.steppingGates.set(task.id, {
        task,
        mode: 'step',
        resolve: () => {},
        lastStmtIdx: task.fiber.stmtIndex,
      })
    }
    this.tasks.push(task)
    this.resumeExecution()
  }

  cancelTask(id: SchedulerId): void {
    const wasPaused = this.wasPaused()
    this.pauseExecution()
    // A step-mode task may be parked in a gate (not in `tasks`), running a burst
    // (in both), or a plain task may be in `tasks` only. Handle all cases.
    const gate = this.steppingGates.get(id)
    const taskI = this.tasks.findIndex((t) => t.id === id)
    if (gate === undefined && taskI === -1) {
      if (!wasPaused) {
        this.resumeExecution()
      }
      return
    }
    const errCh = gate?.task.err ?? this.tasks[taskI].err
    errCh.report(new ScamperError('Runtime', 'Evaluation cancelled'))
    if (taskI !== -1) {
      this.tasks.splice(taskI, 1)
    }
    if (gate) {
      this.steppingGates.delete(id)
      gate.resolve() // unblock any pending stepStmt/stepAll awaiter
    }
    if (!wasPaused) {
      this.resumeExecution()
    }
  }

  pauseExecution() {
    this.controller.abort()
    this.isRunning = false
  }

  resumeExecution() {
    if (this.isRunning) {
      return
    }
    this.controller = new AbortController()
    this.isRunning = true
    void this.execute()
  }

  stepTask(task: SchedulerTask): StepResult | undefined {
    const { fiber } = task
    if (fiber.isDone()) {
      throw new ICE(
        'Scheduler.execute',
        'Scheduling invariant violated: a completed fiber remains in the task queue!',
      )
    }

    try {
      return fiber.step()
    } catch (e) {
      if (e instanceof SuspendSignal) {
        // A blocking primitive suspended the fiber; hand the async action to
        // processStepResult, which runs it and resumes the fiber with the result.
        return blockOnStep(e.action)
      }
      if (!(e instanceof ScamperError)) {
        // either the runtime broke and threw an ICE (which is bad)
        // or we have an unexpected error somewhere (which is really bad)
        // either way, we should probably just rethrow this...
        throw e
      }
      // Give an installed with-handler a chance to recover. ReportError (the
      // query/inspection mechanism) is deliberately never caught by a user
      // handler; ICEs are already excluded above.
      if (!(e instanceof ReportError) && fiber.handleError(e)) {
        return undefined
      }
      if (isReportTask(task)) {
        console.debug(e)
        task.err.report(e)
        this.endCurrFiber()
        return undefined
      }
      this.reportAndUnwind(e, task)
      return undefined
    }
  }

  async processStepResult(
    stepResult: StepResult | undefined,
    task: SchedulerTask,
  ) {
    const fiber = task.fiber
    if (!stepResult) {
      return
    }
    if (stepResult.tag === 'import-file') {
      // TODO: this branch (and the getFS()/fileExists() call in particular)
      //  isn't wrapped in a try/catch. If it throws or rejects for any reason
      //  (e.g. the FS singleton isn't initialized, or a real I/O error), the
      //  exception escapes execute() uncaught, which kills the scheduler loop
      //  entirely and silently stops stepping every other running task, not
      //  just this one. Should report the failure to task.err instead.
      if (!(await getFS().fileExists(stepResult.filename))) {
        task.err.report(
          new ScamperError(
            'Runtime',
            `Attempted to import file "${stepResult.filename}" but it does not exist!`,
          ),
        )
        this.endCurrFiber()
      } else {
        this.removeTaskFromQueue(this.currTaskIdx)
        getFS()
          .loadFile(stepResult.filename)
          .then(
            async (_src) => {
              const { prog, diagnostics } = await S.compile(_src)
              diagnostics.forEach((d) => {
                task.err.report(diagnosticToError(d))
              })
              if (prog === undefined) {
                // TODO: error channel receives the compilation errors as a side-effect,
                // but it would be good to signal to the continuation that importing has
                // failed at this step...
                return
              }
              const moduleFiber = new Fiber(prog)
              const id = crypto.randomUUID()
              this.schedule({
                id,
                fiber: moduleFiber,
                err: task.err,
                onComplete: () => {
                  const mod = moduleFiber.topLevelEnv.getTopLevelAsModule()
                  fiber.topLevelEnv = fiber.topLevelEnv.extendWithImport(
                    stepResult.filename,
                    mod,
                  )
                  fiber.advanceStmt()
                  this.schedule(task)
                },
              })
            },
            (_err: unknown) => {
              task.err.report(
                new ScamperError(
                  'Runtime',
                  `Attempted to import file "${stepResult.filename}" but it failed to load!`,
                ),
              )
              fiber.advanceStmt()
              this.schedule(task)
            },
          )
      }
    }

    if (stepResult.tag === 'block-on') {
      // A blocking primitive suspended the fiber mid-expression. Mirror the
      // import-file pattern: pull the task out of the run queue, run the async
      // action, and on completion resume the SAME fiber in place -- pushing the
      // resolved value as the primitive's result (no advanceStmt: we're mid
      // expression, not at a statement boundary).
      this.removeTaskFromQueue(this.currTaskIdx)
      stepResult.action().then(
        (value) => {
          fiber.resumeWithValue(value)
          this.schedule(task)
        },
        (err: unknown) => {
          // A rejected async action surfaces as a runtime error at the blocking
          // call, catchable by an enclosing with-handler (via handleError).
          const scamperErr =
            err instanceof ScamperError
              ? err
              : new ScamperError(
                  'Runtime',
                  err instanceof Error ? err.message : String(err),
                )
          if (!fiber.handleError(scamperErr)) {
            task.err.report(scamperErr)
            fiber.advanceStmt()
          }
          // advanceStmt may have completed the program; a done fiber must not be
          // re-scheduled (its completion is signaled instead).
          if (fiber.isDone()) {
            task.onComplete?.()
          } else {
            this.schedule(task)
          }
        },
      )
    }

    if (!isDisplayTask(task)) {
      return
    }

    const { out } = task
    const gate = task.stepping ? this.steppingGates.get(task.id) : undefined
    const isMinor =
      stepResult.tag === 'minor' || stepResult.tag === 'yield'

    // Emit this step's output, tracking whether it produced a user-visible
    // reduction. A completed statement (display) renders its value as the final
    // reduction step (`--> value`) in a traced run, or raw otherwise.
    let emittedVisible = false
    if (stepResult.tag === 'display') {
      if (task.isTracing && task.stepper && fiber.lastResult !== null) {
        const v = task.stepper.final(fiber.lastResult)
        if (v !== undefined) {
          out.send(mkTraceOutput(v))
          emittedVisible = true
        }
      } else {
        out.send(fiber.lastResult)
      }
    } else if (!isMinor && task.isTracing && task.stepper) {
      // A trace (major) step: render the reduction, if it is user-visible.
      const v = task.stepper.render(fiber)
      if (v !== undefined) {
        out.send(mkTraceOutput(v))
        emittedVisible = true
      }
    }

    // Step-mode pause: 'step' pauses after each user-visible reduction;
    // 'statement' after each completed statement (the index advances -- checked
    // here so we pause *after* the statement's value is emitted, not on the
    // interior step that merely empties the frame stack); 'all' never pauses. A
    // finished fiber is never parked (it completes via moveNextTask instead).
    if (gate) {
      const advanced = fiber.stmtIndex > gate.lastStmtIdx
      gate.lastStmtIdx = fiber.stmtIndex
      const park =
        !fiber.isDone() &&
        (gate.mode === 'step'
          ? emittedVisible
          : gate.mode === 'statement'
            ? advanced
            : false)
      if (park) {
        this.parkInGate(task)
        return
      }
    }

    if (isMinor) {
      this.currTaskIdx++
    }
  }

  /**
   * Pulls the current (stepping) task out of the run queue and parks it in its
   * gate, awaiting a step()/resume(). Mirrors the block-on suspend at
   * `processStepResult`: removeTaskFromQueue + return, never touching
   * currTaskIdx. Wakes any pending resume() awaiter.
   */
  private parkInGate(task: DisplayTask): void {
    this.removeTaskFromQueue(this.currTaskIdx)
    const gate = this.steppingGates.get(task.id)
    if (gate) {
      const resolve = gate.resolve
      gate.resolve = () => {}
      resolve()
    }
  }

  /** Re-schedules a parked task (reviving the idle loop, exactly as block-on's
   * `this.schedule(task)` does). No-op if it is already running or finished. */
  private wakeGate(id: SchedulerId): void {
    const gate = this.steppingGates.get(id)
    if (!gate) {
      return
    }
    if (this.tasks.some((t) => t.id === id)) {
      // Already in the queue (e.g. mid-block-on resume); a task lives in exactly
      // one place, so don't double-schedule.
      return
    }
    if (gate.task.fiber.isDone()) {
      this.steppingGates.delete(id)
      const resolve = gate.resolve
      gate.task.onComplete?.()
      resolve()
      return
    }
    this.schedule(gate.task)
  }

  /** Advance a parked step-mode run by one user-visible reduction, then re-park. */
  step(id: SchedulerId): void {
    const gate = this.steppingGates.get(id)
    if (!gate) {
      return
    }
    gate.mode = 'step'
    this.wakeGate(id)
  }

  /**
   * Resume a parked step-mode run under a pause policy. The returned promise
   * resolves when the run next parks (statement boundary), completes, or is
   * aborted (pauseStepping/cancel).
   */
  resume(id: SchedulerId, mode: StepMode): Promise<void> {
    const gate = this.steppingGates.get(id)
    if (!gate) {
      return Promise.resolve()
    }
    return new Promise<void>((resolve) => {
      gate.mode = mode
      gate.resolve = resolve
      this.wakeGate(id)
    })
  }

  /** Downgrade a running statement/all burst back to single-step, so it re-parks
   * at the next user-visible reduction (keeps the stepping session alive). */
  pauseStepping(id: SchedulerId): void {
    const gate = this.steppingGates.get(id)
    if (gate) {
      gate.mode = 'step'
    }
  }

  private async execute(): Promise<void> {
    while (!this.wasPaused()) {
      if (this.tasks.length === 0) {
        this.isRunning = false
        return
      }
      // Yield before stepping so callers can observe scheduled tasks (e.g. UI
      // run-in-progress) before fibers run in this frame.
      await schedulerYield()
      const startTime = performance.now()
      while (performance.now() - startTime < this.timeQuantum) {
        if (this.wasPaused()) {
          break
        }
        if (this.currTaskIdx >= this.tasks.length) {
          // check if there are any left; if there are none, wait for more
          if (this.tasks.length === 0) {
            break
          }
          // otherwise go back to the beginning
          this.currTaskIdx = 0
        }
        const task = this.tasks.at(this.currTaskIdx)
        if (!task) {
          throw new ICE(
            'Scheduler.execute',
            `Scheduler attempted to execute task #${this.currTaskIdx.toString()} when there are only ${this.tasks.length.toString()} tasks!`,
          )
        }
        const stepResult = this.stepTask(task)
        await this.processStepResult(stepResult, task)
        this.moveNextTask(task.fiber)
      }
    }
  }

  private removeTaskFromQueue(index: number): SchedulerTask | undefined {
    const lastFiber = this.tasks.at(this.tasks.length - 1)
    if (!lastFiber) {
      throw new ICE(
        'Scheduler.removeTaskFromQueue',
        "Loop iteration atomicity error: somehow scheduler's tasks changed mid-iteration!",
      )
    }
    this.tasks[index] = lastFiber
    return this.tasks.pop()
  }

  private endCurrFiber() {
    const task = this.removeTaskFromQueue(this.currTaskIdx)
    if (task) {
      // A completed step-mode run: drop its gate and wake any pending
      // stepStmt/stepAll awaiter, then signal completion.
      const gate = this.steppingGates.get(task.id)
      if (gate) {
        this.steppingGates.delete(task.id)
        gate.resolve()
      }
      task.onComplete?.()
    }
  }

  private moveNextTask(currFiber: Fiber) {
    if (!currFiber.isDone()) {
      this.currTaskIdx++
      return
    }
    this.endCurrFiber()
  }

  private reportAndUnwind(e: ScamperError, { err, fiber }: DisplayTask) {
    err.report(e)
    fiber.advanceStmt()
  }

  private wasPaused(): boolean {
    return !this.isRunning || this.controller.signal.aborted
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
    this.timeQuantum = timeQuantum
  }
}

function isDisplayTask(t: SchedulerTask): t is DisplayTask {
  return typeof t === 'object' && 'out' in t && 'isTracing' in t
}

function isReportTask(t: SchedulerTask): t is QueryTask {
  return !isDisplayTask(t)
}
