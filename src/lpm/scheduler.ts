import { ErrorChannel, ICE, OutputChannel, ReportError, ScamperError, SuspendSignal, Value } from '.'
import { blockOnStep, Fiber, StepResult } from './fiber'
import { FiberTraceStepper } from './raiser.js'
import { schedulerYield } from './scheduler-yield.js'
import { mkTraceOutput, mkTraceStart } from './trace/index.js'
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
  // Called when the run dies of a non-Scamper failure -- an ICE, or a genuine
  // bug in the runtime. The scheduler loop runs detached, so without this such
  // an error surfaces only as an unhandled rejection and `onComplete` never
  // fires, leaving the task's owner waiting forever. A task that omits it keeps
  // the old behavior: the error is rethrown out of the loop.
  onFatal?: (e: unknown) => void
}

export interface DisplayTask extends BaseSchedulerTask {
  out: OutputChannel
  isTracing: boolean
  /**
   * The program's source text, so output can be captioned with the statement
   * that produced it (statements carry ranges, not the text they span).
   * Absent for a task whose channel does not want captions.
   */
  src?: string
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
  // True only while the task is parked at a reduction (not in the run queue and
  // not mid-block-on). Guards wakeGate so a running or async-suspended task is
  // never re-scheduled onto the queue twice.
  parked: boolean
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
  // Traced tasks that have already emitted their opening line. Cleared when the
  // task ends.
  private tracesStarted = new Set<SchedulerId>()
  // The next statement index each task has still to caption with its source.
  // Captions run in program order and each statement gets exactly one, so a
  // statement emitting ten reductions is captioned once, and one emitting
  // nothing is captioned all the same. Cleared when the task ends.
  private nextCaption = new Map<SchedulerId, number>()
  private isRunning = false
  // The task whose fiber is stepping right now, or undefined between steps.
  // A library function running inside a step uses this to find out which run it
  // belongs to, so a callback it registers is tied to that run rather than to
  // whichever program happened to start last (#375).
  private steppingTaskId: SchedulerId | undefined
  // allows for resuming execution
  private currTaskIdx = 0
  private timeQuantum: number = 1000 / DEFAULT_REFRESH_RATE
  private controller = new AbortController()

  /**
   * @returns the id of the run whose fiber is stepping, or undefined if called
   *          from outside a step -- a DOM callback or timer, say.
   *
   * What lets a library function registering a long-lived handler capture the
   * run it belongs to (#375). See `currentRun` in src/lpm/spawn.ts.
   */
  currentTaskId(): SchedulerId | undefined {
    return this.steppingTaskId
  }

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
        parked: false,
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
    this.tracesStarted.delete(id)
    this.nextCaption.delete(id)
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
        // processStepResult, which runs it and resumes the fiber with the
        // result. The signal's range is the call that suspended (see applyFn),
        // carried along so a rejection can be reported there.
        return blockOnStep(e.action, e.range)
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

  /**
   * @returns true when this step took over managing the task's place in the run
   * queue -- it has already dequeued the task and will re-schedule it (or signal
   * its completion) itself, asynchronously. The caller must then NOT also
   * advance/remove it: doing so removes a *second* task, or trips
   * removeTaskFromQueue's atomicity check when the queue is now empty.
   *
   * N.B., stepTask's isReportTask error branch also dequeues (via endCurrFiber)
   * and then yields `undefined` here, which reports false -- the one place that
   * does not follow this rule. It is harmless today only because that fiber is
   * never done at that point, so the caller's moveNextTask just mis-advances
   * currTaskIdx rather than removing a second task.
   */
  async processStepResult(
    stepResult: StepResult | undefined,
    task: SchedulerTask,
  ): Promise<boolean> {
    const fiber = task.fiber
    if (!stepResult) {
      return false
    }
    if (stepResult.tag === 'import-file') {
      // Resolved lazily, never at module load: src/fs reaches OPFS, and this
      // module is pulled in during test setup -- an eager import would capture
      // the real file system before a test could mock it. Same reason
      // src/scheme/scope.ts and src/js/file do it this way.
      const { getFS } = await import('../fs')
      // The existence probe can fail outright -- the FS singleton may be
      // uninitialized, or the host may refuse the name (one that reaches
      // outside the working directory, say -- see #340). Report that to the
      // task's error channel: letting it escape execute() would kill the
      // scheduler loop and silently stop stepping every other running task.
      let exists: boolean
      try {
        exists = await getFS().fileExists(stepResult.filename)
      } catch (e) {
        task.err.report(
          e instanceof ScamperError
            ? e
            : new ScamperError(
                'Runtime',
                `Attempted to import file "${stepResult.filename}" but it could not be read!`,
              ),
        )
        this.endCurrFiber()
        return true
      }
      if (!exists) {
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
              // A module loads with the standard library in scope, exactly as a
              // user program does, so its top-level statements resolve at load
              // time (notably `struct`, which expands to `runtime` calls). The
              // library sits in the env's imports, not its top level, so it is
              // not re-exported by the module (see Env.getTopLevelAsModule).
              // Closures defined in an imported file are stepped over in
              // traces, like the builtin libraries (see Closure.stepOver).
              const moduleFiber = new Fiber(prog, S.mkInitialEnv(), true)
              const id = crypto.randomUUID()
              // Binds the module's exports into the importer and resumes it.
              const finishImport = () => {
                // Only the file's declared exports are visible to the importer.
                const mod = moduleFiber.getModule()
                // A qualified file import (alias set) is reachable only as
                // `alias.member`; an unqualified one injects into scope.
                fiber.topLevelEnv =
                  stepResult.alias !== undefined
                    ? fiber.topLevelEnv.extendWithQualifiedImport(
                        stepResult.alias,
                        mod,
                      )
                    : fiber.topLevelEnv.extendWithImport(
                        stepResult.filename,
                        mod,
                      )
                fiber.advanceStmt()
                this.resumeOrComplete(task)
              }
              // An empty (or comment-only) module compiles to zero statements,
              // so its fiber is born done and `schedule` would reject it
              // (#366). Here that throw lands in this detached promise with the
              // importer already dequeued, so the run would hang rather than
              // merely do nothing. There is nothing to run: bind its (empty)
              // exports and carry on.
              if (moduleFiber.isDone()) {
                finishImport()
                return
              }
              this.schedule({
                id,
                fiber: moduleFiber,
                err: task.err,
                // A fatal error inside the module would otherwise kill the loop
                // with the importer already dequeued, stranding it forever.
                onFatal: (e: unknown) => {
                  task.err.report(
                    new ScamperError(
                      'Runtime',
                      `Attempted to import file "${stepResult.filename}" but it failed to run: ${e instanceof Error ? e.toString() : String(e)}`,
                    ),
                  )
                  fiber.advanceStmt()
                  this.resumeOrComplete(task)
                },
                onComplete: finishImport,
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
              this.resumeOrComplete(task)
            },
          )
      }
      // This step is fully handled asynchronously; do not fall through to the
      // display-task branch (which would treat it as a reduction and park).
      return true
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
          // The error was raised inside the action, far from the call that
          // suspended the fiber, so it arrives unlocated. Point it at that call
          // -- the range the step carries -- unless it named a site itself
          // (#342). Done before handleError so a with-handler sees it too.
          scamperErr.range ??= stepResult.range
          if (!fiber.handleError(scamperErr)) {
            task.err.report(scamperErr)
            fiber.advanceStmt()
          }
          this.resumeOrComplete(task)
        },
      )
      // Handled asynchronously; don't fall through to the display-task branch
      // (which would re-process this suspended fiber as a reduction and re-park,
      // double-removing it from the queue).
      return true
    }

    if (!isDisplayTask(task)) {
      return false
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
      // The statement handler advanced the fiber before returning, so the
      // statement that produced this value is the one just behind the index.
      if (task.isTracing && task.stepper && fiber.lastResult !== null) {
        const v = task.stepper.final(fiber.lastResult)
        if (v !== undefined) {
          this.captionUpTo(task, fiber.stmtIndex - 1)
          out.send(this.mkTraceValue(task, v))
          emittedVisible = true
        }
      } else {
        this.captionUpTo(task, fiber.stmtIndex - 1)
        out.send(fiber.lastResult)
      }
    } else if (!isMinor && task.isTracing && task.stepper) {
      // A trace (major) step: render the reduction, if it is user-visible.
      const v = task.stepper.render(fiber)
      if (v !== undefined) {
        // Mid-statement, so the index still points at the statement running.
        this.captionUpTo(task, fiber.stmtIndex)
        out.send(this.mkTraceValue(task, v))
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
        // parkInGate dequeued the task, so the caller must leave currTaskIdx
        // alone -- removeTaskFromQueue swapped a different task into this slot,
        // and advancing past it would skip that task for a round.
        this.parkInGate(task)
        return true
      }
    }

    if (isMinor) {
      this.currTaskIdx++
    }
    return false
  }

  /**
   * Wraps a traced run's reduction for output. The run's *first* emitted value
   * renders bare and every later one carries the `-->` reduction marker, which
   * reads as "reduced from the line above".
   *
   * N.B. this is per-*run*, not per-statement: a fresh fiber has no frames to
   * raise, so the opening line can only be the first thing actually emitted --
   * for `(define x 5)` that is the defined value, not the source text. Later
   * statements therefore have their opening state marked `-->` even though
   * nothing reduced to it. Longstanding CLI behavior, preserved deliberately;
   * per-statement tracking would change the pinned `--trace` output.
   */
  private mkTraceValue(task: DisplayTask, v: Value): Value {
    if (this.tracesStarted.has(task.id)) {
      return mkTraceOutput(v)
    }
    this.tracesStarted.add(task.id)
    return mkTraceStart('', v)
  }

  /**
   * Captions every statement through `stmtIdx` that has not been captioned
   * yet, in program order.
   *
   * Statements that produce no output -- an import, a define -- are captioned
   * too, each in its own caption rather than lumped in with the next one that
   * does print. They are emitted here, on the way past, because that is where
   * they belong in the output: after whatever the previous statement printed
   * and before whatever the next one does.
   *
   * @param stmtIdx the last statement to caption. Which one that is depends on
   *        where the caller sits: a completed statement has already advanced
   *        the fiber past itself, while a mid-statement reduction has not.
   */
  private captionUpTo(task: DisplayTask, stmtIdx: number): void {
    const src = task.src
    const beginStatement = task.out.beginStatement
    if (src === undefined || beginStatement === undefined) return

    let next = this.nextCaption.get(task.id) ?? 0
    for (; next <= stmtIdx; next++) {
      const stmt = task.fiber.statementAt(next)
      if (stmt === undefined) break
      // Macro expansion can leave a statement pointing into a library rather
      // than into this source (see the contract-error ranges), so a range that
      // does not land inside `src` is skipped rather than shown as a slice of
      // the wrong file.
      const { begin, end } = stmt.range
      if (begin.idx < 0 || end.idx < begin.idx || end.idx >= src.length) continue
      const text = src.slice(begin.idx, end.idx + 1).trim()
      if (text.length > 0) beginStatement.call(task.out, text, next)
    }
    this.nextCaption.set(task.id, next)
  }

  /**
   * Captions whatever is left once a task has run to the end -- the trailing
   * statements that printed nothing, which no output ever came along to caption
   * on the way past.
   */
  private captionRemaining(task: SchedulerTask): void {
    if (isDisplayTask(task) && task.fiber.isDone()) {
      this.captionUpTo(task, task.fiber.statementCount - 1)
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
      gate.parked = true
      const resolve = gate.resolve
      gate.resolve = () => {}
      resolve()
    }
  }

  /** Re-schedules a parked task (reviving the idle loop, exactly as block-on's
   * `this.schedule(task)` does). No-op unless the task is genuinely parked. */
  private wakeGate(id: SchedulerId): void {
    const gate = this.steppingGates.get(id)
    // Only wake a *parked* task. A running task, or one suspended mid-block-on
    // (removed from the queue but not parked), must not be re-scheduled -- doing
    // so would run the fiber twice / double-queue it.
    if (!gate || !gate.parked) {
      return
    }
    if (gate.task.fiber.isDone()) {
      this.steppingGates.delete(id)
      this.tracesStarted.delete(id)
      this.nextCaption.delete(id)
      const resolve = gate.resolve
      gate.task.onComplete?.()
      resolve()
      return
    }
    gate.parked = false
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
    // Settle any previously-pending resume awaiter before taking over its slot,
    // so its promise can't be lost (which would hang it forever).
    const prev = gate.resolve
    gate.resolve = () => {}
    prev()
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
        try {
          this.steppingTaskId = task.id
          let stepResult: StepResult | undefined
          try {
            stepResult = this.stepTask(task)
          } finally {
            // Only across the step itself: `processStepResult` awaits, and
            // anything running during that await belongs to no task.
            this.steppingTaskId = undefined
          }
          // A step that suspended the fiber (block-on, import-file) or parked it
          // has already taken the task out of the run queue and owns re-scheduling
          // it. Advancing here as well would remove a second task -- or, if the
          // async action already finished the program during the await above,
          // trip removeTaskFromQueue's atomicity check on an empty queue.
          if (!(await this.processStepResult(stepResult, task))) {
            this.moveNextTask(task.fiber)
          }
        } catch (e) {
          // An ICE or a genuine runtime bug (stepTask rethrows anything that is
          // not a ScamperError). This loop is detached, so rethrowing would
          // strand the task's owner on a promise that never settles.
          // Drop the task either way: it is poisoned, and leaving it queued
          // means the next round dies on it again.
          this.dropTask(task)
          if (task.onFatal === undefined) {
            // Nobody to hand this to, so let it escape as an unhandled
            // rejection (the old behavior) -- but clear isRunning first, or
            // resumeExecution() sees a "running" loop that has in fact died and
            // returns early forever, wedging this scheduler (and, for the
            // Scamper singleton, every later run) with nothing surfaced.
            this.isRunning = false
            throw e
          }
          task.onFatal(e)
        }
      }
    }
  }

  /**
   * Forgets `task` entirely: out of the run queue (wherever it sits) and out of
   * both id-keyed maps. Unlike removeTaskFromQueue this is positional-agnostic
   * and tolerates an already-dequeued task, since a fatal error can strike
   * either side of a dequeue.
   */
  private dropTask(task: SchedulerTask): void {
    const i = this.tasks.findIndex((t) => t.id === task.id)
    if (i !== -1) {
      this.tasks.splice(i, 1)
    }
    // Resolve the gate as cancelTask/endCurrFiber do: a step-mode run killed
    // mid-burst still has a resume() awaiter, and the IDE's step buttons stay
    // disabled until it settles.
    const gate = this.steppingGates.get(task.id)
    if (gate) {
      this.steppingGates.delete(task.id)
      gate.resolve()
    }
    this.tracesStarted.delete(task.id)
    this.nextCaption.delete(task.id)
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
      this.captionRemaining(task)
      this.tracesStarted.delete(task.id)
      this.nextCaption.delete(task.id)
      const gate = this.steppingGates.get(task.id)
      if (gate) {
        this.steppingGates.delete(task.id)
        gate.resolve()
      }
      task.onComplete?.()
    }
  }

  /**
   * Returns a task that an async branch dequeued -- a file import, a blocking
   * primitive -- to the run queue now that its action has settled, or signals
   * its completion if that action's statement was the program's last.
   *
   * N.B., the isDone check is the point: `schedule` rejects a finished fiber, so
   * re-scheduling one raises an ICE from inside a detached promise, killing the
   * run silently (#341). The task is already out of the queue at this point, so
   * completion is signaled directly rather than through endCurrFiber.
   */
  private resumeOrComplete(task: SchedulerTask) {
    if (task.fiber.isDone()) {
      this.captionRemaining(task)
      this.tracesStarted.delete(task.id)
      this.nextCaption.delete(task.id)
      task.onComplete?.()
    } else {
      this.schedule(task)
    }
  }

  private moveNextTask(currFiber: Fiber) {
    if (!currFiber.isDone()) {
      this.currTaskIdx++
      return
    }
    this.endCurrFiber()
  }

  private reportAndUnwind(e: ScamperError, task: DisplayTask) {
    // An error is output too, and which statement raised it is the first thing
    // worth knowing. The fiber has not advanced past the failing statement yet.
    this.captionUpTo(task, task.fiber.stmtIndex)
    task.err.report(e)
    task.fiber.advanceStmt()
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
