// TODO: will eventually replace scamper.ts and scamper-vue.ts
import { builtinLibs, initializeLibs } from './lib'
import {
  Env,
  ErrorChannel,
  Loc,
  mkAp,
  mkLit,
  mkStmtExp,
  OutputChannel,
  Prog,
  Range,
  rangesEqual,
  ScamperError,
  setRunResolver,
  type RunHandle,
  Value,
} from './lpm'
import { Fiber } from './lpm/fiber'
import { DiscardOutput } from './lpm/output/discard'
import { SimpleErrorChannel } from './lpm/output/simple-error'
import {
  DEFAULT_TRACE_STEP_LIMIT,
  TraceCollector,
} from './lpm/output/trace-collector'
import { Scheduler, SchedulerId, StepMode } from './lpm/scheduler'
import { compile, countStatements } from './scheme'
import { makeTraceStepper } from './scheme/trace'
import { diagnosticToError } from './scheme/diagnostic'
import * as SymbolDB from './scheme/symbol-db'

interface ExecutionConfig {
  src: string
}

interface DisplayExecutionConfig extends ExecutionConfig {
  out: OutputChannel
  err: ErrorChannel
  isTracing?: boolean // whether to enable tracing of execution steps
  // Start the run paused in step mode: it advances one user-visible reduction at
  // a time, driven by step()/resume(). Implies tracing.
  stepping?: boolean
}

/**
 * How an embedded run is wired (#375). `env` seeds the top level, which is what
 * lets one reading widget continue another's program.
 */
export interface EmbeddedExecutionConfig extends ExecutionConfig {
  out: OutputChannel
  err: ErrorChannel
  env?: Env
}

interface QueryExecutionConfig extends ExecutionConfig {
  err: ErrorChannel
  queryLoc: Loc
}

interface RunRequest {
  id: SchedulerId
  done: Promise<void>
}
export interface DisplayRequest extends RunRequest {
  tracing: boolean
}
export type QueryRequest = RunRequest

// Re-exported because `EmbeddedExecutionConfig` and `EmbeddedRequest` are
// written in terms of it: an embedder should not have to reach into src/lpm to
// name the type this module hands it.
export type { Env }

/**
 * A handle on an embedded run. `done` resolves to the program's final top-level
 * environment, which is what a widget hands to the next one it feeds (#375).
 */
export interface EmbeddedRequest {
  id: SchedulerId
  done: Promise<Env>
}

/**
 * A REPL: a program written one statement at a time (#399).
 *
 * One run rather than one per entry. Every entry is evaluated in the
 * environment the last one left, and a handler any of them registered -- a
 * timer, a key listener -- sees the definitions made since, exactly as one
 * registered by a program's own top level does. Ending the session tears all of
 * them down together.
 */
export interface ReplSession {
  /** The run every entry is evaluated as. */
  readonly id: SchedulerId
  /**
   * The top level as it stands: the environment the next entry will be
   * evaluated in.
   *
   * Read before an entry runs, this is the environment that entry ran in, and
   * it stays that way -- a top level is persistent, so the defines that later
   * entries make build new environments rather than changing this one. That is
   * what lets an entry be replayed exactly as it was, which is how stepping one
   * works (#424).
   */
  readonly env: Env
  /**
   * Runs the whole of `src` for what it defines, discarding its output, and
   * makes the environment it leaves behind the one entries start from.
   *
   * The output is discarded because this is the file the REPL was opened on,
   * which the person has already run; what they want is its definitions.
   *
   * @returns whether it compiled. Diagnostics and runtime errors reach the
   *          session's error channel either way, and a session that could not
   *          be seeded is still usable -- it just starts from the standard
   *          library.
   */
  seed: (src: string) => Promise<boolean>
  /**
   * Evaluates one statement, sending what it produces to the session's output
   * channel and carrying what it defines forward to the next entry.
   *
   * An entry is one statement. More than that is refused rather than half-run
   * -- a file pasted into the prompt is a mistake worth naming -- and so is
   * none at all, which would otherwise run an empty program and print nothing.
   *
   * @returns whether it ran. False for an entry that was refused or did not
   *          compile -- one that never became part of the program, and so must
   *          not be treated as part of it.
   */
  evaluate: (src: string) => Promise<boolean>
  /** Abandons the entry in flight, if any, leaving the session usable. */
  interrupt: () => void
  /** Ends the session: whatever is running, and every handler it registered. */
  end: () => void
}

// TODO: this and all query-related code should
//  honestly be moved out into a separate singleton
export interface QueryEntry {
  id: SchedulerId
  queriedRange: Range
  err: ErrorChannel
  done: Promise<void>
}

export type QueryMap = ReadonlyMap<number, readonly QueryEntry[]>

export const QUERIES_CHANGED = 'scamper:querieschanged'
export const QUERY_EXPANDED_CHANGED = 'scamper:queryexpandedchanged'

let defaultEnv: Env | undefined
let initialized = false

// Kicks off web/renderers.ts's custom Vue/HTML renderer registration. The
// import stays dynamic and guarded on `window`: that file transitively
// imports Vue single-file components, which the CLI's plain Node runtime
// cannot load (see its header comment), so a static import here would break
// the CLI.
//
// It fires here, at module evaluation, rather than from a shared global call
// site (like a test suite's global setup), so it only ever runs as a
// consequence of *something* importing scamper.ts -- e.g. a test file's own
// import graph. Firing it from such a call site would grab real (unmocked)
// transitive dependencies -- notably src/fs/opfs.ts -- out from under tests
// that mock them, before their `vi.mock(...)` calls have been registered.
//
// initialize() awaits this handle, once, so the registration cannot outlive
// the environment that started it: an in-flight module fetch still resolving
// past a vitest worker's teardown is rejected, and with nothing awaiting it
// that unhandled rejection fails an otherwise green run (#511).
//
// execute()/query() must still NOT await it before scheduling. Doing so once
// delayed task-id generation past the window stopRun()'s cancel-by-id logic
// (see use-scamper-session.ts) needs, so a pending run could dodge
// cancellation and a second run would duplicate its output instead of
// replacing it. initialize() awaits well before any run is scheduled, so
// that hazard is untouched.
const renderersReady: Promise<void> =
  typeof window !== 'undefined'
    ? import('./app/web/renderers.js').then(() => undefined)
    : Promise.resolve()

/**
 * Compiles the builtin libraries and prepares the default top-level
 * environment they're imported into. Must be awaited once, by application
 * startup code, before Scamper.getInstance() (or anything else in this
 * module) is used -- getInstance() throws if called first. Idempotent: a
 * second call is a no-op.
 */
export async function initialize(): Promise<void> {
  if (initialized) {
    return
  }
  await initializeLibs()
  SymbolDB.initialize()
  await renderersReady
  defaultEnv = Env.empty
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    .extendWithImport('runtime', builtinLibs.get('runtime')!)
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    .extendWithImport('prelude', builtinLibs.get('prelude')!)
  initialized = true
}

/** Unreachable once getInstance() has gated on `initialized`. */
/** Ids for embedded runs; see executeEmbedded on why this is not a UUID. */
let nextEmbedId = 0

function getDefaultEnv(): Env {
  if (!defaultEnv) {
    throw new Error("Scamper's default environment used before initialize()")
  }
  return defaultEnv
}

/**
 * One running program: its fiber, where its errors go, and the signal that
 * tears down what it left behind.
 *
 * Kept per run rather than in one set of slots because a page can hold several
 * programs at once (#375). With slots, each new run aborted the one before it
 * and a callback resolved its environment from whichever ran last.
 */
interface RunContext {
  fiber: Fiber
  err: ErrorChannel
  controller: AbortController
}

export default class Scamper {
  // singleton structure
  private static instance?: Scamper
  static getInstance(): Scamper {
    if (!initialized) {
      throw new Error(
        'Scamper.getInstance() called before initialize() completed',
      )
    }
    Scamper.instance ??= new Scamper()
    return Scamper.instance
  }

  /*  =====  instance-related fields  =====  */
  private scheduler: Scheduler
  private _queries = new Map<number, QueryEntry[]>()
  private _expandedQueryId: SchedulerId | null = null
  private queryBus = new EventTarget()
  // Every program currently running on this instance, by scheduler id.
  //
  // A page can hold several at once -- the reading widgets are the reason
  // (#375) -- so a run's fiber, error channel and AbortController live together
  // here rather than in one set of slots holding whichever program started
  // last. With slots, N programs each tore down the one before it, and a
  // callback resolved its environment from the wrong one.
  private runs = new Map<SchedulerId, RunContext>()
  // The foreground program, i.e. the one the IDE's Run button drives. Only this
  // one supersedes, and only this one is what a callback falls back to when it
  // fires from outside any step.
  private mainRunId?: SchedulerId

  private constructor() {
    this.scheduler = new Scheduler()
    // Let library event handlers reach their own run -- to spawn a fiber, or to
    // find the AbortSignal that tears their handler down -- without importing
    // this singleton.
    setRunResolver(() => this.resolveRun())
  }

  /**
   * @returns a handle on the run a library call belongs to: the one whose fiber
   *          is stepping, or the foreground program if the call came from
   *          outside a step.
   *
   * The fallback is what a callback gets when it fires with nothing running and
   * no handle captured. That is the old behaviour, and wrong as soon as a page
   * holds more than one program -- which is why `currentRun()` is documented to
   * be captured at registration rather than called from inside a callback.
   */
  private resolveRun(): RunHandle | undefined {
    const id = this.scheduler.currentTaskId() ?? this.mainRunId
    if (id === undefined) return undefined
    const run = this.runs.get(id)
    if (run === undefined) return undefined
    return {
      spawn: (fn, args, onComplete) => {
        this.spawnClosure(run, fn, args, onComplete)
      },
      signal: run.controller.signal,
    }
  }

  /** Registers a run and returns its context. */
  private beginRun(id: SchedulerId, fiber: Fiber, err: ErrorChannel): RunContext {
    const run = { fiber, err, controller: new AbortController() }
    this.runs.set(id, run)
    return run
  }

  /**
   * Forgets a finished run, and tears down whatever it left behind.
   *
   * Without this the map grows by one entry per widget run and per re-run, and
   * a handler registered by a program that has ended keeps firing.
   */
  private endRun(id: SchedulerId): void {
    const run = this.runs.get(id)
    if (run === undefined) return
    run.controller.abort()
    this.runs.delete(id)
    if (this.mainRunId === id) this.mainRunId = undefined
  }

  /**
   * Runs the closure `fn` applied to `args` as a fresh fiber in the current
   * program's top-level environment (so it sees the user's definitions and
   * imports). Used by event/callback library functions -- a DOM/timer callback
   * fires with no fiber running, so it must originate a new one. `onComplete`
   * receives the closure's result (or null).
   */
  private spawnClosure(
    run: RunContext,
    fn: Value,
    args: Value[],
    onComplete?: (result: Value | null) => void,
  ): void {
    // The run's *evolving* top level, read now rather than captured when the
    // handler was registered, so a callback sees definitions made since.
    const env = run.fiber.topLevelEnv
    const err = run.err
    const prog: Prog = [
      mkStmtExp([mkLit(fn), ...args.map((a) => mkLit(a)), mkAp(args.length)]),
    ]
    const fiber = new Fiber(prog, env)
    // A callback runs on the student's behalf, so it inherits whatever depth
    // the program set for itself (#477) -- and only that. A run that never
    // asked leaves its callbacks on the current default, so a depth raised in
    // the preferences pane reaches them too (#497).
    if (run.fiber.hasOwnCallStackDepth) {
      fiber.setMaxCallStackDepth(run.fiber.maxCallStackDepth)
    }
    const id = crypto.randomUUID()
    this.scheduler.schedule({
      id,
      fiber,
      err,
      onComplete: () => onComplete?.(fiber.lastResult),
    })
  }

  /**
   * Collects the reduction trace of the single statement `cursorLoc` sits in.
   *
   * The whole program runs -- the traced statement usually leans on what the
   * ones before it defined -- but only its own reductions are kept. The trace
   * is gathered in full rather than streamed because what reads it offers a
   * "step 12 of 35" and a slider, and neither is answerable until the run ends.
   *
   * @param maxSteps how many reductions any one statement may take before the
   *        rest are abandoned, so a statement that loops forever cannot hang
   *        the page. Defaults to {@link DEFAULT_TRACE_STEP_LIMIT}, so a caller
   *        with no opinion does not have to invent a number.
   * @returns the statement's source and its steps, or null when the cursor is
   *          not inside a statement or the program did not compile.
   */
  public async traceStatement({
    src,
    cursorLoc,
    err,
    maxSteps = DEFAULT_TRACE_STEP_LIMIT,
  }: {
    src: string
    cursorLoc: Loc
    err: ErrorChannel
    maxSteps?: number
  }): Promise<{ source: string; steps: Value[]; truncated: boolean } | null> {
    const { prog, diagnostics } = await compile(src)
    diagnostics.forEach((d) => {
      err.report(diagnosticToError(d))
    })
    if (prog === undefined) return null

    const target = prog.findIndex(
      (stmt) =>
        stmt.range.begin.idx >= 0 && stmt.range.contains(cursorLoc),
    )
    if (target === -1) return null
    const { begin, end } = prog[target].range
    const source = src.slice(begin.idx, end.idx + 1).trim()

    return {
      source,
      ...(await this.collectTrace({
        prog,
        target,
        src,
        env: getDefaultEnv(),
        maxSteps,
      })),
    }
  }

  /**
   * Runs `prog` from `env`, keeping only statement `target`'s reductions.
   *
   * The shared engine behind {@link traceStatement} and
   * {@link traceReplEntry}; what separates the two is which program is run and
   * which environment it starts from.
   *
   * @param src the program's text, which the scheduler needs in order to
   *        announce each statement -- without it `beginStatement` never fires
   *        and the collector keeps nothing.
   * @param maxSteps reductions one statement may take before the rest are
   *        abandoned, so a statement that loops forever cannot hang the page.
   */
  private async collectTrace({
    prog,
    target,
    src,
    env,
    maxSteps,
  }: {
    prog: Prog
    target: number
    src: string
    env: Env
    maxSteps: number
  }): Promise<{ steps: Value[]; truncated: boolean }> {
    const id = crypto.randomUUID()
    const { promise, resolve } = deferred()
    const collector = new TraceCollector(target, maxSteps, () => {
      // Deferred out of the send that tripped it: this runs from inside the
      // scheduler's own loop, and cancelling there would splice the task list
      // out from under the iteration. A microtask lands between steps instead.
      //
      // `resolve` here rather than relying on onComplete, because a cancelled
      // task never reaches it -- cancelTask reports and unschedules, and that
      // is the end of it.
      queueMicrotask(() => {
        this.scheduler.cancelTask(id)
        resolve()
      })
    })
    const fiber = new Fiber(prog, env)
    // Deliberately not registered as a run: a trace is a side run, and
    // adopting it would point spawned event handlers at it and leave the
    // editor's actual program -- or the REPL session being stepped -- behind.
    this.scheduler.schedule({
      id,
      fiber,
      out: collector,
      err: collector,
      src,
      isTracing: true,
      stepper: makeTraceStepper(),
      onComplete: () => {
        resolve()
      },
      onFatal: () => {
        resolve()
      },
    })
    await promise
    return {
      steps: collector.steps,
      truncated: collector.truncated,
    }
  }

  /**
   * Collects the reduction trace of one REPL entry, by replaying it in the
   * environment it originally ran in (#424).
   *
   * The REPL's counterpart to {@link traceStatement}, and cheaper than it: an
   * entry is one statement, and `env` -- captured before the entry ran, which
   * costs one reference because a top level is persistent -- already holds
   * everything the file and the earlier entries defined. So nothing has to be
   * re-run to reach it, and a name redefined since is not the one the trace
   * sees.
   *
   * The entry does run a second time, which is what stepping in the editor
   * does too. Its output is collected rather than shown, but its effects are
   * real: a `(random ...)` may not agree with the transcript, and an entry
   * that mutates -- `vector-set!`, `hash-set!` -- applies that mutation again
   * to state the session still shares.
   *
   * @returns the entry's source and its steps, or null when it does not
   *          compile or holds no statement to step (a comment, say).
   */
  public async traceReplEntry({
    src,
    env,
    err,
    maxSteps = DEFAULT_TRACE_STEP_LIMIT,
  }: {
    src: string
    env: Env
    err: ErrorChannel
    maxSteps?: number
  }): Promise<{ source: string; steps: Value[]; truncated: boolean } | null> {
    const { prog, diagnostics } = await compile(src)
    diagnostics.forEach((d) => {
      err.report(diagnosticToError(d))
    })
    if (prog === undefined || prog.length === 0) return null

    return {
      source: src.trim(),
      ...(await this.collectTrace({
        prog,
        target: 0,
        src,
        env,
        maxSteps,
      })),
    }
  }

  /**
   * Compiles and runs `src` as this Scamper instance's main program.
   *
   * @returns the scheduled run, or null when a fatal parse error left no
   *          program (its diagnostics go to `err`). A program with no
   *          statements is a no-op, and comes back already finished.
   */
  public async execute({
    src,
    out,
    err,
    isTracing,
    stepping,
  }: DisplayExecutionConfig): Promise<DisplayRequest | null> {
    // compile src to lpm bytecode
    const { prog, diagnostics } = await compile(src)
    diagnostics.forEach((d) => {
      err.report(diagnosticToError(d))
    })
    if (prog === undefined) {
      // a fatal parse error left no program; diagnostics reported above
      return null
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(prog, getDefaultEnv())
    // Supersede the previous foreground run: abort its background handlers
    // (timers, DOM listeners, animation loops) so they don't leak into this
    // one. Embedded runs are left alone -- they are other programs on the page,
    // not earlier versions of this one (#375).
    if (this.mainRunId !== undefined) this.endRun(this.mainRunId)

    // schedule task
    // note: crypto is only available on HTTPS/localhost.
    // should never be a problem but just noting for future
    const id = crypto.randomUUID()
    this.mainRunId = id
    this.beginRun(id, fiber, err)
    const isStepping = stepping ?? false
    // Stepping implies tracing (each step renders a reduction); a stepper is
    // needed by any traced run.
    const traced = (isTracing ?? false) || isStepping
    // A program with no statements is born done, and `schedule` rejects a
    // completed fiber (#366). Running nothing is a legitimate no-op, so report
    // the run as already over rather than relaxing the scheduler's invariant.
    // Mirrors the same guard in runFiberOnScheduler.
    if (fiber.isDone()) {
      this.endRun(id)
      return { id, tracing: traced, done: Promise.resolve() }
    }
    const { promise, resolve } = deferred()
    this.scheduler.schedule({
      id,
      fiber,
      out,
      err,
      // Statements carry ranges into this text, so the scheduler needs it to
      // caption output with the statement that produced it.
      src,
      isTracing: traced,
      stepping: isStepping,
      stepper: traced ? makeTraceStepper() : undefined,
      onComplete: () => {
        resolve()
      },
      // An ICE or other non-Scamper failure would otherwise escape the
      // scheduler's detached loop as an unhandled rejection: the run would stop
      // dead with nothing shown and `done` never settling. Surface it on the
      // error channel and settle normally -- callers treat `done` as "the run
      // is over", and rejecting it would just relocate the unhandled rejection.
      onFatal: (e: unknown) => {
        err.report(
          new ScamperError(
            'Runtime',
            e instanceof Error ? e.toString() : String(e),
          ),
        )
        resolve()
      },
    })
    return { id, tracing: traced, done: promise }
  }

  /**
   * Runs one `@example` checking program (issue #374) as a side run.
   *
   * Scheduled with an error channel and no output channel, which makes it a
   * report task: the `(##report## ...)` the program ends with stops the fiber
   * and lands on the channel, and the student's own `display`s go nowhere.
   *
   * Deliberately not registered as a run, as in
   * {@link traceStatement}: a check is a side run, and adopting it would point
   * spawned event handlers at it and leave the editor's own program behind.
   *
   * @returns the run's id, so a caller that gets bored can {@link cancel} it,
   *          and what it reported once it is over -- which, for a cancelled
   *          run, is never.
   */
  public checkExample(prog: Prog): {
    id: SchedulerId
    done: Promise<readonly ScamperError[]>
  } {
    const err = new SimpleErrorChannel()
    const id = crypto.randomUUID()
    const fiber = new Fiber(prog, getDefaultEnv())
    // A completed fiber is rejected by `schedule` (#366); nothing to report.
    if (fiber.isDone()) {
      return { id, done: Promise.resolve(err.errors) }
    }
    const { promise, resolve } = deferred()
    this.scheduler.schedule({
      id,
      fiber,
      err,
      onComplete: () => {
        resolve()
      },
      // As in execute(): surface the failure and settle, rather than leaving
      // the caller waiting on a run that has already died.
      onFatal: (e: unknown) => {
        err.report(
          new ScamperError(
            'Runtime',
            e instanceof Error ? e.toString() : String(e),
          ),
        )
        resolve()
      },
    })
    return { id, done: promise.then(() => err.errors) }
  }

  /**
   * Runs `src` on the shared scheduler as a program of its own (#375).
   *
   * Unlike {@link execute}, this does not supersede: it leaves the foreground
   * run alone and registers a run of its own, so a page holding many programs
   * -- the reading widgets -- does not have each one tear down the one before
   * it. Callbacks a widget registers fire against its own environment and
   * report to its own output.
   *
   * @param env seeds the top level, so one widget's program can continue
   *        another's. Omitted means the standard library alone.
   * @returns the scheduled run, or null when a fatal parse error left no
   *          program (its diagnostics go to `err`).
   */
  public async executeEmbedded({
    src,
    out,
    err,
    env,
  }: EmbeddedExecutionConfig): Promise<EmbeddedRequest | null> {
    const { prog, diagnostics } = await compile(src)
    diagnostics.forEach((d) => {
      err.report(diagnosticToError(d))
    })
    if (prog === undefined) {
      return null
    }

    const fiber = new Fiber(prog, env ?? getDefaultEnv())
    // A counter rather than crypto.randomUUID(), which needs a secure context:
    // a reading served over plain http:// still has to run. Same reasoning as
    // runFiberOnScheduler.
    const id = `embed-${(nextEmbedId++).toString()}`
    this.beginRun(id, fiber, err)

    // A program with no statements is born done, and `schedule` rejects a
    // completed fiber (#366). Its environment is still the one to hand on.
    if (fiber.isDone()) {
      return { id, done: Promise.resolve(fiber.topLevelEnv) }
    }

    const { promise, resolve } = deferred()
    this.scheduler.schedule({
      id,
      fiber,
      out,
      err,
      // Statements carry ranges into this text, so the scheduler needs it to
      // caption output with the statement that produced it -- which is the
      // whole point of a transcript widget.
      src,
      isTracing: false,
      onComplete: () => {
        resolve()
      },
      // As in execute: settle normally so a widget that dies of an ICE still
      // hands its environment on rather than stranding the widgets after it.
      onFatal: (e: unknown) => {
        err.report(
          new ScamperError(
            'Runtime',
            e instanceof Error ? e.toString() : String(e),
          ),
        )
        resolve()
      },
    })
    // The run is NOT ended here: a widget's handlers -- an animation loop, a
    // button -- have to keep working after its program finishes, exactly as the
    // IDE's do. It ends when the page does, or when `cancel` is called.
    return { id, done: promise.then(() => fiber.topLevelEnv) }
  }

  /**
   * Opens a REPL: a program built up an entry at a time, in an environment that
   * carries forward from one to the next (#399).
   *
   * The session *is* a run, registered like any other, so a handler an entry
   * registers reaches the right program and dies with it. Its fiber is replaced
   * per entry and holds the environment between them -- the top level a REPL
   * builds up is exactly the top level of whatever it last evaluated.
   *
   * Deliberately independent of the foreground run: a REPL is scratch work
   * beside the file rather than a version of it, so pressing Run does not
   * disturb it and it does not supersede what Run started.
   */
  public startRepl({
    out,
    err,
  }: {
    out: OutputChannel
    err: ErrorChannel
  }): ReplSession {
    const id = crypto.randomUUID()
    // An empty program: born done, so it never runs, and its only job is to
    // hold the environment until the first entry replaces it. Without it the
    // run would have no fiber, and a handler registered by a library call
    // during seeding would have no top level to resolve against.
    const run = this.beginRun(id, new Fiber([], getDefaultEnv()), err)

    // The entry in flight, so interrupting one can settle the promise its
    // caller is waiting on: a cancelled task never reaches `onComplete`.
    let settle: (() => void) | null = null
    // True once the session has ended.
    //
    // Checked after every await: compiling is asynchronous, so a session can be
    // ended while an entry is still on its way to running. Without this the
    // program would be scheduled on a run that has already been torn down --
    // running work that was abandoned, and leaving its caller waiting on a
    // promise that `end` had already been past to settle.
    let ended = false

    /**
     * Runs `prog` as the session's next fiber, in the environment the last one
     * left.
     */
    const runProgram = async (
      prog: Prog,
      out: OutputChannel,
    ): Promise<void> => {
      if (ended) return
      const fiber = new Fiber(prog, run.fiber.topLevelEnv)
      // As with the environment, a depth an earlier entry *asked* for carries
      // into this one -- a REPL session is one continuous program to its user
      // (#477). One it merely started at does not, so raising the preference
      // reaches a REPL that is already open, which is where a student who has
      // just hit the limit is sitting (#497).
      if (run.fiber.hasOwnCallStackDepth) {
        fiber.setMaxCallStackDepth(run.fiber.maxCallStackDepth)
      }
      // Before the run, not after: a handler registered *by this entry* has to
      // see this fiber's top level, and so does one registered by an earlier
      // entry that fires while this one is running.
      run.fiber = fiber
      // A program of no statements -- a blank entry, a comment -- is born done,
      // and `schedule` rejects a completed fiber (#366).
      if (fiber.isDone()) return
      const { promise, resolve } = deferred()
      settle = resolve
      this.scheduler.schedule({
        id,
        fiber,
        out,
        err,
        // Present and false rather than absent: a task is a display task only
        // if it carries both `out` and `isTracing`, and a report task's output
        // goes nowhere. A REPL does not trace -- an entry shows its value, not
        // the reductions that reached it, which is what Step is for.
        isTracing: false,
        // No `src`: an entry is shown above its own output by the REPL itself,
        // and a caption would print it a second time.
        onComplete: () => {
          resolve()
        },
        // As in execute(): surface it and settle, rather than leaving the REPL
        // waiting forever on an entry that has already died.
        onFatal: (e: unknown) => {
          err.report(
            new ScamperError(
              'Runtime',
              e instanceof Error ? e.toString() : String(e),
            ),
          )
          resolve()
        },
      })
      await promise
      settle = null
    }

    return {
      id,
      // A getter, not a value: the session's top level is replaced by every
      // define, and what a caller wants is the one in force when it asks.
      get env() {
        return run.fiber.topLevelEnv
      },
      seed: async (src: string) => {
        const { prog, diagnostics } = await compile(src)
        if (ended) return false
        diagnostics.forEach((d) => {
          err.report(diagnosticToError(d))
        })
        if (prog === undefined) return false
        // Into a channel that drops what it is given, so the file's own output
        // goes nowhere. Errors still land: a file that fails half way through
        // leaves a half-built environment, and saying so beats the definitions
        // after the failure quietly not being there.
        await runProgram(prog, new DiscardOutput())
        return true
      },
      evaluate: async (src: string) => {
        if (ended) return false
        // An entry is one statement. Anything else is refused rather than
        // half-run: a file pasted into the prompt is a mistake worth naming,
        // and an entry that is only a comment would otherwise run an empty
        // program and print nothing, which reads as the REPL ignoring it.
        const statements = countStatements(src)
        if (statements !== undefined && statements !== 1) {
          err.report(
            new ScamperError(
              'Parser',
              statements === 0
                ? 'A REPL entry is one statement, and there is none here to run.'
                : `A REPL entry is one statement at a time, and this is ${statements.toString()}. ` +
                  'Enter them one by one.',
            ),
          )
          return false
        }
        const { prog, diagnostics } = await compile(src)
        // `end()` can run while this awaits, which TypeScript's narrowing does
        // not follow across the boundary (#154).
        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        if (ended) return false
        diagnostics.forEach((d) => {
          err.report(diagnosticToError(d))
        })
        if (prog === undefined) return false
        await runProgram(prog, out)
        return true
      },
      interrupt: () => {
        this.scheduler.cancelTask(id)
        // cancelTask reports and unschedules, and that is the end of it: the
        // task never reaches onComplete, so the entry's promise is settled
        // here or not at all.
        settle?.()
        settle = null
      },
      end: () => {
        ended = true
        this.cancel(id)
        settle?.()
        settle = null
      },
    }
  }

  /*  =====  scheduler  =====  */
  public cancel(id: SchedulerId) {
    // Stopping a run also tears down its background handlers -- whichever run
    // it is, since an embedded one has handlers of its own (#375).
    this.endRun(id)
    this.scheduler.cancelTask(id)
  }

  /* =====  step mode  ===== */

  /** Advance a paused step-mode run by one user-visible reduction. */
  public step(id: SchedulerId): void {
    this.scheduler.step(id)
  }

  /** Resume a paused step-mode run to the next statement boundary or to
   * completion; resolves when it next pauses or finishes. */
  public resume(id: SchedulerId, mode: StepMode): Promise<void> {
    return this.scheduler.resume(id, mode)
  }

  /** Stop an in-flight statement/all burst, re-pausing at the next reduction. */
  public pauseStepping(id: SchedulerId): void {
    this.scheduler.pauseStepping(id)
  }

  public calibrateScheduler(): void {
    void this.scheduler.setTimeQuantumFromFPS()
  }

  /*  =====  querying  =====  */
  get queryEvents(): EventTarget {
    return this.queryBus
  }

  get queries(): QueryMap {
    return new Map(
      [...this._queries].map(
        ([line, bucket]) => [line, bucket.slice()] as const,
      ),
    )
  }

  get expandedQueryId(): SchedulerId | null {
    return this._expandedQueryId
  }

  private updateQueries(mutate: (queries: Map<number, QueryEntry[]>) => void): void {
    mutate(this._queries)
    this.queryBus.dispatchEvent(new Event(QUERIES_CHANGED))
  }

  private setExpandedQueryId(id: SchedulerId | null): void {
    if (this._expandedQueryId === id) return
    this._expandedQueryId = id
    this.queryBus.dispatchEvent(new Event(QUERY_EXPANDED_CHANGED))
  }

  public async query({
    src,
    err,
    queryLoc,
  }: QueryExecutionConfig): Promise<void> {
    const { prog, queriedRange, diagnostics } = await compile(src, { queryLoc })
    diagnostics.forEach((d) => {
      err.report(diagnosticToError(d))
    })
    if (prog === undefined || queriedRange === undefined) {
      // diagnostics reported above
      return
    }

    if (
      this._queries
        .get(queriedRange.begin.line)
        ?.some((q) => rangesEqual(q.queriedRange, queriedRange))
    ) {
      console.warn('attempted duplicate query')
      return
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(prog, getDefaultEnv())

    // schedule query task
    const id = crypto.randomUUID()
    const { promise, resolve } = deferred()
    this.scheduler.schedule({
      id,
      fiber,
      err,
      onComplete: () => {
        resolve()
      },
      // As in execute(): surface the failure and settle, rather than leaving
      // `done` pending forever on a run that has already died.
      onFatal: (e: unknown) => {
        err.report(
          new ScamperError(
            'Runtime',
            e instanceof Error ? e.toString() : String(e),
          ),
        )
        resolve()
      },
    })
    const entry: QueryEntry = {
      id,
      err,
      done: promise,
      queriedRange,
    }
    this.registerQueryEntry(entry)
  }

  public invalidateAllQueries() {
    for (const bucket of this._queries.values()) {
      for (const q of bucket) {
        this.cancel(q.id)
      }
    }
    this.setExpandedQueryId(null)
    this.updateQueries((queries) => {
      queries.clear()
    })
  }

  public invalidateQuery(id: SchedulerId) {
    this.cancel(id)
    if (this._expandedQueryId === id) {
      this.setExpandedQueryId(null)
    }
    this.updateQueries((queries) => {
      for (const [line, bucket] of queries) {
        const i = bucket.findIndex((q) => q.id === id)
        if (i === -1) continue
        bucket.splice(i, 1)
        if (bucket.length === 0) {
          queries.delete(line)
        }
        return
      }
    })
  }

  public expandQuery(id: SchedulerId) {
    this.setExpandedQueryId(id)
  }

  public collapseQuery() {
    this.setExpandedQueryId(null)
  }

  public toggleQueryExpanded(id: SchedulerId) {
    if (this._expandedQueryId === id) {
      this.collapseQuery()
    } else {
      this.expandQuery(id)
    }
  }

  public getQuery(id: SchedulerId) {
    for (const bucket of this._queries.values()) {
      const query = bucket.find((q) => q.id === id)
      if (query) {
        return query
      }
    }
  }

  /** Adds a query entry to the line bucket and notifies listeners. */
  registerQueryEntry(entry: QueryEntry): void {
    this.updateQueries((queries) => {
      const line = entry.queriedRange.begin.line
      const bucket = queries.get(line)
      if (bucket) {
        bucket.push(entry)
        bucket.sort(
          (a, b) => a.queriedRange.begin.col - b.queriedRange.begin.col,
        )
      } else {
        queries.set(line, [entry])
      }
    })
  }
}

function deferred(): { promise: Promise<void>; resolve: () => void } {
  let resolve!: () => void
  const promise = new Promise<void>((r) => {
    resolve = r
  })
  return { promise, resolve }
}
