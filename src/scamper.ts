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
  setRunSignalProvider,
  setSpawn,
  Value,
} from './lpm'
import { Fiber } from './lpm/fiber'
import { TraceCollector } from './lpm/output/trace-collector'
import { Scheduler, SchedulerId, StepMode } from './lpm/scheduler'
import { compile } from './scheme'
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
  defaultEnv = Env.empty
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    .extendWithImport('runtime', builtinLibs.get('runtime')!)
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    .extendWithImport('prelude', builtinLibs.get('prelude')!)
  initialized = true
}

// Kicks off web/renderers.ts's custom Vue/HTML renderer registration as
// early as possible (browser-only; see its header comment for why this must
// be a guarded dynamic import). Deliberately independent of initialize()
// above (and fire-and-forget): execute()/query() must NOT await this before
// scheduling. Doing so once delayed task-id generation past the window
// stopRun()'s cancel-by-id logic (see use-scamper-session.ts) needs, so a
// pending run could dodge cancellation and a second run would duplicate its
// output instead of replacing it. Tradeoff: a value displayed before this
// resolves can render via the generic fallback instead of its custom
// renderer -- in practice only a risk for the very first values shown after
// a fresh page load. Kept as a plain module-load-time side effect (as
// opposed to living inside initialize()) so it only ever fires as a
// consequence of *something* importing scamper.ts -- e.g. a test file's own
// import graph -- rather than from a shared global call site (like a test
// suite's global setup) that runs before that particular test file's own
// module mocks (`vi.mock(...)`) have been registered; running it from there
// grabs real (unmocked) transitive dependencies -- notably src/fs/opfs.ts --
// out from under tests that mock them.
if (typeof window !== 'undefined') {
  void import('./app/web/renderers.js')
}

/** Unreachable once getInstance() has gated on `initialized`. */
function getDefaultEnv(): Env {
  if (!defaultEnv) {
    throw new Error("Scamper's default environment used before initialize()")
  }
  return defaultEnv
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
  // The most recent top-level program's fiber and error channel, so spawned
  // event-handler fibers (see spawnClosure) run in the user's environment and
  // report errors to the same place.
  private mainFiber?: Fiber
  private mainErr?: ErrorChannel
  // Aborted when the current program is re-run or stopped, so the previous run's
  // background handlers (DOM listeners, timers, animation loops) tear themselves
  // down instead of leaking into the next run (see currentRunSignal).
  private currentRunController?: AbortController
  private currentRunId?: SchedulerId

  private constructor() {
    this.scheduler = new Scheduler()
    // Let library event handlers run Scamper closures as fibers (spawn()) and
    // find the current run's AbortSignal, without importing this singleton.
    setSpawn((fn, args, onComplete) => this.spawnClosure(fn, args, onComplete))
    setRunSignalProvider(() => this.currentRunController?.signal)
  }

  /**
   * Runs the closure `fn` applied to `args` as a fresh fiber in the current
   * program's top-level environment (so it sees the user's definitions and
   * imports). Used by event/callback library functions -- a DOM/timer callback
   * fires with no fiber running, so it must originate a new one. `onComplete`
   * receives the closure's result (or null).
   */
  private spawnClosure(
    fn: Value,
    args: Value[],
    onComplete?: (result: Value | null) => void,
  ): void {
    const env = this.mainFiber?.topLevelEnv
    const err = this.mainErr
    if (env === undefined || err === undefined) {
      // No active program to run the callback in; drop it.
      return
    }
    const prog: Prog = [
      mkStmtExp([mkLit(fn), ...args.map((a) => mkLit(a)), mkAp(args.length)]),
    ]
    const fiber = new Fiber(prog, env)
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
   * @param maxSteps how many reductions to keep before abandoning the rest, so
   *        a statement that loops forever cannot hang the page.
   * @returns the statement's source and its steps, or null when the cursor is
   *          not inside a statement or the program did not compile.
   */
  public async traceStatement({
    src,
    cursorLoc,
    err,
    maxSteps,
  }: {
    src: string
    cursorLoc: Loc
    err: ErrorChannel
    maxSteps: number
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
    const fiber = new Fiber(prog, getDefaultEnv())
    // Deliberately not this.mainFiber/mainErr: a trace is a side run, and
    // adopting it would point spawned event handlers at it and leave the
    // editor's actual program behind.
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
      source,
      steps: collector.steps,
      truncated: collector.truncated,
    }
  }

  /**
   * @returns ID of task
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
    // Remember this run's fiber/error channel so spawned event-handler fibers
    // (spawnClosure) run in its evolving top-level env and report to the same
    // error channel.
    this.mainFiber = fiber
    this.mainErr = err
    // Supersede the previous run: abort its background handlers (timers, DOM
    // listeners, animation loops) so they don't leak into this run.
    this.currentRunController?.abort()
    this.currentRunController = new AbortController()

    // schedule task
    // note: crypto is only available on HTTPS/localhost.
    // should never be a problem but just noting for future
    const id = crypto.randomUUID()
    this.currentRunId = id
    const isStepping = stepping ?? false
    // Stepping implies tracing (each step renders a reduction); a stepper is
    // needed by any traced run.
    const traced = (isTracing ?? false) || isStepping
    // A program with no statements is born done, and `schedule` rejects a
    // completed fiber (#366). Running nothing is a legitimate no-op, so report
    // the run as already over rather than relaxing the scheduler's invariant.
    // Mirrors the same guard in runFiberOnScheduler.
    if (fiber.isDone()) {
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

  /*  =====  scheduler  =====  */
  public cancel(id: SchedulerId) {
    // Stopping the main run also tears down its background handlers.
    if (id === this.currentRunId) {
      this.currentRunController?.abort()
    }
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
