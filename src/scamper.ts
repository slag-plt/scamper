// TODO: will eventually replace scamper.ts and scamper-vue.ts
import { builtinLibs, initializeLibs } from "./lib"
import {
  Env,
  ErrorChannel,
  Loc,
  OutputChannel,
  Range,
  rangesEqual,
} from "./lpm"
import { Fiber } from "./lpm/fiber"
import { Scheduler, SchedulerId } from "./lpm/scheduler"
import { compile } from "./scheme"

interface ExecutionConfig {
  src: string
}

interface DisplayExecutionConfig extends ExecutionConfig {
  out: OutputChannel
  err: ErrorChannel
  isTracing?: boolean // whether to enable tracing of execution steps
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

export const QUERIES_CHANGED = "scamper:querieschanged"
export const QUERY_EXPANDED_CHANGED = "scamper:queryexpandedchanged"

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
  defaultEnv = Env.empty
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    .extendWithImport("runtime", builtinLibs.get("runtime")!)
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    .extendWithImport("prelude", builtinLibs.get("prelude")!)
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
if (typeof window !== "undefined") {
  void import("./web/renderers.js")
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
  static #instance?: Scamper
  static getInstance(): Scamper {
    if (!initialized) {
      throw new Error(
        "Scamper.getInstance() called before initialize() completed",
      )
    }
    Scamper.#instance ??= new Scamper()
    return Scamper.#instance
  }

  /*  =====  instance-related fields  =====  */
  #scheduler: Scheduler
  #queries = new Map<number, QueryEntry[]>()
  #expandedQueryId: SchedulerId | null = null
  #queryBus = new EventTarget()

  private constructor() {
    this.#scheduler = new Scheduler()
  }

  /**
   * @returns ID of task
   */
  public async execute({
    src,
    out,
    err,
    isTracing,
  }: DisplayExecutionConfig): Promise<DisplayRequest | null> {
    // compile src to lpm bytecode
    const compiled = await compile(err, src)
    if (!compiled) {
      // err channel should have caught the error
      return null
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled, getDefaultEnv())

    // schedule task
    // note: crypto is only available on HTTPS/localhost.
    // should never be a problem but just noting for future
    const id = crypto.randomUUID()
    const tracing = isTracing ?? false
    const { promise, resolve } = deferred()
    this.#scheduler.schedule({
      id,
      fiber,
      out,
      err,
      isTracing: tracing,
      onComplete: () => {
        resolve()
      },
    })
    return { id, tracing, done: promise }
  }

  /*  =====  scheduler  =====  */
  public cancel(id: SchedulerId) {
    this.#scheduler.cancelTask(id)
  }
  public calibrateScheduler(): void {
    void this.#scheduler.setTimeQuantumFromFPS()
  }

  /*  =====  querying  =====  */
  get queryEvents(): EventTarget {
    return this.#queryBus
  }
  get queries(): QueryMap {
    return new Map(
      [...this.#queries].map(
        ([line, bucket]) => [line, bucket.slice()] as const,
      ),
    )
  }
  get expandedQueryId(): SchedulerId | null {
    return this.#expandedQueryId
  }
  #updateQueries(mutate: (queries: Map<number, QueryEntry[]>) => void): void {
    mutate(this.#queries)
    this.#queryBus.dispatchEvent(new Event(QUERIES_CHANGED))
  }
  #setExpandedQueryId(id: SchedulerId | null): void {
    if (this.#expandedQueryId === id) return
    this.#expandedQueryId = id
    this.#queryBus.dispatchEvent(new Event(QUERY_EXPANDED_CHANGED))
  }
  public async query({
    src,
    err,
    queryLoc,
  }: QueryExecutionConfig): Promise<void> {
    const compiled = await compile(err, src, queryLoc)
    if (!compiled) {
      // report channel should have caught the error
      return
    }

    const { prog, queriedRange } = compiled
    if (
      this.#queries
        .get(queriedRange.begin.line)
        ?.some((q) => rangesEqual(q.queriedRange, queriedRange))
    ) {
      console.warn("attempted duplicate query")
      return
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(prog, getDefaultEnv())

    // schedule query task
    const id = crypto.randomUUID()
    const { promise, resolve } = deferred()
    this.#scheduler.schedule({
      id,
      fiber,
      err,
      onComplete: () => {
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
    for (const bucket of this.#queries.values()) {
      for (const q of bucket) {
        this.cancel(q.id)
      }
    }
    this.#setExpandedQueryId(null)
    this.#updateQueries((queries) => {
      queries.clear()
    })
  }
  public invalidateQuery(id: SchedulerId) {
    this.cancel(id)
    if (this.#expandedQueryId === id) {
      this.#setExpandedQueryId(null)
    }
    this.#updateQueries((queries) => {
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
    this.#setExpandedQueryId(id)
  }
  public collapseQuery() {
    this.#setExpandedQueryId(null)
  }
  public toggleQueryExpanded(id: SchedulerId) {
    if (this.#expandedQueryId === id) {
      this.collapseQuery()
    } else {
      this.expandQuery(id)
    }
  }
  public getQuery(id: SchedulerId) {
    for (const bucket of this.#queries.values()) {
      const query = bucket.find((q) => q.id === id)
      if (query) {
        return query
      }
    }
  }

  /** Adds a query entry to the line bucket and notifies listeners. */
  registerQueryEntry(entry: QueryEntry): void {
    this.#updateQueries((queries) => {
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
