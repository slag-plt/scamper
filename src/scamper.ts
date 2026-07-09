// TODO: will eventually replace scamper.ts and scamper-vue.ts
import builtinLibs from "./lib"
import { Env, ErrorChannel, Loc, OutputChannel } from "./lpm"
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

const defaultEnv =
  Env.empty.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('runtime', builtinLibs.get('runtime')!).
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('prelude', builtinLibs.get('prelude')!)

// N.B., web/renderers.ts registers the Vue/HTML custom renderers for the
// builtin libraries' value types. It transitively imports Vue single-file
// components, which the CLI's plain Node runtime can't load -- so this must
// stay a *dynamic* import, guarded to only ever run in a browser.
//
// Deliberately fire-and-forget: Scamper.execute()/query() must NOT await
// this before scheduling. Doing so once gated task-id generation on it,
// which stopRun()'s cancel-by-id logic (see use-scamper-session.ts) depends
// on happening promptly -- a cold dynamic import of ~20 modules, including
// several Vue SFCs, can take longer than the window between two "Run"
// clicks, so a run that hadn't finished awaiting it yet was neither
// registered nor cancellable, and a second run scheduled independently
// instead of replacing it, duplicating output. The tradeoff: a value
// displayed before this resolves can render via the generic fallback
// instead of its custom renderer, since VueRenderer's registry mutation
// isn't reactive -- that specific display won't upgrade itself in place. In
// practice this only risks the very first values of a freshly loaded page,
// since the import starts as soon as this module does.
if (typeof window !== "undefined") {
  void import("./web/renderers.js")
}

export default class Scamper {
  // singleton structure
  static #instance?: Scamper
  #scheduler: Scheduler

  static getInstance(): Scamper {
    Scamper.#instance ??= new Scamper()
    return Scamper.#instance
  }
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
    const fiber = new Fiber(compiled, defaultEnv)

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
  public async query({
    src,
    err,
    queryLoc,
  }: QueryExecutionConfig): Promise<QueryRequest | null> {
    // compile src to lpm bytecode
    const compiled = await compile(err, src, queryLoc)
    if (!compiled) {
      // report channel should have caught the error
      return null
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled, defaultEnv)

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
    return { id, done: promise }
  }
  public cancel(id: SchedulerId) {
    this.#scheduler.cancelTask(id)
  }

  public calibrateScheduler(): void {
    void this.#scheduler.setTimeQuantumFromFPS()
  }
}

function deferred(): { promise: Promise<void>; resolve: () => void } {
  let resolve!: () => void
  const promise = new Promise<void>((r) => {
    resolve = r
  })
  return { promise, resolve }
}
