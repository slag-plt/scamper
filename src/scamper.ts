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

export class ScamperInstance {
  // singleton structure
  static #instance?: ScamperInstance
  #scheduler: Scheduler

  static getInstance(): ScamperInstance {
    ScamperInstance.#instance ??= new ScamperInstance()
    return ScamperInstance.#instance
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
