// TODO: will eventually replace scamper.ts and scamper-vue.ts
import builtinLibs from "./lib"
import { ErrorChannel, Library, Loc, OutputChannel, ScamperError } from "./lpm"
import { Fiber } from "./lpm/fiber"
import { Scheduler, SchedulerId } from "./scheduler"
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

export class ScamperInstance {
  // singleton structure
  static #instance?: ScamperInstance
  // we will lazy load all libraries
  #libs: Map<string, Library>
  #scheduler: Scheduler

  static getInstance(): ScamperInstance {
    ScamperInstance.#instance ??= new ScamperInstance()
    return ScamperInstance.#instance
  }
  private constructor() {
    this.#libs = new Map()
    this.#scheduler = new Scheduler()
  }

  public tryGetLib(name: string): Library | undefined {
    const cached = this.#libs.get(name)
    if (cached) {
      return cached
    }

    // TODO: should support user-defined libraries in the future, but for now we will only support built-in libraries
    const lib = builtinLibs.get(name)
    if (!lib) {
      throw new ScamperError("Runtime", `Library ${name} not found`)
    }

    // start loading the library in the background
    void (async () => {
      await lib.initializer?.()
      // memoize the library so that we don't have to initialize it again
      this.#libs.set(name, lib)
    })()
    return undefined
  }

  public execute({
    src,
    out,
    err,
    isTracing,
  }: DisplayExecutionConfig): DisplayRequest | null {
    // compile src to lpm bytecode
    const compiled = compile(err, src)
    if (!compiled) {
      // err channel should have caught the error
      return null
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled)
    // TODO: we can't load prelude yet until the rewrite of the library as a module
    // await fiber.loadLib("prelude")

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
  public query({
    src,
    err,
    queryLoc,
  }: QueryExecutionConfig): QueryRequest | null {
    // compile src to lpm bytecode
    const compiled = compile(err, src, queryLoc)
    if (!compiled) {
      // report channel should have caught the error
      return null
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled)
    // TODO: we can't load prelude yet until the rewrite of the library as a module
    // await fiber.loadLib("prelude")

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
