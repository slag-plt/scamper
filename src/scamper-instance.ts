// TODO: will eventually replace scamper.ts and scamper-vue.ts
import { ErrorChannel, Loc, OutputChannel } from "./lpm"
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
  public execute({
    src,
    out,
    err,
    isTracing,
  }: DisplayExecutionConfig): SchedulerId | null {
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
    this.#scheduler.schedule({
      id,
      fiber,
      out,
      err,
      isTracing: isTracing ?? false,
    })
    return id
  }
  public query({
    src,
    err,
    queryLoc,
  }: QueryExecutionConfig): SchedulerId | null {
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
    this.#scheduler.schedule({ id, fiber, err })
    return id
  }
  public cancel(id: SchedulerId) {
    this.#scheduler.cancelTask(id)
  }

  public calibrateScheduler(): void {
    void this.#scheduler.setTimeQuantumFromFPS()
  }
}
