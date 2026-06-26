// TODO: will eventually replace scamper.ts and scamper-vue.ts
import { ErrorChannel, Loc, OutputChannel } from "./lpm"
import { Fiber } from "./lpm/fiber"
import { Scheduler } from "./scheduler"
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
  rep: ErrorChannel
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

  public execute({ src, out, err, isTracing }: DisplayExecutionConfig): void {
    // compile src to lpm bytecode
    const compiled = compile(err, src)
    if (!compiled) {
      // err channel should have caught the error
      return
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled)
    // TODO: we can't load prelude yet until the rewrite of the library as a module
    // await fiber.loadLib("prelude")

    // schedule task
    this.#scheduler.schedule({ fiber, out, err, isTracing: isTracing ?? false })
  }
  public query({ src, rep, queryLoc }: QueryExecutionConfig): void {
    // compile src to lpm bytecode
    const compiled = compile(rep, src, queryLoc)
    if (!compiled) {
      // report channel should have caught the error
      return
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled)
    // TODO: we can't load prelude yet until the rewrite of the library as a module
    // await fiber.loadLib("prelude")

    // schedule query task
    this.#scheduler.query({ fiber, rep })
  }

  public calibrateScheduler(): void {
    void this.#scheduler.setTimeQuantumFromFPS()
  }
}
