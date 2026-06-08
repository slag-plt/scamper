// TODO: will eventually replace scamper.ts and scamper-vue.ts
import builtinLibs from "./lib"
import { ErrorChannel, Module, OutputChannel, ScamperError } from "./lpm"
import { Fiber } from "./lpm/fiber"
import { Scheduler } from "./scheduler"
import { compile } from "./scheme"

interface ExecutionConfig {
  src: string
  out: OutputChannel
  err: ErrorChannel
  isTracing?: boolean // whether to enable tracing of execution steps
}

export class ScamperInstance {
  // singleton structure
  static #instance?: ScamperInstance
  // we will lazy load all libraries
  #libs: Map<string, Module>
  #scheduler: Scheduler

  static getInstance(): ScamperInstance {
    ScamperInstance.#instance ??= new ScamperInstance()
    return ScamperInstance.#instance
  }
  private constructor() {
    this.#libs = new Map()
    this.#scheduler = new Scheduler()
  }

  public tryGetLib(name: string): Module | undefined {
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

  public execute({ src, out, err, isTracing }: ExecutionConfig): void {
    // compile src to lpm bytecode
    const compiled = compile(err, src)
    if (!compiled) {
      // err channel should have caught the error
      return
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(out, err, isTracing ?? false, compiled)
    // TODO: we can't load prelude yet until the rewrite of the library as a module
    // await fiber.loadLib("prelude")

    // schedule task
    this.#scheduler.schedule({ fiber, out, err, isTracing: isTracing ?? false })
  }
}
