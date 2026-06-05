// TODO: will eventually replace scamper.ts and scamper-vue.ts
import builtinLibs from "./lib"
import { ErrorChannel, Module, OutputChannel, ScamperError } from "./lpm"
import { Fiber } from "./lpm/fiber"
import { mkTraceOutput } from "./lpm/trace"
import { compile, fiberRaiser } from "./scheme"

interface ExecutionConfig {
  src: string
  out: OutputChannel
  err: ErrorChannel
  isTracing?: boolean // whether to enable tracing of execution steps
}

export class ScamperInstance {
  // singleton structure
  static #instance: ScamperInstance | null
  public static get instance(): ScamperInstance {
    ScamperInstance.#instance ??= new ScamperInstance()
    return ScamperInstance.#instance
  }

  // we will lazy load all libraries
  #libs: Map<string, Module>
  private constructor() {
    this.#libs = new Map()
  }

  public async getLib(name: string): Promise<Module> {
    const cached = this.#libs.get(name)
    if (cached) {
      return cached
    }

    // TODO: should support user-defined libraries in the future, but for now we will only support built-in libraries
    const lib = builtinLibs.get(name)
    if (!lib) {
      throw new ScamperError("Runtime", `Library ${name} not found`)
    }

    await lib.initializer?.()
    // memoize the library so that we don't have to initialize it again
    this.#libs.set(name, lib)
    return lib
  }

  public async execute({
    src,
    out,
    err,
    isTracing,
  }: ExecutionConfig): Promise<void> {
    // compile src to lpm bytecode
    const compiled = compile(err, src)
    if (!compiled) {
      return
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled)
    // TODO: we can't load prelude yet until the rewrite of the library as a module
    // await fiber.loadLib("prelude")

    // TODO: do actual scheduling
    // this should just push to a queue and our scamper init? will take care of it
    void fiber
    void out
    void isTracing
  }
}
