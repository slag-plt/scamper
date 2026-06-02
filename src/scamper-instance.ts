// TODO: will eventually replace scamper.ts and scamper-vue.ts
import builtinLibs from "./lib"
import { ErrorChannel, Library, OutputChannel, ScamperError } from "./lpm"
import { Fiber } from "./lpm/fiber"
import { compile } from "./scheme"

interface ExecutionConfig {
  src: string;
  out: OutputChannel;
  err: ErrorChannel;
  isTracing?: boolean; // whether to enable tracing of execution steps
}

export class ScamperInstance {
  // singleton structure
  static #instance: ScamperInstance | null
  public static getInstance(): ScamperInstance {
    ScamperInstance.#instance ??= new ScamperInstance()
    return ScamperInstance.#instance
  }

  // we will lazy load all libraries
  #libs: Map<string, Library>
  private constructor() {
    this.#libs = new Map()
  }

  public async getLib(name: string): Promise<Library> {
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

  public async execute({src, out, err, isTracing}: ExecutionConfig): Promise<void> {
    // compile src to lpm bytecode
    const compiled = compile(err, src)
    if (!compiled) {
      throw new ScamperError("Runtime", `Failed to compile source code: ${err.errors.join("\n")}`)
    }

    // make new fiber with prelude as initial environment
    const fiber = new Fiber(compiled)
    await fiber.loadLib("prelude")
    
    // execute fiber until it's done
    while (!fiber.isDone) {
      try {
        // skip minor steps
        if (!await fiber.step()) continue
        
        // there are two types of major steps:
        // 1. steps we always want to output, and
        // 2. steps that we only want to output when tracing.
        // ideally, we always output the final result of executing a top-level statement.
        // so, we address the first type of major step by checking if the fiber is currently processing a top-level statement (i.e. not in the middle of processing a Blk statement).
        if (!fiber.isProcessingBlk) {
          out.send(fiber.lastResult)  
        }
        // for the tracing kind of major step, we will reraise the state of the fiber back to an expression.
        else if (isTracing) {
          // TODO: something like out.send(mkTraceOutput(provider.raise(fiber)))
        }
      } catch (e) {
        if (e instanceof ScamperError) {
          err.report(e)
          return
        }
        err.report(new ScamperError("Runtime", `Unexpected error: ${e instanceof Error ? e.message : String(e)}`))
      }
    }
  }
}
