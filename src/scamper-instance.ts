// TODO: will eventually replace scamper.ts and scamper-vue.ts

import builtinLibs, { Runtime } from "./lib"
import { Library, OutputChannel, ScamperError } from "./lpm"

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

  public async execute(src: string, out: OutputChannel): Promise<void> {
    // TODO: to implement!
    void src
    void out
    await new Promise(() => null)
  }
}
