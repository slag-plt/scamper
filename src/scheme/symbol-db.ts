import builtinLibs from '../lib'
import * as A from './ast'
import * as L from '../lpm/lang'


/** A collection of external symbols from importable modules. */
export default class SymbolDB {
  // TODO: I'm pretty sure we need to store more information for each identifier
  // but I'll defer those design decisions till later. This is enough to support
  // scope checking and the scope tree for now.
  private modules: Map<string, A.Identifier[]>

  /** Initializes this symbol table with entries from the standard library. */
  constructor () {
    this.modules = new Map<string, A.Identifier[]>()
    builtinLibs.forEach((lib, name) => {
      this.addModule(name, lib)
    })
  }

  /**
   * Adds the identifiers from the given module to this symbol table.
   * @param name the name of the module
   * @param mod the module object containing the identifiers to add
   */
  addModule (name: string, mod: L.Module): void {
    const identifiers: A.Identifier[] = []
    mod.bindings.forEach((_v, name) => {
      identifiers.push(A.mkId(name))
    })
    this.modules.set(name, identifiers)
  }

  /**
   * @returns retrieves the identifiers for the given module name, or
   *          undefined if no such module exists in this symbol table.
   */
  get (name: string): A.Identifier[] | undefined {
    return this.modules.get(name)
  }
}