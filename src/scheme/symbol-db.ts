import builtinLibs from '../lib'
import * as A from './ast'
import * as L from '../lpm/lang'

// A global collection of external symbols from importable modules.
//
// TODO: I'm pretty sure we need to store more information for each identifier
// but I'll defer those design decisions till later. This is enough to support
// scope checking and the scope tree for now.
let modules: Map<string, A.Identifier[]> | undefined

/** @returns the module map, throwing if the DB has not been initialized */
function table(): Map<string, A.Identifier[]> {
  if (!modules) {
    throw new Error('SymbolDB used before initialize()')
  }
  return modules
}

/** Initializes the symbol DB from the loaded builtin libraries (idempotent). */
export function initialize(): void {
  if (modules) {
    return
  }
  modules = new Map<string, A.Identifier[]>()
  builtinLibs.forEach((lib, name) => {
    addModule(name, lib)
  })
}

/** Adds `mod`'s binding names to the DB under the given module name. */
export function addModule(name: string, mod: L.Module): void {
  const ids: A.Identifier[] = []
  mod.bindings.forEach((_v, bindingName) => {
    ids.push(A.mkId(bindingName))
  })
  table().set(name, ids)
}

/** @returns the identifiers for the given module, or undefined if absent */
export function get(name: string): A.Identifier[] | undefined {
  return table().get(name)
}