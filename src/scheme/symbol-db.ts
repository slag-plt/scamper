import builtinLibs from '../lib'
import * as A from './ast'
import * as L from '../lpm/lang'
import { ScamperError } from '../lpm'
import { parseProgramFromSource } from './lezer-bridge'

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

/**
 * Reads and parses a file-system module to its AST.
 * @throws the first ScamperError raised if the module fails to parse
 */
async function parseFile(filename: string): Promise<A.Prog> {
  // N.B., load the file system lazily: importing '../fs' statically would pull
  // the OPFS implementation into this module's graph, and since the test
  // harness's global setup imports this module (for initialize()), that would
  // disturb tests that mock the file system (see test/setup.ts).
  const { getFS } = await import('../fs')
  const src = await getFS().loadFile(filename)
  const errors: ScamperError[] = []
  const prog = parseProgramFromSource(errors, src)
  if (errors.length > 0) {
    throw errors[0]
  }
  return prog
}

/** @returns the identifiers a module exports: its top-level definitions */
function moduleIdentifiers(prog: A.Prog): A.Identifier[] {
  // TODO: a struct also exports its `${name}?` predicate and `${name}-${field}`
  // accessors (only synthesized during expansion); re-exported imports aren't
  // surfaced either.
  const ids: A.Identifier[] = []
  for (const stmt of prog) {
    if (stmt.tag === 'define' || stmt.tag === 'struct') {
      ids.push(stmt.name)
    }
  }
  return ids
}

/**
 * Loads a file-system module: parses `filename` and files its top-level
 * definitions under `filename` in the DB.
 * @throws the first ScamperError raised if the module fails to parse
 */
export async function loadModuleFromFile(filename: string): Promise<void> {
  table().set(filename, moduleIdentifiers(await parseFile(filename)))
}

/** @returns the file-system modules directly imported by `prog` */
export function fileImports(prog: A.Prog): string[] {
  const imports: string[] = []
  for (const stmt of prog) {
    if (stmt.tag === 'import' && stmt.kind === 'file') {
      imports.push(stmt.module)
    }
  }
  return imports
}

/**
 * Walks `prog` and every file-system module it imports, recursively.
 * @returns every transitively-imported file, in discovery order, with
 *          duplicates and cycles collapsed
 * @throws the first ScamperError raised if any imported file fails to parse
 */
export async function transitiveFileImports(prog: A.Prog): Promise<string[]> {
  const result: string[] = []
  const seen = new Set<string>()
  const visit = async (p: A.Prog): Promise<void> => {
    for (const filename of fileImports(p)) {
      if (seen.has(filename)) {
        continue
      }
      seen.add(filename)
      result.push(filename)
      await visit(await parseFile(filename))
    }
  }
  await visit(prog)
  return result
}

/**
 * Loads every file transitively imported by `prog` into the DB, best-effort:
 * a module that is missing or fails to parse is skipped (its symbols simply
 * won't be available). Callers detect a skipped module by its absent DB entry
 * and report it with the import's own source range (see scope.ts).
 */
export async function loadTransitiveImports(prog: A.Prog): Promise<void> {
  const seen = new Set<string>()
  const visit = async (p: A.Prog): Promise<void> => {
    for (const filename of fileImports(p)) {
      if (seen.has(filename)) {
        continue
      }
      seen.add(filename)
      try {
        const imported = await parseFile(filename)
        table().set(filename, moduleIdentifiers(imported))
        await visit(imported)
      } catch {
        // Best-effort: skip a missing / unparseable module (see above).
      }
    }
  }
  await visit(prog)
}

/** @returns the identifiers for the given module, or undefined if absent */
export function get(name: string): A.Identifier[] | undefined {
  return table().get(name)
}