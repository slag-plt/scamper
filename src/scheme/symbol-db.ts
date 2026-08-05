import builtinLibs from '../lib'
import * as A from './ast'
import * as L from '../lpm/lang'
import { Range } from '../lpm'
import { ScamperDiagnostic, diagnosticToError } from './diagnostic.js'
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
  const diagnostics: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(diagnostics, src)
  if (diagnostics.length > 0) {
    throw diagnosticToError(diagnostics[0])
  }
  return prog
}

/**
 * @returns the identifiers a module exports: the names its `export` /
 *   `define-export` statements declare (the union), restricted to those it
 *   actually binds at top level. A module with no export statement exports
 *   nothing. Mirrors the runtime (Fiber.getModule / getTopLevelAsModule).
 */
function moduleIdentifiers(prog: A.Prog): A.Identifier[] {
  // The names the module binds at top level, and which of them it exports.
  const defined = new Map<string, A.Identifier>()
  const exported = new Set<string>()
  const bind = (id: A.Identifier) => defined.set(id.name, id)
  for (const stmt of prog) {
    switch (stmt.tag) {
      case 'define':
        bind(stmt.name)
        break
      case 'defexport':
        bind(stmt.name)
        exported.add(stmt.name.name)
        break
      case 'struct':
        // A struct binds the constructor plus the synthesized `${name}?`
        // predicate and `${name}-${field}` accessors (see expansion.ts); any of
        // them may be named by an export statement.
        bind(stmt.name)
        bind(A.mkId(`${stmt.name.name}?`, stmt.name.range))
        for (const field of stmt.fields) {
          bind(A.mkId(`${stmt.name.name}-${field.name}`, stmt.name.range))
        }
        break
      case 'export':
        for (const n of stmt.names) {
          exported.add(n.name)
        }
        break
    }
  }
  // Export only names actually bound here (re-exporting an import is a no-op, as
  // at runtime -- imports aren't top-level bindings of this module).
  const ids: A.Identifier[] = []
  for (const name of exported) {
    const id = defined.get(name)
    if (id !== undefined) {
      ids.push(id)
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

/** @returns the file-import statements in `prog`, in source order */
function fileImportStmts(prog: A.Prog): A.Import[] {
  return prog.filter(
    (stmt): stmt is A.Import => stmt.tag === 'import' && stmt.kind === 'file',
  )
}

/** @returns the file-system modules directly imported by `prog` */
export function fileImports(prog: A.Prog): string[] {
  return fileImportStmts(prog).map((stmt) => stmt.module)
}

/** A file-import failure discovered while loading the transitive import graph. */
export interface ImportFailure {
  /** The module that could not be loaded or parsed. */
  filename: string
  /** The file whose import statement directly referenced it. */
  importer: string
  /** The range of the top-level import that transitively pulled it in. */
  range: Range
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
 * Loads every file transitively imported by `prog` into the DB. A module that
 * is missing or fails to parse is skipped (its symbols won't be available).
 *
 * The top-level program's own direct-import failures are reported separately by
 * scope checking (which detects them via the absent DB entry and the import's
 * own range -- see scope.ts). This function therefore collects only the
 * failures found *below* the top level -- inside an imported file -- which
 * nothing else surfaces, and returns them so the caller can report them.
 *
 * @returns the transitive (non-top-level) import failures encountered
 */
export async function loadTransitiveImports(
  prog: A.Prog,
): Promise<ImportFailure[]> {
  const failures: ImportFailure[] = []
  const seen = new Set<string>()
  // `importer` is the file currently being visited (undefined for the top-level
  // program); `origin` is the top-level import statement whose subtree we're
  // descending (undefined at the top level). Both are set together exactly when
  // we are below the top level.
  const visit = async (
    p: A.Prog,
    importer: string | undefined,
    origin: A.Import | undefined,
  ): Promise<void> => {
    for (const stmt of fileImportStmts(p)) {
      const filename = stmt.module
      if (seen.has(filename)) {
        continue
      }
      seen.add(filename)
      try {
        const imported = await parseFile(filename)
        table().set(filename, moduleIdentifiers(imported))
        await visit(imported, filename, origin ?? stmt)
      } catch {
        // A missing / unparseable module. Skip it, but if it is below the top
        // level record the failure: the diagnostic points at the top-level
        // import that pulled it in (a location in the program being checked)
        // and names the broken file and its direct importer.
        if (importer !== undefined && origin !== undefined) {
          failures.push({ filename, importer, range: origin.range })
        }
      }
    }
  }
  await visit(prog, undefined, undefined)
  return failures
}

/** @returns the identifiers for the given module, or undefined if absent */
export function get(name: string): A.Identifier[] | undefined {
  return table().get(name)
}