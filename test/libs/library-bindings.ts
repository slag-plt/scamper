// The standard library's documented definitions, enumerated once, for the
// tests that need to sweep across all of them (docstring well-formedness,
// contract exercising). Each of those had grown its own copy of the same walk
// over `librarySources`; this is that walk, written once.
import { librarySources } from '../../src/lib/generated/sources'
import { tokenizeAndParse } from '../../src/scheme'
import type * as A from '../../src/scheme/ast'
import type { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { isContracted } from '../../src/scheme/contract'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../src/scheme/docstring/docstring'

/** One documented top-level definition of one standard library module. */
export interface LibraryBinding {
  /** The library module the definition lives in, e.g. `image`. */
  module: string
  /** The name bound, e.g. `solid-square`. */
  name: string
  /**
   * The definition itself. Carried alongside `doc` because a docstring says
   * nothing about the *value* being defined -- a consumer that needs to reach
   * the `(js-var "...")` behind a binding has to read it from here.
   */
  stmt: A.Define | A.DefineExport
  /** The parsed docstring, or undefined when it failed to parse. */
  doc?: FunctionDoc
  /** Why the docstring failed to parse; empty when it parsed. */
  diagnostics: ScamperDiagnostic[]
}

/**
 * Every documented `define`/`define-export` across every standard library
 * module, in source order. An undocumented definition is absent; a definition
 * whose docstring is malformed is present with `doc: undefined` and the
 * failure in `diagnostics` -- which is how the library load path treats it too
 * (see src/lib/index.ts's extractDocs).
 */
export function libraryBindings(): LibraryBinding[] {
  const bindings: LibraryBinding[] = []
  for (const [module, src] of librarySources) {
    const { program } = tokenizeAndParse(src, undefined, {
      allowInternalNames: module === 'runtime',
    })
    for (const stmt of program ?? []) {
      if (
        (stmt.tag !== 'define' && stmt.tag !== 'defexport') ||
        stmt.docComments === undefined
      ) {
        continue
      }
      const { doc, diagnostics } = parseFunctionDocFromComments(stmt.docComments)
      bindings.push({ module, name: stmt.name.name, stmt, doc, diagnostics })
    }
  }
  return bindings
}

/** A {@link LibraryBinding} whose docstring parsed, so it carries a doc. */
export interface DocumentedBinding extends LibraryBinding {
  doc: FunctionDoc
}

/**
 * The bindings that get a contract wrapper at library load: those whose
 * docstring parses and documents at least one parameter (see contract.ts's
 * isContracted).
 */
export function contractedBindings(): DocumentedBinding[] {
  return libraryBindings().filter(
    (b): b is DocumentedBinding => b.doc !== undefined && isContracted(b.doc),
  )
}
