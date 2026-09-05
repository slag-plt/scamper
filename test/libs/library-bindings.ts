// The standard library's documented definitions, enumerated once, for the
// tests that sweep across all of them (docstring well-formedness, contract
// exercising). Each of those had grown its own copy of the same walk over
// `librarySources`; this is that walk, written once.
import { librarySources } from '../../src/lib/generated/sources'
import { tokenizeAndParse } from '../../src/scheme'
import type { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { isContracted } from '../../src/scheme/contract'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../../src/scheme/docstring/docstring'

/** One commented top-level definition of one standard library module. */
export interface LibraryBinding {
  /** The library module the definition lives in, e.g. `image`. */
  module: string
  /** The name bound, e.g. `solid-square`. */
  name: string
  /** The parsed docstring, or undefined when there is not one. */
  doc?: FunctionDoc
  /** Why the docstring failed to parse; empty when it parsed or was absent. */
  diagnostics: ScamperDiagnostic[]
}

/**
 * Every commented `define`/`define-export` across every standard library
 * module, in source order. A definition with no comment at all is absent.
 *
 * `doc: undefined` covers two cases the caller has to tell apart, which is what
 * `diagnostics` is for: a definition carrying only ordinary `;`/`;;` comments
 * has no docstring to parse and no diagnostic (17 of them, e.g. `prelude:apply`
 * and the `runtime:##...##` primitives), while a malformed `;;;` block has the
 * parse failure in `diagnostics`. The library load path treats the second the
 * same way -- see src/lib/index.ts's extractDocs.
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
      bindings.push({ module, name: stmt.name.name, doc, diagnostics })
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
