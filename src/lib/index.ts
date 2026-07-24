import * as L from '../lpm'
import { Fiber } from '../lpm/fiber.js'
import { builtinLibs } from '../lpm/builtin-registry.js'
import { SimpleErrorChannel } from '../lpm/output/simple-error.js'
import * as A from '../scheme/ast.js'
import { compile, tokenizeAndParse } from '../scheme/index.js'
import {
  FunctionDoc,
  parseFunctionDocFromComments,
} from '../scheme/docstring/docstring.js'

import { librarySources } from './generated/sources.js'

/**
 * Compiles and runs a standard library module's Scamper source (a flat
 * sequence of `(define name (js-var "..."))` forms -- see src/lib/*.scm) and
 * snapshots the resulting top-level bindings as a Module.
 */
async function loadLibrary(name: string, src: string): Promise<L.Module> {
  const errChannel = new SimpleErrorChannel()
  // N.B., insertContracts=true: only the standard library gets its exports
  // wrapped with contract checks derived from their docstrings, not
  // arbitrary user programs.
  const prog = await compile(errChannel, src, undefined, true)
  if (prog === undefined || errChannel.errors.length > 0) {
    throw new L.ICE(
      'lib.loadLibrary',
      `Failed to compile builtin library "${name}": ${errChannel.errors.map((e) => e.toString()).join('; ')}`,
    )
  }
  const fiber = new Fiber(prog)
  while (!fiber.isDone()) {
    fiber.step()
  }
  return fiber.topLevelEnv.getTopLevelAsModule()
}

/**
 * Parses every documented top-level define in a library's (pre-lowering)
 * AST into a name -> FunctionDoc map, for the doc registry below. A define
 * with no docstring is simply absent from the map, and a define whose
 * docstring fails to parse is skipped rather than failing the whole library
 * load -- malformed documentation is a documentation-quality issue, not a
 * reason to refuse loading otherwise-working code (see docstring.ts's
 * parseFunctionDocFromComments).
 */
function extractDocs(prog: A.Prog): Map<string, FunctionDoc> {
  const docs = new Map<string, FunctionDoc>()
  for (const stmt of prog) {
    if (stmt.tag !== 'define' || !stmt.docComments) {
      continue
    }
    try {
      const doc = parseFunctionDocFromComments(stmt.docComments)
      if (doc) {
        docs.set(stmt.name.name, doc)
      }
    } catch (e) {
      if (!(e instanceof L.ScamperError)) {
        throw e
      }
    }
  }
  return docs
}

/**
 * Module name -> (binding name -> FunctionDoc) for every builtin library, for
 * consumers that need to read parsed docstrings (e.g. a search/docs viewer)
 * without recompiling library source themselves. A binding with no
 * documented entry is simply absent, so a two-level `.get(mod)?.get(name)`
 * naturally yields undefined for "no docstring"/"module doesn't exist".
 * Populated by initializeLibs() below -- empty until then.
 */
export const docRegistry = new Map<string, Map<string, FunctionDoc>>()

let initialized = false

/**
 * Compiles and runs every builtin library, populating both `builtinLibs`
 * (see builtin-registry.ts for why that's a separate shared Map, mutated in
 * place rather than replaced) and `docRegistry` above. Idempotent -- safe to
 * call from multiple entry points (e.g. once from Scamper's own initialize()
 * and once from a test's global setup) without redoing the work.
 *
 * N.B., must be called explicitly, rather than running as a side effect of
 * importing this module (as it used to, via top-level await): src/lib and
 * src/scheme are already mutually circular (see the N.B. on scheme/index.ts's
 * `import { builtinLibs } from "../lib"`), and having this module reach into
 * scheme/docstring/docstring.js -- itself reachable from scheme/index.ts by
 * a different path (docstring.ts -> tags/example-tag.ts -> scheme/index.ts)
 * -- as an eager, load-time side effect hit a second, genuinely
 * unresolvable cycle through some entry points (e.g. a test file statically
 * importing scheme/docstring/param.ts before anything else touches
 * src/lib), under Vitest/vite-node's SSR module loader: one of param.ts's
 * own circular imports (`tokenizeAndParse` from scheme/index.ts) would
 * still be mid-initialization. Deferring all of this to an explicit call,
 * made once by application startup code well after the whole module graph
 * has finished loading, sidesteps it entirely.
 */
export async function initializeLibs(): Promise<void> {
  if (initialized) {
    return
  }
  for (const [name, mod] of await Promise.all(
    librarySources.map(
      async ([name, src]): Promise<[string, L.Module]> => [
        name,
        await loadLibrary(name, src),
      ],
    ),
  )) {
    builtinLibs.set(name, mod)
  }
  for (const [name, src] of librarySources) {
    const parsed = tokenizeAndParse(new SimpleErrorChannel(), src)
    docRegistry.set(
      name,
      parsed ? extractDocs(parsed) : new Map<string, FunctionDoc>(),
    )
  }
  initialized = true
}

export { builtinLibs }
export default builtinLibs
