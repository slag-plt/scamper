import * as L from '../lpm'
import { Fiber } from '../lpm/fiber.js'
import { runFiberOnScheduler } from '../lpm/run.js'
import { builtinLibs } from '../lpm/builtin-registry.js'
import * as A from '../scheme/ast.js'
import { compile, tokenizeAndParse } from '../scheme/index.js'
import { jsVar } from '../js/index.js'
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
async function loadLibrary(
  name: string,
  src: string,
  runtime?: L.Module,
): Promise<L.Module> {
  // N.B., insertContracts=true: only the standard library gets its exports
  // wrapped with contract checks derived from their docstrings, not
  // arbitrary user programs. allowInternalNames is narrower still: the
  // `##...##` shape is reserved for the runtime (see ParseOptions), and
  // runtime.scm is the interop layer that binds those primitives.
  const { prog, diagnostics } = await compile(src, {
    insertContracts: true,
    allowInternalNames: name === 'runtime',
  })
  if (prog === undefined || diagnostics.length > 0) {
    throw new L.ICE(
      'lib.loadLibrary',
      `Failed to compile builtin library "${name}": ${diagnostics.map((d) => d.message).join('; ')}`,
    )
  }
  // js-var is the FFI root primitive -- it can't be bound via itself, so it's
  // injected directly into every library's load environment (and exported
  // explicitly below, since it carries no export statement).
  //
  // `runtime` joins it for every library but runtime.scm itself, which is what
  // lets a library be written in plain Scamper rather than as a wrapper around
  // JS: `struct` expands to calls to `##mkCtorFn##` and friends, and those are
  // runtime.scm's bindings. They are needed at *load* time -- a struct's
  // constructor is built when the define runs -- so an importer's env, which is
  // what a library closure's free names otherwise resolve against (see
  // Env.rehomeExports), is too late.
  const baseEnv = L.Env.empty.extendWithTopLevel(['js-var', jsVar])
  const fiber = new Fiber(
    prog,
    runtime === undefined
      ? baseEnv
      : baseEnv.extendWithImport('runtime', runtime),
    // Mark every closure this library defines as step-over, so a reduction
    // trace treats library calls atomically (see Closure.stepOver).
    true,
  )
  // Run it the way any other program runs. Going through the scheduler is what
  // lets a builtin library use a blocking primitive at load time; a hand-stepped
  // fiber cannot service one.
  //
  // The scheduler reports a runtime error and continues at the next statement,
  // which here would install a half-initialized module. A builtin library that
  // fails to load is a bug in Scamper itself, so fail loudly instead -- as the
  // compile-failure check above does.
  const out = new L.LoggingChannel(false, false)
  await runFiberOnScheduler(fiber, { out, err: out })
  if (out.errLog.length > 0) {
    throw new L.ICE(
      'lib.loadLibrary',
      `Failed to run builtin library "${name}": ${out.errLog.join('; ')}`,
    )
  }
  // Every library now declares its exports with `define-export` (see src/lib/*.scm).
  // js-var is injected rather than defined (it's the FFI root, so it can't be
  // bound via itself), so it carries no export statement -- add it explicitly so
  // user code and the scope-checker pick it up, as before.
  fiber.addExports(['js-var'])
  return fiber.getModule()
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
    // Libraries document their exports with define-export (see src/lib/*.scm);
    // a plain documented define is still supported for anything not exported.
    if ((stmt.tag !== 'define' && stmt.tag !== 'defexport') || !stmt.docComments) {
      continue
    }
    const { doc } = parseFunctionDocFromComments(stmt.docComments)
    if (doc) {
      docs.set(stmt.name.name, doc)
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
  // runtime.scm goes first and alone: it binds the `##...##` primitives that
  // `struct` (and any other form that lowers to them) expands into, so every
  // other library loads with it already in scope.
  const runtimeSrc = librarySources.find(([name]) => name === 'runtime')
  if (runtimeSrc === undefined) {
    throw new L.ICE('lib.initializeLibs', 'The runtime library is missing!')
  }
  const runtime = await loadLibrary('runtime', runtimeSrc[1])
  builtinLibs.set('runtime', runtime)
  for (const [name, mod] of await Promise.all(
    librarySources
      .filter(([name]) => name !== 'runtime')
      .map(
        async ([name, src]): Promise<[string, L.Module]> => [
          name,
          await loadLibrary(name, src, runtime),
        ],
      ),
  )) {
    builtinLibs.set(name, mod)
  }
  for (const [name, src] of librarySources) {
    const { program } = tokenizeAndParse(src, undefined, {
      allowInternalNames: name === 'runtime',
    })
    docRegistry.set(
      name,
      program ? extractDocs(program) : new Map<string, FunctionDoc>(),
    )
  }
  initialized = true
}

export { builtinLibs }
export default builtinLibs
