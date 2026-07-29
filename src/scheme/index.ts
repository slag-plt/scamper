import * as S from '../lpm'
import { Loc, Range } from '../lpm'
import { lowerProgram } from './codegen.js'
import { expandProgram } from './expansion.js'
import { scopeCheckProgram } from './scope.js'
import { sugarExpr } from './sugarer.js'
import { FiberRaiser } from '../lpm/raiser.js'
import { raiseFiber } from './raise.js'
import { parseProgramFromSource } from './lezer-bridge.js'
import { Exp, mkDisp, Prog } from './ast.js'
import { getQueriedProgram } from './query'
import { isExampleTag } from './docstring/tags/example-tag'
import { parseFunctionDocFromComments } from './docstring/docstring.js'
import { contractProgram } from './contract.js'
// N.B., src/lib/index.ts compiles and runs its own Scamper source (the
// builtin libraries) through this module's `compile()` at load time, which
// makes this a circular import. Keeping it as the *last* import here matters:
// by the time this module re-enters (via that circular reference back into
// `compile()`), every other export `compile()` depends on must already be
// bound, or the reentrant call sees them mid-TDZ.
import { builtinLibs } from '../lib'

export const fiberRaiser: FiberRaiser<Exp> = {
  raise: (fiber) => sugarExpr(raiseFiber(fiber)),
  equals: S.equals,
}

/** The result of parsing: the program (absent on a fatal parse error) and any diagnostics. */
export interface ParseResult {
  program?: Prog
  diagnostics: S.ScamperDiagnostic[]
}

/** A ParseResult from a query parse, additionally carrying the reported range. */
export interface QueryParseResult extends ParseResult {
  queriedRange?: Range
}

export function tokenizeAndParse(src: string): ParseResult
export function tokenizeAndParse(src: string, queryLoc: Loc): QueryParseResult
export function tokenizeAndParse(
  src: string,
  queryLoc?: Loc,
): ParseResult | QueryParseResult {
  const diagnostics: S.ScamperDiagnostic[] = []
  const program = parseProgramFromSource(diagnostics, src)
  if (diagnostics.length > 0) {
    // A parse error leaves no usable program.
    return { diagnostics }
  }

  if (!queryLoc) {
    return { program, diagnostics }
  }

  // given cursor position, find the deepest queried sub-expression and wrap
  // it in a report expression
  const queried = getQueriedProgram(program, queryLoc)
  if (!queried.ok) {
    return { diagnostics: [queried.diagnostic] }
  }
  const queriedProgram = queried.prog
  const queriedRange = queried.range.firstLineSpan(src)

  // determine if query loc inside define statement
  const queriedStmt = queriedProgram.find((s) => s.range.contains(queryLoc))
  if (queriedStmt?.tag !== 'define' || queriedStmt.docComments === undefined) {
    return {
      diagnostics: [
        S.mkDiagnostic(
          'Query',
          'error',
          'Querying is only allowed within function definitions with docstrings',
        ),
      ],
    }
  }
  // docstring parsing is deferred until here, since it's only ever needed
  // for this on-demand query/example-tag feature -- a malformed docstring
  // fails the query itself rather than blocking compilation altogether.
  const { doc, diagnostics: docDiagnostics } = parseFunctionDocFromComments(
    queriedStmt.docComments,
  )
  if (docDiagnostics.length > 0) {
    return { diagnostics: docDiagnostics }
  }
  if (doc === undefined) {
    return {
      diagnostics: [
        S.mkDiagnostic(
          'Query',
          'error',
          'Querying is only allowed within function definitions with docstrings',
        ),
      ],
    }
  }
  // find example tag if exists
  const exampleTags = doc.tags.filter((t) => isExampleTag(t))
  // TODO: only choosing first example tag for input prototype
  const firstExample = exampleTags.at(0)
  if (!firstExample) {
    return {
      diagnostics: [
        S.mkDiagnostic('Query', 'error', 'Querying requires an example tag'),
      ],
    }
  }
  queriedProgram.push(mkDisp(firstExample.contents.functionCall))
  return { program: queriedProgram, queriedRange, diagnostics: [] }
}

/** Knobs for {@link compile}. */
export interface CompileOptions {
  /** A cursor position for the on-demand query/example feature. */
  queryLoc?: Loc
  /** Whether to wrap documented definitions in runtime contract checks. */
  insertContracts?: boolean
  /** Whether to run the (optional) scope-check pass, collecting its diagnostics. */
  scopeCheck?: boolean
}

/** The result of compilation: the lowered program (absent on a fatal parse error) and any diagnostics. */
export interface CompileResult {
  prog?: S.Prog
  diagnostics: S.ScamperDiagnostic[]
}

/** A CompileResult from a query compile, additionally carrying the reported range. */
export interface QueryCompileResult extends CompileResult {
  queriedRange?: Range
}

export async function compile(
  src: string,
  opts: CompileOptions & { queryLoc: Loc },
): Promise<QueryCompileResult>
export async function compile(
  src: string,
  opts?: CompileOptions,
): Promise<CompileResult>
export async function compile(
  src: string,
  opts: CompileOptions = {},
): Promise<CompileResult | QueryCompileResult> {
  const { queryLoc, insertContracts = false, scopeCheck = false } = opts

  const parsed = queryLoc
    ? tokenizeAndParse(src, queryLoc)
    : tokenizeAndParse(src)
  const diagnostics = [...parsed.diagnostics]
  if (parsed.program === undefined) {
    return queryLoc ? { queriedRange: undefined, diagnostics } : { diagnostics }
  }
  const program = parsed.program

  // Scope checking is an optional pass (CLI --check); it only collects
  // diagnostics and never blocks lowering -- callers decide, from the
  // returned diagnostics, whether to run.
  if (scopeCheck) {
    await scopeCheckProgram(diagnostics, expandProgram(program))
  }

  // Lowering/codegen
  const contracted = insertContracts ? contractProgram(program) : program
  const prog = lowerProgram(expandProgram(contracted))
  return queryLoc
    ? { prog, queriedRange: (parsed as QueryParseResult).queriedRange, diagnostics }
    : { prog, diagnostics }
}

export function mkInitialEnv(): S.Env {
  return S.Env.empty.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('runtime', builtinLibs.get('runtime')!).
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('prelude', builtinLibs.get('prelude')!)
}
