import * as S from '../lpm'
import { Loc, Range, ScamperError } from '../lpm'
import { lowerProgram } from './codegen.js'
import { expandProgram } from './expansion.js'
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

export function tokenizeAndParse(
  err: S.ErrorChannel,
  src: string,
  queryLoc?: Loc,
): Prog | undefined
export function tokenizeAndParse(
  err: S.ErrorChannel,
  src: string,
  queryLoc: Loc,
): { program: Prog; queriedRange: Range } | undefined
export function tokenizeAndParse(
  err: S.ErrorChannel,
  src: string,
  queryLoc?: Loc,
): Prog | { program: Prog; queriedRange: Range } | undefined {
  const errors: S.ScamperError[] = []
  const program = parseProgramFromSource(errors, src)
  if (errors.length > 0) {
    errors.forEach((e) => {
      err.report(e)
    })
    return undefined
  }

  if (!queryLoc) {
    return program
  }

  // given cursor position, find the deepest queried sub-expression and wrap
  // it in a report expression
  let queriedProgram: Prog
  let queriedRange: Range
  try {
    const queried = getQueriedProgram(program, queryLoc)
    queriedProgram = queried.prog
    queriedRange = queried.range.firstLineSpan(src)
  } catch (e) {
    if (e instanceof ScamperError) {
      err.report(e)
      return undefined
    }
    throw e
  }

  // determine if query loc inside define statement
  const queriedStmt = queriedProgram.find((s) => s.range.contains(queryLoc))
  if (queriedStmt?.tag !== 'define' || queriedStmt.docComments === undefined) {
    err.report(
      new ScamperError(
        'Parser',
        'Querying is only allowed within function definitions with docstrings',
      ),
    )
    return undefined
  }
  // docstring parsing is deferred until here, since it's only ever needed
  // for this on-demand query/example-tag feature -- a malformed docstring
  // fails the query itself rather than blocking compilation altogether.
  let doc
  try {
    doc = parseFunctionDocFromComments(queriedStmt.docComments)
  } catch (e) {
    if (e instanceof ScamperError) {
      err.report(e)
      return undefined
    }
    throw e
  }
  if (doc === undefined) {
    err.report(
      new ScamperError(
        'Parser',
        'Querying is only allowed within function definitions with docstrings',
      ),
    )
    return undefined
  }
  // find example tag if exists
  const exampleTags = doc.tags.filter((t) => isExampleTag(t))
  // TODO: only choosing first example tag for input prototype
  const firstExample = exampleTags.at(0)
  if (!firstExample) {
    err.report(new ScamperError('Parser', 'Querying requires an example tag'))
    return undefined
  }
  queriedProgram.push(mkDisp(firstExample.contents.functionCall))
  return { program: queriedProgram, queriedRange }
}

export async function compile(
  err: S.ErrorChannel,
  src: string,
  queryLoc?: undefined,
  insertContracts?: boolean,
): Promise<S.Prog | undefined>
export async function compile(
  err: S.ErrorChannel,
  src: string,
  queryLoc: Loc,
  insertContracts?: boolean,
): Promise<{ prog: S.Prog; queriedRange: Range } | undefined>
// Scope checking will add await here eventually.
// eslint-disable-next-line @typescript-eslint/require-await
export async function compile(
  err: S.ErrorChannel,
  src: string,
  queryLoc?: Loc,
  insertContracts = false,
): Promise<S.Prog | { prog: S.Prog; queriedRange: Range } | undefined> {
  if (queryLoc) {
    const parsed = tokenizeAndParse(err, src, queryLoc) as
      | { program: Prog; queriedRange: Range }
      | undefined
    if (parsed === undefined) {
      return undefined
    }
    const program = insertContracts
      ? contractProgram(parsed.program)
      : parsed.program
    const prog = lowerProgram(expandProgram(program))
    return { prog, queriedRange: parsed.queriedRange }
  }

  const program = tokenizeAndParse(err, src)
  if (program === undefined) {
    return undefined
  }

  // Scope checking
  // TODO: disabled while we fix up modules
  // const errors: S.ScamperError[] = []
  // await scopeCheckProgram(builtinLibs, errors, program)
  // errors.forEach((e) => {
  //   err.report(e)
  // })
  // if (errors.some((e) => e.isFatal)) {
  //   return undefined
  // }

  // Lowering/codegen
  const contracted = insertContracts ? contractProgram(program) : program
  return lowerProgram(expandProgram(contracted))
}

export function mkInitialEnv(): S.Env {
  return S.Env.empty.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('runtime', builtinLibs.get('runtime')!).
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('prelude', builtinLibs.get('prelude')!)
}
