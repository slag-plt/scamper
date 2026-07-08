import * as S from "../lpm"
import { Loc, Range, ScamperError } from "../lpm"
import { builtinLibs } from "../lib"
import { lowerProgram } from "./codegen.js"
import { expandProgram } from "./expansion.js"
import { sugarExpr } from "./sugarer.js"
import { FiberRaiser } from "../lpm/raiser.js"
import { raiseFiber } from "./raise.js"
import { read } from "./reader.js"
import { parseProgram } from "./parser.js"
import { Exp, mkDisp, Prog } from "./ast.js"
import { getQueriedAST } from "./query"
import { isExampleTag } from "./docstring/tags/example-tag"

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
  let sexps
  try {
    sexps = read(src)
  } catch (e) {
    if (e instanceof ScamperError) {
      err.report(e)
      return undefined
    }
    throw e
  }

  let queriedRange: Range | undefined
  if (queryLoc) {
    // given cursor position, find the sexp to wrap
    // and wrap sexp in new report expression
    try {
      const queried = getQueriedAST(sexps, queryLoc)
      sexps = queried.ast
      queriedRange = queried.range.firstLineSpan(src)
    } catch (e) {
      if (e instanceof ScamperError) {
        err.report(e)
        return undefined
      }
      throw e
    }
  }

  const errors: S.ScamperError[] = []
  const program = parseProgram(errors, sexps)
  if (errors.length > 0) {
    errors.forEach((e) => {
      err.report(e)
    })
    return undefined
  }

  if (!queryLoc) {
    return program
  }

  // determine if query loc inside define statement
  const queriedStmt = program.find((s) => s.range.contains(queryLoc))
  if (queriedStmt?.tag !== "define" || queriedStmt.doc === undefined) {
    err.report(
      new ScamperError(
        "Parser",
        "Querying is only allowed within function definitions with docstrings",
      ),
    )
    return undefined
  }
  // find example tag if exists
  const exampleTags = queriedStmt.doc.tags.filter((t) => isExampleTag(t))
  // TODO: only choosing first example tag for input prototype
  const firstExample = exampleTags.at(0)
  if (!firstExample) {
    err.report(new ScamperError("Parser", "Querying requires an example tag"))
    return undefined
  }
  program.push(mkDisp(firstExample.contents.functionCall))
  if (queriedRange === undefined) {
    return undefined
  }
  return { program, queriedRange }
}

export async function compile(
  err: S.ErrorChannel,
  src: string,
): Promise<S.Prog | undefined>
export async function compile(
  err: S.ErrorChannel,
  src: string,
  queryLoc: Loc,
): Promise<{ prog: S.Prog; queriedRange: Range } | undefined>
// Scope checking will add await here eventually.
// eslint-disable-next-line @typescript-eslint/require-await
export async function compile(
  err: S.ErrorChannel,
  src: string,
  queryLoc?: Loc,
): Promise<S.Prog | { prog: S.Prog; queriedRange: Range } | undefined> {
  if (queryLoc) {
    const parsed = tokenizeAndParse(err, src, queryLoc) as
      | { program: Prog; queriedRange: Range }
      | undefined
    if (parsed === undefined) {
      return undefined
    }
    const prog = lowerProgram(expandProgram(parsed.program))
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
  // if (errors.length > 0) {
  //   errors.forEach((e) => {
  //     err.report(e)
  //   })
  //   return undefined
  // }

  // Lowering/codegen
  return lowerProgram(expandProgram(program))
}

export function mkInitialEnv(): S.Env {
  return S.Env.empty.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('runtime', builtinLibs.get('runtime')!).
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    extendWithImport('prelude', builtinLibs.get('prelude')!)
}
