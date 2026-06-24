import * as L from "../lpm"
import { Loc, ScamperError } from "../lpm"
import { builtinLibs } from "../lib"
import { lowerProgram } from "./codegen.js"
import { expandProgram } from "./expansion.js"
import { scopeCheckProgram } from "./scope.js"
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
  equals: L.equals,
}

export function tokenizeAndParse(
  err: L.ErrorChannel,
  src: string,
  queryLoc?: Loc,
): Prog | undefined {
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

  if (queryLoc) {
    // given cursor position, find the sexp to wrap
    // and wrap sexp in new report expression
    try {
      sexps = getQueriedAST(sexps, queryLoc)
    } catch (e) {
      if (e instanceof ScamperError) {
        err.report(e)
        return undefined
      }
      throw e
    }
  }

  const errors: L.ScamperError[] = []
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
    return program
  }
  // find example tag if exists
  const exampleTags = queriedStmt.doc.tags.filter((t) => isExampleTag(t))
  // TODO: only choosing first example tag for input prototype
  const firstExample = exampleTags.at(0)
  if (!firstExample) {
    return program
  }
  program.push(mkDisp(firstExample.contents.functionCall))
  return program
}

export function compile(
  err: L.ErrorChannel,
  src: string,
  queryLoc?: Loc,
): L.Prog | undefined {
  let program = tokenizeAndParse(err, src, queryLoc)
  if (program === undefined) {
    return undefined
  }

  // Macro expansion
  program = expandProgram(program)

  // Scope checking
  const errors: L.ScamperError[] = []
  scopeCheckProgram(builtinLibs, errors, program)
  if (errors.length > 0) {
    errors.forEach((e) => {
      err.report(e)
    })
    return undefined
  }

  // Lowering/codegen
  return lowerProgram(program)
}

export function mkInitialEnv(): L.Env {
  const env = L.Env.empty
  // for (const [name, fn] of Runtime.lib) {
  //   env.set(name, fn)
  // }
  // for (const [name, fn] of Prelude.lib) {
  //   env.set(name, fn)
  // }
  return env
}