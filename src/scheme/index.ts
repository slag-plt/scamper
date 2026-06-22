import * as L from "../lpm"
import { Prelude, Runtime, builtinLibs } from "../lib"
import { lowerProgram } from "./codegen.js"
import { expandProgram } from "./expansion.js"
import { scopeCheckProgram } from "./scope.js"
import { sugarExpr } from "./sugarer.js"
import { raiseThread } from "./raise.js"
import { Raiser } from "../lpm/raiser.js"
import { read } from "./reader.js"
import { parseProgram } from "./parser.js"
import { Exp, Prog } from "./ast.js"
import { ScamperError } from "../lpm"

export const raiser: Raiser<Exp> = {
  raise: (t) => sugarExpr(raiseThread(t)),
  equals: L.equals,
}

export function tokenizeAndParse(
  err: L.ErrorChannel,
  src: string,
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
  const errors: L.ScamperError[] = []
  const program = parseProgram(errors, sexps)
  if (errors.length > 0) {
    errors.forEach((e) => {
      err.report(e)
    })
    return undefined
  }
  return program
}

export function compile(err: L.ErrorChannel, src: string): L.Prog | undefined {
  let program = tokenizeAndParse(err, src)
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
  const env = new L.Env()
  for (const [name, fn] of Runtime.lib) {
    env.set(name, fn)
  }
  for (const [name, fn] of Prelude.lib) {
    env.set(name, fn)
  }
  return env
}
