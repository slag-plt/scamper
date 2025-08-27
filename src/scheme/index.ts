import * as L from '../lpm'
import { Prelude, Runtime, builtinLibs } from '../lib'
import { lowerProgram } from './codegen'
import { expandProgram } from './expansion'
import { read } from './reader.js'
import { scopeCheckProgram } from './scope.js'
import { parseProgram } from './parser.js'

export function compile (err: L.ErrorChannel, src: string): L.Blk | undefined {
  // Tokenization and reading (to Sexps)
  let sexps = undefined 
  try {
    sexps = read(src)
  } catch (e: any) {
    err.report(e)
    return undefined
  }

  // Parsing (to AST)
  let errors: L.ScamperError[] = []
  let program = parseProgram(errors, sexps)
  if (errors.length > 0) {
    errors.forEach((e) => err.report(e))
    return undefined
  }

  // Macro expansion
  program = expandProgram(program)

  // Scope checking
  errors = []
  scopeCheckProgram(builtinLibs, errors, program)
  if (errors.length > 0) {
    errors.forEach((e) => err.report(e))
    return undefined
  }

  // Lowering/codegen
  return lowerProgram(program)
}

export function mkInitialEnv (): L.Env {
  const env = new L.Env()
  for (const [name, fn] of Runtime.lib) {
    env.set(name, fn)
  }
  for (const [name, fn] of Prelude.lib) {
    env.set(name, fn)
  }
  return env
}