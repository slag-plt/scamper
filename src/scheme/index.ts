import * as L from '../lpm'
import builtinLibs from '../lib'
import { lowerProgram } from './codegen'
import { expandProgram } from './expansion'
import { stringToTokens, parseValues } from './reader.js'
import { scopeCheckProgram } from './scope.js'
import { checkSyntaxProgram } from './syntax.js'

export function compile (err: L.ErrorChannel, src: string): L.Blk | undefined {
  // Tokenization
  let tokens = undefined 
  try {
    tokens = stringToTokens(src)
  } catch (e: any) {
    err.err(e)
    return undefined
  }

  // Parsing (to S-expressions)
  let program = undefined
  try {
    program = parseValues(tokens)
  } catch (e: any) {
    err.err(e)
    return undefined
  }

  // Syntax check
  let errors = checkSyntaxProgram(program)
  if (errors.length > 0) {
    for (const e of errors) {
      err.err(e)
    }
    return undefined
  }

  // Macro expansion
  program = expandProgram(program)

  // Scope checking
  const scopeCheckResult = scopeCheckProgram(builtinLibs, program)
  program = scopeCheckResult.program
  errors = scopeCheckResult.errors
  if (errors.length > 0) {
    for (const e of errors) {
      err.err(e)
    }
    return undefined
  }

  // Lowering/codegen
  return lowerProgram(program)
}

export function mkInitialEnv (): L.Env {
  const env = new L.Env()
  for (const [name, fn] of builtinLibs.get('prelude')!.lib) {
    env.set(name, fn)
  }
  return env
}