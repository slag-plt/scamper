import * as L from '../lpm'
import { lowerProgram } from './codegen'
import { expandProgram } from './expansion'
import { stringToTokens, parseValues } from './reader.js'
import { scopeCheckProgram } from './scope.js'
import { checkSyntaxProgram } from './syntax.js'

export function compile (builtinLibs: Map<string, L.Library>, err: L.ErrorChannel, src: string): L.Blk | undefined {
  // Tokenization
  let tokens = undefined 
  try {
    tokens = stringToTokens(src)
  } catch (e: any) {
    err.send(e)
    return undefined
  }

  // Parsing (to S-expressions)
  let program = undefined
  try {
    program = parseValues(tokens)
  } catch (e: any) {
    err.send(e)
    return undefined
  }

  // Syntax check
  let errors = checkSyntaxProgram(program)
  if (errors.length > 0) {
    for (const e of errors) {
      err.send(e)
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
      err.send(e)
    }
    return undefined
  }

  // Lowering/codegen
  return lowerProgram(program)
}