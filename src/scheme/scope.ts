import { ScamperError, Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import * as A from './ast.js'

/**
 * @return the recorded De Brujin index of the given variable name. Note that
 * because of scoping this will be the most recent index recorded for the name
 * in the locals array.
 */
function getIndexOfLocal (locals: string[], name: string): number {
  for (let i = locals.length - 1; i >= 0; i--) {
    if (locals[i] === name) {
      return i
    }
  }
  return -1 
}

function scopeCheckSingle (errors: ScamperError[], globals: string[], locals: string[], v: Value): Value {
  const { metadata, value: uv } = A.unpackSyntax(v)
  if (R.isSym(uv)) {
    const name = uv.value
    const localIndex = getIndexOfLocal(locals, name)
    if (localIndex === -1) {
      if (globals.includes(name)) {
        return A.mkSyntax(uv, ...metadata)
      } else {
        errors.push(new ScamperError('Parser', `Variable '${name}' is undefined`, undefined, metadata.get('range')))
      }
    } else {
      return A.mkSyntax(uv, ...metadata, ['local', localIndex])
    }
  } else {
    return v
  }
}

function scopeCheckExpr (errors: ScamperError[], globals: string[], locals: string[], v: Value): Value {
  if (A.isAtom(v)) {
    return scopeCheckSingle(errors, globals, locals, v)
  } else if (A.isLambda(v)) {
    let { params, body, metadata: _metadata } = A.asLambda(v)
    const boundVars: string[] = params.map((v) => A.stripSyntax(v) as string)
    body = scopeCheckExpr(errors, globals, [...locals, ...boundVars], body)
  } else if (A.isLet(v)) {

  } else if (A.isBegin(v)) {

  } else if (A.isIf(v)) {

  } else if (A.isMatch(v)) {

  } else if (A.isQuote(v)) {

  } else {
    // TODO: function application case!
  }
}

function scopeCheckStmt (errors: ScamperError[], globals: string[], v: Value) {
  if (A.isImport(v)) {
    // const { name, range } = A.asImport(v)
    // TODO: check to see if libname is a valid library name
    // TODO: add the library names to global scope
    // N.B., if libraries have name conflicts, we probably want to them
    // to shadow naturally. Probably should give a warning in this case!
  } else if (A.isDefine(v)) {
    const { name, value, metadata } = A.asDefine(v)
    if (globals.includes(A.stripSyntax(name) as string)) {
      errors.push(new ScamperError('Parser',  `Global variable '${name}' is already defined`, undefined, metadata.get('range')))
    } else {
      globals.push(A.stripSyntax(name) as string)
    }
    scopeCheckExpr(errors, globals, [], value)
  } else if (A.isDisplay(v)) {
    const { value, metadata } = A.asDisplay(v)
    scopeCheckExpr(errors, globals, [], value)
  } else if (A.isStruct(v)) {
    // TODO: steal this from sem.ts, just adds a bunch of
    // JS functions to global scope!
  } else {
    scopeCheckExpr(errors, globals, [], v)
  }
}

export function scopeCheckProgram (vs: Value[]): ScamperError[] {
  const errors: ScamperError[] = []
  const globals: string[] = []
  for (const v of vs) {
    scopeCheckStmt(errors, globals, v)
  }
  return errors
}