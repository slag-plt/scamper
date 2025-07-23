import { ScamperError, Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import * as A from './ast.js'

function scopeCheckSingle (errors: ScamperError[], globals: string[], locals: string[], v: Value) {
  const { range: _range, value } = A.unpackSyntax(v)
  v = value
  if (R.isSym(v)) {
    // TODO: need to distinguish between a local with
    // an index and a global with a name
  }
}

function scopeCheckExpr (errors: ScamperError[], globals: string[], locals: string[], v: Value) {
  if (A.isAtom(v)) {
    scopeCheckSingle(errors, globals, locals, v)
  } else if (A.isLambda(v)) {

  } else if (A.isLet(v)) {

  } else if (A.isLetStar(v)) {

  } else if (A.isAnd(v)) {

  } else if (A.isOr(v)) {

  } else if (A.isBegin(v)) {

  } else if (A.isIf(v)) {

  } else if (A.isCond(v)) {

  } else if (A.isMatch(v)) {

  } else if (A.isQuote(v)) {

  } else if (A.isSection(v)) {

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
    const { name, value, range } = A.asDefine(v)
    if (globals.includes(A.stripSyntax(name) as string)) {
      errors.push(new ScamperError('Parser',  `Global variable '${name}' is already defined`, undefined, A.rangeOf(name, range)))
    } else {
      globals.push(A.stripSyntax(name) as string)
    }
    scopeCheckExpr(errors, globals, [], value)
  } else if (A.isDisplay(v)) {
    const { value, range: _range } = A.asDisplay(v)
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