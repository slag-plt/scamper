import { ScamperError, Value } from '../lpm'
import * as L from '../lpm'
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
  // N.B., this is the key operation! We resolve variables by using the maps
  // to determine whether they are local or global. Global variables are left
  // as is. However, local variables are annotated with a 'local' metadata
  // field that contains their calculated De Brujin index.
  if (L.isSym(uv)) {
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
    let { params, body, metadata } = A.asLambda(v)
    const localBase = locals.length
    const boundVars: string[] = params.map((v) => A.stripSyntax(v) as string)
    body = scopeCheckExpr(errors, globals, [...locals, ...boundVars], body)
    return A.mkSyntax(L.mkList(L.mkSym('lambda'), L.mkList(...params), body), ['localBase', localBase], ...metadata)
  } else if (A.isLet(v)) {
    let { bindings, body, metadata } = A.asLet(v)
    bindings = bindings.map(({ fst, snd, metadata }) => ({
      fst,
      snd: scopeCheckExpr(errors, globals, locals, snd),
      metadata
    }))
    const localBase = locals.length
    const boundVars: string[] = bindings.map(({ fst }) => (A.stripSyntax(fst) as L.Sym).value)
    body = scopeCheckExpr(errors, globals, [...locals, ...boundVars], body)
    return A.mkSyntax(L.mkList(L.mkSym('let'), L.mkList(...bindings), body), ['localBase', localBase], ...metadata)
  } else if (A.isBegin(v)) {
    let { values, metadata } = A.asBegin(v)
    values = values.map((v) => scopeCheckExpr(errors, globals, locals, v))
    return A.mkSyntax(L.mkList(L.mkSym('begin'), ...values), ...metadata)
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, metadata } = A.asIf(v)
    const guardExpr = scopeCheckExpr(errors, globals, locals, guard)
    const ifExpr = scopeCheckExpr(errors, globals, locals, ifB)
    const elseExpr = scopeCheckExpr(errors, globals, locals, elseB)
    return A.mkSyntax(L.mkList(L.mkSym('if'), guardExpr, ifExpr, elseExpr), ...metadata)
  } else if (A.isMatch(v)) {
    // TODO: need to handle binders in the patterns... darn!
  } else if (A.isQuote(v)) {
    const { value, metadata } = A.asQuote(v)
    const body = scopeCheckExpr(errors, globals, locals, value)
    return A.mkSyntax(L.mkList(L.mkSym('quote'), body), ...metadata)
  } else {
    const { values, metadata } = A.asApp(v)
    return A.mkSyntax(L.mkList(...values.map((v) => scopeCheckExpr(errors, globals, locals, v))), ...metadata)
  }
}

function scopeCheckStmt (errors: ScamperError[], builtinLibs: Map<string, L.Library>, globals: string[], v: Value): Value {
  if (A.isImport(v)) {
    const { name: ident, metadata } = A.asImport(v)
    const { name, metadata: nm } = A.asIdentifier(ident)
    if (!builtinLibs.has(name)) {
      errors.push(new ScamperError('Parser', `Library '${name}' is not defined`, undefined, metadata.get('range')))
    } else {
      const lib = builtinLibs.get(name)!
      for (const [name, _v] of lib.lib) {
        if (globals.includes(name)) {
          // TODO: push a warning noting that shadowing is occuring!
        } else {
          globals.push(name)
        }
      }
    }
    return v
  } else if (A.isDefine(v)) {
    const { name, value, metadata } = A.asDefine(v)
    if (globals.includes(A.stripSyntax(name) as string)) {
      errors.push(new ScamperError('Parser',  `Global variable '${name}' is already defined`, undefined, metadata.get('range')))
    } else {
      globals.push(A.stripSyntax(name) as string)
    }
    const body = scopeCheckExpr(errors, globals, [], value)
    return A.mkSyntax(L.mkList(L.mkSym('define'), name, body), ...metadata)
  } else if (A.isDisplay(v)) {
    const { value, metadata } = A.asDisplay(v)
    const body = scopeCheckExpr(errors, globals, [], value)
    return A.mkSyntax(L.mkList(L.mkSym('display'), body), ...metadata)
  } else if (A.isStruct(v)) {
    const { name, fields, metadata: _metadata } = A.asStruct(v)
    const { name: nameValue, metadata: nameMetadata } = A.asIdentifier(name)
    // Constructor
    if (globals.includes(nameValue)) {
      errors.push(new ScamperError('Parser',  `Global variable '${nameValue}' is already defined`, undefined, nameMetadata.get('range')))
    } else {
      globals.push(nameValue)
    }
    // Predicate
    const pred = A.structPredName(nameValue)
    if (globals.includes(pred)) {
      errors.push(new ScamperError('Parser',  `Global variable '${pred}' is already defined`, undefined, nameMetadata.get('range')))
    } else {
      globals.push(pred)
    }
    // Fields
    fields.forEach((f) => {
      const { name: fieldName, metadata: fieldMetadata } = A.asIdentifier(f)
      const getter = A.structFieldName(nameValue, fieldName)
      if (globals.includes(getter)) {
        errors.push(new ScamperError('Parser', `Global variable '${getter}' is already defined`, undefined, fieldMetadata.get('range')))
      } else {
        globals.push(getter)
      }
    })
    return v
  } else {
    return scopeCheckExpr(errors, globals, [], v)
  }
}

export function scopeCheckProgram (builtinLibs: Map<string, L.Library>, vs: Value[]): { program: Value[], errors: ScamperError[] } {
  const errors: ScamperError[] = []
  const globals: string[] = []
  const program: Value[] = []
  for (const v of vs) {
    program.push(scopeCheckStmt(errors, builtinLibs, globals, v))
  }
  return { program, errors }
}