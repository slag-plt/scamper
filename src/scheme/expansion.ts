import { Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import * as A from './ast.js'

let holeSymCounter = 0
function genHoleSym(): string {
  return `_${holeSymCounter++}`
}

function collectSectionHoles (bvars: string[], v: Value): Value {
  const orig = v
  let { value, metadata } = A.unpackSyntax(v)
  v = value
  if (R.isSymName(A.stripSyntax(v), '_')) {
    const x = genHoleSym()
    bvars.push(x)
    return A.mkSyntax(R.mkSym(x), ...metadata)
  } else if (v === null) {
    return orig
  } else if (R.isList(v)) {
    const values = R.listToVector(v)
    // N.B., do _not_ recursively collect holes in enclosed section forms
    if (R.isSymName(A.stripSyntax(values[0]), 'section')) {
      return orig
    } else {
      return A.mkSyntax(R.mkList(...values.map((v) => collectSectionHoles(bvars, v))), ...metadata)
    }
  } else if (R.isPair(v)) {
    return A.mkSyntax(R.mkPair(
      collectSectionHoles(bvars, (v as R.Pair).fst),
      collectSectionHoles(bvars, (v as R.Pair).snd)), ...metadata)
  } else if (R.isArray(v)) {
    return A.mkSyntax((v as R.Vector).map((v) => collectSectionHoles(bvars, v)), ...metadata)
  } else {
    return orig
  }
}

export function expandExpr (v: Value): Value {
  if (A.isAtom(v)) {
    return v
  } else if (A.isLambda(v)) {
    const { params, body, metadata } = A.asLambda(v)
    return A.mkSyntax(R.mkList(params, expandExpr(body)), ...metadata)
  } else if (A.isLet(v)) {
    let { bindings, body, metadata } = A.asLet(v)
    bindings = bindings.map(({fst, snd, metadata }) => ({ fst, snd: expandExpr(snd), metadata }))
    body = expandExpr(body)
    return A.mkSyntax(R.mkList(R.mkSym('let'), R.mkList(...bindings), body), ...metadata)
  } else if (A.isLetStar(v)) {
    // (let* ([x1 e1] ... [xk ek]) e)
    // --> (let ([x1 e1]) (let ([x2 e2]) ... (let ([xk ek]) e) ...))
    let { bindings, body, metadata: _metadata } = A.asLetStar(v)
    bindings = bindings.map(({fst, snd, metadata }) => ({ fst, snd: expandExpr(snd), metadata }))
    body = expandExpr(body)
    let expr: Value = body
    for (let i = bindings.length - 1; i >= 0; i--) {
      let { fst, snd, metadata } = bindings[i]
      expr = A.mkSyntax(R.mkList(R.mkSym('let'), fst, snd, expr), ...metadata, ['desugared', 'let*'])
    }
    // TODO: same deal as cond: the range of the top-level let is the first binding, is that ok?
    return expr
  } else if (A.isAnd(v)) {
    // (and e1 ... ek)
    // ---> (if e1 (if e2 ... (if ek true false) ... false) false)
    const { values, metadata } = A.asAnd(v)
    let expr: Value = true
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(R.mkList(R.mkSym('if'), b, expr, false), ...metadata, ['desugared', 'and'])
    }
    return A.mkSyntax(expr, ...metadata)
  } else if (A.isOr(v)) {
    // (or e1 ... ek)
    // ---> (if e1 true (if e2 ... (if ek true false)
    const { values, metadata } = A.asOr(v)
    let expr: Value = false
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(R.mkList(R.mkSym('if'), b, true, expr), ...metadata, ['desugared', 'or'])
    }
    return A.mkSyntax(expr, ...metadata)
  } else if (A.isBegin(v)) {
    // (begin e)
    // --> e
    // (begin e1 ... ek)
    // --> (let ([_ e1]) ([_ e2]) ... ([_ ek-1]) ek)
    const { values, metadata } = A.asBegin(v)
    return A.mkSyntax(R.mkList(R.mkSym('begin'), ...values.map(expandExpr)), ...metadata)
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, metadata } = A.asIf(v)
    return A.mkSyntax(R.mkList(R.mkSym('if'), expandExpr(guard), expandExpr(ifB), expandExpr(elseB)), ...metadata)
  } else if (A.isCond(v)) {
    // (cond [e11 e12] ... [ek1 ek2])
    // --> (if e11 e12 (if e21 e22 ... (if ek1 e2k void) ...))
    let { clauses, metadata: _metadata } = A.asCond(v)
    clauses = clauses.map(({ fst, snd, metadata }) => ({ fst: expandExpr(fst), snd: expandExpr(snd), metadata }))
    let expr: Value = undefined
    for (let i = clauses.length - 1; i >= 0; i--) {
      expr = A.mkSyntax(R.mkList(R.mkSym('if'), clauses[i].fst, clauses[i].snd, expr), ...clauses[i].metadata, ['desugared', 'cond'])
    }
    // N.B., the range of the top if is the range of the first clause and not the overall cond.
    // Will that be ok for error reporting?
    return expr
  } else if (A.isMatch(v)) {
    const { clauses, metadata } = A.asMatch(v)
    return A.mkSyntax(R.mkList(R.mkSym('match'), ...clauses.map(({ fst, snd, metadata }) => ({
      fst,
      snd: expandExpr(snd),
      metadata
    }))), ...metadata)
  } else if (A.isQuote(v)) {
    const { value, metadata } = A.asQuote(v)
    return A.mkSyntax(R.mkList(R.mkSym('quote'), value), ...metadata)
  } else if (A.isSection(v)) {
    const { values, metadata } = A.asSection(v)
    const params: string[] = []
    const app = R.mkList(...values.map((arg) => collectSectionHoles(params, arg)))
    return A.mkSyntax(
        R.mkList(
          R.mkSym('lambda'), R.mkList(...params.map((p) => R.mkSym(p))), app),
        ['desugared', 'section'],
        ...metadata
    )
  } else {
    const { values, metadata } = A.asApp(v)
    return A.mkSyntax(R.mkList(...values.map(expandExpr)), ...metadata)
  }
}

export function expandStmt (v: Value): Value{
  if (A.isImport(v)) {
    return v
  } else if (A.isDefine(v)) {
    const { name, value, metadata } = A.asDefine(v)
    return A.mkSyntax(R.mkList(R.mkSym('define'), name, expandExpr(value)), ...metadata)
  } else if (A.isDisplay(v)) {
    const { value, metadata } = A.asDisplay(v)
    return A.mkSyntax(R.mkList(R.mkSym('display'), expandExpr(value)), ...metadata)
  } else if (A.isStruct(v)) {
    // TODO: Problems! At this point, we need to generate code for the various
    // struct functions. What can we do?
    // 1. Add ops to be able to perform the struct operations and define the
    //    struct functions directly in terms of the ops.
    // 2. Add library function(s) to perform the struct functions in terms of
    //    the names introduced by the struct.
    // Actually, yeah, that'll work! If we expose the struct-creation functions
    // in the library, then we can desugar as follows:
    //
    // (struct S (f1 ... fk))
    // -->
    // (define S (make-struct-constructor S (list f1 ... fk)))
    // (define S? (make-struct-predicate S))
    // (define S-f1 (lambda (e) (e[f1])))
    // ...
    // (define S-fk (lambda (e) (e[fk])))
    return v
  } else {
    return expandExpr(v)
  }
}

export function scopeCheckProgram (vs: Value[]): Value[] {
  return vs.map(expandStmt)
}