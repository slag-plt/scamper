import { Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import * as A from './ast.js'

let holeSymCounter = 0
function genHoleSym(): string {
  return `_${holeSymCounter++}`
}

function collectSectionHoles (bvars: string[], v: Value): Value {
  const orig = v
  let { range, value } = A.unpackSyntax(v)
  v = value
  if (R.isSymName(A.stripSyntax(v), '_')) {
    const x = genHoleSym()
    bvars.push(x)
    return A.mkSyntax(range, R.mkSym(x))
  } else if (v === null) {
    return orig
  } else if (R.isList(v)) {
    const values = R.listToVector(v)
    // N.B., do _not_ recursively collect holes in enclosed section forms
    if (R.isSymName(A.stripSyntax(values[0]), 'section')) {
      return orig
    } else {
      return A.mkSyntax(range, R.mkList(...values.map((v) => collectSectionHoles(bvars, v))))
    }
  } else if (R.isPair(v)) {
    return A.mkSyntax(range, R.mkPair(
      collectSectionHoles(bvars, (v as R.Pair).fst),
      collectSectionHoles(bvars, (v as R.Pair).snd)))
  } else if (R.isArray(v)) {
    return A.mkSyntax(range, (v as R.Vector).map((v) => collectSectionHoles(bvars, v)))
  } else {
    return orig
  }
}

export function expandExpr (v: Value): Value {
  if (A.isAtom(v)) {
    return v
  } else if (A.isLambda(v)) {
    const { params, body, range } = A.asLambda(v)
    return A.mkSyntax(range, R.mkList(params, expandExpr(body)))
  } else if (A.isLet(v)) {
    let { bindings, body, range } = A.asLet(v)
    bindings = bindings.map(({fst, snd, range }) => ({ fst, snd: expandExpr(snd), range }))
    body = expandExpr(body)
    return A.mkSyntax(range, R.mkList(R.mkSym('let'), R.mkList(...bindings), body))
  } else if (A.isLetStar(v)) {
    let { bindings, body, range: _range } = A.asLetStar(v)
    bindings = bindings.map(({fst, snd, range }) => ({ fst, snd: expandExpr(snd), range }))
    body = expandExpr(body)
    let expr: Value = body
    for (let i = bindings.length - 1; i >= 0; i--) {
      let { fst, snd, range: r } = bindings[i]
      expr = A.mkSyntax(r, R.mkList(R.mkSym('let'), fst, snd, expr), ['desugared', 'let*'])
    }
    // TODO: same deal as cond: the range of the top-level let is the first binding, is that ok?
    return expr
  } else if (A.isAnd(v)) {
    const { values, range } = A.asAnd(v)
    let expr: Value = true
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(A.rangeOf(b), R.mkList(R.mkSym('if'), b, expr, false), ['desugared', 'and']) 
    }
    return A.mkSyntax(range, expr)
  } else if (A.isOr(v)) {
    const { values, range } = A.asOr(v)
    let expr: Value = false
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(A.rangeOf(b), R.mkList(R.mkSym('if'), b, true, expr), ['desugared', 'or'])
    }
    return A.mkSyntax(range, expr)
  } else if (A.isBegin(v)) {
    const { values, range } = A.asBegin(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('begin'), ...values.map(expandExpr)))
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, range } = A.asIf(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('if'), expandExpr(guard), expandExpr(ifB), expandExpr(elseB)))
  } else if (A.isCond(v)) {
    let { clauses, range: _range } = A.asCond(v)
    clauses = clauses.map(({ fst, snd, range }) => ({ fst: expandExpr(fst), snd: expandExpr(snd), range }))
    let expr: Value = undefined
    for (let i = clauses.length - 1; i >= 0; i--) {
      expr = A.mkSyntax(clauses[i].range, R.mkList(R.mkSym('if'), clauses[i].fst, clauses[i].snd, expr), ['desugared', 'cond'])
    }
    // N.B., the range of the top if is the range of the first clause and not the overall cond.
    // Will that be ok for error reporting?
    return expr
  } else if (A.isMatch(v)) {
    const { clauses, range } = A.asMatch(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('match'), ...clauses.map(({ fst, snd, range }) => ({
      fst,
      snd: expandExpr(snd),
      range: range
    }))))
  } else if (A.isQuote(v)) {
    const { values, range } = A.asQuote(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('quote'), ...values))
  } else if (A.isSection(v)) {
    const { values, range } = A.asSection(v)
    const params: string[] = []
    const app = R.mkList(...values.map((arg) => collectSectionHoles(params, arg)))
    return A.mkSyntax(range,
        R.mkList(
          R.mkSym('lambda'), R.mkList(...params.map((p) => R.mkSym(p))), app),
        ['desugared', 'section']
    )
  } else {
    const { values, range } = A.asApp(v)
    return A.mkSyntax(range, R.mkList(...values.map(expandExpr)))
  }
}

export function expandStmt (v: Value): Value{
  if (A.isImport(v)) {
    return v
  } else if (A.isDefine(v)) {
    const { name, value, range } = A.asDefine(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('define'), name, expandExpr(value)))
  } else if (A.isDisplay(v)) {
    const { value, range } = A.asDisplay(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('display'), expandExpr(value)))
  } else if (A.isStruct(v)) {
    return v
  } else {
    return expandExpr(v)
  }
}

export function scopeCheckProgram (vs: Value[]): Value[] {
  return vs.map(expandStmt)
}