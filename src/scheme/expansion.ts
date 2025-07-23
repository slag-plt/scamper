import { Value } from '../lpm/runtime.js'
import * as R from '../lpm/runtime.js'
import * as A from './ast.js'

function expandExpr (v: Value): Value {
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

  } else if (A.isAnd(v)) {
    const { values, range } = A.asAnd(v)
    let expr: Value = true
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(A.rangeOf(b), R.mkList(R.mkSym('if'), b, expr, R.mkSym('false')), ['desugared', 'and']) 
    }
    return A.mkSyntax(range, expr)
  } else if (A.isOr(v)) {
    const { values, range } = A.asOr(v)
    let expr: Value = false
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(A.rangeOf(b), R.mkList(R.mkSym('if'), b, R.mkSym('true'), expr), ['desugared', 'or'])
    }
    return A.mkSyntax(range, expr)
  } else if (A.isBegin(v)) {
    const { values, range } = A.asBegin(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('begin'), ...values.map(expandExpr)))
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, range } = A.asIf(v)
    return A.mkSyntax(range, R.mkList(R.mkSym('if'), expandExpr(guard), expandExpr(ifB), expandExpr(elseB)))
  } else if (A.isCond(v)) {
    let { clauses, range } = A.asCond(v)
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
    // TODO: need to portion section-expanding code to here!
    throw new R.ICE('expandExpr', 'Section expressions are not yet supported in the expansion phase')
  } else {
    const { values, range } = A.asApp(v)
    return A.mkSyntax(range, R.mkList(...values.map(expandExpr)))
  }
}

function expandStmt (v: Value): Value{
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