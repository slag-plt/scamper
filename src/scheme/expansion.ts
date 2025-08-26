import * as L from '../lpm'
import * as A from './ast.js'

let holeSymCounter = 0
function genHoleSym(): string {
  return `_${holeSymCounter++}`
}

function collectSectionHoles (bvars: string[], v: L.Value): L.Value {
  const orig = v
  let { value, metadata } = A.unpackSyntax(v)
  v = value
  if (L.isSymName(A.stripSyntax(v), '_')) {
    const x = genHoleSym()
    bvars.push(x)
    return A.mkSyntax(L.mkSym(x), ...metadata)
  } else if (v === null) {
    return orig
  } else if (L.isList(v)) {
    const values = L.listToVector(v)
    // N.B., do _not_ recursively collect holes in enclosed section forms
    if (L.isSymName(A.stripSyntax(values[0]), 'section')) {
      return orig
    } else {
      return A.mkSyntax(L.mkList(...values.map((v) => collectSectionHoles(bvars, v))), ...metadata)
    }
  } else if (L.isPair(v)) {
    return A.mkSyntax(L.mkPair(
      collectSectionHoles(bvars, (v as L.Pair).fst),
      collectSectionHoles(bvars, (v as L.Pair).snd)), ...metadata)
  } else if (L.isArray(v)) {
    return A.mkSyntax((v as L.Vector).map((v) => collectSectionHoles(bvars, v)), ...metadata)
  } else {
    return orig
  }
}

export function expandExpr (v: L.Value): L.Value {
  if (A.isAtom(v)) {
    return v
  } else if (A.isLambda(v)) {
    const { params, body, metadata } = A.asLambda(v)
    return A.mkSyntax(L.mkList(L.mkSym('lambda'), L.vectorToList(params), expandExpr(body)), ...metadata)
  } else if (A.isLet(v)) {
    let { bindings, body, metadata } = A.asLet(v)
    bindings = bindings.map(({fst, snd, metadata }) => ({ fst, snd: expandExpr(snd), metadata }))
    body = expandExpr(body)
    return A.mkSyntax(L.mkList(L.mkSym('let'), L.mkList(...bindings), body), ...metadata)
  } else if (A.isLetStar(v)) {
    // (let* ([x1 e1] ... [xk ek]) e)
    // --> (let ([x1 e1]) (let ([x2 e2]) ... (let ([xk ek]) e) ...))
    let { bindings, body, metadata: _metadata } = A.asLetStar(v)
    bindings = bindings.map(({fst, snd, metadata }) => ({ fst, snd: expandExpr(snd), metadata }))
    body = expandExpr(body)
    let expr: L.Value = body
    for (let i = bindings.length - 1; i >= 0; i--) {
      let { fst, snd, metadata } = bindings[i]
      expr = A.mkSyntax(L.mkList(L.mkSym('let'), fst, snd, expr), ...metadata, ['desugared', 'let*'])
    }
    // TODO: same deal as cond: the range of the top-level let is the first binding, is that ok?
    return expr
  } else if (A.isAnd(v)) {
    // (and e1 ... ek)
    // ---> (if e1 (if e2 ... (if ek true false) ... false) false)
    const { values, metadata } = A.asAnd(v)
    let expr: L.Value = true
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(L.mkList(L.mkSym('if'), b, expr, false), ...metadata, ['desugared', 'and'])
    }
    return A.mkSyntax(expr, ...metadata)
  } else if (A.isOr(v)) {
    // (or e1 ... ek)
    // ---> (if e1 true (if e2 ... (if ek true false)
    const { values, metadata } = A.asOr(v)
    let expr: L.Value = false
    for (let i = values.length - 1; i >= 0; i--) {
      let b = values[i]
      expr = A.mkSyntax(L.mkList(L.mkSym('if'), b, true, expr), ...metadata, ['desugared', 'or'])
    }
    return A.mkSyntax(expr, ...metadata)
  } else if (A.isBegin(v)) {
    // (begin e)
    // --> e
    // (begin e1 ... ek)
    // --> (let ([_ e1]) ([_ e2]) ... ([_ ek-1]) ek)
    const { values, metadata } = A.asBegin(v)
    let expr: L.Value = values[values.length - 1]
    for (let i = values.length - 2; i >= 0; i--) {
      expr = A.mkSyntax(L.mkList(L.mkSym('let'), L.mkList(L.mkSym('_'), values[i]), expr), ...metadata, ['desugared', 'begin'])
    }
    return expr
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, metadata } = A.asIf(v)
    return A.mkSyntax(L.mkList(L.mkSym('if'), expandExpr(guard), expandExpr(ifB), expandExpr(elseB)), ...metadata)
  } else if (A.isCond(v)) {
    // (cond [e11 e12] ... [ek1 ek2])
    // --> (if e11 e12 (if e21 e22 ... (if ek1 e2k void) ...))
    let { clauses, metadata: _metadata } = A.asCond(v)
    clauses = clauses.map(({ fst, snd, metadata }) => ({ fst: expandExpr(fst), snd: expandExpr(snd), metadata }))
    let expr: L.Value = undefined
    for (let i = clauses.length - 1; i >= 0; i--) {
      expr = A.mkSyntax(L.mkList(L.mkSym('if'), clauses[i].fst, clauses[i].snd, expr), ...clauses[i].metadata, ['desugared', 'cond'])
    }
    // N.B., the range of the top if is the range of the first clause and not the overall cond.
    // Will that be ok for error reporting?
    return expr
  } else if (A.isMatch(v)) {
    const { clauses, metadata } = A.asMatch(v)
    return A.mkSyntax(L.mkList(L.mkSym('match'), ...clauses.map(({ fst, snd, metadata }) => ({
      fst,
      snd: expandExpr(snd),
      metadata
    }))), ...metadata)
  } else if (A.isQuote(v)) {
    const { value, metadata } = A.asQuote(v)
    return A.mkSyntax(L.mkList(L.mkSym('quote'), value), ...metadata)
  } else if (A.isSection(v)) {
    const { values, metadata } = A.asSection(v)
    const params: string[] = []
    const app = L.mkList(...values.map((arg) => collectSectionHoles(params, arg)))
    return A.mkSyntax(
        L.mkList(
          L.mkSym('lambda'), L.mkList(...params.map((p) => L.mkSym(p))), app),
        ['desugared', 'section'],
        ...metadata
    )
  } else {
    const { values, metadata } = A.asApp(v)
    return A.mkSyntax(L.mkList(...values.map(expandExpr)), ...metadata)
  }
}

export function expandStmt (v: L.Value): L.Value[] {
  if (A.isImport(v)) {
    return [v]
  } else if (A.isDefine(v)) {
    const { name, value, metadata } = A.asDefine(v)
    return [A.mkSyntax(L.mkList(L.mkSym('define'), name, expandExpr(value)), ...metadata)]
  } else if (A.isDisplay(v)) {
    const { value, metadata } = A.asDisplay(v)
    return [A.mkSyntax(L.mkList(L.mkSym('display'), expandExpr(value)), ...metadata)]
  } else if (A.isStruct(v)) {
    // (struct S (f1 ... fk))
    // -->
    // (define S (##mkCtorFn## S f1 ... fk))
    // (define S? (##mkPredFn## S))
    // (define S-f1 (##mkGetFn## S f1))
    // ...
    // (define S-fk (##mkGetFn## S fk))
    const { name: ident, fields, metadata } = A.asStruct(v)
    const name = A.nameFromIdentifier(ident)
    const ctor = A.mkSyntax(L.mkList(L.mkSym('define'), name,
      L.mkList(
        L.mkSym('##mkCtorFn##'),
        L.mkSym(name),
        fields.map(A.nameFromIdentifier))),
      ['desugared', 'struct'], ['struct', name], ...metadata)
    const pred = A.mkSyntax(L.mkList(L.mkSym('define'), L.mkSym(A.structPredName(name)),
        L.mkList(L.mkSym('##mkPredFn##'), L.mkSym(name))),
      ['desugared', 'struct'], ['struct', name], ...metadata)
    const accessors = fields.map((ident) => {
      const fielName = A.nameFromIdentifier(ident)
      return A.mkSyntax(L.mkList(L.mkSym('define'), L.mkSym(A.structFieldName(name, fielName)),
        L.mkList(L.mkSym('##mkGetFn##'), L.mkSym(name), L.mkSym(fielName))),
        ['desugared', 'struct'], ['struct', name], ...metadata)
    })
    return [ctor, pred, ...accessors]
  } else {
    return [expandExpr(v)]
  }
}

export function expandProgram (vs: L.Value[]): L.Value[] {
  return vs.flatMap(expandStmt)
}