import * as A from './ast.js'
import * as L from '../lpm/index.js'

function lowerSingle (v: L.Value): L.Blk {
  const { value, metadata: _metadata } = A.unpackSyntax(v)
  if (L.isSym(value)) {
    return [L.mkVar(value.value)]
  } else {
    return [L.mkLit(value)]
  }
}

function lowerPat (v: L.Value): L.Pat {
  if (A.isAtom(v)) {
    const { value, metadata: _metadata } = A.unpackSyntax(v)
    if (L.isSym(value)) {
      if (value.value === '_') {
        return L.mkPWild()
      } else {
        return L.mkPVar(value.value)
      }
    } else {
      return L.mkPLit(value)
    }
  } else {
    const { values, metadata: _metadata } = A.asApp(v)
    const name = (values[0] as L.Sym).value
    const args = values.slice(1).map(lowerPat)
    return L.mkPCtor(name, args)
  }
}

function lowerExpr (v: L.Value): L.Blk {
  if (A.isAtom(v)) {
    return lowerSingle(v)
  } else if (A.isLambda(v)) {
    const { params, body, metadata: _metadata } = A.asLambda(v)
    const blk = lowerExpr(body)
    return [...blk, L.mkCls(params.map((v) => A.asIdentifier(v).name), blk, '##anonymous##')]
  } else if (A.isLet(v)) {
    const { bindings, body, metadata: _metadata } = A.asLet(v)
    let ret = lowerExpr(body)
    for (let i = bindings.length - 1; i >= 0; i--) {
      const { fst, snd } = bindings[i]
      const name = A.asIdentifier(fst).name
      const blk = lowerExpr(snd)
      ret = [...blk, L.mkMatch([[L.mkPVar(name), ret]])]
    }
    return ret
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, metadata: _metadata } = A.asIf(v)
    return [...lowerExpr(guard), L.mkMatch([
      [L.mkPLit(true), lowerExpr(ifB)],
      [L.mkPLit(false), lowerExpr(elseB)]
    ])]
  } else if (A.isMatch(v)) {
    const { scrutinee, clauses, metadata: _metadata } = A.asMatch(v)
    return [
      ...lowerExpr(scrutinee),
      L.mkMatch(clauses.map(({ fst, snd }) => [lowerPat(fst), lowerExpr(snd)] as [L.Pat, L.Blk]))
    ]
  } else if (A.isQuote(v)) {
    const { value, metadata: _metadata } = A.asQuote(v)
    return [L.mkLit(value)]
  } else {
    // N.B., function application
    const { values, metadata: _metadata } = A.asApp(v)
    const blk = []
    for (const arg of values) {
      blk.push(...lowerExpr(arg))
    }
    blk.push(L.mkAp(values.length - 1))
    return blk
  }
}

function lowerStmt (v: L.Value): L.Blk {
  if (A.isImport(v)) {
    const { name, metadata: _metadata } = A.asImport(v)
    return [L.mkImport(A.nameFromIdentifier(name))]
  } else if (A.isDefine(v)) {
    const { name, value, metadata: _metadata } = A.asDefine(v)
    return [...lowerExpr(value), L.mkDefine(A.nameFromIdentifier(name))]
  } else if (A.isDisplay(v)) {
    const { value, metadata: _metadata } = A.asDisplay(v)
    return [...lowerExpr(value), L.mkDisp()]
  } else {
    // N.B., statement-expression case
    return [...lowerExpr(v), L.mkPopv()]
  }
}

export function lowerProgram (v: L.Value[]): L.Blk {
  return v.flatMap(lowerStmt)
}