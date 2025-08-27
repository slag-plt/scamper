import * as A from './ast.js'
import * as L from '../lpm/index.js'

function lowerSingle (v: L.Value): L.Blk {
  const { value, metadata: metadata } = A.unpackSyntax(v)
  if (L.isSym(value)) {
    return [L.mkVar(value.value, metadata.get('range'))]
  } else {
    return [L.mkLit(value, metadata.get('range'))]
  }
}

function lowerPat (v: L.Value): L.Pat {
  if (A.isAtom(v)) {
    const { value, metadata: metadata } = A.unpackSyntax(v)
    if (L.isSym(value)) {
      if (value.value === '_') {
        return L.mkPWild(metadata.get('range'))
      } else {
        return L.mkPVar(value.value, metadata.get('range'))
      }
    } else {
      return L.mkPLit(value, metadata.get('range'))
    }
  } else {
    const { values, metadata: metadata } = A.asApp(v)
    const name = (values[0] as L.Sym).value
    const args = values.slice(1).map(lowerPat)
    return L.mkPCtor(name, args, metadata.get('range'))
  }
}

function lowerExpr (v: L.Value): L.Blk {
  if (A.isAtom(v)) {
    return lowerSingle(v)
  } else if (A.isLambda(v)) {
    const { params, body, metadata: metadata } = A.asLambda(v)
    const blk = lowerExpr(body)
    return [L.mkCls(params.map((v) => A.asIdentifier(v).name), blk, '##anonymous##', metadata.get('range'))]
  } else if (A.isLet(v)) {
    const { bindings, body, metadata: metadata } = A.asLet(v)
    let ret = lowerExpr(body)
    for (let i = bindings.length - 1; i >= 0; i--) {
      const { fst, snd } = bindings[i]
      const { name, metadata: nameMetadata } = A.asIdentifier(fst)
      const blk = lowerExpr(snd)
      ret = [...blk, L.mkMatch([[L.mkPVar(name, nameMetadata.get('range')), ret]], metadata.get('range'))]
    }
    return ret
  } else if (A.isIf(v)) {
    const { guard, ifB, elseB, metadata: metadata } = A.asIf(v)
    return [...lowerExpr(guard), L.mkMatch([
      [L.mkPLit(true), lowerExpr(ifB)],
      [L.mkPLit(false), lowerExpr(elseB)]
    ], metadata.get('range'))]
  } else if (A.isMatch(v)) {
    const { scrutinee, clauses, metadata: metadata } = A.asMatch(v)
    return [
      ...lowerExpr(scrutinee),
      L.mkMatch(clauses.map(({ fst, snd }) => [lowerPat(fst), lowerExpr(snd)] as [L.Pat, L.Blk]), metadata.get('range'))
    ]
  } else if (A.isQuote(v)) {
    const { value, metadata: metadata } = A.asQuote(v)
    return [L.mkLit(value, metadata.get('range'))]
  } else {
    // N.B., function application
    const { values, metadata: metadata } = A.asApp(v)
    const blk = []
    for (const arg of values) {
      blk.push(...lowerExpr(arg))
    }
    blk.push(L.mkAp(values.length - 1, metadata.get('range')))
    return blk
  }
}

function lowerStmt (v: L.Value, displayStmtExpr: boolean = true): L.Blk {
  if (A.isImport(v)) {
    const { name, metadata: metadata } = A.asImport(v)
    return [L.mkImport(A.nameFromIdentifier(name), metadata.get('range'))]
  } else if (A.isDefine(v)) {
    const { name, value, metadata: metadata } = A.asDefine(v)
    return [...lowerExpr(value), L.mkDefine(A.nameFromIdentifier(name), metadata.get('range'))]
  } else if (A.isDisplay(v)) {
    const { value, metadata: metadata } = A.asDisplay(v)
    return [...lowerExpr(value), L.mkDisp(metadata.get('range'))]
  } else {
    // N.B., statement-expression case
    return [...lowerExpr(v), displayStmtExpr ? L.mkDisp() : L.mkPopv()]
  }
}

export function lowerProgram (v: L.Value[], displayStmtExpr: boolean = true): L.Blk {
  const ret = v.flatMap((e) => lowerStmt(e, displayStmtExpr))
  // N.B., the main block must return a value to successfully exit
  ret.push(L.mkLit(0))
  return ret
}