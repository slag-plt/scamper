import * as A from './ast.js'
import * as L from '../lpm/index.js'

function lowerPat (p: A.Pat): L.Pat {
  switch (p.tag) {
    case 'pwild': return L.mkPWild(p.range)
    case 'pvar': return L.mkPVar(p.name, p.range)
    case 'plit': return L.mkPLit(p.value, p.range)
    case 'pctor': return L.mkPCtor(p.name, p.args.map(lowerPat), p.range)
  }
}

function lowerExpr (e: A.Exp): L.Blk {
  switch (e.tag) {
    case 'lit': return [L.mkLit(e.value, e.range)]
    case 'var': return [L.mkVar(e.name, e.range)]
    case 'app': return [
      ...lowerExpr(e.head),
      ...e.args.flatMap(lowerExpr),
      L.mkAp(e.args.length, e.range)
    ]
    case 'lam': return [
      L.mkCls(e.params, lowerExpr(e.body), '##anonymous##', e.range)
    ]
    case 'let': {
      let ret = lowerExpr(e.body)
      for (let i = e.bindings.length - 1; i >= 0; i--) {
        const blk = lowerExpr(e.bindings[i].value)
        ret = [...blk, L.mkMatch([
          [L.mkPVar(e.bindings[i].name, e.range), ret]
        ])]
      }
      return ret
    }
    case 'begin':
      return e.exps.flatMap((e) => [...lowerExpr(e), L.mkPopv()])
    case 'if': return [
      ...lowerExpr(e.guard), L.mkMatch([
        [L.mkPLit(true), lowerExpr(e.ifB)],
        [L.mkPLit(false), lowerExpr(e.elseB)]
      ], e.range)
    ]
    case 'match': return [
      ...lowerExpr(e.scrutinee),
      L.mkMatch(e.branches.map(({ pat, body }) => [lowerPat(pat), lowerExpr(body)] as [L.Pat, L.Blk]), e.range)
    ]
    case 'quote': return [L.mkLit(e.value, e.range)]
    default:
      throw new L.ICE('lowerExpr', `Non-core expression encountered: ${e.tag}`)
  }

}

function lowerStmt (s: A.Stmt, displayStmtExpr: boolean = true): L.Blk {
  switch (s.tag) {
    case 'import':
      return [L.mkImport(s.module, s.range)]
    case 'define':
      return [...lowerExpr(s.value), L.mkDefine(s.name, s.range)]
    case 'display':
      return [...lowerExpr(s.value), L.mkDisp(s.range)]
    case 'stmtexp':
      return [...lowerExpr(s.expr), displayStmtExpr ? L.mkDisp() : L.mkPopv()]
    default:
      throw new L.ICE('lowerStmt', `Non-core statement encountered: ${s.tag}`)
  }
}

export function lowerProgram (prog: A.Prog, displayStmtExpr: boolean = true): L.Blk {
  const ret = prog.flatMap((s) => lowerStmt(s, displayStmtExpr))
  // N.B., the main block must return a value to successfully exit
  ret.push(L.mkLit(0))
  return ret
}