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
      // N.B., this was solved by copilot! Because let-bindings, by default, do not telescope,
      // we must proceed by first evaluating all binding expressions (without binding), then
      // bind the variables, and finally evaluate the body.
      //
      // This behavior _really_ makes me want to embrace Clojure-style Scheme more and more
      // where let telescopes by default, i.e., is let*. But we support traditional Scheme
      // behavior for now.
      let bindings = e.bindings.flatMap((b) => lowerExpr(b.value))

      let ret = lowerExpr(e.body)
      // We must ensure that the inner-most match corresponds to the _first_ binding since
      // we're building the matches inside-out.
      // for (let i = e.bindings.length - 1; i >= 0; i--) {
      for (let i = 0; i < e.bindings.length; i++) {
        ret = [L.mkMatch([
          [L.mkPVar(e.bindings[i].name, e.range), ret]
        ])]
      }
      return [...bindings, ...ret]
    }
    case 'begin': {
      const last = lowerExpr(e.exps[e.exps.length - 1])
      const exps = e.exps.slice(0, e.exps.length - 1).flatMap((e) => [...lowerExpr(e), L.mkPopv()])
      return [...exps, ...last]
    }
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

function lowerStmt (s: A.Stmt, displayStmtExpr: boolean = true): L.Stmt {
  switch (s.tag) {
    case 'import': return L.mkImport(s.module, s.range)
    case 'define': return L.mkDefine(s.name, lowerExpr(s.value), s.range)
    case 'display': return L.mkDisp(lowerExpr(s.value), s.range)
    case 'stmtexp': return displayStmtExpr ?
      L.mkDisp(lowerExpr(s.expr), s.range) :
      L.mkStmtExp(lowerExpr(s.expr), s.range)
    default: throw new L.ICE('lowerStmt', `Unknown expected statement type: ${s.tag}`)
  }
}

export function lowerProgram (prog: A.Prog, displayStmtExpr: boolean = true): L.Prog {
  return prog.map((s) => lowerStmt(s, displayStmtExpr))
}