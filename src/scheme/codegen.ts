import * as A from './ast.js'
import * as L from '../lpm/index.js'

function lowerPat(p: A.Pat): L.Pat {
  switch (p.tag) {
    case 'pwild':
      return L.mkPWild(p.range)
    case 'id':
      return L.mkPVar(p.name, p.range)
    case 'plit':
      return L.mkPLit(p.value, p.range)
    case 'pctor':
      return L.mkPCtor(p.name.name, p.args.map(lowerPat), p.range)
  }
}

function lowerExpr(e: A.Exp): L.Blk {
  switch (e.tag) {
    case 'lit':
      return [L.mkLit(e.value, e.range, e.provenance)]
    case 'id':
      return [L.mkVar(e.name, e.range)]
    case 'app':
      // Internal spread-application `(##ap-spread## fn argList)`, emitted by
      // contract.ts for rest-parameter targets. Lowered to an *inline*
      // ap-spread (not the user-facing `apply` closure) so a raised error is
      // attributed to the enclosing contract-wrapper frame rather than to a
      // synthetic `apply` frame. Never produced by the reader.
      if (e.head.tag === 'id' && e.head.name === '##ap-spread##') {
        return [
          ...lowerExpr(e.args[0]),
          ...lowerExpr(e.args[1]),
          L.mkApSpread(e.range),
        ]
      }
      return [
        ...lowerExpr(e.head),
        ...e.args.flatMap(lowerExpr),
        L.mkAp(e.args.length, e.range, e.provenance),
      ]
    case 'lam':
      return [L.mkCls(e.params.map((p) => p.name), lowerExpr(e.body), '##anonymous##', e.range, e.restParam?.name, e.provenance)]
    case 'let': {
      // letrec: every binder shares one scope (declared as holes); the `let`
      // op evaluates each value sub-block left-to-right, filling holes as it
      // goes, then runs the body. The trailing `pop-scope` discards the scope.
      // A value that fails to match its pattern raises a binding-flavored error.
      const bindings = e.bindings.map((b) => ({
        pat: lowerPat(b.pat),
        value: lowerExpr(b.value),
        failMsg: `let: value did not match pattern ${A.patToString(b.pat)}`,
      }))
      return [
        L.mkLet(bindings, lowerExpr(e.body), e.range, 0, e.provenance),
        L.mkPopScope(e.range),
      ]
    }
    case 'if':
      return [
        ...lowerExpr(e.guard),
        L.mkIf(lowerExpr(e.ifB), lowerExpr(e.elseB), e.range, e.provenance),
      ]
    case 'match':
      // A matched branch binds its pattern variables in a fresh scope; the
      // trailing `pop-scope` discards them once the branch body completes.
      return [
        ...lowerExpr(e.scrutinee),
        L.mkMatch(
          e.branches.map(
            ({ pat, body }) =>
              [lowerPat(pat), lowerExpr(body)] as [L.Pat, L.Blk],
          ),
          e.range,
        ),
        L.mkPopScope(e.range),
      ]
    case 'quote':
      return [L.mkLit(e.value, e.range)]
    default:
      throw new L.ICE('lowerExpr', `Non-core expression encountered: ${e.tag}`)
  }
}

function lowerStmt(s: A.Stmt, displayStmtExpr = true): L.Stmt {
  switch (s.tag) {
    case 'import':
      return L.mkImport(s.module, s.kind, s.range)
    case 'define':
      // N.B., docComments is documentation metadata, not executable code --
      // it deliberately doesn't carry forward into the lowered bytecode.
      return L.mkDefine(s.name.name, lowerExpr(s.value), s.range)
    case 'display':
      return L.mkDisp(lowerExpr(s.value), s.range)
    case 'stmtexp':
      return displayStmtExpr
        ? L.mkDisp(lowerExpr(s.expr), s.range)
        : L.mkStmtExp(lowerExpr(s.expr), s.range)
    default:
      throw new L.ICE('lowerStmt', `Unknown expected statement type: ${s.tag}`)
  }
}

export function lowerProgram(prog: A.Prog, displayStmtExpr = true): L.Prog {
  return prog.map((s) => lowerStmt(s, displayStmtExpr))
}
