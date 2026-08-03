import * as AST from './ast'

export function sugarExpr(e: AST.Exp): AST.Exp {
  switch (e.tag) {
    case 'lit': {
      return e
    }
    case 'id': {
      return e
    }
    case 'app': {
      return AST.mkApp(sugarExpr(e.head), e.args.map(sugarExpr))
    }
    case 'lam': {
      return AST.mkLam(e.params, sugarExpr(e.body), undefined, e.restParam)
    }
    case 'let': {
      return AST.mkLet(
        e.bindings.map(({ pat, value }) => ({
          pat,
          value: sugarExpr(value),
        })),
        sugarExpr(e.body),
      )
    }
    case 'begin': {
      return AST.mkBegin(e.exps.map(sugarExpr))
    }
    case 'if': {
      return AST.mkIf(sugarExpr(e.guard), sugarExpr(e.ifB), sugarExpr(e.elseB))
    }
    case 'match': {
      // Let binding: a single-branch match is exactly `(let ([pat scrutinee])
      // body)` -- both bind the pattern and error on a non-match -- so any
      // single-branch match sugars back to a let, whatever the pattern.
      if (e.branches.length === 1) {
        return AST.mkLet(
          [{ pat: e.branches[0].pat, value: sugarExpr(e.scrutinee) }],
          sugarExpr(e.branches[0].body),
        )
      }

      // If branch
      if (e.branches.length === 2) {
        const [first, second] = e.branches
        if (
          first.pat.tag === 'plit' &&
          first.pat.value === true &&
          second.pat.tag === 'plit' &&
          second.pat.value === false
        ) {
          return AST.mkIf(
            sugarExpr(e.scrutinee),
            sugarExpr(first.body),
            sugarExpr(second.body),
          )
        }
      }

      // Default case
      return e
    }
    case 'quote': {
      return e
    }
    case 'let*': {
      return AST.mkLetS(
        e.bindings.map(({ pat, value }) => ({
          pat,
          value: sugarExpr(value),
        })),
        sugarExpr(e.body),
      )
    }
    case 'and': {
      return AST.mkAnd(e.exps.map(sugarExpr))
    }
    case 'or': {
      return AST.mkOr(e.exps.map(sugarExpr))
    }
    case 'cond': {
      return AST.mkCond(
        e.branches.map(({ test, body }) => ({
          test: sugarExpr(test),
          body: sugarExpr(body),
        })),
      )
    }
    case 'section': {
      return AST.mkSection(e.exps.map(sugarExpr))
    }
  }
}
