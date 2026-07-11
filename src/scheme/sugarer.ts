import * as AST from "./ast"

export function sugarExpr(e: AST.Exp): AST.Exp {
  return sugarExprWithAppMetadata(e, new Map()).expr
}

/**
 * Sugars an expression while carrying metadata attached to application nodes.
 *
 * Page raising uses this to retain navigation targets after a core `match`
 * becomes an `if` or `let`. Other callers can use `sugarExpr` as before.
 */
export function sugarExprWithAppMetadata<T>(
  e: AST.Exp,
  appMetadata: ReadonlyMap<AST.App, T>,
): { expr: AST.Exp; appMetadata: Map<AST.App, T> } {
  const rewrittenApps = new Map<AST.App, AST.App>()

  const sugar = (exp: AST.Exp): AST.Exp => {
    switch (exp.tag) {
      case "lit": {
        return exp
      }
      case "var": {
        return exp
      }
      case "app": {
        const app = AST.mkApp(sugar(exp.head), exp.args.map(sugar))
        rewrittenApps.set(exp, app)
        return app
      }
      case "lam": {
        return AST.mkLam(exp.params, sugar(exp.body))
      }
      case "let": {
        return AST.mkLet(
          exp.bindings.map(({ name, value }) => ({
            name,
            value: sugar(value),
          })),
          sugar(exp.body),
        )
      }
      case "begin": {
        return AST.mkBegin(exp.exps.map(sugar))
      }
      case "if": {
        return AST.mkIf(sugar(exp.guard), sugar(exp.ifB), sugar(exp.elseB))
      }
      case "match": {
        // Let binding
        if (exp.branches.length === 1 && exp.branches[0].pat.tag === "pvar") {
          return AST.mkLet(
            [{ name: exp.branches[0].pat.name, value: sugar(exp.scrutinee) }],
            sugar(exp.branches[0].body),
          )
        }

        // If branch
        if (exp.branches.length === 2) {
          const [first, second] = exp.branches
          if (
            first.pat.tag === "plit" &&
            first.pat.value === true &&
            second.pat.tag === "plit" &&
            second.pat.value === false
          ) {
            return AST.mkIf(
              sugar(exp.scrutinee),
              sugar(first.body),
              sugar(second.body),
            )
          }
        }

        // Default case
        return exp
      }
      case "quote": {
        return exp
      }
      case "let*": {
        return AST.mkLetS(
          exp.bindings.map(({ name, value }) => ({
            name,
            value: sugar(value),
          })),
          sugar(exp.body),
        )
      }
      case "and": {
        return AST.mkAnd(exp.exps.map(sugar))
      }
      case "or": {
        return AST.mkOr(exp.exps.map(sugar))
      }
      case "cond": {
        return AST.mkCond(
          exp.branches.map(({ test, body }) => ({
            test: sugar(test),
            body: sugar(body),
          })),
        )
      }
      case "section": {
        return AST.mkSection(exp.exps.map(sugar))
      }
      case "report": {
        return AST.mkReport(sugar(exp.exp))
      }
    }
  }

  const expr = sugar(e)
  const sugaredMetadata = new Map<AST.App, T>()
  for (const [app, metadata] of appMetadata) {
    sugaredMetadata.set(rewrittenApps.get(app) ?? app, metadata)
  }
  return { expr, appMetadata: sugaredMetadata }
}
