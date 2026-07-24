import { Loc, Range, ScamperError } from '../lpm'
import * as A from './ast.js'

/**
 * @returns prog with the statement at [queryLoc] having its deepest queried
 *          sub-expression wrapped in a Report expression, plus the range of
 *          that inner reported expression
 * @throws ScamperError if there is no valid statement to report at [queryLoc]
 */
export function getQueriedProgram(
  prog: A.Prog,
  queryLoc: Loc,
): { prog: A.Prog; range: Range } {
  const queriedI = prog.findIndex((stmt) => stmt.range.contains(queryLoc))
  if (queriedI < 0) {
    throw new ScamperError(
      'Parser',
      `Received invalid query location: ${queryLoc.toString()}`,
    )
  }
  const { stmt, range } = getReportedStmt(prog[queriedI], queryLoc)
  return {
    prog: prog.map((s, i) => (i === queriedI ? stmt : s)),
    range,
  }
}

/**
 * Precondition: stmt.range contains queryLoc
 */
export function getReportedStmt(
  stmt: A.Stmt,
  queryLoc: Loc,
): { stmt: A.Stmt; range: Range } {
  switch (stmt.tag) {
    case 'define': {
      const inner = getReportedExp(stmt.value, queryLoc)
      return {
        stmt: A.mkDefine(stmt.name, inner.exp, stmt.range, stmt.docComments),
        range: inner.range,
      }
    }
    case 'display': {
      const inner = getReportedExp(stmt.value, queryLoc)
      return { stmt: A.mkDisp(inner.exp, stmt.range), range: inner.range }
    }
    case 'stmtexp': {
      const inner = getReportedExp(stmt.expr, queryLoc)
      return { stmt: A.mkStmtExp(inner.exp, stmt.range), range: inner.range }
    }
    case 'import':
    case 'struct':
      // N.B., these have no expression content to report on. In practice
      // this is unreachable from the UI: the caller (tokenizeAndParse) only
      // ever acts on the result when the queried statement is a `define`
      // with a docstring, so querying an import/struct statement is always
      // rejected downstream regardless of what's returned here.
      return { stmt, range: stmt.range }
  }
}

/**
 * The ordered list of an expression's immediate sub-expression "slots" --
 * each one pairs the child expression with a function that rebuilds the
 * parent with that one child replaced. Used by getReportedExp to find the
 * most specific sub-expression containing the query location.
 */
interface Slot {
  exp: A.Exp
  rebuild: (replacement: A.Exp) => A.Exp
}

function slotsOf(exp: A.Exp): Slot[] {
  switch (exp.tag) {
    case 'lit':
    case 'id':
    case 'quote':
    case 'jsvar':
      return []

    case 'error':
      return [{ exp: exp.exp, rebuild: (r) => A.mkError(r, exp.range) }]

    case 'apply':
      return [
        { exp: exp.fn, rebuild: (r) => A.mkApply(r, exp.args, exp.range) },
        { exp: exp.args, rebuild: (r) => A.mkApply(exp.fn, r, exp.range) },
      ]

    case 'app':
      return [
        { exp: exp.head, rebuild: (r) => A.mkApp(r, exp.args, exp.range) },
        ...exp.args.map((a, i) => ({
          exp: a,
          rebuild: (r: A.Exp) =>
            A.mkApp(
              exp.head,
              exp.args.map((x, j) => (j === i ? r : x)),
              exp.range,
            ),
        })),
      ]

    case 'lam':
      return [
        {
          exp: exp.body,
          rebuild: (r) => A.mkLam(exp.params, r, exp.range, exp.restParam),
        },
      ]

    case 'if':
      return [
        {
          exp: exp.guard,
          rebuild: (r) => A.mkIf(r, exp.ifB, exp.elseB, exp.range),
        },
        {
          exp: exp.ifB,
          rebuild: (r) => A.mkIf(exp.guard, r, exp.elseB, exp.range),
        },
        {
          exp: exp.elseB,
          rebuild: (r) => A.mkIf(exp.guard, exp.ifB, r, exp.range),
        },
      ]

    // "and"/"or"/"begin"/"section" all share the same shape (a flat list of
    // sub-expressions), differing only in which constructor rebuilds them.
    case 'and':
    case 'or':
    case 'begin':
    case 'section': {
      const mk = {
        and: A.mkAnd,
        or: A.mkOr,
        begin: A.mkBegin,
        section: A.mkSection,
      }[exp.tag]
      return exp.exps.map((e, i) => ({
        exp: e,
        rebuild: (r: A.Exp) =>
          mk(
            exp.exps.map((x, j) => (j === i ? r : x)),
            exp.range,
          ),
      }))
    }

    case 'let':
    case 'let*': {
      const mk = exp.tag === 'let' ? A.mkLet : A.mkLetS
      return [
        ...exp.bindings.map((b, i) => ({
          exp: b.value,
          rebuild: (r: A.Exp) =>
            mk(
              exp.bindings.map((x, j) =>
                j === i ? { id: x.id, value: r } : x,
              ),
              exp.body,
              exp.range,
            ),
        })),
        {
          exp: exp.body,
          rebuild: (r: A.Exp) => mk(exp.bindings, r, exp.range),
        },
      ]
    }

    case 'cond':
      return exp.branches.flatMap((b, i) => [
        {
          exp: b.test,
          rebuild: (r: A.Exp) =>
            A.mkCond(
              exp.branches.map((x, j) =>
                j === i ? { test: r, body: x.body } : x,
              ),
              exp.range,
            ),
        },
        {
          exp: b.body,
          rebuild: (r: A.Exp) =>
            A.mkCond(
              exp.branches.map((x, j) =>
                j === i ? { test: x.test, body: r } : x,
              ),
              exp.range,
            ),
        },
      ])

    case 'match':
      return [
        {
          exp: exp.scrutinee,
          rebuild: (r) => A.mkMatch(r, exp.branches, exp.range),
        },
        // N.B., patterns aren't reportable expressions -- only branch bodies
        // are considered here, matching the fact that a match branch's
        // pattern is never itself evaluated at runtime.
        ...exp.branches.map((b, i) => ({
          exp: b.body,
          rebuild: (r: A.Exp) =>
            A.mkMatch(
              exp.scrutinee,
              exp.branches.map((x, j) =>
                j === i ? { pat: x.pat, body: r } : x,
              ),
              exp.range,
            ),
        })),
      ]

    case 'report':
      return [{ exp: exp.exp, rebuild: (r) => A.mkReport(r, exp.range) }]
  }
}

/**
 * Precondition: exp.range contains queryLoc
 */
export function getReportedExp(
  exp: A.Exp,
  queryLoc: Loc,
): { exp: A.Exp; range: Range } {
  for (const slot of slotsOf(exp)) {
    if (slot.exp.range.contains(queryLoc)) {
      const inner = getReportedExp(slot.exp, queryLoc)
      return { exp: slot.rebuild(inner.exp), range: inner.range }
    }
  }
  // N.B., none of this expression's sub-expression slots contain the query
  // location, but the expression itself does (by precondition) -- so the
  // query must have landed on syntax that belongs to this node itself (a
  // keyword, a bracket, or a leaf with no children at all). Wrap the whole
  // thing.
  return { exp: A.mkReport(exp, exp.range), range: exp.range }
}
