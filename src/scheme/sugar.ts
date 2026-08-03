import * as A from './ast.js'

// Sugaring is the dual of expansion (expansion.ts): an AST -> AST rewrite that
// recovers the derived forms `and`, `or`, `begin`, and `cond` from the core
// `if`/`let` shapes they expand to. `section` is intentionally not recovered.
//
// Recovery is inherently a heuristic -- several source forms expand to the same
// core shape (e.g. both `(and e)` and `(or e)` expand to `(if e #t #f)`) -- so a
// derived form is recovered only where its expansion is unambiguous:
//   - and/or: at least two operands (the single-operand shape is ambiguous)
//   - cond:   the chain ends in cond's exact fall-through `error` sentinel
//   - begin:  a single wildcard-binding let
// Each derived form's operands are collected from the raw core shape and then
// sugared *recursively*, so a derived form nested inside a recovered one -- in
// an operand, a branch, or the body -- is itself recovered rather than lost.

/** @return true iff `e` is the boolean literal `value`. */
function isBool(e: A.Exp, value: boolean): boolean {
  return e.tag === 'lit' && e.value === value
}

/** The exact fall-through call `cond` expands to when no clause matches. */
const COND_SENTINEL_MESSAGE = 'No matching clause in cond'
function isCondSentinel(e: A.Exp): boolean {
  return (
    e.tag === 'app' &&
    e.head.tag === 'id' &&
    e.head.name === 'error' &&
    e.args.length === 1 &&
    e.args[0].tag === 'lit' &&
    e.args[0].value === COND_SENTINEL_MESSAGE
  )
}

/**
 * @return the (raw) operands of an `and` if `e` is its expansion, else null.
 * `(and e1 ... ek)` expands to a right-nested `(if ei <rest> #f)` chain whose
 * base is `#t`.
 */
function collectAnd(e: A.Exp): A.Exp[] | null {
  if (isBool(e, true)) return []
  if (e.tag === 'if' && isBool(e.elseB, false)) {
    const rest = collectAnd(e.ifB)
    if (rest !== null) return [e.guard, ...rest]
  }
  return null
}

/**
 * @return the (raw) operands of an `or` if `e` is its expansion, else null.
 * `(or e1 ... ek)` expands to a right-nested `(if ei #t <rest>)` chain whose
 * base is `#f`.
 */
function collectOr(e: A.Exp): A.Exp[] | null {
  if (isBool(e, false)) return []
  if (e.tag === 'if' && isBool(e.ifB, true)) {
    const rest = collectOr(e.elseB)
    if (rest !== null) return [e.guard, ...rest]
  }
  return null
}

/**
 * @return the (raw) branches of a `cond` if `e` is its expansion, else null.
 * `(cond [t1 b1] ... [tk bk])` expands to a right-nested `(if ti bi <rest>)`
 * chain whose base is the fall-through error sentinel.
 */
function collectCond(e: A.Exp): { test: A.Exp; body: A.Exp }[] | null {
  if (isCondSentinel(e)) return []
  if (e.tag === 'if') {
    const rest = collectCond(e.elseB)
    if (rest !== null) return [{ test: e.guard, body: e.ifB }, ...rest]
  }
  return null
}

/**
 * @return the (raw) sequenced expressions of a `begin` if `e` is its expansion
 * -- a single wildcard-binding let chain -- else null. Always yields at least
 * two expressions (the discarded value plus the body).
 */
function flattenBegin(e: A.Exp): A.Exp[] | null {
  if (
    e.tag === 'let' &&
    e.bindings.length === 1 &&
    e.bindings[0].pat.tag === 'pwild'
  ) {
    return [e.bindings[0].value, ...(flattenBegin(e.body) ?? [e.body])]
  }
  return null
}

export function sugarExpr(e: A.Exp): A.Exp {
  switch (e.tag) {
    case 'lit':
    case 'id':
    case 'quote':
      return e

    case 'app':
      return A.mkApp(sugarExpr(e.head), e.args.map(sugarExpr), e.range)

    case 'lam':
      return A.mkLam(e.params, sugarExpr(e.body), e.range, e.restParam)

    case 'if': {
      // Recognize and/or/cond from the *raw* if-shape first, then sugar the
      // collected operands so any nested derived forms are recovered too.
      const and = collectAnd(e)
      if (and !== null && and.length >= 2) {
        return A.mkAnd(and.map(sugarExpr), e.range)
      }
      const or = collectOr(e)
      if (or !== null && or.length >= 2) {
        return A.mkOr(or.map(sugarExpr), e.range)
      }
      const cond = collectCond(e)
      if (cond !== null && cond.length >= 1) {
        return A.mkCond(
          cond.map((b) => ({
            test: sugarExpr(b.test),
            body: sugarExpr(b.body),
          })),
          e.range,
        )
      }
      // A plain `if`: still sugar its parts so nested derived forms survive.
      return A.mkIf(
        sugarExpr(e.guard),
        sugarExpr(e.ifB),
        sugarExpr(e.elseB),
        e.range,
      )
    }

    case 'let': {
      // A single wildcard-binding let is a `begin`.
      const begin = flattenBegin(e)
      if (begin !== null) {
        return A.mkBegin(begin.map(sugarExpr), e.range)
      }
      return A.mkLet(
        e.bindings.map((b) => ({ pat: b.pat, value: sugarExpr(b.value) })),
        sugarExpr(e.body),
        e.range,
      )
    }

    case 'match':
      return A.mkMatch(
        sugarExpr(e.scrutinee),
        e.branches.map((b) => ({ pat: b.pat, body: sugarExpr(b.body) })),
        e.range,
      )

    // Derived forms are not produced by expansion, but a caller may sugar a
    // non-core AST; recurse so their sub-expressions are still handled.
    case 'begin':
      return A.mkBegin(e.exps.map(sugarExpr), e.range)
    case 'and':
      return A.mkAnd(e.exps.map(sugarExpr), e.range)
    case 'or':
      return A.mkOr(e.exps.map(sugarExpr), e.range)
    case 'cond':
      return A.mkCond(
        e.branches.map((b) => ({
          test: sugarExpr(b.test),
          body: sugarExpr(b.body),
        })),
        e.range,
      )
    case 'section':
      return A.mkSection(e.exps.map(sugarExpr), e.range)
  }
}

export function sugarStmt(s: A.Stmt): A.Stmt {
  switch (s.tag) {
    case 'import':
    case 'struct':
      return s
    case 'define':
      return A.mkDefine(s.name, sugarExpr(s.value), s.range, s.docComments)
    case 'display':
      return A.mkDisp(sugarExpr(s.value), s.range)
    case 'stmtexp':
      return A.mkStmtExp(sugarExpr(s.expr), s.range)
  }
}

export function sugarProgram(prog: A.Prog): A.Prog {
  return prog.map(sugarStmt)
}
