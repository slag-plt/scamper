import * as A from './ast.js'

// Sugaring is the dual of expansion (expansion.ts): a recursive AST -> AST walk
// (sugarExpr / sugarStmt / sugarProgram) that recovers the derived forms `and`,
// `or`, `begin`, and `cond` from the core `if`/`let` shapes they expand to.
// `section` is intentionally not recovered.
//
// Recovery is *exact*, not heuristic. Expansion tags every node it inserts with
// a `provenance` (see L.Provenance), and that tag is threaded through the LPM op
// and restored on raise, so a node is recovered only when it genuinely came from
// expanding that derived form. A hand-written `(if e #t #f)`, or a
// `(let ([_ e]) body)`, or an `(error "...")` -- none of which carry a
// provenance -- is left untouched, so there is no over-sugaring.
//
// Each form's operands are collected from the tagged chain and then sugared
// *recursively*, so a derived form nested inside a recovered one -- in an
// operand, a branch, or the body -- is itself recovered rather than lost.

/**
 * @return the operands of an `and` if `e` is (the head of) its expansion, else
 * null. `(and e1 ... ek)` expands to a `provenance:'and'`-tagged right-nested
 * `(if ei <rest> #f)` chain whose base is a tagged `#t`.
 */
function collectAnd(e: A.Exp): A.Exp[] | null {
  if (e.provenance !== 'and') return null
  if (e.tag === 'lit' && e.value === true) return [] // base of the chain
  if (e.tag === 'if') {
    const rest = collectAnd(e.ifB)
    if (rest !== null) return [e.guard, ...rest]
  }
  return null
}

/**
 * @return the operands of an `or` if `e` is (the head of) its expansion, else
 * null. `(or e1 ... ek)` expands to a `provenance:'or'`-tagged right-nested
 * `(if ei #t <rest>)` chain whose base is a tagged `#f`.
 */
function collectOr(e: A.Exp): A.Exp[] | null {
  if (e.provenance !== 'or') return null
  if (e.tag === 'lit' && e.value === false) return [] // base of the chain
  if (e.tag === 'if') {
    const rest = collectOr(e.elseB)
    if (rest !== null) return [e.guard, ...rest]
  }
  return null
}

/**
 * @return the branches of a `cond` if `e` is (the head of) its expansion, else
 * null. `(cond [t1 b1] ... [tk bk])` expands to a `provenance:'cond'`-tagged
 * right-nested `(if ti bi <rest>)` chain whose base is the tagged fall-through
 * error sentinel (the only app expansion inserts).
 */
function collectCond(e: A.Exp): { test: A.Exp; body: A.Exp }[] | null {
  if (e.provenance !== 'cond') return null
  if (e.tag === 'app') return [] // the fall-through error sentinel
  if (e.tag === 'if') {
    const rest = collectCond(e.elseB)
    if (rest !== null) return [{ test: e.guard, body: e.ifB }, ...rest]
  }
  return null
}

/**
 * @return the sequenced expressions of a `begin` if `e` is its expansion -- a
 * `provenance:'begin'`-tagged single wildcard-binding let chain -- else null.
 */
function flattenBegin(e: A.Exp): A.Exp[] | null {
  if (
    e.provenance === 'begin' &&
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
      // A tagged `if` is the head of exactly one of and/or/cond; recover it and
      // sugar the collected operands so nested derived forms are recovered too.
      const and = collectAnd(e)
      if (and !== null) return A.mkAnd(and.map(sugarExpr), e.range)
      const or = collectOr(e)
      if (or !== null) return A.mkOr(or.map(sugarExpr), e.range)
      const cond = collectCond(e)
      if (cond !== null) {
        return A.mkCond(
          cond.map((b) => ({
            test: sugarExpr(b.test),
            body: sugarExpr(b.body),
          })),
          e.range,
        )
      }
      // An untagged `if`: still sugar its parts so nested derived forms survive.
      return A.mkIf(
        sugarExpr(e.guard),
        sugarExpr(e.ifB),
        sugarExpr(e.elseB),
        e.range,
      )
    }

    case 'let': {
      // A tagged single wildcard-binding let is a `begin`.
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
