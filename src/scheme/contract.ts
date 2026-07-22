import { Range } from "../lpm"
import * as A from "./ast.js"
import { parseFunctionDocFromComments, Pred } from "./docstring/docstring.js"
import { Param } from "./docstring/param.js"

// Adapted from the (now-removed) LPM-bytecode version of this idea in
// src/lpm/contract/util.ts: that version generated the check directly as
// LPM ops, referencing the wrapped function by name (`U.mkVar(fn)`) from
// inside its own replacement -- which, once spliced back in under that same
// name, would have recursed into itself instead of the original. Doing this
// as a source-to-source AST pass instead lets the wrapper close over the
// original value via an ordinary `let` before the `define` shadows it.

const contractTargetName = "##contract-target##"

/**
 * A short, human-readable description of a predicate, suitable for embedding
 * in an "expected ..." contract violation message, e.g. `number?` ~> `a
 * number`, `integer?` ~> `an integer`. Complex predicates (`(list-of
 * number?)`) don't reduce to a single word, so they're rendered as-is.
 */
function describePred(pred: Pred): string {
  if (pred.tag !== "var") {
    return `a value matching \`${A.expToString(pred)}\``
  }
  const name = pred.name.endsWith("?") ? pred.name.slice(0, -1) : pred.name
  const article = /^[aeiou]/i.test(name) ? "an" : "a"
  return `${article} ${name}`
}

/**
 * Builds `(string-append "expected " descPred ", received " (##typeOf## argVar))`.
 * Written in terms of ordinary prelude/runtime calls (rather than a host
 * closure) so the result is ordinary Scamper source.
 */
function mkErrorMsg(descPred: string, argVar: string, range: Range): A.Exp {
  return A.mkApp(
    A.mkVar("string-append", range),
    [
      A.mkLit("expected ", range),
      A.mkLit(descPred, range),
      A.mkLit(", received ", range),
      A.mkApp(A.mkVar("##typeOf##", range), [A.mkVar(argVar, range)], range),
    ],
    range,
  )
}

/**
 * Builds `(string-append "expected every value of restVar to be " descPred)`
 * -- the rest-parameter analog of mkErrorMsg. There's no single culprit
 * value to report a "received ..." clause for (all-satisfy? only reports
 * pass/fail, not which element failed), so this is intentionally simpler.
 */
function mkRestErrorMsg(descPred: string, restVar: string, range: Range): A.Exp {
  return A.mkApp(
    A.mkVar("string-append", range),
    [
      A.mkLit(`expected every value of ${restVar} to be `, range),
      A.mkLit(descPred, range),
      A.mkLit(", but at least one was not", range),
    ],
    range,
  )
}

/**
 * Builds the call to the (let-bound) original function once every check has
 * passed. For a fixed-arity function this is just `(##contract-target##
 * x1 ... xk)`. When there's also a rest parameter, the fixed args and the
 * rest list have to be combined into a single list first (via nested cons,
 * innermost-out) and passed through `apply`, since Ap's bytecode always has
 * a compile-time-fixed argument count -- there's no way to statically emit
 * "call with N args" when N (the rest list's length) is only known at
 * runtime.
 */
function mkTargetCall(
  params: Param[],
  restParam: Param | undefined,
  range: Range,
): A.Exp {
  if (!restParam) {
    return A.mkApp(
      A.mkVar(contractTargetName, range),
      params.map((p) => A.mkVar(p.name, range)),
      range,
    )
  }
  const combined = params.reduceRight<A.Exp>(
    (acc, p) => A.mkApp(A.mkVar("cons", range), [A.mkVar(p.name, range), acc], range),
    A.mkVar(restParam.name, range),
  )
  return A.mkApply(A.mkVar(contractTargetName, range), combined, range)
}

/**
 * Builds a cascading if-chain that checks each param against its documented
 * predicate before invoking the (let-bound) original function, e.g. for
 * params `[{name: "x", predicate: number?}, {name: "y", predicate: string?}]`:
 *
 *   (if (number? x)
 *       (if (string? y)
 *           (##contract-target## x y)
 *           (error "expected a string, received ..."))
 *       (error "expected a number, received ..."))
 *
 * A predicate is applied as `(predicate argVar)` regardless of whether it's
 * a simple identifier (`number?`) or a derived predicate application
 * (`(list-of number?)`) -- both are valid expressions in operator position,
 * so no special-casing is needed between the two.
 *
 * If a rest parameter is present, one more check is appended after all the
 * fixed params': `(all-satisfy? restPred restVar)`.
 */
function mkCheckChain(
  params: Param[],
  restParam: Param | undefined,
  range: Range,
): A.Exp {
  const targetCall = mkTargetCall(params, restParam, range)

  const restCheck: A.Exp = restParam
    ? A.mkIf(
        A.mkApp(
          A.mkVar("all-satisfy?", range),
          [restParam.predicate, A.mkVar(restParam.name, range)],
          range,
        ),
        targetCall,
        A.mkError(
          mkRestErrorMsg(describePred(restParam.predicate), restParam.name, range),
          range,
        ),
        range,
      )
    : targetCall

  const checkAt = (i: number): A.Exp => {
    if (i === params.length) {
      return restCheck
    }
    const { name, predicate } = params[i]
    return A.mkIf(
      A.mkApp(predicate, [A.mkVar(name, range)], range),
      checkAt(i + 1),
      A.mkError(mkErrorMsg(describePred(predicate), name, range), range),
      range,
    )
  }
  return checkAt(0)
}

/**
 * Wraps a define's value in a contract check extracted from its docstring:
 *
 *   (define name expr)
 *
 * becomes
 *
 *   (define name
 *     (let ([##contract-target## expr])
 *       (lambda (x1 ... xk [. rest])
 *         <cascading predicate checks, then (##contract-target## x1 ... xk)>)))
 *
 * @returns the statement unchanged if it isn't a define, has no docstring,
 *          the docstring fails to parse (a documentation-quality issue, not
 *          a reason to fail compiling -- see ast.ts's Define.docComments),
 *          or documents neither fixed params nor a rest param. The last case
 *          is deliberately left alone rather than wrapped in a zero-arg
 *          checking thunk: a zero-param docstring is also how a documented
 *          *constant* (e.g. `pi`) is written, and there's no way to tell the
 *          two apart from the parsed docstring alone -- wrapping a constant
 *          this way would silently turn it into a function that must be
 *          called. A rest-only signature (`(+ . xs)`) can't be confused with
 *          a constant's, though (constants never have a dot), so that case
 *          is still wrapped even with zero fixed params.
 */
export function contractStmt(s: A.Stmt): A.Stmt {
  if (s.tag !== "define" || !s.docComments) {
    return s
  }
  let doc
  try {
    doc = parseFunctionDocFromComments(s.docComments)
  } catch {
    return s
  }
  if (!doc || (doc.params.length === 0 && !doc.restParam)) {
    return s
  }
  const wrapped = A.mkLet(
    [{ name: contractTargetName, value: s.value }],
    A.mkLam(
      doc.params.map((p) => p.name),
      mkCheckChain(doc.params, doc.restParam, s.range),
      s.range,
      doc.restParam?.name,
      true,
    ),
    s.range,
  )
  return A.mkDefine(s.name, wrapped, s.range, s.docComments)
}

/** Applies contractStmt to every statement in a program. */
export function contractProgram(prog: A.Prog): A.Prog {
  return prog.map(contractStmt)
}
