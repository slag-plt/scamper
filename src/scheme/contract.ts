import { Range } from '../lpm'
import * as A from './ast.js'
import { parseFunctionDocFromComments, Pred } from './docstring/docstring.js'
import { Param } from './docstring/param.js'

// Adapted from the (now-removed) LPM-bytecode version of this idea in
// src/lpm/contract/util.ts: that version generated the check directly as
// LPM ops, referencing the wrapped function by name (`U.mkVar(fn)`) from
// inside its own replacement -- which, once spliced back in under that same
// name, would have recursed into itself instead of the original. Doing this
// as a source-to-source AST pass instead lets the wrapper close over the
// original value via an ordinary `let` before the `define` shadows it.

const contractTargetName = '##contract-target##'
const contractedName = '##contracted##'

/**
 * The bare name of a simple `var` predicate with any trailing `?` stripped,
 * e.g. `pair?` ~> "pair", `nonempty-list?` ~> "nonempty-list". Used to render
 * the disjuncts of an `(or/p ...)` predicate without an article.
 */
function predBareName(pred: A.Identifier): string {
  return pred.name.endsWith('?') ? pred.name.slice(0, -1) : pred.name
}

/**
 * A short, human-readable description of a predicate, suitable for embedding
 * in an "expected ..." contract violation message, e.g. `number?` ~> `a
 * number`, `integer?` ~> `an integer`. An `(or/p p1 ... pk)` predicate over
 * simple `var` disjuncts renders as `p1, ..., or pk` (Oxford join, no
 * leading article), e.g. `(or/p pair? nonempty-list?)` ~> "pair or
 * nonempty-list". Other complex predicates (`(list-of number?)`) don't reduce
 * to a single word, so they're rendered as-is.
 */
function describePred(pred: Pred): string {
  if (pred.tag !== 'id') {
    const args = pred.args
    if (pred.head.name === 'or/p' && args.length > 0 && args.every(A.isVar)) {
      const names = args.map(predBareName)
      if (names.length === 1) {
        return names[0]
      }
      if (names.length === 2) {
        return `${names[0]} or ${names[1]}`
      }
      return `${names.slice(0, -1).join(', ')}, or ${names[names.length - 1]}`
    }
    return `a value matching \`${A.expToString(pred)}\``
  }
  const name = predBareName(pred)
  const article = /^[aeiou]/i.test(name) ? 'an' : 'a'
  return `${article} ${name}`
}

/**
 * Builds `(string-append "expected " descPred ", received " (##typeOf## argVar))`.
 * Written in terms of ordinary prelude/runtime calls (rather than a host
 * closure) so the result is ordinary Scamper source.
 */
function mkErrorMsg(descPred: string, argVar: string, range: Range): A.Exp {
  return A.mkApp(
    A.mkId('string-append', range),
    [
      A.mkLit('expected ', range),
      A.mkLit(descPred, range),
      A.mkLit(', received ', range),
      A.mkApp(A.mkId('##typeOf##', range), [A.mkId(argVar, range)], range),
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
    A.mkId('string-append', range),
    [
      A.mkLit(`expected every value of ${restVar} to be `, range),
      A.mkLit(descPred, range),
      A.mkLit(', but at least one was not', range),
    ],
    range,
  )
}

/**
 * Builds the call to the (let-bound) original function once every check has
 * passed. For a fixed-arity function this is just `(##contract-target##
 * x1 ... xk)`. When there's also a rest parameter, the fixed args and the
 * rest list have to be combined into a single list first (via nested cons,
 * innermost-out) and spread into the call via an inline `ap-spread` (the
 * internal `##ap-spread##` form; see codegen), since Ap's bytecode always has
 * a compile-time-fixed argument count -- there's no way to statically emit
 * "call with N args" when N (the rest list's length) is only known at
 * runtime. An inline ap-spread keeps a raised error attributed to this
 * contract-wrapper frame rather than to the user-facing `apply` closure.
 */
function mkTargetCall(
  params: Param[],
  restParam: Param | undefined,
  range: Range,
): A.Exp {
  if (!restParam) {
    return A.mkApp(
      A.mkId(contractTargetName, range),
      params.map((p) => A.mkId(p.name, range)),
      range,
    )
  }
  const combined = params.reduceRight<A.Exp>(
    (acc, p) =>
      A.mkApp(A.mkId('cons', range), [A.mkId(p.name, range), acc], range),
    A.mkId(restParam.name, range),
  )
  return A.mkApp(
    A.mkId('##ap-spread##', range),
    [A.mkId(contractTargetName, range), combined],
    range,
  )
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
 * The optional params' checks follow the fixed params', each skipped when its
 * argument is void -- the caller left it out. If a rest parameter is present,
 * one more check is appended after those: `(all-satisfy? restPred restVar)`.
 */
function mkCheckChain(
  params: Param[],
  optParams: Param[],
  restParam: Param | undefined,
  range: Range,
): A.Exp {
  const targetCall = mkTargetCall([...params, ...optParams], restParam, range)

  const restCheck: A.Exp = restParam
    ? A.mkIf(
        A.mkApp(
          A.mkId('all-satisfy?', range),
          [restParam.predicate, A.mkId(restParam.name, range)],
          range,
        ),
        targetCall,
        A.mkApp(
          A.mkId('##error##', range),
          [mkRestErrorMsg(describePred(restParam.predicate), restParam.name, range)],
          range,
        ),
        range,
      )
    : targetCall

  // An optional parameter is void when the caller left it out, and a value
  // that has to satisfy the predicate when they did not. Nesting the two
  // tests as `(if (if (##voidQ## x) #t (pred x)) ...)` keeps the continuation
  // written once, where `or` over the pair would duplicate it per parameter.
  const optChecks = optParams.reduceRight<(next: A.Exp) => A.Exp>(
    (rest, { name, predicate }) =>
      (next) =>
        A.mkIf(
          A.mkIf(
            A.mkApp(A.mkId(voidQName, range), [A.mkId(name, range)], range),
            A.mkLit(true, range),
            A.mkApp(predicate, [A.mkId(name, range)], range),
            range,
          ),
          rest(next),
          A.mkApp(
            A.mkId('##error##', range),
            [mkErrorMsg(describePred(predicate), name, range)],
            range,
          ),
          range,
        ),
    (next) => next,
  )

  const checkAt = (i: number): A.Exp => {
    if (i === params.length) {
      return optChecks(restCheck)
    }
    const { name, predicate } = params[i]
    return A.mkIf(
      A.mkApp(predicate, [A.mkId(name, range)], range),
      checkAt(i + 1),
      A.mkApp(
        A.mkId('##error##', range),
        [mkErrorMsg(describePred(predicate), name, range)],
        range,
      ),
      range,
    )
  }
  return checkAt(0)
}

/**
 * The internal bindings the generated wrapper uses to take the optional
 * parameters off its own rest parameter. They are internal, and separate from
 * the prelude functions that would otherwise do the job, so that a documented
 * parameter named `car` or `void?` cannot change what its own function does on
 * every call -- see src/lib/runtime.scm.
 */
const optsName = '##opts##'
const optArgName = '##optArg##'
const optRestName = '##optRest##'
const checkArityName = '##checkArity##'
const voidQName = '##voidQ##'

/**
 * Wraps `body` in the bindings that take the optional parameters, in order,
 * off the wrapper lambda's rest parameter: each one is that argument, or void
 * once the caller's arguments run out. A signature with no optional parameters
 * gets no bindings and no rest parameter of its own, so it lowers exactly as
 * before.
 *
 * Whatever follows the last optional is the declared rest parameter, if the
 * signature has one; if it does not, anything left over is an arity error,
 * since the wrapper's own rest parameter is what let those arguments through.
 */
function mkOptBindings(
  numRequired: number,
  optParams: Param[],
  restParam: Param | undefined,
  body: A.Exp,
  range: Range,
): A.Exp {
  const opts = () => A.mkId(optsName, range)
  const numOpts = optParams.length
  let inner = restParam
    ? A.mkLet(
        [
          {
            pat: A.mkId(restParam.name, range),
            value: A.mkApp(
              A.mkId(optRestName, range),
              [opts(), A.mkLit(numOpts, range)],
              range,
            ),
          },
        ],
        body,
        range,
      )
    : A.mkBegin(
        [
          A.mkApp(
            A.mkId(checkArityName, range),
            [opts(), A.mkLit(numOpts, range), A.mkLit(numRequired, range)],
            range,
          ),
          body,
        ],
        range,
      )
  for (let i = numOpts - 1; i >= 0; i--) {
    inner = A.mkLet(
      [
        {
          pat: A.mkId(optParams[i].name, range),
          value: A.mkApp(
            A.mkId(optArgName, range),
            [opts(), A.mkLit(i, range)],
            range,
          ),
        },
      ],
      inner,
      range,
    )
  }
  return inner
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
 *       (##contracted## ##contract-target##
 *         (lambda (x1 ... xk [& rest])
 *           <cascading predicate checks, then (##contract-target## x1 ... xk)>))))
 *
 * ##contracted## tags the wrapper with the value it checks, so library code
 * naming this definition reaches that value and skips the checks (see
 * VarHandler).
 *
 * A signature with optional parameters -- `(substring s start [end])` -- has
 * no fixed arity, so the wrapper takes them through a rest parameter of its
 * own and binds each in turn (mkOptBindings), leaving an unsupplied one void.
 * They are still passed to the wrapped value positionally, so *it* declares
 * them as ordinary parameters: a Javascript primitive receives `undefined`,
 * which is already how Javascript spells "not supplied".
 *
 * @returns the statement unchanged if it isn't a define, has no docstring,
 *          the docstring fails to parse (a documentation-quality issue, not
 *          a reason to fail compiling -- see ast.ts's Define.docComments),
 *          or documents neither fixed params nor a rest param. That last
 *          case is both zero-parameter forms, and each is left alone for its
 *          own reason. A documented *constant* (`pi: number?`) has to be:
 *          wrapping it in a zero-arg thunk would silently turn a value into
 *          something that must be called. A documented *nullary function*
 *          (`(rex-empty) -> rex?`) is left alone by choice -- the two forms
 *          are distinguishable here, via the parsed signature's `isConstant`
 *          (#412, see docstring/signature.ts) -- because its wrapper would
 *          have nothing to check: every check comes from a parameter, and a
 *          signature's return predicate is not checked anywhere, so wrapping
 *          would buy only an arity error on an over-supplied call (#469).
 *          A rest-only signature (`(rex-concat & xs)`) does carry a
 *          predicate to apply, so it is wrapped even with zero fixed params.
 */
export function contractStmt(s: A.Stmt): A.Stmt {
  // Both a plain define and a define-export bind a documented value that a
  // docstring can describe (the standard library uses define-export -- see
  // src/lib/*.scm), so both are wrapped, preserving the original form.
  if ((s.tag !== 'define' && s.tag !== 'defexport') || !s.docComments) {
    return s
  }
  // A malformed docstring yields `doc: undefined` (handled below); a genuine
  // internal error (ICE) is intentionally NOT caught here -- it should surface
  // as a loud failure rather than silently skip contract insertion.
  const { doc } = parseFunctionDocFromComments(s.docComments)
  if (
    !doc ||
    (doc.params.length === 0 && doc.optParams.length === 0 && !doc.restParam)
  ) {
    return s
  }
  const checks = mkCheckChain(doc.params, doc.optParams, doc.restParam, s.range)
  // With optional parameters the wrapper's own rest parameter is what collects
  // them, so the declared rest parameter (if any) is bound from what is left
  // rather than by the lambda itself.
  const hasOpts = doc.optParams.length > 0
  const body = hasOpts
    ? mkOptBindings(
        doc.params.length,
        doc.optParams,
        doc.restParam,
        checks,
        s.range,
      )
    : checks
  const lamRest = hasOpts
    ? A.mkId(optsName, s.range)
    : doc.restParam
      ? A.mkId(doc.restParam.name, s.range)
      : undefined
  // The wrapper is handed to ##contracted## along with the value it checks, so
  // the machine can reach that value directly and skip the checks on a call
  // made from library code (see VarHandler).
  const wrapped = A.mkLet(
    [{ pat: A.mkId(contractTargetName, s.range), value: s.value }],
    A.mkApp(
      A.mkId(contractedName, s.range),
      [
        A.mkId(contractTargetName, s.range),
        A.mkLam(
          doc.params.map((p) => A.mkId(p.name, s.range)),
          body,
          s.range,
          lamRest,
        ),
      ],
      s.range,
    ),
    s.range,
  )
  return s.tag === 'define'
    ? A.mkDefine(s.name, wrapped, s.range, s.docComments)
    : A.mkDefineExport(s.name, wrapped, s.range, s.docComments)
}

/** Applies contractStmt to every statement in a program. */
export function contractProgram(prog: A.Prog): A.Prog {
  return prog.map(contractStmt)
}
