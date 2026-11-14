import * as A from './ast.js'
import { behindContracts, isFunction } from '../lpm/index.js'
import builtinLibs, { docRegistry } from '../lib/index.js'
import { Pred } from './docstring/docstring.js'
import { predTypeName } from './docstring/render.js'

/** Shared so that the common `shapeOf(e, env)` call allocates nothing. */
const noLocals: ReadonlyMap<string, Shape> = new Map()

// What a `define` binds, as far as anything short of running it can tell
// (#745). The warning that motivated this -- "Function docstring attached to
// non-function definition" -- used to be `!A.isLam(value)`, so every way of
// producing a function other than a bare lambda was reported as a mistake: a
// `let` wrapping one, `(o f g)`, `(if c f g)`, even `(define inc add1)`.
//
// Deciding this exactly is undecidable in general ("f is a function iff f is a
// halting computation"), so this is the generous syntactic approximation worked
// out on the issue. Two properties make it usable:
//
//   1. It is three-valued. `unknown` is the answer for anything the rules do
//      not recognise, and only `not-function` warns. A new false positive is
//      therefore impossible to introduce by accident -- the worst a gap in the
//      rules can do is stay quiet.
//   2. It runs after expansion (see scope.ts's caller), where the expression
//      language is just eight forms. `and`, `or`, `cond`, `anonfn`, `vec`,
//      `obj` and `begin` are all gone by then, and two of them come back for
//      free: `begin` lowers to nested `let` and `cond` to nested `if`, so the
//      `let` and `if` rules below cover them without naming them.

/** What a value is, where `unknown` means "no rule applies -- do not warn". */
export type Shape = 'function' | 'not-function' | 'unknown'

/**
 * What the names in scope are, and what applying them yields.
 *
 * Two maps rather than one record per name: `shapes` answers "is this value a
 * function" for an identifier, and `returns` answers "does applying this yield
 * a function" for the head of an application. Every reader wants one or the
 * other and never both, which is the shape `docRegistry` and
 * `moduleDocRegistry` already use next door in src/lib.
 */
export interface ShapeEnv {
  /** name -> what its value is. */
  shapes: Map<string, Shape>
  /** name -> what applying it returns, read off its docstring. */
  returns: Map<string, Shape>
}

/** The shape a documented return predicate describes. */
function shapeOfPred(pred: Pred): Shape {
  // Only a *simple* predicate is read. A combinator could perfectly well
  // admit a procedure -- `(any-of number? procedure?)` -- and answering
  // `not-function` for one would be this file's single warning-producing
  // answer given on a guess, which is what it exists not to do. No library
  // return predicate is a combinator today, so nothing is lost by waiting
  // until one is and deciding then.
  if (pred.tag !== 'id') {
    return 'unknown'
  }
  // predTypeName drops the trailing `?`.
  switch (predTypeName(pred)) {
    case 'procedure':
    case 'function':
      return 'function'
    // `any` promises nothing either way -- `id` returns whatever it was given.
    case 'any':
      return 'unknown'
    default:
      return 'not-function'
  }
}

/**
 * Binds every variable a pattern introduces to `unknown`, so that within the
 * pattern's scope the name resolves to neither a library value nor an
 * enclosing binding. `unknown` is enough: it is the answer both the `id` rule
 * and the `app` rule need in order to say nothing about it.
 *
 * Mirrors scopeCheckPat's walk over the same `Pat` shapes.
 */
function bindPatVars(p: A.Pat, scope: Map<string, Shape>): void {
  switch (p.tag) {
    case 'id':
      scope.set(p.name, 'unknown')
      return
    case 'pctor':
    case 'pvec':
      p.args.forEach((arg) => {
        bindPatVars(arg, scope)
      })
      return
    case 'pwild':
    case 'plit':
      return
  }
}

/** Joins two branches' shapes: they agree, or nothing is known. */
function join(a: Shape, b: Shape): Shape {
  return a === b ? a : 'unknown'
}

/**
 * The shape of `e`.
 *
 * @param locals the `let`-bound names enclosing `e`, which shadow `env`.
 *        Threaded separately rather than copied into `env` so that a `let`
 *        costs one small map rather than a clone of every library name.
 */
export function shapeOf(
  e: A.Exp,
  env: ShapeEnv,
  locals: ReadonlyMap<string, Shape> = noLocals,
): Shape {
  switch (e.tag) {
    case 'lam':
      return 'function'

    // A number, string, boolean or char. The one shape that is reliably *not*
    // a function, and the case the warning was originally written for: a
    // function docstring over `(define my-pi 3.14159)` is a real mistake
    // (#450, test/regressions/user-constant-docstring.test.ts).
    case 'lit':
      return 'not-function'

    // `??` is an expression the student has not written yet. Warning about the
    // documentation of unwritten code is noise.
    case 'hole':
      return 'unknown'

    case 'id':
      return locals.get(e.name) ?? env.shapes.get(e.name) ?? 'unknown'

    case 'let': {
      // The bindings are walked in order, each binder shadowing the enclosing
      // environment from that point on -- which is what makes
      // `(let ([add1 5]) add1)` read as the 5 rather than the library's add1.
      //
      // N.B., Scamper's `let` is really letrec: every binder is in scope
      // throughout every binding value as well as the body (scope.ts, and
      // docs/DIFFERENCES.md calls it the single most surprising difference).
      // The difference cannot be observed here. For an earlier value to see a
      // later binder it must name it, and naming a not-yet-evaluated binder is
      // a runtime error unless the reference sits inside a lambda -- which
      // `shapeOf` answers `function` for without descending into.
      const scope = new Map(locals)
      for (const b of e.bindings) {
        const shape = shapeOf(b.value, env, scope)
        if (b.pat.tag === 'id') {
          scope.set(b.pat.name, shape)
        } else {
          // A destructuring pattern binds names too, and they are not the
          // enclosing environment's. Left unbound, `(let ([(pair f n) ...])
          // (f 1))` would read `f` as a library name of the same spelling.
          bindPatVars(b.pat, scope)
        }
      }
      return shapeOf(e.body, env, scope)
    }

    case 'if':
      return join(
        shapeOf(e.ifB, env, locals),
        shapeOf(e.elseB, env, locals),
      )

    case 'match': {
      // A match with no branches answers nothing rather than vacuously
      // agreeing with itself. `(match 1)` parses, so this is reachable.
      if (e.branches.length === 0) {
        return 'unknown'
      }
      return e.branches
        .map((b) => {
          // Each branch's pattern variables shadow the environment within it,
          // for the reason given in the `let` case above.
          const scope = new Map(locals)
          bindPatVars(b.pat, scope)
          return shapeOf(b.body, env, scope)
        })
        .reduce(join)
    }

    // An application is read off the head's *documented* return predicate
    // rather than a hardcoded list of combinators. `compose`, `o`, `l-s` and
    // `r-s` all declare `-> procedure?` already, so they are covered without
    // being named, as is any future library function that returns one; and
    // `(+ 1 2)` is `not-function` off `+`'s own `-> number?`. A head that is
    // not a plain name, or names something undocumented (a student's own
    // combinator), is unknown.
    case 'app': {
      if (e.head.tag !== 'id') {
        return 'unknown'
      }
      // A `let`-bound name shadows the library's, and nothing records what a
      // locally-bound function returns.
      if (locals.has(e.head.name)) {
        return 'unknown'
      }
      return env.returns.get(e.head.name) ?? 'unknown'
    }

    default:
      return 'unknown'
  }
}

/**
 * Every lambda that could be the function a docstring describes: the value
 * itself, or the ones reachable through the forms that merely choose between
 * values. This is what lets the parameter checks keep working for a
 * `let`-wrapped function, which the old `isLam` test skipped entirely.
 *
 * A branching form yields lambdas only when *every* branch does. One branch
 * that is a function by some other route -- `(if c (lambda (x) x) add1)` --
 * means the definition's parameters are as much `add1`'s as the lambda's, so
 * checking the docstring against the lambda alone would be a guess. Empty is
 * the answer that declines to make it.
 */
export function reachableLambdas(e: A.Exp): A.Lam[] {
  switch (e.tag) {
    case 'lam':
      return [e]
    case 'let':
      return reachableLambdas(e.body)
    case 'if':
      return allBranches([e.ifB, e.elseB])
    case 'match':
      return allBranches(e.branches.map((b) => b.body))
    default:
      return []
  }
}

/** The branches' lambdas, or none if any branch contributes none. */
function allBranches(branches: A.Exp[]): A.Lam[] {
  const found = branches.map(reachableLambdas)
  return found.some((ls) => ls.length === 0) ? [] : found.flat()
}

/** Whether two lambdas take the same parameters under the same names. */
function sameParams(a: A.Lam, b: A.Lam): boolean {
  return (
    a.params.length === b.params.length &&
    a.params.every((p, i) => p.name === b.params[i].name) &&
    a.restParam?.name === b.restParam?.name
  )
}

/**
 * The one lambda a docstring's parameters can be checked against, if there is
 * one: the single reachable lambda, or several that agree on their parameters
 * (`(if c (lambda (x) ...) (lambda (x) ...))`). Several that *disagree* yield
 * undefined -- both are the function, so blaming the docstring for matching
 * only one of them would be a guess, and so does a branching form any branch
 * of which contributes no lambda (see reachableLambdas).
 */
export function documentedLambda(e: A.Exp): A.Lam | undefined {
  const lams = reachableLambdas(e)
  if (lams.length === 0) {
    return undefined
  }
  return lams.every((l) => sameParams(l, lams[0])) ? lams[0] : undefined
}

/** The builtin modules whose names a program can reach unqualified. */
function modulesInScope(prog: A.Prog): string[] {
  // runtime and prelude are always in scope; scopeCheckProgram seeds `globals`
  // from exactly these two and then from each import.
  const mods = ['runtime', 'prelude']
  for (const s of prog) {
    // A qualified import (`as alias`) injects nothing unqualified, so its
    // names are deliberately left out -- an `alias.member` reference is not a
    // plain identifier and lands on the `unknown` default.
    if (s.tag === 'import' && s.kind === 'builtin' && s.alias === undefined) {
      mods.push(s.module)
    }
  }
  return mods
}

/**
 * The environment for `prog`: every library name in scope, then its own
 * top-level definitions.
 *
 * Library names are settled against the *values* rather than guessed, since
 * `builtinLibs` holds them -- the same trick symbol-db.ts uses to tell whether
 * two modules export one shared procedure.
 */
export function shapeEnvFor(prog: A.Prog): ShapeEnv {
  const env: ShapeEnv = { shapes: new Map(), returns: new Map() }
  for (const mod of modulesInScope(prog)) {
    for (const [name, v] of builtinLibs.get(mod)?.bindings ?? []) {
      env.shapes.set(
        name,
        isFunction(behindContracts(v)) ? 'function' : 'not-function',
      )
    }
    for (const [name, doc] of docRegistry.get(mod) ?? []) {
      if (!doc.signature.isConstant) {
        env.returns.set(name, shapeOfPred(doc.signature.predicate))
      }
    }
  }
  addTopLevelShapes(env, prog)
  return env
}

/**
 * Classifies each top-level definition, to a fixpoint.
 *
 * A loop rather than one pass because scopeCheckProgram's two passes promise
 * that top-level definitions are mutually recursive, so a definition that
 * refers to one written below it must not read as `not-function`. Each name is
 * set at most once, out of `unknown`, so this terminates in at most one round
 * per definition and the result does not depend on statement order.
 *
 * The one exception is a name defined twice, where the first definition to
 * yield a definite shape wins. Such a program is already reported
 * ("Global variable 'f' is already defined"), so it is not worth resolving.
 */
function addTopLevelShapes(env: ShapeEnv, prog: A.Prog): void {
  const defines = prog.filter((s): s is A.Define => s.tag === 'define')
  // A definition owns its name even when a library exports it too, so clear
  // the library's answer first -- `(define add1 5)` is the 5.
  for (const d of defines) {
    env.shapes.delete(d.name.name)
    env.returns.delete(d.name.name)
  }
  let changed = true
  while (changed) {
    changed = false
    for (const d of defines) {
      if (env.shapes.has(d.name.name)) {
        continue
      }
      const shape = shapeOf(d.value, env)
      if (shape !== 'unknown') {
        env.shapes.set(d.name.name, shape)
        changed = true
      }
    }
  }
}
