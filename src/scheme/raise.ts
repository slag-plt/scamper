import * as LPM from "../lpm"
import { Fiber } from "../lpm/fiber"
import { Frame } from "../lpm/frame"
import type {
  InvocationNode,
  MatchOutcome,
} from "../lpm/reporting/invocation-node"
import type { PageLink, RecursivePage } from "../lpm/reporting/pruning"
import * as A from "./ast.js"
import { sugarExprWithAppMetadata } from "./sugarer.js"

/** One raised page and the source applications that lead to other pages. */
export interface RaisedPage {
  source: A.Exp
  links: Map<A.App, PageLink>
}

type RaiseBody = (body: LPM.Blk, env: LPM.Env) => A.Exp

interface RaiseHooks {
  application?: (op: LPM.Ap, app: A.App) => A.Exp
  match?: (
    op: LPM.Match,
    scrutinee: A.Exp,
    env: LPM.Env,
    raiseBody: RaiseBody,
  ) => A.Exp
  closure?: (op: LPM.Cls, env: LPM.Env, raiseBody: RaiseBody) => A.Exp
}

/** @return a stack of expressions created from the given value stack. */
export function valuesToExps(values: LPM.Value[]): A.Exp[] {
  return values.map((v) => {
    if ((LPM.isFunction(v) || LPM.isClosure(v)) && v.name) {
      return A.mkVar(v.name)
    } else {
      return A.mkLit(v)
    }
  })
}

/** Raises one partially evaluated frame using the shared block-raising engine. */
export function raiseFrame(
  values: A.Exp[],
  env: LPM.Env,
  ops: LPM.Ops[],
): A.Exp {
  return raiseBlock(values, env, ops)
}

/** Raises a completed recursive page without exposing paging details to the AST. */
export function raisePage(page: RecursivePage): RaisedPage {
  const node = page.invocation.node
  if (!LPM.isClosure(node.fn)) {
    throw new LPM.ICE(
      "raisePage",
      "Attempted to raise a page whose invocation is not a closure",
    )
  }

  const context = pageRaiseContext(page)
  const source = raiseBlock([], node.env, node.fn.code.toReversed(), {
    application: (op, app) => raisePageApplication(context, op, app),
    match: (op, scrutinee, env, raiseBody) =>
      raisePageMatch(context, op, scrutinee, env, raiseBody),
    // Source lambdas currently carry a synthetic closure name. Do not descend
    // into their unexecuted body while reconstructing an enclosing page.
    closure: (op, env, raiseBody) =>
      op.name
        ? A.mkVar(op.name)
        : A.mkLam(
            op.params,
            raiseBody(op.body, env.withoutLocals(...op.params)),
          ),
  })
  const sugared = sugarExprWithAppMetadata(source, context.links)
  return { source: sugared.expr, links: sugared.appMetadata }
}

/** Raises all active frames from innermost to outermost. */
export function raiseFrames(frames: Frame[]): A.Exp {
  if (frames.length === 0) {
    throw new LPM.ICE("raiseFrames", "no frames to raise")
  }
  const lastFrame = frames[frames.length - 1]
  let ret = raiseFrame(
    valuesToExps(lastFrame.values),
    lastFrame.env,
    lastFrame.ops,
  )
  for (let i = frames.length - 2; i >= 0; i--) {
    const values = valuesToExps(frames[i].values)
    values.push(ret)
    ret = raiseFrame(values, frames[i].env, frames[i].ops)
  }
  return ret
}

export function raiseFiber(fiber: Fiber): A.Exp {
  return raiseFrames(fiber.frames)
}

/**
 * The common engine for live-frame and completed-page raising. `ops` is in
 * frame-stack order, so reading it right-to-left reconstructs source order.
 */
function raiseBlock(
  values: A.Exp[],
  env: LPM.Env,
  ops: LPM.Ops[],
  hooks: RaiseHooks = {},
): A.Exp {
  const raiseBody: RaiseBody = (body, bodyEnv) =>
    raiseBlock([], bodyEnv, body.toReversed(), hooks)

  for (let i = ops.length - 1; i >= 0; i--) {
    const op = ops[i]
    switch (op.tag) {
      case "lit": {
        values.push(A.mkLit(op.value))
        break
      }

      case "var": {
        if (env.has(op.name)) {
          const value = env.get(op.name)
          values.push(
            LPM.isFunction(value) || LPM.isClosure(value)
              ? A.mkVar(op.name)
              : A.mkLit(value),
          )
        } else {
          values.push(A.mkVar(op.name))
        }
        break
      }

      case "ctor": {
        const arity = op.fields.length
        const args = arity === 0 ? [] : values.splice(-arity)
        values.push(A.mkApp(A.mkVar(op.name), args))
        break
      }

      case "cls": {
        values.push(
          hooks.closure?.(op, env, raiseBody) ??
            raiseClosure(op, env, raiseBody),
        )
        break
      }

      case "ap": {
        const vs = values.splice(-(op.numArgs + 1))
        const head = vs[0]
        const args = op.numArgs === 0 ? [] : vs.slice(1)
        const app = A.mkApp(head, args)
        values.push(hooks.application?.(op, app) ?? app)
        break
      }

      case "match": {
        const scrutinee = values.pop()
        if (!scrutinee) {
          throw new LPM.ICE(
            "raiseBlock",
            "Expected a match scrutinee while raising",
          )
        }
        values.push(
          hooks.match?.(op, scrutinee, env, raiseBody) ??
            raiseMatch(op, scrutinee, env, raiseBody),
        )
        break
      }

      case "raise": {
        values.push(A.mkApp(A.mkVar("raise"), [A.mkLit(op.msg)]))
        break
      }

      case "pops": {
        // N.B., pops the local environment, but we don't track that here!
        break
      }

      case "popv": {
        values.pop()
        break
      }
    }
  }
  const result = values.pop()
  if (!result) {
    throw new LPM.ICE("raiseBlock", "No expression remained after raising")
  }
  return result
}

function raiseClosure(op: LPM.Cls, env: LPM.Env, raiseBody: RaiseBody): A.Exp {
  const body = raiseBody(op.body, env.withoutLocals(...op.params))
  return op.name ? A.mkVar(op.name) : A.mkLam(op.params, body)
}

function raiseMatch(
  op: LPM.Match,
  scrutinee: A.Exp,
  env: LPM.Env,
  raiseBody: RaiseBody,
): A.Exp {
  return A.mkMatch(
    scrutinee,
    op.branches.map(([pat, body]) => ({ pat, body: raiseBody(body, env) })),
  )
}

interface PageRaiseContext {
  invocationsByApIdx: Map<number, InvocationNode>
  linksByApIdx: Map<number, PageLink>
  matchesByIdx: Map<number, MatchOutcome>
  links: Map<A.App, PageLink>
}

function pageRaiseContext(page: RecursivePage): PageRaiseContext {
  const directChildren = new Set(
    page.invocation.children.map((child) => child.node),
  )
  const invocationsByApIdx = new Map<number, InvocationNode>()
  for (const child of page.invocation.children) {
    addUnique(
      invocationsByApIdx,
      child.node.apIdx,
      child.node,
      "multiple direct invocations share an application label",
    )
  }

  const linksByApIdx = new Map<number, PageLink>()
  for (const link of page.links) {
    const source =
      link.skippedInvocations.at(0)?.node ?? link.targetPage.invocation.node
    if (!directChildren.has(source)) {
      throw new LPM.ICE(
        "raisePage",
        "A page link does not originate in the page invocation",
      )
    }
    addUnique(
      linksByApIdx,
      source.apIdx,
      link,
      "multiple page links share an application label",
    )
  }

  const matchesByIdx = new Map<number, MatchOutcome>()
  for (const outcome of page.invocation.node.matches) {
    addUnique(
      matchesByIdx,
      outcome.matchIdx,
      outcome,
      "multiple match outcomes share a static label",
    )
  }

  return {
    invocationsByApIdx,
    linksByApIdx,
    matchesByIdx,
    links: new Map(),
  }
}

function raisePageApplication(
  context: PageRaiseContext,
  op: LPM.Ap,
  app: A.App,
): A.Exp {
  if (op.apIdx === undefined) {
    throw new LPM.ICE(
      "raisePage",
      "Encountered an executed application without a static label",
    )
  }
  const invocation = context.invocationsByApIdx.get(op.apIdx)
  if (!invocation) {
    throw new LPM.ICE(
      "raisePage",
      `No captured invocation for application label ${op.apIdx.toString()}`,
    )
  }

  const link = context.linksByApIdx.get(op.apIdx)
  if (link) {
    context.links.set(app, link)
    return app
  }
  if (hasRouteLink(app, context.links)) {
    return app
  }
  if (!("result" in invocation)) {
    throw new LPM.ICE(
      "raisePage",
      `Application label ${op.apIdx.toString()} did not settle`,
    )
  }
  return A.mkLit(invocation.result)
}

function raisePageMatch(
  context: PageRaiseContext,
  op: LPM.Match,
  scrutinee: A.Exp,
  env: LPM.Env,
  raiseBody: RaiseBody,
): A.Exp {
  if (op.matchIdx === undefined) {
    throw new LPM.ICE(
      "raisePage",
      "Encountered an executed match without a static label",
    )
  }
  const outcome = context.matchesByIdx.get(op.matchIdx)
  if (!outcome) {
    throw new LPM.ICE(
      "raisePage",
      `No captured outcome for match label ${op.matchIdx.toString()}`,
    )
  }
  const selected = op.branches.at(outcome.branchIdx)
  if (!selected) {
    throw new LPM.ICE(
      "raisePage",
      `Captured branch ${outcome.branchIdx.toString()} is outside match label ${op.matchIdx.toString()}`,
    )
  }
  const [selectedPat, selectedBody] = selected
  const bindings = LPM.pMatch(outcome.scrutinee, selectedPat)
  if (!bindings) {
    throw new LPM.ICE(
      "raisePage",
      `Captured match branch ${outcome.branchIdx.toString()} no longer matches its scrutinee`,
    )
  }
  const raisedSelectedBody = raiseBody(
    selectedBody,
    env.extendWithLocals(...bindings),
  )
  const renderedScrutinee = hasRouteLink(scrutinee, context.links)
    ? scrutinee
    : A.mkLit(outcome.scrutinee)

  return A.mkMatch(
    renderedScrutinee,
    op.branches.map(([pat], index) => ({
      pat,
      body: index === outcome.branchIdx ? raisedSelectedBody : A.mkVar("…"),
    })),
  )
}

function addUnique<T>(
  map: Map<number, T>,
  key: number,
  value: T,
  duplicateMessage: string,
): void {
  if (map.has(key)) {
    throw new LPM.ICE("raisePage", duplicateMessage)
  }
  map.set(key, value)
}

function hasRouteLink(
  exp: A.Exp,
  links: ReadonlyMap<A.App, PageLink>,
): boolean {
  switch (exp.tag) {
    case "lit":
    case "var":
    case "quote":
      return false
    case "app":
      return (
        links.has(exp) ||
        hasRouteLink(exp.head, links) ||
        exp.args.some((arg) => hasRouteLink(arg, links))
      )
    case "lam":
      return hasRouteLink(exp.body, links)
    case "let":
    case "let*":
      return (
        exp.bindings.some(({ value }) => hasRouteLink(value, links)) ||
        hasRouteLink(exp.body, links)
      )
    case "begin":
    case "and":
    case "or":
    case "section":
      return exp.exps.some((subexp) => hasRouteLink(subexp, links))
    case "if":
      return (
        hasRouteLink(exp.guard, links) ||
        hasRouteLink(exp.ifB, links) ||
        hasRouteLink(exp.elseB, links)
      )
    case "match":
      return (
        hasRouteLink(exp.scrutinee, links) ||
        exp.branches.some(({ body }) => hasRouteLink(body, links))
      )
    case "cond":
      return exp.branches.some(
        ({ test, body }) =>
          hasRouteLink(test, links) || hasRouteLink(body, links),
      )
    case "report":
      return hasRouteLink(exp.exp, links)
  }
}
