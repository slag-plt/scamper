import { ICE, ReportError, ScamperError } from "../error"
import { Fiber, minorStep, StepResult, traceStep } from "../fiber"
import { Env, Ops, Value } from "../lang"
import { Frame } from "../frame"
import { isJsFunction, isScamperFn, mkClosure, mkStruct, pMatch } from "../util"
import { InvocationNode } from "../reporting/invocation-node"

/* Definition */
type OpHandler<T extends Ops["tag"]> = (
  op: Extract<Ops, { tag: T }>,
  currFrame: Frame,
  fiber: Fiber,
) => StepResult

/* Handlers */
export const LitHandler: OpHandler<"lit"> = (op, currFrame) => {
  currFrame.values.push(op.value)
  return minorStep
}

export const VarHandler: OpHandler<"var"> = (op, currFrame) => {
  if (!currFrame.env.has(op.name)) {
    throw new ScamperError("Runtime", `Variable not found: ${op.name}`)
  }
  currFrame.values.push(currFrame.env.get(op.name))
  return minorStep
}

export const CtorHandler: OpHandler<"ctor"> = (op, currFrame) => {
  currFrame.values.push(
    mkStruct(op.name, op.fields, currFrame.values.splice(-op.fields.length)),
  )
  return minorStep
}

export const ClsHandler: OpHandler<"cls"> = (op, currFrame) => {
  currFrame.values.push(
    mkClosure(
      op.params,
      op.body,
      currFrame.env.getLocals(),
      // TODO: this dummy function should exist until we remove all calls to L.callScamperFn
      () => {
        throw new ICE("Fiber.ClsHandler", "Closure.call was deprecated!")
      },
    ),
  )
  return minorStep
}

export const ApHandler: OpHandler<"ap"> = (op, currFrame, fiber) => {
  if (currFrame.values.length < op.numArgs + 1) {
    throw new ICE(
      "Fiber.ApHandler",
      `Not enough values for application: expected ${(op.numArgs + 1).toString()}, currently have ${currFrame.values.length.toString()}`,
    )
  }
  const values = currFrame.values.splice(-(op.numArgs + 1))
  const fn = values[0]
  const args = op.numArgs === 0 ? [] : values.splice(-op.numArgs)
  if (!isScamperFn(fn)) {
    throw new ScamperError(
      "Runtime",
      `Not a function or closure: ${JSON.stringify(fn)}`,
      undefined,
      op.range,
      undefined,
    )
  }

  const node: InvocationNode | undefined = currFrame.rptTrace && {
    fn,
    env: Env.snapshot(currFrame.env),
    args: [...args],
    children: [],
    apIdx: op.apIdx ?? -1,
  }
  if (node) {
    currFrame.rptTrace?.stack.push(node)
  }

  if (isJsFunction(fn)) {
    try {
      const result = fn(...args)
      currFrame.values.push(result)
      currFrame.settleTop(result)
      return traceStep
    } catch (e) {
      if (e instanceof ScamperError) {
        e.range = op.range
        e.source = fn.name
        throw e
      } else {
        throw new ScamperError(
          "Runtime",
          `Unexpected error in Javascript function call: ${JSON.stringify(e)}`,
          undefined,
          op.range,
          undefined,
        )
      }
    }
  }

  // implied fn is closure
  if (fn.params.length !== args.length) {
    throw new ScamperError(
      "Runtime",
      `Arity mismatch in function call: expected ${fn.params.length.toString()} arguments, got ${args.length.toString()}`,
      undefined,
      op.range,
      undefined,
    )
  }
  const newFrame = new Frame(
    fn.name ?? "##anonymous##",
    fiber.topLevelEnv.extendReplacingLocals(
      ...fn.locals,
      ...fn.params.map((p, i): [string, Value] => [p, args[i]]),
    ),
    fn.code,
    currFrame.rptTrace,
  )
  // update invocation node env
  if (node) {
    node.env = Env.snapshot(newFrame.env)
  }
  if (currFrame.isFinished()) {
    // tail-call optimize by replacing current empty frame
    newFrame.tailCallDepth = currFrame.tailCallDepth + 1
    fiber.replaceFrame(newFrame)
  } else {
    fiber.pushFrame(newFrame)
  }
  return traceStep
}

export const MatchHandler: OpHandler<"match"> = (op, currFrame) => {
  const scrutinee = currFrame.values.pop()
  if (scrutinee === undefined) {
    throw new ICE("Fiber.MatchHandler", "Match requires at least one value")
  }
  // we will always step match to abide by a small work quantum
  // TODO: we need to figure out if we want to keep this, hack fix for now
  op.currBranchIdx ??= 0
  const currBranch = op.branches.at(op.currBranchIdx++)
  if (!currBranch) {
    throw new ScamperError("Runtime", `Inexhaustive pattern match failure`)
  }
  const [pat, blk] = currBranch
  const bindings = pMatch(scrutinee, pat)
  if (!bindings) {
    currFrame.pushBlk([op])
    // make sure to push the scrutinee back for the next branch!
    currFrame.values.push(scrutinee)
  } else {
    currFrame.env = currFrame.env.extendWithLocals(...bindings)
    op.currBranchIdx = 0
    currFrame.pushBlk(blk)
  }
  return traceStep
}

export const PopVHandler: OpHandler<"popv"> = (_, currFrame) => {
  currFrame.values.pop()
  return traceStep
}

export const RptBeginHandler: OpHandler<"rpt-begin"> = (_, currFrame) => {
  // initialize stack
  currFrame.rptTrace = { root: { children: [] }, stack: [] }
  return traceStep
}

export const RptEndHandler: OpHandler<"rpt-end"> = (op, currFrame) => {
  if (currFrame.values.length < 1) {
    throw new ICE(
      "Fiber.RptEndHandler",
      "Expected to report a value, but none remain?",
    )
  }
  throw new ReportError(currFrame.values.at(-1), op.range)
}
