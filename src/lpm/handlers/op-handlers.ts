import { ICE, ReportError, ScamperError } from "../error"
import { Fiber, MinorStep, StepResult, TraceStep } from "../fiber"
import { Ops, Value } from "../lang"
import { Frame } from "../frame"
import { isClosure, isJsFunction, mkClosure, mkStruct, pMatch } from "../util"

/* Definition */
type OpHandler<T extends Ops["tag"]> = (
  op: Extract<Ops, { tag: T }>,
  currFrame: Frame,
  fiber: Fiber,
) => StepResult

/* Handlers */
export const LitHandler: OpHandler<"lit"> = (op, currFrame) => {
  currFrame.values.push(op.value)
  return MinorStep
}

export const VarHandler: OpHandler<"var"> = (op, currFrame) => {
  if (!currFrame.env.has(op.name)) {
    throw new ScamperError("Runtime", `Variable not found: ${op.name}`)
  }
  currFrame.values.push(currFrame.env.get(op.name))
  return MinorStep
}

export const CtorHandler: OpHandler<"ctor"> = (op, currFrame) => {
  currFrame.values.push(
    mkStruct(op.name, op.fields, currFrame.values.splice(-op.fields.length)),
  )
  return MinorStep
}

export const ClsHandler: OpHandler<"cls"> = (op, currFrame) => {
  currFrame.values.push(
    mkClosure(
      op.params,
      op.body,
      currFrame.env,
      // TODO: this dummy function should exist until we remove all calls to L.callScamperFn
      () => {
        throw new ICE("Fiber.ClsHandler", "Closure.call was deprecated!")
      },
    ),
  )
  return MinorStep
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
  if (isJsFunction(fn)) {
    currFrame.values.push(fn(...args))
    return TraceStep
  }
  if (isClosure(fn)) {
    if (fn.params.length !== args.length) {
      throw new ScamperError(
        "Runtime",
        `Arity mismatch in function call: expected ${fn.params.length.toString()} arguments, got ${args.length.toString()}`,
      )
    }
    const newFrame = new Frame(
      fn.name ?? "##anonymous##",
      fn.env.extend(...fn.params.map((p, i): [string, Value] => [p, args[i]])),
      fn.code,
    )
    if (currFrame.isFinished()) {
      // tail-call optimize by replacing current empty frame
      fiber.replaceFrame(newFrame)
    } else {
      fiber.pushFrame(newFrame)
    }
    return TraceStep
  }
  throw new ScamperError(
    "Runtime",
    `Not a function or closure: ${JSON.stringify(fn)}`,
  )
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
    currFrame.env = currFrame.env.extend(...bindings)
    op.currBranchIdx = 0
    currFrame.pushBlk(blk)
  }
  return TraceStep
}

export const PopVHandler: OpHandler<"popv"> = (_, currFrame) => {
  currFrame.values.pop()
  return TraceStep
}

export const ReptHandler: OpHandler<"rept"> = (op, currFrame) => {
  if (currFrame.values.length < 1) {
    throw new ICE(
      "Fiber.ReptHandler",
      "Expected to report a value, but none remain?",
    )
  }
  throw new ReportError(currFrame.values.at(-1), op.range)
}
