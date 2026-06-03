import { ICE, ScamperError } from "../error"
import { Fiber } from "../fiber"
import { Ops, Value } from "../lang"
import { Frame } from "../thread"
import { isClosure, isJsFunction, mkClosure, mkStruct, pMatch } from "../util"

/* Definition */
type OpHandler<T extends Ops["tag"]> = (
  op: Extract<Ops, { tag: T }>,
  currFrame: Frame,
  fiber: Fiber,
) => boolean

/* Handlers */
export const LitHandler: OpHandler<"lit"> = (op, currFrame) => {
  currFrame.values.push(op.value)
  return false
}

export const VarHandler: OpHandler<"var"> = (op, currFrame) => {
  if (!currFrame.env.has(op.name)) {
    throw new ScamperError("Runtime", `Variable not found: ${op.name}`)
  }
  currFrame.values.push(currFrame.env.get(op.name))
  return false
}

export const CtorHandler: OpHandler<"ctor"> = (op, currFrame) => {
  currFrame.values.push(
    mkStruct(op.name, op.fields, currFrame.values.splice(-op.fields.length)),
  )
  return false
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
  return false
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
    return true
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
    return true
  }
  throw new ScamperError(
    "Runtime",
    `Not a function or closure: ${JSON.stringify(fn)}`,
  )
}

export const MatchHandler: OpHandler<"match"> = (op, currFrame) => {
  const scrutinee = currFrame.values.pop()
  if (!scrutinee) {
    throw new ICE("Fiber.MatchHandler", "Match requires at least one value")
  }
  // we will always step match to abide by a small work quantum
  const currBranch = op.branches.shift()
  if (!currBranch) {
    throw new ScamperError("Runtime", `Inexhaustive pattern match failure`)
  }
  const [pat, blk] = currBranch
  const bindings = pMatch(scrutinee, pat)
  if (!bindings) {
    currFrame.pushBlk([op])
  } else {
    currFrame.env = currFrame.env.extend(...bindings)
    currFrame.pushBlk(blk)
  }
  return true
}

export const PopVHandler: OpHandler<"popv"> = (op, currFrame) => {
  currFrame.values.pop()
  return true
}