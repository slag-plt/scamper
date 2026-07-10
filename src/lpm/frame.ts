import * as L from "./lang.js"
import { ICE } from "./error.js"
import { InvocationNode, ReportCapture } from "./reporting/invocation-node"

/**
 * A stack frame records all relevant to track the execution of a single function call.
 */
export class Frame {
  name: string
  env: L.Env
  values: L.Value[]
  ops: L.Ops[]
  // for reporting
  rptCapture?: ReportCapture
  tailCallDepth = 0

  constructor(
    name: string,
    env: L.Env,
    blk: L.Blk,
    rptCapture?: ReportCapture,
  ) {
    this.name = name
    this.env = env
    this.values = []
    this.ops = [...blk]
    ensureApIndices(this.ops)
    this.ops.reverse()
    this.rptCapture = rptCapture
  }

  isFinished(): boolean {
    return this.ops.length === 0
  }

  pushBlk(blk: L.Blk) {
    this.ops.push(...blk.toReversed())
  }

  popInstr(): L.Ops {
    const op = this.ops.pop()
    if (!op)
      throw new ICE(
        "Frame.popInstr",
        `Attempted to pop operation off frame ${this.name} when none remain`,
      )
    return op
  }

  settleTop(result: L.Value): InvocationNode | null {
    const capture = this.rptCapture
    if (capture === undefined) {
      return null
    }

    const node = capture.stack.pop()
    if (!node) {
      throw new ICE(
        "Frame.settleTop",
        "No node left to settle in report capture stack",
      )
    }
    node.result = result

    const parent = capture.stack.at(-1) ?? capture.root
    parent.children.push(node)
    return node
  }
}

function ensureApIndices(blk: L.Blk, startIdx = 0): number {
  let currIdx = startIdx
  for (const op of blk) {
    switch (op.tag) {
      case "ap": {
        op.apIdx ??= currIdx
        currIdx = Math.max(currIdx, op.apIdx + 1)
        break
      }
      case "match": {
        for (const [_, blk] of op.branches) {
          currIdx = ensureApIndices(blk, currIdx)
        }
        break
      }
    }
  }
  return currIdx
}
