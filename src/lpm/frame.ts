import * as L from "./lang.js"
import { ICE } from "./error.js"
import { InvocationNode, ReportTrace } from "./reporting/invocation-node"

/**
 * A stack frame records all relevant to track the execution of a single function call.
 */
export class Frame {
  name: string
  env: L.Env
  values: L.Value[]
  ops: L.Ops[]
  // for reporting
  rptTrace?: ReportTrace
  /** Active report boundaries in this frame, paired from begin to end. */
  rptBoundaries: ReportBoundary[] = []
  /** True for frames descended from the documented example call. */
  queryRun: boolean
  /** True only for the documented example call's closure frame. */
  queryRoot: boolean
  tailCallDepth = 0

  constructor(
    name: string,
    env: L.Env,
    blk: L.Blk,
    rptTrace?: ReportTrace,
    queryRun = false,
    queryRoot = false,
  ) {
    this.name = name
    this.env = env
    this.values = []
    this.ops = [...blk]
    ensureApIndices(this.ops)
    this.ops.reverse()
    this.rptTrace = rptTrace
    this.queryRun = queryRun
    this.queryRoot = queryRoot
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
    const trace = this.rptTrace
    if (trace === undefined) {
      return null
    }

    const node = trace.stack.pop()
    if (!node) {
      throw new ICE(
        "Frame.settleTop",
        "No node left to settle in report capture stack",
      )
    }
    node.result = result

    const parent = trace.stack.at(-1) ?? trace.root
    parent.children.push(node)
    return node
  }
}

interface ReportBoundary {
  ownsTerminalResult: boolean
  targetIsApplication: boolean
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
