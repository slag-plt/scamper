import * as L from "./lang.js"
import { ICE } from "./error.js"

/**
 * A stack frame records all relevant to track the execution of a single function call.
 */
export class Frame {
  name: string
  env: L.Env
  values: L.Value[]
  ops: L.Ops[]

  constructor(name: string, env: L.Env, blk: L.Blk) {
    this.name = name
    this.env = env
    this.values = []
    this.ops = blk.toReversed()
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
}
