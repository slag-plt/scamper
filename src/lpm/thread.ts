import * as R from './runtime.js'
import { ICE } from './runtime.js'

/**
 * An execution frame captures the ongoing execution of a function call:
 * + `code`: the ID of the code block being executed
 * + `pc`: the program counter that tracks the current instructions.
 * + `values`: the value stack that holds intermediate results.
 * + `locals`: the local environment.
 */
export type Frame = {
  code: R.Id
  pc: number,
  values: R.Value[],
  locals: R.Env,
}

/** A thread represents a single sequence of execution within Scamper. */
export class Thread {
  private frames: Frame[]

  constructor (code: R.Id, env?: R.Env) {
    this.frames = []
    this.push(code, env)
  }

  isFinished (): boolean {
    return this.frames.length === 0
  }

  getActiveFrame (): Frame {
    return this.frames[this.frames.length - 1]
  }

  getNumFrames (): number {
    return this.frames.length
  }

  push (code: R.Id, env?: R.Env): void {
    this.frames.push({
      code,
      pc: 0,
      values: [],
      locals: env ?? []
    })
  }

  pop (): void {
    const prevFrame = this.frames.pop()!
    if (prevFrame.values.length !== 0) {
      throw new ICE('Thread.pop', 'Value stack must have exactly one value when returning')
    }
    if (this.frames.length !== 0) {
      // N.B., we expect that the PC of the new frame has already been
      // advanced during the initial dump/function call.
      this.getActiveFrame().values.push(prevFrame.values[0])
    }
  }
}

export default Thread