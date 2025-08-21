import * as R from './runtime.js'

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
  public result: R.Value 

  /**
   * Creates a new thread with an initial frame created from the given
   * arguments.
   */
  constructor (code: R.Id, env?: R.Env) {
    this.frames = []
    this.push(code, env)
  }

  /** @return true iff this thread has finished execution. */
  isFinished (): boolean {
    return this.frames.length === 0
  }

  /** @return the current active frame of this thread. */
  getActiveFrame (): Frame {
    return this.frames[this.frames.length - 1]
  }

  /** @return the number of frames in this thread. */
  getNumFrames (): number {
    return this.frames.length
  }

  /** Advances the PC of this thread by one instruction. */
  advancePc (): void {
    if (this.isFinished()) {
      throw new R.ICE('advancePc', 'ICE: No current frame to advance program counter')
    }
    this.getActiveFrame().pc += 2;
  }

  /** 
   * Pushes a new frame onto this thread's stack created from the given
   * arguments.
   */
  push (code: R.Id, env?: R.Env): void {
    this.frames.push({
      code,
      pc: 0,
      values: [],
      locals: env ?? []
    })
  }

  /** Pops the currently active frame from this thread's stack. */
  pop (): void {
    this.frames.pop()!
  }
}

export default Thread