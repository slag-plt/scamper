import { ScamperInstance } from "../scamper-instance"
import { Blk, Env, Prog, Stmt, Value } from "./lang"
import {
  DefineHandler,
  DispHandler,
  ImportHandler,
  StmtExpHandler,
} from "./handlers/stmt-handlers"
import { Frame } from "./thread"
import { ScamperError } from "./error"
import {
  ApHandler,
  ClsHandler,
  CtorHandler,
  LitHandler,
  MatchHandler,
  PopVHandler,
  VarHandler,
} from "./handlers/op-handlers"

// a fiber is a concurrent thread of execution
// not named thread because we can't multithread in javascript, but we can use async/await to achieve similar results
export class Fiber {
  isDone = false
  topLevelEnv: Env = new Env()
  lastResult: Value | null = null

  #scamperInstance: ScamperInstance = ScamperInstance.getInstance()
  #prog: Stmt[]
  #currStmtIdx = 0
  #frames: Frame[] = []
  #isProcessingBlk = false
  #maxCallStackDepth = 10_000

  constructor(prog: Prog) {
    this.#prog = prog
  }

  /**
   * Completes one execution step.
   * If the completed step also completes a statement, it will move on to the next statement.
   * @returns true if the step is a major step of execution, false if it's a minor step (e.g. variable loading)
   */
  async step() {
    const currStmt = this.#prog[this.#currStmtIdx]
    switch (currStmt.tag) {
      case "import":
        return ImportHandler(currStmt, this)
      case "define":
        return DefineHandler(currStmt, this)
      case "disp":
        return DispHandler(currStmt, this)
      case "stmtexp":
        return StmtExpHandler(currStmt, this)
    }
  }

  /* Statement execution helper functions */
  advanceStmt() {
    this.#frames = []
    this.#currStmtIdx++
    this.#isProcessingBlk = false
  }
  get isProcessingBlk() {
    return this.#isProcessingBlk
  }
  // Populate the stack frames for a Blk statement, and set the isProcessingBlk flag to true
  beginProcessingBlk(expr: Blk) {
    this.#isProcessingBlk = true

    // populate stack frames
    this.pushFrame(
      new Frame(
        `##stmt-${this.#currStmtIdx.toString()}##`,
        this.topLevelEnv,
        expr,
      ),
    )
  }

  /* Stack frame helper functions */
  get currentFrame() {
    return this.#frames.at(-1)
  }
  pushFrame(frame: Frame) {
    if (this.#frames.length >= this.#maxCallStackDepth) {
      throw new ScamperError("Runtime", `Max call stack depth ${this.#maxCallStackDepth.toString()} exceeded!`)
    }
    this.#frames.push(frame)
  }
  popFrame() {
    this.#frames.pop()
  }
  hasFramesRemaining() {
    return this.#frames.length > 0
  }
  replaceFrame(frame: Frame) {
    this.popFrame()
    this.pushFrame(frame)
  }
  /**
   * Steps through one operation in the current frame.
   * @returns true if the step was a major step, false if it was a minor step
   */
  stepFrame(): boolean {
    const currFrame = this.currentFrame
    if (!currFrame)
      throw new ScamperError(
        "Runtime",
        "Attempted to step stack frame when none exist!",
      )
    const currOp = currFrame.popInstr()
    // TODO: switch on operation type and execute accordingly
    switch (currOp.tag) {
      case "lit":
        return LitHandler(currOp, this.currentFrame, this)
      case "var":
        return VarHandler(currOp, this.currentFrame, this)
      case "ctor":
        return CtorHandler(currOp, this.currentFrame, this)
      case "cls":
        return ClsHandler(currOp, this.currentFrame, this)
      case "ap":
        return ApHandler(currOp, this.currentFrame, this)
      case "match":
        return MatchHandler(currOp, this.currentFrame, this)
      case "popv":
        return PopVHandler(currOp, this.currentFrame, this)
      // TODO: the following instructions are useless
      // should be removed later
      case "raise":
        return false
      case "pops":
        return false
    }
  }

  /* Library importing helper functions */
  async loadLib(libName: string) {
    const lib = await this.#scamperInstance.getLib(libName)
    for (const [name, value] of lib.lib) {
      this.topLevelEnv.set(name, value)
    }
  }
}
