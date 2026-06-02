import { ScamperInstance } from "../scamper-instance"
import { Blk, Env, Prog, Stmt, Value } from "./lang"
import { DefineHandler, DispHandler, ImportHandler, StmtExpHandler } from "./handlers/stmt-handlers"
import { Frame } from "./thread"

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
  pushFrame(frame: Frame) {
    this.#frames.push(frame)
  }
  popFrame() {
    this.#frames.pop()
  }
  hasFramesRemaining() {
    return this.#frames.length > 0
  }
  /**
   * Steps through one operation in the current frame.
   * @returns true if the step was a major step, false if it was a minor step
   */
  async stepFrame(): Promise<boolean> {
    // TODO: get current frame
    // TODO: switch on operation type and execute accordingly
    return new Promise((resolve) => {
      resolve(true)
    })
  }

  /* Library importing helper functions */
  async loadLib(libName: string) {
    const lib = await this.#scamperInstance.getLib(libName)
    for (const [name, value] of lib.lib) {
      this.topLevelEnv.set(name, value)
    }
  }
}

export type StatementHandler<T extends Stmt["tag"]> = (
  stmt: Extract<Stmt, { tag: T }>,
  fiber: Fiber,
) => Promise<boolean>
