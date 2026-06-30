import { Blk, Env, Prog, Stmt, Value } from "./lang"
import {
  DefineHandler,
  DispHandler,
  ImportHandler,
  StmtExpHandler,
} from "./handlers/stmt-handlers"
import { Frame } from "./frame"
import { ICE, ScamperError } from "./error"
import {
  ApHandler,
  ClsHandler,
  CtorHandler,
  LitHandler,
  MatchHandler,
  PopVHandler,
  ReptHandler,
  VarHandler,
} from "./handlers/op-handlers"
import builtinLibs from "../lib"


export interface DisplayStep { tag: "display" }
export interface TraceStep { tag: "trace" }
export interface MinorStep { tag: "minor" }
export interface YieldStep { tag: "yield" }
export interface ImportFileStep {
  tag: "import-file"
  filename: string
}

export type StepResult =
  DisplayStep |
  TraceStep |
  MinorStep |
  YieldStep |
  ImportFileStep

export const displayStep: DisplayStep = { tag: "display" }
export const traceStep: TraceStep = { tag: "trace" }
export const minorStep: MinorStep = { tag: "minor" }
export const yieldStep: YieldStep = { tag: "yield" }
export function importFileStep(filename: string): ImportFileStep {
  return { tag: "import-file", filename }
}

// a fiber is a concurrent thread of execution
// not named thread because we can't multithread in javascript, but we can use async/await to achieve similar results
export class Fiber {
  topLevelEnv: Env = Env.empty
  frames: Frame[] = []
  lastResult: Value | null = null

  #prog: Stmt[]
  #currStmtIdx = 0
  #isProcessingBlk = false
  #maxCallStackDepth = 10_000
  #isRunning = true

  constructor(prog: Prog) {
    this.#prog = prog
  }

  /** Pauses this fiber's execution */
  pause(): void {
    this.#isRunning = false
  }

  /** Resumes this fiber's execution */
  resume(): void {
    this.#isRunning = true
  }

  /** @returns true iff this fiber is marked as running */
  get isRunning(): boolean {
    return this.#isRunning
  }

  /**
   * Completes one execution step.
   * If the completed step also completes a statement, it will move on to the next statement.
   * @returns true if the step is a major step of execution, false if it's a minor step (e.g. variable loading)
   */
  step(): StepResult {
    if (!this.#isRunning) {
      throw new ICE(
        "Fiber.step",
        "Attempted to step fiber when it is paused!",
      )
    }
    const currStmt = this.#prog.at(this.#currStmtIdx)
    if (!currStmt) {
      throw new ICE("Fiber.step", "Attempted to step but no statements remain!")
    }
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
    this.frames = []
    this.#currStmtIdx++
    this.#isProcessingBlk = false
  }
  get isProcessingBlk() {
    return this.#isProcessingBlk
  }
  isDone(): boolean {
    return this.#currStmtIdx >= this.#prog.length
  }
  // TODO: this may be unnecessary later
  get lastStatement(): Stmt {
    const stmt = this.#prog.at(this.#currStmtIdx - 1)
    if (!stmt) {
      throw new ICE(
        "Fiber.lastStatement",
        `Attempted to get the last completed statement in fiber when none exist at index ${(this.#currStmtIdx - 1).toString()}`,
      )
    }
    return stmt
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
    return this.frames.at(-1)
  }
  pushFrame(frame: Frame) {
    if (this.frames.length >= this.#maxCallStackDepth) {
      throw new ScamperError(
        "Runtime",
        `Max call stack depth ${this.#maxCallStackDepth.toString()} exceeded!`,
      )
    }
    this.frames.push(frame)
  }
  popFrame() {
    this.frames.pop()
  }
  replaceFrame(frame: Frame) {
    this.popFrame()
    this.pushFrame(frame)
  }
  hasFramesRemaining() {
    return this.frames.length > 0
  }
  /**
   * @returns the output of the current frame
   * @throws if the frame was completed in a bad state
   */
  private completeCurrentFrame() {
    const currFrame = this.currentFrame
    if (!currFrame) {
      throw new ICE(
        "Fiber.completeCurrentFrame",
        "Attempted to complete a frame when none remain",
      )
    }
    if (currFrame.values.length !== 1) {
      throw new ICE(
        "Fiber.stepFrame",
        `Frame must finish with exactly one value on the stack, finished with ${currFrame.values.length.toString()} instead`,
      )
    }
    const ret = currFrame.values.pop()
    this.popFrame()
    if (this.hasFramesRemaining()) {
      this.currentFrame.values.push(ret)
    } else {
      this.lastResult = ret
    }
  }
  /**
   * Steps through one operation in the current frame.
   * @returns true if the step was a major step, false if it was a minor step
   */
  stepFrame(): StepResult {
    if (!this.currentFrame) {
      throw new ScamperError(
        "Runtime",
        "Attempted to step stack frame when none exist!",
      )
    }

    // handle op and save if it was a major step or not
    const currOp = this.currentFrame.popInstr()
    let isMajorStep: StepResult
    switch (currOp.tag) {
      case "lit":
        isMajorStep = LitHandler(currOp, this.currentFrame, this)
        break
      case "var":
        isMajorStep = VarHandler(currOp, this.currentFrame, this)
        break
      case "ctor":
        isMajorStep = CtorHandler(currOp, this.currentFrame, this)
        break
      case "cls":
        isMajorStep = ClsHandler(currOp, this.currentFrame, this)
        break
      case "ap":
        isMajorStep = ApHandler(currOp, this.currentFrame, this)
        break
      case "match":
        isMajorStep = MatchHandler(currOp, this.currentFrame, this)
        break
      case "popv":
        isMajorStep = PopVHandler(currOp, this.currentFrame, this)
        break
      case "rept":
        isMajorStep = ReptHandler(currOp, this.currentFrame, this)
        break
      // TODO: the following instructions are useless
      // should be removed later
      case "raise":
      case "pops":
        throw new ICE("Fiber.stepFrame", `${currOp.tag} is deprecated!`)
    }

    if (this.currentFrame.isFinished()) {
      this.completeCurrentFrame()
    }
    return isMajorStep
  }

  /* Library importing helper functions */
  loadLib(libName: string): StepResult {
    const builtin = builtinLibs.get(libName)
    if (builtin) {
      // TODO: for simplicity's sake, we assume that all built-in libs
      // initializers have been invoked already. The only built-in lib
      // with an initializer at this point is the music lib, so we might
      // want to remove initializers altogether and do something for
      // the music lib instead.
      this.topLevelEnv = this.topLevelEnv.extendWithImport(libName, builtin)
      return traceStep
    } else {
      // ...otherwise, we're loading a user file as a lib. The libname
      // should correspond to the exact name of the source file. This will
      // be handled by the scheduler who will, ultimately, pause the current
      // fiber and execute an asynchronous load of that file.
      return importFileStep(libName)
    }
  }
}
