import { Blk, Env, Prog, Stmt, Value } from './lang'
import {
  DefineHandler,
  DispHandler,
  ImportHandler,
  StmtExpHandler,
} from './handlers/stmt-handlers'
import { Frame } from './frame'
import { ICE, ScamperError } from './error'
import { Range } from './range'
import {
  ApHandler,
  applyFn,
  ApplyHandler,
  ClsHandler,
  CtorHandler,
  ErrorHandler,
  JsVarHandler,
  LitHandler,
  MatchHandler,
  PopHandlerHandler,
  PopVHandler,
  PushHandlerHandler,
  ReptHandler,
  VarHandler,
} from './handlers/op-handlers'
import { builtinLibs } from './builtin-registry.js'

/**
 * A record of an installed exception handler (from a `with-handler` form). See
 * Fiber.handleError for how these are consumed on a raised ScamperError.
 */
export interface HandlerRecord {
  // frame-stack length to unwind to (the frame that installed the handler stays).
  frameDepth: number
  // the installing frame's value-stack length just after the handler was pushed;
  // on error we reset the stack to `valDepth - 1`, dropping the handler value.
  valDepth: number
  handler: Value
}


export interface DisplayStep { tag: 'display' }
export interface TraceStep { tag: 'trace' }
export interface MinorStep { tag: 'minor' }
export interface YieldStep { tag: 'yield' }
export interface ImportFileStep {
  tag: 'import-file'
  filename: string
}
// A fiber suspended mid-expression by a blocking primitive (see SuspendSignal).
// The scheduler runs `action`, then resumes the fiber with the resolved value
// (Fiber.resumeWithValue).
export interface BlockOnStep {
  tag: 'block-on'
  action: () => Promise<Value>
}

export type StepResult =
  DisplayStep |
  TraceStep |
  MinorStep |
  YieldStep |
  ImportFileStep |
  BlockOnStep

export const displayStep: DisplayStep = { tag: 'display' }
export const traceStep: TraceStep = { tag: 'trace' }
export const minorStep: MinorStep = { tag: 'minor' }
export const yieldStep: YieldStep = { tag: 'yield' }
export function importFileStep(filename: string): ImportFileStep {
  return { tag: 'import-file', filename }
}
export function blockOnStep(action: () => Promise<Value>): BlockOnStep {
  return { tag: 'block-on', action }
}

// a fiber is a concurrent thread of execution
// not named thread because we can't multithread in javascript, but we can use async/await to achieve similar results
export class Fiber {
  topLevelEnv: Env
  frames: Frame[] = []
  lastResult: Value | null = null
  // Stack of installed exception handlers, innermost last. Pushed by the
  // push-handler op, popped by pop-handler (normal completion) or handleError
  // (on a raised error).
  handlerStack: HandlerRecord[] = []

  private prog: Stmt[]
  private currStmtIdx = 0
  private _isProcessingBlk = false
  private maxCallStackDepth = 10_000

  constructor(prog: Prog, topLevelEnv: Env = Env.empty) {
    this.prog = prog
    this.topLevelEnv = topLevelEnv
  }

  /**
   * Completes one execution step.
   * If the completed step also completes a statement, it will move on to the next statement.
   * @returns true if the step is a major step of execution, false if it's a minor step (e.g. variable loading)
   */
  step(): StepResult {
    const currStmt = this.prog.at(this.currStmtIdx)
    if (!currStmt) {
      throw new ICE('Fiber.step', 'Attempted to step but no statements remain!')
    }
    switch (currStmt.tag) {
      case 'import':
        return ImportHandler(currStmt, this)
      case 'define':
        return DefineHandler(currStmt, this)
      case 'disp':
        return DispHandler(currStmt, this)
      case 'stmtexp':
        return StmtExpHandler(currStmt, this)
    }
  }

  /* Statement execution helper functions */
  advanceStmt() {
    this.frames = []
    this.currStmtIdx++
    this._isProcessingBlk = false
  }

  get isProcessingBlk() {
    return this._isProcessingBlk
  }

  isDone(): boolean {
    return this.currStmtIdx >= this.prog.length
  }

  // TODO: this may be unnecessary later
  get lastStatement(): Stmt {
    const stmt = this.prog.at(this.currStmtIdx - 1)
    if (!stmt) {
      throw new ICE(
        'Fiber.lastStatement',
        `Attempted to get the last completed statement in fiber when none exist at index ${(this.currStmtIdx - 1).toString()}`,
      )
    }
    return stmt
  }

  // Populate the stack frames for a Blk statement, and set the isProcessingBlk flag to true
  beginProcessingBlk(expr: Blk) {
    this._isProcessingBlk = true

    // populate stack frames
    this.pushFrame(
      new Frame(
        `##stmt-${this.currStmtIdx.toString()}##`,
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
    if (this.frames.length >= this.maxCallStackDepth) {
      throw new ScamperError(
        'Runtime',
        `Max call stack depth ${this.maxCallStackDepth.toString()} exceeded!`,
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
        'Fiber.completeCurrentFrame',
        'Attempted to complete a frame when none remain',
      )
    }
    if (currFrame.values.length !== 1) {
      throw new ICE(
        'Fiber.stepFrame',
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
   * Attempts to recover from a raised runtime error using the innermost
   * installed exception handler (from a `with-handler` form). Unwinds the frame
   * stack back to the installing frame, discards the failed sub-computation, and
   * schedules the handler applied to the error's message string.
   * @returns true if a handler caught the error (execution should resume), false
   *          if no handler was installed (the caller should report the error).
   */
  handleError(e: ScamperError): boolean {
    const rec = this.handlerStack.pop()
    if (rec === undefined) {
      return false
    }
    // Discard the failed sub-computation's frames, landing back on the frame
    // that installed the handler.
    this.frames.length = rec.frameDepth
    const target = this.currentFrame
    if (target === undefined) {
      throw new ICE(
        'Fiber.handleError',
        'Handler unwound past the bottom of the frame stack',
      )
    }
    // The installing frame's next op is the matching pop-handler (its guarded
    // `ap` already ran). Discard it -- we're taking the error path, not
    // completing normally.
    const pending = target.popInstr()
    if (pending.tag !== 'pop-handler') {
      throw new ICE(
        'Fiber.handleError',
        `Expected a pop-handler op while unwinding, found ${pending.tag}`,
      )
    }
    // Drop the handler value (and anything the failed computation left), so the
    // stack is exactly as it was before the with-handler form began.
    target.values.length = rec.valDepth - 1
    // Apply the handler to the error's message string (matching the historical
    // prelude_withHandler contract). Its result becomes the form's value.
    applyFn(rec.handler, [e.message], target, this, e.range ?? Range.none)
    return true
  }

  /**
   * Resumes a fiber suspended mid-expression by a blocking primitive (see
   * SuspendSignal / BlockOnStep), delivering `value` as the suspended
   * primitive-call's result. Mirrors exactly what applyFn's JsFunction branch
   * (push the result) plus stepFrame's post-op finalize (complete the frame if
   * it is now finished) would have done had the primitive returned synchronously.
   */
  resumeWithValue(value: Value): void {
    const frame = this.currentFrame
    if (frame === undefined) {
      throw new ICE(
        'Fiber.resumeWithValue',
        'Attempted to resume a fiber with no current frame',
      )
    }
    frame.values.push(value)
    if (frame.isFinished()) {
      this.completeCurrentFrame()
    }
  }

  /**
   * Steps through one operation in the current frame.
   * @returns true if the step was a major step, false if it was a minor step
   */
  stepFrame(): StepResult {
    if (!this.currentFrame) {
      throw new ScamperError(
        'Runtime',
        'Attempted to step stack frame when none exist!',
      )
    }

    // handle op and save if it was a major step or not
    const currOp = this.currentFrame.popInstr()
    let isMajorStep: StepResult
    switch (currOp.tag) {
      case 'lit':
        isMajorStep = LitHandler(currOp, this.currentFrame, this)
        break
      case 'var':
        isMajorStep = VarHandler(currOp, this.currentFrame, this)
        break
      case 'ctor':
        isMajorStep = CtorHandler(currOp, this.currentFrame, this)
        break
      case 'cls':
        isMajorStep = ClsHandler(currOp, this.currentFrame, this)
        break
      case 'ap':
        isMajorStep = ApHandler(currOp, this.currentFrame, this)
        break
      case 'match':
        isMajorStep = MatchHandler(currOp, this.currentFrame, this)
        break
      case 'popv':
        isMajorStep = PopVHandler(currOp, this.currentFrame, this)
        break
      case 'rept':
        isMajorStep = ReptHandler(currOp, this.currentFrame, this)
        break
      case 'jsvar':
        isMajorStep = JsVarHandler(currOp, this.currentFrame, this)
        break
      case 'error':
        isMajorStep = ErrorHandler(currOp, this.currentFrame, this)
        break
      case 'apply':
        isMajorStep = ApplyHandler(currOp, this.currentFrame, this)
        break
      case 'push-handler':
        isMajorStep = PushHandlerHandler(currOp, this.currentFrame, this)
        break
      case 'pop-handler':
        isMajorStep = PopHandlerHandler(currOp, this.currentFrame, this)
        break
      // TODO: the following instructions are useless
      // should be removed later
      case 'raise':
      case 'pops':
        throw new ICE('Fiber.stepFrame', `${currOp.tag} is deprecated!`)
    }

    if (this.currentFrame.isFinished()) {
      this.completeCurrentFrame()
    }
    return isMajorStep
  }

  /* Module importing helper functions */
  loadModule(name: string, kind: 'builtin' | 'file'): StepResult {
    if (kind === 'builtin') {
      const builtin = builtinLibs.get(name)
      if (!builtin) {
        throw new ScamperError('Runtime', `No such built-in library: ${name}`)
      }
      // TODO: for simplicity's sake, we assume that all built-in libs
      // initializers have been invoked already. The only built-in lib
      // with an initializer at this point is the music lib, so we might
      // want to remove initializers altogether and do something for
      // the music lib instead.
      this.topLevelEnv = this.topLevelEnv.extendWithImport(name, builtin)
      return traceStep
    } else {
      // ...otherwise, we're loading a user file as a lib. The libname
      // should correspond to the exact name of the source file. This will
      // be handled by the scheduler who will, ultimately, pause the current
      // fiber and execute an asynchronous load of that file.
      return importFileStep(name)
    }
  }
}
