import { displayStep, Fiber, StepResult, stmtValueStep, traceStep } from '../fiber'
import { Stmt } from '../lang'
import { isClosure, isFunction } from '../util'

type StatementHandler<T extends Stmt['tag']> = (
  stmt: Extract<Stmt, { tag: T }>,
  fiber: Fiber,
) => StepResult

export const ImportHandler: StatementHandler<'import'> = (stmt, fiber) => {
  const result = fiber.loadModule(stmt.name, stmt.kind, stmt.alias)
  if (result.tag === 'trace') {
    fiber.advanceStmt()
  }
  return result
}
export const ExportHandler: StatementHandler<'export'> = (stmt, fiber) => {
  // Records the exported names; the module snapshot (Fiber.getModule) restricts
  // its bindings to them. Introduces no binding and produces no value.
  fiber.addExports(stmt.names)
  fiber.advanceStmt()
  return traceStep
}
export const DefineHandler: StatementHandler<'define'> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return traceStep
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // N.B., every lambda is compiled with the placeholder name "##anonymous##"
  // (codegen has no notion of "this expression happens to be a define's
  // value") -- so a defined closure only picks up a real, Scamper-facing
  // name here, at the point it's actually bound to one. Only the first
  // binding wins: an alias (`(define g f)`) must not rename the closure out
  // from under whichever name it already has, since it's the same shared
  // object.
  if (
    isClosure(fiber.lastResult) &&
    (fiber.lastResult.name === undefined || fiber.lastResult.name === '##anonymous##')
  ) {
    fiber.lastResult.name = stmt.name
    // A contract wrapper and the value it checks are two closures for one
    // definition, so they answer to the same name: an error raised inside the
    // wrapped function is still about the student's call to *this* name, and
    // applyFn reports a frame by its name (see Closure.contractTarget).
    const target = fiber.lastResult.contractTarget
    if (
      isClosure(target) &&
      (target.name === undefined || target.name === '##anonymous##')
    ) {
      target.name = stmt.name
    }
  }
  const value = fiber.lastResult
  fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([stmt.name, value])
  fiber.advanceStmt()
  // The defined value is the last step of the define's trace (#568): without it
  // the trace stops on the reduction just before, so `(define y (sqr (+ 2 3)))`
  // ends at `(* 5 5)`. A function is the exception -- the step before already
  // showed its lambda, and naming it again says nothing.
  return isFunction(value) ? traceStep : stmtValueStep
}
export const DispHandler: StatementHandler<'disp'> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return traceStep
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // execute should know that lastResult is the value to be printed, and print it to the output channel
  // so, do nothing
  fiber.advanceStmt()
  return displayStep
}
export const StmtExpHandler: StatementHandler<'stmtexp'> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return traceStep
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // do nothing with the result, just advance to the next statement
  fiber.advanceStmt()
  return traceStep
}
