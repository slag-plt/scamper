import { displayStep, Fiber, StepResult, traceStep } from '../fiber'
import { Stmt } from '../lang'
import { isClosure } from '../util'

type StatementHandler<T extends Stmt['tag']> = (
  stmt: Extract<Stmt, { tag: T }>,
  fiber: Fiber,
) => StepResult

export const ImportHandler: StatementHandler<'import'> = (stmt, fiber) => {
  const result = fiber.loadModule(stmt.name, stmt.kind)
  if (result.tag === 'trace') {
    fiber.advanceStmt()
  }
  return result
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
  }
  fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel([stmt.name, fiber.lastResult])
  fiber.advanceStmt()
  return traceStep
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
