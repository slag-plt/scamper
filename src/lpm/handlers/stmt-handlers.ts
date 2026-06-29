import { displayStep, Fiber, StepResult, traceStep } from "../fiber"
import { Stmt } from "../lang"

type StatementHandler<T extends Stmt["tag"]> = (
  stmt: Extract<Stmt, { tag: T }>,
  fiber: Fiber,
) => StepResult

export const ImportHandler: StatementHandler<"import"> = (stmt, fiber) => {
  const result = fiber.loadLib(stmt.name)
  if (result.tag === "trace") {
    fiber.advanceStmt()
  }
  return result
}
export const DefineHandler: StatementHandler<"define"> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return traceStep
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  fiber.topLevelEnv = fiber.topLevelEnv.extendWithTopLevel(stmt.name, fiber.lastResult)
  fiber.advanceStmt()
  return traceStep
}
export const DispHandler: StatementHandler<"disp"> = (stmt, fiber) => {
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
export const StmtExpHandler: StatementHandler<"stmtexp"> = (stmt, fiber) => {
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
