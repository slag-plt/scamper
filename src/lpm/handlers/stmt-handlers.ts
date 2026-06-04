import { Fiber } from "../fiber"
import { Stmt } from "../lang"

type StatementHandler<T extends Stmt["tag"]> = (
  stmt: Extract<Stmt, { tag: T }>,
  fiber: Fiber,
) => Promise<boolean> | boolean

export const ImportHandler: StatementHandler<"import"> = async (stmt, fiber) => {
  await fiber.loadLib(stmt.name)
  fiber.advanceStmt()
  return true
}
export const DefineHandler: StatementHandler<"define"> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return false
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  fiber.topLevelEnv.set(stmt.name, fiber.lastResult)
  fiber.advanceStmt()
  return true
}
export const DispHandler: StatementHandler<"disp"> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return false
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // execute should know that lastResult is the value to be printed, and print it to the output channel
  // so, do nothing
  fiber.advanceStmt()
  return true
}
export const StmtExpHandler: StatementHandler<"stmtexp"> = (stmt, fiber) => {
  if (!fiber.isProcessingBlk) {
    fiber.beginProcessingBlk(stmt.expr)
    return false
  }
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // do nothing with the result, just advance to the next statement
  fiber.advanceStmt()
  return true
}
