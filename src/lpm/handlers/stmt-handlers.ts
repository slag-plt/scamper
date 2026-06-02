import { StatementHandler } from "../fiber"

export const ImportHandler: StatementHandler<"import"> = async (stmt, fiber) => {
  await fiber.loadLib(stmt.name)
  fiber.advanceStmt()
  return true
}
export const DefineHandler: StatementHandler<"define"> = async (stmt, fiber) => {
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
export const DispHandler: StatementHandler<"disp"> = async (stmt, fiber) => {
  fiber.beginProcessingBlk(stmt.expr)
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // execute should know that lastResult is the value to be printed, and print it to the output channel
  // so, do nothing
  fiber.advanceStmt() // pops trace
  return true
}
export const StmtExpHandler: StatementHandler<"stmtexp"> = async (stmt, fiber) => {
  fiber.beginProcessingBlk(stmt.expr)
  if (fiber.hasFramesRemaining()) {
    return fiber.stepFrame()
  }
  // do nothing with the result, just advance to the next statement
  fiber.advanceStmt()
  return true
}
