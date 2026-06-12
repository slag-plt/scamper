import { ScamperError } from "../../lpm"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import { tokenizeAndParse } from "../index"
import { isStmtExp } from "../ast"
import { isPred, isVarApp, Pred, VarApp } from "./docstring"

export interface Signature {
  // Function should get changed to App at some point
  function: VarApp
  predicate: Pred
}

function parseFunctionSignature(docLine: string): VarApp {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, docLine)
  if (docLine.startsWith(" ")) {
    throw new ScamperError(
      "Parser",
      `Function signature cannot start with whitespace`,
    )
  }
  if (!parsed || errChannel.errors.length > 0) {
    throw new ScamperError("Parser", `Malformed function signature`)
  }
  if (parsed.length < 1) {
    throw new ScamperError("Parser", `Function signature is missing`)
  }
  if (parsed.length > 1) {
    throw new ScamperError("Parser", `More than one function signature found`)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw new ScamperError(
      "Parser",
      `Not a function signature. Expected an expression`,
    )
  }
  if (!isVarApp(parsedStmt.expr)) {
    throw new ScamperError(
      "Parser",
      `Not a function signature. Expected an application of a variable with variable arguments`,
    )
  }
  const funct = parsedStmt.expr
  return funct
}

function parseContractSignature(docLine: string): Pred {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, docLine)
  if (!parsed || errChannel.errors.length > 0 || parsed.length > 1) {
    throw new ScamperError("Parser", `Malformed predicate field`)
  }
  if (parsed.length < 1) {
    throw new ScamperError("Parser", `Predicate field is missing`)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw new ScamperError("Parser", `Not a contract signature`)
  }
  if (!isPred(parsedStmt.expr)) {
    throw new ScamperError(
      "Parser",
      `Not a contract signature. Expected a variable or variable application`,
    )
  }
  const predicate = parsedStmt.expr
  return predicate
}

export function parseSignature(docLine: string): Signature {
  // Check form function name, space, parameter. (separate function and call it)
  // Check contract.

  // verify no split (?)
  const separator = " -> "
  const [functStr, ...rest] = docLine.split(separator)
  const predStr = rest.join(separator)

  if (docLine.split(separator).length < 2) {
    throw new ScamperError(
      "Parser",
      `Missing separator in doc string signature`,
    )
  }
  const funct = parseFunctionSignature(functStr)
  const predicate = parseContractSignature(predStr)

  return {
    function: funct,
    predicate: predicate,
  }
}
