import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import { tokenizeAndParse } from "../index"
import { isStmtExp } from "../ast"
import { DocComment, isPred, isVarApp, Pred, VarApp } from "./docstring"
import { mkScamperErrorWithRange } from "../util"
import { Range } from "../../lpm"

// originally authored by @bacracm, refactored to new file

export interface Signature {
  function: VarApp
  predicate: Pred
  range: Range
}

function parseFunctionSignature({ line, range }: DocComment): VarApp {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, line)
  if (line.startsWith(" ")) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Function signature cannot start with whitespace`,
      range,
    )
  }
  if (!parsed || errChannel.errors.length > 0) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Malformed function signature`,
      range,
    )
  }
  if (parsed.length < 1) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Function signature is missing`,
      range,
    )
  }
  if (parsed.length > 1) {
    throw mkScamperErrorWithRange(
      "Parser",
      `More than one function signature found`,
      range,
    )
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Not a function signature. Expected an expression`,
      range,
    )
  }
  if (!isVarApp(parsedStmt.expr)) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Not a function signature. Expected an application of a variable with variable arguments`,
      range,
    )
  }
  const funct = parsedStmt.expr
  // TODO: more granular range is possible
  funct.range = range
  return funct
}

function parseContractSignature({ line, range }: DocComment): Pred {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, line)
  if (!parsed || errChannel.errors.length > 0 || parsed.length > 1) {
    throw mkScamperErrorWithRange("Parser", `Malformed predicate field`, range)
  }
  if (parsed.length < 1) {
    throw mkScamperErrorWithRange("Parser", `Predicate field is missing`, range)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw mkScamperErrorWithRange("Parser", `Not a contract signature`, range)
  }
  if (!isPred(parsedStmt.expr)) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Not a contract signature. Expected a variable or variable application`,
      range,
    )
  }
  const predicate = parsedStmt.expr
  // TODO: more granular range is possible
  predicate.range = range
  return predicate
}

export function parseSignature({
  line: docLine,
  range,
}: DocComment): Signature {
  // Check form function name, space, parameter. (separate function and call it)
  // Check contract.

  // verify no split (?)
  const separator = " -> "
  const [functStr, ...rest] = docLine.split(separator)

  if (docLine.split(separator).length < 2) {
    throw mkScamperErrorWithRange(
      "Parser",
      `Missing separator in doc string signature`,
      range,
    )
  }

  // TODO: more granular range is possible
  const funcComment: DocComment = { line: functStr, range }
  const funct = parseFunctionSignature(funcComment)

  const predStr = rest.join(separator)
  // TODO: more granular range is possible
  const predComment: DocComment = { line: predStr, range }
  const predicate = parseContractSignature(predComment)

  return {
    function: funct,
    predicate,
    range,
  }
}
