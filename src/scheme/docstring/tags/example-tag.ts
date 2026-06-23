import { tokenizeAndParse } from "../.."
import { Range, Value } from "../../../lpm"
import { SimpleErrorChannel } from "../../../lpm/output/simple-error"
import { App, isApp, isLit, isStmtExp, Stmt } from "../../ast"
import { mkScamperErrorWithRange } from "../../util"
import { DocTag, registerDocTagParser } from "./tag"

interface Example {
  functionCall: App
  result: Value
}
export type ExampleTag = DocTag<Example>

const separator = " -> "

function exampleTagError(
  message: string,
  range: Range,
  errChannel?: SimpleErrorChannel,
): never {
  const cause =
    errChannel !== undefined && errChannel.errors.length > 0
      ? `, ${errChannel.errors[0].message}`
      : ""
  throw mkScamperErrorWithRange(
    "Parser",
    `Error in @example tag: ${message}${cause}`,
    range,
  )
}

function parseExampleExpression(
  source: string,
  range: Range,
  field: "function call" | "result",
): Stmt {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, source.trim())
  if (!parsed || errChannel.errors.length > 0) {
    exampleTagError(`${field} is malformed`, range, errChannel)
  }
  if (parsed.length < 1) {
    exampleTagError(`${field} is missing`, range)
  }
  if (parsed.length > 1) {
    exampleTagError(`more than one expression found in ${field}`, range)
  }
  return parsed[0]
}

registerDocTagParser("@example", (contents, range): ExampleTag => {
  const splitContents = contents.split(separator)
  if (splitContents.length < 2) {
    exampleTagError('missing separator, expected "expression -> result"', range)
  }
  const functionCallStr = splitContents[0]
  const resultStr = splitContents.slice(1).join(separator)

  const parsedFunctionCall = parseExampleExpression(
    functionCallStr,
    range,
    "function call",
  )
  if (!isStmtExp(parsedFunctionCall)) {
    exampleTagError("function call should be an expression", range)
  }
  const functionCall = parsedFunctionCall.expr
  if (!isApp(functionCall)) {
    exampleTagError("function call should be an application expression", range)
  }

  const parsedResult = parseExampleExpression(resultStr, range, "result")
  if (!isStmtExp(parsedResult)) {
    exampleTagError("result should be an expression", range)
  }
  if (!isLit(parsedResult.expr)) {
    exampleTagError("result should be a literal value", range)
  }
  const result = parsedResult.expr.value

  // TODO: semantic validation against the documented function happens in scopeCheckFunctionDoc

  return {
    tag: "@example",
    contents: {
      functionCall,
      result,
    },
    range,
  }
})
