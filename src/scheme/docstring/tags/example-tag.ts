import { tokenizeAndParse } from '../..'
import { Range, Value } from '../../../lpm'
import { App, isApp, isLit, isStmtExp, Stmt } from '../../ast'
import { mkDocError } from '../error'
import { DocTag, registerDocTagParser } from './tag'

interface Example {
  functionCall: App
  result: Value
}
export type ExampleTag = DocTag<Example>

export function isExampleTag(t: DocTag): t is ExampleTag {
  return (
    typeof t.contents === 'object' &&
    t.contents !== null &&
    'functionCall' in t.contents &&
    'result' in t.contents
  )
}

const separator = ' -> '

function exampleTagError(
  message: string,
  range: Range,
  cause?: string,
): never {
  throw mkDocError(
    `Error in @example tag: ${message}${cause ? `, ${cause}` : ''}`,
    range,
  )
}

function parseExampleExpression(
  source: string,
  range: Range,
  field: 'function call' | 'result',
): Stmt {
  const { program: parsed, diagnostics } = tokenizeAndParse(source.trim())
  if (!parsed || diagnostics.length > 0) {
    exampleTagError(`${field} is malformed`, range, diagnostics[0]?.message)
  }
  if (parsed.length < 1) {
    exampleTagError(`${field} is missing`, range)
  }
  if (parsed.length > 1) {
    exampleTagError(`more than one expression found in ${field}`, range)
  }
  const toReturn = parsed[0]
  toReturn.range = range
  return toReturn
}

registerDocTagParser('@example', (contents, range): ExampleTag => {
  const splitContents = contents.split(separator)
  if (splitContents.length < 2) {
    exampleTagError('missing separator, expected "expression -> result"', range)
  }
  const functionCallStr = splitContents[0]
  const resultStr = splitContents.slice(1).join(separator)

  const parsedFunctionCall = parseExampleExpression(
    functionCallStr,
    range,
    'function call',
  )
  if (!isStmtExp(parsedFunctionCall)) {
    exampleTagError('function call should be an expression', range)
  }
  const functionCall = parsedFunctionCall.expr
  if (!isApp(functionCall)) {
    exampleTagError('function call should be an application expression', range)
  }

  const parsedResult = parseExampleExpression(resultStr, range, 'result')
  if (!isStmtExp(parsedResult)) {
    exampleTagError('result should be an expression', range)
  }
  if (!isLit(parsedResult.expr)) {
    exampleTagError('result should be a literal value', range)
  }
  const result = parsedResult.expr.value

  // TODO: semantic validation against the documented function happens in scopeCheckFunctionDoc

  return {
    tag: '@example',
    contents: {
      functionCall,
      result,
    },
    range,
  }
})
