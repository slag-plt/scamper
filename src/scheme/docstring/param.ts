import { isStmtExp, isVar } from "../ast"
import { Range, ScamperError } from "../../lpm"
import { isWhitespace, readSingle, Token } from "../reader"
import { parseIdentifier } from "../parser"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import { tokenizeAndParse } from "../index"
import { catchIf } from "../util"
import { isVarApp, ParseStage, Pred } from "./docstring"

export interface Param {
  name: string
  predicate: Pred
  description?: string
}

export const WhitespaceLocation = {
  Beginning: "beginning",
  PrePredicate: "pre-predicate",
} as const
type WhitespaceLocation =
  (typeof WhitespaceLocation)[keyof typeof WhitespaceLocation]

class ParamParseError extends ScamperError {
  constructor(message: string) {
    super("Parser", `Error while parsing param in doc string: ${message}`)
  }
}

export class ParamWhitespaceError extends ParamParseError {
  constructor(
    public loc: WhitespaceLocation,
    message: string,
  ) {
    super(message)
  }
}

class ParamMissingFieldError extends ParamParseError {}

class ParamMalformedFieldError extends ParamParseError {}

export function parseSingleParam(docLines: string[]): ParseStage | Param {
  const line = docLines.shift()
  if (line === undefined) {
    throw new ScamperError(
      "Parser",
      "Doc lines expected to be not empty when calling parseSingleParam",
    )
  }
  // get param signature
  const parsedSignature = catchIf(
    () => parseParamSignature(line),
    isRecoverableParamParseError,
  )
  if (!parsedSignature) {
    // put the line back and switch to description stage
    docLines.unshift(line)
    return ParseStage.Description
  }
  const { param, beginningWhitespaces } = parsedSignature
  // get optional param description
  // get first description line
  const nextLine = docLines.shift()
  if (nextLine === undefined) {
    throw new ScamperError(
      "Parser",
      "Doc string is missing function description",
    )
  }
  param.description = catchIf(
    () => parseParamDescriptionLine(nextLine, beginningWhitespaces),
    isRecoverableParamParseError,
  )
  if (param.description === undefined) {
    docLines.unshift(nextLine)
    // we might still have more params to look for, so don't change stage
    return param
  }
  while (docLines.length > 0) {
    const nextDescLine = docLines.shift()
    if (nextDescLine === undefined) {
      throw new ScamperError(
        "Parser",
        "Doc string is missing function description",
      )
    }
    const remainingDesc = catchIf(
      () => parseParamDescriptionLine(nextDescLine, beginningWhitespaces),
      isRecoverableParamParseError,
    )
    if (remainingDesc === undefined) {
      docLines.unshift(nextDescLine)
      return param
    }
    param.description += " " + remainingDesc
  }
  // if we broke out of the while loop, we ran out of lines
  throw new ScamperError("Parser", "Doc string is missing function description")
}

const minSignatureBeginningWhitespace = 1
const minPrePredicateWhitespace = 1

interface ParseParamSignatureResult {
  param: Param
  beginningWhitespaces: number
}
/**
 * @returns constructed param object if successful
 * @throws ParamParseError when docLine does not parse into a correct Params object
 */
export function parseParamSignature(
  docLine: string,
): ParseParamSignatureResult {
  // check and chomp beginning whitespace
  const beginningWhitespaces = getLeadingWhitespaceCount(
    docLine,
    minSignatureBeginningWhitespace,
    WhitespaceLocation.Beginning,
  )
  // get param name
  const splitDocLine = docLine.slice(beginningWhitespaces).split(":")
  if (splitDocLine.length < 2) {
    throw new ParamMissingFieldError(
      "Line is missing separating colon between name and predicate",
    )
  }
  const [untrimmedName, ...rest] = splitDocLine
  const postNameDocLine = rest.join(":")
  const errs: ScamperError[] = []
  // TODO: range should actually be populated
  const nameSyn = readSingle(
    new Token(untrimmedName.trimEnd(), Range.none),
    false,
  )
  const name = parseIdentifier(errs, nameSyn)
  if (errs.length > 0) {
    throw new ParamMalformedFieldError(
      `Name field is malformed, ${errs[0].message}`,
    )
  }

  // check and chomp pre-predicate whitespace
  const prePredicateWhitespace = getLeadingWhitespaceCount(
    postNameDocLine,
    minPrePredicateWhitespace,
    WhitespaceLocation.PrePredicate,
  )
  // get predicate
  // two cases for the predicate: either it's a simple predicate identifier i.e. "pred?"
  // or it's a complex predicate expression i.e. "(complexPred? pred1? pred2? ...)"
  const predicateStr = postNameDocLine.slice(prePredicateWhitespace).trim()
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, predicateStr)
  if (!parsed || errChannel.errors.length > 0 || parsed.length > 1) {
    throw new ParamMalformedFieldError(
      `Predicate field is malformed, ${errChannel.errors[0].message}`,
    )
  }
  if (parsed.length < 1) {
    throw new ParamMissingFieldError("Predicate field is missing")
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw new ParamMalformedFieldError("Predicate should be an expression")
  }
  // TODO: range should actually be populated
  if (!isVar(parsedStmt.expr) && !isVarApp(parsedStmt.expr)) {
    throw new ParamMalformedFieldError(
      "Predicate should be either a simple predicate identifier or a complex predicate application",
    )
  }
  const predicate: Pred = parsedStmt.expr
  return { param: { name, predicate }, beginningWhitespaces }
  // predicate signature does not have line
}

const minDescriptionPadding = 1

export function parseParamDescriptionLine(
  docLine: string,
  sigBeginningWhitespaces: number,
): string {
  // check and chomp leading padding
  const padding = getLeadingWhitespaceCount(
    docLine,
    sigBeginningWhitespaces + minDescriptionPadding,
    WhitespaceLocation.Beginning,
  )
  return docLine.slice(padding).trim()
}

function getLeadingWhitespaceCount(
  line: string,
  minWhitespaces: number,
  loc: WhitespaceLocation,
): number {
  let whitespaces = 0
  for (const ch of line) {
    if (isWhitespace(ch)) {
      whitespaces++
      continue
    }
    // ch isn't whitespace then
    if (whitespaces < minWhitespaces) {
      throw new ParamWhitespaceError(
        loc,
        `Line does not have enough ${loc} whitespace, expected at least ${minWhitespaces.toString()} but got ${whitespaces.toString()}`,
      )
    }
    // we're good, break out of this for loop
    break
  }
  return whitespaces
}

function isRecoverableParamParseError(e: unknown) {
  return (
    !(e instanceof ParamWhitespaceError) ||
    e.loc !== WhitespaceLocation.Beginning
  )
}

export function isParam(v: unknown): v is Param {
  return typeof v === "object" && v !== null && "name" in v && "predicate" in v
}
