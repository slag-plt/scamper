import { Exp, isStmtExp } from "./ast"
import { Range, ScamperError } from "../lpm"
import { isWhitespace, readSingle, Token } from "./reader"
import { parseIdentifier } from "./parser"
import { SimpleErrorChannel } from "../lpm/output/simple-error"
import { tokenizeAndParse } from "./index"

export interface Param {
  name: string
  predicate: Exp
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

const minBeginningWhitespace = 1
const minPrePredicateWhitespace = 1

/**
 * @returns constructed param object if successful
 * @throws ParamParseError when docLine does not parse into a correct Params object
 */
export function parseParamSignature(docLine: string): Param {
  // check and chomp beginning whitespace
  const beginningWhitespaces = getLeadingWhitespaceCount(
    docLine,
    minBeginningWhitespace,
    WhitespaceLocation.Beginning,
  )
  // get param name
  const splitDocLine = docLine.slice(beginningWhitespaces).split(":", 2)
  if (splitDocLine.length < 2) {
    throw new ParamMissingFieldError(
      "Line is missing separating colon between name and predicate",
    )
  }
  const [untrimmedName, postNameDocLine] = splitDocLine
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
  const predicate = parsedStmt.expr
  return { name, predicate }
  // predicate signature does not have line
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
