import { isStmtExp } from "../ast"
import { ICE, Range, ScamperError } from "../../lpm"
import { looksLikeIdentifier } from "../literals.js"
import { reservedWords } from "../reserved-words.js"
import { SimpleErrorChannel } from "../../lpm/output/simple-error"
import { tokenizeAndParse } from "../index"
import { catchIf, mkScamperErrorWithRange } from "../util"
import { DocComment, isPred, ParseStage, Pred } from "./docstring"

const isWhitespace = (c: string): boolean => /\s/.test(c)

export interface Param {
  name: string
  predicate: Pred
  description?: string
  range: Range
}

export const WhitespaceLocation = {
  Beginning: "beginning",
  PrePredicate: "pre-predicate",
} as const
type WhitespaceLocation =
  (typeof WhitespaceLocation)[keyof typeof WhitespaceLocation]

class ParamParseError extends ScamperError {
  constructor(
    message: string,
    public range: Range,
  ) {
    super("Parser", `Error while parsing param in doc string: ${message}`)
  }
}

export class ParamWhitespaceError extends ParamParseError {
  constructor(
    public loc: WhitespaceLocation,
    message: string,
    range: Range,
  ) {
    super(message, range)
  }
}

class ParamMissingFieldError extends ParamParseError {}

class ParamMalformedFieldError extends ParamParseError {}

export function parseSingleParam(
  docComments: DocComment[],
): ParseStage | Param {
  const firstComment = docComments.shift()
  if (firstComment === undefined) {
    throw new ICE(
      "Docstring.parseSingleParam",
      "Doc lines expected to be not empty when calling parseSingleParam",
    )
  }
  // get param signature
  const parsedSignature = catchIf(
    () => parseParamSignature(firstComment),
    isRecoverableParamParseError,
  )
  if (!parsedSignature) {
    // put the line back and switch to description stage
    docComments.unshift(firstComment)
    return ParseStage.Description
  }
  const { param, beginningWhitespaces } = parsedSignature
  // get optional param description
  // get first description line
  const firstDescComment = docComments.shift()
  if (firstDescComment === undefined) {
    throw mkScamperErrorWithRange(
      "Parser",
      "Doc string is missing function description",
      firstComment.range,
    )
  }
  param.description = catchIf(
    () => parseParamDescriptionLine(firstDescComment, beginningWhitespaces),
    isRecoverableParamParseError,
  )
  if (param.description === undefined) {
    docComments.unshift(firstDescComment)
    // we might still have more params to look for, so don't change stage
    return param
  }
  let lastRange: Range = firstDescComment.range
  while (docComments.length > 0) {
    const nextDescComment = docComments.shift()
    if (nextDescComment === undefined) {
      throw mkScamperErrorWithRange(
        "Parser",
        "Doc string is missing function description",
        firstDescComment.range,
      )
    }
    lastRange = nextDescComment.range
    const remainingDesc = catchIf(
      () => parseParamDescriptionLine(nextDescComment, beginningWhitespaces),
      isRecoverableParamParseError,
    )
    if (remainingDesc === undefined) {
      docComments.unshift(nextDescComment)
      return param
    }
    param.description += " " + remainingDesc
  }
  // if we broke out of the while loop, we ran out of lines
  throw mkScamperErrorWithRange(
    "Parser",
    "Doc string is missing function description",
    lastRange,
  )
}

const minSignatureBeginningWhitespace = 1
const minPrePredicateWhitespace = 1

interface ParseParamSignatureResult {
  param: Param
  beginningWhitespaces: number
}
/**
 * @returns constructed param object if successful
 * @throws ParamParseError when line does not parse into a correct Params object
 */
export function parseParamSignature(
  docComment: DocComment,
): ParseParamSignatureResult {
  // check and chomp beginning whitespace
  const beginningWhitespaces = getLeadingWhitespaceCount(
    docComment,
    minSignatureBeginningWhitespace,
    WhitespaceLocation.Beginning,
  )
  const { line, range } = docComment
  // get param name
  const splitDocLine = line.slice(beginningWhitespaces).split(":")
  if (splitDocLine.length < 2) {
    throw new ParamMissingFieldError(
      "Line is missing separating colon between name and predicate",
      range,
    )
  }
  const [untrimmedName, ...rest] = splitDocLine
  const postNameDocLine = rest.join(":")
  const trimmedName = untrimmedName.trimEnd()
  const errs: ScamperError[] = []
  if (!looksLikeIdentifier(trimmedName)) {
    errs.push(
      new ScamperError("Parser", "Expected an identifier", undefined, Range.none),
    )
  } else if (trimmedName.startsWith("_")) {
    errs.push(
      new ScamperError(
        "Parser",
        'Identifiers cannot begin with "_" unless inside of "section" or patterns',
        undefined,
        Range.none,
      ),
    )
  } else if (reservedWords.includes(trimmedName)) {
    errs.push(
      new ScamperError(
        "Parser",
        `The identifier "${trimmedName}" is a reserved word and cannot be used as a variable name`,
        undefined,
        Range.none,
      ),
    )
  }
  const name = errs.length > 0 ? "<error>" : trimmedName
  if (errs.length > 0) {
    throw new ParamMalformedFieldError(
      `Name field is malformed, ${errs[0].message}`,
      range,
    )
  }

  // check and chomp pre-predicate whitespace
  const prePredicateWhitespace = getLeadingWhitespaceCount(
    // TODO: should get more granular ranges in the future
    { line: postNameDocLine, range },
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
      `Predicate field is malformed${errChannel.errors.length > 0 ? ", " + errChannel.errors[0].message : ""}`,
      range,
    )
  }
  if (parsed.length < 1) {
    throw new ParamMissingFieldError("Predicate field is missing", range)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw new ParamMalformedFieldError(
      "Predicate should be an expression",
      range,
    )
  }

  if (!isPred(parsedStmt.expr)) {
    throw new ParamMalformedFieldError(
      "Predicate should be either a simple predicate identifier or a complex predicate application",
      range,
    )
  }
  const predicate: Pred = parsedStmt.expr
  // TODO: more granular range is possible
  predicate.range = range
  return { param: { name, predicate, range }, beginningWhitespaces }
  // predicate signature does not have line
}

const minDescriptionPadding = 1

export function parseParamDescriptionLine(
  docComment: DocComment,
  sigBeginningWhitespaces: number,
): string {
  // check and chomp leading padding
  const padding = getLeadingWhitespaceCount(
    docComment,
    sigBeginningWhitespaces + minDescriptionPadding,
    WhitespaceLocation.Beginning,
  )
  return docComment.line.slice(padding).trim()
}

function getLeadingWhitespaceCount(
  { line, range }: DocComment,
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
        range,
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
