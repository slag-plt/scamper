import { Range, ScamperError } from "../lpm"
import { Comment, isWhitespace, readSingle, Token } from "./reader"
import { Exp } from "./ast"
import { parseExp, parseIdentifier } from "./parser"

type Signature = unknown
export interface Param {
  name: string
  predicate: Exp
  description?: string
}
type Params = Param[]
type Description = unknown
type Tags = unknown[]
interface CommentStruct {
  signature: Signature
  params: Params
  description: Description
  tags: Tags
}

const ParseStage = {
  Params: 0,
  Description: 1,
  Tags: 2,
} as const
type ParseStage = (typeof ParseStage)[keyof typeof ParseStage]
const WhitespaceLocation = {
  Beginning: "beginning",
  PrePredicate: "pre-predicate",
} as const
type WhitespaceLocation =
  (typeof WhitespaceLocation)[keyof typeof WhitespaceLocation]

/**
 * @param docString looks like ";;; \n;;; \n..."
 */
export function parseDocString(docString: Comment): CommentStruct {
  // we reverse docChars to make popping the first character O(1)
  const docChars = docString.split("").toReversed()
  // retrieve all doc lines or throw
  const docLines: string[] = []
  while (docChars.length > 0) {
    docLines.push(nextDocLine(docChars))
  }
  // get the signature
  const firstLine = docLines.at(0)
  if (firstLine === undefined) {
    throw new ScamperError(
      "Parser",
      "Doc string is too short to be able to parse the function signature!",
    )
  }
  const signature = parseSignature(firstLine)
  // get the params
  // get the description
  // get the tags
  const params: Params = []
  const description: Description = null
  const tags: Tags = []
  let line: string | undefined
  let stage: ParseStage = ParseStage.Params
  while ((line = docLines.shift()) !== undefined) {
    if (stage === ParseStage.Params) {
      try {
        params.push(parseParam(line))
      } catch (e) {
        if (!(e instanceof ParamWhitespaceError)) {
          throw e
        }
        if (e.loc !== WhitespaceLocation.Beginning) {
          throw e
        }
        stage = ParseStage.Description
      }
    }
    if (stage === ParseStage.Description) {
      // TODO: implement
      void null
    }
    // if (stage === ParseStage.Tags) {
    // }
  }
  // return the comment struct
  return { signature, params, description, tags }
}

const docLinePrefix = ";;; "
const maxSemicolonCount = docLinePrefix.split(";").length - 1
const maxWhitespaceCount = docLinePrefix.split(" ").length - 1
/**
 * @param docChars should be reversed!
 * @returns one unparsed doc line
 */
export function nextDocLine(docChars: string[]): string {
  // parse the beginning of the doc line ";;; "
  let semicolonCount = 0
  let canCheckWhitespace = false
  let whitespaceCount = 0
  for (const _ of docLinePrefix) {
    const ch = docChars.pop()
    if (ch === undefined) {
      throw new ScamperError(
        "Parser",
        `Expected doc string to start with "${docLinePrefix}"`,
      )
    }
    if (ch === ";") {
      if (semicolonCount >= maxSemicolonCount) {
        throw new ScamperError(
          "Parser",
          `Too many semicolons in doc string prefix, expected ${maxSemicolonCount.toString()}, got ${(semicolonCount + 1).toString()}`,
        )
      }

      semicolonCount++
      if (semicolonCount === maxSemicolonCount) {
        canCheckWhitespace = true
      }
      continue
    }
    if (isWhitespace(ch)) {
      if (!canCheckWhitespace) {
        throw new ScamperError(
          "Parser",
          `Not enough semicolons in doc string prefix, expected ${maxSemicolonCount.toString()}, got ${semicolonCount.toString()}`,
        )
      }

      whitespaceCount++
      if (whitespaceCount === maxWhitespaceCount) {
        break
      }
      continue
    }
    throw new ScamperError(
      "Parser",
      `Malformed doc string: prefix must be ";;; " exactly!`,
    )
  }

  // add on the rest of the line to return string
  let ret = ""
  let ch: string | undefined
  while ((ch = docChars.pop()) !== undefined) {
    ret += ch
  }
  // return the string
  return ret
}

function parseSignature(docLine: string): Signature {
  // TODO: implement
  void docLine
  return null
}

class ParamParseError extends ScamperError {
  constructor(message: string) {
    super("Parser", `Error while parsing param in doc string: ${message}`)
  }
}
class ParamWhitespaceError extends ParamParseError {
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
export function parseParam(docLine: string): Param {
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
  const err: ScamperError[] = []
  // TODO: range should actually be populated
  const nameSyn = readSingle(
    new Token(untrimmedName.trimEnd(), Range.none),
    false,
  )
  const name = parseIdentifier(err, nameSyn)
  if (err.length > 0) {
    throw new ParamMalformedFieldError(
      `Name field is malformed, ${err[0].message}`,
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
  const predicateStr = postNameDocLine.slice(prePredicateWhitespace)
  // TODO: range should actually be populated
  const predSyn = readSingle(new Token(predicateStr, Range.none), false)
  const predicate = parseExp(err, predSyn)
  if (err.length > 0) {
    throw new ParamMalformedFieldError(
      `Predicate field is malformed, ${err[0].message}`,
    )
  }
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
