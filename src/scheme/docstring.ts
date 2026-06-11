import { ScamperError } from "../lpm"
import { Comment, isWhitespace } from "./reader"
import { Param, parseSingleParam } from "./doc-param"
import { parseFunctionDescription } from "./doc-description"
import { hasTag, makeTagged } from "./util"

type Signature = unknown
type Params = Param[]
type Description = unknown
type Tags = unknown[]
interface CommentStruct {
  signature: Signature
  params: Params
  description: Description
  tags: Tags
}

const ParseStageTag = Symbol("ParseStageTag")
export const ParseStage = {
  Params: makeTagged(ParseStageTag, "params"),
  Description: makeTagged(ParseStageTag, "description"),
  Tags: makeTagged(ParseStageTag, "tags"),
} as const
export type ParseStage = (typeof ParseStage)[keyof typeof ParseStage]

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
  const params: Params = []
  // get the description
  let description: Description
  // get the tags
  const tags: Tags = []
  let stage: ParseStage = ParseStage.Params
  while (docLines.length > 0) {
    switch (stage) {
      case ParseStage.Params: {
        const result = parseSingleParam(docLines)
        if (hasTag(result, ParseStageTag)) {
          stage = result
        } else {
          params.push(result)
        }
        break
      }
      case ParseStage.Description: {
        const result = parseFunctionDescription(docLines)
        if (typeof result === "object" && "stage" in result) {
          ;({ stage, description } = result)
        } else {
          description = result
        }
        break
      }
      case ParseStage.Tags: {
        // TODO: implement
        void null
        break
      }
    }
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
  // TODO: this can be simplified by splitting by prefix and checking if the resulting array is length 2
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
