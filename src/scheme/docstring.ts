import { ScamperError } from "../lpm"
import { Comment } from "./reader"
import { Param, parseSingleParam } from "./doc-param"
import { parseFunctionDescription } from "./doc-description"
import { hasTag, makeTagged } from "./util"
import { DocTag, parseAllTags } from "./doc-tag"

type Signature = unknown
type Params = Param[]
type Description = string
type Tags = DocTag[]
interface FunctionDoc {
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
export function parseDocString(docString: Comment): FunctionDoc {
  // split by newline and verify each
  const linesToCheck = docString.split("\n")
  const docLines: string[] = []
  for (const line of linesToCheck) {
    docLines.push(verifyDocLine(line))
  }
  // get the signature
  const firstLine = docLines.shift()
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
  let description: Description = ""
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
        parseAllTags(docLines, tags)
        break
      }
    }
  }
  if (description === "") {
    throw new ScamperError(
      "Parser",
      "Doc string must have a function description!",
    )
  }
  // return the comment struct
  return { signature, params, description, tags }
}

const docLinePrefix = ";;; "
/**
 * @returns line without doc line prefix
 * @throws ScamperError if not a doc line
 */
export function verifyDocLine(line: string): string {
  const [shouldBeEmpty, ...rest] = line.split(docLinePrefix)
  // check starts with prefix
  if (shouldBeEmpty !== "") {
    throw new ScamperError(
      "Parser",
      `Expected doc line to start with "${docLinePrefix}", but doesn't: got "${line}" instead`,
    )
  }
  return rest.join(docLinePrefix)
}

function parseSignature(docLine: string): Signature {
  // TODO: implement
  void docLine
  return null
}
