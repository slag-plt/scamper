import { ScamperError } from "../../lpm"
import { Comment } from "../reader"
import { Param, parseSingleParam } from "./param"
import { App, Exp, isApp, isVar, Var } from "../ast"

import { parseFunctionDescription } from "./description"
import { hasTag, makeTagged } from "../util"
import { DocTag, parseAllTags } from "./tag"
import { parseSignature, Signature } from "./signature"

type Params = Param[]
export type Pred = Var | VarApp
type Tags = DocTag[]
export interface FunctionDoc {
  signature: Signature
  params: Params
  description: string
  tags: Tags
}

const ParseStageTag = Symbol("ParseStageTag")
export const ParseStage = {
  Params: makeTagged(ParseStageTag, "params"),
  Description: makeTagged(ParseStageTag, "description"),
  Tags: makeTagged(ParseStageTag, "tags"),
} as const
export type ParseStage = (typeof ParseStage)[keyof typeof ParseStage]

export interface VarApp extends App {
  head: Var
  args: Var[]
}

export function isVarApp(e: Exp): e is VarApp {
  return isApp(e) && isVar(e.head) && e.args.every(isVar)
}

/**
 * @param docString looks like ";;; \n;;; \n..."
 */
export function parseDocString(docString: Comment): FunctionDoc {
  // split by newline and throw away non-doc lines
  const docLines: string[] = docString
    .split("\n")
    .map(parseDocLineContents)
    .filter((line) => line !== undefined)
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
  let description = ""
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
 * @returns line without doc line prefix, undefined if not a doc line
 */
export function parseDocLineContents(line: string): string | undefined {
  const [shouldBeEmpty, ...rest] = line.split(docLinePrefix)
  // check starts with prefix
  if (shouldBeEmpty !== "") {
    return undefined
  }
  return rest.join(docLinePrefix)
}
