import { ICE, Range } from "../../lpm"
import { Comment } from "../reader"
import { Param, parseSingleParam } from "./param"
import { App, Exp, isApp, isVar, Var } from "../ast"

import { parseFunctionDescription } from "./description"
import { hasTag, makeTagged, mkScamperErrorWithRange } from "../util"
import { DocTag, parseAllTags } from "./tags"
import { parseSignature, Signature } from "./signature"

type Params = Param[]
type Tags = DocTag[]
export interface FunctionDoc {
  signature: Signature
  params: Params
  description: string
  tags: Tags
  range: Range
}

interface SimplePred extends Var {
  range: Range
}
export interface ComplexPred extends App {
  head: Var
  args: Pred[]
  range: Range
}
export type Pred = SimplePred | ComplexPred
export function isComplexPred(a: App): a is ComplexPred {
  return (
    isVar(a.head) &&
    a.args.every((e) => isVar(e) || (isApp(e) && isComplexPred(e)))
  )
}
export function isPred(e: Exp): e is Pred {
  return isVar(e) || (isApp(e) && isComplexPred(e))
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

export type DocComment = Comment

/**
 * @param docString looks like ";;; \n;;; \n..."
 */
export function parseDocString(docComments: DocComment[]): FunctionDoc {
  // get the signature
  const firstDocComment = docComments.shift()
  if (firstDocComment === undefined) {
    throw new ICE(
      "Docstring.parseDocString",
      "Attempted to parse docstring from comment block with no doc lines!",
    )
  }
  const firstRange = docComments[0].range
  const lastRange = docComments[docComments.length - 1].range

  const signature = parseSignature(firstDocComment)
  const params: Params = []
  let description = ""
  const tags: Tags = []

  let stage: ParseStage = ParseStage.Params
  while (docComments.length > 0) {
    switch (stage) {
      case ParseStage.Params: {
        const result = parseSingleParam(docComments)
        if (hasTag(result, ParseStageTag)) {
          if (params.length !== signature.function.args.length) {
            // TODO: should do more granular range
            throw mkScamperErrorWithRange(
              "Parser",
              "Encountered function description before all parameters were described",
              firstRange,
            )
          }
          stage = result
        } else {
          params.push(result)
        }
        break
      }
      case ParseStage.Description: {
        const result = parseFunctionDescription(docComments)
        if (typeof result === "object" && "stage" in result) {
          ;({ stage, description } = result)
        } else {
          description = result
        }
        break
      }
      case ParseStage.Tags: {
        parseAllTags(docComments, tags)
        break
      }
    }
  }
  if (description === "") {
    throw mkScamperErrorWithRange(
      "Parser",
      "Docstring must have a function description",
      lastRange,
    )
  }
  // return the comment struct
  return {
    signature,
    params,
    description,
    tags,
    range: new Range(firstRange.begin, lastRange.end),
  }
}

const docLinePrefix = ";;; "
/**
 * @returns line without doc line prefix, undefined if not a doc line
 */
export function parseDocLineContents({
  line,
  range,
}: Comment): DocComment | undefined {
  const [shouldBeEmpty, ...rest] = line.split(docLinePrefix)
  // check at least 2
  // check starts with prefix
  if (rest.length === 0 || shouldBeEmpty !== "") {
    return undefined
  }

  return { line: rest.join(docLinePrefix), range }
}

export function commentsToDocComments(comments: Comment[]): DocComment[] {
  return comments.map(parseDocLineContents).filter((line) => line !== undefined)
}
