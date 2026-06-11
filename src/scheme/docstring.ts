import { ScamperError } from "../lpm"
import { Comment } from "./reader"
import { Param, parseSingleParam } from "./doc-param"
import { App, Exp, isApp, isStmtExp, Var, isVar } from "./ast"
import { SimpleErrorChannel } from "../lpm/output/simple-error"
import { tokenizeAndParse } from "."

import { parseFunctionDescription } from "./doc-description"
import { hasTag, makeTagged } from "./util"
import { DocTag, parseAllTags } from "./doc-tag"

type Params = Param[]
export type Pred = Var | VarApp
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

export interface VarApp extends App {
  head: Var
  args: Var[]
}

export function isVarApp(e: Exp): e is VarApp {
  return isApp(e) && isVar(e.head) && e.args.every(isVar)
}

interface Signature {
  // Function should get changed to App at some point
  function: VarApp
  predicate: Pred
}

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

function parseFunctionSignature(docLine: string): VarApp {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, docLine)
  if (docLine.startsWith(" ")) {
    throw new ScamperError(
      "Parser",
      `Function signature cannot start with whitespace`,
    )
  }
  if (!parsed || errChannel.errors.length > 0) {
    throw new ScamperError("Parser", `Malformed function signature`)
  }
  if (parsed.length < 1) {
    throw new ScamperError("Parser", `Function signature is missing`)
  }
  if (parsed.length > 1) {
    throw new ScamperError("Parser", `More than one function signature found`)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw new ScamperError(
      "Parser",
      `Not a function signature. Expected an expression`,
    )
  }
  // Missing check for whether the expression is an application
  const funct = parsedStmt.expr
  return funct
}

function parseContractSignature(docLine: string): Exp {
  const errChannel = new SimpleErrorChannel()
  const parsed = tokenizeAndParse(errChannel, docLine)
  if (!parsed || errChannel.errors.length > 0 || parsed.length > 1) {
    throw new ScamperError("Parser", `Malformed predicate field`)
  }
  if (parsed.length < 1) {
    throw new ScamperError("Parser", `Predicate field is missing`)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw new ScamperError("Parser", `Not a contract signature`)
  }
  const predicate = parsedStmt.expr
  return predicate
}

function parseSignature(docLine: string): Signature {
  // TODO: implement
  // Check form function name, space, parameter. (separate function and call it)
  // Check contract.

  // verify no split (?)
  const separator = " -> "
  const [functStr, predStr] = docLine.split(separator, 2)

  if (docLine.split(separator).length < 2) {
    throw new ScamperError(
      "Parser",
      `Missing separator in doc string signature`,
    )
  }
  const funct = parseFunctionSignature(functStr)
  const predicate = parseContractSignature(predStr)

  return {
    function: funct,
    predicate: predicate,
  }
}
