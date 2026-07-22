import { ICE, Range, ScamperError } from "../../lpm"
import type { ComplexPred, DocTag, FunctionDoc, Pred, VarApp } from "../../lpm/docstring.js"
import { Param, parseSingleParam } from "./param"
import { App, Comment, Exp, isApp, isVar, Prog } from "../ast"

import { parseFunctionDescription } from "./description"
import { hasTag, makeTagged, mkScamperErrorWithRange } from "../util"
import { parseAllTags } from "./tags"
import { parseSignature } from "./signature"

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
  const firstRange = firstDocComment.range
  const lastRange = (docComments.at(-1) ?? firstDocComment).range

  const signature = parseSignature(firstDocComment)
  const params: Param[] = []
  let description = ""
  const tags: DocTag[] = []

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

/**
 * Parses a define's preceding comments into a FunctionDoc, on demand --
 * callers decide when parsing a docstring is actually necessary (e.g. the
 * IDE's live linter, or the query/example-tag feature), rather than it
 * happening unconditionally as part of the main parse pass. A malformed
 * docstring is a documentation-quality issue, not a reason to fail
 * compiling otherwise-valid code, so any ScamperError raised while parsing
 * is re-tagged with phase "Docstring" here (regardless of which internal
 * helper actually threw it) so callers -- notably linter.ts -- can report
 * it as a warning instead of a blocking error.
 * @returns undefined if there are no doc-comment lines among `comments`
 * @throws ScamperError (phase "Docstring") if the doc comments are malformed
 */
export function parseFunctionDocFromComments(
  comments: Comment[],
): FunctionDoc | undefined {
  const docComments = commentsToDocComments(comments)
  if (docComments.length === 0) {
    return undefined
  }
  try {
    return parseDocString(docComments)
  } catch (e) {
    if (e instanceof ScamperError) {
      throw new ScamperError("Docstring", e.message, e.modName, e.range, e.source)
    }
    throw e
  }
}

/**
 * Parses every define's docstring in a program, for attaching to a Module's
 * bindings (see Env.getTopLevelAsModule). Mirrors
 * parseFunctionDocFromComments's stance on malformed docstrings: a
 * documentation-quality issue, not a reason to fail -- a define with a
 * malformed or missing docstring is simply omitted from the result.
 * @returns parsed docstrings for top-level defines, keyed by name
 */
export function extractModuleDocs(prog: Prog): Map<string, FunctionDoc> {
  const docs = new Map<string, FunctionDoc>()
  for (const stmt of prog) {
    if (stmt.tag !== "define" || !stmt.docComments) {
      continue
    }
    try {
      const doc = parseFunctionDocFromComments(stmt.docComments)
      if (doc) {
        docs.set(stmt.name, doc)
      }
    } catch {
      // malformed docstring -- treat as undocumented rather than failing
    }
  }
  return docs
}
