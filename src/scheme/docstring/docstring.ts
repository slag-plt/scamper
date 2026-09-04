import { ICE, Range } from '../../lpm'
import { ScamperDiagnostic, mkDiagnostic } from '../diagnostic.js'
import { Param, parseSingleParam } from './param'
import { App, Comment, Exp, Identifier, isApp, isVar } from '../ast'

import { parseFunctionDescription } from './description'
import { hasTag, makeTagged } from '../util'
import { DocstringError, mkDocError } from './error'
import { DocTag, parseAllTags } from './tags'
import { parseSignature, Signature } from './signature'

type Params = Param[]
type Tags = DocTag[]
export interface FunctionDoc {
  signature: Signature
  params: Params
  /** The documented optional parameters, e.g. `end` in `(substring s start
   * [end])` -- see VarApp.optArgs. They follow `params` positionally. */
  optParams: Params
  /** The documented rest parameter, e.g. `xs` in `(+ & xs)`, if the
   * signature declares one -- see VarApp.restParam. */
  restParam?: Param
  description: string
  tags: Tags
  range: Range
}

type SimplePred = Identifier
export interface ComplexPred extends App {
  head: Identifier
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

const ParseStageTag = Symbol('ParseStageTag')
export const ParseStage = {
  Params: makeTagged(ParseStageTag, 'params'),
  Description: makeTagged(ParseStageTag, 'description'),
  Tags: makeTagged(ParseStageTag, 'tags'),
} as const
export type ParseStage = (typeof ParseStage)[keyof typeof ParseStage]

export interface VarApp extends App {
  head: Identifier
  args: Identifier[]
  /** The signature's optional parameters, e.g. `end` in `(substring s start
   * [end])`. They come after every required parameter and before the rest
   * parameter, so a call site's arguments still line up positionally. */
  optArgs: Identifier[]
  /** The signature's rest parameter, e.g. `xs` in `(+ & xs)` or
   * `(map f & xs)`, mirroring the lambda arglist's `&` rest-param syntax
   * (#272). undefined for a fixed-arity signature. */
  restParam?: Identifier
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
      'Docstring.parseDocString',
      'Attempted to parse docstring from comment block with no doc lines!',
    )
  }
  const firstRange = firstDocComment.range
  const lastRange = (docComments.at(-1) ?? firstDocComment).range

  const signature = parseSignature(firstDocComment)
  const params: Params = []
  const optParams: Params = []
  const optNames = new Set(signature.function.optArgs.map((a) => a.name))
  let restParam: Param | undefined
  let description = ''
  const tags: Tags = []

  let stage: ParseStage = ParseStage.Params
  while (docComments.length > 0) {
    switch (stage) {
      case ParseStage.Params: {
        const result = parseSingleParam(docComments)
        if (hasTag(result, ParseStageTag)) {
          const expectedParamLines =
            signature.function.args.length +
            signature.function.optArgs.length +
            (signature.function.restParam ? 1 : 0)
          const documented =
            params.length + optParams.length + (restParam ? 1 : 0)
          if (documented !== expectedParamLines) {
            // TODO: should do more granular range
            throw mkDocError(signature.function.restParam && !restParam
                ? `Rest parameter "${signature.function.restParam.name}" was declared in the signature but not documented`
                : 'Encountered function description before all parameters were described',
              firstRange,
            )
          }
          // The counts agreeing is not enough: a misspelled optional's name
          // would fall out of `optNames` and be counted as a required
          // parameter instead, quietly costing the function its optional. The
          // names have to be the signature's, in the signature's order.
          checkNamesMatch(params, signature.function.args, firstRange)
          checkNamesMatch(optParams, signature.function.optArgs, firstRange)
          stage = result
        } else if (
          result.name === signature.function.restParam?.name
        ) {
          restParam = result
        } else if (optNames.has(result.name)) {
          optParams.push(result)
        } else {
          params.push(result)
        }
        break
      }
      case ParseStage.Description: {
        const result = parseFunctionDescription(docComments)
        if (typeof result === 'object' && 'stage' in result) {
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
  if (description === '') {
    throw mkDocError('Docstring must have a function description',
      lastRange,
    )
  }
  // return the comment struct
  return {
    signature,
    params,
    optParams,
    restParam,
    description,
    tags,
    range: new Range(firstRange.begin, lastRange.end),
  }
}

/**
 * Checks that the documented parameters are the signature's, in its order.
 * @throws DocstringError naming the first line that does not match.
 */
function checkNamesMatch(
  documented: Param[],
  declared: Identifier[],
  range: Range,
): void {
  const wrong = documented.findIndex(
    (p, i) => i >= declared.length || p.name !== declared[i].name,
  )
  if (wrong === -1) {
    return
  }
  const name = documented[wrong].name
  throw mkDocError(wrong >= declared.length
      ? `Parameter "${name}" is not declared in the signature`
      : `Parameter "${name}" does not match the signature, which declares "${declared[wrong].name}" in that position`,
    range,
  )
}

const docLinePrefix = ';;; '
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
  if (rest.length === 0 || shouldBeEmpty !== '') {
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
 * docstring is a documentation-quality issue, not a reason to fail compiling
 * otherwise-valid code, so a parse failure is collected as a "Docstring"
 * warning diagnostic (regardless of which internal helper threw it) rather
 * than propagated as an error.
 */
export interface ParsedFunctionDoc {
  /** The parsed doc, or undefined if the comments have no docstring or it was malformed. */
  doc?: FunctionDoc
  /** A single "Docstring" warning if the docstring was malformed; empty otherwise. */
  diagnostics: ScamperDiagnostic[]
}

/**
 * Parses a define's preceding comments into a FunctionDoc, on demand.
 * @returns the parsed doc (absent if there is no docstring or it is malformed)
 *          plus any malformed-docstring diagnostic
 */
export function parseFunctionDocFromComments(
  comments: Comment[],
): ParsedFunctionDoc {
  const docComments = commentsToDocComments(comments)
  if (docComments.length === 0) {
    return { diagnostics: [] }
  }
  try {
    return { doc: parseDocString(docComments), diagnostics: [] }
  } catch (e) {
    if (e instanceof DocstringError) {
      return {
        diagnostics: [mkDiagnostic('Docstring', 'warning', e.message, e.range)],
      }
    }
    throw e
  }
}
