import { tokenizeAndParse } from '../index'
import { Identifier, isStmtExp, mkId } from '../ast'
import { DocComment, isPred, Pred, VarApp } from './docstring'
import { looksLikeIdentifier } from '../literals.js'
import { reservedWords } from '../reserved-words.js'
import { mkDocError } from './error'
import { Range } from '../../lpm'

// originally authored by @bacracm, refactored to new file

export interface Signature {
  /**
   * The documented name, and its parameters when it has any. A constant's is
   * its name alone -- see {@link isConstant}.
   */
  function: VarApp
  predicate: Pred
  range: Range
  /**
   * True when the docstring documents a *constant* -- `null: list?` -- rather
   * than a function. The distinction is what a reader needs: `pi` is written
   * bare, while a nullary function like `rex-empty` has to be called (#412).
   */
  isConstant: boolean
}

/**
 * Names that read as a *literal* in Scamper source, and so are not identifiers,
 * but that the standard library nonetheless binds -- so they can be documented.
 * `null` is the only one: `#t` and `#f` are literals and nothing more (#412).
 */
const literalShapedBindings = ['null']

/**
 * Validates a single signature-line token as a legal Scamper identifier,
 * mirroring param.ts's parseParamSignature -- the signature line's function
 * name and parameter names are always bare identifiers, never arbitrary
 * expressions, so this is deliberately simple token validation rather than
 * a full grammar parse (which also has no notation for a rest parameter --
 * see the N.B. below).
 *
 * @param allowLiteralShaped admits a literal-shaped *bound* name, which only a
 *        constant's own name may be.
 */
function validateIdentifierToken(
  token: string,
  range: Range,
  allowLiteralShaped = false,
): void {
  if (
    !looksLikeIdentifier(token) &&
    !(allowLiteralShaped && literalShapedBindings.includes(token))
  ) {
    throw mkDocError('Expected an identifier', range)
  }
  if (token.startsWith('_')) {
    throw mkDocError('Identifiers cannot begin with "_" unless inside of patterns',
      range,
    )
  }
  if (reservedWords.includes(token)) {
    throw mkDocError(`The identifier "${token}" is a reserved word and cannot be used as a variable name`,
      range,
    )
  }
}

// N.B., deliberately not parsed via tokenizeAndParse/the real grammar, unlike
// the rest of this file: the signature line's rest-parameter notation
// (`(+ & xs)`, mirroring lambda's `(lambda (x1 & rest) ...)`) has no
// equivalent in Application's grammar (paren<expression*> has no rest-marker
// alternative -- that's Lambda's arglist-only), and adding one there would
// make rest-marked applications parse as ordinary (if meaningless) Scamper
// source everywhere, not just inside docstrings. Since every token here is
// always a bare identifier (never a nested expression), hand-tokenizing on
// whitespace and validating each token the same way param.ts already does
// for parameter names avoids that risk entirely.
function parseFunctionSignature({ line, range }: DocComment): VarApp {
  if (line.startsWith(' ')) {
    throw mkDocError('Function signature cannot start with whitespace',
      range,
    )
  }
  if (!line.startsWith('(') || !line.endsWith(')')) {
    throw mkDocError('Malformed function signature',
      range,
    )
  }
  const tokens = line.slice(1, -1).trim().split(/\s+/).filter((t) => t.length > 0)
  if (tokens.length === 0) {
    throw mkDocError('Function signature is missing',
      range,
    )
  }

  const [nameTok, ...rest] = tokens
  validateIdentifierToken(nameTok, range)

  const ampIdx = rest.indexOf('&')
  let argToks: string[]
  let restTok: string | undefined
  if (ampIdx === -1) {
    argToks = rest
  } else {
    if (ampIdx !== rest.length - 2) {
      throw mkDocError('Malformed rest parameter: expected a single "&" immediately before the final (rest) parameter name',
        range,
      )
    }
    argToks = rest.slice(0, ampIdx)
    restTok = rest[ampIdx + 1]
  }
  if (rest.filter((t) => t === '&').length > 1) {
    throw mkDocError('Malformed function signature: more than one "&" found',
      range,
    )
  }

  argToks.forEach((t) => {
    validateIdentifierToken(t, range)
  })
  const args: Identifier[] = argToks.map((t) => mkId(t, range))

  let restParam: Identifier | undefined
  if (restTok !== undefined) {
    validateIdentifierToken(restTok, range)
    restParam = mkId(restTok, range)
  }

  return {
    tag: 'app',
    head: mkId(nameTok, range),
    args,
    restParam,
    range,
  }
}

function parseContractSignature({ line, range }: DocComment): Pred {
  const { program: parsed, diagnostics } = tokenizeAndParse(line)
  if (!parsed || diagnostics.length > 0 || parsed.length > 1) {
    throw mkDocError('Malformed predicate field', range)
  }
  if (parsed.length < 1) {
    throw mkDocError('Predicate field is missing', range)
  }
  const parsedStmt = parsed[0]
  if (!isStmtExp(parsedStmt)) {
    throw mkDocError('Not a contract signature', range)
  }
  if (!isPred(parsedStmt.expr)) {
    throw mkDocError('Not a contract signature. Expected a variable or variable application',
      range,
    )
  }
  const predicate = parsedStmt.expr
  // TODO: more granular range is possible
  predicate.range = range
  return predicate
}

/**
 * Parses the first line of a docstring, which is either
 *
 * + a function: `(name arg ...) -> pred`, or
 * + a constant: `name: pred`.
 *
 * The opening paren is what tells them apart, so each form gets the error
 * belonging to it rather than a shared "this did not parse" (#412).
 */
export function parseSignature({
  line: docLine,
  range,
}: DocComment): Signature {
  return docLine.trimStart().startsWith('(')
    ? parseFunctionForm(docLine, range)
    : parseConstantForm(docLine, range)
}

function parseFunctionForm(docLine: string, range: Range): Signature {
  // verify no split (?)
  const separator = ' -> '
  const [functStr, ...rest] = docLine.split(separator)

  if (docLine.split(separator).length < 2) {
    throw mkDocError('Missing separator in doc string signature',
      range,
    )
  }

  // TODO: more granular range is possible
  const funcComment: DocComment = { line: functStr, range }
  const funct = parseFunctionSignature(funcComment)

  const predStr = rest.join(separator)
  // TODO: more granular range is possible
  const predComment: DocComment = { line: predStr, range }
  const predicate = parseContractSignature(predComment)

  return {
    function: funct,
    predicate,
    range,
    isConstant: false,
  }
}

/**
 * A constant: `name: pred`, e.g. `null: list?`. The name may be literal-shaped
 * where a parameter's may not, since it is being declared rather than read.
 */
function parseConstantForm(docLine: string, range: Range): Signature {
  const at = docLine.indexOf(':')
  if (at === -1) {
    throw mkDocError('Missing separator in doc string signature: expected "(name ...) -> predicate" or "name: predicate"',
      range,
    )
  }
  const nameTok = docLine.slice(0, at).trim()
  if (nameTok.includes(' ')) {
    throw mkDocError('A constant signature names one binding and takes no parameters',
      range,
    )
  }
  validateIdentifierToken(nameTok, range, true)

  const predComment: DocComment = { line: docLine.slice(at + 1), range }
  const predicate = parseContractSignature(predComment)

  return {
    function: { tag: 'app', head: mkId(nameTok, range), args: [], range },
    predicate,
    range,
    isConstant: true,
  }
}
