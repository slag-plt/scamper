// Converts a Lezer parse tree (from generated/parser.ts, built off syntax.grammar)
// directly into the same A.Prog/A.Stmt/A.Exp/A.Pat shapes that the old
// reader.ts/parser.ts pipeline used to build. This lets
// expansion.ts/scope.ts/codegen.ts stay untouched: they only ever see the
// ast.ts contract, never the parser that produced it.
import type { SyntaxNode } from "@lezer/common"
import * as A from "./ast.js"
import { parser } from "./generated/parser.js"
import * as L from "../lpm/index.js"
import {
  parseCharLiteral,
  parseNumberLiteral,
  parseStringLiteral,
} from "./literals.js"
import { reservedWords } from "./reserved-words.js"

///// Source position bookkeeping ////////////////////////////////////////////////

function computeLineStarts(src: string): number[] {
  const starts = [0]
  for (let i = 0; i < src.length; i++) {
    if (src[i] === "\n") {
      starts.push(i + 1)
    }
  }
  return starts
}

function locOf(offset: number, lineStarts: number[]): L.Loc {
  let lo = 0
  let hi = lineStarts.length - 1
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1
    if (lineStarts[mid] <= offset) {
      lo = mid
    } else {
      hi = mid - 1
    }
  }
  return new L.Loc(lo + 1, offset - lineStarts[lo] + 1, offset)
}

class Ctx {
  constructor(
    public src: string,
    public lineStarts: number[],
    public errors: L.ScamperError[],
  ) {}

  // N.B., reader.ts's ranges are inclusive on the end position (it points at
  // the last character of the token/span, not one past it -- see the "puffed
  // up by 1" comment in linter.ts that compensates for this when talking to
  // CodeMirror). Lezer's node.to is exclusive, so it's adjusted back by one
  // here to match reader.ts's convention exactly.
  range(node: SyntaxNode): L.Range {
    return new L.Range(
      locOf(node.from, this.lineStarts),
      locOf(Math.max(node.from, node.to - 1), this.lineStarts),
    )
  }

  text(node: SyntaxNode): string {
    return this.src.slice(node.from, node.to)
  }
}

function children(node: SyntaxNode): SyntaxNode[] {
  const result: SyntaxNode[] = []
  let child = node.firstChild
  while (child) {
    result.push(child)
    child = child.nextSibling
  }
  return result
}

// Splits a flat, even-length list into adjacent pairs: [a, b, c, d] ->
// [[a, b], [c, d]]. Used for the several grammar productions that flatten a
// list of pairs into their parent's children (let/let* bindings, cond
// branches, match branches) rather than wrapping each pair in its own node.
function pairs<T>(items: T[]): [T, T][] {
  const result: [T, T][] = []
  for (let i = 0; i < items.length; i += 2) {
    result.push([items[i], items[i + 1]])
  }
  return result
}

///// Error recovery ///////////////////////////////////////////////////////////

// Lezer always produces a tree, marking unparseable spans with an anonymous
// "⚠" error node rather than throwing. Error recovery can shift the number
// and shape of a form's remaining children in ways that make positional
// slicing (as used below for e.g. Lambda/Let/Cond) unreliable -- so rather
// than trying to partially salvage a malformed form's structure, any node
// with an erroring child (or that is itself an error node) is treated as
// wholly malformed: report one error covering its span and fall back to a
// placeholder, mirroring parser.ts's own phExp/phStmt recovery strategy.
//
// Each node type gets a short, human-readable description of what it's
// supposed to look like -- not as precise as the old hand-written parser's
// per-arity messages (Lezer's tree doesn't expose *why* a span failed to
// parse the way explicit arity checks did), but a meaningful step up from a
// single generic message for every malformed form.
const formDescriptions: Record<string, string> = {
  Lambda: "lambda expression (a list of parameters and a body)",
  If: "if expression (a guard, an if-branch, and an else-branch)",
  Let: "let expression (a list of bindings and a body)",
  LetStar: "let* expression (a list of bindings and a body)",
  Cond: "cond expression (a list of [test body] branches)",
  Match: "match expression (a scrutinee and a list of [pattern body] branches)",
  And: "and expression",
  Or: "or expression",
  Begin: "begin expression (at least one sub-expression)",
  Section: "section expression (at least one sub-expression)",
  Report: "report expression",
  Error: "error expression (a message to raise)",
  Apply: "apply expression (a function and a list of arguments)",
  Application: "function application",
  Quote: "quoted expression",
  Vector: "vector literal",
  PApp: "constructor pattern",
  PVector: "vector pattern",
  Import: "import statement (a module name)",
  Define: "define statement (a name and a value)",
  Display: "display statement (a value to display)",
  Struct: "struct statement (a name and a list of fields)",
}

function reportSyntaxError(ctx: Ctx, node: SyntaxNode): void {
  if (node.type.isError) {
    ctx.errors.push(
      new L.ScamperError("Parser", "Malformed syntax.", undefined, ctx.range(node)),
    )
    return
  }
  const desc =
    formDescriptions[node.type.name] ?? `${node.type.name.toLowerCase()} expression`
  ctx.errors.push(
    new L.ScamperError(
      "Parser",
      `Malformed ${desc}.`,
      undefined,
      ctx.range(node),
    ),
  )
}

// Returns `placeholder` (after recording a diagnostic) if `node` itself or
// any of its already-computed `cs` children is a Lezer error node;
// otherwise undefined, meaning the caller should proceed with its own
// node.type.name switch. `cs` is passed in (rather than recomputed here) so
// callers only walk a node's children once.
function errorOr<T>(
  ctx: Ctx,
  node: SyntaxNode,
  cs: SyntaxNode[],
  placeholder: T,
): T | undefined {
  if (node.type.isError || cs.some((c) => c.type.isError)) {
    reportSyntaxError(ctx, node)
    return placeholder
  }
  return undefined
}

///// Leaf conversion //////////////////////////////////////////////////////////

// N.B., unlike reader.ts's readSingle (which has to disambiguate a leaf's
// kind by testing its text against a cascade of regexes, since its tokenizer
// lumps every non-bracket, non-string atom into one category), the grammar
// has already done that disambiguation -- Number/String/Boolean/Char are
// distinct node types here, so this just dispatches directly on node.type.name.
function leafValue(ctx: Ctx, node: SyntaxNode): L.Value {
  const text = ctx.text(node)
  switch (node.type.name) {
    case "Number":
      return parseNumberLiteral(text)
    case "String":
      return parseStringLiteral(text, ctx.range(node))
    case "Boolean":
      return text === "#t"
    case "Char":
      return parseCharLiteral(text, ctx.range(node))
    case "Identifier":
      return text === "null" ? null : L.mkSym(text)
    default:
      throw new L.ICE(
        "lezer-bridge.leafValue",
        `Unexpected leaf node: ${node.type.name}`,
      )
  }
}

// Quote payloads and vector literals are inert data, not sub-expressions to
// evaluate (e.g., (quote (lambda (x) x)) is a 3-element list, not a real
// lambda) -- so this walks the tree directly into a raw L.Value rather than
// through expFromNode, which would (incorrectly) treat e.g. a nested Lambda
// node as a real closure to construct. The result is fully raw, recursively:
// no element of quoted/vector data is ever wrapped in metadata (no source
// range, no Syntax struct) -- this dialect has no true quotation (no
// quasiquote/unquote, no macros), so nothing downstream has any legitimate
// reason to inspect a quoted element's provenance. (An earlier version of
// this function wrapped nested elements in a Syntax struct to match the old
// reader.ts/parser.ts pipeline's actual behavior, but that turned out to be
// a real bug, not a deliberate design: nothing in the runtime knows how to
// unwrap a Syntax struct, so e.g. (car '(1 2 3)), (+ (car '(1 2 3)) 1), and
// (equal? '(1 2 3) '(1 2 3)) were all broken.)
//
// N.B., checks for errors at every level of its own recursion (unlike
// expFromNode/patFromNode's check, which only re-runs because they recurse
// into each other -- nodeToRawValue recurses into itself directly, so it
// needs its own check or a syntax error nested inside quoted/vector data
// would silently produce a corrupted value instead of a reported error).
function nodeToRawValue(ctx: Ctx, node: SyntaxNode): L.Value {
  const cs = children(node)
  if (node.type.isError || cs.some((c) => c.type.isError)) {
    reportSyntaxError(ctx, node)
    return undefined
  }
  switch (node.type.name) {
    case "Number":
    case "String":
    case "Boolean":
    case "Char":
      return leafValue(ctx, node)

    case "Identifier": {
      const text = ctx.text(node)
      return text === "null" ? null : L.mkSym(text)
    }

    case "Vector":
    case "PVector":
      return cs.map((c) => nodeToRawValue(ctx, c))

    case "Quote": {
      const inner = cs.length === 2 ? cs[1] : cs[0]
      return L.mkList(L.mkSym("quote"), nodeToRawValue(ctx, inner))
    }

    default:
      // A keyword token (e.g. the "lambda" in a Lambda node) is a leaf
      // whose node type name is exactly the keyword text, thanks to the
      // grammar's kw<> specialization -- reduce it to the equivalent
      // symbol, matching what reader.ts's reading would have produced
      // before any special-form recognition ever happened.
      if (reservedWords.includes(node.type.name)) {
        return L.mkSym(node.type.name)
      }
      // Any other compound node (Lambda, If, Let, Application, PApp, ...)
      // is just a plain list of its children when treated as raw,
      // unevaluated data -- there's no such thing as a "special form" at
      // this level; (lambda (x) x), quoted, is just a 3-element list.
      return L.mkList(...cs.map((c) => nodeToRawValue(ctx, c)))
  }
}

function identifierName(
  ctx: Ctx,
  node: SyntaxNode,
  errorMsg = "Expected an identifier",
): string {
  const name = ctx.text(node)
  if (reservedWords.includes(name)) {
    ctx.errors.push(
      new L.ScamperError(
        "Parser",
        `The identifier "${name}" is a reserved word and cannot be used as a variable name`,
        undefined,
        ctx.range(node),
      ),
    )
    return "<error>"
  }
  if (node.type.name !== "Identifier") {
    ctx.errors.push(new L.ScamperError("Parser", errorMsg, undefined, ctx.range(node)))
    return "<error>"
  }
  return name
}

///// Comments / docstrings ////////////////////////////////////////////////////

// N.B., this only captures the raw comment text/ranges -- it can't fail.
// Actually parsing this into a FunctionDoc (which can fail on a malformed
// docstring) is deferred to whoever needs it (see ast.ts's Define.docComments
// and docstring.ts's parseFunctionDocFromComments), so a malformed docstring
// never blocks parsing/compiling otherwise-valid code.
function precedingComments(
  ctx: Ctx,
  node: SyntaxNode,
): A.Comment[] | undefined {
  const comments: A.Comment[] = []
  let sib = node.prevSibling
  while (sib?.type.name === "LineComment") {
    comments.unshift({ line: ctx.text(sib), range: ctx.range(sib) })
    sib = sib.prevSibling
  }
  return comments.length > 0 ? comments : undefined
}

///// Patterns //////////////////////////////////////////////////////////////////

function patFromNode(ctx: Ctx, node: SyntaxNode): A.Pat {
  const range = ctx.range(node)
  const cs = children(node)
  const err = errorOr(ctx, node, cs, A.mkPLit("<error>", range))
  if (err) {
    return err
  }
  switch (node.type.name) {
    case "Number":
    case "String":
    case "Boolean":
    case "Char":
      return A.mkPLit(leafValue(ctx, node), range)

    case "Identifier": {
      const v = leafValue(ctx, node)
      if (!L.isSym(v)) {
        return A.mkPLit(v, range)
      }
      const name = identifierName(
        ctx,
        node,
        "Expected a valid constructor name",
      )
      return name === "_" ? A.mkPWild(range) : A.mkPVar(name, range)
    }

    case "PApp": {
      if (cs.length === 0) {
        return A.mkPLit(null, range)
      }
      const head = identifierName(
        ctx,
        cs[0],
        "The first element of a pattern list must be a constructor name",
      )
      const args = cs.slice(1).map((c) => patFromNode(ctx, c))
      return A.mkPCtor(head, args, range)
    }

    case "PVector":
      return A.mkPLit(nodeToRawValue(ctx, node), range)

    default:
      throw new L.ICE(
        "lezer-bridge.patFromNode",
        `Unexpected pattern node: ${node.type.name}`,
      )
  }
}

///// Expressions //////////////////////////////////////////////////////////////

function expFromNode(ctx: Ctx, node: SyntaxNode): A.Exp {
  const range = ctx.range(node)
  const cs = children(node)
  const err = errorOr(ctx, node, cs, A.mkLit(undefined, range))
  if (err) {
    return err
  }
  switch (node.type.name) {
    case "Number":
    case "String":
    case "Boolean":
    case "Char":
      return A.mkLit(leafValue(ctx, node), range)

    case "Identifier": {
      const v = leafValue(ctx, node)
      if (!L.isSym(v)) {
        return A.mkLit(v, range)
      }
      return A.mkVar(identifierName(ctx, node), range)
    }

    case "Quote": {
      const inner = cs.length === 2 ? cs[1] : cs[0]
      return A.mkQuote(nodeToRawValue(ctx, inner), range)
    }

    case "JsVar": {
      const name = leafValue(ctx, cs[1]) as string
      return A.mkJsVar(name, range)
    }

    case "Error":
      return A.mkError(expFromNode(ctx, cs[1]), range)

    case "Apply":
      return A.mkApply(
        expFromNode(ctx, cs[1]),
        expFromNode(ctx, cs[2]),
        range,
      )

    case "Vector":
      return A.mkLit(nodeToRawValue(ctx, node), range)

    case "Lambda": {
      const rest = cs.slice(1)
      const body = expFromNode(ctx, rest[rest.length - 1])
      const argNodes = rest.slice(0, -1)
      const dotIndex = argNodes.findIndex((c) => c.type.name === "RestDot")
      if (dotIndex === -1) {
        const params = argNodes.map((c) => identifierName(ctx, c))
        return A.mkLam(params, body, range)
      }
      const params = argNodes.slice(0, dotIndex).map((c) => identifierName(ctx, c))
      const restParam = identifierName(ctx, argNodes[dotIndex + 1])
      return A.mkLam(params, body, range, restParam)
    }

    case "If": {
      const rest = cs.slice(1)
      return A.mkIf(
        expFromNode(ctx, rest[0]),
        expFromNode(ctx, rest[1]),
        expFromNode(ctx, rest[2]),
        range,
      )
    }

    case "And":
      return A.mkAnd(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Or":
      return A.mkOr(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Begin":
      return A.mkBegin(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Section":
      return A.mkSection(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Report":
      return A.mkReport(expFromNode(ctx, cs[1]), range)

    case "Application": {
      if (cs.length === 0) {
        return A.mkLit(null, range)
      }
      return A.mkApp(
        expFromNode(ctx, cs[0]),
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )
    }

    case "Let":
    case "LetStar": {
      const rest = cs.slice(1)
      const body = expFromNode(ctx, rest[rest.length - 1])
      const bindings = pairs(rest.slice(0, -1)).map(([n, v]) => ({
        name: identifierName(ctx, n),
        value: expFromNode(ctx, v),
      }))
      return node.type.name === "Let"
        ? A.mkLet(bindings, body, range)
        : A.mkLetS(bindings, body, range)
    }

    case "Cond": {
      const branches = pairs(cs.slice(1)).map(([test, body]) => ({
        test: expFromNode(ctx, test),
        body: expFromNode(ctx, body),
      }))
      return A.mkCond(branches, range)
    }

    case "Match": {
      const rest = cs.slice(1)
      const scrutinee = expFromNode(ctx, rest[0])
      const branches = pairs(rest.slice(1)).map(([pat, body]) => ({
        pat: patFromNode(ctx, pat),
        body: expFromNode(ctx, body),
      }))
      return A.mkMatch(scrutinee, branches, range)
    }

    default:
      throw new L.ICE(
        "lezer-bridge.expFromNode",
        `Unexpected expression node: ${node.type.name}`,
      )
  }
}

///// Statements ////////////////////////////////////////////////////////////////

function stmtFromNode(ctx: Ctx, node: SyntaxNode): A.Stmt {
  const range = ctx.range(node)
  const cs = children(node)
  const err = errorOr(
    ctx,
    node,
    cs,
    A.mkStmtExp(A.mkLit(undefined, range), range),
  )
  if (err) {
    return err
  }
  switch (node.type.name) {
    case "Import": {
      const name = identifierName(ctx, cs[1])
      return A.mkImport(name, range)
    }

    case "Define": {
      const rest = cs.slice(1)
      const name = identifierName(ctx, rest[0])
      const value = expFromNode(ctx, rest[1])
      const docComments = precedingComments(ctx, node)
      return A.mkDefine(name, value, range, docComments)
    }

    case "Display": {
      const value = expFromNode(ctx, cs[1])
      return A.mkDisp(value, range)
    }

    case "Struct": {
      const rest = cs.slice(1)
      const name = identifierName(ctx, rest[0])
      const fields = rest.slice(1).map((c) => identifierName(ctx, c))
      return A.mkStruct(name, fields, range)
    }

    case "SExpr":
      return A.mkStmtExp(expFromNode(ctx, cs[0]), range)

    default:
      throw new L.ICE(
        "lezer-bridge.stmtFromNode",
        `Unexpected statement node: ${node.type.name}`,
      )
  }
}

///// Entry point ///////////////////////////////////////////////////////////////

export function parseProgramFromSource(
  errors: L.ScamperError[],
  src: string,
): A.Prog {
  const tree = parser.parse(src)
  const ctx = new Ctx(src, computeLineStarts(src), errors)
  const prog: A.Prog = []
  for (const node of children(tree.topNode)) {
    if (node.type.name === "LineComment") {
      continue
    }
    // N.B., a stray error node here (e.g. an extra unmatched closing paren)
    // isn't part of any statement at all -- there's nothing to attach a
    // placeholder statement to, so just record the error and move on.
    if (node.type.isError) {
      reportSyntaxError(ctx, node)
      continue
    }
    prog.push(stmtFromNode(ctx, node))
  }
  return prog
}
