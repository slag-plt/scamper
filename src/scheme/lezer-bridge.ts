// Converts a Lezer parse tree (from generated/parser.ts, built off syntax.grammar)
// directly into the same A.Prog/A.Stmt/A.Exp/A.Pat shapes that parser.ts builds
// from reader.ts's raw s-expressions. This lets expansion.ts/scope.ts/codegen.ts
// stay untouched: they only ever see the ast.ts contract, never the parser that
// produced it.
import type { SyntaxNode } from "@lezer/common"
import * as A from "./ast.js"
import {
  commentsToDocComments,
  FunctionDoc,
  parseDocString,
} from "./docstring/docstring.js"
import { parser } from "./generated/parser.js"
import * as L from "../lpm/index.js"
import { Comment, read as readRaw, readSingle, Token } from "./reader.js"
import { reservedWords } from "./parser.js"
import { isSyntax, mkSyntax, stripSyntax } from "./syntax.js"

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

///// Error recovery ///////////////////////////////////////////////////////////

// Lezer always produces a tree, marking unparseable spans with an anonymous
// "⚠" error node rather than throwing. Error recovery can shift the number
// and shape of a form's remaining children in ways that make positional
// slicing (as used below for e.g. Lambda/Let/Cond) unreliable -- so rather
// than trying to partially salvage a malformed form's structure, any node
// with an erroring child (or that is itself an error node) is treated as
// wholly malformed: report one error covering its span and fall back to a
// placeholder, mirroring parser.ts's own phExp/phStmt recovery strategy.
function containsError(node: SyntaxNode): boolean {
  if (node.type.isError) {
    return true
  }
  let child = node.firstChild
  while (child) {
    if (child.type.isError) {
      return true
    }
    child = child.nextSibling
  }
  return false
}

function reportSyntaxError(ctx: Ctx, node: SyntaxNode): void {
  const desc = node.type.isError ? "syntax" : node.type.name.toLowerCase()
  ctx.errors.push(
    new L.ScamperError(
      "Parser",
      `Malformed ${desc} expression`,
      undefined,
      ctx.range(node),
    ),
  )
}

///// Leaf conversion //////////////////////////////////////////////////////////

// N.B., delegates to reader.ts's readSingle so number/string/char/boolean/null
// literal parsing (escape decoding, named chars, etc.) has exactly one
// implementation. This is the one remaining dependency on reader.ts; retiring
// it requires a tree-based literal reconstruction (see migration Phase 8).
function leafValue(ctx: Ctx, node: SyntaxNode): L.Value {
  const tok = new Token(ctx.text(node), ctx.range(node))
  return stripSyntax(readSingle(tok, true))
}

// Re-reading a source slice in isolation gives ranges relative to that slice
// (starting at line 1, col 1), not the real document. This walks the value
// reader.ts produced and rebases every nested range onto `origin`, the slice's
// real starting position.
function rebaseLoc(loc: L.Loc, origin: L.Loc): L.Loc {
  return loc.line === 1
    ? new L.Loc(origin.line, origin.col + loc.col - 1, origin.idx + loc.idx)
    : new L.Loc(origin.line + loc.line - 1, loc.col, origin.idx + loc.idx)
}

function rebaseRange(range: L.Range, origin: L.Loc): L.Range {
  return new L.Range(
    rebaseLoc(range.begin, origin),
    rebaseLoc(range.end, origin),
  )
}

function rebaseValue(v: L.Value, origin: L.Loc): L.Value {
  if (isSyntax(v)) {
    return mkSyntax(
      rebaseValue(v.value, origin),
      rebaseRange(v.range, origin),
      v.comments,
    )
  } else if (L.isList(v)) {
    return L.mkList(...L.listToVector(v).map((x) => rebaseValue(x, origin)))
  } else if (L.isPair(v)) {
    return L.mkPair(rebaseValue(v.fst, origin), rebaseValue(v.snd, origin))
  } else if (L.isArray(v)) {
    return v.map((x) => rebaseValue(x, origin))
  } else {
    return v
  }
}

// N.B., quote payloads and vector literals are inert data, not sub-expressions
// to evaluate (e.g., (quote (lambda (x) x)) is a 3-element list, not a real
// lambda). Re-reading the underlying source text with reader.ts's raw reader
// is the simplest way to reconstruct that data with the exact same semantics
// as the rest of the pipeline. Same caveat as leafValue above.
//
// N.B., matches parser.ts's S.stripSyntax(arr[1]) exactly: only the outermost
// Syntax wrapper is removed. Nested elements stay Syntax-wrapped, which is the
// real (if a little surprising) existing behavior for quoted/vector data.
function rawValueFromNode(ctx: Ctx, node: SyntaxNode): L.Value {
  const [syn] = readRaw(ctx.text(node))
  return stripSyntax(rebaseValue(syn, ctx.range(node).begin))
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

function precedingComments(ctx: Ctx, node: SyntaxNode): Comment[] | undefined {
  const comments: Comment[] = []
  let sib = node.prevSibling
  while (sib?.type.name === "LineComment") {
    comments.unshift({ line: ctx.text(sib), range: ctx.range(sib) })
    sib = sib.prevSibling
  }
  return comments.length > 0 ? comments : undefined
}

function docFromPrecedingComments(
  ctx: Ctx,
  node: SyntaxNode,
): FunctionDoc | undefined {
  const comments = precedingComments(ctx, node)
  if (!comments) {
    return undefined
  }
  try {
    const docComments = commentsToDocComments(comments)
    return docComments.length > 0 ? parseDocString(docComments) : undefined
  } catch (e) {
    if (!(e instanceof L.ScamperError)) {
      throw e
    }
    ctx.errors.push(e)
    return undefined
  }
}

///// Patterns //////////////////////////////////////////////////////////////////

function patFromNode(ctx: Ctx, node: SyntaxNode): A.Pat {
  const range = ctx.range(node)
  if (containsError(node)) {
    reportSyntaxError(ctx, node)
    return A.mkPLit("<error>", range)
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
      const cs = children(node)
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
      return A.mkPLit(rawValueFromNode(ctx, node), range)

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
  if (containsError(node)) {
    reportSyntaxError(ctx, node)
    return A.mkLit(undefined, range)
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
      const cs = children(node)
      const inner = cs.length === 2 ? cs[1] : cs[0]
      return A.mkQuote(rawValueFromNode(ctx, inner), range)
    }

    case "Vector":
      return A.mkLit(rawValueFromNode(ctx, node), range)

    case "Lambda": {
      const cs = children(node).slice(1)
      const body = expFromNode(ctx, cs[cs.length - 1])
      const params = cs.slice(0, -1).map((c) => identifierName(ctx, c))
      return A.mkLam(params, body, range)
    }

    case "If": {
      const cs = children(node).slice(1)
      return A.mkIf(
        expFromNode(ctx, cs[0]),
        expFromNode(ctx, cs[1]),
        expFromNode(ctx, cs[2]),
        range,
      )
    }

    case "And":
      return A.mkAnd(
        children(node)
          .slice(1)
          .map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Or":
      return A.mkOr(
        children(node)
          .slice(1)
          .map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Begin":
      return A.mkBegin(
        children(node)
          .slice(1)
          .map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Section":
      return A.mkSection(
        children(node)
          .slice(1)
          .map((c) => expFromNode(ctx, c)),
        range,
      )

    case "Report":
      return A.mkReport(expFromNode(ctx, children(node)[1]), range)

    case "Application": {
      const cs = children(node)
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
      const cs = children(node).slice(1)
      const body = expFromNode(ctx, cs[cs.length - 1])
      const bindingsFlat = cs.slice(0, -1)
      const bindings = []
      for (let i = 0; i < bindingsFlat.length; i += 2) {
        bindings.push({
          name: identifierName(ctx, bindingsFlat[i]),
          value: expFromNode(ctx, bindingsFlat[i + 1]),
        })
      }
      return node.type.name === "Let"
        ? A.mkLet(bindings, body, range)
        : A.mkLetS(bindings, body, range)
    }

    case "Cond": {
      const cs = children(node).slice(1)
      const branches = []
      for (let i = 0; i < cs.length; i += 2) {
        branches.push({
          test: expFromNode(ctx, cs[i]),
          body: expFromNode(ctx, cs[i + 1]),
        })
      }
      return A.mkCond(branches, range)
    }

    case "Match": {
      const cs = children(node).slice(1)
      const scrutinee = expFromNode(ctx, cs[0])
      const branchesFlat = cs.slice(1)
      const branches = []
      for (let i = 0; i < branchesFlat.length; i += 2) {
        branches.push({
          pat: patFromNode(ctx, branchesFlat[i]),
          body: expFromNode(ctx, branchesFlat[i + 1]),
        })
      }
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
  if (containsError(node)) {
    reportSyntaxError(ctx, node)
    return A.mkStmtExp(A.mkLit(undefined, range), range)
  }
  switch (node.type.name) {
    case "Import": {
      const name = identifierName(ctx, children(node)[1])
      return A.mkImport(name, range)
    }

    case "Define": {
      const cs = children(node).slice(1)
      const name = identifierName(ctx, cs[0])
      const value = expFromNode(ctx, cs[1])
      const doc = docFromPrecedingComments(ctx, node)
      return A.mkDefine(name, value, range, doc)
    }

    case "Display": {
      const value = expFromNode(ctx, children(node)[1])
      return A.mkDisp(value, range)
    }

    case "Struct": {
      const cs = children(node).slice(1)
      const name = identifierName(ctx, cs[0])
      const fields = cs.slice(1).map((c) => identifierName(ctx, c))
      return A.mkStruct(name, fields, range)
    }

    case "SExpr":
      return A.mkStmtExp(expFromNode(ctx, children(node)[0]), range)

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
