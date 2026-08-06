// Converts a Lezer parse tree (from generated/parser.ts, built off syntax.grammar)
// directly into the same A.Prog/A.Stmt/A.Exp/A.Pat shapes that the old
// reader.ts/parser.ts pipeline used to build. This lets
// expansion.ts/scope.ts/codegen.ts stay untouched: they only ever see the
// ast.ts contract, never the parser that produced it.
import type { SyntaxNode } from '@lezer/common'
import * as A from './ast.js'
import { parser } from './generated/parser.js'
import * as L from '../lpm/index.js'
import { ScamperDiagnostic, mkDiagnostic } from './diagnostic.js'
import {
  parseCharLiteral,
  parseNumberLiteral,
  parseStringLiteral,
} from './literals.js'
import { reservedWords } from './reserved-words.js'
import { pairs } from './util.js'

///// Source position bookkeeping ////////////////////////////////////////////////

function computeLineStarts(src: string): number[] {
  const starts = [0]
  for (let i = 0; i < src.length; i++) {
    if (src[i] === '\n') {
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
  // How many enclosing `#(...)` forms we are inside. Drives the `%`-identifier
  // rules in identifierName: a `%` identifier is legal only at depth > 0, and a
  // `#(...)` nested at depth > 0 is rejected (see the AnonFn case in
  // expFromNode).
  public anonFnDepth = 0

  constructor(
    public src: string,
    public lineStarts: number[],
    public diagnostics: ScamperDiagnostic[],
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

// Collects a node's meaningful children for AST construction. LineComment is a
// @skip token, so Lezer may attach one anywhere between other children -- e.g.
// an inline comment inside an application, (+ 1 ; note\n 2). Comments carry no
// AST meaning here (docstrings are recovered separately via prevSibling, see
// precedingComments), so they're dropped to keep positional access (cs[0], ...)
// and expression-list iteration stable regardless of where a comment lands.
function children(node: SyntaxNode): SyntaxNode[] {
  const result: SyntaxNode[] = []
  let child = node.firstChild
  while (child) {
    if (child.type.name !== 'LineComment') {
      result.push(child)
    }
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
//
// Each node type gets a short, human-readable description of what it's
// supposed to look like -- not as precise as the old hand-written parser's
// per-arity messages (Lezer's tree doesn't expose *why* a span failed to
// parse the way explicit arity checks did), but a meaningful step up from a
// single generic message for every malformed form.
const formDescriptions: Record<string, string> = {
  Lambda: 'lambda expression (a list of parameters and a body)',
  If: 'if expression (a guard, an if-branch, and an else-branch)',
  Let: 'let expression (a list of bindings and a body)',
  Cond: 'cond expression (a list of [test body] branches)',
  Match: 'match expression (a scrutinee and a list of [pattern body] branches)',
  And: 'and expression',
  Or: 'or expression',
  Begin: 'begin expression (at least one sub-expression)',
  AnonFn: 'anonymous function #(...)',
  Application: 'function application',
  Vector: 'vector literal',
  Obj: 'map literal (an even number of key/value expressions)',
  PApp: 'constructor pattern',
  PVector: 'vector pattern',
  Import: 'import statement (a built-in library name, or a quoted file name)',
  Define: 'define statement (a name and a value)',
  Export: 'export statement (a list of names to export)',
  DefineExport: 'define-export statement (a name and a value)',
  Display: 'display statement (a value to display)',
  Struct: 'struct statement (a name and a list of fields)',
}

function reportSyntaxError(ctx: Ctx, node: SyntaxNode): void {
  if (node.type.isError) {
    ctx.diagnostics.push(
      mkDiagnostic('Parse', 'error', 'Malformed syntax.', ctx.range(node)),
    )
    return
  }
  const desc =
    formDescriptions[node.type.name] ?? `${node.type.name.toLowerCase()} expression`
  ctx.diagnostics.push(
    mkDiagnostic('Parse', 'error', `Malformed ${desc}.`, ctx.range(node)),
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
    case 'Number':
      return parseNumberLiteral(text)
    case 'String':
      return parseStringLiteral(text, ctx.range(node))
    case 'Boolean':
      return text === '#t'
    case 'Char':
      return parseCharLiteral(text, ctx.range(node))
    default:
      throw new L.ICE(
        'lezer-bridge.leafValue',
        `Unexpected leaf node: ${node.type.name}`,
      )
  }
}

/** `null` is spelled as an identifier, but denotes the empty-list value. */
const isNullLiteral = (ctx: Ctx, node: SyntaxNode): boolean =>
  node.type.name === 'Identifier' && ctx.text(node) === 'null'

// The special identifiers of the anonymous-function form `#(...)`: `%` (the
// first parameter), `%1`, `%2`, ... (the k-th parameter), and `%&` (the rest
// parameter). Every other `%`-prefixed name is an illegal identifier.
function isPercentId(name: string): boolean {
  return name === '%' || name === '%&' || /^%[1-9][0-9]*$/.test(name)
}

// Validates a qualified reference `mod.member` (only reached when allowQualified
// is set -- see identifierName). Each half must be a legal simple name: neither
// a reserved word nor a `%` identifier (those name a `#(...)` parameter, which
// can't be qualified). Returns the name unchanged, or '<error>' after reporting.
function qualifiedName(ctx: Ctx, node: SyntaxNode, name: string): string {
  const { qualifier, member } = A.splitQualifiedName(name)
  for (const half of [qualifier, member]) {
    if (reservedWords.includes(half) || half.startsWith('%')) {
      ctx.diagnostics.push(
        mkDiagnostic(
          'Parse',
          'error',
          `The qualified name "${name}" is invalid: "${half}" is not a valid name`,
          ctx.range(node),
        ),
      )
      return '<error>'
    }
  }
  return name
}

// `allowPercent` is set only for a variable *reference* (see the Identifier
// case in expFromNode); a `%` identifier is legal there (inside a `#(...)`) but
// never as a binder, so every binder call leaves it false. `allowQualified` is
// likewise set only for a reference: a qualified name (`mod.member`) resolves a
// binding through an imported module and is meaningless in a binder position.
function identifierName(
  ctx: Ctx,
  node: SyntaxNode,
  errorMsg = 'Expected an identifier',
  allowPercent = false,
  allowQualified = false,
): string {
  const name = ctx.text(node)
  if (reservedWords.includes(name)) {
    ctx.diagnostics.push(
      mkDiagnostic(
        'Parse',
        'error',
        `The identifier "${name}" is a reserved word and cannot be used as a variable name`,
        ctx.range(node),
      ),
    )
    return '<error>'
  }
  if (node.type.name !== 'Identifier') {
    ctx.diagnostics.push(mkDiagnostic('Parse', 'error', errorMsg, ctx.range(node)))
    return '<error>'
  }
  if (A.isQualifiedName(name)) {
    if (!allowQualified) {
      ctx.diagnostics.push(
        mkDiagnostic(
          'Parse',
          'error',
          `Qualified names (like "${name}") may only be used as variable references, not as a binding name`,
          ctx.range(node),
        ),
      )
      return '<error>'
    }
    return qualifiedName(ctx, node, name)
  }
  if (name.startsWith('%')) {
    if (!isPercentId(name)) {
      ctx.diagnostics.push(
        mkDiagnostic(
          'Parse',
          'error',
          `The identifier "${name}" is invalid: identifiers cannot begin with "%" (only "%", "%1", ..., "%k", and "%&" may, and only inside an anonymous function #(...))`,
          ctx.range(node),
        ),
      )
      return '<error>'
    }
    if (!allowPercent) {
      // A binder position: `%` identifiers are the implicit parameters of a
      // `#(...)` and may only be referenced, never bound (binding one would
      // shadow/clash with the parameter it names).
      ctx.diagnostics.push(
        mkDiagnostic(
          'Parse',
          'error',
          `The identifier "${name}" cannot be used as a binding name`,
          ctx.range(node),
        ),
      )
      return '<error>'
    }
    if (ctx.anonFnDepth === 0) {
      ctx.diagnostics.push(
        mkDiagnostic(
          'Parse',
          'error',
          `The identifier "${name}" can only be used inside an anonymous function #(...)`,
          ctx.range(node),
        ),
      )
      return '<error>'
    }
  }
  return name
}

// As identifierName, but keeps the identifier's own source range alongside its
// text -- this is where each A.Identifier's range gets populated from the parse
// tree. Used everywhere an identifier is a genuine AST node (variable
// references, binders, ...) rather than a plain string (import module names).
function identifier(
  ctx: Ctx,
  node: SyntaxNode,
  errorMsg = 'Expected an identifier',
  allowPercent = false,
  allowQualified = false,
): A.Identifier {
  return A.mkId(
    identifierName(ctx, node, errorMsg, allowPercent, allowQualified),
    ctx.range(node),
  )
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
  while (sib?.type.name === 'LineComment') {
    comments.unshift({ line: ctx.text(sib), range: ctx.range(sib) })
    sib = sib.prevSibling
  }
  return comments.length > 0 ? comments : undefined
}

///// Patterns //////////////////////////////////////////////////////////////////

function patFromNode(ctx: Ctx, node: SyntaxNode): A.Pat {
  const range = ctx.range(node)
  const cs = children(node)
  const err = errorOr(ctx, node, cs, A.mkPLit('<error>', range))
  if (err) {
    return err
  }
  switch (node.type.name) {
    case 'Number':
    case 'String':
    case 'Boolean':
    case 'Char':
      return A.mkPLit(leafValue(ctx, node), range)

    case 'Identifier': {
      if (isNullLiteral(ctx, node)) {
        return A.mkPLit(null, range)
      }
      const id = identifier(ctx, node, 'Expected a valid constructor name')
      return id.name === '_' ? A.mkPWild(range) : id
    }

    case 'PApp': {
      if (cs.length === 0) {
        return A.mkPLit(null, range)
      }
      const head = identifier(
        ctx,
        cs[0],
        'The first element of a pattern list must be a constructor name',
      )
      const args = cs.slice(1).map((c) => patFromNode(ctx, c))
      return A.mkPCtor(head, args, range)
    }

    case 'PVector':
      // `[p1 ... pk]`: a vector pattern, whose elements are ordinary
      // sub-patterns -- so `[1 x]` binds `x` rather than matching a literal
      // vector containing the symbol `x` (issue #325).
      return A.mkPVec(
        cs.map((c) => patFromNode(ctx, c)),
        range,
      )

    default:
      throw new L.ICE(
        'lezer-bridge.patFromNode',
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
    case 'Number':
    case 'String':
    case 'Boolean':
    case 'Char':
      return A.mkLit(leafValue(ctx, node), range)

    case 'Identifier': {
      if (isNullLiteral(ctx, node)) {
        return A.mkLit(null, range)
      }
      // A variable reference: the only position where a `%` identifier (a
      // #(...) parameter) or a qualified name (`mod.member`) is permitted.
      return identifier(ctx, node, 'Expected an identifier', true, true)
    }

    case 'Vector':
      // `[e1 ... ek]`: every element is an ordinary sub-expression, evaluated
      // like any other (issue #325).
      return A.mkVec(
        cs.map((c) => expFromNode(ctx, c)),
        range,
      )

    case 'Obj': {
      // `{k1 v1 ... kn vn}`: alternating keys and values, so an odd number of
      // elements leaves a key with no value. The grammar cannot count, so the
      // check lives here.
      if (cs.length % 2 !== 0) {
        ctx.diagnostics.push(
          mkDiagnostic(
            'Parse',
            'error',
            `A map literal must have an even number of expressions (alternating keys and values), but this one has ${cs.length.toString()}`,
            range,
          ),
        )
        return A.mkLit(undefined, range)
      }
      return A.mkObj(
        pairs(cs.map((c) => expFromNode(ctx, c))).map(([key, value]) => ({
          key,
          value,
        })),
        range,
      )
    }

    case 'Lambda': {
      const rest = cs.slice(1)
      const body = expFromNode(ctx, rest[rest.length - 1])
      const argNodes = rest.slice(0, -1)
      const ampIndex = argNodes.findIndex((c) => c.type.name === 'Amp')
      if (ampIndex === -1) {
        const params = argNodes.map((c) => identifier(ctx, c))
        return A.mkLam(params, body, range)
      }
      const params = argNodes.slice(0, ampIndex).map((c) => identifier(ctx, c))
      const restParam = identifier(ctx, argNodes[ampIndex + 1])
      return A.mkLam(params, body, range, restParam)
    }

    case 'If': {
      const rest = cs.slice(1)
      return A.mkIf(
        expFromNode(ctx, rest[0]),
        expFromNode(ctx, rest[1]),
        expFromNode(ctx, rest[2]),
        range,
      )
    }

    case 'And':
      return A.mkAnd(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case 'Or':
      return A.mkOr(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case 'Begin':
      return A.mkBegin(
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )

    case 'AnonFn': {
      // #(body). cs[0] is the "#" marker; cs[1] is the parenthesized body.
      // `%` identifiers inside are legal (anonFnDepth > 0), and a `#(...)` that
      // is itself nested inside one is rejected.
      if (ctx.anonFnDepth > 0) {
        ctx.diagnostics.push(
          mkDiagnostic(
            'Parse',
            'error',
            'Anonymous functions #(...) cannot be nested',
            range,
          ),
        )
      }
      ctx.anonFnDepth++
      const body = expFromNode(ctx, cs[1])
      ctx.anonFnDepth--
      return A.mkAnonFn(body, range)
    }

    case 'Application': {
      if (cs.length === 0) {
        return A.mkLit(null, range)
      }
      return A.mkApp(
        expFromNode(ctx, cs[0]),
        cs.slice(1).map((c) => expFromNode(ctx, c)),
        range,
      )
    }

    case 'Let': {
      const rest = cs.slice(1)
      const body = expFromNode(ctx, rest[rest.length - 1])
      const bindings = pairs(rest.slice(0, -1)).map(([n, v]) => ({
        pat: patFromNode(ctx, n),
        value: expFromNode(ctx, v),
      }))
      return A.mkLet(bindings, body, range)
    }

    case 'Cond': {
      const branches = pairs(cs.slice(1)).map(([test, body]) => ({
        test: expFromNode(ctx, test),
        body: expFromNode(ctx, body),
      }))
      return A.mkCond(branches, range)
    }

    case 'Match': {
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
        'lezer-bridge.expFromNode',
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
    case 'Import': {
      const target = cs[1]
      // The optional third child is the qualified name (alias): a simple
      // identifier, so identifierName rejects a *qualified* alias. A reserved
      // word or non-identifier in that slot isn't an Identifier node at all, so
      // it's already caught above by errorOr as a malformed import.
      const alias =
        cs.length > 2
          ? identifierName(ctx, cs[2], 'Expected a module alias')
          : undefined
      if (target.type.name === 'String') {
        const filename = leafValue(ctx, target) as string
        return A.mkImport(filename, 'file', range, alias)
      }
      const name = ctx.text(target)
      if (target.type.name === 'Identifier' && A.isQualifiedName(name)) {
        // A dotted, unquoted module name -- a builtin library name is always a
        // simple identifier, so this is almost certainly a file name missing
        // its quotes. Point the user at the fix rather than the generic
        // qualified-name-in-a-binder message.
        ctx.diagnostics.push(
          mkDiagnostic(
            'Parse',
            'error',
            `Malformed import statement: a file name like "${name}" must be quoted, e.g. (import "${name}")`,
            ctx.range(target),
          ),
        )
        return A.mkStmtExp(A.mkLit(undefined, range), range)
      }
      const modName = identifierName(ctx, target)
      return A.mkImport(modName, 'builtin', range, alias)
    }

    case 'Define': {
      const rest = cs.slice(1)
      const name = identifier(ctx, rest[0])
      const value = expFromNode(ctx, rest[1])
      const docComments = precedingComments(ctx, node)
      return A.mkDefine(name, value, range, docComments)
    }

    case 'Export': {
      // Each name is a reference to a top-level binding -- a simple identifier
      // (identifier rejects a qualified name), never a binder.
      const names = cs.slice(1).map((c) => identifier(ctx, c))
      return A.mkExport(names, range)
    }

    case 'DefineExport': {
      const rest = cs.slice(1)
      const name = identifier(ctx, rest[0])
      const value = expFromNode(ctx, rest[1])
      const docComments = precedingComments(ctx, node)
      return A.mkDefineExport(name, value, range, docComments)
    }

    case 'Display': {
      const value = expFromNode(ctx, cs[1])
      return A.mkDisp(value, range)
    }

    case 'Struct': {
      const rest = cs.slice(1)
      const name = identifier(ctx, rest[0])
      const fields = rest.slice(1).map((c) => identifier(ctx, c))
      return A.mkStruct(name, fields, range)
    }

    case 'SExpr':
      return A.mkStmtExp(expFromNode(ctx, cs[0]), range)

    default:
      throw new L.ICE(
        'lezer-bridge.stmtFromNode',
        `Unexpected statement node: ${node.type.name}`,
      )
  }
}

///// Entry point ///////////////////////////////////////////////////////////////

export function parseProgramFromSource(
  diagnostics: ScamperDiagnostic[],
  src: string,
): A.Prog {
  const tree = parser.parse(src)
  const ctx = new Ctx(src, computeLineStarts(src), diagnostics)
  const prog: A.Prog = []
  for (const node of children(tree.topNode)) {
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
