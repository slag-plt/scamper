import { AstPath, doc, Doc, Printer } from 'prettier'
import * as A from '../../scheme/ast'
import TextRenderer from '../../lpm/renderers/text'
import { styleOf } from '../../scheme/style'

const {
  builders: {
    align,
    group,
    indent,
    join,
    line,
    hardline,
    lineSuffix,
    breakParent,
  },
} = doc

// ---- Layout (see src/scheme/style.ts and FORMATTING.md) ---------------------
// The two helpers below lay every form out from the same rule table the
// editor's indenter reads, so `Ctrl-Shift-I` (reformat) and `Ctrl-I`
// (re-indent) agree about where things belong.
//
// One caveat is structural, and is why FORMATTING.md recommends eventually
// retiring this printer: Prettier tracks indentation as a virtual stack rather
// than an output column. The two coincide while a form begins at the start of
// its own line, which is what the rules below assume, but not when a form
// begins part-way through one -- `(map (lambda (x) ...) xs)` indents the lambda
// body from the `(map` rather than from the `(lambda`. src/scheme/pretty.ts,
// which measures real columns, has no such limitation.

/**
 * A parenthesized form, laid out by its entry in the style table.
 *
 * @param headDoc the printed head, which may carry its own comments
 * @param headText the head as text, for measuring; `''` when the head is a
 *   compound expression, whose width cannot be known here -- such a form falls
 *   back to the default rule.
 */
function formDoc(headDoc: Doc, headText: string, rest: Doc[]): Doc {
  if (rest.length === 0) return group(['(', headDoc, ')'])
  const style = styleOf(headText)
  // Where the first argument sits, measured from the opening bracket:
  // "(" + head + " ".
  const firstArgOffset = headText.length + 2

  if (style.kind === 'align') {
    // Rule 7: the head and first argument share a line, the rest line up under
    // that argument. `if` is a plain instance -- its branches land at column 4.
    const [first, ...tail] = rest
    return group([
      '(',
      headDoc,
      ' ',
      first,
      tail.length === 0 ? '' : align(firstArgOffset, [line, join(line, tail)]),
      ')',
    ])
  }

  // Rules 1, 2, 4, 5: the arguments the rule holds back stay on the opening
  // line and the remainder is a body indented one unit. A held argument is
  // aligned to where it actually sits, so a binding list that breaks lines its
  // bindings up under the first one rather than under the `let`.
  const held = rest.slice(0, style.head).map((d) => align(firstArgOffset, d))
  const body = rest.slice(style.head)
  return group([
    '(',
    headDoc,
    ...held.flatMap((d) => [' ', d] as Doc[]),
    body.length === 0 ? '' : indent([line, join(line, body)]),
    ')',
  ])
}

/** A bracketed list -- a vector, a clause, a binding or parameter list --
 * whose items line up under the first one. */
function itemsDoc(open: string, close: string, items: Doc[]): Doc {
  if (items.length === 0) return open + close
  const [first, ...tail] = items
  return group([
    open,
    first,
    tail.length === 0 ? '' : align(1, [line, join(line, tail)]),
    close,
  ])
}

// ---- Type predicates -------------------------------------------------------

export function isSchemeNode(v: unknown): v is A.SchemeNode {
  if (typeof v !== 'object' || v === null) return false
  if (!('tag' in v)) return false
  return typeof v.tag === 'string'
}

function isLetBinding(v: unknown): v is { pat: A.Pat; value: A.Exp } {
  return typeof v === 'object' && v !== null && 'pat' in v && 'value' in v
}

function isMatchBranch(v: unknown): v is { pat: A.Pat; body: A.Exp } {
  return typeof v === 'object' && v !== null && 'pat' in v && 'body' in v
}

function isCondBranch(v: unknown): v is { test: A.Exp; body: A.Exp } {
  return typeof v === 'object' && v !== null && 'test' in v && 'body' in v
}

function isObjPair(v: unknown): v is { key: A.Exp; value: A.Exp } {
  return typeof v === 'object' && v !== null && 'key' in v && 'value' in v
}

// ---- Comments (issue #304) -------------------------------------------------
// Comments are attached to AST nodes by the parser (see scheme/comments.ts);
// the printer reads them off each node. Every node is wrapped by `print` below,
// so a node only needs to be reachable via path.call/path.map for its comments
// to be emitted -- that is why identifiers, params, fields, and binding names
// are printed through the path rather than as bare strings.

/** Comments printed on their own line(s) before the node; forces a line break. */
function leadingDoc(node: A.Node): Doc {
  if (!node.leading || node.leading.length === 0) return ''
  return node.leading.flatMap((c) => [c.line, hardline])
}

/** A comment printed at the end of the node's line; forces the line to break. */
function trailingDoc(node: A.Node): Doc {
  if (!node.trailing || node.trailing.length === 0) return ''
  return node.trailing.flatMap((c) => [lineSuffix([' ', c.line]), breakParent])
}

function renderNode(path: AstPath, print: (p: AstPath) => Doc): Doc {
  const node: unknown = path.node
  if (!isSchemeNode(node)) return ''

  switch (node.tag) {
    ///// Program ///////////////////////////////////////////////////////////////

    case 'prog':
      return join(hardline, path.map(print, 'body'))

    ///// Statements ////////////////////////////////////////////////////////////

    case 'import': {
      const mod =
        node.kind === 'file' ? JSON.stringify(node.module) : node.module
      return `(import ${mod}${node.alias !== undefined ? ` ${node.alias}` : ''})`
    }

    case 'define':
      return formDoc('define', 'define', [
        path.call(print, 'name'),
        path.call(print, 'value'),
      ])

    case 'export':
      return node.names.length === 0
        ? '(export)'
        : formDoc('export', 'export', path.map(print, 'names'))

    case 'defexport':
      return formDoc('define-export', 'define-export', [
        path.call(print, 'name'),
        path.call(print, 'value'),
      ])

    case 'display':
      return formDoc('display', 'display', [path.call(print, 'value')])

    case 'stmtexp':
      return path.call(print, 'expr')

    case 'struct':
      return formDoc('struct', 'struct', [
        path.call(print, 'name'),
        itemsDoc('(', ')', path.map(print, 'fields')),
      ])

    ///// Expressions ///////////////////////////////////////////////////////////

    case 'lit':
      return TextRenderer.render(node.value)

    // Identifiers double as variable references (Exp) and pattern
    // variables (Pat) -- one case handles both.
    case 'id':
      return node.name

    case 'app':
      // A head that is a plain name can be measured, so rule 7's alignment
      // applies; a compound head cannot be, and takes the default.
      return formDoc(
        path.call(print, 'head'),
        node.head.tag === 'id' ? node.head.name : '',
        path.map(print, 'args'),
      )

    case 'lam': {
      // Rest parameters use Clojure-style "&", e.g. (lambda (x & xs) ...) or
      // the rest-only (lambda (& xs) ...).
      const paramDocs: Doc[] = path.map(print, 'params')
      if (node.restParam) paramDocs.push('&', path.call(print, 'restParam'))
      return formDoc('lambda', 'lambda', [
        itemsDoc('(', ')', paramDocs),
        path.call(print, 'body'),
      ])
    }

    case 'let': {
      const bindingDocs: Doc[] = path.map((bindingPath: AstPath) => {
        if (!isLetBinding(bindingPath.node)) return ''
        return group([
          '[',
          bindingPath.call(print, 'pat'),
          ' ',
          bindingPath.call(print, 'value'),
          ']',
        ])
      }, 'bindings')
      return formDoc('let', 'let', [
        itemsDoc('(', ')', bindingDocs),
        path.call(print, 'body'),
      ])
    }

    case 'begin':
      return formDoc('begin', 'begin', path.map(print, 'exps'))

    case 'if':
      return formDoc('if', 'if', [
        path.call(print, 'guard'),
        path.call(print, 'ifB'),
        path.call(print, 'elseB'),
      ])

    case 'match': {
      const branchDocs: Doc[] = path.map((branchPath: AstPath) => {
        if (!isMatchBranch(branchPath.node)) return ''
        return group([
          '[',
          branchPath.call(print, 'pat'),
          ' ',
          branchPath.call(print, 'body'),
          ']',
        ])
      }, 'branches')
      return formDoc('match', 'match', [
        path.call(print, 'scrutinee'),
        ...branchDocs,
      ])
    }

    case 'and':
      return formDoc('and', 'and', path.map(print, 'exps'))

    case 'or':
      return formDoc('or', 'or', path.map(print, 'exps'))

    case 'cond': {
      const branchDocs: Doc[] = path.map((branchPath: AstPath) => {
        if (!isCondBranch(branchPath.node)) return ''
        return group([
          '[',
          branchPath.call(print, 'test'),
          ' ',
          branchPath.call(print, 'body'),
          ']',
        ])
      }, 'branches')
      return formDoc('cond', 'cond', branchDocs)
    }

    case 'anonfn':
      // #(body): "#" immediately before the body's parenthesized doc. An empty
      // #() (whose body is the `null` literal) is printed literally.
      return node.body.tag === 'lit' && node.body.value === null
        ? '#()'
        : ['#', path.call(print, 'body')]

    case 'vec':
      return itemsDoc('[', ']', path.map(print, 'exps'))

    case 'obj': {
      // {k1 v1 ... kn vn}: each pair prints as an unbreakable "key value" unit,
      // so a map that has to break does so between pairs, never inside one.
      if (node.pairs.length === 0) {
        return '{}'
      }
      const pairDocs: Doc[] = path.map((pairPath: AstPath) => {
        if (!isObjPair(pairPath.node)) return ''
        return group([
          pairPath.call(print, 'key'),
          ' ',
          pairPath.call(print, 'value'),
        ])
      }, 'pairs')
      return itemsDoc('{', '}', pairDocs)
    }

    ///// Patterns //////////////////////////////////////////////////////////////

    case 'pwild':
      return '_'

    case 'plit':
      return TextRenderer.render(node.value)

    case 'pctor':
      return formDoc(
        path.call(print, 'name'),
        node.name.name,
        path.map(print, 'args'),
      )

    case 'pvec':
      return itemsDoc('[', ']', path.map(print, 'args'))
  }
  return ''
}

export const SchemePrinter: Printer = {
  print: (path, _options, print) => {
    const node: unknown = path.node
    if (!isSchemeNode(node)) return ''
    const rendered = renderNode(path, print)
    const parts: Doc[] = [leadingDoc(node), rendered]
    // Dangling comments (inside a form with no following child, or the whole of
    // a comment-only program) print on their own line just after the node's doc.
    // The trailing hardline keeps an enclosing closing paren off the comment's
    // line so a line comment can never swallow it.
    if (node.dangling && node.dangling.length > 0) {
      const isEmpty =
        rendered === '' || (Array.isArray(rendered) && rendered.length === 0)
      const dcs = join(
        hardline,
        node.dangling.map((c) => c.line),
      )
      parts.push(isEmpty ? [dcs, hardline] : [hardline, dcs, hardline])
    }
    parts.push(trailingDoc(node))
    return parts
  },
}
