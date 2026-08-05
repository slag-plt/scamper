import { AstPath, doc, Doc, Printer } from 'prettier'
import * as A from '../../scheme/ast'
import TextRenderer from '../../lpm/renderers/text'

const {
  builders: { group, indent, join, line, hardline, lineSuffix, breakParent },
} = doc

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
      const mod = node.kind === 'file' ? JSON.stringify(node.module) : node.module
      return `(import ${mod}${node.alias !== undefined ? ` ${node.alias}` : ''})`
    }

    case 'define':
      return group([
        '(define ',
        path.call(print, 'name'),
        indent([line, path.call(print, 'value')]),
        ')',
      ])

    case 'display':
      return group(['(display', indent([line, path.call(print, 'value')]), ')'])

    case 'stmtexp':
      return path.call(print, 'expr')

    case 'struct':
      return group([
        '(struct ',
        path.call(print, 'name'),
        ' (',
        join(' ', path.map(print, 'fields')),
        '))',
      ])

    ///// Expressions ///////////////////////////////////////////////////////////

    case 'lit':
      return TextRenderer.render(node.value)

    // Identifiers double as variable references (Exp) and pattern
    // variables (Pat) -- one case handles both.
    case 'id':
      return node.name

    case 'app':
      if (node.args.length === 0) {
        return group(['(', path.call(print, 'head'), ')'])
      }
      return group([
        '(',
        path.call(print, 'head'),
        indent([line, join(line, path.map(print, 'args'))]),
        ')',
      ])

    case 'lam': {
      // Rest parameters use Clojure-style "&", e.g. (lambda (x & xs) ...) or
      // the rest-only (lambda (& xs) ...).
      const paramDocs: Doc[] = path.map(print, 'params')
      if (node.restParam) paramDocs.push('&', path.call(print, 'restParam'))
      return group([
        '(lambda (',
        join(' ', paramDocs),
        ')',
        indent([line, path.call(print, 'body')]),
        ')',
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
      return group([
        '(let',
        indent([line, group(['(', join(line, bindingDocs), ')'])]),
        indent([line, path.call(print, 'body')]),
        ')',
      ])
    }

    case 'begin':
      return group([
        '(begin',
        indent([line, join(line, path.map(print, 'exps'))]),
        ')',
      ])

    case 'if':
      return group([
        '(if ',
        path.call(print, 'guard'),
        indent([line, path.call(print, 'ifB'), line, path.call(print, 'elseB')]),
        ')',
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
      return group([
        '(match ',
        path.call(print, 'scrutinee'),
        indent([line, join(line, branchDocs)]),
        ')',
      ])
    }

    case 'quote':
      return `'${TextRenderer.render(node.value)}`

    case 'and':
      return group([
        '(and',
        indent([line, join(line, path.map(print, 'exps'))]),
        ')',
      ])

    case 'or':
      return group([
        '(or',
        indent([line, join(line, path.map(print, 'exps'))]),
        ')',
      ])

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
      return group(['(cond', indent([line, join(line, branchDocs)]), ')'])
    }

    case 'anonfn':
      // #(body): "#" immediately before the body's parenthesized doc. An empty
      // #() (whose body is the `null` literal) is printed literally.
      return node.body.tag === 'lit' && node.body.value === null
        ? '#()'
        : ['#', path.call(print, 'body')]

    ///// Patterns //////////////////////////////////////////////////////////////

    case 'pwild':
      return '_'

    case 'plit':
      return TextRenderer.render(node.value)

    case 'pctor':
      if (node.args.length === 0) {
        return group(['(', path.call(print, 'name'), ')'])
      }
      return group([
        '(',
        path.call(print, 'name'),
        indent([line, join(line, path.map(print, 'args'))]),
        ')',
      ])
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
      const dcs = join(hardline, node.dangling.map((c) => c.line))
      parts.push(isEmpty ? [dcs, hardline] : [hardline, dcs, hardline])
    }
    parts.push(trailingDoc(node))
    return parts
  },
}
