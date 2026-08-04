// Comment recovery and attachment for the AST (issue #304).
//
// Scamper's compiler discards comments, but tools like the formatter need them.
// Rather than carry comments in a side channel, we ornament the AST: each node
// can hold leading/trailing/dangling comments (see ast.ts's Node). This module
// recovers comments from the Lezer tree and attaches them to nodes by source
// position, with our own placement rules -- no dependency on any particular
// printer's comment model. It runs only when a caller asks for it (e.g. the
// Prettier plugin), so normal compilation is unaffected.
import * as L from '../lpm'
import * as A from './ast.js'
import { parser } from './generated/parser.js'

function computeLineStarts(src: string): number[] {
  const starts = [0]
  for (let i = 0; i < src.length; i++) {
    if (src[i] === '\n') starts.push(i + 1)
  }
  return starts
}

function locOf(offset: number, lineStarts: number[]): L.Loc {
  let lo = 0
  let hi = lineStarts.length - 1
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1
    if (lineStarts[mid] <= offset) lo = mid
    else hi = mid - 1
  }
  return new L.Loc(lo + 1, offset - lineStarts[lo] + 1, offset)
}

/**
 * Recovers every line comment from `src`, in source order, by walking the Lezer
 * tree (which reuses the real tokenizer, so `;` inside strings and `#\;` char
 * literals are not mistaken for comments). Ranges use the same inclusive-end
 * convention as AST nodes so the two order consistently.
 */
export function collectComments(src: string): A.Comment[] {
  const lineStarts = computeLineStarts(src)
  const comments: A.Comment[] = []
  const cursor = parser.parse(src).cursor()
  do {
    if (cursor.name === 'LineComment') {
      const { from, to } = cursor
      comments.push({
        line: src.slice(from, to).trimEnd(),
        range: new L.Range(
          locOf(from, lineStarts),
          locOf(Math.max(from, to - 1), lineStarts),
        ),
      })
    }
  } while (cursor.next())
  return comments
}

/** A node's child AST nodes, in source order. Identifiers, parameters, struct
 * fields, and binding names are real nodes here (with ranges), so comments can
 * attach to them even where a printer renders them as bare text. */
export function astChildNodes(node: A.SchemeNode): A.SchemeNode[] {
  switch (node.tag) {
    case 'prog':
      return node.body
    case 'define':
      return [node.name, node.value]
    case 'display':
      return [node.value]
    case 'stmtexp':
      return [node.expr]
    case 'struct':
      return [node.name, ...node.fields]
    case 'app':
      return [node.head, ...node.args]
    case 'lam':
      return [
        ...node.params,
        ...(node.restParam ? [node.restParam] : []),
        node.body,
      ]
    case 'let':
      return [...node.bindings.flatMap((b) => [b.pat, b.value]), node.body]
    case 'begin':
    case 'and':
    case 'or':
      return node.exps
    case 'if':
      return [node.guard, node.ifB, node.elseB]
    case 'match':
      return [node.scrutinee, ...node.branches.flatMap((b) => [b.pat, b.body])]
    case 'cond':
      return node.branches.flatMap((b) => [b.test, b.body])
    case 'pctor':
      return [node.name, ...node.args]
    case 'import':
    case 'lit':
    case 'id':
    case 'quote':
    case 'pwild':
    case 'plit':
      return []
  }
}

function append(
  existing: A.Comment[] | undefined,
  more: A.Comment[],
): A.Comment[] {
  return existing ? [...existing, ...more] : [...more]
}

// Distributes `comments` (sorted, each within `node`'s span) among node's
// children and onto the node itself. Rule: a comment on the same line as the
// preceding child is *trailing* on that child; otherwise it is *leading* on the
// following child; a comment with no following child *dangles* on the node.
function attachInto(node: A.SchemeNode, comments: A.Comment[]): void {
  if (comments.length === 0) return
  const kids = astChildNodes(node)
  if (kids.length === 0) {
    node.dangling = append(node.dangling, comments)
    return
  }
  let i = 0
  let pendingLeading: A.Comment[] = []
  let lastKid: A.SchemeNode | null = null
  const flush = (c: A.Comment) => {
    if (lastKid !== null && lastKid.range.end.line === c.range.begin.line) {
      lastKid.trailing = append(lastKid.trailing, [c])
    } else {
      pendingLeading.push(c)
    }
  }
  for (const kid of kids) {
    while (i < comments.length && comments[i].range.end.idx < kid.range.begin.idx) {
      flush(comments[i++])
    }
    if (pendingLeading.length > 0) {
      kid.leading = append(kid.leading, pendingLeading)
      pendingLeading = []
    }
    const inside: A.Comment[] = []
    while (i < comments.length && comments[i].range.end.idx <= kid.range.end.idx) {
      inside.push(comments[i++])
    }
    if (inside.length > 0) attachInto(kid, inside)
    lastKid = kid
  }
  while (i < comments.length) {
    flush(comments[i++])
  }
  if (pendingLeading.length > 0) {
    node.dangling = append(node.dangling, pendingLeading)
  }
}

/**
 * Attaches `comments` to `root` and its descendants in place, as
 * leading/trailing/dangling on the nearest node by source position.
 */
export function attachComments(root: A.SchemeNode, comments: A.Comment[]): void {
  if (comments.length === 0) return
  const sorted = [...comments].sort(
    (a, b) => a.range.begin.idx - b.range.begin.idx,
  )
  attachInto(root, sorted)
}
