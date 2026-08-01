import type { SyntaxNode } from '@lezer/common'
import { parser } from './generated/parser.js'

/** An identifier token located in source, with its half-open `[from, to)` span. */
export interface IdentifierToken {
  name: string
  /** Inclusive start offset. */
  from: number
  /** Exclusive end offset (Lezer's native convention). */
  to: number
}

/** The application enclosing a cursor: the callee's name and the argument index the cursor is in. */
export interface CallContext {
  /** The head identifier's name, e.g. `map` in `(map f lst)`. */
  name: string
  /** Zero-based index of the argument slot the cursor sits in. */
  activeParam: number
}

/**
 * Finds the identifier token at [offset], if any, using the error-tolerant
 * Lezer parse tree -- so it works even on syntactically incomplete source
 * (unlike {@link tokenizeAndParse}, which yields no program on a parse error).
 * Reserved words are specialized into their own grammar nodes, so this never
 * matches a special form like `define` or `lambda`.
 * @returns the identifier under the cursor, or undefined for whitespace,
 *          literals, brackets, or reserved words
 */
export function identifierAt(
  src: string,
  offset: number,
): IdentifierToken | undefined {
  const tree = parser.parse(src)
  // Resolve from both sides so hovering at either edge of a token still finds
  // it; prefer whichever side lands on an Identifier.
  for (const side of [-1, 1] as const) {
    const node = tree.resolveInner(offset, side)
    if (node.name === 'Identifier' && node.from <= offset && offset <= node.to) {
      return { name: src.slice(node.from, node.to), from: node.from, to: node.to }
    }
  }
  return undefined
}

/**
 * Finds the function application enclosing [offset], for signature help. Walks
 * up the (error-tolerant) Lezer tree to the nearest `Application`, reads its
 * head identifier, and counts how many argument slots precede the cursor.
 * @returns the enclosing call, or undefined when the cursor isn't inside a call
 *          with a simple identifier head
 */
export function enclosingCallAt(
  src: string,
  offset: number,
): CallContext | undefined {
  const tree = parser.parse(src)
  let node = tree.resolveInner(offset, -1)
  while (node.name !== 'Application') {
    const parent = node.parent
    if (parent === null) {
      return undefined
    }
    node = parent
  }
  // An Application's named children are [head, ...args]; the brackets are
  // anonymous and don't appear (see lezer-bridge's Application handling).
  const children: SyntaxNode[] = []
  for (let c = node.firstChild; c !== null; c = c.nextSibling) {
    children.push(c)
  }
  // An empty application `()` has no head to describe.
  if (children.length === 0) {
    return undefined
  }
  const head = children[0]
  if (head.name !== 'Identifier') {
    return undefined
  }
  const args = children.slice(1)
  // The active slot is the number of arguments that already end at/before the
  // cursor -- i.e. how many are "complete" to its left.
  const activeParam = args.filter((a) => a.to <= offset).length
  return { name: src.slice(head.from, head.to), activeParam }
}

/**
 * Every occurrence of the identifier [name] in [src], as half-open `[from, to)`
 * spans, via a full walk of the (error-tolerant) Lezer tree. Reserved words are
 * specialized nodes, so this only matches genuine identifier tokens. Callers
 * that care about binding (not just spelling) must still resolve each span.
 */
export function identifierOccurrences(
  src: string,
  name: string,
): IdentifierToken[] {
  const cursor = parser.parse(src).cursor()
  const result: IdentifierToken[] = []
  do {
    if (
      cursor.name === 'Identifier' &&
      src.slice(cursor.from, cursor.to) === name
    ) {
      result.push({ name, from: cursor.from, to: cursor.to })
    }
  } while (cursor.next())
  return result
}
