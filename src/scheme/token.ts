import { parser } from './generated/parser.js'

/** An identifier token located in source, with its half-open `[from, to)` span. */
export interface IdentifierToken {
  name: string
  /** Inclusive start offset. */
  from: number
  /** Exclusive end offset (Lezer's native convention). */
  to: number
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
