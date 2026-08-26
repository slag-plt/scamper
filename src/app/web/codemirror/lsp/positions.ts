import type { Position, Range } from 'vscode-languageserver-protocol'

// LSP positions are zero-based `{ line, character }` pairs over UTF-16 code
// units; CodeMirror and JS string offsets are also UTF-16 code units, so the
// two align without any encoding conversion.

/** Offsets of the first character of each line (index 0 is offset 0). */
export function computeLineStarts(text: string): number[] {
  const starts = [0]
  for (let i = 0; i < text.length; i++) {
    if (text[i] === '\n') {
      starts.push(i + 1)
    }
  }
  return starts
}

/** Converts a character offset to an LSP position via binary search over line starts. */
export function offsetToPosition(offset: number, lineStarts: number[]): Position {
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
  return { line: lo, character: offset - lineStarts[lo] }
}

/** Converts an LSP position to a character offset, clamped to the document. */
export function positionToOffset(
  pos: Position,
  lineStarts: number[],
  textLength: number,
): number {
  if (pos.line < 0) {
    return 0
  }
  if (pos.line >= lineStarts.length) {
    return textLength
  }
  return Math.min(lineStarts[pos.line] + pos.character, textLength)
}

/**
 * Turns a half-open `[from, to)` span of the *analysed* source into a range in
 * the document the client holds. The two differ when a document is analysed
 * inside a context that precedes it -- see the server's `frame`.
 */
export type RangeMapper = (from: number, to: number) => Range

/** Builds an LSP range from a half-open `[from, to)` offset span. */
export function rangeFromOffsets(
  from: number,
  to: number,
  lineStarts: number[],
): Range {
  return {
    start: offsetToPosition(from, lineStarts),
    end: offsetToPosition(to, lineStarts),
  }
}
