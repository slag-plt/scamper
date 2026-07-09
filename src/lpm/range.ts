/** Locations are used to track single positions within source code. */
export class Loc {
  line: number
  col: number
  idx: number

  constructor(line: number, col: number, idx: number) {
    this.line = line
    this.col = col
    this.idx = idx
  }

  public toString(): string {
    return `${String(this.line)}:${String(this.col)}`
  }

  static none: Loc = new Loc(-1, -1, -1)
}

export function compareLoc(loc1: Loc, loc2: Loc): number {
  return loc1.idx - loc2.idx
}

export function rangesEqual(a: Range, b: Range): boolean {
  return a.begin.idx === b.begin.idx && a.end.idx === b.end.idx
}

/** Ranges bundle start and end locations within source code. */
export class Range {
  begin: Loc
  end: Loc

  constructor(begin: Loc, end: Loc) {
    this.begin = begin
    this.end = end
  }

  public toString(): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }

  public contains(loc: Loc): boolean {
    return compareLoc(this.begin, loc) <= 0 && compareLoc(this.end, loc) >= 0
  }

  /** Last location of this range on its first source line. */
  public firstLineEnd(src: string): Loc {
    if (this.begin.line === this.end.line) {
      return this.end
    }

    let idx = this.begin.idx
    while (idx < this.end.idx && idx < src.length && src[idx] !== "\n") {
      idx++
    }
    const endIdx = src[idx] === "\n" ? idx - 1 : idx
    const lineStartIdx = this.begin.idx - this.begin.col + 1
    const col = endIdx - lineStartIdx + 1
    return new Loc(this.begin.line, col, endIdx)
  }

  /** Sub-range covering only the portion on the first source line. */
  public firstLineSpan(src: string): Range {
    return new Range(this.begin, this.firstLineEnd(src))
  }

  static none: Range = new Range(Loc.none, Loc.none)
  static of(
    startLine: number,
    startCol: number,
    startIdx: number,
    endLine: number,
    endCol: number,
    endIdx: number,
  ): Range {
    return new Range(
      new Loc(startLine, startCol, startIdx),
      new Loc(endLine, endCol, endIdx),
    )
  }
}
