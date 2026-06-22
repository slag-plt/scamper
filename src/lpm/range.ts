/** Locations are used to track single positions within source code. */
export class Loc {
  line: number
  col: number
  idx: number

  constructor (line: number, col: number, idx: number) {
    this.line = line
    this.col = col
    this.idx = idx
  }

  public toString (): string {
    return `${this.line}:${this.col}`
  }

  static none: Loc = new Loc(-1, -1, -1)
}

/** Ranges bundle start and end locations within source code. */
export class Range {
  begin: Loc
  end: Loc

  constructor (begin: Loc, end: Loc) {
    this.begin = begin
    this.end = end
  }
 
  public toString (): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }

  static none: Range = new Range(Loc.none, Loc.none)
  static of (startLine: number, startCol: number, startIdx: number, endLine: number, endCol: number, endIdx: number): Range {
    return new Range(new Loc(startLine, startCol, startIdx), new Loc(endLine, endCol, endIdx))
  }
}
