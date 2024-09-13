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
}

export class Range {
  begin: Loc
  end: Loc

  constructor (
    startLine: number, startCol: number, startIdx: number,
    endLine: number, endCol: number, endIdx: number
  ) {
    this.begin = new Loc(startLine, startCol, startIdx)
    this.end = new Loc(endLine, endCol, endIdx)
  }

  public toString (): string {
    return `${this.begin.toString()}-${this.end.toString()}`
  }
}

export const mkRange = (beg: Loc, end: Loc): Range => new Range(beg.line, beg.col, beg.idx, end.line, end.col, end.idx)

export const noLoc: Loc = new Loc(-1, -1, -1)
export const noRange: Range = new Range(-1, -1, -1, -1, -1, -1)