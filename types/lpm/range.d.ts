/** Locations are used to track single positions within source code. */
export declare class Loc {
    line: number;
    col: number;
    idx: number;
    constructor(line: number, col: number, idx: number);
    toString(): string;
    static none: Loc;
}
/** Ranges bundle start and end locations within source code. */
export declare class Range {
    begin: Loc;
    end: Loc;
    constructor(begin: Loc, end: Loc);
    toString(): string;
    static none: Range;
    static of(startLine: number, startCol: number, startIdx: number, endLine: number, endCol: number, endIdx: number): Range;
}
//# sourceMappingURL=range.d.ts.map