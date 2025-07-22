import { ParserOutput } from '../lang';
import { Range, Stmt, Op, Value } from '../lang';
import { TokenHandlingSettings } from "./tokenhandler";
import { ParseHandlingSettings } from "./parsehandler";
export declare class Token {
    text: string;
    range: Range;
    constructor(text: string, range: Range);
    toString(): string;
}
export declare const isWhitespace: (c: string) => boolean;
export declare const isOpeningBracket: (ch: string) => boolean;
export declare const isClosingBracket: (ch: string) => boolean;
export declare const isBracket: (ch: string) => boolean;
export declare const areMatchingBrackets: (open: string, close: string) => boolean;
export declare class Tokenizer {
    private src;
    private idx;
    private row;
    private col;
    private startIdx;
    private startRow;
    private startCol;
    private endRow;
    private endCol;
    private endIdx;
    private tokenLen;
    private customHandlers;
    private defaultHandler;
    constructor(src: string, { customHandlers, defaultHandler }?: TokenHandlingSettings);
    isEmpty(): boolean;
    peek(): string;
    isTracking(): boolean;
    resetTracking(): void;
    get currentRange(): Range;
    beginTracking(): void;
    emitToken(): Token;
    advance(): void;
    chompWhitespaceAndComments(): void;
    next(): Token;
}
export declare function stringToTokens(src: string, tokenizer?: Tokenizer): Token[];
export declare function puffRange(r: Range): Range;
export declare const namedCharValues: Map<string, string>;
export declare function parseSingle(t: Token, wildAllowed: boolean): Value.Syntax;
export declare function parseValue(tokens: Token[], { customHandlers, defaultHandler }?: ParseHandlingSettings): Value.Syntax;
export declare function parseValues(tokens: Token[]): Value.Syntax[];
export declare function lower(v: Value.T): Op.T[];
export declare function parseStmt(v: Value.T): Stmt.T;
export declare function parseProgram(src: string): ParserOutput;
//# sourceMappingURL=parser.d.ts.map