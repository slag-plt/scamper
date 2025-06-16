import { ParserOutput } from './lang.js';
import { Range, Stmt, Op, Value } from './lang.js';
declare class Token {
    text: string;
    range: Range;
    constructor(text: string, range: Range);
    toString(): string;
}
export declare function stringToTokens(src: string): Token[];
export declare const namedCharValues: Map<string, string>;
export declare function parseSingle(t: Token, wildAllowed: boolean): Value.Syntax;
export declare function parseValue(tokens: Token[]): Value.Syntax;
export declare function parseValues(tokens: Token[]): Value.Syntax[];
export declare function lower(v: Value.T): Op.T[];
export declare function parseStmt(v: Value.T): Stmt.T;
export declare function parseProgram(src: string): ParserOutput;
export {};
//# sourceMappingURL=parser.d.ts.map