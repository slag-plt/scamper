import { Range } from '../lpm/runtime.js';
import { Syntax } from './ast.js';
declare class Token {
    text: string;
    range: Range;
    constructor(text: string, range: Range);
    toString(): string;
}
export declare function stringToTokens(src: string): Token[];
export declare const namedCharValues: Map<string, string>;
export declare function parseSingle(t: Token, wildAllowed: boolean): Syntax;
export declare function parseValue(tokens: Token[]): Syntax;
export declare function parseValues(tokens: Token[]): Syntax[];
export {};
//# sourceMappingURL=parser.d.ts.map