import * as L from '../lpm';
import { Syntax } from './syntax.js';
declare class Token {
    text: string;
    range: L.Range;
    constructor(text: string, range: L.Range);
    toString(): string;
}
export declare function stringToTokens(src: string): Token[];
export declare function readSingle(t: Token, wildAllowed: boolean): Syntax;
export declare function readValue(tokens: Token[]): Syntax;
export declare function readValues(tokens: Token[]): Syntax[];
export declare function read(src: string): Syntax[];
export {};
//# sourceMappingURL=reader.d.ts.map