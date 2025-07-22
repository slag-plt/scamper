import { Token } from "./parser";
import { Value } from "../lang";
interface ParseHandler {
    shouldHandle: (beg: Token) => boolean;
    handle: (beg: Token, tokens: Token[]) => Value.Syntax;
}
export interface ParseHandlingSettings {
    customHandlers: ParseHandler[];
    defaultHandler: ParseHandler;
}
export declare const DefaultParseHandlingSettings: ParseHandlingSettings;
export {};
//# sourceMappingURL=parsehandler.d.ts.map