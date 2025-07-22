import { Token } from "./parser";
import { Value } from "../lang";
export interface ParseHandler {
    shouldHandle: (beg: Token) => boolean;
    handle: (beg: Token, tokens: Token[]) => Value.Syntax;
}
export interface ParseHandlingSettings {
    customHandlers: ParseHandler[];
    defaultHandler: ParseHandler;
}
export declare const DefaultParseHandlingSettings: ParseHandlingSettings;
//# sourceMappingURL=parsehandler.d.ts.map