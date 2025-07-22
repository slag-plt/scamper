import { Token, Tokenizer } from "./parser";
export interface TokenHandler {
    shouldHandle: (ch: string) => boolean;
    handle: (tokenizer: Tokenizer) => Token;
}
export interface TokenHandlingSettings {
    customHandlers: TokenHandler[];
    defaultHandler: TokenHandler;
}
export declare const DefaultTokenHandlingSettings: TokenHandlingSettings;
//# sourceMappingURL=tokenhandler.d.ts.map