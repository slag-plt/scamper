import {isBracket, isWhitespace, Token, Tokenizer} from "./parser";
import {ScamperError} from "./lang";

export interface TokenHandler {
    shouldHandle: (ch: string) => boolean;
    handle: (tokenizer: Tokenizer) => Token;
}

export interface TokenHandlingSettings {
    customHandlers: TokenHandler[];
    defaultHandler: TokenHandler;
}

const BracketHandler: TokenHandler = {
    shouldHandle: ch => isBracket(ch),
    handle: tokenizer => {
        tokenizer.beginTracking()
        tokenizer.advance()
        return tokenizer.emitToken()
    }
}

const QuotationHandler: TokenHandler = {
    shouldHandle: ch => ch === "'",
    handle: tokenizer => {
        tokenizer.beginTracking()
        tokenizer.advance()
        return tokenizer.emitToken()
    }
}

const StringLiteralHandler: TokenHandler = {
    shouldHandle: ch => ch === '"',
    handle: tokenizer => {
        tokenizer.beginTracking()
        tokenizer.advance()
        let ch: string;
        while (!tokenizer.isEmpty()) {
            ch = tokenizer.peek()
            if (ch === '"') {
                tokenizer.advance()
                return tokenizer.emitToken()
                // N.B., since any escape sequence that does not have a meaning is
                // the identity escape sequence, we can simply advance past the
                // the entire sequence and let Javascript handle interpreting the
                // sequence for us!
            } else if (ch === '\\') {
                tokenizer.advance()  // advance past '\\
                tokenizer.advance()  // advance past the escaped character
            } else {
                tokenizer.advance()
            }
        }
        // NOTE: error is localized to the open quote to, presumably, the end of the
        // file. Depending on error reporting, it may make sense to report only the
        // starting quote or try to approx. where the string should end.
        throw new ScamperError('Parser', 'Unterminated string literal.',
            undefined,
            tokenizer.currentRange
        )
    }
}

const DefaultHandler: TokenHandler = {
    shouldHandle: _ => true,
    handle: tokenizer => {
        tokenizer.beginTracking()
        tokenizer.advance()
        let ch: string;
        while (!tokenizer.isEmpty()) {
            ch = tokenizer.peek()
            if (isWhitespace(ch) || isBracket(ch) || ch === ';' || ch === "'") {
                // N.B., don't include the terminating char in this token!
                return tokenizer.emitToken()
            } else {
                tokenizer.advance()
            }
        }
        // N.B., should only get here if a complete token ends the file
        return tokenizer.emitToken()
    }
}

export const DefaultTokenHandlingSettings: TokenHandlingSettings = {
    customHandlers: [BracketHandler, QuotationHandler, StringLiteralHandler],
    defaultHandler: DefaultHandler
}