import {
    areMatchingBrackets,
    isClosingBracket,
    isOpeningBracket,
    parseSingle,
    parseValue,
    puffRange,
    Token
} from "./parser";
import {mkRange, ScamperError, Value} from "../lang";

export interface ParseHandler {
    shouldHandle: (beg: Token) => boolean;
    handle: (beg: Token, tokens: Token[]) => Value.Syntax;
}

export interface ParseHandlingSettings {
    customHandlers: ParseHandler[];
    defaultHandler: ParseHandler;
}

const BracketParseHandler: ParseHandler = {
    shouldHandle: beg => isOpeningBracket(beg.text),
    handle: (beg, tokens) => {
        const values = []
        while (tokens.length > 0 && !isClosingBracket(tokens[0].text)) {
            values.push(parseValue(tokens))
        }
        if (tokens.length === 0) {
            // NOTE: error is localized to the open bracket. We could go the end of file here, instead.
            throw new ScamperError('Parser', `Missing closing bracket for "${beg.text}"`, undefined, puffRange(beg.range))
        } else if (!areMatchingBrackets(beg.text, tokens[0].text)) {
            throw new ScamperError('Parser', `Mismatched brackets. "${beg.text}" closed with "${tokens[0].text}"`,
                undefined, mkRange(beg.range.begin, tokens[0].range.end))
        } else {
            const end = tokens.shift()!
            return Value.mkSyntax(
                mkRange(beg.range.begin, end.range.end),
                // N.B., non '[' brackets are lists, i.e., '('. Will need to change if
                // we ever allow '{' to imply an dictionary/object.
                beg.text === '[' ? values : Value.mkList(...values))
        }
    }
}

const QuoteParseHandler: ParseHandler = {
    shouldHandle: beg => beg.text === "'",
    handle: (beg, tokens) => Value.mkSyntax(beg.range, Value.mkList(Value.mkSym('quote'), parseValue(tokens)))
}

const DefaultParseHandler: ParseHandler = {
    shouldHandle: _ => true,
    handle: beg => parseSingle(beg, true)
}

export const DefaultParseHandlingSettings: ParseHandlingSettings = {
    customHandlers: [BracketParseHandler, QuoteParseHandler],
    defaultHandler: DefaultParseHandler
}