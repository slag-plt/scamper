import {expect, test} from '@jest/globals'
import {stringToTokens} from "../src/parser";

describe("Tokenizer", () => {
    test.each(["(", ")", "{", "}", "[", "]"])("recognizes brackets", (bracket) => {
        expect(stringToTokens(bracket)).toContainEqual({text: bracket, range: expect.anything()})
    })
    test("recognizes quotations", () => {
        expect(stringToTokens("'")).toContainEqual({text: "'", range: expect.anything()})
    });
    test("recognizes string literals", () => {
        const testString = `"please work!"`
        expect(stringToTokens(testString)).toContainEqual({text: `"${testString}"`, range: expect.anything()})
    })
    test("recognizes valid scheme code", () => {
        const testCode = `(list "woo hoo this is a quote" '1 [+ {cons 2 3}])`
        const expectedTokens = [
            "(",
            "list",
            `"woo hoo this is a quote"`,
            "'",
            "1",
            "[",
            "+",
            "{",
            "cons",
            "2",
            "3",
            "}",
            "]",
            ")"
        ]
        const actualTokens = stringToTokens(testCode)
        for (const token of expectedTokens) {
            expect(actualTokens).toContainEqual({text: token, range: expect.anything()})
        }
    })
})