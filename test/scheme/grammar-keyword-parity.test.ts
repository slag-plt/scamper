import { readFileSync } from "fs"
import { resolve } from "path"
import { describe, expect, test } from "vitest"
import { reservedWords } from "../../src/scheme/reserved-words"

describe("syntax.grammar keyword parity", () => {
  test("every reserved word has a matching kw<...> production, and vice versa", () => {
    const grammarSrc = readFileSync(
      resolve(__dirname, "../../src/scheme/syntax.grammar"),
      "utf-8",
    )
    const grammarKeywords = new Set(
      [...grammarSrc.matchAll(/kw<"([^"]+)">/g)].map((m) => m[1]),
    )
    const reservedSet = new Set(reservedWords)

    const missingFromGrammar = reservedWords.filter(
      (w) => !grammarKeywords.has(w),
    )
    const extraInGrammar = [...grammarKeywords].filter(
      (w) => !reservedSet.has(w),
    )

    expect(
      missingFromGrammar,
      `reservedWords entries with no kw<> production in syntax.grammar: ${missingFromGrammar.join(", ")}`,
    ).toEqual([])
    expect(
      extraInGrammar,
      `syntax.grammar kw<> productions not present in reservedWords: ${extraInGrammar.join(", ")}`,
    ).toEqual([])
  })
})
