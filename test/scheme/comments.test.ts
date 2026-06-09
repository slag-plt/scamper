import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { tokenizeAndParse } from "../../src/scheme"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { mkDefine, mkLit } from "../../src/scheme/ast"
import { Range } from "../../src/lpm"
import { nextDocLine } from "../../src/scheme/docstring"

const identifier = "x"
const value = 1
const lit = mkLit(value, expect.anything() as Range)

describe("Comments", () => {
  const comment = "; this should appear in the comment!"
  const testSrc = `
  ${comment}
  (define ${identifier} ${value.toString()})
  `
  test("are read and attached to syntax", () => {
    const vals = read(testSrc)
    expect(vals).toEqual(
      expect.arrayContaining([expect.objectContaining({ comment })]),
    )
  })
  const comment2 = ";;; this should appear in the comment!"
  const testSrc2 = `
  ${comment2}
  (define ${identifier} ${value.toString()})
  `
  test("triple semicolon comments are saved in the comment", () => {
    const vals = read(testSrc2)
    expect(vals).toEqual(
      expect.arrayContaining([expect.objectContaining({ comment: comment2 })]),
    )
  })
  const comment3 = "; this should appear in the comment!;;; this should appear in the comment!"
  const testSrc3 = `
  ${comment}
  ${comment2}
  (define ${identifier} ${value.toString()})
  `
  test("multiple line comments are saved as one comment", () => {
    const vals = read(testSrc3)
    expect(vals).toEqual(
      expect.arrayContaining([expect.objectContaining({ comment: comment3 })]),
    )
  })
  test("are attached to define statements", () => {
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, testSrc)
    const expectedDefine = mkDefine(
      identifier,
      lit,
      expect.anything() as Range,
      comment,
    )
    expect(prog).toEqual(expect.arrayContaining([expectedDefine]))
  })
})

describe("Docstring parsing", () => {
  describe("nextDocLine", () => {
    describe("prefix", () => {
      test("throws when too many semicolons", () => {
        const testDocString = ";;;; a lot of semicolons!"
        const docChars = testDocString.split("").toReversed()
        expect(() => nextDocLine(docChars)).toThrow("many semicolons")
      })
      test("throws when not enough semicolons", () => {
        const testDocString = ";; not many semicolons!"
        const docChars = testDocString.split("").toReversed()
        expect(() => nextDocLine(docChars)).toThrow("enough semicolons")
      })
      test("throws when bad prefix", () => {
        const testDocString = ";%; why is there a % in the prefix"
        const docChars = testDocString.split("").toReversed()
        expect(() => nextDocLine(docChars)).toThrow("Malformed")
      })
    })
    test("returns the rest of the line", () => {
      const restOfLine = "good comment :)"
      const testDocString = `;;; ${restOfLine}`
      const docChars = testDocString.split("").toReversed()
      expect(nextDocLine(docChars)).toEqual(restOfLine)
    })
  })
})
