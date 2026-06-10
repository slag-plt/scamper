import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { tokenizeAndParse } from "../../src/scheme"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { mkApp, mkDefine, mkLit } from "../../src/scheme/ast"
import { mkVar, Range } from "../../src/lpm"
import { nextDocLine } from "../../src/scheme/docstring"
import { Param, parseParamSignature } from "../../src/scheme/doc-param"

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
  const comment3 =
    "; this should appear in the comment!;;; this should appear in the comment!"
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
    test("returns the line with the docstring prefix stripped", () => {
      const restOfLine = "good comment :)"
      const testDocString = `;;; ${restOfLine}`
      const docChars = testDocString.split("").toReversed()
      expect(nextDocLine(docChars)).toEqual(restOfLine)
    })
  })
  describe("parseParam", () => {
    describe("valid param signature", () => {
      const name = "param"
      test("w/ simple predicate", () => {
        const predId = "pred?"
        const predicate = mkVar(predId, expect.anything() as Range)
        const testDocLine = ` ${name} : ${predId}`
        const expectedParam: Param = {
          name,
          predicate,
        }
        expect(parseParamSignature(testDocLine)).toEqual(expectedParam)
      })
      test("w/ complex predicate", () => {
        const predHeadId = "complex-pred?"
        const subPredId1 = "pred1?"
        const subPredId2 = "pred2?"
        const testDocLine = ` ${name} : (${predHeadId} ${subPredId1} ${subPredId2})`
        const predicate = mkApp(
          mkVar(predHeadId, expect.anything() as Range),
          [
            mkVar(subPredId1, expect.anything() as Range),
            mkVar(subPredId2, expect.anything() as Range),
          ],
          expect.anything() as Range,
        )
        const expectedParam: Param = {
          name,
          predicate,
        }
        // TODO: finish complex predicates
        expect(parseParamSignature(testDocLine)).toEqual(expectedParam)
      })
    })
  })
})
