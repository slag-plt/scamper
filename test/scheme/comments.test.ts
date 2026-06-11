import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { tokenizeAndParse } from "../../src/scheme"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { mkApp, mkDefine, mkLit } from "../../src/scheme/ast"
import { mkVar, Range } from "../../src/lpm"
import {
  verifyDocLine,
  parseDocString,
  ParseStage,
  isVarApp,
} from "../../src/scheme/docstring"
import {
  Param,
  parseSingleParam,
  parseParamDescriptionLine,
  parseParamSignature,
} from "../../src/scheme/doc-param"
import {
  DocTag,
  matchesDocTagFormat,
  parseAllTags,
} from "../../src/scheme/doc-tag"
import { parseFunctionDescription } from "../../src/scheme/doc-description"

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
    expect(vals).toStrictEqual(
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
    expect(vals).toStrictEqual(
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
    expect(vals).toStrictEqual(
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
    expect(prog).toStrictEqual(expect.arrayContaining([expectedDefine]))
  })
})

const testComment = `;;; (func p1 p2) -> (complex-pred1? pred1?)
;;;  p1 : pred2?
;;;  p2 : (complex-pred2? pred3? pred4?)
;;; this test function is really cool...
;;; isn't it?
;;; @tag stuff1 stuff2
;;; @another-tag stuff3`
// TODO: should move this to diff file, this one getting long
describe("Docstring parsing", () => {
  describe("parseDocString", () => {
    test("outputs when input string is good", () => {
      // TODO: wait until parseSignature done
      expect(parseDocString(testComment)).toStrictEqual("no")
    })
  })

  describe("verifyDocLine", () => {
    describe("prefix", () => {
      test("throws when too many semicolons", () => {
        const testDocLine = ";;;; a lot of semicolons!"
        expect(() => verifyDocLine(testDocLine)).toThrow("to start with")
      })
      test("throws when not enough semicolons", () => {
        const testDocLine = ";; not many semicolons!"
        expect(() => verifyDocLine(testDocLine)).toThrow("to start with")
      })
      test("throws when bad prefix", () => {
        const testDocLine = ";%; why is there a % in the prefix"
        expect(() => verifyDocLine(testDocLine)).toThrow("to start with")
      })
    })
    test("returns the line with the docstring prefix stripped", () => {
      const restOfLine = "good comment :)"
      const testDocLine = `;;; ${restOfLine}`
      expect(verifyDocLine(testDocLine)).toStrictEqual(restOfLine)
    })
  })

  const predHeadId = "complex-pred?"
  const subPredId1 = "pred1?"
  const subPredId2 = "pred2?"
  const predApp = mkApp(
    mkVar(predHeadId, expect.anything() as Range),
    [
      mkVar(subPredId1, expect.anything() as Range),
      mkVar(subPredId2, expect.anything() as Range),
    ],
    expect.anything() as Range,
  )
  if (!isVarApp(predApp)) {
    throw new Error("this should never happen")
  }
  const predicate = predApp
  describe("parseParamSignature", () => {
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
        expect(parseParamSignature(testDocLine).param).toStrictEqual(
          expectedParam,
        )
      })

      test("w/ complex predicate", () => {
        const testDocLine = ` ${name} : (${predHeadId} ${subPredId1} ${subPredId2})`
        const expectedParam: Param = {
          name,
          predicate,
        }
        expect(parseParamSignature(testDocLine).param).toStrictEqual(
          expectedParam,
        )
      })
    })
    describe("illegal param signature", () => {
      // TODO: test
    })
  })

  describe("parseParamDescriptionLine", () => {
    const leadPaddingCount = 2
    test("returns line if padding is good", () => {
      const expectedResult = "hey this is a test description"
      const leadPadding = " ".repeat(leadPaddingCount)
      const testDescLine = `${leadPadding} ${expectedResult}`
      expect(
        parseParamDescriptionLine(testDescLine, leadPaddingCount),
      ).toStrictEqual(expectedResult)
    })
    test("throws if lead padding is bad", () => {
      const contents = "this should NOT appear..."
      const testDescLine = ` ${contents}`
      expect(() =>
        parseParamDescriptionLine(testDescLine, leadPaddingCount),
      ).toThrow("beginning whitespace")
    })
  })

  describe("parseSingleParam", () => {
    const name = "param"
    const goodParamSignature = ` ${name} : (${predHeadId} ${subPredId1} ${subPredId2})`
    const remainingLine =
      "this one is just here since parseAllParams expects it"
    const otherRemainingLine =
      "this is to check that we don't change the order of stuff"
    const expectedRemainder = [remainingLine, otherRemainingLine]

    test("returns param w/ description", () => {
      const paramDescPart1 = "this param does"
      const paramDescPart2 = "something really cool!"
      const paramDescLine1 = `  ${paramDescPart1}`
      const paramDescLine2 = `  ${paramDescPart2}`
      const testDocLines = [
        goodParamSignature,
        paramDescLine1,
        paramDescLine2,
        remainingLine,
        otherRemainingLine,
      ]

      const description = `${paramDescPart1} ${paramDescPart2}`
      const expectedParam: Param = {
        name,
        predicate,
        description,
      }

      expect(parseSingleParam(testDocLines)).toStrictEqual(expectedParam)
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })

    test("returns param w/o description", () => {
      const testDocLines = [
        goodParamSignature,
        remainingLine,
        otherRemainingLine,
      ]
      const expectedParam: Param = {
        name,
        predicate,
        description: undefined,
      }
      expect(parseSingleParam(testDocLines)).toStrictEqual(expectedParam)
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })

    test("signals to move to description stage when param parsing failure", () => {
      const testDocLines = [remainingLine, otherRemainingLine]
      expect(parseSingleParam(testDocLines)).toStrictEqual(
        ParseStage.Description,
      )
      // it should have pushed back the consumed line
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })
  })

  const exTag1 = "@tag"
  const exTag1Contents = "tag1 tag2"
  const exTagLine1 = `${exTag1} ${exTag1Contents}`

  const exTag2 = "@another-tag"
  const exTag2Contents = "tag3"
  const exTagLine2 = `${exTag2} ${exTag2Contents}`

  describe("parseFunctionDescription", () => {
    const testDescriptionLine1 = "this is the first line of the description"
    const testDescriptionLine2 = "this is the SECOND line!"
    const expectedDescription = `${testDescriptionLine1} ${testDescriptionLine2}`
    const expectedRemainder = [exTagLine1, exTagLine2]
    test("empty string when begins with tag line", () => {
      const testDocLines = [exTagLine1, exTagLine2]
      const expectedResult = { stage: ParseStage.Tags, description: "" }
      expect(parseFunctionDescription(testDocLines)).toStrictEqual(
        expectedResult,
      )
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })
    test("works w/ tag lines after", () => {
      const testDocLines = [
        testDescriptionLine1,
        testDescriptionLine2,
        exTagLine1,
        exTagLine2,
      ]
      const expectedResult = {
        stage: ParseStage.Tags,
        description: expectedDescription,
      }
      expect(parseFunctionDescription(testDocLines)).toStrictEqual(
        expectedResult,
      )
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })
    test("doesn't bundle stage when no tag lines after", () => {
      const testDocLines = [testDescriptionLine1, testDescriptionLine2]
      expect(parseFunctionDescription(testDocLines)).toStrictEqual(
        expectedDescription,
      )
      expect(testDocLines).toStrictEqual([])
    })
  })

  describe("parseAllTags", () => {
    test("extracts all tags", () => {
      const testDocLines = [exTagLine1, exTagLine2]
      const tags: DocTag[] = []
      parseAllTags(testDocLines, tags)

      const expectedTag1: DocTag = {
        tag: exTag1,
        contents: exTag1Contents,
      }
      const expectedTag2: DocTag = {
        tag: exTag2,
        contents: exTag2Contents,
      }
      expect(tags).toStrictEqual([expectedTag1, expectedTag2])
      expect(testDocLines).toStrictEqual([])
    })
    test("throws when we have a non-tag in the tag section", () => {
      const testDocLines = [
        exTagLine1,
        "@ bad bad bad! even though we start with @! bad!",
        exTagLine2,
      ]
      const tags: DocTag[] = []
      expect(() => {
        parseAllTags(testDocLines, tags)
      }).toThrow("non-tag")
    })
  })

  describe("matchesDocTagFormat", () => {
    test("good doc tag line", () => {
      const testLine = "@tag tag1 tag2 tag3"
      expect(matchesDocTagFormat(testLine)).toBe(true)
    })
    test("not a doc tag line", () => {
      const testLine =
        "@ this is definitely NOT a tagged line even though it starts with @"
      expect(matchesDocTagFormat(testLine)).toBe(false)
    })
  })
})
