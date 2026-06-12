import { describe, expect, test } from "vitest"
import { mkApp, mkDefine, mkVar } from "../../src/scheme/ast"
import { mkLit, Range } from "../../src/lpm"
import {
  Param,
  parseParamDescriptionLine,
  parseParamSignature,
  parseSingleParam,
} from "../../src/scheme/docstring/param"
import {
  ComplexPred,
  FunctionDoc,
  isComplexPred,
  parseDocLineContents,
  parseDocString,
  ParseStage,
  Pred,
  VarApp,
} from "../../src/scheme/docstring/docstring"
import { parseFunctionDescription } from "../../src/scheme/docstring/description"
import {
  DocTag,
  matchesDocTagFormat,
  parseAllTags,
} from "../../src/scheme/docstring/tag"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { tokenizeAndParse } from "../../src/scheme"

const anyRange = expect.anything() as Range
describe("Docstring parsing", () => {
  test("are attached to define statements", () => {
    const { testComment, expectedFunctionDoc } = makeTestDocstring()

    const identifier = "x"
    const value = 1
    const lit = mkLit(value, anyRange)

    const testSrc = `${testComment}
(define ${identifier} ${value.toString()})`

    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, testSrc)
    const expectedDefine = mkDefine(
      identifier,
      lit,
      anyRange,
      expectedFunctionDoc,
    )
    expect(prog).toStrictEqual(expect.arrayContaining([expectedDefine]))
  })

  describe("parseDocString", () => {
    test("outputs when input string is good", () => {
      const { testComment, expectedFunctionDoc } = makeTestDocstring()

      expect(parseDocString(testComment)).toStrictEqual(expectedFunctionDoc)
    })
  })

  describe("parseDocLineContents", () => {
    describe("prefix", () => {
      test("undefined when too many semicolons", () => {
        const testDocLine = ";;;; a lot of semicolons!"
        expect(parseDocLineContents(testDocLine)).toBeUndefined()
      })
      test("undefined when not enough semicolons", () => {
        const testDocLine = ";; not many semicolons!"
        expect(parseDocLineContents(testDocLine)).toBeUndefined()
      })
      test("undefined when bad prefix", () => {
        const testDocLine = ";%; why is there a % in the prefix"
        expect(parseDocLineContents(testDocLine)).toBeUndefined()
      })
    })
    test("returns the line with the docstring prefix stripped", () => {
      const restOfLine = "good comment :)"
      const testDocLine = `;;; ${restOfLine}`
      expect(parseDocLineContents(testDocLine)).toStrictEqual(restOfLine)
    })
  })

  const predHeadId = "complex-pred?"
  const subPredId1 = "pred1?"
  const subPredId2 = "pred2?"
  const predicate = mkApp(
    mkVar(predHeadId, anyRange),
    [mkVar(subPredId1, anyRange), mkVar(subPredId2, anyRange)],
    anyRange,
  ) as Pred
  describe("parseParamSignature", () => {
    describe("valid param signature", () => {
      const name = "param"
      test("w/ simple predicate", () => {
        const predId = "pred?"
        const predicate = mkVar(predId, anyRange)
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

  describe("isComplexPred", () => {
    const name = "complex-pred?"
    test("works on simple complex pred", () => {
      const subPred1 = "pred1?"
      const subPred2 = "pred2?"
      const testComplexPred: ComplexPred = mkApp(mkVar(name), [
        mkVar(subPred1),
        mkVar(subPred2),
      ]) as ComplexPred

      expect(isComplexPred(testComplexPred)).toBe(true)
    })
    test("works on nested complex pred", () => {
      const nestedName = "nested-complex?"
      const subPred1 = "pred1?"
      const subNestedName = "another-nested?"
      const subPred2 = "pred2?"
      const subPred3 = "pred3?"
      const testComplexPred: ComplexPred = mkApp(mkVar(name), [
        mkVar(subPred1),
        mkApp(mkVar(nestedName), [
          mkApp(mkVar(subNestedName), [mkVar(subPred2), mkVar(subPred3)]),
        ]),
      ]) as ComplexPred

      expect(isComplexPred(testComplexPred)).toBe(true)
    })
  })
})

function makeTestDocstring() {
  const funcName = "func"
  const paramName1 = "p1"
  const paramName2 = "p2"
  const funcApp = mkApp(
    mkVar(funcName, anyRange),
    [mkVar(paramName1, anyRange), mkVar(paramName2, anyRange)],
    anyRange,
  ) as VarApp

  const complexPredName1 = "complex-pred1?"
  const predName1 = "pred1?"
  const predicate1 = mkApp(
    mkVar(complexPredName1, anyRange),
    [mkVar(predName1, anyRange)],
    anyRange,
  ) as VarApp

  const predName2 = "pred2?"
  const predicate2 = mkVar(predName2, anyRange)

  const paramDescLine1 = "this parameter is amazing"
  const paramDescLine2 = "i really like it"
  const paramDescription = `${paramDescLine1} ${paramDescLine2}`

  const complexPredName2 = "complex-pred2?"
  const predName3 = "pred3?"
  const predName4 = "pred4?"
  const predicate3 = mkApp(
    mkVar(complexPredName2, anyRange),
    [mkVar(predName3, anyRange), mkVar(predName4, anyRange)],
    anyRange,
  ) as VarApp

  const descriptionLine1 = "this test function is really cool..."
  const descriptionLine2 = "isn't it?"
  const description = `${descriptionLine1} ${descriptionLine2}`

  const tag1 = "@tag"
  const tagContents1 = "stuff1 stuff2"

  const tag2 = "@another-tag"
  const tagContents2 = "stuff3"

  const testComment = `;;; (${funcName} ${paramName1} ${paramName2}) -> (${complexPredName1} ${predName1})
;;;  ${paramName1} : ${predName2}
;;;   ${paramDescLine1}
;;;   ${paramDescLine2}
;;;  ${paramName2} : (${complexPredName2} ${predName3} ${predName4})
;;; ${descriptionLine1}
;;; ${descriptionLine2}
;;; ${tag1} ${tagContents1}
;;; ${tag2} ${tagContents2}`

  const expectedFunctionDoc: FunctionDoc = {
    signature: {
      function: funcApp,
      predicate: predicate1,
    },
    params: [
      {
        name: paramName1,
        predicate: predicate2,
        description: paramDescription,
      },
      { name: paramName2, predicate: predicate3, description: undefined },
    ],
    description,
    tags: [
      { tag: tag1, contents: tagContents1 },
      { tag: tag2, contents: tagContents2 },
    ],
  }

  return { testComment, expectedFunctionDoc }
}
