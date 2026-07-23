import './test-tags'

import { describe, expect, test } from 'vitest'
import { mkApp, mkDefine, mkVar } from '../../../src/scheme/ast'
import { mkLit } from '../../../src/lpm'
import {
  Param,
  parseParamDescriptionLine,
  parseParamSignature,
  parseSingleParam,
} from '../../../src/scheme/docstring/param'
import {
  commentsToDocComments,
  ComplexPred,
  DocComment,
  FunctionDoc,
  isComplexPred,
  parseDocLineContents,
  parseDocString,
  parseFunctionDocFromComments,
  ParseStage,
  Pred,
  VarApp,
} from '../../../src/scheme/docstring/docstring'
import { parseFunctionDescription } from '../../../src/scheme/docstring/description'
import { testTag1, testTag2, testTagLine1, testTagLine2 } from './test-tags'
import { SimpleErrorChannel } from '../../../src/lpm/output/simple-error'
import { tokenizeAndParse } from '../../../src/scheme'
import { anyRange } from '../util'
import { Comment } from '../../../src/scheme/ast'

describe('Docstring parsing', () => {
  test('are attached to define statements', () => {
    const { rawTestComments, expectedFunctionDoc } = makeTestDocstring()

    const identifier = 'x'
    const value = 1
    const lit = mkLit(value, anyRange)

    const testSrc = `${rawTestComments.map((c) => c.line).join('\n')}
(define ${identifier} ${value.toString()})`

    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, testSrc)
    // N.B., parsing is deferred: the Define carries raw docComments, not an
    // already-parsed FunctionDoc (see ast.ts's Define.docComments).
    const expectedDefine = mkDefine(identifier, lit, anyRange, rawTestComments)
    expect(prog).toStrictEqual(expect.arrayContaining([expectedDefine]))

    const stmt = prog?.find((s) => s.tag === 'define')
    expect(stmt?.tag).toBe('define')
    if (stmt?.tag !== 'define') return
    expect(parseFunctionDocFromComments(stmt.docComments ?? [])).toEqual(
      expectedFunctionDoc,
    )
  })

  describe('parseDocString', () => {
    test('outputs when input string is good', () => {
      const { testComments, expectedFunctionDoc } = makeTestDocstring()

      expect(parseDocString(testComments)).toStrictEqual(expectedFunctionDoc)
    })
  })

  describe('parseDocLineContents', () => {
    describe('prefix', () => {
      test('undefined when too many semicolons', () => {
        const testDocLine = ';;;; a lot of semicolons!'
        expect(
          parseDocLineContents(makeTestComment(testDocLine)),
        ).toBeUndefined()
      })
      test('undefined when not enough semicolons', () => {
        const testDocLine = ';; not many semicolons!'
        expect(
          parseDocLineContents(makeTestComment(testDocLine)),
        ).toBeUndefined()
      })
      test('undefined when bad prefix', () => {
        const testDocLine = ';%; why is there a % in the prefix'
        expect(
          parseDocLineContents(makeTestComment(testDocLine)),
        ).toBeUndefined()
      })
    })
    test('returns the line with the docstring prefix stripped', () => {
      const restOfLine = 'good comment :)'
      const testDocLine = `;;; ${restOfLine}`
      expect(parseDocLineContents(makeTestComment(testDocLine))).toStrictEqual(
        makeTestComment(restOfLine),
      )
    })
  })

  const predHeadId = 'complex-pred?'
  const subPredId1 = 'pred1?'
  const subPredId2 = 'pred2?'
  const predicate = mkApp(
    mkVar(predHeadId, anyRange),
    [mkVar(subPredId1, anyRange), mkVar(subPredId2, anyRange)],
    anyRange,
  ) as Pred
  describe('parseParamSignature', () => {
    describe('valid param signature', () => {
      const name = 'param'
      test('w/ simple predicate', () => {
        const predId = 'pred?'
        const predicate = mkVar(predId, anyRange)
        const testDocLine = ` ${name} : ${predId}`
        const expectedParam: Param = {
          name,
          predicate,
          range: anyRange,
        }
        expect(
          parseParamSignature(makeTestComment(testDocLine)).param,
        ).toStrictEqual(expectedParam)
      })

      test('w/ complex predicate', () => {
        const testDocLine = ` ${name} : (${predHeadId} ${subPredId1} ${subPredId2})`
        const expectedParam: Param = {
          name,
          predicate,
          range: anyRange,
        }
        expect(
          parseParamSignature(makeTestComment(testDocLine)).param,
        ).toStrictEqual(expectedParam)
      })
    })
    // TODO: test
    // describe("illegal param signature", () => {
    // })
  })

  describe('parseParamDescriptionLine', () => {
    const leadPaddingCount = 2
    test('returns line if padding is good', () => {
      const expectedResult = 'hey this is a test description'
      const leadPadding = ' '.repeat(leadPaddingCount)
      const testDescLine = `${leadPadding} ${expectedResult}`
      expect(
        parseParamDescriptionLine(
          makeTestComment(testDescLine),
          leadPaddingCount,
        ),
      ).toStrictEqual(expectedResult)
    })
    test('throws if lead padding is bad', () => {
      const contents = 'this should NOT appear...'
      const testDescLine = ` ${contents}`
      expect(() =>
        parseParamDescriptionLine(
          makeTestComment(testDescLine),
          leadPaddingCount,
        ),
      ).toThrow('beginning whitespace')
    })
  })

  describe('parseSingleParam', () => {
    const name = 'param'
    const goodParamSignature = ` ${name} : (${predHeadId} ${subPredId1} ${subPredId2})`
    const remainingLine =
      'this one is just here since parseAllParams expects it'
    const otherRemainingLine =
      "this is to check that we don't change the order of stuff"
    const expectedRemainder = convertLinesToComments([
      remainingLine,
      otherRemainingLine,
    ])

    test('returns param w/ description', () => {
      const paramDescPart1 = 'this param does'
      const paramDescPart2 = 'something really cool!'
      const paramDescLine1 = `  ${paramDescPart1}`
      const paramDescLine2 = `  ${paramDescPart2}`
      const testDocLines = convertLinesToComments([
        goodParamSignature,
        paramDescLine1,
        paramDescLine2,
        remainingLine,
        otherRemainingLine,
      ])

      const description = `${paramDescPart1} ${paramDescPart2}`
      const expectedParam: Param = {
        name,
        predicate,
        description,
        range: anyRange,
      }

      expect(parseSingleParam(testDocLines)).toStrictEqual(expectedParam)
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })

    test('returns param w/o description', () => {
      const testDocLines = convertLinesToComments([
        goodParamSignature,
        remainingLine,
        otherRemainingLine,
      ])
      const expectedParam: Param = {
        name,
        predicate,
        description: undefined,
        range: anyRange,
      }
      expect(parseSingleParam(testDocLines)).toStrictEqual(expectedParam)
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })

    test('signals to move to description stage when param parsing failure', () => {
      const testDocLines = convertLinesToComments([
        remainingLine,
        otherRemainingLine,
      ])
      expect(parseSingleParam(testDocLines)).toStrictEqual(
        ParseStage.Description,
      )
      // it should have pushed back the consumed line
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })
  })

  describe('parseFunctionDescription', () => {
    const testDescriptionLine1 = 'this is the first line of the description'
    const testDescriptionLine2 = 'this is the SECOND line!'
    const expectedDescription = `${testDescriptionLine1} ${testDescriptionLine2}`
    const expectedRemainder = convertLinesToComments([
      testTagLine1,
      testTagLine2,
    ])
    test('empty string when begins with tag line', () => {
      const testDocLines = convertLinesToComments([testTagLine1, testTagLine2])
      const expectedResult = { stage: ParseStage.Tags, description: '' }
      expect(parseFunctionDescription(testDocLines)).toStrictEqual(
        expectedResult,
      )
      expect(testDocLines).toStrictEqual(expectedRemainder)
    })
    test('works w/ tag lines after', () => {
      const testDocLines = convertLinesToComments([
        testDescriptionLine1,
        testDescriptionLine2,
        testTagLine1,
        testTagLine2,
      ])
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
      const testDocLines = convertLinesToComments([
        testDescriptionLine1,
        testDescriptionLine2,
      ])
      expect(parseFunctionDescription(testDocLines)).toStrictEqual(
        expectedDescription,
      )
      expect(testDocLines).toStrictEqual([])
    })
  })

  describe('isComplexPred', () => {
    const name = 'complex-pred?'
    test('works on simple complex pred', () => {
      const subPred1 = 'pred1?'
      const subPred2 = 'pred2?'
      const testComplexPred: ComplexPred = mkApp(mkVar(name), [
        mkVar(subPred1),
        mkVar(subPred2),
      ]) as ComplexPred

      expect(isComplexPred(testComplexPred)).toBe(true)
    })
    test('works on nested complex pred', () => {
      const nestedName = 'nested-complex?'
      const subPred1 = 'pred1?'
      const subNestedName = 'another-nested?'
      const subPred2 = 'pred2?'
      const subPred3 = 'pred3?'
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

function makeTestDocstring(): {
  testComments: DocComment[]
  expectedFunctionDoc: FunctionDoc
  rawTestComments: Comment[]
} {
  const funcName = 'func'
  const paramName1 = 'p1'
  const paramName2 = 'p2'
  const funcApp = {
    ...mkApp(
      mkVar(funcName, anyRange),
      [mkVar(paramName1, anyRange), mkVar(paramName2, anyRange)],
      anyRange,
    ),
    restParam: undefined,
  } as VarApp

  const complexPredName1 = 'complex-pred1?'
  const predName1 = 'pred1?'
  const predicate1 = mkApp(
    mkVar(complexPredName1, anyRange),
    [mkVar(predName1, anyRange)],
    anyRange,
  ) as VarApp

  const predName2 = 'pred2?'
  const predicate2 = mkVar(predName2, anyRange)

  const paramDescLine1 = 'this parameter is amazing'
  const paramDescLine2 = 'i really like it'
  const paramDescription = `${paramDescLine1} ${paramDescLine2}`

  const complexPredName2 = 'complex-pred2?'
  const predName3 = 'pred3?'
  const predName4 = 'pred4?'
  const predicate3 = mkApp(
    mkVar(complexPredName2, anyRange),
    [mkVar(predName3, anyRange), mkVar(predName4, anyRange)],
    anyRange,
  ) as VarApp

  const descriptionLine1 = 'this test function is really cool...'
  const descriptionLine2 = "isn't it?"
  const description = `${descriptionLine1} ${descriptionLine2}`

  const tag1 = testTag1
  const tagContents1 = 'stuff1 stuff2'

  const tag2 = testTag2
  const tagContents2 = 'stuff3'

  const rawTestComments =
    `;;; (${funcName} ${paramName1} ${paramName2}) -> (${complexPredName1} ${predName1})
;;;  ${paramName1} : ${predName2}
;;;   ${paramDescLine1}
;;;   ${paramDescLine2}
;;;  ${paramName2} : (${complexPredName2} ${predName3} ${predName4})
;;; ${descriptionLine1}
;;; ${descriptionLine2}
;;; ${tag1} ${tagContents1}
;;; ${tag2} ${tagContents2}`
      .split('\n')
      .map((line): Comment => ({ line, range: anyRange }))

  const expectedFunctionDoc: FunctionDoc = {
    signature: {
      function: funcApp,
      predicate: predicate1,
      range: anyRange,
    },
    params: [
      {
        name: paramName1,
        predicate: predicate2,
        description: paramDescription,
        range: anyRange,
      },
      {
        name: paramName2,
        predicate: predicate3,
        description: undefined,
        range: anyRange,
      },
    ],
    restParam: undefined,
    description,
    tags: [
      { tag: tag1, contents: tagContents1, range: anyRange },
      { tag: tag2, contents: tagContents2, range: anyRange },
    ],
    range: anyRange,
  }

  return {
    testComments: commentsToDocComments(rawTestComments),
    expectedFunctionDoc,
    rawTestComments,
  }
}

function makeTestComment(line: string): Comment {
  return { line, range: anyRange }
}

function convertLinesToComments(lines: string[]): Comment[] {
  return lines.map((line): Comment => ({ line, range: anyRange }))
}
