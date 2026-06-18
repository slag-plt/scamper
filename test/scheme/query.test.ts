import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { getQueriedAST, getReportedSyntax } from "../../src/scheme/query"
import {
  Loc,
  mkAp,
  mkDisp,
  mkList,
  mkLit,
  mkRept,
  mkSym,
  mkVar,
  Prog,
} from "../../src/lpm"
import { mkSyntax, Syntax } from "../../src/scheme/syntax"
import { anyRange } from "./util"
import { compile } from "../../src/scheme"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"

const testLit = `test lit`
const testDispLit = 2

const testProgram = `"${testLit}"
()
(display ${testDispLit.toString()})
(test-func1
  "yo"
  (test-func2 "what's up"))
("not-a-fn" 1)
((lambda (x) x) 5)
(if #t 1 2)`
const testSexps = read(testProgram)

describe("AST querying", () => {
  describe("getQueriedAST", () => {
    test("returns a new ast without mutating the input", () => {
      const ast = read(testProgram)
      const queryLoc = new Loc(5, 4, 41)
      const originalSexp = ast[3]
      const reportedAst = getQueriedAST(ast, queryLoc)

      expect(reportedAst).not.toBe(ast)
      expect(reportedAst[3]).not.toBe(ast[3])
      expect(ast[3]).toBe(originalSexp)
      expect(reportedAst[3]).toStrictEqual(
        getReportedSyntax(originalSexp, queryLoc),
      )
    })
  })

  describe("getReportedSyntax", () => {
    test("wraps lits in report", () => {
      const testSyntax = testSexps[0]
      const testQueryLoc = new Loc(0, 2, 2)
      const expectedSyntax: Syntax = mkSyntax(
        mkList(getReportSym(), testLit),
        anyRange,
      )
      expectSexp(testSyntax, testQueryLoc, expectedSyntax)
    })
    test("wraps null list in report", () => {
      const testSyntax = testSexps[1]
      const testQueryLoc = new Loc(1, 1, 14)
      const expectedSyntax: Syntax = mkSyntax(
        mkList(getReportSym(), null),
        anyRange,
      )
      expectSexp(testSyntax, testQueryLoc, expectedSyntax)
    })
    test("wraps entire func app in report if queried ending bracket", () => {
      const testSyntax = testSexps[2]
      const testQueryLoc = new Loc(2, 10, 27)
      const expectedSyntax: Syntax = mkSyntax(
        mkList(
          getReportSym(),
          mkList(
            mkSyntax(mkSym("display"), anyRange),
            mkSyntax(testDispLit, anyRange),
          ),
        ),
        anyRange,
      )
      expectSexp(testSyntax, testQueryLoc, expectedSyntax)
    })
    describe("recursive case: queried non-head", () => {
      test("reports one level deep", () => {
        const testSyntax = testSexps[3]
        const testQueryLoc = new Loc(5, 4, 41)
        const expectedSyntax: Syntax = mkSyntax(
          mkList(
            mkSyntax(mkSym("test-func1"), anyRange),
            mkSyntax(mkList(getReportSym(), "yo"), anyRange),
            mkSyntax(
              mkList(
                mkSyntax(mkSym("test-func2"), anyRange),
                mkSyntax("what's up", anyRange),
              ),
              anyRange,
            ),
          ),
          anyRange,
        )
        expectSexp(testSyntax, testQueryLoc, expectedSyntax)
      })
      test("reports two levels deep", () => {
        const testSyntax = testSexps[3]
        const testQueryLoc = new Loc(6, 18, 62)
        const expectedSyntax: Syntax = mkSyntax(
          mkList(
            mkSyntax(mkSym("test-func1"), anyRange),
            mkSyntax("yo", anyRange),
            mkSyntax(
              mkList(
                mkSyntax(mkSym("test-func2"), anyRange),
                mkSyntax(mkList(getReportSym(), "what's up"), anyRange),
              ),
              anyRange,
            ),
          ),
          anyRange,
        )
        expectSexp(testSyntax, testQueryLoc, expectedSyntax)
      })
      test("does not mutate the input syntax", () => {
        const testSyntax = testSexps[3]
        const oneLevelLoc = new Loc(5, 4, 41)
        const twoLevelLoc = new Loc(6, 18, 62)

        getReportedSyntax(testSyntax, oneLevelLoc)
        expect(getReportedSyntax(testSyntax, twoLevelLoc)).toStrictEqual(
          mkSyntax(
            mkList(
              mkSyntax(mkSym("test-func1"), anyRange),
              mkSyntax("yo", anyRange),
              mkSyntax(
                mkList(
                  mkSyntax(mkSym("test-func2"), anyRange),
                  mkSyntax(mkList(getReportSym(), "what's up"), anyRange),
                ),
                anyRange,
              ),
            ),
            anyRange,
          ),
        )
      })
    })
    describe("base case: queried head", () => {
      test("wraps entire function if function name is reserved", () => {
        const testSyntax = testSexps[6]
        const testQueryLoc = new Loc(9, 3, 109)
        const expectedSyntax: Syntax = mkSyntax(
          mkList(
            getReportSym(),
            mkList(
              mkSyntax(mkSym("if"), anyRange),
              mkSyntax(true, anyRange),
              mkSyntax(1, anyRange),
              mkSyntax(2, anyRange),
            ),
          ),
          anyRange,
        )
        expectSexp(testSyntax, testQueryLoc, expectedSyntax)
      })
      test("wraps non-function value that was attempted to be applied", () => {
        const testSyntax = testSexps[4]
        const testQueryLoc = new Loc(7, 4, 76)
        const expectedSyntax: Syntax = mkSyntax(
          mkList(
            mkSyntax(mkList(getReportSym(), "not-a-fn"), anyRange),
            mkSyntax(1, anyRange),
          ),
          anyRange,
        )
        expectSexp(testSyntax, testQueryLoc, expectedSyntax)
      })
      test("recurses on anonymous function", () => {
        const testSyntax = testSexps[5]
        const testQueryLoc = new Loc(8, 14, 101)
        const expectedSyntax: Syntax = mkSyntax(
          mkList(
            mkSyntax(
              mkList(
                mkSyntax(mkSym("lambda"), anyRange),
                mkSyntax(mkList(mkSyntax(mkSym("x"), anyRange)), anyRange),
                mkSyntax(mkList(getReportSym(), mkSym("x")), anyRange),
              ),
              anyRange,
            ),
            mkSyntax(5, anyRange),
          ),
          anyRange,
        )
        expectSexp(testSyntax, testQueryLoc, expectedSyntax)
      })
    })
  })

  describe("compilation with query loc", () => {
    test("report operation is contained in bytecode", () => {
      const funcName = "+"
      const lit1 = 1
      const lit2 = 2
      const src = `(${funcName} ${lit1.toString()} ${lit2.toString()})`
      const queryLoc = new Loc(0, 1, 1)
      const err = new SimpleErrorChannel()

      const expectedProg: Prog = [
        mkDisp(
          [
            mkVar(funcName, anyRange),
            mkRept(anyRange),
            mkLit(lit1, anyRange),
            mkLit(lit2, anyRange),
            mkAp(2, anyRange),
          ],
          anyRange,
        ),
      ]

      expect(compile(err, src, queryLoc)).toStrictEqual(expectedProg)
    })
  })
})

function expectSexp(
  testSyntax: Syntax,
  testQueryLoc: Loc,
  expectedSyntax: Syntax,
) {
  expect(getReportedSyntax(testSyntax, testQueryLoc)).toStrictEqual(
    expectedSyntax,
  )
}

function getReportSym() {
  return mkSym("report")
}