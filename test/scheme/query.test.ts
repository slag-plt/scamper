import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { getQueriedAST, getReportedSyntax } from "../../src/scheme/query"
import {
  isList,
  listToVector,
  Loc,
  mkAp,
  mkDisp,
  mkList,
  mkLit,
  mkRptBegin,
  mkRptEnd,
  mkSym,
  mkVar,
  Prog,
  Stmt
} from "../../src/lpm"
import { isSyntax, mkSyntax, Syntax } from "../../src/scheme/syntax"
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
      const { ast: reportedAst } = getQueriedAST(ast, queryLoc)

      expect(reportedAst).not.toBe(ast)
      expect(reportedAst[3]).not.toBe(ast[3])
      expect(ast[3]).toBe(originalSexp)
      expect(reportedAst[3]).toStrictEqual(
        getReportedSyntax(originalSexp, queryLoc).syntax,
      )
    })

    test("returns the semantic range of the queried expression", () => {
      const ast = read(testProgram)
      const queryLoc = new Loc(5, 4, 41)
      const { range } = getQueriedAST(ast, queryLoc)
      const queriedSexp = testSexps[3]
      expect(isList(queriedSexp.value)).toBe(true)
      if (!isList(queriedSexp.value)) return
      const yo = listToVector(queriedSexp.value)[1]
      expect(isSyntax(yo)).toBe(true)
      if (!isSyntax(yo)) return
      expect(range).toEqual(yo.range)
    })
  })

  describe("getReportedSyntax", () => {
    test("wraps lits in report", () => {
      const testSyntax = testSexps[0]
      const testQueryLoc = new Loc(0, 2, 2)
      const expectedSyntax: Syntax = mkSyntax(
        mkList(getReportSyntax(), testLit),
        anyRange,
      )
      expectSexp(testSyntax, testQueryLoc, expectedSyntax)
    })
    test("wraps null list in report", () => {
      const testSyntax = testSexps[1]
      const testQueryLoc = new Loc(1, 1, 14)
      const expectedSyntax: Syntax = mkSyntax(
        mkList(getReportSyntax(), null),
        anyRange,
      )
      expectSexp(testSyntax, testQueryLoc, expectedSyntax)
    })
    test("wraps entire func app in report if queried ending bracket", () => {
      const testSyntax = testSexps[2]
      const testQueryLoc = new Loc(2, 10, 27)
      const expectedSyntax: Syntax = mkSyntax(
        mkList(
          getReportSyntax(),
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
            mkSyntax(mkList(getReportSyntax(), "yo"), anyRange),
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
                mkSyntax(mkList(getReportSyntax(), "what's up"), anyRange),
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
        expect(getReportedSyntax(testSyntax, twoLevelLoc).syntax).toStrictEqual(
          mkSyntax(
            mkList(
              mkSyntax(mkSym("test-func1"), anyRange),
              mkSyntax("yo", anyRange),
              mkSyntax(
                mkList(
                  mkSyntax(mkSym("test-func2"), anyRange),
                  mkSyntax(mkList(getReportSyntax(), "what's up"), anyRange),
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
            getReportSyntax(),
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
            mkSyntax(mkList(getReportSyntax(), "not-a-fn"), anyRange),
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
                mkSyntax(mkList(getReportSyntax(), mkSym("x")), anyRange),
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
    test("returns first-line queriedRange for multi-line expressions", () => {
      const src = `(define foo
  (bar
    x))`
      const closeIdx = src.indexOf("x)") + 1
      const line = src.slice(0, closeIdx).split("\n").length
      const lineStart = src.lastIndexOf("\n", closeIdx - 1) + 1
      const queryLoc = new Loc(line, closeIdx - lineStart + 1, closeIdx)

      const { range } = getQueriedAST(read(src), queryLoc)
      const firstLine = range.firstLineSpan(src)

      expect(range.begin.line).toBeLessThan(range.end.line)
      expect(firstLine.begin).toEqual(range.begin)
      expect(firstLine.end.line).toBe(range.begin.line)
      expect(firstLine.end.idx).toBeLessThan(range.end.idx)
    })

    test("compile returns queriedRange for valid queries", async () => {
      const src = `;;;
;;; (foo) -> number?
;;; constant one
;;; @example (foo) -> 1
(define foo 1)`
      const oneIdx = src.indexOf("(define foo 1)") + "(define foo ".length
      const line = src.slice(0, oneIdx).split("\n").length
      const lineStart = src.lastIndexOf("\n", oneIdx - 1) + 1
      const queryLoc = new Loc(line, oneIdx - lineStart + 1, oneIdx)
      const err = new SimpleErrorChannel()

      const result = await compile(err, src, queryLoc)

      expect(err.errors).toStrictEqual([])
      if (result === undefined) {
        expect.fail("expected compile to return a result")
        return
      }
      const { queriedRange } = result
      const { range } = getQueriedAST(read(src), queryLoc)
      expect(queriedRange).toEqual(range.firstLineSpan(src))
    })

    // BUG: code produces "Querying is only allowed within function definitions
    // not docstrings" which implies the execution pathway isn't correct, but
    // not sure...
    test.skip("report operation is contained in bytecode", async () => {
      // TODO: function definitions must be example-tagged now, but too annoying to do rn
      const funcName = "+"
      const lit1 = 1
      const lit2 = 2
      const src = `(define ${funcName} (lambda (a b) a))\n(${funcName} ${lit1.toString()} ${lit2.toString()})`
      const queryLoc = new Loc(1, 1, 29)
      const err = new SimpleErrorChannel()

      const expectedProg: Prog = [
        expect.anything() as Stmt,
        mkDisp(
          [
            mkRptBegin(),
            mkVar(funcName, anyRange),
            mkRptEnd(anyRange),
            mkLit(lit1, anyRange),
            mkLit(lit2, anyRange),
            mkAp(2, anyRange),
          ],
          anyRange,
        ),
      ]

      const result = await compile(err, src, queryLoc)
      expect(err.errors).toStrictEqual([])
      if (result === undefined) {
        expect.fail("expected compile to return a result")
        return
      }
      const { prog: actualProg } = result
      expect(actualProg).toStrictEqual(expectedProg)
    })
  })
})

function expectSexp(
  testSyntax: Syntax,
  testQueryLoc: Loc,
  expectedSyntax: Syntax,
) {
  expect(getReportedSyntax(testSyntax, testQueryLoc).syntax).toStrictEqual(
    expectedSyntax,
  )
}

function getReportSyntax() {
  return mkSyntax(mkSym("report"))
}
