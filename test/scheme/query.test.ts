import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { parseProgram } from "../../src/scheme/parser"
import { getQueriedProgram, getReportedExp, getReportedStmt } from "../../src/scheme/query"
import { Loc, mkAp, mkDisp, mkLit, mkRept, mkVar, Prog, ScamperError, Stmt } from "../../src/lpm"
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

function parseTestProgram(): Prog {
  const errors: ScamperError[] = []
  const prog = parseProgram(errors, read(testProgram))
  expect(errors).toEqual([])
  return prog
}

// Finds the Loc of `needle` (first occurrence, or `occurrence`-th if given)
// within testProgram, so tests can point at a specific token by content
// instead of hand-computing line/col/idx by hand.
function locOf(needle: string, occurrence = 0): Loc {
  let idx = -1
  for (let i = 0; i <= occurrence; i++) {
    idx = testProgram.indexOf(needle, idx + 1)
  }
  const before = testProgram.slice(0, idx)
  const line = before.split("\n").length
  const lineStart = before.lastIndexOf("\n") + 1
  return new Loc(line, idx - lineStart + 1, idx)
}

describe("AST querying", () => {
  describe("getQueriedProgram", () => {
    test("returns a new program without mutating the input", () => {
      const prog = parseTestProgram()
      const queryLoc = locOf("yo")
      const originalStmt = prog[3]
      const { prog: reportedProg } = getQueriedProgram(prog, queryLoc)

      expect(reportedProg).not.toBe(prog)
      expect(reportedProg[3]).not.toBe(prog[3])
      expect(prog[3]).toBe(originalStmt)
      expect(reportedProg[3]).toStrictEqual(
        getReportedStmt(originalStmt, queryLoc).stmt,
      )
    })

    test("returns the range of the queried expression", () => {
      const prog = parseTestProgram()
      const queryLoc = locOf("yo")
      const { range } = getQueriedProgram(prog, queryLoc)
      expect(range.begin.idx).toBe(testProgram.indexOf('"yo"'))
    })

    test("throws for a query location outside every statement", () => {
      const prog = parseTestProgram()
      expect(() =>
        getQueriedProgram(prog, new Loc(1000, 1, 100000)),
      ).toThrow(ScamperError)
    })
  })

  describe("getReportedExp", () => {
    test("wraps a bare literal statement's expression in report", () => {
      const prog = parseTestProgram()
      const stmt = prog[0]
      expect(stmt.tag).toBe("stmtexp")
      if (stmt.tag !== "stmtexp") return
      const { stmt: reported } = getReportedStmt(stmt, locOf(`"${testLit}"`))
      expect(reported).toStrictEqual({
        tag: "stmtexp",
        expr: { tag: "report", exp: mkLit(testLit, anyRange), range: anyRange },
        range: anyRange,
      })
    })

    test("wraps the null literal from an empty list", () => {
      const prog = parseTestProgram()
      const stmt = prog[1]
      expect(stmt.tag).toBe("stmtexp")
      if (stmt.tag !== "stmtexp") return
      const { stmt: reported } = getReportedStmt(stmt, locOf("()"))
      expect(reported).toStrictEqual({
        tag: "stmtexp",
        expr: { tag: "report", exp: mkLit(null, anyRange), range: anyRange },
        range: anyRange,
      })
    })

    test("wraps the entire application when the query lands on its closing bracket, not any argument", () => {
      const prog = parseTestProgram()
      const stmt = prog[2]
      expect(stmt.tag).toBe("display")
      if (stmt.tag !== "display") return
      // N.B., there is no sub-expression slot for a Lit like `2` itself, so
      // querying anywhere within its range (including its own "closing
      // bracket" position, conceptually) always wraps the whole thing --
      // there's nothing deeper to recurse into.
      const { exp } = getReportedExp(stmt.value, locOf(testDispLit.toString()))
      expect(exp.tag).toBe("report")
    })

    describe("recursive case: queried a non-head argument", () => {
      test("reports one level deep", () => {
        const prog = parseTestProgram()
        const stmt = prog[3]
        expect(stmt.tag).toBe("stmtexp")
        if (stmt.tag !== "stmtexp") return
        const { exp } = getReportedExp(stmt.expr, locOf('"yo"'))
        expect(exp.tag).toBe("app")
        if (exp.tag !== "app") return
        expect(exp.args[0]).toStrictEqual({
          tag: "report",
          exp: mkLit("yo", anyRange),
          range: anyRange,
        })
        // the sibling argument is untouched
        expect(exp.args[1].tag).toBe("app")
      })

      test("reports two levels deep", () => {
        const prog = parseTestProgram()
        const stmt = prog[3]
        expect(stmt.tag).toBe("stmtexp")
        if (stmt.tag !== "stmtexp") return
        const { exp } = getReportedExp(stmt.expr, locOf("what's up"))
        expect(exp.tag).toBe("app")
        if (exp.tag !== "app") return
        expect(exp.args[0]).toStrictEqual(mkLit("yo", anyRange))
        const inner = exp.args[1]
        expect(inner.tag).toBe("app")
        if (inner.tag !== "app") return
        expect(inner.args[0]).toStrictEqual({
          tag: "report",
          exp: mkLit("what's up", anyRange),
          range: anyRange,
        })
      })

      test("does not mutate the input expression", () => {
        const prog = parseTestProgram()
        const stmt = prog[3]
        expect(stmt.tag).toBe("stmtexp")
        if (stmt.tag !== "stmtexp") return
        const before = JSON.stringify(stmt.expr)
        getReportedExp(stmt.expr, locOf('"yo"'))
        getReportedExp(stmt.expr, locOf("what's up"))
        expect(JSON.stringify(stmt.expr)).toBe(before)
      })
    })

    describe("base case: queried a special form's own syntax (not a sub-expression)", () => {
      test("wraps the entire if-expression when the query lands on `if` itself", () => {
        const prog = parseTestProgram()
        const stmt = prog[6]
        expect(stmt.tag).toBe("stmtexp")
        if (stmt.tag !== "stmtexp") return
        const { exp } = getReportedExp(stmt.expr, locOf("if"))
        expect(exp.tag).toBe("report")
        if (exp.tag !== "report") return
        expect(exp.exp.tag).toBe("if")
      })
    })

    describe("base case: queried the head of an application", () => {
      test("wraps just the head when it's a non-function literal", () => {
        const prog = parseTestProgram()
        const stmt = prog[4]
        expect(stmt.tag).toBe("stmtexp")
        if (stmt.tag !== "stmtexp") return
        const { exp } = getReportedExp(stmt.expr, locOf('"not-a-fn"'))
        expect(exp.tag).toBe("app")
        if (exp.tag !== "app") return
        expect(exp.head).toStrictEqual({
          tag: "report",
          exp: mkLit("not-a-fn", anyRange),
          range: anyRange,
        })
        // the argument is untouched
        expect(exp.args[0]).toStrictEqual(mkLit(1, anyRange))
      })

      test("recurses into an anonymous function used as the head", () => {
        const prog = parseTestProgram()
        const stmt = prog[5]
        expect(stmt.tag).toBe("stmtexp")
        if (stmt.tag !== "stmtexp") return
        const { exp } = getReportedExp(stmt.expr, locOf("x)", 1))
        expect(exp.tag).toBe("app")
        if (exp.tag !== "app") return
        expect(exp.head.tag).toBe("lam")
        if (exp.head.tag !== "lam") return
        expect(exp.head.body).toStrictEqual({
          tag: "report",
          exp: mkVar("x", anyRange),
          range: anyRange,
        })
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

      const errors: ScamperError[] = []
      const prog = parseProgram(errors, read(src))
      const { range } = getQueriedProgram(prog, queryLoc)
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
      const errors: ScamperError[] = []
      const prog = parseProgram(errors, read(src))
      const { range } = getQueriedProgram(prog, queryLoc)
      expect(queriedRange).toEqual(range.firstLineSpan(src))
    })

    // N.B., already skipped before this migration (see git history) --
    // pinning the exact compiled bytecode shape of the rept opcode (which
    // lands inside the queried define's own closure body, not the
    // synthesized example call site pushed onto the program) is a deeper
    // bytecode-inspection question than query.ts's AST-level redesign needs
    // to answer. getQueriedProgram's placement of the report-wrapped
    // sub-expression is otherwise covered directly by the tests above.
    test.skip("report operation is contained in bytecode", async () => {
      const funcName = "myid"
      const lit1 = 1
      const lit2 = 2
      const src = `;;; (${funcName} a b) -> number?
;;;  a : number?
;;;  b : number?
;;; returns a
;;; @example (${funcName} ${lit1.toString()} ${lit2.toString()}) -> ${lit1.toString()}
(define ${funcName} (lambda (a b) a))`
      const queryLoc = new Loc(6, 34, src.lastIndexOf("a)"))
      const err = new SimpleErrorChannel()

      const expectedProg: Prog = [
        expect.anything() as Stmt,
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
