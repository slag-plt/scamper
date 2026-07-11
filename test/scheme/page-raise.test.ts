import { describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import { Loc, ReportError } from "../../src/lpm"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { expToString } from "../../src/scheme/ast"
import { raisePage } from "../../src/scheme/raise"
import { compile, mkInitialEnv } from "../../src/scheme"

function executeUntilReport(fiber: Fiber): ReportError {
  try {
    while (!fiber.isDone()) {
      fiber.step()
    }
  } catch (error) {
    if (error instanceof ReportError) {
      return error
    }
    throw error
  }
  throw new Error("Expected the query to report a value")
}

function executeReportedTarget(fiber: Fiber): ReportError {
  let markedRoot = false
  try {
    while (!fiber.isDone()) {
      fiber.step()
      if (!markedRoot && fiber.currentFrame?.name === "##stmt-1##") {
        fiber.currentFrame.queryRoot = true
        markedRoot = true
      }
    }
  } catch (error) {
    if (error instanceof ReportError) {
      return error
    }
    throw error
  }
  throw new Error("Expected the report to capture a target")
}

function locAt(source: string, index: number): Loc {
  const line = source.slice(0, index).split("\n").length
  const lineStart = source.lastIndexOf("\n", index - 1) + 1
  return new Loc(line, index - lineStart + 1, index)
}

async function queryTargetPage(source: string, selected: string) {
  const err = new SimpleErrorChannel()
  const selectedIndex = source.lastIndexOf(selected)
  expect(selectedIndex).toBeGreaterThanOrEqual(0)
  const compiled = await compile(
    err,
    source,
    locAt(source, selectedIndex + selected.length - 1),
  )
  expect(err.errors).toEqual([])
  if (!compiled) {
    throw new Error("Expected query compilation to succeed")
  }
  const report = executeUntilReport(new Fiber(compiled.prog, mkInitialEnv()))
  if (report.capture.tag !== "page-graph") {
    throw new Error("Expected the query to capture recursive pages")
  }
  return report.capture.pageGraph.rootPage
}

async function raiseQueryTarget(source: string, selected: string) {
  return raisePage(await queryTargetPage(source, selected))
}

async function raiseReportedExpression(source: string) {
  const err = new SimpleErrorChannel()
  const compiled = await compile(err, source)
  expect(err.errors).toEqual([])
  if (!compiled) {
    throw new Error("Expected report compilation to succeed")
  }
  const report = executeReportedTarget(new Fiber(compiled, mkInitialEnv()))
  if (report.capture.tag !== "page-graph") {
    throw new Error("Expected the report to capture recursive pages")
  }
  return raisePage(report.capture.pageGraph.rootPage)
}

const fibSource = `;;; (fib n) -> number?
;;;  n: number?
;;; calculate fib n
;;; @example (fib 4) -> 3
(define fib
  (lambda (n)
    (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))`

describe("recursive page raising", () => {
  test("retains recursive applications inside a settled enclosing application", async () => {
    const raised = await raiseQueryTarget(fibSource, "(fib (- n 1))")

    expect(expToString(raised.source)).toBe("(if #f … (+ (fib 2) 1))")
    expect(raised.links).toHaveLength(1)

    const [linkApp, link] = [...raised.links.entries()][0]
    expect(expToString(linkApp)).toBe("(fib 2)")
    expect(link.targetPage.invocation.node.args).toEqual([2])
  })

  test("keeps lets while substituting their selected binding", async () => {
    const source = `;;; (count n) -> number?
;;;  n: number?
;;; count down
;;; @example (count 3) -> 0
(define count
  (lambda (n)
    (let ([next (- n 1)])
      (if (<= n 0)
        0
        (count next)))))`

    const raised = await raiseQueryTarget(source, "(count next)")

    expect(expToString(raised.source)).toBe(
      "(let ([next 1]) (if #f … (count 1)))",
    )
    expect(raised.links).toHaveLength(1)
  })

  test("matches the frame raiser by omitting completed begin prefixes", async () => {
    const source = `;;; (count n) -> number?
;;;  n: number?
;;; count down
;;; @example (count 3) -> 0
(define count
  (lambda (n)
    (begin
      (+ n 1)
      (if (<= n 0)
        0
        (count (- n 1))))))`

    const raised = await raiseQueryTarget(source, "(count (- n 1))")

    expect(expToString(raised.source)).toBe("(if #f … (count 1))")
  })

  test("keeps a route-bearing scrutinee but reduces ordinary scrutinees", async () => {
    const source = `;;; (f n) -> number?
;;;  n: number?
;;; recurse through a condition
;;; @example (f 4) -> 1
(define f
  (lambda (n)
    (if (= n 0)
      0
      (if (= (f (- n 1)) 0)
        1
        2))))`

    const raised = await raiseQueryTarget(source, "(f (- n 1))")

    expect(expToString(raised.source)).toBe("(if #f … (if (= (f 2) 0) … 2))")
    expect(raised.links).toHaveLength(1)
  })

  test("fails rather than inventing source when trace provenance is missing", async () => {
    const page = await queryTargetPage(fibSource, "(fib (- n 1))")
    const malformed = {
      ...page,
      invocation: {
        ...page.invocation,
        children: page.invocation.children.slice(1),
      },
    }

    expect(() => raisePage(malformed)).toThrow("No captured invocation")
  })

  test("keeps match patterns and replaces every unselected body", async () => {
    const source = `(define sum
  (lambda (xs)
    (match xs
      [null 0]
      [(cons x rest) (+ x (sum rest))])))
(report (sum (cons 2 (cons 1 null))))`

    const raised = await raiseReportedExpression(source)

    expect(expToString(raised.source)).toBe(
      "(match (list 2 1) [null …] [(cons x rest) (+ 2 (sum (list 1)))])",
    )
    expect(raised.links).toHaveLength(1)
  })
})
