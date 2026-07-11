import { describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import { getReportCaptureValue, Loc, ReportError } from "../../src/lpm"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
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

function locAt(source: string, index: number): Loc {
  const line = source.slice(0, index).split("\n").length
  const lineStart = source.lastIndexOf("\n", index - 1) + 1
  return new Loc(line, index - lineStart + 1, index)
}

async function runQueryAt(source: string, index: number): Promise<ReportError> {
  const err = new SimpleErrorChannel()
  const compiled = await compile(err, source, locAt(source, index))
  expect(err.errors).toEqual([])
  if (!compiled) {
    throw new Error("Expected query compilation to succeed")
  }
  const fiber = new Fiber(compiled.prog, mkInitialEnv())
  const report = executeUntilReport(fiber)
  return report
}

const fibSource = `;;; (fib n) -> number?
;;;  n: number?
;;; calculate fib n
;;; @example (fib 5) -> 5
(define fib
  (lambda (n)
    (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))`

describe("re-entrant target reports", () => {
  test("reports the outer selected recursive call after nested re-entries finish", async () => {
    const selected = "(fib (- n 1))"
    const report = await runQueryAt(
      fibSource,
      fibSource.indexOf(selected) + selected.length - 1,
    )

    expect(report.capture.tag).toBe("page-graph")
    if (report.capture.tag !== "page-graph") {
      return
    }
    expect(report.capture.pageGraph.rootPage.invocation.node.args).toEqual([4])
    expect(report.capture.pageGraph.rootPage.invocation.node.result).toBe(5)
    expect(getReportCaptureValue(report.capture)).toBe(5)
  })

  test("reports the selected second recursive call after unselected work completes", async () => {
    const selected = "(fib (- n 2))"
    const report = await runQueryAt(
      fibSource,
      fibSource.indexOf(selected) + selected.length - 1,
    )

    expect(report.capture.tag).toBe("page-graph")
    if (report.capture.tag !== "page-graph") {
      return
    }
    expect(report.capture.pageGraph.rootPage.invocation.node.args).toEqual([3])
    expect(getReportCaptureValue(report.capture)).toBe(3)
  })

  test("does not let a recursive argument claim the owner's target call", async () => {
    const source = `;;; (count-down n) -> number?
;;;  n: number?
;;; count down through a recursive argument
;;; @example (count-down 3) -> 0
(define count-down
  (lambda (n)
    (if (<= n 0)
      0
      (count-down (begin (count-down (- n 1)) (- n 1))))))`
    const selected = "(count-down (begin (count-down (- n 1)) (- n 1)))"
    const report = await runQueryAt(
      source,
      source.indexOf(selected) + selected.length - 1,
    )

    expect(report.capture.tag).toBe("page-graph")
    if (report.capture.tag !== "page-graph") {
      return
    }
    expect(report.capture.pageGraph.rootPage.invocation.node.args).toEqual([2])
    expect(getReportCaptureValue(report.capture)).toBe(0)
  })

  test("reports non-recursive targets as settled values", async () => {
    const source = `;;; (decrement n) -> number?
;;;  n: number?
;;; subtract one
;;; @example (decrement 5) -> 4
(define decrement
  (lambda (n)
    (- n 1)))`
    const selected = "(- n 1)"
    const report = await runQueryAt(
      source,
      source.indexOf(selected) + selected.length - 1,
    )

    expect(report.capture).toEqual({ tag: "value", value: 4 })
  })

  test("reports literal targets directly", async () => {
    const source = `;;; (increment n) -> number?
;;;  n: number?
;;; add one
;;; @example (increment 5) -> 6
(define increment
  (lambda (n)
    (+ n 1)))`
    const selected = "1)))"
    const report = await runQueryAt(source, source.indexOf(selected))

    expect(report.capture).toEqual({ tag: "value", value: 1 })
  })

  test("reports recursive compound targets as settled values", async () => {
    const selected = "(if (<= n 1)"
    const report = await runQueryAt(fibSource, fibSource.indexOf(selected) + 1)

    expect(report.capture).toEqual({ tag: "value", value: 8 })
  })
})
