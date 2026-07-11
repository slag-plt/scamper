import { describe, expect, test } from "vitest"
import { Fiber } from "../../src/lpm/fiber"
import { Loc, ReportError } from "../../src/lpm"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { compile, mkInitialEnv } from "../../src/scheme"
import { renderRecursivePage } from "../../src/web/components/query/page-render"

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

async function queryTargetPage(source: string, selected: string) {
  const err = new SimpleErrorChannel()
  const selectedIndex = source.lastIndexOf(selected)
  const compiled = await compile(
    err,
    source,
    locAt(source, selectedIndex + selected.length - 1),
  )
  if (!compiled) {
    throw new Error("Expected query compilation to succeed")
  }
  const report = executeUntilReport(new Fiber(compiled.prog, mkInitialEnv()))
  if (report.capture.tag !== "page-graph") {
    throw new Error("Expected the query to capture recursive pages")
  }
  return report.capture.pageGraph.rootPage
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

describe("recursive page rendering", () => {
  test("formats raised code and separates the retained page link", async () => {
    const page = await queryTargetPage(fibSource, "(fib (- n 1))")

    const segments = await renderRecursivePage(page)

    expect(segments).toEqual([
      { tag: "text", text: "(if #f\n  …\n  (+ " },
      expect.objectContaining({ tag: "link", text: "(fib 2)" }),
      { tag: "text", text: " 1))" },
    ])
  })
})
