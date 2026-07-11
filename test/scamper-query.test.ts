import { afterEach, describe, expect, test } from "vitest"
import { Loc } from "../src/lpm"
import { SimpleErrorChannel } from "../src/lpm/output/simple-error"
import Scamper from "../src/scamper"

function locAt(source: string, index: number): Loc {
  const line = source.slice(0, index).split("\n").length
  const lineStart = source.lastIndexOf("\n", index - 1) + 1
  return new Loc(line, index - lineStart + 1, index)
}

describe("query completion", () => {
  afterEach(() => {
    Scamper.getInstance().invalidateAllQueries()
  })

  test("settles an unreachable target without a report error", async () => {
    const source = `;;; (choose n) -> number?
;;;  n: number?
;;; choose the input
;;; @example (choose 0) -> 0
(define choose
  (lambda (n)
    (if #t
      n
      (+ n 1))))`
    const selected = "(+ n 1)"
    const scamper = Scamper.getInstance()
    const err = new SimpleErrorChannel()

    await scamper.query({
      src: source,
      err,
      queryLoc: locAt(source, source.indexOf(selected) + selected.length - 1),
    })

    const query = [...scamper.queries.values()].flat().at(0)
    expect(query).toBeDefined()
    if (!query) {
      return
    }
    await query.done

    expect(err.errors).toEqual([])
  })
})
