import { describe, expect, test } from "vitest"
import { mkDefine, mkLit } from "../../src/scheme/ast"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { tokenizeAndParse } from "../../src/scheme"
import { anyRange } from "../scheme/util"

describe("docstring bugs", () => {
  // `;` comments are not docstrings (`;;;`); they must not be passed to parseDocString.
  test("should not attempt to parse a block of non-doc comments", () => {
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, `
; test

(define x 1)
`)
    expect(err.errors).toEqual([])
    expect(prog).toStrictEqual([mkDefine("x", mkLit(1, anyRange), anyRange)])
  })
})
