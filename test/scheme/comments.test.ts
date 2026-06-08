import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"
import { tokenizeAndParse } from "../../src/scheme"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"
import { mkDefine, mkLit } from "../../src/scheme/ast"
import { Range } from "../../src/lpm"

const comment = "this should appear in the comment!"
const identifier = "x"
const value = 1
const lit = mkLit(value, expect.anything() as Range)
const testSrc = `
;${comment}
(define ${identifier} ${value.toString()})
`

describe("Comments", () => {
  test("are read and attached to syntax", () => {
    const vals = read(testSrc)
    expect(vals).toEqual(
      expect.arrayContaining([expect.objectContaining({ comment })]),
    )
  })
  test("are attached to define statements", () => {
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, testSrc)
    const expectedDefine = mkDefine(
      identifier,
      lit,
      expect.anything() as Range,
      comment,
    )
    expect(prog).toEqual(expect.arrayContaining([expectedDefine]))
  })
})
