import { describe, expect, test } from "vitest"
import { read } from "../../src/scheme/reader"

const identifier = "x"
const value = 1

describe("Comments", () => {
  const comment = "; this should appear in the comment!"
  const testSrc = `
  ${comment}
  (define ${identifier} ${value.toString()})
  `
  test("are read and attached to syntax", () => {
    const vals = read(testSrc)
    expect(vals).toStrictEqual(
      expect.arrayContaining([expect.objectContaining({ comment })]),
    )
  })
  const comment2 = ";;; this should appear in the comment!"
  const testSrc2 = `
  ${comment2}
  (define ${identifier} ${value.toString()})
  `
  test("triple semicolon comments are saved in the comment", () => {
    const vals = read(testSrc2)
    expect(vals).toStrictEqual(
      expect.arrayContaining([expect.objectContaining({ comment: comment2 })]),
    )
  })
  const comment3 =
    "; this should appear in the comment!;;; this should appear in the comment!"
  const testSrc3 = `
  ${comment}
  ${comment2}
  (define ${identifier} ${value.toString()})
  `
  test("multiple line comments are saved as one comment", () => {
    const vals = read(testSrc3)
    expect(vals).toStrictEqual(
      expect.arrayContaining([expect.objectContaining({ comment: comment3 })]),
    )
  })
})
