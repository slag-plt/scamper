import { describe, expect, test } from "vitest"
import { Comment, read } from "../../src/scheme/reader"
import { anyRange } from "./util"

const identifier = "x"
const value = 1

describe("Comments", () => {
  const comment1 = "; this should appear in the comment!"
  const testSrc = `
  ${comment1}
  (define ${identifier} ${value.toString()})
  `
  const expectedComment: Comment = {
    line: comment1,
    range: anyRange,
  }
  test("are read and attached to syntax", () => {
    const vals = read(testSrc)
    expect(vals).toStrictEqual(
      expect.arrayContaining([
        expect.objectContaining({
          comments: [expectedComment],
        }),
      ]),
    )
  })
  const comment2 = ";;; this should appear in the comment!"
  const testSrc2 = `
  ${comment2}
  (define ${identifier} ${value.toString()})
  `
  const expectedComment2: Comment = {
    line: comment2,
    range: anyRange,
  }
  test("triple semicolon comments are saved in the comment", () => {
    const vals = read(testSrc2)
    expect(vals).toStrictEqual(
      expect.arrayContaining([
        expect.objectContaining({
          comments: [expectedComment2],
        }),
      ]),
    )
  })
  const testSrc3 = `
  ${comment1}
  ${comment2}
  (define ${identifier} ${value.toString()})
  `
  const expectedComments: Comment[] = [expectedComment, expectedComment2]
  test("multiple line comments are saved as one comment", () => {
    const vals = read(testSrc3)
    expect(vals).toStrictEqual(
      expect.arrayContaining([
        expect.objectContaining({
          comments: expectedComments,
        }),
      ]),
    )
  })
})
