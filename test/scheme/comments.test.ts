import { describe, expect, test } from "vitest"
import { read, stringToTokens } from "../../src/scheme/reader"
import { mkComment } from "../../src/lpm"
import { tokenizeAndParse } from "../../src/scheme"
import { SimpleErrorChannel } from "../../src/lpm/output/simple-error"

describe("Comments in LPM Prog", () => {
  test("comments are tokenized", () => {
    const mockCommentText1 = "; This is a comment"
    const mockCommentText2 = "; This is another comment"
    const mockCommentText3 = "; Display the value of x"
    const src = `
      ${mockCommentText1}
      (define x 10) ${mockCommentText2}
      (display x) ${mockCommentText3}
    `
    const tokens = stringToTokens(src)
    expect(tokens).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ text: mockCommentText1 }),
        expect.objectContaining({ text: mockCommentText2 }),
        expect.objectContaining({ text: mockCommentText3 }),
      ]),
    )
  })

  test("comments are read as values", () => {
    const mockCommentText = "This is a comment"
    const src = `
      ;${mockCommentText}
      (define x 10)
    `
    const sexps = read(src)
    expect(sexps).toEqual(
      expect.arrayContaining([
        expect.objectContaining({ value: mkComment(mockCommentText) }),
      ]),
    )
  })

  test("comments are in LPM prog", () => {
    const mockCommentText = "This is a comment"
    const src = `
      ;${mockCommentText}
      (define x 10)
    `
    const err = new SimpleErrorChannel()
    const prog = tokenizeAndParse(err, src)
    if (!prog) {
      expect.fail()
    }
    expect(prog).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          expr: expect.objectContaining({
            value: mkComment(mockCommentText),
          }) as unknown,
        }),
      ]),
    )
  })
})
