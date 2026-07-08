import { describe, expect, test } from "vitest"
import { Loc, Range, rangesEqual } from "../../src/lpm"

describe("Range", () => {
  describe("firstLineEnd", () => {
    test("returns end when range is on a single line", () => {
      const range = Range.of(1, 2, 1, 1, 5, 4)
      const src = "xabcd"
      expect(range.firstLineEnd(src)).toEqual(new Loc(1, 5, 4))
    })

    test("returns last character before newline on first line", () => {
      const src = "(foo\n bar)"
      const range = Range.of(1, 1, 0, 2, 5, 9)
      expect(range.firstLineEnd(src)).toEqual(new Loc(1, 4, 3))
    })
  })

  describe("firstLineSpan", () => {
    test("returns the original range on a single line", () => {
      const range = Range.of(1, 2, 1, 1, 5, 4)
      const src = "xabcd"
      expect(range.firstLineSpan(src)).toEqual(range)
    })

    test("truncates to the first line of a multi-line range", () => {
      const src = "(foo\n bar)"
      const range = Range.of(1, 1, 0, 2, 5, 9)
      expect(range.firstLineSpan(src)).toEqual(
        Range.of(1, 1, 0, 1, 4, 3),
      )
    })
  })
})

describe("rangesEqual", () => {
  test("returns true for identical ranges", () => {
    const range = Range.of(1, 1, 0, 1, 3, 2)
    expect(rangesEqual(range, range)).toBe(true)
  })

  test("returns false for different ranges", () => {
    const a = Range.of(1, 1, 0, 1, 3, 2)
    const b = Range.of(1, 1, 0, 1, 4, 3)
    expect(rangesEqual(a, b)).toBe(false)
  })
})
