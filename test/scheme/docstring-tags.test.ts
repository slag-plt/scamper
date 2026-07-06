import "./docstring/test-tags"

import { describe, expect, test } from "vitest"
import { Comment } from "../../src/scheme/reader"
import {
  DocTag,
  matchesDocTagFormat,
  parseAllTags,
} from "../../src/scheme/docstring/tags"
import { anyRange } from "./util"
import {
  testTag1,
  testTag1Contents,
  testTagLine1,
  testTag2,
  testTag2Contents,
  testTagLine2,
} from "./docstring/test-tags"

describe("docstring tags", () => {
  describe("parseAllTags", () => {
    test("extracts all tags", () => {
      const testDocLines = convertLinesToComments([testTagLine1, testTagLine2])
      const tags: DocTag[] = []
      parseAllTags(testDocLines, tags)

      const expectedTag1: DocTag = {
        tag: testTag1,
        contents: testTag1Contents,
        range: anyRange,
      }
      const expectedTag2: DocTag = {
        tag: testTag2,
        contents: testTag2Contents,
        range: anyRange,
      }
      expect(tags).toStrictEqual([expectedTag1, expectedTag2])
      expect(testDocLines).toStrictEqual([])
    })
    test("throws when we have a non-tag in the tag section", () => {
      const testDocLines = [
        testTagLine1,
        "@ bad bad bad! even though we start with @! bad!",
        testTagLine2,
      ]
      const tags: DocTag[] = []
      expect(() => {
        parseAllTags(convertLinesToComments(testDocLines), tags)
      }).toThrow("non-tag")
    })
  })

  describe("matchesDocTagFormat", () => {
    test("good doc tag line", () => {
      const testLine = "@tag tag1 tag2 tag3"
      expect(matchesDocTagFormat(testLine)).toBe(true)
    })
    test("not a doc tag line", () => {
      const testLine =
        "@ this is definitely NOT a tagged line even though it starts with @"
      expect(matchesDocTagFormat(testLine)).toBe(false)
    })
  })
})

function convertLinesToComments(lines: string[]): Comment[] {
  return lines.map((line): Comment => ({ line, range: anyRange }))
}
