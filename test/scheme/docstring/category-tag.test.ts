import { describe, expect, test } from 'vitest'
import { Comment } from '../../../src/scheme/ast'
import { CategoryTag, isCategoryTag } from '../../../src/scheme/docstring/tags/category-tag'
import { DocTag, parseAllTags } from '../../../src/scheme/docstring/tags'
import { anyRange } from '../util'

function convertLinesToComments(lines: string[]): Comment[] {
  return lines.map((line): Comment => ({ line, range: anyRange }))
}

describe('@category tag', () => {
  test('parses a single category', () => {
    const tags: DocTag[] = []
    parseAllTags(convertLinesToComments(['@category math']), tags)

    const expected: CategoryTag = {
      tag: '@category',
      contents: ['math'],
      range: anyRange,
    }
    expect(tags).toStrictEqual([expected])
  })

  test('parses and trims a comma-separated list of categories', () => {
    const tags: DocTag[] = []
    parseAllTags(
      convertLinesToComments(['@category math, comparator,  predicates']),
      tags,
    )

    const expected: CategoryTag = {
      tag: '@category',
      contents: ['math', 'comparator', 'predicates'],
      range: anyRange,
    }
    expect(tags).toStrictEqual([expected])
  })

  test('throws when no categories are given', () => {
    const tags: DocTag[] = []
    expect(() => {
      parseAllTags(convertLinesToComments(['@category']), tags)
    }).toThrow('@category')
  })

  test('throws when only empty/whitespace categories are given', () => {
    const tags: DocTag[] = []
    expect(() => {
      parseAllTags(convertLinesToComments(['@category  ,  ,']), tags)
    }).toThrow('@category')
  })

  describe('isCategoryTag', () => {
    test('recognizes a category tag', () => {
      const tags: DocTag[] = []
      parseAllTags(convertLinesToComments(['@category math']), tags)
      expect(isCategoryTag(tags[0])).toBe(true)
    })

    test('rejects a non-category tag', () => {
      const other: DocTag = { tag: '@other', contents: 'not a category', range: anyRange }
      expect(isCategoryTag(other)).toBe(false)
    })
  })
})
