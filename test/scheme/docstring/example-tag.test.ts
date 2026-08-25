import { describe, expect, test } from 'vitest'
import { Comment, expToString } from '../../../src/scheme/ast'
import {
  ExampleTag,
  isExampleTag,
} from '../../../src/scheme/docstring/tags/example-tag'
import { DocTag, parseAllTags } from '../../../src/scheme/docstring/tags'
import { anyRange } from '../util'

function convertLinesToComments(lines: string[]): Comment[] {
  return lines.map((line): Comment => ({ line, range: anyRange }))
}

function parseOne(line: string): DocTag {
  const tags: DocTag[] = []
  parseAllTags(convertLinesToComments([line]), tags)
  return tags[0]
}

function parseExample(line: string): ExampleTag {
  const tag = parseOne(line)
  if (!isExampleTag(tag)) throw new Error('not an @example tag')
  return tag
}

describe('@example tag', () => {
  test('parses a call and a literal result', () => {
    const tag = parseExample('@example (fact 5) -> 120')
    expect(expToString(tag.contents.functionCall)).toBe('(fact 5)')
    expect(expToString(tag.contents.result)).toBe('120')
  })

  // The result used to have to be a literal, so this line was a Docstring
  // warning. A list is what a student's first recursive function returns, and
  // an example that cannot say so is not much of an example, so the expected
  // side is now any expression -- evaluated beside the call rather than read
  // off the page (see src/scheme/examples.ts).
  test('parses a result that is not a literal', () => {
    const tag = parseExample('@example (upto 3) -> (list 1 2 3)')
    expect(expToString(tag.contents.result)).toBe('(list 1 2 3)')
  })

  test('keeps a "->" inside the result', () => {
    const tag = parseExample('@example (arrow) -> (string-append "-" "> ")')
    expect(expToString(tag.contents.result)).toBe('(string-append "-" "> ")')
  })

  test('throws without the separator', () => {
    expect(() => parseOne('@example (fact 5) 120')).toThrow(
      /missing separator/,
    )
  })

  test('throws when the call is not an application', () => {
    expect(() => parseOne('@example fact -> 120')).toThrow(
      /application expression/,
    )
  })

  test('throws when a side holds more than one expression', () => {
    expect(() => parseOne('@example (fact 5) -> 120 121')).toThrow(
      /more than one expression/,
    )
  })

  test('throws when a side is malformed', () => {
    expect(() => parseOne('@example (fact 5 -> 120')).toThrow(/@example/)
  })

  describe('isExampleTag', () => {
    test('recognizes an example tag', () => {
      expect(isExampleTag(parseOne('@example (fact 5) -> 120'))).toBe(true)
    })

    test('rejects a non-example tag', () => {
      expect(isExampleTag(parseOne('@category math'))).toBe(false)
    })
  })
})
