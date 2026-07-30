import { mkDocError } from '../error'
import { DocTag, registerDocTagParser } from './tag'

export type CategoryTag = DocTag<string[]>

export function isCategoryTag(t: DocTag): t is CategoryTag {
  return Array.isArray(t.contents) && t.contents.every((c) => typeof c === 'string')
}

registerDocTagParser('@category', (contents, range): CategoryTag => {
  const categories = contents
    .split(',')
    .map((c) => c.trim())
    .filter((c) => c.length > 0)
  if (categories.length === 0) {
    throw mkDocError('Error in @category tag: expected at least one category',
      range,
    )
  }

  return {
    tag: '@category',
    contents: categories,
    range,
  }
})
