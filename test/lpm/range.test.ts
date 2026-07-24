import { describe, expect, test } from 'vitest'
import { Loc, Range, rangesEqual } from '../../src/lpm'

describe('Range', () => {
  describe('firstLineEnd', () => {
    test('returns end when range is on a single line', () => {
      const range = Range.of(1, 2, 1, 1, 5, 4)
      const src = 'xabcd'
      expect(range.firstLineEnd(src)).toEqual(new Loc(1, 5, 4))
    })

    test('returns last character before newline on first line', () => {
      const src = '(foo\n bar)'
      const range = Range.of(1, 1, 0, 2, 5, 9)
      expect(range.firstLineEnd(src)).toEqual(new Loc(1, 4, 3))
    })
  })

  describe('firstLineSpan', () => {
    test('returns the original range on a single line', () => {
      const range = Range.of(1, 2, 1, 1, 5, 4)
      const src = 'xabcd'
      expect(range.firstLineSpan(src)).toEqual(range)
    })

    test('truncates to the first line of a multi-line range', () => {
      const src = '(foo\n bar)'
      const range = Range.of(1, 1, 0, 2, 5, 9)
      expect(range.firstLineSpan(src)).toEqual(
        Range.of(1, 1, 0, 1, 4, 3),
      )
    })
  })

  describe('union', () => {
    test('spans the earliest begin and latest end, regardless of argument order', () => {
      const a = Range.of(1, 1, 0, 1, 3, 2) // idx 0..2
      const b = Range.of(2, 1, 10, 2, 5, 14) // idx 10..14
      const c = Range.of(1, 5, 4, 1, 8, 7) // idx 4..7
      const expected = Range.of(1, 1, 0, 2, 5, 14)
      expect(Range.union(a, b, c)).toEqual(expected)
      expect(Range.union(c, a, b)).toEqual(expected)
      expect(Range.union(b, c, a)).toEqual(expected)
    })

    test('reuses the actual begin/end Locs so line/col stay accurate', () => {
      const a = Range.of(1, 1, 0, 1, 3, 2)
      const b = Range.of(2, 1, 10, 2, 5, 14)
      const u = Range.union(a, b)
      expect(u.begin).toBe(a.begin)
      expect(u.end).toBe(b.end)
    })

    test('a single range unions to an equal range', () => {
      const a = Range.of(3, 2, 20, 3, 6, 24)
      expect(Range.union(a)).toEqual(a)
    })

    test('no ranges unions to Range.none', () => {
      expect(Range.union()).toBe(Range.none)
    })
  })
})

describe('rangesEqual', () => {
  test('returns true for identical ranges', () => {
    const range = Range.of(1, 1, 0, 1, 3, 2)
    expect(rangesEqual(range, range)).toBe(true)
  })

  test('returns false for different ranges', () => {
    const a = Range.of(1, 1, 0, 1, 3, 2)
    const b = Range.of(1, 1, 0, 1, 4, 3)
    expect(rangesEqual(a, b)).toBe(false)
  })
})
