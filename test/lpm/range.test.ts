import { describe, expect, test } from 'vitest'
import { Loc, Range, rangesEqual } from '../../src/lpm'
import { runProgram } from '../harness.js'

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

// End-to-end coverage that runtime and parse errors carry the correct *source*
// range (line:col) all the way through compile -> run -> ScamperError.toString.
//
// The library test suites (test/libs) deliberately strip ranges from their
// error assertions: a contract error points at the offending definition's line
// in the .scm library source, so coupling those tests to line numbers made
// unrelated library edits shift the ranges and break the tests (see
// test/libs/harness.ts). These tests therefore own the range-*reporting*
// coverage instead, using only errors whose range falls inside the user
// program -- arity mismatches (reported at the call site), `(error ...)` calls,
// and parse errors -- so they stay stable no matter how the libraries change.
describe('error range reporting (end-to-end)', () => {
  test('an arity mismatch points at the offending call site', async () => {
    expect(await runProgram('(define f (lambda (x) x))\n(f 1 2)')).toEqual([
      'Runtime error [2:1-2:7]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('a builtin arity mismatch points at the call, not the definition', async () => {
    expect(await runProgram('(car 1 2)')).toEqual([
      'Runtime error [1:1-1:9]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('(error ...) reports the range of the error call', async () => {
    expect(await runProgram('(error "boom")')).toEqual([
      'Runtime error [1:1-1:14]: (error) boom',
    ])
  })

  test('the reported line tracks which statement raised in a multi-line program', async () => {
    expect(await runProgram('(+ 1 1)\n(+ 2 2)\n(error "third")')).toEqual([
      '2',
      '4',
      'Runtime error [3:1-3:15]: (error) third',
    ])
  })

  test('the reported column tracks a nested sub-expression', async () => {
    expect(await runProgram('(+ 1 (error "inner"))')).toEqual([
      'Runtime error [1:6-1:20]: (error) inner',
    ])
  })

  test('a parse error reports its source range', async () => {
    expect(await runProgram('(+ 1 2')).toEqual([
      'Parser error [1:1-1:6]: Malformed function application.',
    ])
  })
})
