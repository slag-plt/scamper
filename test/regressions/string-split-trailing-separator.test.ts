import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/437
//
// `string-split` was JavaScript's `String.prototype.split` verbatim, so a
// separator at either end of the string contributed an empty string to the
// result: (string-split "snicker snack" "ck") gave (list "sni" "er sna" "").
// Racket's `string-split` trims one occurrence of the separator from each end
// before splitting, and answers the empty list when nothing is left, so the
// separator that ends a string no longer shows up as an empty piece.

test('string-split ignores a separator at either end (#437)', async () => {
  expect(
    await runProgram(`
  (string-split "snicker snack" "ck")
  (string-split ",a,b" ",")
  (string-split "ck" "ck")
  (string-split "a,b,c" ",")
  (string-split "a,,b" ",")
  (string-split "aaa" "aa")
  `),
  ).toEqual([
    '(list "sni" "er sna")', // trailing separator dropped
    '(list "a" "b")', // leading separator dropped
    'null', // nothing but a separator: the empty list
    '(list "a" "b" "c")', // unchanged
    '(list "a" "" "b")', // an interior empty piece is still a piece
    'null', // a separator overlapping itself is not trimmed twice
  ])
})

test('string-split-vector ignores a separator at either end (#437)', async () => {
  expect(
    await runProgram(`
  (string-split-vector "snicker snack" "ck")
  (string-split-vector "ck" "ck")
  `),
  ).toEqual(['(vector "sni" "er sna")', '(vector)'])
})
