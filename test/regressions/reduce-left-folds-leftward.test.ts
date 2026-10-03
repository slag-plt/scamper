import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/712
//
// `reduce-left` was added in 4.7.0 (#664, #698) as `fold-left` seeded with the
// list's first element, which made its combiner element-first: SRFI-1's
// `reduce`, under a name SRFI-1 does not define. So `(reduce-left list (list 1
// 2 3 4 5))` answered `(list 5 (list 4 (list 3 (list 2 1))))` -- a right-
// leaning nest from a procedure whose name says it folds leftward -- and
// `reduce-left` disagreed with `reduce` on the same arguments.
//
// The name is MIT/GNU Scheme's, and MIT's `reduce-left` is accumulator-first
// and left-associative:
//
//   https://web.mit.edu/scheme_v9.2/doc/mit-scheme-ref/Reduction-of-Lists.html
//   "the arguments are reduced in a left-associative fashion. For example:
//    (reduce-left list '() '(1 2 3 4))       => (((1 2) 3) 4)"
//
// so `reduce-left` is now `reduce`'s combiner order with the direction pinned
// in the name, which is what issue #664 asked for: an explicit left reduction
// to lean on while `reduce` stays free to change.
//
// N.B. this test pins a DELIBERATE REVERSAL of what #698 pinned. The old order
// was asserted in test/libs/prelude.test.ts ('reduce-left') and tabulated in
// docs/DIFFERENCES.md ("Folds: a warning"); both moved with it. `fold-left` is
// untouched -- it keeps its element-first order, so `reduce-left` is no longer
// `fold-left` seeded with `(car l)`.

test('reduce-left nests leftward, as reduce does (#712)', async () => {
  expect(
    await runProgram(`
(reduce-left list (list 1 2 3 4 5))
(reduce list (list 1 2 3 4 5))
(equal? (reduce-left list (list 1 2 3 4 5)) (reduce list (list 1 2 3 4 5)))
`),
  ).toEqual([
    '(list (list (list (list 1 2) 3) 4) 5)',
    '(list (list (list (list 1 2) 3) 4) 5)',
    '#t',
  ])
})

test('reduce-left hands the combiner the accumulator first (#712)', async () => {
  expect(
    await runProgram(`
(reduce-left - (list 1 2 3))
(reduce-left (lambda (a b) a) (list 1 2 3))
(equal? (reduce-left - (list 10 3 2)) (fold - 10 (list 3 2)))
`),
  ).toEqual([
    // (- (- 1 2) 3) = -4, not the element-first (- 3 (- 2 1)) = 2
    '-4',
    // keeping the first argument keeps the accumulator, so the seed survives;
    // element-first it would keep each element and answer the last one, 3
    '1',
    // reduce-left f l is now fold f (car l) (cdr l)
    '#t',
  ])
})

test('reduce-left still agrees with reduce on the easy cases (#712)', async () => {
  expect(
    await runProgram(`
(reduce-left + (list 1 2 3 4 5))
(reduce-left + (list 42))
(reduce-left max (list 3 1 4 1 5 9 2 6))
(reduce-left + (list))
`, { stripRanges: true }),
  ).toEqual([
    '15',
    // a singleton list accumulates to its only element
    '42',
    '9',
    // the empty list has no first element to start from
    'Runtime error: (reduce-left) car: expected a pair or a non-empty list',
  ])
})
