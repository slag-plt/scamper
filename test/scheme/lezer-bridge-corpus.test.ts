import { readFileSync } from 'fs'
import { resolve } from 'path'
import { describe, test } from 'vitest'
import { expectParses } from './lezer-bridge-test-utils'

describe('lezer-bridge parsing of realistic multi-statement programs', () => {
  test("the repo's own test.scm fixture", () => {
    const src = readFileSync(resolve(__dirname, '../../test.scm'), 'utf-8')
    expectParses(src)
  })

  test('recursive factorial with a docstring, comments, and a helper', () => {
    expectParses(
      [
        '; a simple recursive program',
        ';;; (fact n) -> integer?',
        ';;;  n : integer?',
        ';;; Computes n factorial.',
        '(define fact',
        '  (lambda (n)',
        '    (if (<= n 0)',
        '        1',
        '        (* n (fact (- n 1))))))',
        '',
        '; not documented',
        '(define fact5 (fact 5))',
        '(display fact5)',
      ].join('\n'),
    )
  })

  test('structs with pattern matching over a small list-processing program', () => {
    expectParses(
      [
        '(struct point (x y))',
        '(define origin (point 0 0))',
        '(define dist',
        '  (lambda (p)',
        '    (match p',
        '      [(point 0 0) 0]',
        '      [(point x y) (+ (* x x) (* y y))])))',
        '(define pts (list origin (point 1 2) (point 3 4)))',
        '(define sum-dists',
        '  (lambda (ps)',
        '    (if (null? ps)',
        '        0',
        '        (+ (dist (car ps)) (sum-dists (cdr ps))))))',
        '(display (sum-dists pts))',
      ].join('\n'),
    )
  })

  test('nested let/let*/cond/and/or/section combined', () => {
    expectParses(
      [
        '(define classify',
        '  (lambda (n)',
        '    (let* ([abs-n (if (< n 0) (- 0 n) n)]',
        '           [even? (= 0 (modulo abs-n 2))])',
        '      (cond [(and (= n 0) (or #t #f)) "zero"]',
        '            [even? "even"]',
        '            [#t "odd"]))))',
        '(display (map classify (list -4 -3 0 1 2)))',
        '(display (map (section classify _) (list 5 6 7)))',
      ].join('\n'),
    )
  })

  test('quoted data and vector literals mixed into a larger program', () => {
    expectParses(
      [
        "(define shopping-list '(eggs milk bread))",
        '(define matrix [[1 2] [3 4]])',
        "(define tagged (list 'point [1 2] \"label\" #\\!))",
        '(display shopping-list)',
        '(display matrix)',
        '(display tagged)',
      ].join('\n'),
    )
  })

  test('import followed by multiple defines and a struct with pattern matching', () => {
    expectParses(
      [
        '(import lists)',
        '(import lists)',
        '(struct tree (value left right))',
        '(define leaf (lambda (v) (tree v null null)))',
        '(define sum-tree',
        '  (lambda (t)',
        '    (match t',
        '      [null 0]',
        '      [(tree v l r) (+ v (+ (sum-tree l) (sum-tree r)))])))',
        '(define t (tree 1 (leaf 2) (tree 3 (leaf 4) null)))',
        '(display (sum-tree t))',
      ].join('\n'),
    )
  })
})
