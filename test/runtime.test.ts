import {expect, test} from '@jest/globals'

import { mkOptions, Scamper } from '../src/scamper'

function runProgram (src: string): string {
  const output = document.createElement('div')
  const opts = mkOptions()
  opts.defaultDisplay = true
  const scamper = new Scamper(output, src, opts)
  scamper.runProgram()
  return output.textContent!
}

export function scamperTest (label: string, src: string, expected: string[]) {
  test(label, () => expect(runProgram(src.trim())).toBe(expected.join('')))
}

////////////////////////////////////////////////////////////////////////////////

scamperTest('and-or-short-circuit', `
(and (error "hello")
     #f)

(and #f
     (error "hello"))

(or (error "hello")
     #t)

(or #t
    (error "hello"))
`, [
  'Runtime error [1:6-1:20]: (error) hello',
  '#f',
  'Runtime error [7:5-7:19]: (error) hello',
  '#t'
])

scamperTest('chained-defs', `
(define x 10)
(define y x)
(define z y)

(+ z 50)

(define f +)
(define g f)
(define h g)
(define i h)

(i x y z)
`, [
  '60',
  '30'
])

scamperTest('closures', `
(define x 10)

(define f1
  (lambda (y) (+ x y)))

(f1 20)

(define f2
  (let ([x 100])
    (lambda (y) (+ x y))))

(f2 20)

(define f3
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (+ x y z)))))

(((f3 11) 3) 7)

(define f4
  (let* ([x 51]
         [f (lambda (y) (+ x y))]
         [g (lambda (x) (+ (f x) 1))])
    g))
  
(f4 100)
`, [
  '30',
  '120',
  '21',
  '152'
])

scamperTest('cond-else-test', `
(import image)

(define factorial
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (factorial (- n 1)))])))

(factorial 5)

(define red-square (rectangle 15 15 "solid" "red"))

(define type-of
  (lambda (datum)
    (cond
      [(number? datum) "number"]
      [(string? datum) "string"]
      [else "some-other-type"])))

(type-of red-square)

`, [
  '120',
  '"some-other-type"'
])

scamperTest('contract-check', `
(string-length (list 1 2 3))

(+ 1 2 3 "bye")

(map char-upcase (list "h" "e" "l" "l" "o"))
`, [
  'Runtime error [1:1-1:28]: (string-length) expected a string, received list',
  'Runtime error [3:1-3:15]: (+) expected a number, received string',
  'Runtime error [5:1-5:44]: (map) expected a character, received string'
])

scamperTest('define-test1', `
(define x 10)

(define f
  (lambda (y) (+ x y)))

(f x)
`, [
  '20'
])

scamperTest('duplicate-binders', `
(lambda (x x y) (+ x x))

(struct foo (z y z))
`, [
  ':8:0: Parser error:',
  'Duplicate name x given in definition.',
  'In program: (x x y)'
])

scamperTest('fact', `
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(fact 0)

(fact 5)
`, [
  '1',
  '120'
])

scamperTest('fizzbuzz', `
(define fizzbuzz
  (lambda (n)
    (cond
      [(and (zero? (modulo n 3)) (zero? (modulo n 5))) "fizzbuzz"]
      [(zero? (modulo n 3)) "fizz"]
      [(zero? (modulo n 5)) "buzz"]
      [#t (number->string n)])))

(fizzbuzz 1)
(fizzbuzz 2)
(fizzbuzz 3)
(fizzbuzz 4)
(fizzbuzz 5)
(fizzbuzz 6)
(fizzbuzz 7)
(fizzbuzz 8)
(fizzbuzz 9)
(fizzbuzz 10)
(fizzbuzz 11)
(fizzbuzz 12)
(fizzbuzz 13)
(fizzbuzz 14)
(fizzbuzz 15)
`, [
  '"1"',
  '"2"',
  '"fizz"',
  '"4"',
  '"buzz"',
  '"fizz"',
  '"7"',
  '"8"',
  '"fizz"',
  '"buzz"',
  '"11"',
  '"fizz"',
  '"13"',
  '"14"',
  '"fizzbuzz"'
])

scamperTest('let-binding-errors', `
; let bindings telescope
(let
  ([x1 1]
   [y1 (+ x1 6)])
  (+ x1 y1))

; let bindings refer to future bindings

(let
  ([x2 y2]
   [y2 5])
  (+ x2 y2))

(let*
  ([x3 y3]
   [y3 5])
  (+ x3 y3))
`, [
  ':10:3: Scope error:',
  'Variable x1 is not defined',
  'In program: x1',
  ':7:9: Scope error:',
  'Variable y2 is not defined',
  'In program: y2',
  ':7:14: Scope error:',
  'Variable y3 is not defined',
  'In program: y3'
])

scamperTest('let-binding', `
; bindings are not dependent on each other
(let
  ([x 1]
   [y 7]
   [z 11])
  (+ x y z))

(let*
  ([x 1]
   [y 7]
   [z 11])
  (+ x y z))

; bindings telescope
(let*
  ([x 1]
   [y (+ x 6)]
   [z (+ y 4)])
  (+ x y z))
`, [
  '19',
  '19',
  '19'
])

scamperTest('list-length', `
(define list-length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (list-length (cdr l))))))

(list-length null)

(list-length (cons 9 null))

(list-length (cons 9 (cons 9 (cons 9 (cons 9 (cons 9 null))))))

(list-length (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" null))))))
`, [
  '0',
  '1',
  '5',
  '5'
])

scamperTest('match-lists', `
(define list-length
  (lambda (l)
    (match l
      [null 0]
      [(cons _ tail) (+ 1 (list-length tail))])))

(list-length (list 0 0 0 0 0))

(list-length null)

(list-length (list 0 0 0 0 0 0 0 0 0 0))

(define list-append
  (lambda (l1 l2)
    (match l1
      [null l2]
      [(cons head tail) (cons head (list-append tail l2))])))

(list-append (list 1 2 3) (list 4 5 6))

(define intersperse
  (lambda (x l)
    (match l
      [null null]
      [(cons _ null) l]
      [(cons x1 (cons x2 tail)) (cons x1 (cons x (intersperse x (cons x2 tail))))])))

(intersperse "," (list "a" "b" "c"))
`, [
  '5',
  '0',
  '10',
  '(list 1 2 3 4 5 6)',
  '(list "a" "," "b" "," "c")'
])

scamperTest('match-lit', `
(match 5
  [1 "fail"]
  [5 "numbers"])

(match "baz"
  ["foo" "fail"]
  ["bar" "fail"]
  ["baz" "strings"]
  ["boop" "fail"]) 

(match #\\q
  [#\\a "fail"]
  [#\\q "chars"]
  [#\\z "fail"])

(match #t
  [#f "fail"]
  [#t "bools"])

(match null
  [null "null"])

(match (list "lists" "a" "b")
  [null "fail"]
  [(cons head _) head])
`, [
  '"numbers"',
  '"strings"',
  '"chars"',
  '"bools"',
  '"null"',
  '"lists"'
])

scamperTest('match-repeated-bindings', `
(match (list 1 2 3)
  [null "fail"]
  [(cons x x) "fail"])
`, [
  ':3:2: Scope error:',
  'Variable x is repeated in the pattern',
  'In program: (match (list 1 2 3)',
  '[null "fail"]',
  '[(cons x x) "fail"])'
])

scamperTest('match-struct', `
(struct leaf (value))

(struct node (left right))

(define tree-count
  (lambda (t)
    (match t
      [(leaf _) 1]
      [(node l r) (+ (tree-count l) (tree-count r))])))

(tree-count (leaf "a"))

(tree-count
  (node (leaf "a")
        (node (leaf "b")
              (node (leaf "c")
                    (leaf "d")))))
`, [
  '1',
  '4'
])

scamperTest('mixed-brackets', `
{- [* 3
     (+ {* 1
           [ / 5 8]}
         12)]
   (- 5 1)}
`, [
  '33.875'
])

scamperTest('numbers', `
4129
0
-48902
+48902
00142
-089
+098

3.14
.14
0.14
314.

-3.14
-.14
-0.14
-314.

+3.14
+.14
+0.14
+314.

3e2
3.0e2
.3e2
3E2
3.0E2
.3E2

-3e2
-.3e2
-3e-2
-.3e-2
`, [
  '4129',
  '0',
  '-48902',
  '48902',
  '142',
  '-89',
  '98',
  '3.14',
  '0.14',
  '0.14',
  '314',
  '-3.14',
  '-0.14',
  '-0.14',
  '-314',
  '3.14',
  '0.14',
  '0.14',
  '314',
  '300',
  '300',
  '30',
  '300',
  '300',
  '30',
  '-300',
  '-30',
  '-0.03',
  '-0.003'
])

scamperTest('shadowing', `
(define x 3)

(define y (+ x 2))

(define x -5)

(+ x y)

(define f
  (lambda (x)
    (* x 2)))

(f 3)

(let*
  ([z 10]
   [x (+ z x)]
   [z 100])
  (+ x z))

x
`, [
  '0',
  '6',
  '105',
  '-5'
])

scamperTest('simple-exp', `
(let ((x 1) (y (+ 1 1))) (+ (- 1 1) y (* x 5 8) x))

(+ (car (cdr (cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 null)))))))) 100)
`, [
  '43',
  '103'
])

scamperTest('tree-test', `
(struct leaf (value))

(struct node (left right))

(define tree-size
  (lambda (t)
    (if (leaf? t)
        1
        (+ (tree-size (node-left t))
           (tree-size (node-right t))))))

(define tree-to-list
  (lambda (t)
    (if (leaf? t)
        (list (leaf-value t))
        (append (tree-to-list (node-left t))
                (tree-to-list (node-right t))))))

(define t1
  (node (leaf "a")
        (node (leaf "b")
              (leaf "c"))))

t1

(leaf-value (node-left (node-right t1)))

(tree-size t1)

(tree-to-list t1)
`, [
  '(node (leaf "a") (node (leaf "b") (leaf "c")))',
  '"b"',
  '3',
  '(list "a" "b" "c")'
])

scamperTest('undefined-variable', `
(+ x 1)
`, [
  'Runtime error [1:4-1:4]: Referenced unbound identifier "x".'
])