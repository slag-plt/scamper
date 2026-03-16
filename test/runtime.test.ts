import { expect, test } from 'vitest'
import { runProgram } from './harness.js'

////////////////////////////////////////////////////////////////////////////////

test('and-or-short-circuit', () => {
  expect(runProgram(`
(and (error "hello")
     #f)

(and #f
     (error "hello"))

(or (error "hello")
     #t)

(or #t
    (error "hello"))
`)).toEqual([
  'Runtime error [1:6-1:20]: (error) hello',
  '#f',
  'Runtime error [7:5-7:19]: (error) hello',
  '#t'
])
})

test('chained-defs', () => {
  expect(runProgram(`
(define x 10)
(define y x)
(define z y)

(+ z 50)

(define f +)
(define g f)
(define h g)
(define i h)

(i x y z)
`)).toEqual([
  '60',
  '30'
])
})

test('closures', () => {
  expect(runProgram(`
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
`)).toEqual([
  '30',
  '120',
  '21',
  '152'
])
})

test('cond-else-test', () => {
  expect(runProgram(`
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

`)).toEqual([
  '120',
  '"some-other-type"'
])
})

test('contract-check', () => {
  expect(runProgram(`
(string-length (list 1 2 3))

(+ 1 2 3 "bye")

(map char-upcase (list "h" "e" "l" "l" "o"))
`)).toEqual([
  'Runtime error [1:1-1:28]: (string-length) expected a string, received list',
  'Runtime error [3:1-3:15]: (+) expected a number, received string',
  'Runtime error [5:1-5:44]: (map) expected a character, received string'
])
})

test('define-test1', () => {
  expect(runProgram(`
(define x 10)

(define f
  (lambda (y) (+ x y)))

(f x)
`)).toEqual([
  '20'
])
})

test.fails('duplicate-binders', () => {
  expect(runProgram(`
(lambda (x x y) (+ x x))

(struct foo (z y z))
`)).toEqual([
  ':8:0: Parser error:',
  'Duplicate name x given in definition.',
  'In program: (x x y)'
])
})

test('fact', () => {
  expect(runProgram(`
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(fact 0)

(fact 5)
`)).toEqual([
  '1',
  '120'
])
})

test('fizzbuzz', () => {
  expect(runProgram(`
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
`)).toEqual([
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
})

test('let-binding-errors', () => {
  expect(runProgram(`
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
`)).toEqual([
  "Parser error [4:11-4:12]: Undefined variable 'x1'",
  "Parser error [10:8-10:9]: Undefined variable 'y2'",
  "Parser error [15:8-15:9]: Undefined variable 'y3'"
])
})

test('let-binding', () => {
  expect(runProgram(`
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
`)).toEqual([
  '19',
  '19',
  '19'
])
})

test('list-length', () => {
  expect(runProgram(`
(define list-length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (list-length (cdr l))))))

(list-length null)

(list-length (cons 9 null))

(list-length (cons 9 (cons 9 (cons 9 (cons 9 (cons 9 null))))))

(list-length (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" null))))))
`)).toEqual([
  '0',
  '1',
  '5',
  '5'
])
})

test('match-lists', () => {
  expect(runProgram(`
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
`)).toEqual([
  '5',
  '0',
  '10',
  '(list 1 2 3 4 5 6)',
  '(list "a" "," "b" "," "c")'
])
})

test('match-lit', () => {
  expect(runProgram(`
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
`)).toEqual([
  '"numbers"',
  '"strings"',
  '"chars"',
  '"bools"',
  '"null"',
  '"lists"'
])
})

test.fails('match-repeated-bindings', () => {
  expect(runProgram(`
(match (list 1 2 3)
  [null "fail"]
  [(cons x x) "fail"])
`)).toEqual([
  ':3:2: Scope error:',
  'Variable x is repeated in the pattern',
  'In program: (match (list 1 2 3)',
  '[null "fail"]',
  '[(cons x x) "fail"])'
])
})

test('match-struct', () => {
  expect(runProgram(`
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
`)).toEqual([
  '1',
  '4'
])
})

test('mixed-brackets', () => {
  expect(runProgram(`
{- {* 3
     (+ {* 1
           { / 5 8}}
         12)}
   (- 5 1)}
`)).toEqual([
  '33.875'
])
})

test('numbers', () => {
  expect(runProgram(`
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
`)).toEqual([
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
})

test.fails('shadowing', () => {
  expect(runProgram(`
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
`)).toEqual([
  '0',
  '6',
  '105',
  '-5'
])
})

test('simple-exp', () => {
  expect(runProgram(`
(let ([x 1] [y (+ 1 1)]) (+ (- 1 1) y (* x 5 8) x))

(+ (car (cdr (cdr (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 null)))))))) 100)
`)).toEqual([
  '43',
  '103'
])
})

test('tree-test', () => {
  expect(runProgram(`
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
`)).toEqual([
  '(node (leaf "a") (node (leaf "b") (leaf "c")))',
  '"b"',
  '3',
  '(list "a" "b" "c")'
])
})

test('undefined-variable', () => {
  expect(runProgram(`
(+ x 1)
`)).toEqual([
  "Parser error [1:4-1:4]: Undefined variable 'x'"
])
})

test('section', () => {
  expect(runProgram(`
((section + _ 1) 1)

(|> (list "a" "b" "c" "d" "e")
    (section map (section string-upcase _) _))

`)).toEqual([
  '2',
  '(list "A" "B" "C" "D" "E")'
])
})
