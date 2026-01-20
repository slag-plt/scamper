import { expect, test } from 'vitest'
import { runProgram } from './harness.js'

////////////////////////////////////////////////////////////////////////////////

test('abs-quotient', () => {
  expect(runProgram(`
(abs 13)
(abs 0.71)
(abs 111)
(abs 4)
(abs 6.1)
(abs -5)
(abs -0.69)
(abs 89)
(abs 0)
(abs -0.10000000000000009)
(quotient 7 2)
(quotient 100 1)
(quotient 2 2)
`)).toEqual([
  '13',
  '0.71',
  '111',
  '4',
  '6.1',
  '5',
  '0.69',
  '89',
  '0',
  '0.10000000000000009',
  '3',
  '100',
  '1',
])
})

test('append-reverse', () => {
  expect(runProgram(`
(append (list "ab" "ba") (list 1 2 3) (list))
(append (list) (list))
(append (list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld")
 (list "Hello" "World"))
(append (list #t #t #t #t) (list #f #f #f #f) (list "a" "a"))
(append (list 10 10 10 10 10) (list #t #t) (list "abab" "abab"))
(reverse (list "ab" "ba"))
(reverse (list))
(reverse (list "Hello" "World"))
(reverse (list "abab" "abab"))
(reverse (list 1 2 3))
`)).toEqual([
  '(list "ab" "ba" 1 2 3)',
  'null',
  '(list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "Hello" "World")',
  '(list #t #t #t #t #f #f #f #f "a" "a")',
  '(list 10 10 10 10 10 #t #t "abab" "abab")',
  '(list "ba" "ab")',
  'null',
  '(list "World" "Hello")',
  '(list "abab" "abab")',
  '(list 3 2 1)'
])
})

test('apply-map', () => {
  expect(runProgram(`
(apply string-length (list "HelloWorld"))
(apply list (list "HelloWorld" "HelloWorld" "HelloWorld"))
(apply string-split (list "HelloWorld" "l"))
(apply + (list 1 2 3 4 5 6 7 8 9 10))
(apply * (list 9 5 10))
(map string-length (list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld"))
(map procedure? (list string-length list + -) )
(map car (list (pair 2 4) (pair "a" "b") (pair "first" "second")))
(map cdr (list (pair 2 4) (pair "a" "b") (pair "first" "second")))
`)).toEqual([
  '10',
  '(list "HelloWorld" "HelloWorld" "HelloWorld")',
  '(list "He" "" "oWor" "d")',
  '55',
  '450',
  '(list 10 10 10 10 10 10 10 10 10 10)',
  '(list #t #t #t #t)',
  '(list 2 "a" "first")',
  '(list 4 "b" "second")'
])
})

test('apply', () => {
  expect(runProgram(`
(apply (lambda (x) (+ x 1)) (list 1))

(apply + (list 3 4))

(apply min (list 3 1 8 9 2 -5 0 3 5 1))
`)).toEqual([
  '2',
  '7',
  '-5'
])
})

test('arithmetic', () => {
  expect(runProgram(`
(+ 1 2 3 4 5)
(- 22 7 2 1 8)
(* 1 2 3 4 5)
(/ 22 2 5 4)
(+ 5)
(- 5)
(* 5)
(/ 5)
`)).toEqual([
  '15',
  '4',
  '120',
  '0.55',
  '5',
  '-5',
  '5',
  '0.2'
])
})

test('asin-acos-atan', () => {
  expect(runProgram(`
(asin 0.4201670368266409)
(asin 0.6518337710215366)
(asin -0.8645514486106083)
(asin -0.7568024953079282)
(asin -0.18216250427209588)
(asin 0.9589242746631385)
(asin -0.6365371822219679)
(asin 0.8600694058124532)
(asin 0)
(acos 0.9074467814501962)
(acos 0.7583618759905082)
(acos -0.5025443191453852)
(acos -0.6536436208636119)
(acos 0.9832684384425845)
(acos 0.28366218546322625)
(acos 0.7712460149971067)
(acos 0.5101770449416689)
(acos 1)
(atan 0.4630211329364896)
(atan 0.8595286652169407)
(atan 1.7203486651303583)
(atan 1.1578212823495777)
(atan -0.18526223068913525)
(atan 3.380515006246586)
(atan -0.8253361052690248)
(atan 1.6858253705060158)
(atan 0)
`)).toEqual([
  '0.43362938564082704',
  '0.71',
  '-1.0442571243572367',
  '-0.8584073464102067',
  '-0.1831853071795868',
  '1.2831853071795865',
  '-0.69',
  '1.0354056994857892',
  '0',
  '0.4336293856408271',
  '0.71',
  '2.0973355292325566',
  '2.2831853071795867',
  '0.18318530717958706',
  '1.2831853071795865',
  '0.69',
  '1.0354056994857892',
  '0',
  '0.43362938564082704',
  '0.71',
  '1.0442571243572367',
  '0.8584073464102068',
  '-0.1831853071795868',
  '1.2831853071795865',
  '-0.69',
  '1.0354056994857894',
  '0'
])
})

test('assoc', () => {
  expect(runProgram(`
(define inventory (list (pair "apples" 5) (pair "bananas" 2) (pair "oranges" 8)))

inventory

(assoc-key? "apples" inventory)

(assoc-key? "grapes" inventory)

(assoc-ref "apples" inventory)

(assoc-ref "bananas" inventory)

(assoc-ref "oranges" inventory)

(define updated-inventory (assoc-set "apples" 3 inventory))

updated-inventory

(assoc-ref "apples" updated-inventory)

(assoc-ref "bananas" updated-inventory)

(assoc-ref "oranges" updated-inventory)

`)).toEqual([
  '(list (pair "apples" 5) (pair "bananas" 2) (pair "oranges" 8))',
  '#t',
  '#f',
  '5',
  '2',
  '8',
  '(list (pair "apples" 3) (pair "bananas" 2) (pair "oranges" 8))',
  '3',
  '2',
  '8'
])
})

test('car-cdr', () => {
  expect(runProgram(`
(car (pair #t #f))
(car (pair 1 2))
(car (pair "hi" "bye"))
(car (pair "a" "b"))
(car (pair 0.003 100))
(cdr (pair 1 2))
(cdr (pair #t #f))
(cdr (pair "hi" "bye"))
(cdr (pair "a" "b"))
(cdr (pair 0.003 100))

`)).toEqual([
  '#t',
  '1',
  '"hi"',
  '"a"',
  '0.003',
  '2',
  '#f',
  '"bye"',
  '"b"',
  '100'
])
})

test('char-comp', () => {
  expect(runProgram(`
(define c1 #\\c)
(define c2 #\\f)

(char=? c1 c1)
(char=? c1 c2)
(char=? c2 c1)
(char=? c2 c2)

(char<? c1 c1)
(char<? c1 c2)
(char<? c2 c1)
(char<? c2 c2)

(char>? c1 c1)
(char>? c1 c2)
(char>? c2 c1)
(char>? c2 c2)

(char<=? c1 c1)
(char<=? c1 c2)
(char<=? c2 c1)
(char<=? c2 c2)

(char>=? c1 c1)
(char>=? c1 c2)
(char>=? c2 c1)
(char>=? c2 c2)

(define c3 #\\A)
(define c4 #\\d)

(char-ci=? c3 c3)
(char-ci=? c3 c4)
(char-ci=? c4 c3)
(char-ci=? c4 c4)

(char-ci<? c3 c3)
(char-ci<? c3 c4)
(char-ci<? c4 c3)
(char-ci<? c4 c4)

(char-ci>? c3 c3)
(char-ci>? c3 c4)
(char-ci>? c4 c3)
(char-ci>? c4 c4)

(char-ci<=? c3 c3)
(char-ci<=? c3 c4)
(char-ci<=? c4 c3)
(char-ci<=? c4 c4)

(char-ci>=? c3 c3)
(char-ci>=? c3 c4)
(char-ci>=? c4 c3)
(char-ci>=? c4 c4)
`)).toEqual([
  '#t',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#t',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t'
])
})

test('char-ops', () => {
  expect(runProgram(`
(digit-value #\\5)
(digit-value #\\0)

(char->integer #\\a)
(integer->char 97)

(char-upcase #\\a)
(char-upcase #\\A)
(char-downcase #\\a)
(char-downcase #\\A)
(char-foldcase #\\a)
(char-foldcase #\\A)
`)).toEqual([
  '5',
  '0',
  '97',
  '#\\a',
  '#\\A',
  '#\\A',
  '#\\a',
  '#\\a',
  '#\\a',
  '#\\a'
])
})

test('char-pred', () => {
  expect(runProgram(`
(char-alphabetic? #\\c)
(char-alphabetic? #\\5)
(char-alphabetic? #\\space)

(char-numeric? #\\c)
(char-numeric? #\\5)
(char-numeric? #\\space)

(char-whitespace? #\\c)
(char-whitespace? #\\5)
(char-whitespace? #\\space)

(char-upper-case? #\\c)
(char-upper-case? #\\F)
(char-upper-case? #\\newline)

(char-lower-case? #\\c)
(char-lower-case? #\\F)
(char-lower-case? #\\newline)
`)).toEqual([
  '#t',
  '#f',
  '#f',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#f'
])
})

test('compose', () => {
  expect(runProgram(`
(define inc
  (lambda (x) (+ x 1)))

(inc 1)

((compose inc) 1)

((compose inc inc inc inc inc) 1)

(|> 1 inc)

(|> 1 inc inc inc inc inc)

(|> "hello"
    string->list
    (lambda (l) (filter (lambda (c) (not (char=? c #\\l))) l))
    list->string)

((compose length
          (lambda (l) (filter (lambda (n) (even? n)) l)))
 (range 10))

(define string-reverse (o list->string reverse string->list))

(string-reverse "hello")
`)).toEqual([
  '2',
  '2',
  '6',
  '2',
  '6',
  '"heo"',
  '5',
  '"olleh"'
])
})

test('cons-pair', () => {
  expect(runProgram(`
(cons #t #f)
(cons 1 2)
(cons "hi" "bye")
(cons "a" "b")
(cons 0.003 100)
(pair 1 2)
(pair #t #f)
(pair "hi" "bye")
(pair "a" "b")
(pair 0.003 100)
`)).toEqual([
  'Runtime error [1:1-1:12]: (cons) The second argument to cons should be a list',
  'Runtime error [2:1-2:10]: (cons) The second argument to cons should be a list',
  'Runtime error [3:1-3:17]: (cons) The second argument to cons should be a list',
  'Runtime error [4:1-4:14]: (cons) The second argument to cons should be a list',
  'Runtime error [5:1-5:16]: (cons) The second argument to cons should be a list',
  '(pair 1 2)',
  '(pair #t #f)',
  '(pair "hi" "bye")',
  '(pair "a" "b")',
  '(pair 0.003 100)'
])
})

test('equal', () => {
  expect(runProgram(`
(equal? 4 4)
(equal? 10 20)
(equal? "Hello" "Hello")
(equal? "Hello" "HELLO")
(equal? 4 "4")
(equal? 4 4.0)
`)).toEqual([
  '#t',
  '#f',
  '#t',
  '#f',
  '#f',
  '#t'
])
})

test('error-qq', () => {
  expect(runProgram(`
(error "existing")
(+ 5 (??))
`)).toEqual([
  'Runtime error [1:1-1:18]: (error) existing',
  'Runtime error [2:6-2:9]: (??) Hole encountered in program!'
])
})

test('exp-log', () => {
  expect(runProgram(`
(exp 13)
(exp 0.71)
(exp 111)
(exp 4)
(exp 6.1)
(exp -5)
(exp -0.69)
(exp 89)
(exp 0)
(exp -0.10000000000000009)
(log 13)
(log 0.71)
(log 111)
(log 4)
(log 6.1)
`)).toEqual([
  '442413.3920089205',
  '2.0339912586467506',
  '1.609487066961518e+48',
  '54.598150033144236',
  '445.85777008251677',
  '0.006737946999085467',
  '0.5015760690660556',
  '4.489612819174345e+38',
  '1',
  '0.9048374180359595',
  '2.5649493574615367',
  '-0.342490308946776',
  '4.709530201312334',
  '1.3862943611198906',
  '1.8082887711792655'
])
})

test('expt', () => {
  expect(runProgram(`
(expt 7 2)
(expt 0.2 0.5)
(expt 100 1)
(expt 2.0 2)
(expt 3.0 3.1)
`)).toEqual([
  '49',
  '0.4472135954999579',
  '100',
  '4',
  '30.135325698915423'
])
})

test('filter-fold-reduce', () => {
  expect(runProgram(`
(filter string? (list 4 "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" #t "HelloWorld" "HelloWorld" "HelloWorld" list))
(filter procedure? (list string-length list? + - "true" #f = string?))
(filter null? (list (pair 2 4) (pair "a" "b") (pair "first" "second") (list) (list 4 5 6)))
(filter list? (list (pair 2 4) (pair "a" "b") (pair "first" "second") (list) (list 4 5 6)))
(fold + 5 (list 0 1 2 3 4))
(fold - 5 (list 10 9 8 7 6))
(reduce + (list 5 0 1 2 3 4))
(reduce - (list 5 10 9 8 7 6))
(fold-right - 5 (list 10 9 8 7 6))
(reduce-right - (list 10 9 8 7 6 5))
`)).toEqual([
  '(list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld")',
  '(list [Function: string-length] [Function: list?] [Function: +] [Function: -] [Function: =] [Function: string?])',
  '(list null)',
  '(list null (list 4 5 6))',
  '15',
  '-35',
  '15',
  '-35',
  '3',
  '3'
])
})

test('floor-ceiling', () => {
  expect(runProgram(`
(floor 0.71)
(floor 111)
(floor 6.1)
(floor 0.69)
(floor 0.10000000000000009)
(floor 0)
(ceiling 0.71)
(ceiling 111)
(ceiling 6.1)
(ceiling 0.69)
(ceiling 0.10000000000000009)
(ceiling 0)
`)).toEqual([
  '0',
  '111',
  '6',
  '0',
  '0',
  '0',
  '1',
  '111',
  '7',
  '1',
  '1',
  '0'
])
})

test('gt-geq', () => {
  expect(runProgram(`
(> 4 5)
(> 5.0 4.99)
(> 0 0.1)
(> 7 9)
(>= 5.0 5)
(>= 5.1 5)
(>= 10 5)
(>= 0.02 0.03)
(>= 0.01 0.02)
`)).toEqual([
  '#f',
  '#t',
  '#f',
  '#f',
  '#t',
  '#t',
  '#t',
  '#f',
  '#f'
])
})

test('implies-xor', () => {
  expect(runProgram(`
(implies #t #f)
(implies #f #f)
(implies #t #t)
(implies #f #t)
(xor #t #f)
(xor #f #f)
(xor #t #t)
(xor #f #t)
`)).toEqual([
  '#f',
  '#t',
  '#t',
  '#t',
  '#t',
  '#f',
  '#f',
  '#t'
])
})

test('index-of', () => {
  expect(runProgram(`
(define l (list "a" "b" "c" "d" "e"))

(index-of l "a")
(index-of l "b")
(index-of l "c")
(index-of l "d")
(index-of l "e")
(index-of l "f")
`)).toEqual([
  '0',
  '1',
  '2',
  '3',
  '4',
  '-1'
])
})

test('integer', () => {
  expect(runProgram(`
(integer? 5)
(integer? 78.0)
(integer? "5")
(integer? (/ 10 2))
(integer? 5.5)
`)).toEqual([
  '#t',
  '#t',
  '#f',
  '#t',
  '#f'
])
})

test('length', () => {
  expect(runProgram(`
(length (list 28 0 "know"))
`)).toEqual([
  '3'
])
})

test('list-makeList', () => {
  expect(runProgram(`
(list "ab" "ba")
(list 1 2 3)
(list)
(list #t #t #t #t)
(list #f #f #f #f)
(make-list 2 "a")
(make-list 10 "HelloWorld")
(make-list 5 10)
(make-list 2 #t)
(make-list 2 "abab")
(make-list 0 "")
`)).toEqual([
  '(list "ab" "ba")',
  '(list 1 2 3)',
  'null',
  '(list #t #t #t #t)',
  '(list #f #f #f #f)',
  '(list "a" "a")',
  '(list "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld" "HelloWorld")',
  '(list 10 10 10 10 10)',
  '(list #t #t)',
  '(list "abab" "abab")',
  'null'
])
})

test('list-ref', () => {
  expect(runProgram(`
(define l (list 1 2 3 4 5))

(list-ref l 0)
(list-ref l 1)
(list-ref l 2)
(list-ref l 3)
(list-ref l 4)
`)).toEqual([
  '1',
  '2',
  '3',
  '4',
  '5'
])
})

test('list-tail', () => {
  expect(runProgram(`
(define l (list 1 2 3 4 5))

(list-tail l 0)

(list-tail l 3)

(list-tail l 5)

(list-drop l 0)

(list-drop l 3)

(list-drop l 5)
`)).toEqual([
  '(list 1 2 3 4 5)',
  '(list 4 5)',
  'null',
  '(list 1 2 3 4 5)',
  '(list 4 5)',
  'null'
])
})

test('list-take', () => {
  expect(runProgram(`
(define l (list 1 2 3 4 5))

(list-take l 0)

(list-take l 3)

(list-take l 5)
`)).toEqual([
  'null',
  '(list 1 2 3)',
  '(list 1 2 3 4 5)'
])
})

test('lt-leq', () => {
  expect(runProgram(`
(< 4 5)
(< 5.0 5)
(< 0 0.1)
(< 7 9)
(<= 5.0 5)
(<= 5.1 5)
(<= 10 5)
(<= 0.02 0.02)
(<= 0.01 0.02)
`)).toEqual([
  '#t',
  '#f',
  '#t',
  '#t',
  '#t',
  '#f',
  '#f',
  '#t',
  '#t'
])
})

test('min-max', () => {
  expect(runProgram(`
(min 4 7 2)
(min 0.01 0.2 0.5)
(min 100 10 1)
(min 2.0 2)
(min 3.0 3.1)
(max 4 7 2)
(max 0.01 0.2 0.5)
(max 100 10 1)
(max 2.0 2)
(max 3.0 3.1)
`)).toEqual([
  '2',
  '0.01',
  '1',
  '2',
  '3',
  '7',
  '0.5',
  '100',
  '2',
  '3.1'
])
})

test('nanQ', () => {
  expect(runProgram(`
(nan? "nan")
(nan? 34)
(nan? 0.1)
(nan? 0.0)
(nan? -3)
`)).toEqual([
  '#f',
  '#f',
  '#f',
  '#f',
  '#f'
])
})

test('nand-nor', () => {
  expect(runProgram(`
(nand #t #f #t #f #t #f)
(nand #f #f #f)
(nand #t #t #t)
(nand #t #f)
(nand #t #f #f #f)
(nor #t #f #t #f #t #f)
(nor #f #f #f)
(nor #t #t #t)
(nor #t #f)
(nor #t #f #f #f)
`)).toEqual([
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f'
])
})

test('not-boolean', () => {
  expect(runProgram(`
(not #t)
(not #f)
(not 1)
(boolean? #t)
(boolean? #f)
(boolean? 0)
(boolean? 2)
(boolean? 1)
`)).toEqual([
  '#f',
  '#t',
  'Runtime error [3:1-3:7]: (not) expected a boolean, received number',
  '#t',
  '#t',
  '#f',
  '#f',
  '#f'
])
})

test('nullQ-listQ', () => {
  expect(runProgram(`
(null? (list))
(null? (list "ab" "ba"))
(null? (list 1 2 3))
(null? (list ""))
(null? (list))
(list? (list))
(list? (list "ab" "ba"))
(list? (list 1 2 3))
(list? (list ""))
(list? (pair "hello" "world"))
`)).toEqual([
  '#t',
  '#f',
  '#f',
  '#f',
  '#t',
  '#t',
  '#t',
  '#t',
  '#t',
  '#f'
])
})

test('number', () => {
  expect(runProgram(`
(number? 56)
(number? "56")
(number? 56.2)
(number? (/ 56 2))
(number? 56.0)
`)).toEqual([
  '#t',
  '#f',
  '#t',
  '#t',
  '#t'
])
})

test('numeq', () => {
  expect(runProgram(`
(= 4 5)
(= 5.0 5)
(= 0 0.1)
(= 7 9)
(= 5.0 5)
(= 5.1 5)
(= 10 5)
(= 0.02 0.03)
(= 0.01 0.01)
`)).toEqual([
  '#f',
  '#t',
  '#f',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f',
  '#t'
])
})

test('odd-even', () => {
  expect(runProgram(`
(odd? 2)
(odd? (- 10 9))
(odd? 0.0)
(odd? (- 7 4))
(odd? 8)
(even? 2)
(even? 1)
(even? (- 7 3))
(even? 8)
(even? 0.0)
`)).toEqual([
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#t',
  '#t',
  '#t'
])
})

test('pairQ', () => {
  expect(runProgram(`
(pair? (pair #t #f))
(pair? (+ 1 2))
(pair? "bye")
(pair? (pair "a" "b"))
(pair? (/ 100 1))
`)).toEqual([
  '#t',
  '#f',
  '#f',
  '#t',
  '#f'
])
})

test('plus-minus', () => {
  expect(runProgram(`
(+ 4 7 2)
(+ 0.01 0.2 0.5)
(+ 100 10 1)
(+ 2.0 2)
(+ 3.0 3.1)
(- 4 7 2)
(- 0.01 0.2 0.5)
(- 100 10 1)
(- 2.0 2)
(- 3.0 3.1)
`)).toEqual([
  '13',
  '0.71',
  '111',
  '4',
  '6.1',
  '-5',
  '-0.69',
  '89',
  '0',
  '-0.10000000000000009'
])
})

test('positive-negative', () => {
  expect(runProgram(`
(positive? -0.002)
(positive? (- 7 10))
(positive? 0.0)
(positive? (- 7 3))
(positive? (- 7.8 7.7))
(negative? (- 7.8 7.7))
(negative? -0.1)
(negative? (- 7 10))
(negative? 0)
(negative? (- 7 4))
`)).toEqual([
  '#f',
  '#f',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#f'
])
})

test('random', () => {
  expect(runProgram(`
(define max-value 5)
(define num-tests 100)

(|> (make-list num-tests max-value)
    (lambda (l) (map random l))
    (lambda (l) (map (lambda (x) (and (>= x 0) (< x max-value))) l))
    (lambda (l) (reduce (lambda (b1 b2) (and b1 b2)) l))
    )

(define dice
  (lambda ()
    (+ 1 (random 6))))

(define make-dice-rolls
  (lambda (n)
    (if (zero? n)
        null
        (cons (dice) (make-dice-rolls (- n 1))))))

(|> (make-dice-rolls num-tests)
    (lambda (l) (map (lambda (x) (and (>= x 1) (<= x 6))) l))
    (lambda (l) (reduce (lambda (b1 b2) (and b1 b2)) l))
    )

`)).toEqual([
  '#t',
  '#t'
])
})

test('range', () => {
  expect(runProgram(`
(range 10)

(range 50)

(range 0)

(range -1)

(range 5 10)

(range -3 5)

(range 10 5)

(range 5 -3)

(range 0 10 2)

(range 10 5 -1)

(range 10 0 -3)
`)).toEqual([
  '(list 0 1 2 3 4 5 6 7 8 9)',
  '(list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49)',
  'null',
  'null',
  '(list 5 6 7 8 9)',
  '(list -3 -2 -1 0 1 2 3 4)',
  'null',
  'null',
  '(list 0 2 4 6 8)',
  '(list 10 9 8 7 6)',
  '(list 10 7 4 1)'
])
})

test('real', () => {
  expect(runProgram(`
(real? 56)
(real? 56.2)
(real? "56.2")
(real? (/ 50 2))
(real? (/ 51 2))
`)).toEqual([
  '#f',
  '#t',
  '#f',
  '#f',
  '#t'
])
})

test('remainder-modulo', () => {
  expect(runProgram(`
(remainder 7 2)
(remainder 35 9)
(remainder 2 2)
(modulo 7 2)
(modulo 35 9)
(modulo 2 2)
`)).toEqual([
  '1',
  '8',
  '0',
  '1',
  '8',
  '0',
])
})

test('sin-cos-tan', () => {
  expect(runProgram(`
(sin 13)
(sin 0.71)
(sin 111)
(sin 4)
(sin 6.1)
(sin -5)
(sin -0.69)
(sin 89)
(sin 0)
(cos 13)
(cos 0.71)
(cos 111)
(cos 4)
(cos 6.1)
(cos -5)
(cos -0.69)
(cos 89)
(cos 0)
(tan 13)
(tan 0.71)
(tan 111)
(tan 4)
(tan 6.1)
(tan -5)
(tan -0.69)
(tan 89)
(tan 0)
`)).toEqual([
  '0.4201670368266409',
  '0.6518337710215366',
  '-0.8645514486106083',
  '-0.7568024953079282',
  '-0.18216250427209588',
  '0.9589242746631385',
  '-0.6365371822219679',
  '0.8600694058124532',
  '0',
  '0.9074467814501962',
  '0.7583618759905082',
  '-0.5025443191453852',
  '-0.6536436208636119',
  '0.9832684384425845',
  '0.28366218546322625',
  '0.7712460149971067',
  '0.5101770449416689',
  '1',
  '0.4630211329364896',
  '0.8595286652169407',
  '1.7203486651303583',
  '1.1578212823495777',
  '-0.18526223068913525',
  '3.380515006246586',
  '-0.8253361052690248',
  '1.6858253705060158',
  '0'
])
})

test('square-sqrt', () => {
  expect(runProgram(`
(square 0.71)
(square 111)
(square 6.1)
(square 0.69)
(square 0.10000000000000009)
(square 0)
(sqrt 0.5041)
(sqrt 12321)
(sqrt 37.21)
(sqrt 0.4761)
(sqrt 0.01)
(sqrt 0)
`)).toEqual([
  '0.5041',
  '12321',
  '37.209999999999994',
  '0.4760999999999999',
  '0.010000000000000018',
  '0',
  '0.71',
  '111',
  '6.1',
  '0.6900000000000001',
  '0.1',
  '0'
])
})

test('string-comp', () => {
  expect(runProgram(`
(define s1 "hello world!")
(define s2 "hello zoo!")

(string=? s1 s1)
(string=? s1 s2)
(string=? s2 s1)
(string=? s2 s2)

(string<? s1 s1)
(string<? s1 s2)
(string<? s2 s1)
(string<? s2 s2)

(string>? s1 s1)
(string>? s1 s2)
(string>? s2 s1)
(string>? s2 s2)

(string<=? s1 s1)
(string<=? s1 s2)
(string<=? s2 s1)
(string<=? s2 s2)

(string>=? s1 s1)
(string>=? s1 s2)
(string>=? s2 s1)
(string>=? s2 s2)

(define s3 "HEllo World!")
(define s4 "heLLo zoo!")

(string-ci=? s3 s3)
(string-ci=? s3 s4)
(string-ci=? s4 s3)
(string-ci=? s4 s4)

(string-ci<? s3 s3)
(string-ci<? s3 s4)
(string-ci<? s4 s3)
(string-ci<? s4 s4)

(string-ci>? s3 s3)
(string-ci>? s3 s4)
(string-ci>? s4 s3)
(string-ci>? s4 s4)

(string-ci<=? s3 s3)
(string-ci<=? s3 s4)
(string-ci<=? s4 s3)
(string-ci<=? s4 s4)

(string-ci>=? s3 s3)
(string-ci>=? s3 s4)
(string-ci>=? s4 s3)
(string-ci>=? s4 s4)
`)).toEqual([
  '#t',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#t',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#f',
  '#f',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t',
  '#f',
  '#t',
  '#t'
])
})

test('string-length-ref', () => {
  expect(runProgram(`
(string-length "HelloWorld")
(string-length "Hello woww World")
(string-length "")
(string-length "00110011")
(string-length "1234567")
(string-ref "HelloWorld" 7)
(string-ref "Hello woww World" 10)
(string-ref " " 0)
(string-ref "00110011" 3)
(string-ref "1234567" 5)
`)).toEqual([
  '10',
  '16',
  '0',
  '8',
  '7',
  '#\\r',
  '#\\space',
  '#\\space',
  '#\\1',
  '#\\6'
])
})

test('string-map', () => {
  expect(runProgram(`
(string-map char-upcase "hello world")

(string-map char-downcase "")
`)).toEqual([
  '"HELLO WORLD"',
  '""'
])
})

test('string-number-conversions', () => {
  expect(runProgram(`
(number->string 9)
(number->string 0.4472135954999579)
(number->string 100)
(number->string 4)
(number->string 30.135325698915423)
(string->number "9")
(string->number "0.4472135954999579")
(string->number "100")
(string->number "4")
(string->number "30.135325698915423")
`)).toEqual([
  '"9"',
  '"0.4472135954999579"',
  '"100"',
  '"4"',
  '"30.135325698915423"',
  '9',
  '0.4472135954999579',
  '100',
  '4',
  '30.135325698915423'
])
})

test('string-ops', () => {
  expect(runProgram(`
(make-string 5 #\\a)
(make-string 0 #\\c)
(string-upcase "aCcD01-E")
(string-downcase "aCcD01-E")
(string-foldcase "aCcD01-E")
(substring "hello world" 3 7)
(string->list "hello world")
(list->string (list #\\h #\\e #\\l #\\l #\\o #\\space #\\w #\\o #\\r #\\l #\\d))
`)).toEqual([
  '"aaaaa"',
  '""',
  '"ACCD01-E"',
  '"accd01-e"',
  '"accd01-e"',
  '"lo w"',
  '(list #\\h #\\e #\\l #\\l #\\o #\\space #\\w #\\o #\\r #\\l #\\d)',
  '"hello world"'
])
})

test('string-split-append', () => {
  expect(runProgram(`
(string-split "HelloWorld" "w")
(string-split "Hello woww World" "w")
(string-split "" "")
(string-split "00110011" "1")
(string-split "1234567" "5")
(string-append "HelloWorld" "Hello woww World" )
(string-append " " "00110011" "1234567" )
(string-append "1234567" "89101112" "13141516")
`)).toEqual([
  '(list "HelloWorld")',
  '(list "Hello " "o" "" " World")',
  'null',
  '(list "00" "" "00" "" "")',
  '(list "1234" "67")',
  '"HelloWorldHello woww World"',
  '" 001100111234567"',
  '"12345678910111213141516"'
])
})

test('stringQ-procedure', () => {
  expect(runProgram(`
(string? (pair #t #f))
(string? 1)
(string? "bye")
(string? "a")
(string? 100)
(procedure? (pair #t #f))
(procedure? list)
(procedure? "bye")
(procedure? +)
(procedure? string-length)
`)).toEqual([
  '#f',
  '#f',
  '#t',
  '#t',
  '#f',
  '#f',
  '#t',
  '#f',
  '#t',
  '#t'
])
})

test('times-div', () => {
  expect(runProgram(`
(* 4 7 2)
(* 0.01 0.2 0.5)
(* 100 10 1)
(* 2.0 2)
(* 3.0 3.1)
(/ 4 7 2)
(/ 0.01 0.2 0.5)
(/ 100 10 1)
(/ 2.0 2)
(/ 3.0 3.1)
`)).toEqual([
  '56',
  '0.001',
  '1000',
  '4',
  '9.3',
  '0.2857142857142857',
  '0.09999999999999999',
  '10',
  '1',
  '0.9677419354838709'
])
})

test('truncate-round', () => {
  expect(runProgram(`
(truncate 0.71)
(truncate 111)
(truncate 6.1)
(truncate 0.69)
(truncate 0.10000000000000009)
(truncate 0)
(round 0.71)
(round 111)
(round 6.1)
(round 0.69)
(round 0.10000000000000009)
(round 0)
`)).toEqual([
  '0',
  '111',
  '6',
  '0',
  '0',
  '0',
  '1',
  '111',
  '6',
  '1',
  '0',
  '0'
])
})

test('vector-immutable', () => {
  expect(runProgram(`
(define empty (vector))

empty

(vector? empty)

(vector-length empty)

(define non-empty (vector 1 2 3 4 5))

non-empty

(vector? non-empty)

(vector-length non-empty)

(vector-ref non-empty 2)

(vector-ref non-empty 4)

(vector-map (lambda (x) (+ x 1)) non-empty)

(define range-test (vector-range 0 35 5))

range-test

(vector-filter (lambda (x) (= (remainder x 3) 0)) range-test)

(vector-map * (vector 1 2 3) (vector 4 5 6))

(define append-result (vector-append (vector 1 2) (vector 3 4 5) (vector 6 7 8 9) (vector 10)))

(vector-length append-result)

append-result

(vector-append)

(vector-append (vector) (vector 1 2) (vector) (vector 3) (vector) (vector 4 5))
`)).toEqual([
  '(vector)',
  '#t',
  '0',
  '(vector 1 2 3 4 5)',
  '#t',
  '5',
  '3',
  '5',
  '(vector 2 3 4 5 6)',
  '(vector 0 5 10 15 20 25 30)',
  '(vector 0 15 30)',
  '(vector 4 10 18)',
  '10',
  '(vector 1 2 3 4 5 6 7 8 9 10)',
  '(vector)',
  '(vector 1 2 3 4 5)'
])
})

test('vector-mutable', () => {
  expect(runProgram(`
(define sample-vector (vector "alpha" "beta" "gamma" "delta" "epsilon"))
sample-vector
(vector-set! sample-vector 2 "zeta")
sample-vector
(vector-set! sample-vector 0 "foo")
sample-vector
(vector-set! sample-vector 2 -38.72)
sample-vector
(vector-fill! sample-vector "woot")
sample-vector
`)).toEqual([
  '(vector "alpha" "beta" "gamma" "delta" "epsilon")',
  'void',
  '(vector "alpha" "beta" "zeta" "delta" "epsilon")',
  'void',
  '(vector "foo" "beta" "zeta" "delta" "epsilon")',
  'void',
  '(vector "foo" "beta" -38.72 "delta" "epsilon")',
  'void',
  '(vector "woot" "woot" "woot" "woot" "woot")'
])
})

test('with-handler', () => {
  expect(runProgram(`
(with-handler
  (lambda (err) (string-append "This is the error that was generated: " err))
  (lambda (x y z) (+ x y z))
  1 2 3)

(with-handler
  (lambda (err) (string-append "This is the error that was generated: " err))
  (lambda (x y z) (error "oh no, an error!"))
  1 2 3)

`)).toEqual([
  '6',
  'Runtime error [8:19-8:44]: (error) oh no, an error!',
  'void'
])
})

test('zero', () => {
  expect(runProgram(`
(zero? 0.002)
(zero? 0.1)
(zero? 0.0)
(zero? 0)
(zero? 1)
`)).toEqual([
  '#f',
  '#f',
  '#t',
  '#t',
  '#f'
])
})
