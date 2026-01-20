import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

////////////////////////////////////////////////////////////////////////////////

test('begin', () => {
  expect(runProgram(`
(begin
  (+ 1 1)
  5
  "hello!"
  "goodbye!")
`)).toEqual([
  '"goodbye!"'
])
})

test('ceiling', () => {
  expect(runProgram(`
(ceiling 3.2)

(ceiling 4)

(ceiling 3.001)

(ceiling -2.1)

(ceiling 0.0)
`)).toEqual([
  '4',
  '4',
  '4',
  '-2',
  '0'
])
})

test('control', () => {
  expect(runProgram(`
(define inc
  (lambda (x) (+ x 1)))

(define l (list 1 2 3 4 5 6 7 8 9 10))

(map inc l)

(map (lambda (p) (car p))
  (list (pair "a" "b") (pair "c" "d") (pair "e" "f")))

(map inc null)

(map - (list 3 5 7 1 9)
       (list 1 2 3 0 13))

(map (lambda (x y z) (if x y z))
     (list #t #f #t)
     (list "yes" "no" "maybe")
     (list "y" "n" "m"))

(filter
  (lambda (v)
    (= (remainder v 2) 0))
  l)

(filter
  (lambda (v)
    (>= v 5))
  l)

(filter
  (lambda (v)
    (< v 5))
  l)

(filter (lambda (v) (> v 20)) l)

(filter (lambda (v) #t) null)

(fold + 0 l)
(reduce + l)
`)).toEqual([
  '(list 2 3 4 5 6 7 8 9 10 11)',
  '(list "a" "c" "e")',
  'null',
  '(list 2 3 4 1 -4)',
  '(list "yes" "n" "maybe")',
  '(list 2 4 6 8 10)',
  '(list 5 6 7 8 9 10)',
  '(list 1 2 3 4)',
  'null',
  'null',
  '55',
  '55'
])
})

test('error', () => {
  expect(runProgram(`
(error "This is an example runtime error")
`)).toEqual([
  'Runtime error [1:1-1:42]: (error) This is an example runtime error'
])
})

test('length', () => {
  expect(runProgram(`
(length (list 1 2 3 4 5))
(length (list))
(length (list "a" "b" "c"))

`)).toEqual([
  '5',
  '0',
  '3'
])
})

test('max', () => {
  expect(runProgram(`
(max 3 2 8 4 10 -4 5)
(max -5)
(max 1 1 1 1 1 1 1)
`)).toEqual([
  '10',
  '-5',
  '1'
])
})

test('min', () => {
  expect(runProgram(`
(min 3 2 8 4 10 -4 5)
(min -5)
(min 1 1 1 1 1 1 1)
`)).toEqual([
  '-4',
  '-5',
  '1'
])
})

test('qq', () => {
  expect(runProgram(`
(+ (??) 1)
`)).toEqual([
  'Runtime error [1:4-1:7]: (??) Hole encountered in program!'
])
})

test('reverse', () => {
  expect(runProgram(`
(reverse (list 1 2 3 4 5))

(reverse (list))

(reverse (reverse (list 1 2 3 4 5 6 7 8 9 10)))
`)).toEqual([
  '(list 5 4 3 2 1)',
  'null',
  '(list 1 2 3 4 5 6 7 8 9 10)'
])
})

test('string-append', () => {
  expect(runProgram(`
(string-append "hello" " " "world!")

(string-append "hi")
`)).toEqual([
  '"hello world!"',
  '"hi"'
])
})

test('string-length', () => {
  expect(runProgram(`
(string-length "hello world")
(string-length "")
(string-length "\n\n\n\n\n")
`)).toEqual([
  '11',
  '0',
  '5'
])
})

test('string-split', () => {
  expect(runProgram(`
(string-split "Twas brillig and the slithy toves" " ")

`)).toEqual([
  '(list "Twas" "brillig" "and" "the" "slithy" "toves")'
])
})
