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

scamperTest('begin', `
(begin
  (+ 1 1)
  5
  "hello!"
  "goodbye!")
`, [
  '"goodbye!"'
])
scamperTest('ceiling', `
(ceiling 3.2)

(ceiling 4)

(ceiling 3.001)

(ceiling -2.1)

(ceiling 0.0)
`, [
  '4',
  '4',
  '4',
  '-2',
  '0'
])
scamperTest('control', `
(define inc
  (lambda (x) (+ x 1)))

(define l (list 1 2 3 4 5 6 7 8 9 10))

(map inc l)

(map (lambda (p) (car p))
  (list (cons "a" "b") (cons "c" "d") (cons "e" "f")))

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
`, [
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
scamperTest('error', `
(error "This is an example runtime error")
`, [
  ':0:0: Runtime error:',
  'A runtime error was encountered: This is an example runtime error',
  'In program: (error "This is an example runtime error")'
])
scamperTest('length', `
(length (list 1 2 3 4 5))
(length (list))
(length (list "a" "b" "c"))

`, [
  '5',
  '0',
  '3'
])
scamperTest('max', `
(max 3 2 8 4 10 -4 5)
(max -5)
(max 1 1 1 1 1 1 1)
`, [
  '10',
  '-5',
  '1'
])
scamperTest('min', `
(min 3 2 8 4 10 -4 5)
(min -5)
(min 1 1 1 1 1 1 1)
`, [
  '-4',
  '-5',
  '1'
])
scamperTest('qq', `
(+ {??} 1)
`, [
  ':3:0: Runtime error:',
  'A hole was encountered!',
  'In program: {??}'
])
scamperTest('reverse', `
(reverse (list 1 2 3 4 5))

(reverse (list))

(reverse (reverse (list 1 2 3 4 5 6 7 8 9 10)))
`, [
  '(list 5 4 3 2 1)',
  'null',
  '(list 1 2 3 4 5 6 7 8 9 10)'
])
scamperTest('string-append', `
(string-append "hello" " " "world!")

(string-append "hi")
`, [
  '"hello world!"',
  '"hi"'
])
scamperTest('string-length', `
(string-length "hello world")
(string-length "")
(string-length "\n\n\n\n\n")
`, [
  '11',
  '0',
  '5'
])
scamperTest('string-split', `
(string-split "Twas brillig and the slithy toves" " ")

`, [
  '(list "Twas" "brillig" "and" "the" "slithy" "toves")'
])
