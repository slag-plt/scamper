import { expect, test } from "vitest"
import { runProgram } from "../harness.js"

// https://github.com/slag-plt/scamper/issues/138
// https://github.com/slag-plt/scamper/issues/176

test("fold-arguments", () => {
  expect(
    runProgram(`
  (fold-left string-append "" (list "!" "%" "#" "@"))
  (fold-right string-append "" (list "!" "%" "#" "@"))

  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Folding-of-Lists.html#index-fold_002dleft
  ;; "obsolete procedure: fold-left proc knil list
  ;; Deprecated, use fold instead."
  ;; so, https://srfi.schemers.org/srfi-1/srfi-1.html#fold
  ;; "fold(-left) kons knil clist ... -> value
  ;;  First, consider the single list-parameter case.
  ;;  If clist = (e1 e2 ... en), then this procedure returns
  ;;  (kons en ... (kons e2 (kons e1 knil)) ... )"
  (equal?
    (fold-left - 0 (list 1 2 3 4 5))
    (- 5 (- 4 (- 3 (- 2 (- 1 0))))))
   
  ;; https://srfi.schemers.org/srfi-1/srfi-1.html#fold-right
  ;;  "fold-right kons knil clist ... -> value
  ;;  First, consider the single list-parameter case.
  ;;  If clist = (e1 e2 ... en), then this procedure returns
  ;;  (kons e1 (kons e2 ... (kons en knil)))"
  (equal?
    (fold-right - 0 (list 1 2 3 4 5))
    (- 1 (- 2 (- 3 (- 4 (- 5 0))))))
  
  ;; cases adapted from https://srfi.schemers.org/srfi-1/srfi-1.html#fold
  (fold-left cons (list) (list 1 2 3 4 5))          ; Reverse list.
  
  (equal?
    (fold-left cons (list 4 5 6) (list 1 2 3))
    (append (reverse (list 1 2 3)) (list 4 5 6)))   ; https://srfi.schemers.org/srfi-1/srfi-1.html#append-reverse
    
  ;; How many booleans in list?
  (fold-left (lambda (x count) (if (boolean? x) (+ count 1) count))
      0
      (list 1 #t 3 #f #f))
      
  ;; Length of the longest string in LIS:
  (fold-left (lambda (s max-len) (max max-len (string-length s)))
      0
      (list "short" "longest" "long" "short2"))
      
  ;; cases adapted from https://srfi.schemers.org/srfi-1/srfi-1.html#fold-right
  ;; Copy list.
  (fold-right cons (list) (list 1 2 3 4 5))                   

  ;; Filter the even numbers out of list.
  (fold-right
    (lambda (x l) (if (even? x) (cons x l) l))
    (list)
    (list 1 2 3 4 5))
  `),
  ).toEqual([
    '"@#%!"',
    '"!%#@"',
    "#t",
    "#t",
    "(list 5 4 3 2 1)",
    "#t",
    "3",
    "7",
    "(list 1 2 3 4 5)",
    "(list 2 4)",
  ])
})
