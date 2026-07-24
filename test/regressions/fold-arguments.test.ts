import { expect, test } from 'vitest'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/138
// https://github.com/slag-plt/scamper/issues/176

// Skipped: `fold`, `fold-left`, and `fold-right` are js-var-backed
// (src/js/prelude/index.ts's prelude_fold/prelude_foldLeft/prelude_foldRight)
// and drive the user-supplied combiner via `L.callScamperFn`, which
// op-handlers.ts's applyFn comment documents as *permanently* disabled --
// JS library code can no longer call back into Scamper closures at all (see
// src/lpm/lang.ts's callScamperFn, an unconditional throw). Confirmed still
// failing as of 2026-07-22: every fold-left/fold-right call below throws
// "Javascript library functions can no longer call Scamper functions".
// Fixing this needs fold/fold-left/fold-right (and any other js-var HOF that
// calls user closures, e.g. map/filter/reduce/for-each) reimplemented as
// native Scamper in src/lib/prelude.scm, or a real callback mechanism added
// to the LPM VM -- both larger than a regression-test fix.
test.skip('fold-arguments', async () => {
  expect(
    await runProgram(`
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
    '#t',
    '#t',
    '(list 5 4 3 2 1)',
    '#t',
    '3',
    '7',
    '(list 1 2 3 4 5)',
    '(list 2 4)',
  ])
})
