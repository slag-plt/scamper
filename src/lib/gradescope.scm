;; The Gradescope autograder library (issue #404).
;;
;; A `gradescope-test-suite-output` prints as exactly the JSON blob Gradescope
;; reads from `results/results.json`, so an autograder is a Scamper program
;; whose last expression is a call to `gradescope-test-suite`. The harness that
;; runs one lives in `gradescope/` at the root of this repository.
;;
;; N.B., the two struct types are built from the runtime's struct primitives
;; rather than declared with `struct`, so that each of their functions carries
;; a docstring (and, from it, a contract) as the rest of the standard library
;; does. `struct` is sugar for exactly these calls -- see expansion.ts.

;;; (gradescope-test-result name status score max-score output) -> gradescope-test-result?
;;;  name : string?
;;;   the name Gradescope shows for the case
;;;  status : string?
;;;   "passed" or "failed"
;;;  score : number?
;;;  max-score : number?
;;;  output : any
;;;   the text shown under the case; a string is used as it is, and any other
;;;   value is rendered the way Scamper prints it
;;; Returns a single Gradescope test case. Build one directly for a case that
;;; is not simply one point -- a bonus mark, or work you scored by hand -- and
;;; hand it to `gradescope-test-suite` alongside your test results.
;;; @category testing
(define-export gradescope-test-result
  ((js-var "runtime_mkCtorFn")
    "gradescope-test-result"
    ((js-var "prelude_vector") "name" "status" "score" "max-score" "output")))

;;; (gradescope-test-result? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a Gradescope test case.
(define-export gradescope-test-result?
  ((js-var "runtime_mkPredFn") "gradescope-test-result"))

;;; (gradescope-test-result-name c) -> string?
;;;  c : gradescope-test-result?
;;; Returns the name Gradescope shows for the case `c`.
;;; @category testing
(define-export gradescope-test-result-name
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "name"))

;;; (gradescope-test-result-status c) -> string?
;;;  c : gradescope-test-result?
;;; Returns the status of the case `c`, either "passed" or "failed".
;;; @category testing
(define-export gradescope-test-result-status
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "status"))

;;; (gradescope-test-result-score c) -> number?
;;;  c : gradescope-test-result?
;;; Returns the points the case `c` awards.
;;; @category testing
(define-export gradescope-test-result-score
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "score"))

;;; (gradescope-test-result-max-score c) -> number?
;;;  c : gradescope-test-result?
;;; Returns the points the case `c` is out of.
;;; @category testing
(define-export gradescope-test-result-max-score
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "max-score"))

;;; (gradescope-test-result-output c) -> any
;;;  c : gradescope-test-result?
;;; Returns the value shown as the output of the case `c`.
;;; @category testing
(define-export gradescope-test-result-output
  ((js-var "runtime_mkGetFn") "gradescope-test-result" "output"))

;; N.B., the suite constructor is deliberately *not* exported.
;; `gradescope-test-suite` is the only way to build one, so every case a suite
;; holds has been checked to be one -- the renderer
;; (src/js/gradescope/renderers/json.ts) reads their fields without re-checking,
;; and a suite built by hand from the wrong values would otherwise print JSON
;; missing the fields Gradescope needs, which it accepts without complaint.
(define mk-suite-output
  ((js-var "runtime_mkCtorFn")
    "gradescope-test-suite-output"
    ((js-var "prelude_vector") "tests")))

;;; (gradescope-test-suite-output? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is the results of a whole Gradescope test
;;; suite.
(define-export gradescope-test-suite-output?
  ((js-var "runtime_mkPredFn") "gradescope-test-suite-output"))

;;; (gradescope-test-suite-output-tests s) -> list?
;;;  s : gradescope-test-suite-output?
;;; Returns the list of `gradescope-test-result?` cases the suite `s` holds.
;;; @category testing
(define-export gradescope-test-suite-output-tests
  ((js-var "runtime_mkGetFn") "gradescope-test-suite-output" "tests"))

;; A test result as the Gradescope test case it becomes: worth one point,
;; awarded only when it passed, with the result itself as the case's output so
;; the student reads the same message the IDE would have shown them. A case
;; built by hand is already in its final form and passes straight through.
(define test-result->case
  (lambda (r)
    (match r
      [(gradescope-test-result _ _ _ _ _) r]
      [(test-result-ok desc)
       (gradescope-test-result desc "passed" 1 1 r)]
      [(test-result-error-expected desc _ _)
       (gradescope-test-result desc "failed" 0 1 r)]
      [(test-result-error-exn desc _)
       (gradescope-test-result desc "failed" 0 1 r)]
      [(test-result-error-gen desc _)
       (gradescope-test-result desc "failed" 0 1 r)]
      [_ (error "gradescope-test-suite: expected a list of test results")])))

;;; (gradescope-test-suite tests) -> gradescope-test-suite-output?
;;;  tests : list?
;;;   a list of test-result? values, and any gradescope-test-result? cases
;;;   built by hand
;;; Collects `tests` into the results Gradescope expects of an autograder: each
;;; test result is worth one point, awarded only if it passed, and carries its
;;; usual message as its output; a hand-built case is kept as it is. The result
;;; prints as the JSON blob to write to `results/results.json`.
;;; @category testing
(define-export gradescope-test-suite
  (lambda (tests)
    (mk-suite-output (map test-result->case tests))))
