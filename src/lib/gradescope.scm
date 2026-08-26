;;; The Gradescope autograder library (issue #404).
;;;
;;; A `gradescope-test-suite-output` prints as exactly the JSON blob Gradescope
;;; reads from `results/results.json`, so an autograder is a Scamper program
;;; whose last expression is a call to `gradescope-test-suite`. The harness that
;;; runs one lives in `gradescope/` at the root of this repository.

;; A single Gradescope test case, mirroring the fields of its test result API.
;; `output` holds the value shown as the case's output: a string is used as-is,
;; anything else is rendered the way Scamper prints it -- which is how a test
;; result carries its own failure message across.
(struct gradescope-test-result (name status score max-score output))
(export gradescope-test-result
        gradescope-test-result?
        gradescope-test-result-name
        gradescope-test-result-status
        gradescope-test-result-score
        gradescope-test-result-max-score
        gradescope-test-result-output)

;; A whole suite: the `tests` array Gradescope reads.
;;
;; N.B., the constructor is deliberately *not* exported. gradescope-test-suite
;; is the only way to build one, so every case a suite holds has been checked to
;; be one -- the renderer (src/js/gradescope/renderers/json.ts) reads their
;; fields without re-checking, and a suite built by hand from the wrong values
;; would otherwise print JSON missing the fields Gradescope needs, which it
;; accepts without complaint.
(struct gradescope-test-suite-output (tests))
(export gradescope-test-suite-output?
        gradescope-test-suite-output-tests)

;; A test result as the Gradescope test case it becomes: worth one point,
;; awarded only when it passed, with the result itself as the case's output so
;; the student reads the same message the IDE would have shown them. A case
;; built by hand -- for a bonus mark, or one worth more than a point -- is
;; already in its final form and passes straight through.
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
    (gradescope-test-suite-output (map test-result->case tests))))
