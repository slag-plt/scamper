;; An example Gradescope harness. Replace the tests with your own, and make the
;; imported file name match what the assignment asks students to submit.
;;
;; The rules are simple:
;;
;;   + the student's file is imported by name, since run_autograder puts it in
;;     the same directory as this one;
;;   + the last expression must be the call to gradescope-test-suite, and
;;     nothing else may print, since this program's output *is* results.json.

(import test)
(import gradescope)
(import "hw01.scm")

(gradescope-test-suite
  (list
    (test-case "double 4 is 8" equal? 8 (lambda () (double 4)))
    (test-case "double 0 is 0" equal? 0 (lambda () (double 0)))
    (test-case "double -3 is -6" equal? -6 (lambda () (double -3)))
    (test-exn "double rejects a string" (lambda () (double "four")))))
