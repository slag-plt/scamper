(define test-result-ok (js-var "test_testResultOk"))

(define test-result-error-expected (js-var "test_testResultErrorExpected"))

(define test-result-error-exn (js-var "test_testResultErrorExn"))

(define test-result-error-gen (js-var "test_testResultErrorGeneric"))

;;; (test-result? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a test result.
(define test-result? (js-var "test_isResult"))

;;; (test-case desc eq? expected test-fn) -> test-result?
;;;  desc : string?
;;;  eq? : procedure?
;;;   a function that tests for equality between two values
;;;  expected : any
;;;  test-fn : procedure?
;;;   a function that produces the actual value to be tested
;;; Returns a test result indicating whether the given equality test passed: `(eq? expected (test-fn))`.
;;; @category testing
(define test-case (js-var "test_testCase"))

;;; (test-exn desc test-fn) -> test-result?
;;;  desc : string?
;;;  test-fn : procedure?
;;;   a function that should throw an exception
;;; Returns a test result indicating whether the given function threw an exception.
;;; @category testing
(define test-exn (js-var "test_testExn"))
