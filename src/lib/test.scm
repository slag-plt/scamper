;;; (test-result-ok desc) -> test-result?
;;;  desc : string?
;;; Returns a test result indicating that the test named `desc` passed.
;;; @category testing
(define test-result-ok (js-var "test_testResultOk"))

;;; (test-result-error-expected desc expected actual) -> test-result?
;;;  desc : string?
;;;  expected : any
;;;  actual : any
;;; Returns a test result indicating that the test named `desc` failed because
;;; it produced `actual` instead of the `expected` value.
;;; @category testing
(define test-result-error-expected (js-var "test_testResultErrorExpected"))

;;; (test-result-error-exn desc exn) -> test-result?
;;;  desc : string?
;;;  exn : any
;;; Returns a test result indicating that the test named `desc` failed because
;;; it raised the unexpected exception `exn`.
;;; @category testing
(define test-result-error-exn (js-var "test_testResultErrorExn"))

;;; (test-result-error-gen desc reason) -> test-result?
;;;  desc : string?
;;;  reason : string?
;;; Returns a test result indicating that the test named `desc` failed for the
;;; given `reason`. (This constructor was formerly named `test-error`.)
;;; @category testing
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
(define test-case
  (lambda (desc eq? expected test-fn)
    (with-handler
      (lambda (err) (test-result-error-exn desc err))
      (lambda ()
        (let* ([actual (test-fn)]
               [is-equal (eq? expected actual)])
          (cond
            [(equal? is-equal #t) (test-result-ok desc)]
            [(equal? is-equal #f) (test-result-error-expected desc expected actual)]
            [else (error "Test case function should have produced a boolean")]))))))

;;; (test-exn desc test-fn) -> test-result?
;;;  desc : string?
;;;  test-fn : procedure?
;;;   a function that should throw an exception
;;; Returns a test result indicating whether the given function threw an exception.
;;; @category testing
(define test-exn
  (lambda (desc test-fn)
    (with-handler
      (lambda (err) (test-result-ok desc))
      (lambda ()
        (begin
          (test-fn)
          (test-result-error-gen desc "Test case did not throw an exception"))))))
