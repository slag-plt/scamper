(define-export ##mkPredFn## (js-var "runtime_mkPredFn"))

(define-export ##mkCtorFn## (js-var "runtime_mkCtorFn"))

(define-export ##mkGetFn## (js-var "runtime_mkGetFn"))

(define-export ##typeOf## (js-var "runtime_typeOf"))

; Internal: aborts the running fiber and reports its argument as the answer to a
; live-evaluation query. A query wraps its target sub-expression in
; (##report## <expr>).
(define-export ##report## (js-var "runtime_report"))

(define-export any (js-var "runtime_any"))
