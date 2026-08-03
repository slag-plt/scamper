(define ##mkPredFn## (js-var "runtime_mkPredFn"))

(define ##mkCtorFn## (js-var "runtime_mkCtorFn"))

(define ##mkGetFn## (js-var "runtime_mkGetFn"))

(define ##typeOf## (js-var "runtime_typeOf"))

; Internal: aborts the running fiber and reports its argument as the answer to a
; live-evaluation query. A query wraps its target sub-expression in
; (##report## <expr>).
(define ##report## (js-var "runtime_report"))

(define any (js-var "runtime_any"))
