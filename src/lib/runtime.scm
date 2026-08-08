(define-export ##mkPredFn## (js-var "runtime_mkPredFn"))

(define-export ##mkCtorFn## (js-var "runtime_mkCtorFn"))

(define-export ##mkGetFn## (js-var "runtime_mkGetFn"))

(define-export ##typeOf## (js-var "runtime_typeOf"))

; Internal: builds the vector a vector literal [e1 ... ek] denotes, and the
; Javascript object a map literal {k1 v1 ... kn vn} denotes. Expansion rewrites
; every [...] / {...} into a call to these. They are internal (rather than the
; prelude's `vector`) so that a user binding cannot change what a literal means.
(define-export ##mkVec## (js-var "runtime_mkVec"))

(define-export ##mkObj## (js-var "runtime_mkObj"))

; Internal: raises a runtime error carrying its argument. Expansion injects it
; for a `cond` fall-through, and contract insertion for a failed check. It is
; internal (rather than the prelude's `error`) so that a user binding named
; `error` cannot change what those forms do.
(define-export ##error## (js-var "runtime_error"))

; Internal: aborts the running fiber and reports its argument as the answer to a
; live-evaluation query. A query wraps its target sub-expression in
; (##report## <expr>).
(define-export ##report## (js-var "runtime_report"))

(define-export any (js-var "runtime_any"))
