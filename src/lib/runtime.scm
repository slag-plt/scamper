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

; Internal: the pieces a contract wrapper needs to take a signature's optional
; parameters off its own rest parameter -- the i'th optional (or void), what
; follows the optionals, the too-many-arguments check, and the void test the
; predicate checks skip on. They are internal (rather than the prelude's `car`,
; `list-tail`, and `void?`) so that a documented parameter named after one of
; those cannot change what every call to its own function does.
(define-export ##optArg## (js-var "runtime_optArg"))

(define-export ##optRest## (js-var "runtime_optRest"))

(define-export ##checkArity## (js-var "runtime_checkArity"))

(define-export ##voidQ## (js-var "runtime_voidQ"))

(define-export any (js-var "runtime_any"))
