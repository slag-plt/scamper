;;; (equal? v1 v2) -> boolean?
;;;  v1 : any
;;;  v2 : any
;;; Returns `#t` if and only `v1` and `v2` are (structurally) equal values.
;;; @category predicates
(define equal? (js-var "prelude_equalQ"))

;;; (number? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a number.
;;; @category math, comparator, typecheck, predicates
(define number? (js-var "prelude_numberQ"))

;;; (real? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a real number.
;;; @category math, comparator, typecheck, predicates
(define real? (js-var "prelude_realQ"))

;;; (integer? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is an integer.
;;; @category math, comparator, typecheck, predicates
(define integer? (js-var "prelude_integerQ"))

;;; (nan? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is the number `NaN`.
;;; @category math, comparator, typecheck, predicates
(define nan? (js-var "prelude_nanQ"))

;;; (< v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns `#t` if and only `v1` is strictly less than `v2`.
;;; @category math, comparator
(define < (js-var "prelude_lt"))

;;; (<= v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns `#t` if and only `v1` is less than or equal to `v2`.
;;; @category math, comparator
(define <= (js-var "prelude_leq"))

;;; (> v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns `#t` if and only `v1` is strictly greater than `v2`.
;;; @category math, comparator
(define > (js-var "prelude_gt"))

;;; (>= v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns `#t` if and only `v1` is greater than or equal to `v2`.
;;; @category math, comparator
(define >= (js-var "prelude_geq"))

;;; (= v1 v2) -> boolean?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns `#t` if and only `v1` is equal to `v2`.
;;; @category math, comparator
(define = (js-var "prelude_eq"))

;;; (=-eps n) -> procedure?
;;;  n : number?
;;; Returns a function that takes two numbers `x` and `y` as input returns `#t` if `|x - y| < n`.
;;; @category function composition
(define =-eps (js-var "prelude_equalsEps"))

;;; (zero? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is zero.
;;; @category comparator, math, predicates, typecheck
(define zero? (js-var "prelude_zeroQ"))

;;; (positive? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is positive.
;;; @category math, comparator, typecheck, predicates
(define positive? (js-var "prelude_positiveQ"))

;;; (negative? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is negative.
;;; @category math, comparator, typecheck, predicates
(define negative? (js-var "prelude_negativeQ"))

;;; (odd? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is odd.
;;; @category math, comparator, typecheck, predicates
(define odd? (js-var "prelude_oddQ"))

;;; (even? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is even.
;;; @category math, comparator, predicates
(define even? (js-var "prelude_evenQ"))

;;; (max . v) -> number?
;;;  v : number?
;;; Returns the maximum of the given numbers.
;;; @category math, comparator
(define max (js-var "prelude_max"))

;;; (min . v) -> number?
;;;  v : number?
;;; Returns the minimum of the given numbers.
;;; @category math, comparator
(define min (js-var "prelude_min"))

;;; (+ . v1) -> number?
;;;  v1 : number?
;;; Returns the sum of `v1`, `v2`, ... .
;;; @category math, algebra
(define + (js-var "prelude_plus"))

;;; (- . v1) -> number?
;;;  v1 : number?
;;; Returns the difference of `v1`, `v2`, ... .
;;; @category math, algebra
(define - (js-var "prelude_minus"))

;;; (* . v1) -> number?
;;;  v1 : number?
;;; Returns the product of `v1`, `v2`, ... .
;;; @category math, algebra
(define * (js-var "prelude_times"))

;;; (/ . v1) -> number?
;;;  v1 : number?
;;; Returns the quotient of `v1`, `v2`, ... .
;;; @category math, algebra
(define / (js-var "prelude_div"))

;;; (abs v) -> number?
;;;  v : number?
;;; Returns the absolute value of `v`.
;;; @category math, algebra
(define abs (js-var "prelude_abs"))

;;; (quotient v1 v2) -> number?
;;;  v1 : integer?
;;;  v2 : integer?
;;; Returns the quotient of `v1` and `v2`, _i.e._, the whole number part of `v1 / v2`.
;;; @category math, algebra
(define quotient (js-var "prelude_quotient"))

;;; (remainder v1 v2) -> number?
;;;  v1 : integer?
;;;  v2 : integer?
;;; Returns the remainder of `v1` and `v2`, _i.e._, the remainder of `v1 / v2`.
;;; @category math, algebra
(define remainder (js-var "prelude_remainder"))

;;; (modulo v1 v2) -> number?
;;;  v1 : number?
;;;  v2 : number?
;;; Returns `k = n - d * q` where `q` is the integer such that `k` has the same sign as the divisor `d` while being as close to 0 as possible. (Source: [MDN docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Remainder).)
;;; @category math, algebra
(define modulo (js-var "prelude_modulo"))

;;; (floor v) -> integer?
;;;  v : number?
;;; Returns the largest integer less than or equal to `v`.
;;; @category math, algebra
(define floor (js-var "prelude_floor"))

;;; (ceiling v) -> integer?
;;;  v : number?
;;; Returns the smallest integer greater than or equal to `v`.
;;; @category math, algebra
(define ceiling (js-var "prelude_ceiling"))

;;; (truncate v) -> integer?
;;;  v : number?
;;; Returns the integer closest to `v` less than or equal to `v`.
;;; @category math, algebra
(define truncate (js-var "prelude_truncate"))

;;; (round v) -> integer?
;;;  v : number?
;;; Returns the integer closest to `v`.
;;; @category math, algebra
(define round (js-var "prelude_round"))

;;; (square v) -> number?
;;;  v : number?
;;; Returns the square of `v`.
;;; @category math, algebra
(define square (js-var "prelude_square"))

;;; (sqrt v) -> number?
;;;  v : number?
;;; Returns the square root of `v`.
;;; @category math, algebra
(define sqrt (js-var "prelude_sqrt"))

;;; (expt x y) -> number?
;;;  x : number?
;;;  y : number?
;;; Returns `x` raised to the power of `y`.
;;; @category math, algebra
(define expt (js-var "prelude_expt"))

;;; (number->string v) -> string?
;;;  v : number?
;;; Returns the string representation of `v`.
;;; @category string
(define number->string (js-var "prelude_numberToString"))

;;; (string->number s) -> number?
;;;  s : string?
;;; Returns the number denoted by `s`, or `#f` if `s`
;;; does not denote a number.
;;; @category string
(define string->number (js-var "prelude_stringToNumber"))

;;; (exp v) -> number?
;;;  v : number?
;;; Returns the exponential of `v`.
;;; @category math, algebra
(define exp (js-var "prelude_exp"))

;;; (log v) -> number?
;;;  v : number?
;;; Returns the natural logarithm of `v`.
;;; @category math, algebra
(define log (js-var "prelude_log"))

;;; (sin v) -> number?
;;;  v : number?
;;; Returns the sine of `v`.
;;; @category math, trigonometry
(define sin (js-var "prelude_sin"))

;;; (cos v) -> number?
;;;  v : number?
;;; Returns the cosine of `v`.
;;; @category math, trigonometry
(define cos (js-var "prelude_cos"))

;;; (tan v) -> number?
;;;  v : number?
;;; Returns the tangent of `v`.
;;; @category math, trigonometry
(define tan (js-var "prelude_tan"))

;;; (asin v) -> number?
;;;  v : number?
;;; Returns the arc sine of `v`.
;;; @category math, trigonometry
(define asin (js-var "prelude_asin"))

;;; (acos v) -> number?
;;;  v : number?
;;; Returns the arc cosine of `v`.
;;; @category math, trigonometry
(define acos (js-var "prelude_acos"))

;;; (atan v) -> number?
;;;  v : number?
;;; Returns the arc tangent of `v`.
;;; @category math, trigonometry
(define atan (js-var "prelude_atan"))

;;; (not v) -> boolean?
;;;  v : boolean?
;;; Returns `#t` if and only `v` is `#f`.
;;; @category boolean/logic
(define not (js-var "prelude_not"))

;;; (boolean? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a boolean.
;;; @category typecheck, boolean/logic, predicates
(define boolean? (js-var "prelude_booleanQ"))

;;; (nand . v1) -> boolean?
;;;  v1 : boolean?
;;; Equivalent to `(not (and v1 v2 ...))`.
;;; @category boolean/logic
(define nand (js-var "prelude_nand"))

;;; (nor . v1) -> boolean?
;;;  v1 : boolean?
;;; Equivalent to `(not (or v1 v2 ...))`.
;;; @category boolean/logic
(define nor (js-var "prelude_nor"))

;;; (implies v1 v2) -> boolean?
;;;  v1 : boolean?
;;;  v2 : boolean?
;;; Equivalent to `(if v1 v2 #t)`.
;;; @category boolean/logic, predicates
(define implies (js-var "prelude_implies"))

;;; (xor v1 v2) -> boolean?
;;;  v1 : boolean?
;;;  v2 : boolean?
;;; Equivalent to `(or (and v1 (not v2)) (and (not v1) v2))`.
;;; @category boolean/logic
(define xor (js-var "prelude_xor"))

;;; (any-of . f1) -> procedure?
;;;  f1 : any
;;;   procedure? that takes a value as input and returns a boolean.
;;; Returns a unary function that returns `#t` if and only one of `f1`, `f2`, ... is `#t` for its argument.
;;; @category function composition, boolean/logic
(define any-of
  (lambda (f . fs)
    (lambda (v)
      (some-satisfy? (lambda (g) (g v)) (cons f fs)))))

;;; (all-of . f1) -> procedure?
;;;  f1 : any
;;;   procedure? that takes a value as input and returns a boolean.
;;; Returns a unary function that returns `#t` if and only all of `f1`, `f2`, ... are `#t` for its argument.
;;; @category function composition, boolean/logic
(define all-of
  (lambda (f . fs)
    (lambda (v)
      (all-satisfy? (lambda (g) (g v)) (cons f fs)))))

;;; (pair? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a pair.
;;; @category typecheck, predicates
(define pair? (js-var "prelude_pairQ"))

;;; (list-of p) -> procedure?
;;;  p : procedure?
;;;   returns `#t` if its argument is of the desired type
;;; Returns a new predicate that tests whether its argument is a list of elements that satisfy the predicate `p`.
;;; @category list, function composition, association list
(define list-of
  (lambda (p)
    (lambda (l)
      (and (list? l) (all-satisfy? p l)))))

;; N.B., deliberately plain Scamper (not js-var-backed): the contract-check
;; codegen (contract.ts) needs to call an arbitrary predicate -- possibly a
;; user-defined closure -- once per element of a variadic argument's
;; collected rest-list, and JS code can no longer call back into Scamper
;; (Closure.call/callScamperFn are both disabled). Written in Scamper, the
;; call to pred? is ordinary application, so it works uniformly whether
;; pred? is a closure or a js-var-backed primitive.
;; N.B., left undocumented like "any", for the same reason: it's
;; compiler-support infrastructure, not a documented user-facing binding.
(define all-satisfy?
  (lambda (pred? lst)
    (if (null? lst)
        #t
        (if (pred? (car lst))
            (all-satisfy? pred? (cdr lst))
            #f))))

;;; (cons v1 v2) -> pair?
;;;  v1 : any
;;;  v2 : any
;;; Returns a new cons cell containing `v1` and `v2`.
;;; @category list, list manipulation, association list
(define cons (js-var "prelude_cons"))

;;; (pair v1 v2) -> pair?
;;;  v1 : any
;;;  v2 : any
;;; Returns a new pair containing `v1` and `v2`.
;;; @category list, list creation
(define pair (js-var "prelude_pair"))

;; N.B., left undocumented, like all-satisfy?: it's contract-support
;; infrastructure (or/p's fold over its predicates), not a user-facing
;; binding. Documenting it would make contract.ts wrap it in a checking
;; lambda whose own checks call all-satisfy? -> car/cdr -> back into the
;; contracted car/cdr, so it MUST stay undocumented to avoid that cycle.
;; Uses match (not car/cdr) to walk the list for the same reason: match
;; decomposes via the runtime's pMatch, so it never reaches contracted car/cdr.
(define some-satisfy?
  (lambda (pred? lst)
    (match lst
      [null #f]
      [(cons x rest) (if (pred? x) #t (some-satisfy? pred? rest))])))

;; N.B., or/p combines predicates disjunctively: (or/p p q) is a predicate
;; that holds when p OR q holds. Written in plain Scamper (not a js-var) so
;; that applying each predicate is ordinary Scamper application -- JS code can
;; no longer call back into Scamper (Closure.call/callScamperFn are disabled).
;; Left UNDOCUMENTED for the same reason as some-satisfy?: a docstring would
;; make contract.ts wrap it, and that wrapper's checks would recurse through
;; car/cdr into or/p (which car/cdr's own contract references), re-creating the
;; cycle. Requires at least one predicate: the arglist grammar has no
;; zero-fixed-param rest form, and or/p over no predicates is never needed.
(define or/p
  (lambda (first . rest)
    (lambda (x) (some-satisfy? (lambda (p) (p x)) (cons first rest)))))

;;; (car v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Returns the first element of `v`.
;;; @category list, list manipulation, association list
(define car (js-var "prelude_car"))

;;; (cdr v) -> any
;;;  v : (or/p pair? nonempty-list?)
;;; Returns the second element of `v`.
;;; @category list, list manipulation, association list
(define cdr (js-var "prelude_cdr"))

;;; (null? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is the empty list.
;;; @category list, association list, typecheck, predicates
(define null? (js-var "prelude_nullQ"))

;;; (list? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a list.
;;; @category list, association list, typecheck, predicates
(define list? (js-var "prelude_listQ"))

;;; (nonempty-list? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a non-empty list.
;;; @category typecheck, predicates
(define nonempty-list? (js-var "prelude_nonemptyListQ"))

;;; (list . v1) -> list?
;;;  v1 : any
;;; Returns a new list containing `v1`, `v2`, ... .
;;; @category list, list creation, association list
(define list (js-var "prelude_list"))

;;; (make-list n v) -> list?
;;;  n : integer?
;;;  v : any
;;; Returns a new list containing `n` copies of `v`.
;;; @category list, list creation, association list
(define make-list (js-var "prelude_makeList"))

;;; (length v) -> integer?
;;;  v : list?
;;; Returns the length of `v`.
;;; @category list, list manipulation
(define length (js-var "prelude_length"))

;;; (append . l1) -> list?
;;;  l1 : list?
;;; Returns a new list containing the elements of lists `l1`, `l2`, ... in sequence.
;;; @category list, list manipulation
(define append (js-var "prelude_append"))

;;; (reverse l) -> list?
;;;  l : list?
;;; Returns a new list containing the elements of `l` in reverse order.
;;; @category list, list manipulation
(define reverse (js-var "prelude_reverse"))

;;; (list-tail l k) -> list?
;;;  l : list?
;;;  k : integer?
;;;   0 <= k <= (length l)
;;; Returns `l` but with the first `k` elements of `l` omitted.
;;; @category list, list manipulation, association list
(define list-tail (js-var "prelude_listTail"))

;;; (list-take l k) -> list?
;;;  l : list?
;;;  k : integer?
;;;   0 <= k <= (length l)
;;; Returns a new list containing the first `k` elements of `l`.
;;; @category list, list manipulation, association list
(define list-take (js-var "prelude_listTake"))

;;; (list-drop l k) -> list?
;;;  l : list?
;;;  k : integer?
;;;   0 <= k <= (length l)
;;; An alias for `(list-tail l k)`.
;;; @category list, list manipulation, association list
(define list-drop (js-var "prelude_listDrop"))

;;; (list-ref l n) -> any
;;;  l : list?
;;;  n : integer?
;;;   0 <= n < (length l)
;;; Returns the `n`th element of `l`.
;;; @category list, association list
(define list-ref (js-var "prelude_listRef"))

;;; (index-of l v) -> integer?
;;;  l : list?
;;;  v : any
;;; Returns the index of the first occurrence of `v` in `l` or `-1` if `v` is not in `l`.
;;; @category list, list manipulation, association list
(define index-of (js-var "prelude_indexOf"))

;;; (assoc-key? k l) -> any
;;;  k : any
;;;  l : list?
;;;   an association list
;;; Returns `#t` if `k` is a key in association list `l`.
;;; @category list, list manipulation, association list, predicates
(define assoc-key? (js-var "prelude_assocKey"))

;;; (assoc-ref k l) -> any
;;;  k : any
;;;  l : list?
;;;   an association list
;;; Returns the value associated with key `k` in association list `l`.
;;; @category list, list manipulation, association list
(define assoc-ref (js-var "prelude_assocRef"))

;;; (assoc-set k v l) -> list?
;;;  k : any
;;;  v : any
;;;  l : list?
;;;   an association list
;;; Returns a new association list containing the same key-value pairs as `l` except that `k` is associated with `v`.
;;; @category list, list manipulation, association list
(define assoc-set (js-var "prelude_assocSet"))

;; N.B., internal helper for sort: merges two lists that are each already
;; sorted by lt? into one sorted list. Undocumented (like all-satisfy?) so the
;; contract-check codegen leaves it alone. Ties favor xs (the left list) to
;; keep sort stable.
(define sort-merge
  (lambda (lt? xs ys)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [(lt? (car ys) (car xs)) (cons (car ys) (sort-merge lt? xs (cdr ys)))]
      [else (cons (car xs) (sort-merge lt? (cdr xs) ys))])))

;;; (sort l lt?) -> list?
;;;  l : list?
;;;  lt? : procedure?
;;;   returns `#t` if the first arg is less than the second
;;; Returns a new list containing the elements of `l` sorted in ascending order according to the comparison function `lt?`.
;;; @category list, list manipulation, association list
(define sort
  (lambda (l lt?)
    (if (<= (length l) 1)
        l
        (let ([mid (quotient (length l) 2)])
          (sort-merge lt?
                      (sort (list-take l mid) lt?)
                      (sort (list-drop l mid) lt?))))))

;;; (char? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a character.
;;; @category typecheck, char, predicates
(define char? (js-var "prelude_charQ"))

;;; (digit-value c) -> integer?
;;;  c : char?
;;; Returns the numeric value of `c` if `c` is a decimal digit (0-10), otherwise raises an error.
;;; @category typecheck, char
(define digit-value (js-var "prelude_digitalValue"))

;;; (char->integer c) -> integer?
;;;  c : char?
;;; Returns the codepoint value of character `c`.
;;; @category char
(define char->integer (js-var "prelude_charToInteger"))

;;; (integer->char n) -> char?
;;;  n : integer?
;;; Returns the character with codepoint value `n`.
;;; @category char
(define integer->char (js-var "prelude_integerToChar"))

;;; (char-upcase c) -> char?
;;;  c : char?
;;; Returns the upper-case equivalent of `c`.
;;; @category char
(define char-upcase (js-var "prelude_charUpcase"))

;;; (char-downcase c) -> char?
;;;  c : char?
;;; Returns the lower-case equivalent of `c`.
;;; @category char
(define char-downcase (js-var "prelude_charDowncase"))

;;; (char-foldcase c) -> char?
;;;  c : char?
;;; Returns the case-folded equivalent of `c`. This is a version of `c` that is appropriate for case-insensitive comparison.
;;; @category char
(define char-foldcase (js-var "prelude_charFoldcase"))

;;; (string? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a string.
;;; @category typecheck, string, predicates
(define string? (js-var "prelude_stringQ"))

;;; (make-string k c) -> string?
;;;  k : integer?
;;;  c : char?
;;; Returns a string of length `k` with each character set to `c`.
;;; @category string
(define make-string (js-var "prelude_makeString"))

;;; (string . c1) -> string?
;;;  c1 : char?
;;; Returns a string consisting of the characters `c1`, `c2`, ...
;;; @category string
(define string (js-var "prelude_string"))

;;; (string-length v) -> integer?
;;;  v : string?
;;; Returns the length of `v`.
;;; @category string
(define string-length (js-var "prelude_stringLength"))

;;; (string-ref s n) -> char?
;;;  s : string?
;;;  n : integer?
;;; Returns the character at index `n` of string `s`.
;;; @category string
(define string-ref (js-var "prelude_stringRef"))

;;; (string-upcase s) -> string?
;;;  s : string?
;;; Returns the upper-case version of `s`.
;;; @category string
(define string-upcase (js-var "prelude_stringUpcase"))

;;; (string-downcase s) -> string?
;;;  s : string?
;;; Returns the lower-case version of `s`.
;;; @category string
(define string-downcase (js-var "prelude_stringDowncase"))

;;; (string-foldcase s) -> string?
;;;  s : string?
;;; Returns the case-folded version of `s`. This is a version of `s` that is appropriate for case-insensitive comparison.
;;; @category string
(define string-foldcase (js-var "prelude_stringFoldcase"))

;;; (substring s start end) -> string?
;;;  s : string?
;;;  start : integer?
;;;  end : integer?
;;; Returns the substring of `s` from index `start` (inclusive) to index `end` (exclusive).
;;; @category string
(define substring (js-var "prelude_substring"))

;;; (string-append . s1) -> string?
;;;  s1 : string?
;;; Returns a string made by joining `s1`, `s2`, ... together.
;;; @category string
(define string-append (js-var "prelude_stringAppend"))

;;; (string->list s) -> list?
;;;  s : string?
;;; Returns a list of the characters in `s`.
;;; @category list, list creation, string
(define string->list (js-var "prelude_stringToList"))

;;; (list->string l) -> string?
;;;  l : list?
;;; Returns a string made by joining the characters in `l` together.
;;; @category list, list manipulation, association list
(define list->string (js-var "prelude_listToString"))

;;; (string->vector s) -> vector?
;;;  s : string?
;;; Returns a vector of the characters in `s`.
;;; @category string, vectors
(define string->vector (js-var "prelude_stringToVector"))

;;; (vector->string v) -> string?
;;;  v : vector?
;;; Returns a string made by joining the characters in `v` together.
;;; @category vectors
(define vector->string (js-var "prelude_vectorToString"))

;;; (string-contains s1 s2) -> boolean?
;;;  s1 : string?
;;;  s2 : string?
;;; Returns `#t` if and only if string `s1` contains string `s2`.
;;; @category string
(define string-contains (js-var "prelude_stringContains"))

;;; (string-split s sep) -> list?
;;;  s : string?
;;;  sep : string?
;;; Returns a list of strings obtained by splitting `s` at occurrences of `sep`.
;;; @category string
(define string-split (js-var "prelude_stringSplit"))

;;; (string-split-vector s sep) -> vector?
;;;  s : string?
;;;  sep : string?
;;; Returns a vector of strings obtained by splitting `s` at occurrences of `sep`.
;;; @category string, vectors
(define string-split-vector (js-var "prelude_stringSplitVector"))

;;; (vector? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a vector.
;;; @category typecheck, vectors, predicates
(define vector? (js-var "prelude_vectorQ"))

;;; (vector . v1) -> vector?
;;;  v1 : any
;;; Returns a vector consisting of the values `v1`, `v2`, ...
;;; @category vectors
(define vector (js-var "prelude_vector"))

;;; (make-vector k v) -> vector?
;;;  k : integer?
;;;  v : any
;;; Returns a vector of length `k` with each element set to `v`.
;;; @category vectors
(define make-vector (js-var "prelude_makeVector"))

;;; (vector-length v) -> integer?
;;;  v : vector?
;;; Returns the length of vector `v`.
;;; @category vectors
(define vector-length (js-var "prelude_vectorLength"))

;;; (vector-ref v n) -> any
;;;  v : vector?
;;;  n : integer?
;;;   a valid index into v
;;; Returns the value at index `n` of vector `v`.
;;; @category vectors
(define vector-ref (js-var "prelude_vectorRef"))

;;; (vector-set! v n x) -> void
;;;  v : vector?
;;;  n : integer?
;;;   a valid index into v
;;;  x : any
;;; Sets the value at index `n` of vector `v` to `x`.
;;; @category vectors, mutation, predicates
(define vector-set! (js-var "prelude_vectorSet"))

;;; (vector-fill! v x) -> void
;;;  v : vector?
;;;  x : any
;;; Sets each element of vector `v` to `x`.
;;; @category vectors, mutation, predicates
(define vector-fill! (js-var "prelude_vectorFill"))

;;; (vector->list v) -> list?
;;;  v : vector?
;;; Returns a list consisting of the values in vector `v`.
;;; @category list, list creation, vectors
(define vector->list (js-var "prelude_vectorToList"))

;;; (list->vector l) -> vector?
;;;  l : list?
;;; Returns a vector consisting of the values in list `l`.
;;; @category list, list manipulation, association list, vectors
(define list->vector (js-var "prelude_listToVector"))

;;; (vector-range . args) -> vector?
;;;  args : integer?
;;; Can be called with one, two, or three arguments, all of which are integers.
;;; (vector-range end) returns a vector containing the numbers from 0 to `end` (exclusive).
;;; (vector-range beg end) returns a vector containing the numbers from `beg` to `end`
;;; (exclusive). (vector-range beg end step) returns a vector containing the numbers from
;;; `beg` to `end` (exclusive) with a step size of `step`. `step` must be non-zero
;;; to avoid an infinite loop.
;;; @category vectors
(define vector-range (js-var "prelude_vectorRange"))

;;; (vector-append . v1) -> vector?
;;;  v1 : vector?
;;; Returns a new vector containing the elements of `v1`, ..., `vk` in order.
;;; @category vectors
(define vector-append (js-var "prelude_vectorAppend"))

;;; (procedure? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a procedure.
(define procedure? (js-var "prelude_procedureQ"))

;;; (string-map f s) -> string?
;;;  f : procedure?
;;;  s : string?
;;; Returns a new string containing the results of applying `f` to each character of `s`.
;;; @category string
(define string-map
  (lambda (f s)
    (list->string (map f (string->list s)))))

;; N.B., internal helpers for n-ary map: given a list of lists, collect the
;; cars (heads) or cdrs (tails) of each. Undocumented (like all-satisfy?) so
;; the contract-check codegen leaves them alone.
(define lists-cars
  (lambda (lsts)
    (if (null? lsts)
        null
        (cons (car (car lsts)) (lists-cars (cdr lsts))))))

(define lists-cdrs
  (lambda (lsts)
    (if (null? lsts)
        null
        (cons (cdr (car lsts)) (lists-cdrs (cdr lsts))))))

;;; (map f . l) -> list?
;;;  f : procedure?
;;;  l : list?
;;; Returns a new list containing the results of applying `f` to each element of `l`. When several lists are given, `f` is applied element-wise across them and all lists must have the same length.
;;; @category list, list manipulation, association list
(define map
  (lambda (f . lsts)
    (cond
      [(null? lsts) null]
      [(some-satisfy? null? lsts)
       (if (all-satisfy? null? lsts)
           null
           (error "map: all lists must have the same length"))]
      [else (cons (apply f (lists-cars lsts))
                  (apply map (cons f (lists-cdrs lsts))))])))

;;; (filter f l) -> list?
;;;  f : procedure?
;;;  l : list?
;;; Returns a new list containing the elements of `l` for which `f` returns `#t`.
;;; @category list, list manipulation, association list
(define filter
  (lambda (f l)
    (cond
      [(null? l) null]
      [(f (car l)) (cons (car l) (filter f (cdr l)))]
      [else (filter f (cdr l))])))

;;; (fold f v l) -> any
;;;  f : procedure?
;;;  v : any
;;;  l : list?
;;; Returns the result of accumulating the result of applying `f` to each element of `l`, starting with initial value `v`. The function `f` takes two arguments, the first is the accumulated value and the second is the current element.
;;; @category list, list manipulation, association list
(define fold
  (lambda (f v l)
    (if (null? l)
        v
        (fold f (f v (car l)) (cdr l)))))

;;; (reduce f l) -> any
;;;  f : procedure?
;;;  l : list?
;;; Like `fold` but uses the first element of `l` as the initial value.
;;; @category list, list manipulation
(define reduce
  (lambda (f l)
    (fold f (car l) (cdr l))))

;;; (fold-left f v l) -> any
;;;  f : procedure?
;;;  v : any
;;;  l : list?
;;; Like `fold`, but the combining function `f` takes the current element as its first argument and the accumulated value as its second.
;;; @category list, list manipulation, association list
(define fold-left
  (lambda (f v l)
    (if (null? l) v (fold-left f (f (car l) v) (cdr l)))))

;;; (fold-right f v l) -> any
;;;  f : procedure?
;;;  v : any
;;;  l : list?
;;; Returns the result of accumulating the result of applying `f` to each element of `l` in reverse order, starting with initial value `v`. The function `f` takes two arguments, the first is the current element and the second is the accumulated value.
;;; @category list, list manipulation, association list
(define fold-right
  (lambda (f v l)
    (if (null? l)
        v
        (f (car l) (fold-right f v (cdr l))))))

;;; (reduce-right f l) -> any
;;;  f : procedure?
;;;  l : list?
;;; Like `fold-right` but uses the last element of `l` as the initial value.
;;; @category list, list manipulation
(define reduce-right
  (lambda (f l)
    (match l
      [(cons x null) x]
      [(cons x rest) (f x (reduce-right f rest))])))

;;; (vector-map f . v) -> vector?
;;;  f : procedure?
;;;  v : vector?
;;; Returns a new vector containing the results of applying `f` to each element of `v1`, ..., `vk` in a element-wise fashion.
;;; @category vectors
(define vector-map
  (lambda (f . vs)
    (list->vector (apply map (cons f (map vector->list vs))))))

;;; (vector-map! f v) -> void
;;;  f : procedure?
;;;  v : vector?
;;; Mutates `v` in place, replacing each element with the result of applying `f` to it.
;;; @category vectors, mutation, predicates
(define vector-map!
  (lambda (f v)
    (for-range 0 (vector-length v)
      (lambda (i) (vector-set! v i (f (vector-ref v i)))))))

;;; (vector-for-each f v) -> void
;;;  f : procedure?
;;;  v : vector?
;;; Runs `f` on each element of `v` in order, for its side effects.
;;; @category vectors
(define vector-for-each
  (lambda (f v)
    (for-range 0 (vector-length v)
      (lambda (i) (f (vector-ref v i))))))

;;; (for-range beg end f) -> void
;;;  beg : number?
;;;  end : number?
;;;  f : procedure?
;;; Runs `f` on each integer in the range `[beg, end)`. `f` takes one argument, the current value of integer.
;;; @category other
(define for-range
  (lambda (beg end f)
    (cond
      [(< beg end) (begin (f beg) (for-range (+ beg 1) end f))]
      [(> beg end) (begin (f beg) (for-range (- beg 1) end f))]
      [else void])))

;;; (vector-filter f l) -> list?
;;;  f : procedure?
;;;  l : vector?
;;; Returns a new vector containing the elements of `l` for which `f` returns `#t`.
;;; @category vectors
(define vector-filter
  (lambda (f l)
    (list->vector (filter f (vector->list l)))))

;;; (void? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is the void value.
;;; @category predicates, typecheck
(define void? (js-var "prelude_voidQ"))

;;; (??) -> any
;;; A placeholder for an expression that is not yet implemented.
;;; @category constants, other
(define ?? (js-var "prelude_qq"))

;;; (compose . f1) -> procedure?
;;;  f1 : procedure?
;;; Returns a new procedure that is the composition of the given functions, _i.e._, `f(x) = f1(f2(...(fk(x))))`.
;;; @category function composition
(define compose
  (lambda (f . fs)
    (lambda (x)
      (fold-right (lambda (g acc) (g acc)) x (cons f fs)))))

;;; (o . f) -> procedure?
;;;  f : procedure?
;;; A synonym for `compose`.
;;; @category function composition
(define o
  (lambda (f . fs)
    (apply compose (cons f fs))))

;;; (|> v . f1) -> any
;;;  v : any
;;;  f1 : procedure?
;;; Returns the result of applying the given function in sequence, starting with initial value `v`, _i.e._, `(fk (fk-1(...(f1 v)))`.
;;; @category function composition
(define |>
  (lambda (v . fs)
    (fold (lambda (acc f) (f acc)) v fs)))

;;; (range . args) -> list?
;;;  args : integer?
;;; Can be called with one, two, or three arguments, all of which are integers.
;;; (range end) returns a list containing the numbers from 0 to `end` (exclusive).
;;; (range beg end) returns a list containing the numbers from `beg` to `end`
;;; (exclusive). (range beg end step) returns a list containing the numbers from
;;; `beg` to `end` (exclusive) with a step size of `step`. `step` must be non-zero
;;; to avoid an infinite loop.
;;; @category list, list creation
(define range (js-var "prelude_range"))

;;; (random n) -> list?
;;;  n : integer?
;;;   n >= 0
;;; Returns a random number in the range 0 to n (exclusive).
;;; @category other
(define random (js-var "prelude_random"))

;; N.B., `with-handler` is now a reserved-word special form, not a library
;; binding -- it must install an exception handler in the bytecode, which a
;; js-var-backed procedure cannot do. Its handler/function/args are ordinary
;; sub-expressions; on a raised runtime error the fiber unwinds to the handler
;; and applies it to the error's message string. Parsing lives in
;; syntax.grammar (WithHandler) + lezer-bridge; the AST node is WithHandlerExp;
;; codegen lowers it to push-handler/apply/pop-handler (see the LPM handler
;; stack in src/lpm/fiber.ts). As it is no longer a documented define, it does
;; not appear on the generated docs site (like `error`/`if`/`cond`).

;;; (ignore v) -> void
;;;  v : any
;;; Suppresses the output of value `v` to the page.
;;; @category other
(define ignore (js-var "prelude_ignore"))

;;; (set-maximum-recursion-depth! n) -> void
;;;  n : any
;;;   number? n >= 0
;;; Sets the maximum recursion depth of Scamper to n. Note that tail call-optimized functions do _not_ count towards this limit.
;;; @category mutation, predicates
(define set-maximum-recursion-depth! (js-var "prelude_setMaximumRecursionDepth"))

;;; (string->words s) -> list?
;;;  s : string?
;;; Returns a list of the words in `s`, stripping whitespace and punctuation.
;;; @category string
(define string->words (js-var "prelude_stringToWords"))

;;; (ref v) -> ref?
;;;  v : any
;;; Returns a reference cell initially containing `v`.
;;; @category other
(define ref (js-var "prelude_ref"))

;;; (ref? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only `v` is a reference cell.
;;; @category typecheck, predicates
(define ref? (js-var "prelude_isRef"))

;;; (deref r) -> any
;;;  r : ref?
;;; Returns the value contained in reference cell `r`.
;;; @category other
(define deref (js-var "prelude_deref"))

;;; (ref-set! r v) -> void
;;;  r : ref?
;;;  v : any
;;; Sets the value contained in reference cell `r` to `v`.
;;; @category mutation
(define ref-set! (js-var "prelude_refSet"))

;;; (else) -> boolean?
;;; A synonym for `#t` appropriate for use as the final guard of a `cond` expression.
;;; @category boolean/logic, constants
(define else (js-var "prelude_elseConst"))

;;; (null) -> list?
;;; The empty list.
;;; @category list, list creation, association list, constants
(define null (js-var "prelude_nullConst"))

;;; (pi) -> number?
;;; The constant π.
;;; @category math, algebra, constants
(define pi (js-var "prelude_piConst"))

;;; (π) -> number?
;;; The constant π.
;;; @category math, algebra, constants
(define π (js-var "prelude_piConst"))

;;; (void) -> void
;;; The void value.
;;; @category constants
(define void (js-var "prelude_voidConst"))

;;; (with-file filename fn) -> any
;;;  filename : string?
;;;  fn : procedure?
;;; Loads `filename` from storage and passes its contents to `fn` as input. The output of `fn` is returned (and rendered to the screen if this is a top-level expression).
;;; @category other
(define with-file
  (lambda (filename fn)
    (fn ((js-var "prelude_blockOnReadFile") filename))))

;;; (with-file-chooser fn) -> void
;;;  fn : procedure?
;;; Renders a file chooser widget. When the user selects a file, its contents are passed to `fn` as input. The output of `fn` is then rendered to the screen.
;;; @category interactive
(define with-file-chooser (js-var "prelude_withFileChooser"))

;;; (caar v) -> any
;;;  v : any
;;; Equivalent to `(car (car v))`.
;;; @category list, list manipulation, association list
(define caar (lambda (v) (car (car v))))

;;; (cadr v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr v))`.
;;; @category list, list manipulation, association list
(define cadr (lambda (v) (car (cdr v))))

;;; (cdar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car v))`.
;;; @category list, list manipulation, association list
(define cdar (lambda (v) (cdr (car v))))

;;; (cddr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr v))`.
;;; @category list, list manipulation, association list
(define cddr (lambda (v) (cdr (cdr v))))

;;; (caaar v) -> any
;;;  v : any
;;; Equivalent to `(car (car (car v)))`.
;;; @category list, list manipulation, association list
(define caaar (lambda (v) (car (car (car v)))))

;;; (cadar v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr (car v)))`.
;;; @category list, list manipulation, association list
(define cadar (lambda (v) (car (cdr (car v)))))

;;; (cdaar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car (car v)))`.
;;; @category list, list manipulation, association list
(define cdaar (lambda (v) (cdr (car (car v)))))

;;; (cddar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr (car v)))`.
;;; @category list, list manipulation, association list
(define cddar (lambda (v) (cdr (cdr (car v)))))

;;; (caadr v) -> any
;;;  v : any
;;; Equivalent to `(car (car (cdr v)))`.
;;; @category list, list manipulation, association list
(define caadr (lambda (v) (car (car (cdr v)))))

;;; (caddr v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr (cdr v)))`.
;;; @category list, list manipulation, association list
(define caddr (lambda (v) (car (cdr (cdr v)))))

;;; (cdadr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car (cdr v)))`.
;;; @category list, list manipulation, association list
(define cdadr (lambda (v) (cdr (car (cdr v)))))

;;; (cdddr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr (cdr v)))`.
;;; @category list, list manipulation, association list
(define cdddr (lambda (v) (cdr (cdr (cdr v)))))

;;; (caaaar v) -> any
;;;  v : any
;;; Equivalent to `(car (car (car (car v))))`.
;;; @category list, list manipulation, association list
(define caaaar (lambda (v) (car (car (car (car v))))))

;;; (cadaar v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr (car (car v))))`.
;;; @category list, list manipulation, association list
(define cadaar (lambda (v) (car (cdr (car (car v))))))

;;; (cdaaar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car (car (car v))))`.
;;; @category list, list manipulation, association list
(define cdaaar (lambda (v) (cdr (car (car (car v))))))

;;; (cddaar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr (car (car v))))`.
;;; @category list, list manipulation, association list
(define cddaar (lambda (v) (cdr (cdr (car (car v))))))

;;; (caadar v) -> any
;;;  v : any
;;; Equivalent to `(car (car (cdr (car v))))`.
;;; @category list, list manipulation, association list
(define caadar (lambda (v) (car (car (cdr (car v))))))

;;; (caddar v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr (cdr (car v))))`.
;;; @category list, list manipulation, association list
(define caddar (lambda (v) (car (cdr (cdr (car v))))))

;;; (cdadar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car (cdr (car v))))`.
;;; @category list, list manipulation, association list
(define cdadar (lambda (v) (cdr (car (cdr (car v))))))

;;; (cdddar v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr (cdr (car v))))`.
;;; @category list, list manipulation, association list
(define cdddar (lambda (v) (cdr (cdr (cdr (car v))))))

;;; (caaadr v) -> any
;;;  v : any
;;; Equivalent to `(car (car (car (cdr v))))`.
;;; @category list, list manipulation, association list
(define caaadr (lambda (v) (car (car (car (cdr v))))))

;;; (cadadr v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr (car (cdr v))))`.
;;; @category list, list manipulation, association list
(define cadadr (lambda (v) (car (cdr (car (cdr v))))))

;;; (cdaadr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car (car (cdr v))))`.
;;; @category list, list manipulation, association list
(define cdaadr (lambda (v) (cdr (car (car (cdr v))))))

;;; (cddadr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr (car (cdr v))))`.
;;; @category list, list manipulation, association list
(define cddadr (lambda (v) (cdr (cdr (car (cdr v))))))

;;; (caaddr v) -> any
;;;  v : any
;;; Equivalent to `(car (car (cdr (cdr v))))`.
;;; @category list, list manipulation, association list
(define caaddr (lambda (v) (car (car (cdr (cdr v))))))

;;; (cadddr v) -> any
;;;  v : any
;;; Equivalent to `(car (cdr (cdr (cdr v))))`.
;;; @category list, list manipulation, association list
(define cadddr (lambda (v) (car (cdr (cdr (cdr v))))))

;;; (cdaddr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (car (cdr (cdr v))))`.
;;; @category list, list manipulation, association list
(define cdaddr (lambda (v) (cdr (car (cdr (cdr v))))))

;;; (cddddr v) -> any
;;;  v : any
;;; Equivalent to `(cdr (cdr (cdr (cdr v))))`.
;;; @category list, list manipulation, association list
(define cddddr (lambda (v) (cdr (cdr (cdr (cdr v))))))

;;; (char=? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... are all equivalent characters.
;;; @category char, predicates
(define char=? (js-var "prelude_char=?"))

;;; (char<? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have strictly increasing character values.
;;; @category char, predicates
(define char<? (js-var "prelude_char<?"))

;;; (char>? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have strictly decreasing character values.
;;; @category char, predicates
(define char>? (js-var "prelude_char>?"))

;;; (char<=? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have non-decreasing character values.
;;; @category char, predicates
(define char<=? (js-var "prelude_char<=?"))

;;; (char>=? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have non-increasing character values.
;;; @category char, predicates
(define char>=? (js-var "prelude_char>=?"))

;;; (char-ci=? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... are all equivalent characters, ignoring case.
;;; @category char, predicates
(define char-ci=? (js-var "prelude_char-ci=?"))

;;; (char-ci<? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have strictly increasing character values, ignoring case.
;;; @category char, predicates
(define char-ci<? (js-var "prelude_char-ci<?"))

;;; (char-ci>? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have strictly decreasing character values, ignoring case.
;;; @category char, predicates
(define char-ci>? (js-var "prelude_char-ci>?"))

;;; (char-ci<=? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have non-decreasing character values, ignoring case.
;;; @category char, predicates
(define char-ci<=? (js-var "prelude_char-ci<=?"))

;;; (char-ci>=? . c1) -> boolean?
;;;  c1 : char?
;;; Returns `#t` if and only `c1`, `c2`, ... have non-increasing character values, ignoring case.
;;; @category char, predicates
(define char-ci>=? (js-var "prelude_char-ci>=?"))

;;; (char-alphabetic? c) -> boolean?
;;;  c : char?
;;; Returns `#t` if and only `c` is an alphabetic character.
;;; @category typecheck, char, predicates
(define char-alphabetic? (js-var "prelude_char-alphabetic?"))

;;; (char-numeric? c) -> boolean?
;;;  c : char?
;;; Returns `#t` if and only `c` is a numeric character.
;;; @category typecheck, char, predicates
(define char-numeric? (js-var "prelude_char-numeric?"))

;;; (char-whitespace? c) -> boolean?
;;;  c : char?
;;; Returns `#t` if and only `c` is a whitespace character.
;;; @category typecheck, char, predicates
(define char-whitespace? (js-var "prelude_char-whitespace?"))

;;; (char-upper-case? c) -> boolean?
;;;  c : char?
;;; Returns `#t` if and only `c` is an upper-case character.
;;; @category typecheck, char, predicates
(define char-upper-case? (js-var "prelude_char-upper-case?"))

;;; (char-lower-case? c) -> boolean?
;;;  c : char?
;;; Returns `#t` if and only `c` is a lower-case character.
;;; @category char, predicates
(define char-lower-case? (js-var "prelude_char-lower-case?"))

;;; (string=? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are equivalent strings.
;;; @category string, predicates
(define string=? (js-var "prelude_string=?"))

;;; (string<? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically increasing order.
;;; @category string, predicates
(define string<? (js-var "prelude_string<?"))

;;; (string>? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically decreasing order.
;;; @category string, predicates
(define string>? (js-var "prelude_string>?"))

;;; (string<=? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in lexicographical order.
;;; @category string, predicates
(define string<=? (js-var "prelude_string<=?"))

;;; (string>=? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in reverse lexicographical order.
;;; @category string, predicates
(define string>=? (js-var "prelude_string>=?"))

;;; (string-ci=? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are equivalent strings, ignoring case.
;;; @category string, predicates
(define string-ci=? (js-var "prelude_string-ci=?"))

;;; (string-ci<? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically increasing order, ignoring case.
;;; @category string, predicates
(define string-ci<? (js-var "prelude_string-ci<?"))

;;; (string-ci>? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically decreasing order, ignoring case.
;;; @category string, predicates
(define string-ci>? (js-var "prelude_string-ci>?"))

;;; (string-ci<=? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in lexicographical order, ignoring case.
;;; @category string, predicates
(define string-ci<=? (js-var "prelude_string-ci<=?"))

;;; (string-ci>=? . s1) -> boolean?
;;;  s1 : string?
;;; Returns `#t` if and only `s1`, `s2`, ... are in reverse lexicographical order, ignoring case.
;;; @category string, predicates
(define string-ci>=? (js-var "prelude_string-ci>=?"))
