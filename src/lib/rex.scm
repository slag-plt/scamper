;;; (rex? v) -> boolean?
;;;  v : any
;;; Returns `#t` if `v` is a regex, `#f` otherwise.
;;; @category predicates, regexes, typecheck, rex-matches?, pair?, list?, procedure?, ref?, vector?
(define rex? (js-var "rex_isRegex"))

;;; (rex-empty) -> rex?
;;; Returns a regex that matches the empty string.
;;; @category regexes, rex-empty, rex-split-string, rex-string, rex->string
(define rex-empty (js-var "rex_rexEmpty"))

;;; (rex-string s) -> rex?
;;;  s : string?
;;; Returns a regex that matches the exact string `s`.
;;; @category regexes, rex-empty, rex-split-string, rex->string
(define rex-string (js-var "rex_rexString"))

;;; (rex-repeat r) -> rex?
;;;  r : rex?
;;; Returns a regex that matches one or more repetitions of the regex `r`.
;;; @category regexes, rex-concat, rex-repeat-o
(define rex-repeat (js-var "rex_rexRepeat"))

;;; (rex-repeat-0 r) -> rex?
;;;  r : rex?
;;; Returns a regex that matches zero or more repetitions of the regex `r`.
;;; @category regexes, rex-concat, rex-repeat
(define rex-repeat-0 (js-var "rex_rexRepeat0"))

;;; (rex-concat & xs) -> rex?
;;;  xs : rex?
;;; Returns a regex that matches the concatenation of the regexes `rs` in order.
;;; @category regexes, rex-repeat, rex-repeat-o
(define rex-concat (js-var "rex_rexConcat"))

;;; (rex-any-char) -> rex?
;;; Returns a regex that matches any single character.
;;; @category regexes, rex-char-antiset, rex-char-range, rex-char-set
(define rex-any-char (js-var "rex_rexAnyChar"))

;;; (rex-char-set s) -> rex?
;;;  s : string?
;;; Returns a regex that matches any single character in the string `s`.
;;; @category regexes, rex-any-char, rex-char-antiset, rex-char-range
(define rex-char-set (js-var "rex_rexCharSet"))

;;; (rex-char-antiset s) -> rex?
;;;  s : string?
;;; Returns a regex that matches any single character not in the string `s`.
;;; @category regexes, rex-any-char, rex-char-range, rex-char-set
(define rex-char-antiset (js-var "rex_rexCharAntiset"))

;;; (rex-char-range start end) -> rex?
;;;  start : char?
;;;  end : char?
;;; Returns a regex that matches any single character in the inclusive range from `start` to `end`.
;;; @category regexes, rex-any-char, rex-char-antiset, rex-char-set
(define rex-char-range (js-var "rex_rexCharRange"))

;;; (rex-any-of & xs) -> rex?
;;;  xs : rex?
;;; Returns a regex that matches any one of the regexes `rs`.
;;; @category regexes, rex-optional
(define rex-any-of (js-var "rex_rexAnyOf"))

;;; (rex-optional r) -> rex?
;;;  r : rex?
;;; Returns a regex that matches either the regex `r` or the empty string.
;;; @category regexes, rex-any-of
(define rex-optional (js-var "rex_rexOptional"))

;;; (regex pattern flags) -> rex?
;;;  pattern : string?
;;;  flags : string?
;;; Returns a regex that matches a Javascript regex `pattern`. See [the MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions) for more details.
;;; @category regexes, list, pair, string, vector
(define regex (js-var "rex_rexRegex"))

;;; (rex-find-matches r s) -> list?
;;;  r : rex?
;;;  s : string?
;;; Returns a list of all non-overlapping matches of the regex `r` in the string `s`.
;;; @category regexes, rex-matches?, rex-split-string
(define rex-find-matches (js-var "rex_rexFindMatches"))

;;; (rex-matches? r s) -> boolean?
;;;  r : rex?
;;;  s : string?
;;; Returns `#t` if the regex `r` matches the entire string `s`, `#f` otherwise.
;;; @category predicates, regexes, typecheck, rex-find-matches, rex?
(define rex-matches? (js-var "rex_rexMatches"))

;;; (rex-split-string r s) -> list?
;;;  r : rex?
;;;  s : string?
;;; Splits the string `s` at each match of the regex `r` and returns a list of the resulting substrings.
;;; @category regexes, rex-empty, rex-string, rex->string, rex-find-matches
(define rex-split-string (js-var "rex_rexSplitString"))

;;; (rex->string r) -> string?
;;;  r : rex?
;;; Returns the Javascript regex string representation of the regex `r`.
;;; @category regexes, rex-empty, rex-split-string, rex-string
(define rex->string (js-var "rex_rexToString"))
