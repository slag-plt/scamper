;;; (rex? v) -> boolean
;;;  v : any
;;; Returns `#t` if `v` is a regex, `#f` otherwise.
(define rex? (js-var "rex_isRegex"))
;;; (rex-empty) -> rex
;;; Returns a regex that matches the empty string.
(define rex-empty (js-var "rex_rexEmpty"))
;;; (rex-string s) -> rex
;;;  s : string?
;;; Returns a regex that matches the exact string `s`.
(define rex-string (js-var "rex_rexString"))
;;; (rex-repeat r) -> rex
;;;  r : rex
;;; Returns a regex that matches one or more repetitions of the regex `r`.
(define rex-repeat (js-var "rex_rexRepeat"))
;;; (rex-repeat-0 r) -> rex
;;;  r : rex
;;; Returns a regex that matches zero or more repetitions of the regex `r`.
(define rex-repeat-0 (js-var "rex_rexRepeat0"))
;;; (rex-concat x1) -> rex
;;;  x1 : any
;;;   rex...
;;; Returns a regex that matches the concatenation of the regexes `rs` in order.
(define rex-concat (js-var "rex_rexConcat"))
;;; (rex-any-char) -> rex
;;; Returns a regex that matches any single character.
(define rex-any-char (js-var "rex_rexAnyChar"))
;;; (rex-char-set s) -> rex
;;;  s : string?
;;; Returns a regex that matches any single character in the string `s`.
(define rex-char-set (js-var "rex_rexCharSet"))
;;; (rex-char-antiset s) -> rex
;;;  s : string?
;;; Returns a regex that matches any single character not in the string `s`.
(define rex-char-antiset (js-var "rex_rexCharAntiset"))
;;; (rex-char-range start end) -> rex
;;;  start : char?
;;;  end : char?
;;; Returns a regex that matches any single character in the inclusive range from `start` to `end`.
(define rex-char-range (js-var "rex_rexCharRange"))
;;; (rex-any-of x1) -> rex
;;;  x1 : any
;;;   rex...
;;; Returns a regex that matches any one of the regexes `rs`.
(define rex-any-of (js-var "rex_rexAnyOf"))
;;; (rex-optional r) -> rex
;;;  r : rex
;;; Returns a regex that matches either the regex `r` or the empty string.
(define rex-optional (js-var "rex_rexOptional"))
;;; (regex pattern flags) -> rex
;;;  pattern : string?
;;;  flags : string?
;;; Returns a regex that matches a Javascript regex `pattern`. See [the MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions) for more details.
(define regex (js-var "rex_rexRegex"))
;;; (rex-find-matches r s) -> list?
;;;  r : rex
;;;  s : string?
;;; Returns a list of all non-overlapping matches of the regex `r` in the string `s`.
(define rex-find-matches (js-var "rex_rexFindMatches"))
;;; (rex-matches? r s) -> boolean
;;;  r : rex
;;;  s : string?
;;; Returns `#t` if the regex `r` matches the entire string `s`, `#f` otherwise.
(define rex-matches? (js-var "rex_rexMatches"))
;;; (rex-split-string r s) -> list?
;;;  r : rex
;;;  s : string?
;;; Splits the string `s` at each match of the regex `r` and returns a list of the resulting substrings.
(define rex-split-string (js-var "rex_rexSplitString"))
;;; (rex->string r) -> string?
;;;  r : rex
;;; Returns the Javascript regex string representation of the regex `r`.
(define rex->string (js-var "rex_rexToString"))
