; showcase.scm -- the Scamper language, one feature at a time.
;
; Every statement produces a value, so reading the file top to bottom reads as
; a transcript of the language itself. Nothing here needs a browser: the file
; runs in the IDE, under `npm run cli`, and in the test suite alike.

; ---------------------------------------------------------------------------
; Literals
; ---------------------------------------------------------------------------

; Numbers are integers or floats, and dividing integers gives a float.
42
3.14
(/ 1 3)

; Booleans and characters have their own spellings.
#t
#\a

; Strings are double-quoted.
"the quick brown fox"

; Each bracket means exactly one thing. Parentheses are an application or a
; special form, square brackets a vector, and curly braces a map of alternating
; keys and values.
(+ 1 2)
[1 2 3]
{"name" "Ada" "born" 1815}

; ---------------------------------------------------------------------------
; Definitions and functions
; ---------------------------------------------------------------------------

(define greeting "hello")
greeting

; A function is a lambda bound to a name.
(define double (lambda (n) (* 2 n)))
(double 21)

; A parameter list may end in a rest parameter, which collects the arguments
; left over as a list.
(define count-extras (lambda (first & rest) (length rest)))
(count-extras 1 2 3 4)

; #(...) is shorthand for a small anonymous function. Its parameters are the
; numbered names %1, %2, ...; % is another spelling of %1, and %& is a rest
; parameter.
(map #(* % %) (range 5))
(#(+ %1 %2) 3 4)
(#(apply + %&) 1 2 3 4)

; ---------------------------------------------------------------------------
; Choosing between alternatives
; ---------------------------------------------------------------------------

(if (> 3 2) "bigger" "smaller")

; `cond` takes the first clause whose test holds; `else` always holds.
(define classify
  (lambda (n)
    (cond
      [(< n 0) "negative"]
      [(= n 0) "zero"]
      [else "positive"])))
(map classify (list -5 0 5))

; `and` and `or` are short-circuiting, so neither evaluates more than it must.
(and (> 2 1) (> 3 2))
(or (> 1 2) (> 3 2))

; `begin` sequences expressions and produces the last one, which matters only
; when the earlier ones have an effect.
(begin 1 2 3)

; ---------------------------------------------------------------------------
; Local bindings
; ---------------------------------------------------------------------------

(let ([x 10]
      [y 32])
  (+ x y))

; The left-hand side of a binding is a pattern, not just a name, so a binding
; can take a value apart.
(let ([(cons head tail) (list 1 2 3)])
  head)

; ---------------------------------------------------------------------------
; Structures and pattern matching
; ---------------------------------------------------------------------------

(struct point (x y))
(define origin (point 0 0))
origin

; `match` picks the first clause whose pattern fits the value. A pattern is a
; literal, a name that binds, a structure, or a vector.
(define describe
  (lambda (v)
    (match v
      [0 "zero"]
      [(point x y) (string-append "a point at x=" (number->string x))]
      [[a b] (+ a b)]
      [(cons head _) head]
      [other "something else"])))

(describe 0)
(describe (point 3 4))
(describe [10 20])
(describe (list "first" "second"))
(describe "a string")

; ---------------------------------------------------------------------------
; Lists and recursion
; ---------------------------------------------------------------------------

(list 1 2 3)
(range 5)
(cons 0 (list 1 2))
(null? null)

; A recursive function over a list splits into the empty case and the case of a
; head and a tail.
(define sum
  (lambda (l)
    (match l
      [null 0]
      [(cons head tail) (+ head (sum tail))])))

(sum (list 91 85 96 82 89))
(sum null)

; ---------------------------------------------------------------------------
; Functions as values
; ---------------------------------------------------------------------------

(map double (range 5))
(filter even? (range 10))
(fold + 0 (range 11))
(reduce + (list 1 2 3 4))
(sort (list 3 1 2) <)

; A function that returns a function closes over what was in scope where it was
; made.
(define adder (lambda (n) (lambda (m) (+ n m))))
(define add-five (adder 5))
(add-five 10)

; ---------------------------------------------------------------------------
; Strings, characters, vectors, and maps
; ---------------------------------------------------------------------------

(string-append "scam" "per")
(string-upcase "shout")
(string-split "a,b,c" ",")
(string->list "hi")
(char-upcase #\a)

(vector-ref [10 20 30] 1)
(vector-map #(* % 10) [1 2 3])
(vector->list [1 2 3])

(define ada {"name" "Ada" "born" 1815})
(hash-ref ada "name")
(hash-keys ada)

; ---------------------------------------------------------------------------
; Documenting a function
; ---------------------------------------------------------------------------

; A comment written with three semicolons above a definition is its
; documentation: the IDE shows it on hover, and each @example line is an
; expression the IDE can check against the value written after it.

;;; (fahrenheit->celsius f) -> number?
;;;   f : number?
;;; Converts a temperature in degrees Fahrenheit to degrees Celsius.
;;; @example (fahrenheit->celsius 32) => 0
;;; @example (fahrenheit->celsius 212) => 100
(define fahrenheit->celsius
  (lambda (f)
    (* (- f 32) (/ 5 9))))

(fahrenheit->celsius 98.6)

; ---------------------------------------------------------------------------
; Importing a library
; ---------------------------------------------------------------------------

; A one-argument import brings a library's names into scope.
(import image)
(beside (circle 40 "solid" "red") (square 40 "outline" "blue"))

; A two-argument import binds the library to a name instead, so its functions
; are reached through that name and nothing is added to the surrounding scope.
(import music mus)
(mus.note 60 mus.qn)

; `display` is a statement rather than an expression: it shows a value in the
; output without that value being the result of anything.
(display (fahrenheit->celsius 72))
