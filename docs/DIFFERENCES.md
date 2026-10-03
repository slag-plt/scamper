# How Scamper differs from R7RS Scheme

Scamper is a mini-Scheme, not a Scheme.
It is close enough that R7RS code often reads as Scamper code, and far enough that most R7RS code does not *run* as Scamper code.
This document is for someone sizing it up -- an instructor asking whether a course built on R7RS can be taught with it, or an experienced Schemer trying to find the edges quickly.

It has two halves.
Part 1 is what R7RS-small says that Scamper does not do, or does differently.
Part 2 is what Scamper has that R7RS does not, and where each borrowed idea came from.

For the *positive* statement of any form -- what it is and how to write it -- see `public/reference.html`, the student-facing language reference.
Each form is anchored there under its own name, so `reference.html#let` is what `let` *is*, and this document is only what it is *not*.
For the grammar, see `docs/language.md`, which owns it; this document does not restate it.

## Reading this document

Every claim here is either a citation into the tree or a transcript from the command-line runner.
Reproduce any of the latter with:

~~~console
$> npx tsx src/app/cli/index.ts file.scm
~~~

A top-level expression prints its value, so a one-line file is a usable experiment.
Errors print to standard error rather than to standard output, so on a terminal they interleave with the values, which is why "Variable not found: `quote`" below is a result rather than a failure.
Transcripts are real runs, trimmed to the line that matters: a statement's `void`, and the second error that a malformed form cascades into, are left out.

Two conventions:

+ **`file.ts:line` is a pointer into this repository**, and is given whenever the reason for a difference is written down in the source rather than merely visible in its behaviour.
  The prelude (`src/js/prelude/index.ts`) is organised by R7RS-small section number and carries an `// N.B., we don't implement ...` block at most of its gaps, so a large part of part 1 is the source explaining itself.
  The two files that move most -- `src/js/prelude/index.ts` and `src/lib/prelude.scm` -- are cited by path alone, since a line number into either goes stale within a release; the sentence names the binding or quotes the comment, which is what to search for.
+ **Claims about *other* languages are not citations.**
  Where this document says a name came from Racket, Clojure, or SRFI-1, that is either an attribution the Scamper source itself makes (cited) or a judgement about a resemblance (marked as one).
  Part 2 keeps the two apart deliberately.

Each gap in part 1 is labelled **by design** or **not yet**.
*Not yet* means the source marks it `// TODO: implement`, so it may close.
*By design* means the source gives a reason, which is quoted.
Four `// TODO: implement` blocks mark gaps that may close; everything else is a decision, and the summary table at the end of part 1 marks all four, with the paragraph beneath it naming them together.

## 1. Differences from R7RS

### The shape of a program

A Scamper program is a flat sequence of statements, and the statement forms are fixed: `import`, `define`, `define-export`, `export`, `display`, `struct`, or a bare expression (`src/scheme/syntax.grammar:94-108`).
There is no way to add a new one, because there are no macros.

Three consequences bite immediately.

**A body is exactly one expression.**
`lambda`, `let`, and every derived form take a single body expression, not a sequence (`docs/language.md`, "Core forms").

~~~
> (define f (lambda (x) (display x) x))
Parser error [1:11-1:36]: Malformed lambda expression (a list of parameters and a body).
~~~

`begin` (`reference.html#begin`) is how a sequence is written, and it is sugar for nested `let`, not a primitive: `(begin e1 ... ek)` expands to `(let ([_ e1]) ... ek)` (`src/scheme/expansion.ts:98-117`).
**By design.**

**There are no internal definitions.**
A `define` is a statement, and statements appear only at the top level.

~~~
> (define f (lambda (x) (let ([y 1]) (define z 2) z)))
Parser error [1:23-1:50]: Malformed let expression (a list of bindings and a body).
~~~

**By design.**

**There is no `(define (f x) ...)` shorthand.**
A procedure is defined by binding a `lambda`.

~~~
> (define (f x) (+ x 1))
Parser error [1:1-1:8]: Malformed define statement (a name and a value).
~~~

**By design.**

`display` (`reference.html#display`) is a *statement*, not a procedure, which is the sharpest of these in practice.
It cannot be passed, bound, or used in an expression at all -- the parser specialises the identifier into a keyword, so even a bare reference fails:

~~~
> display
Parser error [1:1-1:7]: Malformed syntax.
> (map display (list 1 2))
Parser error [1:1-1:24]: Malformed function application.
~~~

It also prints the *written* form rather than the displayed one: `(display "hi")` prints `"hi"`, with the quotation marks, and `(display #\a)` prints `#\a`.
R7RS's `write`/`display` distinction does not exist, and the one form Scamper has behaves as `write`.
**By design** -- there are no ports at all (see 6.13 below), so `display` is a request to the IDE's output pane rather than a procedure over a port.

Finally, R7RS's lexical syntax is thinner than it looks.
There are no block comments (`#| ... |#`) and no datum comments (`#;`); `;` to end of line is the only comment (`src/scheme/syntax.grammar:170`).

### Binding and control

**`let` (`reference.html#let`) is `letrec`.**
This is the single most surprising difference, and it is deliberate: every binder in a `let` is in scope throughout all of the binding values *and* the body (`src/scheme/scope.ts:164-189`).
A forward reference is not a scope error but a runtime one, raised only if the value is actually needed before it is built:

~~~
> (let ([f (lambda (n) (if (zero? n) 0 (f (- n 1))))]) (f 5))
0
> (let ([x y] [y 1]) x)
Runtime error: Variable "y" is referenced before it is defined
~~~

Because the bindings are evaluated in order in one recursive scope, a `let` that shadows an outer name behaves like `let*`, not like R7RS `let`:

~~~
> (define x 1)
> (let ([x 2] [y x]) y)
2
~~~

R7RS gives `1` there.
This is the difference most likely to silently change the meaning of ported code rather than fail loudly.
**By design.**

Consequently there is no `let*`, `letrec`, `letrec*`, `do`, or `let-values` -- each is an unbound variable.
Named `let` is not an unbound variable but a parse error, since `let`'s own rule has no room for the name:

~~~
> (let loop ([x 1]) x)
Parser error [1:1-1:5]: Malformed let expression (a list of bindings and a body).
~~~

**By design**; `let` already covers what they were for, and iteration is written as tail recursion.

**`if` (`reference.html#if`) requires all three parts and a boolean guard.**

~~~
> (if #t 1)
Parser error [1:1-1:9]: Malformed if expression (a guard, an if-branch, and an else-branch).
> (if 1 2 3)
Runtime error: if: expected a boolean guard, received number
~~~

There is no truthiness: only `#f` is false, and only `#t` is true.
**By design.**

**`and` and `or` (`reference.html#and`, `reference.html#or`) are boolean-valued, not value-returning.**
Both expand to nested `if` (`src/scheme/expansion.ts:118-150`), so each operand must be a boolean and the result is a boolean:

~~~
> (and 1 2)
Runtime error: if: expected a boolean guard, received number
> (or #f 5)
Runtime error: if: expected a boolean guard, received number
~~~

The R7RS idiom `(or x default)` therefore does not work.
**By design**, and a direct consequence of `if`'s boolean guard.

**`cond` (`reference.html#cond`) raises on fall-through**, rather than producing an unspecified value:

~~~
> (cond [#f 1])
Runtime error [1:1-1:13]: (error) No matching clause in cond
~~~

The raise is built into the expansion and uses a runtime primitive rather than the prelude's `error`, so it fires whether or not the user has rebound that name (`src/scheme/expansion.ts:151-189`, issue #336).
**By design.**

**`else` is a reserved word, not auxiliary syntax.**
R7RS makes `else` a literal a program may shadow; Scamper makes it a keyword no program can bind at all (`src/scheme/reserved-words.ts`), and `[else EXPR]` is a shape the grammar recognizes only as a `cond`'s last clause (`src/scheme/syntax.grammar:44-52`):

~~~
> (define else #f)
Parser error [1:1-1:16]: Malformed define statement (a name and a value).
~~~

So `[else ...]` catches whatever is left, `[#t ...]` does the same thing spelled differently, and rebinding `else` is a parse error at the `define` rather than a silent break of every later `cond` (issue #639).
`cond` also has no `=>` clause form.
**By design.**

**Absent entirely, all as unbound variables:** `set!`, `case`, `when`, `unless`, `delay`, `delay-force`, `force`, `make-promise`, `case-lambda`, `make-parameter`, `parameterize`, `guard`, `assert`, `define-values`, `let-values`.
**By design** -- most are either effectful or redundant given that a body is one expression.

`lambda`'s rest parameter (`reference.html#lambda`) is written with `&` rather than a dotted tail: `(lambda (x & rest) ...)` (`src/scheme/syntax.grammar:147-153`).

### No macros

There is no macro system of any kind: no `define-syntax`, no `syntax-rules`, no `let-syntax`, no `letrec-syntax`, no `define-record-type`, no procedural macros.
Each is simply an unbound variable.

The derived forms Scamper does have -- `and`, `or`, `begin`, `cond`, `struct`, `define-export`, `#(...)`, `[...]`, `{...}` -- are hard-wired into `src/scheme/expansion.ts`, which is a fixed table rather than an extension point.
**By design.**

This is the deepest difference in the document, because R7RS's own prose defines much of the language by macro-expansion, and a course that teaches `syntax-rules` has nothing to teach it with here.

`define-record-type` is replaced by `struct` (`reference.html#struct`), which expands to a constructor, a predicate, and one accessor per field (`src/scheme/expansion.ts:265-293`):

~~~
> (struct posn (x y))
> (define p (posn 1 2))
> p
(posn 1 2)
> (posn-x p)
1
~~~

There is no field mutator, and the accessor names are fixed as `<struct>-<field>` rather than chosen.

### No quotation, no symbols

There is no `quote`, no `'` shorthand, no quasiquotation, and no symbol type.

~~~
> (quote (1 2 3))
Runtime error: Variable not found: quote
> '(1 2 3)
Parser error [1:1-1:1]: Malformed syntax.
> `(1 ,(+ 1 2))
Runtime error: Variable not found: `
~~~

`'` is not even a character an identifier may contain -- the tokenizer treats it as a delimiter (`src/scheme/syntax.grammar:145`) -- so `'x` is a parse error rather than an unbound name.

Lists are built with `list` and vectors with the `[...]` literal, which is why the language can do without quotation at all.
**By design.**

Symbols are the one gap whose source comment is ambivalent.
`src/js/prelude/index.ts` lists `symbol?`, `symbol=?`, `symbol->string`, and `string->symbol` under `// TODO: implement:` and then closes with "...but we don't implement symbols, will we?".
Formally **not yet** -- one of the four TODO blocks -- but in practice the note reads as a decision, and nothing else in the language has a place to put one -- there is no reader syntax for a symbol, and `match` patterns bind bare identifiers rather than matching quoted ones.

### Numbers

Every Scamper number is a JavaScript double.
The prelude says so: it implements "the subset of numbers corresponding to the Javascript numeric stack: number -> real -> integer" (`src/js/prelude/index.ts`).

**There is no exactness.**
`exact?`, `inexact?`, `exact`, `inexact`, `exact->inexact`, `exact-integer?` are all absent, and `1.0` and `1` are the same value:

~~~
> 1.0
1
> (equal? 1 1.0)
#t
~~~

**By design**, per the note above.

**There are no rationals, no complex numbers, and no bignums.**
`(/ 1 3)` is `0.3333333333333333`; `(sqrt -1)` is `NaN`; `(* 99999999999 99999999999)` is `9.9999999998e+21`.
`numerator`, `denominator`, and `rationalize` are absent "since we don't implement rationals" (`src/js/prelude/index.ts`); `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, and `angle` "because we don't implement complex numbers" (`src/js/prelude/index.ts`).
`rational?` and `complex?` are absent for the same reason (`src/js/prelude/index.ts`).
**By design.**

**Division by zero raises, floats included.**
R7RS-small permits `+inf.0` for the inexact case rather than requiring it; Scamper does not:

~~~
> (/ 1 0)
Runtime error [1:1-1:7]: (/) /: division by zero
> (/ 1.0 0.0)
Runtime error [1:1-1:11]: (/) /: division by zero
> (modulo 1 0)
Runtime error [1:1-1:12]: (modulo) modulo: division by zero
~~~

**There is no radix syntax, and only `string->number` takes a radix argument.**
The number token is decimal only, with an optional sign and exponent (`src/scheme/syntax.grammar:121-126`), so `#x10` reads as an identifier, `1/2` reads as the two forms `1` and `/2`, and `+inf.0` is an unbound variable.
`number->string` takes one argument; its radix version raises an arity error.
**By design**: the literals and `number->string` are decimal-only, and nothing marks either as a gap.

`string->number` does take a radix, from 2 to 36, though only for whole numbers: a fractional numeral in a base other than 10 gives `#f` (`src/js/prelude/index.ts`).
A radix outside 2 to 36 raises, since `#f` already means "this string does not denote a number".

`gcd` and `lcm` are absent and marked `// TODO: implement` (`src/js/prelude/index.ts`).
**Not yet** -- one of the four TODO blocks.

The composite division operators (`floor/`, `floor-quotient`, `floor-remainder`, `truncate/`, `truncate-quotient`, `truncate-remainder`) are absent "to avoid clutter in the documentation" (`src/js/prelude/index.ts`), as is `exact-integer-sqrt`, "to avoid polluting the documentation" (`src/js/prelude/index.ts`).
**By design** -- these are documentation-surface decisions rather than implementation ones.
`quotient`, `remainder`, and `modulo` are all present.

**R7RS-small's `square` is spelled `sqr`.**
The name `square` belongs to the image library's shape constructor, and an import that re-binds a library name raises no diagnostic where a `define` of it would, so `(import image)` replaced the numeric `square` without a word (#677).
The numeric one now takes Racket's `racket/math` name, `sqr` (`src/lib/prelude.scm`, pinned by `test/regressions/sqr-not-square.test.ts`).
**By design** -- a deliberate divergence from 6.2.6, taken to end a collision students hit.

`real?` is `Number.isFinite` (`src/js/prelude/index.ts`), so it is false for an infinity, which R7RS would call a real.

### Equivalence

All three predicates are present: `equal?` is structural, and `eqv?` asks the sharper question of whether two values are the *same* value (`src/lib/prelude.scm`, `src/js/prelude/index.ts`).
`eq?` is a second binding of that same procedure.
R7RS permits `eq?` to draw finer distinctions than `eqv?`; Scamper represents values so that there are none left to draw -- every atom is a Javascript primitive except `char`, which the shared `eqv` helper unwraps (`src/lpm/util.ts`) -- so the two always agree here, and one native carries both names.

They were absent until issue #641, on the reasoning that mutation was confined to vectors and so identity never came up.
That stopped being true once Scamper grew reference cells (`ref`, `deref`, `ref-set!`, `src/lib/prelude.scm`) and in-place map update (`hash-set!`, `src/lib/prelude.scm`): identity was observable by mutating one of two cells and watching the other, but not askable, which is precisely what a lesson on aliasing needs to ask.
It now is:

~~~
> (define a (ref 1))
> (define b (ref 1))
> (equal? a b)
#t
> (eq? a b)
#f
> (eq? a a)
#t
~~~

Two **deviations** remain, both downstream of there being no exactness (6.2).
`(eqv? 2 2.0)` is `#t`, where R7RS would hold an exact and an inexact 2 apart.
`(eqv? 0 -0)` is `#t` as well, and this one is a real loss: the two zeroes *are* distinguishable, since `(expt -0 -1)` is `-Infinity` where `(expt 0 -1)` is `Infinity`.
Answering `#f` would nonetheless put `eqv?` at odds with `=` and `equal?`, neither of which separates them, so it answers `#t` and the leak is left documented rather than patched in one predicate.

By contrast `(eqv? (sqrt -1) (sqrt -1))` is `#f`: `NaN` is not `eqv?` to itself, which is what keeps `eqv?` a refinement of `equal?` as R7RS requires, and agrees with `=`.

`(eq? "a" "a")` is `#t`, which R7RS leaves unspecified and so **permits**: strings here are immutable Javascript primitives and there is no `string-copy` (6.7), so Scamper cannot hand out two distinct string locations to be compared in the first place.

`boolean=?` is absent, with no note in the source either way.

### Pairs, lists and mutation

**A pair and a cons cell are different types**, following Clojure (`src/lpm/lang.ts:586-588`, `src/js/prelude/index.ts`).
A cons cell's tail must be a list; a pair holds any two values.
So `cons` does not build a pair:

~~~
> (cons 1 2)
Runtime error [1:1-1:10]: (error) expected a list as the second argument, received number
> (cons 1 null)
(list 1)
> (pair 1 2)
(pair 1 2)
~~~

The rejection comes from `cons`'s declared contract, `v2 : list?` (`src/lib/prelude.scm`), which the library generates from the docstring.
There are no improper lists, and `car`/`cdr` accept either a pair or a non-empty list.
**By design.**

**The empty list is `null`, not `'()`** -- necessarily, since there is no quotation.
`null` is an ordinary binding whose value is JavaScript `null` (`src/js/prelude/index.ts`).

**Lists are immutable.**
`set-car!` and `set-cdr!` are absent "since we only implement the pure, functional subset of Scheme" (`src/js/prelude/index.ts`), `list-set!` "since it is effectful" (`src/js/prelude/index.ts`), and `list-copy` because "we have immutable lists" (`src/js/prelude/index.ts`).
**By design.**

**The `member`/`assoc` family is absent**: `memq`, `memv`, `member`, `assq`, `assv`, and `assoc` are each an unbound variable (`src/js/prelude/index.ts`).
Scamper replaces them with `index-of`, `assoc-key?`, `assoc-ref`, and `assoc-set` (`src/lib/prelude.scm`), which take the value first, matching R7RS's argument order rather than the data-first order the accessors use (`src/js/prelude/index.ts`, issue #103).
**By design**, though `assoc-set`'s implementation carries a `// TODO: implement me—this isn't the right implementation!` (`src/js/prelude/index.ts`).

The full `c[ad]+r` family through `cddddr` is present, as ordinary Scheme compositions rather than natives (`src/js/prelude/index.ts`).

### Strings and characters

**Strings are immutable.**
`string-copy`, `string-copy!`, and `string-fill!` are absent because they "don't make sense in an immutable context" (`src/js/prelude/index.ts`), and `string-set!` "since it is effectful" (`src/js/prelude/index.ts`).
The one-argument `(make-string k)` is absent for the same reason -- "having an 'empty' string of size k does not make sense" (`src/js/prelude/index.ts`) -- while the two-argument form works:

~~~
> (make-string 3 #\a)
"aaa"
> (make-string 3)
Runtime error [1:1-1:15]: (make-string) Arity mismatch in function call: expected 2 arguments, got 1
~~~

**By design.**

`string->list` has only the whole-string form; the substring-bounded variant is a TODO (`src/js/prelude/index.ts`).

Characters, by contrast, are nearly complete.
`char?`, `char->integer`, `integer->char`, `char-upcase`, `char-downcase`, `char-foldcase`, `digit-value`, the five comparisons, the five case-insensitive comparisons, and `char-alphabetic?`/`char-numeric?`/`char-whitespace?`/`char-upper-case?`/`char-lower-case?` are all present.
The source flags one uncertainty of its own: `char-foldcase` is implemented with `toLowerCase`, which "maybe" matches Unicode folding (`src/js/prelude/index.ts`).

### What the runtime does not have

**No bytevectors** (R7RS 6.9), "because they are inherently effectful" (`src/js/prelude/index.ts`).
**By design.**

**No `for-each` and no `string-for-each`**, marked `// TODO: implement` (`src/js/prelude/index.ts`).
**Not yet** -- one of the four TODO blocks.
`vector-for-each` *is* present (`src/lib/prelude.scm`), as are `map`, `string-map`, and `vector-map`; `map` is variadic over several lists.
Iteration for effect is otherwise written with `map` and `ignore`, or with `for-range`.

**No `call/cc`, `values`, `call-with-values`, or `dynamic-wind`**, all four marked `// TODO: implement` (`src/js/prelude/index.ts`).
**Not yet** -- one of the four TODO blocks.
This is a larger hole than the other four together: no continuations means no generators and no multiple-value returns, and nothing in the language substitutes for either.
Non-local exit fares better: `with-handler` and `error` together escape an arbitrarily deep call in one shot (see *Exceptions* below), which covers the commonest use of `call/cc` and nothing else.

**No `eval`, no `environment`, no `interaction-environment`** (R7RS 6.12): "platform-specific stuff with no need to be implemented" (`src/js/prelude/index.ts`).
**By design.**

**No ports** (R7RS 6.13): no `read`, `write`, `newline`, `current-output-port`, `open-input-string`, or `char-ready?`.
The reason is the deployment target -- "in-browser, so can't implement directly without some level of virtualization" (`src/js/prelude/index.ts`).
**By design.**
File I/O itself exists, as whole-file operations rather than ports.
The built-in `file` module both reads and writes -- `file-exists?`, `file->string`, `file->lines`, `string->file`, `lines->file` (`src/lib/file.scm`) -- and is imported like any other module.
The prelude adds two more: `with-file`, which reads a named file and hands its contents to a procedure, and `with-file-chooser`, which is the browser file-picker (`src/lib/prelude.scm`).

**Nothing from R7RS 6.14**: no `exit`, `command-line`, `get-environment-variable`, `current-second`, or `features` -- "all operating system-specific stuff" (`src/js/prelude/index.ts`).
**By design.**

Vectors are mutable, and `[...]` is their literal syntax -- `#(` is unavailable because it is taken by the anonymous-function form.
They are not the only mutable thing a student is handed, though: reference cells (`ref-set!`) and maps (`hash-set!`) are the others, which is why *Equivalence* above has `eqv?` to tell two of them apart.
`vector-copy` and `vector-copy!` are absent, unremarked; `vector-set!`, `vector-fill!`, `vector-append`, `vector-map`, `vector-for-each`, and the conversions are present.

### Exceptions

None of R7RS 6.11 exists: no `raise`, `raise-continuable`, `with-exception-handler`, `guard`, `error-object?`, `error-object-message`, `error-object-irritants`, `read-error?`, or `file-error?`.
The source's reason is that "exception operations are unimplemented because they are inherently effectful" (`src/js/prelude/index.ts`).
**By design.**

What Scamper has instead is `with-handler`, an ordinary two-argument procedure (`src/lib/prelude.scm`):

~~~
> (with-handler (lambda (m) (list "caught" m)) (lambda () (error "boom")))
(list "caught" "boom")
~~~

**The handler receives the error's message as a string**, not a condition object.
There is no way to recover a type, a source location, or irritants, and no way to distinguish a raised error from any other.
A runtime error from the library arrives the same way:

~~~
> (with-handler (lambda (m) m) (lambda () (car null)))
"expected pair or nonempty-list as the first argument, received null"
~~~

`error` exists but takes a message only; R7RS's irritants have no equivalent and extra arguments are silently dropped.

~~~
> (error "bad" 1 2)
Runtime error [1:1-1:17]: (error) bad
~~~

Two comments in the tree used to describe `with-handler` as a reserved-word special form; #616 removed them.
It is a library binding, as `src/scheme/raise.ts:230` notes and as the grammar confirms by having no rule for it.

### Recursion

**Tail calls are proper**, including mutual recursion, and the frame budget is not the JavaScript stack -- frames live in an array the scheduler steps through iteratively (`src/lpm/limits.ts:12-23`).
A 200,000-deep tail loop and a 200,000-deep mutual recursion both return normally.

**Non-tail recursion is capped by an explicit counter.**
The default limit is 10,000 frames (`src/lpm/limits.ts:10`), enforced in `Fiber.pushFrame` (`src/lpm/fiber.ts:294-302`):

~~~
> (define sum (lambda (n) (if (zero? n) 0 (+ n (sum (- n 1))))))
> (sum 9995)
49955010
> (sum 9999)
Runtime error: Max call stack depth 10000 exceeded!
~~~

This has no R7RS counterpart: R7RS bounds only tail recursion (by requiring it to be unbounded) and says nothing about the rest.
It is a teaching decision -- a runaway recursion reports itself instead of hanging the tab -- and it is adjustable from within a program, up to 200,000 (`src/lpm/limits.ts:23`):

~~~
> (set-maximum-recursion-depth! 50000)
> (sum 20000)
200010000
~~~

**By design.**
The practical consequence is that a naively-written non-tail `map` or `length` over a large data set fails where R7RS would succeed, which is why the standard library writes its own folds with tail-recursive helpers -- `map-onto`, `filter-onto` and `fold-right-onto` (`src/lib/prelude.scm`).

### The module system

R7RS's `define-library`, its `(scheme base)`-style library names, and its `import` declaration sets (`only`, `except`, `prefix`, `rename`) do not exist.

~~~
> (import (scheme base))
Parser error [1:1-1:8]: Malformed import statement (a built-in library name, or a quoted file name).
~~~

Scamper's module system is five statement forms (`reference.html#import`, `src/scheme/syntax.grammar:98`): `(import name)`, `(import "file.scm")`, `(import name alias)`, `(import "file.scm" alias)`, and `export` / `define-export`.
A one-argument `import` injects the module's exports into scope; a two-argument one binds them behind a qualified `alias.name` instead.
A module exports only what its `export` statements list.
See `docs/language.md` for the grammar; the built-in module names are the `.scm` files in `src/lib/`.

The top level is one mutually-recursive scope, so definition order does not matter for procedures:

~~~
> (define f (lambda (n) (if (zero? n) 0 (g (- n 1)))))
> (define g (lambda (n) (f n)))
> (f 3)
0
~~~

The source names this: "This matches Racket module semantics -- every module-level definition and import shares one mutually-recursive scope covering the whole body" (`src/scheme/scope.ts:481-492`).
R7RS's top level is sequential by contrast.
Name collisions between two user-introduced bindings are reported symmetrically, also following Racket.
**By design.**

### Summary: R7RS-small section by section

| § | Scamper's position | Why |
| --- | --- | --- |
| 6.1 Equivalence | all three present; `eq?` is a second name for `eqv?` | added in issue #641 once `ref`/`hash-set!` made identity worth asking about. Deviations: `2` is `eqv?` to `2.0`, and `0` to `-0` |
| 6.2 Numbers | JS doubles; no exactness, rationals, complex, bignums, or radix syntax; division by zero raises; `square` is `sqr`; `string->number` takes a radix, `number->string` does not | design: "the Javascript numeric stack" (`index.ts`); `sqr` frees `square` for the image library (`prelude.scm`). `gcd`/`lcm` **not yet** (`index.ts`) |
| 6.3 Booleans | `not`, `boolean?`; no `boolean=?` | unremarked. Extensions: `nand`, `nor`, `implies`, `xor` |
| 6.4 Pairs and lists | pair and cons are distinct types; `null` not `'()`; immutable; no `member`/`assoc` family | design: Clojure's split (`lang.ts:586`); "the pure, functional subset" (`index.ts`) |
| 6.5 Symbols | none | **not yet** by the label (`index.ts`), but the note itself doubts it |
| 6.6 Characters | essentially complete | -- |
| 6.7 Strings | immutable; no `string-set!`, `string-copy`, `string-fill!`, 1-arg `make-string` | design: "don't make sense in an immutable context" (`index.ts`); `string-set!` "since it is effectful" (`index.ts`) |
| 6.8 Vectors | mutable, though not the only mutable type; `[...]` is the literal; no `vector-copy`/`vector-copy!` | unremarked; the bracket literal follows Clojure (`syntax.grammar:5-9`) |
| 6.9 Bytevectors | none | design: "inherently effectful" (`index.ts`) |
| 6.10 Control | `apply`, `map`, `string-map`, `vector-map`, `vector-for-each` present | `for-each`, `string-for-each` **not yet** (`index.ts`); `call/cc`, `values`, `call-with-values`, `dynamic-wind` **not yet** (`index.ts`) |
| 6.11 Exceptions | none; `with-handler` instead, handler takes a message string | design: "inherently effectful" (`index.ts`) |
| 6.12 Environments and evaluation | none | design: "platform-specific stuff" (`index.ts`) |
| 6.13 Input and output | no ports; `display` is a statement; the `file` module reads and writes whole files | design: "in-browser, so can't implement directly" (`index.ts`) |
| 6.14 System interface | none | design: "all operating system-specific stuff" (`index.ts`) |

The four `// TODO: implement` gaps, in full: `gcd`/`lcm`, `for-each`/`string-for-each`, the `call/cc` block, and symbols.
Three further TODOs exist but are narrower -- a variant of something present, or a note on an implementation: `string->list`'s substring-bounded form (`src/js/prelude/index.ts`), `assoc-set`'s algorithm (`src/js/prelude/index.ts`), and fold variants for vectors (`src/js/prelude/index.ts`).

## 2. Where Scamper's extensions come from

Scamper's additions are not a grab bag; they are drawn from a small number of languages, with Racket dominant.
This section separates what the source *says* from what it merely *resembles*, because only the first is evidence.

### From Racket

Racket is the language the TypeScript names most often.

+ **`racket/base` arithmetic and booleans.**
  `// Additional functions from racket/base` heads `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, and `=-eps` (`src/js/prelude/index.ts`), and `// From racket/base` heads `nand`, `nor`, `implies`, `xor` (`src/js/prelude/index.ts`).
  Cited, though in Racket the four boolean operators come from `racket/bool` rather than `racket/base`, and `=-eps` is not a Racket name at all.
+ **`racket/string`.**
  `// Additional functions from racket/string.` heads `string-contains`, `string-split`, `string-split-vector` (`src/js/prelude/index.ts`), and `string-split`'s empty-field behaviour is matched to Racket's deliberately (`src/js/prelude/index.ts`).
  Cited, though `string-split-vector` is not a Racket name either.
+ **The functional hash interface.**
  `hash-ref`, `hash-set`, `hash-remove`, `hash-keys`, `hash-values`, `hash->list` and the rest follow "Racket's functional hash interface", chosen over SRFI-69/125's mutable `hash-table-*` because those "would not operate on what `{...}` produces" (`src/js/prelude/index.ts`).
+ **Module semantics.**
  One mutually-recursive top-level scope, and symmetric collision reporting: "This matches Racket module semantics" (`src/scheme/scope.ts:481-492`).
+ **Indentation.**
  "Scamper formats code by DrRacket's rules, in the editor and in the output and step panes" (`docs/formatting.md:3`, `src/scheme/style.ts:2`).

Unattributed in the source but unmistakably Racket's:

+ **`struct`**, in both its spelling and its expansion to constructor / predicate / accessors.
+ **`match`** (`reference.html#match`), whose `[pattern expression]` clause shape is Racket's.
+ **`sort`**, whose argument order is `(sort l lt?)` (`src/lib/prelude.scm`) -- Racket's, not SRFI-132's `(list-sort < lst)`.
  This is the one place the ecosystem genuinely disagrees with itself: R6RS (`list-sort proc list`) and SRFI-132 put the procedure first, while SRFI-95 and Racket put the data first.
  SRFI-132's rationale records the break in so many words -- "in SRFI 32, the data-first convention was used ... However, R6RS adopted the procedure-first convention, which is more consistent with other Scheme libraries that put the procedure first" -- so a reader arriving from R6RS finds `sort` backwards, and one arriving from Racket does not.
+ **`list-of` and `or/p`** (`src/lib/prelude.scm`), which are Racket's contract combinators `listof` and `or/c` under lighter names.
+ **`andmap` and `ormap`** (`src/lib/prelude.scm`), Racket's names for "does this hold for every element" and "for at least one".

  **They return booleans, where Racket returns values.**
  Racket's `ormap` hands back the first non-`#f` result and `andmap` the last one, so `(ormap add1 '(1 2))` is `2` in Racket; in Scamper the same call is a runtime error, because `add1` is not a predicate.
  This is forced rather than chosen: `if` requires a boolean guard (above), so a non-boolean answer could not be used as one -- `(if (ormap f l) ...)` would raise -- and the short-circuit test *inside* `andmap` could not itself be written with `if` or `cond`.
  Both also stop as soon as the answer is settled, which means a length mismatch between several lists goes unreported when the answer is decided before the shortest list runs out; Racket checks the lengths up front.
+ **`add1` and `sub1`** (`src/lib/prelude.scm`).
  The commit that added them (#604) gives a teaching motivation and names no parent language; `increment` and `decrement`, added alongside, are Scamper's own.
+ **`sqr`** (`src/lib/prelude.scm`), Racket's `racket/math` name for R7RS's `square`.
  Racket draws the same line -- `sqr` is the number and `2htdp/image`'s `square` is the shape -- which is the split Scamper has now taken (#677).
+ **`src/lib/image.scm`**, which is Racket's `2htdp/image` teachpack: `beside`, `beside/align`, `above`, `above/align`, `overlay`, `overlay/align`, `overlay/offset`, `rotate`, `text`, and the shape constructors are all 2htdp names.
  Nothing in the file says so.

  **It deviates in one important place.**
  Scamper's `circle` takes a **diameter** where `2htdp`'s takes a radius (`src/lib/image.scm:343-355`, pinned by `test/regressions/circle-diameter.test.ts`).
  A `2htdp` exercise ported unchanged draws every circle at half size.

### From Clojure

Clojure's influence is **almost entirely syntactic**; the pair/cons split, the last bullet below, is the one place it reaches the data model.
There is not one Clojure-derived procedure name in the standard library: `take-while`, `drop-while`, `partition`, `frequencies`, `some`, `every?`, `conj`, `juxt`, `zipmap`, `interleave`, `into`, and `get-in` are all absent.

What is Clojure, each attributed in the source:

+ **Brackets are not interchangeable.**
  `(...)` is an application or special form, `[...]` a vector, `{...}` a map, and each means exactly one thing (`src/scheme/syntax.grammar:5-9`).
  This is why `[...]` can be the vector literal at all.
+ **The map literal `{k1 v1 ...}`** -- "A Clojure-style map literal" (`:67`).
+ **The anonymous function `#(...)`** with `%`/`%1`/`%2`/`%&` -- "A Clojure-style anonymous function" (`:78`, `src/scheme/ast.ts:237`, `src/scheme/anon-tokens.ts:4`).
  Nesting is disallowed, matching Clojure (#605, settling issue #571).
+ **The `&` rest-parameter marker** -- "Clojure-style" (`src/scheme/syntax.grammar:147-153`).
+ **The pair/cons split** -- "We follow Clojure's lead and distinguish between pairs and lists explicitly" (`src/lpm/lang.ts:586-588`), echoed at `src/js/prelude/index.ts`.

### From SRFI-1 and the ML family

+ **`fold-left` and `fold-right` follow SRFI-1**, deliberately and with the reasoning recorded in a regression test that cites the SRFI text and MIT Scheme's deprecation note (`test/regressions/fold-arguments.test.ts`).
  The combiner takes the current element first and the accumulated value second.
  See the warning below.
+ **`compose`** (`src/lib/prelude.scm`) is common to many Lisps, Racket included, and is not attributed here.
+ **`o`**, a synonym for `compose` (`src/lib/prelude.scm`).
  `o` is SML's composition operator and also Chicken's; the source names neither, so the parent is genuinely uncertain.
+ **`|>`**, left-to-right application (`src/lib/prelude.scm`).
  The spelling is F#'s, OCaml's, and Elm's; the source names none of them.
+ **`l-s` and `r-s`**, left and right sections (`src/lib/prelude.scm`).
  "Section" is Haskell's term for a partially applied operator, but the commit that added them (#605, issue #571) settles the argument order from the issue thread and names no language.
+ **`id`**, the identity function and the unit of `compose` (`src/lib/prelude.scm`).
  The spelling is Haskell's and F#'s; Racket's identity function is `identity`, in `racket/function`, R7RS has none, and the source names no language.

### Scamper's own

Neither R7RS nor, as far as the source records, anyone else's:

+ **`??`, the hole** -- a placeholder for an expression not yet written, which raises when reached.
  A hole in an untaken branch costs nothing, so a partly-written program still runs (`docs/language.md`, "Surface syntax").
  The spelling is exactly two question marks and is reserved: `???` is an ordinary identifier, and `(define ?? 5)` is a parse error.
+ **`set-maximum-recursion-depth!`** (`src/lib/prelude.scm`), which exists because the recursion cap exists.
+ **`ignore`** (`src/lib/prelude.scm`), which suppresses a value's appearance in the output pane -- a notion with no analogue in a language whose output goes to a port.
+ **`=-eps`** (`src/lib/prelude.scm`), approximate numeric equality, which earns its place given that every number is a double.
+ **`pair`** as a constructor separate from `cons`, forced by the pair/cons split.
+ **`index-of`, `assoc-key?`, `assoc-ref`, `assoc-set`**, the replacement for the `member`/`assoc` family.
+ **`tally` and `tally-value`** (`src/lib/prelude.scm`) -- the names are Scamper's, though the function is not: counting the elements that satisfy a predicate is `count` in both SRFI-1 and Racket. Their argument order is discussed below.
+ **`list-take`, `list-drop`, `nonempty-list?`, `for-range`, `string->words`, `string-split-vector`, `vector-range`, `function?`** (a second name for `procedure?`, added in #608 because the readings say "function").
+ **`js-var`**, the FFI root, which is exported to user programs (`src/lib/index.ts:41-51`, `:91`), so the whole native surface is reachable from student code -- `(js-var "prelude_car")` evaluates to `car`'s implementation, and `js-var` itself can be shadowed like any other binding.
+ **The reserved `##...##` names** that derived forms expand into, which are the one thing a program may *not* bind: `(define ##error## 1)` is a parse error.
+ **`import` of a *file*** (`(import "helpers.scm")`), which has no R7RS counterpart because R7RS libraries are named, not located.

Two of these sit under a Racket heading in the source and are Scamper's own all the same: `=-eps`, under `// Additional functions from racket/base` (`src/js/prelude/index.ts`), and `string-split-vector`, under `// Additional functions from racket/string.` (`src/js/prelude/index.ts`).
Neither name exists in Racket, so in each case the heading claims a parent the name does not have.

`range` deserves a note of its own: its one-, two-, and three-argument shape (`src/lib/prelude.scm`) is identical in Racket, Clojure, and Python, so naming a parent would be a guess.

`src/lib/music.scm:1` carries the only explicit attribution in any `.scm` file: "inspired from Hudak's Euterpea library for the Haskell programming language".

### Folds: a warning

This is the likeliest trap in the whole library, and it is worth stating twice.

| Scamper | combiner arguments | the same combiner order elsewhere |
| --- | --- | --- |
| `(fold f v l)` | `(acc elem)` | R6RS `fold-left` |
| `(fold-left f v l)` | `(elem acc)` | SRFI-1 `fold` |
| `(fold-right f v l)` | `(elem acc)` | SRFI-1 `fold-right` |
| `(reduce f l)` | `(acc elem)` | Clojure's two-argument `reduce` |
| `(reduce-left f l)` | `(elem acc)` | SRFI-1 `reduce` |
| `(reduce-right f l)` | `(elem acc)` | SRFI-1 `reduce-right` |

`fold` takes "the accumulated value and ... the current element", in that order (`src/lib/prelude.scm`).
`fold-left` is "like `fold`, but the combining function `f` takes the current element as its first argument and the accumulated value as its second" (`src/lib/prelude.scm`).

So **Scamper's `fold` is what R6RS calls `fold-left`, and Scamper's `fold-left` is what SRFI-1 calls `fold`.**
The names are crossed relative to both standards at once, and both orders are pinned by tests -- `fold-left` and `fold-right` in `test/regressions/fold-arguments.test.ts`, `fold` in `test/libs/prelude.test.ts` -- so neither is an accident.

~~~
> (fold-left cons null (list 1 2 3))
(list 3 2 1)
> (fold-right cons null (list 1 2 3))
(list 1 2 3)
> (fold cons null (list 1 2 3))
Runtime error [3:1-3:29]: (error) expected a list as the second argument, received number
~~~

`fold` is the one that fails, because it hands `cons` the accumulator first.
Whichever spelling a reader arrives with, one of these three is not what they expect.

The `reduce` family seeds each of those folds with an element of the list instead of a value, and inherits its combiner order: `reduce` is `fold` started from the first element, `reduce-left` is `fold-left` started from the first, and `reduce-right` is `fold-right` started from the last (`src/lib/prelude.scm`).
So the crossing above reaches them too -- `(reduce - (list 1 2 3))` is `-4` but `(reduce-left - (list 1 2 3))` is `2`.
The rule that keeps all six straight: **an unsuffixed name takes the accumulator first, and a `-left` or `-right` suffix takes the element first.**

Two things are worth knowing before reading that as a peculiarity of Scamper's.

**R6RS is inconsistent in exactly the same way**: its `fold-left` hands `combine` the accumulator first and its `fold-right` hands it last.
SRFI-1 and Racket are the uniform ones -- every fold in both passes the element first and the seed last.
So a library splitting its own folds between the two orders is not unusual; what is Scamper's own is *which* names fall on which side.

**`reduce` also differs from SRFI-1 in arity, not only in order.**
SRFI-1's is `(reduce f ridentity list)`, and `ridentity` is not a seed -- it is used only when the list is empty, so `(reduce f z l)` is `(fold f (car l) (cdr l))` for any non-empty `l`.
Scamper's `reduce` takes no such value and reports an error on an empty list, which is Clojure's two-argument shape rather than SRFI-1's three-argument one.

### Argument order: where the function goes

The fold table above settles how a *combiner* receives its arguments.
A second question runs through the rest of the library: when a procedure takes both a function and a collection, which of the two comes first?

Scamper answers **function first** in seventeen of its nineteen collection procedures -- `map`, `filter`, `andmap`, `ormap`, the six folds, `string-map`, `vector-map`, `vector-map!`, `vector-for-each`, `vector-filter`, `for-range`, and `image`'s `pixel-map`.
A fold's seed always sits between the function and the list (`fold f v l`), which is what R6RS, SRFI-1, Racket and Clojure all do; there is no divergence to report there.

Two procedures take the collection first, and they are not the same case.

**`sort l lt?`** is Racket's order, and the disagreement belongs to the ecosystem rather than to Scamper -- see the note under *From Racket* above.

**`tally lst pred?`** has no such cover.
Its analogue is `count`, which exists in SRFI-1 as `(count pred clist)` and in Racket as `(count proc lst)`, and **both put the predicate first**.
Clojure has no predicate-counting procedure at all; the idiom there is `(count (filter pred coll))` -- which is, as it happens, exactly how Scamper's `tally` is implemented (`src/lib/prelude.scm`).
So `tally` takes its arguments in an order no language in this comparison uses, and in the opposite order from the `filter` it is built out of.
The order was chosen deliberately when it was added (#666), and a reversed call is caught by the contract rather than silently answering `0`; this note records the cost, which is that `tally` and `filter` cannot be read off one another.

The value-searching family is a third shape and a consistent one: `index-of`, `assoc-key?`, `assoc-ref` and `assoc-set` all take the value or key first and the list second, matching `member` and `assoc` in both R7RS-small and SRFI-1.
`tally-value lst value` is the exception within that family too, for the same reason as `tally`.

Two smaller notes on this axis.
Racket, the language Scamper draws on most, has **no single rule** of its own -- `sort`, `takef`, `dropf`, `remove-duplicates`, `build-list`, `hash-map` and `hash-for-each` are all collection-first while `map`, `filter`, `foldl` and `count` are function-first -- so "follow Racket" does not by itself decide the question.
Clojure, by contrast, is collection-**last** throughout its sequence library, which is what makes its transducers and its `->>` threading work; `|>` (above) is the one place Scamper borrows that shape, and it is value-first for the same reason.
