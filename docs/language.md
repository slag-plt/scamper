# Scamper language reference

The surface syntax below is what `src/scheme/syntax.grammar` accepts, read into the AST of `src/scheme/ast.ts`.
`src/scheme/expansion.ts` then rewrites the derived forms away, leaving the core.
The runtime language is defined in `src/lpm/lang.ts`.

## Surface syntax

~~~
pat ::= _
      | <identifier>
      | <number> | <boolean> | <char> | <string>
      | ( pat1 ... patk )               ; constructor pattern
      | [ pat1 ... patk ]               ; vector pattern

paramlist ::= ( x1 ... xk ) | ( x1 ... xk & x ) | ( & x )

expr ::= <identifier>
       | <number> | <boolean> | <char> | <string>
       | [ expr1 ... exprk ]                        ; vector literal
       | { key1 val1 ... keyn valn }                ; map literal
       | # ( expr )                                 ; anonymous function
       | ( expr1 ... exprk )                        ; application
       | ( lambda paramlist expr )
       | ( if expr1 expr2 expr3 )
       | ( let ( [ pat1 expr1 ] ... [ patk exprk ] ) expr )
       | ( match expr [ pat1 expr1 ] ... [ patk exprk ] )
       | ( and expr1 ... exprk )
       | ( or expr1 ... exprk )
       | ( begin expr1 expr2 ... exprk )
       | ( cond [ expr11 expr12 ] ... [ exprk1 exprk2 ] )

stmt ::= ( import <identifier> ) | ( import <identifier> <identifier> )
       | ( import <string> ) | ( import <string> <identifier> )
       | ( define <identifier> expr )
       | ( define-export <identifier> expr )
       | ( export x1 ... xk )
       | ( display expr )
       | ( struct <identifier> ( x1 ... xk ) )
       | expr

prog ::= stmt1 ... stmtk
~~~

There is no `quote`, and no `'` shorthand.
A list is built with `list`, and a vector with the `[...]` literal.

Each bracket means exactly one thing (#334).
`(...)` is an application or special form, `[...]` is a vector — or, in a pattern, a vector pattern — except in the fixed `[pattern expression]` positions of `let`, `match`, and `cond`.
`{...}` is a map literal, whose elements read as alternating keys and values; an odd number of them is an error, checked in `src/scheme/lezer-bridge.ts` rather than by the grammar.

An identifier may be a one-level *qualified* name: two names joined by a single `.`, such as `img.outlined-square`, referring to a binding through an imported module's alias.
Exactly one `.` is allowed, so `a.b.c` is not a single name, and a qualified name may only be a reference, never a binder.

`#(...)` is a Clojure-style anonymous function.
Its parameters are the `%k` markers appearing in the body — `%` is `%1`, and the arity is the largest index referenced — with `%&` as an optional rest parameter.
The body is an ordinary expression, so any parenthesized form may appear: `#(+ %1 1)`, `#(let ([x 1]) (+ x %))`.

`&` in a parameter list separates the fixed parameters from a rest parameter.

A one-argument `import` injects a module's exported names into the current scope.
A two-argument `import` binds the module under a qualified name instead: its exports are reachable only as `<alias>.<name>` and are not injected into scope.
A module exports only the names its `export` statements list, taking their union; a module with no `export` statement exports nothing.

## Core forms

Expansion leaves seven expression forms:

~~~
expr ::= <identifier>
       | <number> | <boolean> | <char> | <string>
       | ( expr1 ... exprk )
       | ( lambda paramlist expr )
       | ( if expr1 expr2 expr3 )
       | ( let ( [ pat1 expr1 ] ... [ patk exprk ] ) expr )
       | ( match expr [ pat1 expr1 ] ... [ patk exprk ] )

stmt ::= ( import ... ) | ( define <identifier> expr ) | ( export x1 ... xk )
       | ( display expr ) | expr
~~~

Patterns are not expanded and keep the full surface set.

## Derived forms

Each of these is rewritten by `src/scheme/expansion.ts`.
Every rewritten node is tagged with the form it came from, so a reduction trace can recover the original spelling rather than guess at it.

~~~
(and expr1 ... exprk)
  = (if expr1
      ...
        (if exprk #t #f)
      ...
      #f)

(or expr1 ... exprk)
  = (if expr1 #t
      ...
        (if exprk #t #f))

(begin expr1 ... exprk)
  = (let ([_ expr1])
      ...
        (let ([_ expr(k-1)]) exprk))

(cond [expr11 expr12] ... [exprk1 exprk2])
  = (if expr11 expr12
      ...
        (if exprk1 exprk2 (##error## "No matching clause in cond")))

#(body)
  = (lambda (%1 ... %m [& %&]) body)
      where m is the largest %k referenced in body

[expr1 ... exprk]
  = (##mkVec## expr1 ... exprk)

{key1 val1 ... keyn valn}
  = (##mkObj## key1 val1 ... keyn valn)

(define-export x expr)
  = (define x expr)
    (export x)

(struct S (f1 ... fk))
  = (define S  (##mkCtorFn## "S" ["f1" ... "fk"]))
    (define S? (##mkPredFn## "S"))
    (define S-f1 (##mkGetFn## "S" "f1"))
    ...
    (define S-fk (##mkGetFn## "S" "fk"))
~~~

A falling-through `cond` raises rather than producing void (#336).
The `##...##` names are runtime primitives from `src/js/runtime/`, not prelude bindings: a derived form must mean the same thing whether or not the user has bound `error`, `vector`, or a struct name of their own.

## The runtime

### Value language

Scamper values are Javascript values.
`scamperTag` and `structKind` are the two symbols exported by `src/lpm/lang.ts`; see `docs/library-development.md` for using them from a library.

~~~typescript
type Id = string

type Value = number | boolean | string
           | null      // the null list
           | undefined  // void
           | Value[]   // vectors
           | Function  // raw Javascript functions
           | { [scamperTag]: 'char', value: string }  // chars
           | { [scamperTag]: 'closure', params: Id[], restParam?: Id,
               code: Blk, locals: Scope[], name?: Id }  // closures
           | { [scamperTag]: 'struct', [structKind]: string, [key: string]: Value }      // structs
           | { [scamperTag]: 'struct', [structKind]: 'pair', fst: Value, snd: Value }    // pairs
           | { [scamperTag]: 'struct', [structKind]: 'cons', head: Value, tail: List }   // cons cells
~~~

### Linear representation

The runtime language mirrors the core surface language, with the expression language linearized and evaluated against a stack-based abstract machine.
An expression at runtime is a list of instructions, each specifying how many values it pops from and pushes onto the value stack.
The instruction set is the `Ops` union in `src/lpm/lang.ts`; the handler for each is in `src/lpm/handlers/`.

~~~
instr ::= lit(value)                          [0/1]
        | var(name)                           [0/1]
        | cls(params, rest, body, name)       [0/1]
        | ap(k)                               [k+1/1]
        | ap-spread                           [2/1]
        | let(pat)
        | if(ifBranch, elseBranch)            [1/1]
        | match(branches)                     [1/1]
        | push-handler                        [1/0]
        | pop-handler                         [0/0]
        | pop-scope                           [0/0]
~~~