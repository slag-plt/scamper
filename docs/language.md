# Scamper language reference

The grammar below is implemented by `src/scheme/syntax.grammar` (the Lezer
grammar) and `src/scheme/ast.ts` (the AST it parses into). The runtime language
is defined in `src/lpm/lang.ts`.

## Core surface syntax

~~~
lit ::= <number> | <boolean> | <char> | <string>
      | ' ( expr1 ... exprk )
      | [ expr1 ... exprk ]
      | { expr1 ... exprk }
      | # ( expr )

paramlist ::= ( x1 ... xk ) | ( x1 ... xk & x )

pat ::= <identifier> | _ | lit | ( <identifier> p1 ... pk )

expr ::= <identifier>
       | <identifier> . <identifier>
       | lit
       | ( expr1 ... exprk )
       | ( lambda paramlist expr )
       | ( quote expr )
       | ( if expr1 expr2 expr3 )
       | ( let ( [ pat1 expr1 ] ... [ patk exprk ] ) expr )
       | ( match expr [ pat1 expr1 ] ... [ patk exprk ] )

stmt ::= ( import <identifier> ) | ( import <identifier> <identifier> )
       | ( import <string> ) | ( import <string> <identifier> )
       | ( define <identifier> expr )
       | ( export x1 ... xk )
       | ( display <expr> )
       | ( struct <identifier> ( x1 ... xk ) )
       | expr

prog ::= stmt1 ... stmt k
~~~

A one-argument `import` injects a module's exported names into the current
scope. A two-argument `import` instead binds the module under a qualified name
(alias): its exports are reachable only as `<alias>.<name>` and are not injected
into scope. A module exports only the names its `export` statements list (the
union of them); a module with no `export` statement exports nothing.

## Derived forms

Derived forms are surface syntax desugared into the core language by
`src/scheme/expansion.ts`.

~~~
(and expr1 ... exprk)
  = (if (not expr1) #f
      ...
        (if (not exprk) #f #t))

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
        (if exprk1 exprk2 void))

(define-export x expr)
  = (define x expr)
    (export x)
~~~

## The runtime

### Value language

Scamper values are Javascript values. `scamperTag` and `structKind` are the two
symbols exported by `src/lpm/lang.ts`; see `docs/library-development.md` for
using them from a library.

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

The runtime language mirrors the core surface language, with the expression
language linearized and evaluated against a stack-based abstract machine. An
expression at runtime is a list of instructions, each specifying how many values
it pops from and pushes onto the value stack. The instruction set is the `Ops`
union in `src/lpm/lang.ts`; the handler for each is in `src/lpm/handlers/`.

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