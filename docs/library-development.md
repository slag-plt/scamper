# Developing libraries in Scamper

Scamper divides into two parts:

+   A core Scheme runtime, the Little Pattern Machine, in `src/lpm/`.
+   A collection of libraries, most of them backed by Javascript, in `src/js/` and `src/lib/`.

The runtime interoperates with Javascript directly, so a library is an ordinary Typescript module plus a Scheme module that binds its exports.

## The two halves of a library

A library named `foo` is two directories:

+   `src/js/foo/index.ts` exports plain Typescript functions and values.
+   `src/lib/foo.scm` binds each of them to a Scamper name with `js-var`, and carries the docstring the documentation page and the contract checker read.

`src/lib/test.scm` is a short example:

```scheme
;;; (test-result-ok desc) -> test-result?
;;;  desc : string?
;;; Returns a test result indicating that the test named `desc` passed.
;;; @category testing
(define-export test-result-ok (js-var "test_testResultOk"))
```

`js-var` looks a name up in a single map of every Javascript binding, built in `src/js/index.ts`.
That map is flat, so **every Javascript export is prefixed with its library's name** — `test_testResultOk`, `canvas_makeCanvas`.
`src/js/image/` is the exception: it backs four concepts and exports `drawing_*`, `color_*`, `font_*`, and `image_*` (#103).

## Scamper-to-Javascript mapping

Plain Javascript types map onto Scamper types directly:

+   `(boolean? e) <=> typeof e === 'boolean'`
+   `(number? e) <=> typeof e === 'number'`
+   `(string? e) <=> typeof e === 'string'`
+   `(null? e) <=> e === null`
+   `(void? e) <=> e === undefined`
+   `(vector? e) <=> Array.isArray(e)`

Everything else is an object carrying the two symbols `L.scamperTag` and, for structs, `L.structKind`, both from `src/lpm/lang.ts`.
Import them rather than writing the underlying strings:

```typescript
import * as L from '../../lpm'

export interface Ok extends L.Struct {
  [L.structKind]: 'test-result-ok'
  desc: string
}

export function test_testResultOk(desc: string): Ok {
  return { [L.scamperTag]: 'struct', [L.structKind]: 'test-result-ok', desc }
}
```

Pairs and lists are structs of kind `'pair'` and `'cons'`; a list is `null` or a `Cons`.
Functions are either a `Closure` (a Scamper function, compiled to opcodes) or a `JsFunction`.
Any other object is a valid Scamper value, passed opaquely and operated on through the library's own API.

## Runtime support

Import these from `src/lpm/`:

+   `lang.ts` defines `Value` and the tagged-object shapes, and provides the query and construction helpers (`isStructKind`, `mkPair`, `nameFn`).
+   `error.ts` defines `ScamperError`, the exception library code should throw, and `ICE` for conditions that indicate a bug in Scamper itself.

Javascript code cannot call back into Scamper: `callScamperFn` is disabled.
A procedure that needs to apply a caller-supplied function belongs in the `.scm` half instead, written against `with-handler` and the other special forms — as `test-case` and `test-exn` are.

## Argument checking

Arity and argument types are not checked by Javascript, and are not checked by hand.
`src/lib/index.ts` compiles each library with `insertContracts: true`, which derives a contract from the docstring above each `define-export` and wraps the export in it.
The signature line and the `param : predicate?` lines are therefore load-bearing.

## Wiring a new library

1.  Write `src/js/foo/index.ts`, prefixing every export with `foo_`.
2.  Add it to the imports and to the `internals` map in `src/js/index.ts`.
3.  Write `src/lib/foo.scm`, binding each export with `js-var` under a docstring.
4.  If the library ships custom renderers in `src/js/foo/renderers/`, add an entry to `src/app/web/renderers.ts` as well.
    That file and the `internals` map are two independent enumerations of the same set of libraries, so a library missing from it silently renders as plain text in the browser.

The `.scm` file needs no registration.
`scripts/generate-lib-sources.mjs` reads every `*.scm` in `src/lib/` into `src/lib/generated/sources.ts`, and `src/lib/index.ts` compiles and runs each one at startup, recording the result in the registry in `src/lpm/builtin-registry.ts` that `import` consults.

A library needs no Javascript at all if it does not want any: `src/lib/gradescope.scm` is plain Scamper and has no `src/js/gradescope/index.ts` entry in `internals`.
