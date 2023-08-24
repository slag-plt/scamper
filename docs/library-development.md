# Developing libraries in Scamper

At a high level, you can divide Scamper into two parts:

+   A core Scheme runtime.
+   A collection of libraries built primarily in Javascript.

Scamper's runtime is designed to transparently interoperate with Javascript
code (as much as possible, at least), so library development is as simple
as writing a regular Typescript/Javascript module and then performing some
manual wiring so that the Scamper runtime knows the library exists.

## Scamper-to-Javascript Mapping

When writing a library, you can use plain Javascript types which map onto
Scamper types as follows:

+   `(boolean? e) <=> typeof e === 'boolean'`
+   `(number? e) <=> typeof e === 'number'`
+   `(string? e) <==> typeof e === 'string'`
+   `(null? e) <==> e === null`
+   `(void? e) <==> e === undefined`
+   `(vector? e) <==> Array.isArray(e)`

We encode other Scamper types as objects with a field `_scamperTag` denoting
their type.

+   `(list? e) <==> typeof e === 'object': { _scamperTag: 'pair', fst: Value, snd: Value, isList: true }`
+   `(pair? e) <==> typeof e === 'object': { _scamperTag: 'pair', fst: Value, snd: Value, isList: false }`
+   `(struct? e) <==> typeof e === 'object': { _scamperTag: 'struct', 'kind': string, fields: Value[] }`

Functions are encoded in several ways, depending on the type of the function in
question:

+   `typeof e === 'object': { _scamperTag: 'closure', params: string[], ops: Op, env: Env }`:
    A closure over a Scamper function (defined by its `Op` codes).
+   `typeof e === 'object': { _scamperTag: 'jsfunc', fn: Function, arity: number, isVariadic: boolean }`
    A wrapped Javascript function that specifies its arity.

    _TODO_: soon, we will replace the arity check with a full-blown
    first-order contract system for authoring preconditions at either the
    Javascript or Scamper level.
+   `typeof e === 'function'`: A raw Javascript function

When authoring library functions, it is strongly recommended to write _wrapped
Javascript functions_ so that the Scamper runtime can perform runtime checks
on function calls. (Note that plain Javascript does not check that a function
call has the correct number of arguments, assigning `undefined` to arguments
that don't receive values and ignoring extra arguments.) Our usual order of
operations here is:

+   Write library function in pure Javascript.
+   Wrap the function using `mkJsFunction` found in `sem.ts` when including
    the function in the library's Scamper export list.

Finally, any other object (`typeof e === 'object'`) is also a valid Scamper
type. Such values are passed opaquely throughout Scamper and operated on
via the library's API.

## Scamper Runtime Support

A number of the Scamper modules can be imported by libraries for support
purposes. These include:

+   `lang.ts`: `ScamperError` is the primary exception you should throw
    in library code.
+   `value.ts`: provides query and creation functions for the various Scamper
    object types, _e.g._, `isPair` and `mkPair`.  Importantly for library
    development, `mkJsFunction` wraps a Javascript function with arity
    information so the runtime can perform dynamic checks on functionc alls.
+   `sem.ts`: `callFunction` allows a library to invoke a higher-order function
    without knowing what kind of function value it is.

## Library Architecture and Wiring with Scamper

Built-in libraries should be placed in `/lib`. Each library should export a
_library export mapping_ of type `[string, any][]`. Each entry in this mapping
maps a Scamper identifier name, to the value (usually a function) it should be
bound.

For `import` statements to recognize built-in libraries, add an additional
entry in `builtin.ts` for the library. This file maintains a mapping from
library identifiers (used in `(import ...)` statements) to their library
export mappings.