import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { expToString } from '../../src/scheme/ast'
import type { FunctionDoc } from '../../src/scheme/docstring/docstring'
import { libraryBindings } from '../libs/library-bindings'

// https://github.com/slag-plt/scamper/issues/541
//
// `cons` documented `(cons v1 v2) -> pair?` with `v2 : any`, and both
// predicates named the wrong type. `mkCons` (src/lpm/util.ts) requires a list
// tail, so `(cons 1 2)` passed its own contract and was then rejected by the
// native underneath -- #487's shape, and the one entry the contract sweep
// (test/libs/contracts.test.ts) carried on KNOWN_BROKEN. What `cons` builds is
// a *cons*, which `list?` is true of and `pair?` is false of, so the return
// predicate was wrong in the same direction.
//
// The maintainer's call on #541 is that the contract performs the uniform
// language: narrowing `v2` to `list?` makes `(cons 1 2)` report the same
// "expected ..., received ..." message every other contract violation does,
// rather than the native's bespoke "The second argument to cons should be a
// list". That is #256's resolution for `car`/`cdr` applied here -- see
// car-cdr-contract.test.ts. The native's check stays as the backstop for the
// library-internal calls that deliberately skip contracts (#476/#488); it is
// simply no longer what a student sees.

/** `cons`'s parsed docstring, which both predicates are read from. */
function consDoc(): FunctionDoc {
  const binding = libraryBindings().find(
    (b) => b.module === 'prelude' && b.name === 'cons',
  )
  if (binding?.doc === undefined) {
    throw new Error("prelude's cons has no parsed docstring")
  }
  return binding.doc
}

describe("cons's contract rejects a non-list tail (#541)", () => {
  test('(cons 1 2) reports the uniform contract message', async () => {
    expect(await runProgram('(cons 1 2)')).toEqual([
      'Runtime error [1:1-1:10]: (error) expected a list, received number',
    ])
  })

  test('every non-list tail fails the same way, whatever its type', async () => {
    expect(
      await runProgram('(cons 1 "two")\n(cons 1 #t)\n(cons 1 (pair 1 2))'),
    ).toEqual([
      'Runtime error [1:1-1:14]: (error) expected a list, received string',
      'Runtime error [2:1-2:11]: (error) expected a list, received boolean',
      'Runtime error [3:1-3:19]: (error) expected a list, received pair',
    ])
  })

  test('the violation is blamed on the call, not on prelude.scm', async () => {
    // The call-site recovery #254/#239 won, restated for the contract cons now
    // has: the inner call is the one reported, not the enclosing function's.
    expect(
      await runProgram('(define f (lambda (t) (cons 1 t)))\n(f 2)'),
    ).toEqual([
      'Runtime error [1:23-1:32]: (error) expected a list, received number',
    ])
  })

  test('a list tail is still accepted', async () => {
    expect(
      await runProgram('(cons 1 null)\n(cons 1 (list 2 3))\n(cons 1 (cons 2 null))'),
    ).toEqual(['(list 1)', '(list 1 2 3)', '(list 1 2)'])
  })
})

describe("cons's documented types are the ones it has (#541)", () => {
  test('v2 is documented as a list, and v1 stays any', () => {
    expect(
      consDoc().params.map((p) => [p.name, expToString(p.predicate)]),
    ).toEqual([
      ['v1', 'any'],
      ['v2', 'list?'],
    ])
  })

  test('the return predicate is documented as a list, not a pair', () => {
    expect(expToString(consDoc().signature.predicate)).toBe('list?')
  })

  test('because what cons builds is a list and is not a pair', async () => {
    // The reason the return predicate was wrong: `pair?` is
    // isStructKind(v, 'pair') and a cons cell is its own struct kind, so no
    // value cons can produce has ever satisfied it.
    expect(
      await runProgram(
        '(pair? (cons 1 (list 2)))\n(list? (cons 1 (list 2)))\n(pair? (pair 1 2))',
      ),
    ).toEqual(['#f', '#t', '#t'])
  })
})
