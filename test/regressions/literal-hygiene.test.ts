import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import * as A from '../../src/scheme/ast.js'

// Vector and map literals are *literal syntax*: what `[...]` and `{...}` mean
// must not depend on the surrounding program. Both hazards below were found
// reviewing the literals work (#334 / #325).
describe('a literal\'s meaning does not depend on user bindings', () => {
  test('a vector literal still builds a vector when `vector` is shadowed', async () => {
    // Regression: [...] used to expand to (vector ...), so binding `vector`
    // broke every vector literal with "Not a function or closure: 5".
    expect(await runProgram('(define vector 5)\n[1 2]')).toEqual([
      '(vector 1 2)',
    ])
    expect(await runProgram('(let ([vector 5]) [1 2])')).toEqual([
      '(vector 1 2)',
    ])
  })

  test('a map literal still builds a map when `vector`/`list` are shadowed', async () => {
    expect(await runProgram('(define list 5)\n(define vector 6)\n{"a" [1]}')).toEqual([
      '{ "a" : (vector 1) }',
    ])
  })

  // N.B., both literals do still expand to a `##...##` primitive, so rebinding
  // *that* name breaks them -- exactly as rebinding ##mkCtorFn## breaks
  // `struct`. The `##...##` namespace is reserved for the runtime by
  // convention, so this is the same boundary every other expansion relies on,
  // not a property of the literals.
  test('the internal primitive is what a literal depends on', async () => {
    expect(await runProgram('(define ##mkVec## 5)\n[1 2]')).toEqual([
      'Runtime error [2:1-2:5]: Not a function or closure: 5',
    ])
  })
})

describe('a map value is never mistaken for an AST node', () => {
  // Regression: the renderers dispatch AST nodes through isExp/isPat/isStmt,
  // which matched any object carrying a known `tag` string. Map literals made
  // such an object constructible from source, so {"tag" "lit"} printed as
  // `void` (an empty Lit node) instead of as itself.
  test('a map with a "tag" key prints as a map', async () => {
    expect(await runProgram('{"tag" "lit"}')).toEqual(['{ "tag" : "lit" }'])
    expect(await runProgram('{"tag" "lit" "value" 1}')).toEqual([
      '{ "tag" : "lit", "value" : 1 }',
    ])
    expect(await runProgram('{"tag" "define"}')).toEqual(['{ "tag" : "define" }'])
    expect(await runProgram('{"tag" "plit"}')).toEqual(['{ "tag" : "plit" }'])
  })

  test('genuine AST nodes are still recognized', () => {
    // The predicates keep working for real nodes, which always carry a range.
    expect(A.isExp(A.mkLit(1))).toBe(true)
    expect(A.isPat(A.mkPWild())).toBe(true)
    expect(A.isStmt(A.mkStmtExp(A.mkLit(1)))).toBe(true)
    // ...and reject a look-alike that carries no range.
    expect(A.isExp({ tag: 'lit' })).toBe(false)
    expect(A.isPat({ tag: 'plit' })).toBe(false)
    expect(A.isStmt({ tag: 'define' })).toBe(false)
  })
})
