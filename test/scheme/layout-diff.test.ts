import { describe, expect, test } from 'vitest'
import { expToLayout, type Layout } from '../../src/scheme/ast'
import { changedLayoutPath } from '../../src/scheme/layout-diff'
import * as Scheme from '../../src/scheme'
import { initializeLibs } from '../../src/lib'
import * as SymbolDB from '../../src/scheme/symbol-db'

await initializeLibs()
SymbolDB.initialize()

/** The layout of the single expression in `src`, for diffing against another. */
function layoutOf(src: string): Layout {
  const { program, diagnostics } = Scheme.tokenizeAndParse(src)
  if (program === undefined) {
    throw new Error(`${src}: ${diagnostics.map((d) => d.message).join(', ')}`)
  }
  const stmt = program[0]
  if (stmt.tag !== 'stmtexp') throw new Error(`${src} is not an expression`)
  return expToLayout(stmt.expr)
}

/** The sub-expression `path` points at, as text. */
function at(layout: Layout, path: number[]): string {
  let node = layout
  for (const i of path) {
    node =
      node.kind === 'group' ? node.children[i]
      : node.kind === 'hash' ? node.child
      : node
  }
  return JSON.stringify(node)
}

describe('changedLayoutPath', () => {
  function diff(before: string, after: string) {
    return changedLayoutPath(layoutOf(before), layoutOf(after))
  }

  test('identical expressions have no change', () => {
    expect(diff('(+ 1 2)', '(+ 1 2)')).toBeNull()
  })

  test('a reduced argument is found at its own position', () => {
    // (* (+ 1 2) 3) --> (* 3 3): the first argument is what moved.
    expect(diff('(* (+ 1 2) 3)', '(* 3 3)')).toEqual([1])
  })

  test('it descends as far as the change goes', () => {
    // Only the innermost sum reduced; everything above it is untouched.
    expect(diff('(* (+ (+ 1 1) 2) 3)', '(* (+ 2 2) 3)')).toEqual([1, 1])
  })

  test('a whole expression collapsing to a value is the root', () => {
    expect(diff('(+ 1 2)', '3')).toEqual([])
  })

  test('two children changing at once points at the node holding them', () => {
    // No single child is "the" change, so the parent is.
    expect(diff('(+ (* 1 1) (* 2 2))', '(+ 1 4)')).toEqual([])
  })

  test('a change under a different head is still located', () => {
    expect(diff('(f 1 (g 2))', '(f 1 7)')).toEqual([2])
  })

  test('the path really does point at the changed sub-expression', () => {
    const after = layoutOf('(* 3 3)')
    const path = diff('(* (+ 1 2) 3)', '(* 3 3)')
    if (path === null) throw new Error('expected a change')
    // Index 1 is the first argument: '(' is not a child, the head is 0.
    expect(at(after, path)).toContain('3')
  })
})
