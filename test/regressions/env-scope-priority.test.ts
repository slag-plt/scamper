import { describe, expect, test } from 'vitest'
import { Env, Module, ScamperError, Value } from '../../src/lpm'

// Regression for the lookup order of `Env.get`: local scope, then top-level
// scope, then imported modules (most recently imported first). Previously,
// `Env.get` collected matches from all three scopes in the order
// top-level -> imports -> locals and simply returned the *last* match found,
// which (a) put imports ahead of top-level bindings and (b) favored the
// *first*-imported module over later ones whenever names collided.

function mkModule(bindings: Record<string, unknown>): Module {
  const mod = new Module()
  for (const [name, value] of Object.entries(bindings)) {
    mod.registerValue(name, value as Value)
  }
  return mod
}

describe('Env.get lookup order', () => {
  test('resolves a name bound only at the top level', () => {
    const env = Env.empty.extendWithTopLevel(['x', 1])
    expect(env.get('x')).toBe(1)
  })

  test('resolves a name bound only in an imported module', () => {
    const env = Env.empty.extendWithImport('m', mkModule({ x: 1 }))
    expect(env.get('x')).toBe(1)
  })

  test('resolves a name bound only locally', () => {
    const env = Env.empty.extendWithLocals(['x', 1])
    expect(env.get('x')).toBe(1)
  })

  test('throws when a name is unbound in any scope', () => {
    expect(() => Env.empty.get('x')).toThrow(ScamperError)
  })

  test('local scope shadows a top-level binding of the same name', () => {
    const env = Env.empty
      .extendWithTopLevel(['x', 'top-level'])
      .extendWithLocals(['x', 'local'])
    expect(env.get('x')).toBe('local')
  })

  test('local scope shadows an imported binding of the same name', () => {
    const env = Env.empty
      .extendWithImport('m', mkModule({ x: 'import' }))
      .extendWithLocals(['x', 'local'])
    expect(env.get('x')).toBe('local')
  })

  test('top-level scope shadows an imported binding of the same name', () => {
    const env = Env.empty
      .extendWithImport('m', mkModule({ x: 'import' }))
      .extendWithTopLevel(['x', 'top-level'])
    expect(env.get('x')).toBe('top-level')
  })

  test('top-level scope shadows imports even when the import happens after', () => {
    const env = Env.empty
      .extendWithTopLevel(['x', 'top-level'])
      .extendWithImport('m', mkModule({ x: 'import' }))
    expect(env.get('x')).toBe('top-level')
  })

  test('among colliding imports, the most recently imported module wins', () => {
    const env = Env.empty
      .extendWithImport('first', mkModule({ x: 'first' }))
      .extendWithImport('second', mkModule({ x: 'second' }))
      .extendWithImport('third', mkModule({ x: 'third' }))
    expect(env.get('x')).toBe('third')
  })

  test('a non-colliding name still resolves through an earlier import', () => {
    const env = Env.empty
      .extendWithImport('first', mkModule({ x: 'first', onlyInFirst: 1 }))
      .extendWithImport('second', mkModule({ x: 'second' }))
    expect(env.get('onlyInFirst')).toBe(1)
    expect(env.get('x')).toBe('second')
  })

  test('full priority chain: local > top-level > most-recent import', () => {
    const env = Env.empty
      .extendWithImport('first', mkModule({ a: 'first-a', b: 'first-b', c: 'first-c' }))
      .extendWithImport('second', mkModule({ b: 'second-b', c: 'second-c' }))
      .extendWithTopLevel(['c', 'top-level-c'])
      .extendWithLocals(['c', 'local-c'])

    expect(env.get('a')).toBe('first-a')
    expect(env.get('b')).toBe('second-b')
    expect(env.get('c')).toBe('local-c')
  })
})
